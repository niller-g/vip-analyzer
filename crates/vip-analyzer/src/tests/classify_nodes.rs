use {
    hir::Semantics,
    ide::{AnalysisHost, RootDatabase},
    ide_db::{LineIndexDatabase, base_db::SourceDatabase, classify::IdentClass},
    project_model::{ManifestPath, Project, ProjectFiles},
    std::path::Path,
    syntax::{AstNode, NodeOrToken, SyntaxElement, WalkEvent, ast, match_ast},
    vfs::{AbsPath, AbsPathBuf, FileId, Vfs},
};

fn load_project(root: &Path) -> anyhow::Result<(RootDatabase, Vfs, Project)> {
    let manifest = ManifestPath::discover_single(&AbsPathBuf::assert_utf8(root.to_path_buf()))?;
    let project = Project::load(manifest, Default::default(), &|_| {})?;
    let (db, vfs) = project_model::load_workspace_at(root, &|_| {})?;
    let host = AnalysisHost::with_database(db);
    let db = host.raw_database().clone();
    Ok((db, vfs, project))
}

fn find_unclassified<F>(
    sema: &Semantics<'_, RootDatabase>,
    db: &RootDatabase,
    vfs: &Vfs,
    all_files: &[FileId],
    get_elements: F,
) -> anyhow::Result<Vec<(String, String)>>
where
    F: Fn(&Semantics<'_, RootDatabase>, FileId) -> Vec<SyntaxElement>,
{
    let mut result = Vec::new();
    for file_id in all_files.iter().copied() {
        let line_index = db.line_index(file_id);
        let file_text = db.file_text(file_id).text(db);
        let file_path = vfs.file_path(file_id);
        let relative = file_path
            .as_path()
            .unwrap()
            .strip_prefix(AbsPath::assert(&test_utils::project_root()))
            .unwrap()
            .as_str()
            .replace('/', "\\");

        let mut buf = String::new();
        for elem in get_elements(sema, file_id) {
            let range = elem.text_range();
            let line_col = line_index.line_col(range.start());
            let line = line_col.line + 1;
            let col = line_col.col + 1;
            let text = &file_text[range].replace("\r\n", "\n");
            buf.push_str(&format!("{:?} {relative}:{line}:{col}: {text}\n", elem.kind()));
        }
        result.push((relative, buf));
    }

    Ok(result)
}

fn all_files(db: &RootDatabase, project: &Project) -> Vec<FileId> {
    let mut load = |path: &AbsPath| {
        let vfs_path = vfs::VfsPath::from(path.to_path_buf());
        db.file_for_path(vfs_path).unwrap()
    };
    let files = ProjectFiles::load_files_with(db, project, &mut load, |err_msg| {
        panic!("Error message: {err_msg}");
    });
    let mut files: Vec<_> = files.into_iter().collect();
    files.sort();
    files.dedup();
    files
}

fn unclassified_nodes(sema: &Semantics<'_, RootDatabase>, file_id: FileId) -> Vec<SyntaxElement> {
    let file = sema.parse(file_id);
    let root = file.syntax();

    let mut unclassified = Vec::new();
    for event in root.preorder() {
        let WalkEvent::Enter(syntax) = event else {
            continue;
        };
        match_ast! {
            match syntax {
                ast::SourceFile(_) => continue,
                _ => {},
            }
        }

        if IdentClass::classify_node(sema, &syntax).is_none() {
            unclassified.push(NodeOrToken::Node(syntax));
        }
    }
    unclassified
}

fn unclassified_tokens(sema: &Semantics<'_, RootDatabase>, file_id: FileId) -> Vec<SyntaxElement> {
    let file = sema.parse(file_id);
    let root = file.syntax();
    let mut unclassified = Vec::new();

    for event in root.preorder_with_tokens() {
        let WalkEvent::Enter(syntax) = event else {
            continue;
        };
        match &syntax {
            syntax::NodeOrToken::Node(_) => continue,
            syntax::NodeOrToken::Token(tok) => {
                let kind = tok.kind();
                if kind.is_trivia() || kind.is_strict_keyword() || kind.is_punct() {
                    continue;
                } else if IdentClass::classify_token(sema, tok).is_none() {
                    unclassified.push(syntax);
                }
            }
        }
    }

    unclassified
}

#[cfg(test)]
mod tests {
    use expect_test::expect_file;
    use test_utils::skip_slow_tests;
    use {super::*, test_utils::project_root};

    #[test]
    #[ignore = "ProDir is required for it to work"]
    fn test_classify_nodes_and_tokens() {
        if skip_slow_tests() {
            return;
        }

        let data_dir = project_root().join("crates/vip-analyzer/test_data");
        let test_project_root = project_root().join("visual-prolog/examples/dummyProject");

        let (db, vfs, project) = load_project(test_project_root.as_std_path()).unwrap();
        let sema = Semantics::new(&db);

        let all_files = all_files(&db, &project);
        let unclassified =
            find_unclassified(&sema, &db, &vfs, &all_files, unclassified_nodes).unwrap();
        for (file_path, text) in &unclassified {
            let file_name = file_path.replace('\\', "_");
            expect_file![format!("{data_dir}/classify_nodes/{file_name}.txt")].assert_eq(text);
        }

        let unclassified =
            find_unclassified(&sema, &db, &vfs, &all_files, unclassified_tokens).unwrap();
        for (file_path, text) in &unclassified {
            let file_name = file_path.replace('\\', "_");
            expect_file![format!("{data_dir}/classify_tokens/{file_name}.txt")].assert_eq(text);
        }
    }
}
