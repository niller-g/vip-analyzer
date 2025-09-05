//! A set of high-level utility fixture methods to use in tests.

use {
    base_db::{
        Env, ExtraProjectData, FileChange, FileId, IncludedPaths, ProjectData, ProjectGraphBuilder,
        SourceDatabase, VfsPath,
    },
    span::FilePosition,
    test_utils::{
        CURSOR_MARKER, ESCAPED_CURSOR_MARKER, Fixture, FixtureWithProjectMeta, RangeOrOffset,
        extract_range_or_offset,
    },
};

pub trait WithFixture: Default + SourceDatabase + 'static {
    #[track_caller]
    fn with_position(fixture: &str) -> (Self, FilePosition, VfsPath) {
        let (db, file_id, path, range_or_offset) = Self::with_range_or_offset(fixture);
        let offset = range_or_offset.expect_offset();
        (db, FilePosition { file_id, offset }, path)
    }

    #[track_caller]
    fn with_range_or_offset(fixture: &str) -> (Self, FileId, VfsPath, RangeOrOffset) {
        let fixture = ChangeFixture::parse(fixture);
        let mut db = Self::default();
        fixture.change.apply(&mut db);

        let (file_id, path, range_or_offset) = fixture
            .file_position
            .expect("Could not find file position in fixture. Did you forget to add an `$0`?");
        (db, file_id, path, range_or_offset)
    }
}

impl<DB: SourceDatabase + Default + 'static> WithFixture for DB {}

pub struct ChangeFixture {
    pub file_position: Option<(FileId, VfsPath, RangeOrOffset)>,
    pub files: Vec<FileId>,
    pub change: FileChange,
}

const SOURCE_ROOT_PREFIX: &str = "/";

impl ChangeFixture {
    pub fn parse(fixture: &str) -> ChangeFixture {
        let mut source_change = FileChange::default();

        let mut files = Vec::new();
        let mut project_graph = ProjectGraphBuilder::default();

        let mut file_id = FileId::from_raw(0);

        let mut file_position = None;

        let FixtureWithProjectMeta { fixture } = FixtureWithProjectMeta::parse(fixture);

        for entry in fixture {
            let mut range_or_offset = None;
            let text = if entry.text.contains(CURSOR_MARKER) {
                if entry.text.contains(ESCAPED_CURSOR_MARKER) {
                    entry.text.replace(ESCAPED_CURSOR_MARKER, CURSOR_MARKER)
                } else {
                    let (roo, text) = extract_range_or_offset(&entry.text);
                    assert!(file_position.is_none());
                    range_or_offset = Some(roo);
                    text
                }
            } else {
                entry.text.as_str().into()
            };

            let meta = FileMeta::from_fixture(entry);
            assert!(meta.path.starts_with(SOURCE_ROOT_PREFIX));
            let path = VfsPath::new_virtual_path(meta.path);
            if let Some(range_or_offset) = range_or_offset {
                file_position = Some((file_id, path.clone(), range_or_offset));
            }

            source_change.change_file(file_id, path.clone(), Some(text));
            files.push(file_id);
            file_id = FileId::from_raw(file_id.index() + 1);
        }

        let project_root = VfsPath::new_virtual_path("/.".to_owned());
        let includes: IncludedPaths<VfsPath> = IncludedPaths::new(
            vec![project_root.clone()]
                .into_iter()
                .enumerate()
                .map(|(i, path)| (path, i.into()))
                .collect(),
        );
        project_graph.add_project_root(
            ProjectData::new(file_id),
            ExtraProjectData::new("va_test_fixture", project_root, includes, files.to_vec()),
        );

        source_change.set_project_graph(project_graph);

        ChangeFixture { file_position, files, change: source_change }
    }
}

#[derive(Debug)]
struct FileMeta {
    path: String,
    #[expect(dead_code)]
    env: Env,
}

impl FileMeta {
    fn from_fixture(f: Fixture) -> Self {
        Self { path: f.path, env: f.env.into_iter().collect() }
    }
}
