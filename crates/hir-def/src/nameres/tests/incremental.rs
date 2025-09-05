use {
    crate::{project_def_map, test_db::TestDB},
    base_db::SourceDatabase,
    test_fixture::WithFixture,
};

fn check_def_map_is_not_recomputed(fixture_initial: &str, fixture_change: &str) {
    let (mut db, pos, path) = TestDB::with_position(fixture_initial);
    let project = db.fetch_test_project();
    {
        let events = db.log_executed(|| {
            project_def_map(&db, project);
        });
        assert!(
            format!("{events:?}").contains("project_def_map"),
            "no project def map computed:\n{events:#?}"
        )
    }
    db.set_file_text(pos.file_id, path, fixture_change);

    {
        let events = db.log_executed(|| {
            project_def_map(&db, project);
        });
        assert!(
            !format!("{events:?}").contains("project_def_map"),
            "project def map invalidated:\n{events:#?}"
        )
    }
}

#[test]
fn typing_inside_a_function_should_not_invalidate_def_map() {
    check_def_map_is_not_recomputed(
        r"
//- /main.pro
implement foo$0

clauses
    bar().

end implement
",
        r"
implement foo

clauses
    bar() :-
        succeed().

end implement
",
    );
}
