use {
    crate::{Analysis, AnalysisHost},
    span::{FilePosition, FileRange},
    test_fixture::ChangeFixture,
    test_utils::extract_annotations,
};

/// Creates analysis from a multi-file fixture, returns positions marked with $0.
pub(crate) fn annotations(fixture: &str) -> (Analysis, FilePosition, Vec<(FileRange, String)>) {
    let mut host = AnalysisHost::default();
    let change_fixture = ChangeFixture::parse(fixture);
    host.db.apply_change(change_fixture.change);
    let (file_id, _path, range_or_offset) =
        change_fixture.file_position.expect("expected a marker ($0)");
    let offset = range_or_offset.expect_offset();

    let annotations = change_fixture
        .files
        .iter()
        .flat_map(|&file_id| {
            let file_text = host.analysis().file_text(file_id).unwrap();
            let annotations = extract_annotations(&file_text);
            annotations.into_iter().map(move |(range, data)| (FileRange { file_id, range }, data))
        })
        .collect();
    (host.analysis(), FilePosition { file_id, offset }, annotations)
}
