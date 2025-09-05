use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use {
    criterion::{Criterion, black_box, criterion_group, criterion_main},
    std::borrow::Cow,
};

fn criterion_benchmark(c: &mut Criterion) {
    let path = Path::new("../../visual-prolog/examples");
    let documents: Vec<_> = vip_files(path).map(|entry| read_bytes(&entry)).collect();

    c.bench_function(&format!("parsing_{}", documents.len()), |b| {
        b.iter(|| {
            for text in &documents {
                let p = syntax::SourceFile::parse(text);
                black_box(p);
            }
        })
    });
}

fn read_bytes(path: &Path) -> String {
    let bytes = std::fs::read(path).expect("File not found");
    stdx::to_utf8_else_utf16(&bytes, true).map(Cow::into_owned).expect("Invalid byte sequence")
}

fn vip_files(path: &Path) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(path).into_iter().filter_map(|e| e.ok().map(|e| e.path().to_path_buf())).filter(
        |entry| {
            entry
                .extension()
                .map(|ext| {
                    ext.to_str()
                        .map(|ext_str| matches!(ext_str, "pro" | "i" | "cl" | "ph" | "pack"))
                        .unwrap_or_default()
                })
                .unwrap_or_default()
        },
    )
}

criterion_group! {
    name = benches;
    config = Criterion::default().measurement_time(std::time::Duration::new(60, 0)).warm_up_time(std::time::Duration::new(10, 0));
    targets = criterion_benchmark
}
criterion_main!(benches);
