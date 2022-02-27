use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn load_data() -> rink_format::UnitsData {
    rink_data::load_data()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("load", |b| b.iter(|| black_box(load_data())));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
