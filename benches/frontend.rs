use sage::{
    lir::*,
    parse::*,
    targets::{self, CompiledTarget},
    vm::*,
    LOGO_WITH_COLOR, *,
};
use std::{
    fmt,
    fs::{read_to_string, write},
};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

const CALL_STACK_SIZE: usize = 8192;

fn read_file(filename: &str) -> String {
    read_to_string(filename).unwrap()
}

fn parse_frontend_file(filename: &str) -> Expr {
    parse_frontend(read_file(filename), Some(filename)).unwrap()
}

fn compile_frontend_file(filename: &str) -> StandardProgram {
    match parse_frontend_file(filename)
        .compile()
        .unwrap()
    {
        // If we got back a valid program, assemble it and return the result.
        Ok(asm_code) => asm_code
            .assemble(CALL_STACK_SIZE).unwrap().into(),
        Err(asm_code) => asm_code
            .assemble(CALL_STACK_SIZE).unwrap(),
    }
}

fn run_frontend_file(filename: &str, input: &str) -> String {
    let program = compile_frontend_file(filename);

    let device = StandardInterpreter::new(TestingDevice::new(input)).run(&program).unwrap();
    let output_text = device.output_str();

    return output_text;
}

fn bench_frontend(c: &mut Criterion) {
    let mut group = c.benchmark_group("Example Programs");
    group.sample_size(10);
    
    group.bench_function("Hashmap (compile + run)", |b| b.iter(|| run_frontend_file("examples/frontend/hashmap.sg", "hello world!")));
    group.bench_function("Vector (compile + run)", |b| b.iter(|| run_frontend_file("examples/frontend/sequence.sg", "hello world!")));
    group.bench_function("Vector2 (compile + run)", |b| b.iter(|| run_frontend_file("examples/frontend/sequence2.sg", "hello world!")));
    group.bench_function("AES (compile + run)", |b| b.iter(|| run_frontend_file("examples/frontend/AES.sg", "hello world!")));

    let precompiled_hashmap = compile_frontend_file("examples/frontend/hashmap.sg");
    let precompiled_vector = compile_frontend_file("examples/frontend/sequence.sg");
    let precompiled_vector2 = compile_frontend_file("examples/frontend/sequence2.sg");
    let precompiled_aes = compile_frontend_file("examples/frontend/AES.sg");

    group.bench_function("Hashmap (compile)", |b| b.iter(|| compile_frontend_file("examples/frontend/hashmap.sg")));
    group.bench_function("Vector (compile)", |b| b.iter(|| compile_frontend_file("examples/frontend/sequence.sg")));
    group.bench_function("Vector2 (compile)", |b| b.iter(|| compile_frontend_file("examples/frontend/sequence2.sg")));
    group.bench_function("AES (compile)", |b| b.iter(|| compile_frontend_file("examples/frontend/AES.sg")));

    group.bench_function("Hashmap (precompiled run)", |b| b.iter(|| {
        let device = StandardInterpreter::new(TestingDevice::new("hello world!")).run(&precompiled_hashmap).unwrap();
        let output_text = device.output_str();
    }));

    group.bench_function("Vector (precompiled run)", |b| b.iter(|| {
        let device = StandardInterpreter::new(TestingDevice::new("hello world!")).run(&precompiled_vector).unwrap();
        let output_text = device.output_str();
    }));

    group.bench_function("Vector2 (precompiled run)", |b| b.iter(|| {
        let device = StandardInterpreter::new(TestingDevice::new("hello world!")).run(&precompiled_vector2).unwrap();
        let output_text = device.output_str();
    }));

    group.bench_function("AES (precompiled run)", |b| b.iter(|| {
        let device = StandardInterpreter::new(TestingDevice::new("hello world!")).run(&precompiled_aes).unwrap();
        let output_text = device.output_str();
    }));

    group.finish();
}

criterion_group!(benches, bench_frontend);
criterion_main!(benches);
