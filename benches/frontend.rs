use criterion::{criterion_group, criterion_main, Criterion};
use sage::{
    lir::*,
    parse::*,
    targets::{self, CompiledTarget},
    vm::*,
};
use std::fs::{read_to_string, write};

const CALL_STACK_SIZE: usize = 8192;

fn read_file(filename: &str) -> String {
    read_to_string(filename).unwrap()
}

fn parse_frontend_file(filename: &str) -> Expr {
    parse_frontend(read_file(filename), Some(filename)).unwrap()
}

fn compile_frontend_file(filename: &str) -> StandardProgram {
    match parse_frontend_file(filename).compile().unwrap() {
        // If we got back a valid program, assemble it and return the result.
        Ok(asm_code) => asm_code.assemble(CALL_STACK_SIZE).unwrap().into(),
        Err(asm_code) => asm_code.assemble(CALL_STACK_SIZE).unwrap(),
    }
}

fn compile_to_c(filename: &str) -> String {
    let program = compile_frontend_file(filename);
    let c_code = targets::C.build_std(&program).unwrap();
    return c_code;
}

fn compile_with_gcc(filename: &str) {
    let c_code = compile_to_c(filename);
    write("benches/temp.c", c_code).unwrap();
    let output = std::process::Command::new("gcc")
        .arg("benches/temp.c")
        .arg("-O3")
        .arg("-o")
        .arg("benches/temp")
        // .arg("-fwhole-program")
        // .arg("-floop-interchange")
        // .arg("-ftree-loop-linear")
        // .arg("-floop-block")
        // .arg("-fcheck-data-deps")
        // .arg("-ftree-loop-distribution")
        // .arg("-ftree-vect-loop-version")
        // .arg("-ffp-contract=fast")
        // .arg("-funsafe-math-optimizations")
        // .arg("-fassociative-math")
        // .arg("-Wstrict-aliasing=3")
        // .arg("-fipa-strict-aliasing")
        // Flags: -O3 -fwhole-program -floop-interchange -ftree-loop-linear -floop-block -fcheck-data-deps -ftree-loop-distribution -ftree-vect-loop-version -ffp-contract=fast -funsafe-math-optimizations -fassociative-math -Wstrict-aliasing=3 -fipa-strict-aliasing
        .output()
        .expect("failed to execute process");
    // Remove the temp file
    std::fs::remove_file("benches/temp.c").unwrap();
    println!("{}", String::from_utf8_lossy(&output.stdout));
    println!("{}", String::from_utf8_lossy(&output.stderr));
}

/// Get output from a program compiled with GCC
fn run_with_gcc(_filename: &str, input: &str) -> String {
    use std::io::Write;

    // Feed input through stdin
    let mut child = std::process::Command::new("benches/temp")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("failed to execute process");

    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    // Remove the temp file
    return String::from_utf8_lossy(&output.stdout).to_string();
}

fn run_frontend_file(filename: &str, input: &str) -> String {
    let program = compile_frontend_file(filename);

    let device = StandardInterpreter::new(TestingDevice::new(input))
        .run(&program)
        .unwrap();
    let output_text = device.output_str();

    return output_text;
}

fn bench_frontend(c: &mut Criterion) {
    let mut group = c.benchmark_group("GCC Backend");
    group.sample_size(50);

    compile_with_gcc("examples/frontend/hashmap.sg");
    group.bench_function("Hashmap (GCC precompiled run)", |b| {
        b.iter(|| {
            run_with_gcc("examples/frontend/hashmap.sg", "hello world!");
        })
    });

    compile_with_gcc("examples/frontend/sequence.sg");
    group.bench_function("Vector (GCC precompiled run)", |b| {
        b.iter(|| {
            run_with_gcc("examples/frontend/sequence.sg", "hello world!");
        })
    });

    compile_with_gcc("examples/frontend/sequence2.sg");
    group.bench_function("Vector2 (GCC precompiled run)", |b| {
        b.iter(|| {
            run_with_gcc("examples/frontend/sequence2.sg", "hello world!");
        })
    });

    compile_with_gcc("examples/frontend/AES.sg");
    group.bench_function("AES (GCC precompiled run)", |b| {
        b.iter(|| {
            run_with_gcc("examples/frontend/AES.sg", "hello world!");
        })
    });

    std::fs::remove_file("benches/temp").unwrap();
    group.finish();

    let mut group = c.benchmark_group("VM Backend");
    group.sample_size(10);

    group.bench_function("Hashmap (compile + VM run)", |b| {
        b.iter(|| run_frontend_file("examples/frontend/hashmap.sg", "hello world!"))
    });
    group.bench_function("Vector (compile + VM run)", |b| {
        b.iter(|| run_frontend_file("examples/frontend/sequence.sg", "hello world!"))
    });
    group.bench_function("Vector2 (compile + VM run)", |b| {
        b.iter(|| run_frontend_file("examples/frontend/sequence2.sg", "hello world!"))
    });
    group.bench_function("AES (compile + VM run)", |b| {
        b.iter(|| run_frontend_file("examples/frontend/AES.sg", "hello world!"))
    });

    let precompiled_hashmap = compile_frontend_file("examples/frontend/hashmap.sg");
    let precompiled_vector = compile_frontend_file("examples/frontend/sequence.sg");
    let precompiled_vector2 = compile_frontend_file("examples/frontend/sequence2.sg");
    let precompiled_aes = compile_frontend_file("examples/frontend/AES.sg");

    group.bench_function("Hashmap (compile)", |b| {
        b.iter(|| compile_frontend_file("examples/frontend/hashmap.sg"))
    });
    group.bench_function("Vector (compile)", |b| {
        b.iter(|| compile_frontend_file("examples/frontend/sequence.sg"))
    });
    group.bench_function("Vector2 (compile)", |b| {
        b.iter(|| compile_frontend_file("examples/frontend/sequence2.sg"))
    });
    group.bench_function("AES (compile)", |b| {
        b.iter(|| compile_frontend_file("examples/frontend/AES.sg"))
    });

    group.bench_function("Hashmap (VM precompiled run)", |b| {
        b.iter(|| {
            StandardInterpreter::new(TestingDevice::new("hello world!"))
                .run(&precompiled_hashmap)
                .unwrap();
        })
    });

    group.bench_function("Vector (VM precompiled run)", |b| {
        b.iter(|| {
            StandardInterpreter::new(TestingDevice::new("hello world!"))
                .run(&precompiled_vector)
                .unwrap();
        })
    });

    group.bench_function("Vector2 (VM precompiled run)", |b| {
        b.iter(|| {
            StandardInterpreter::new(TestingDevice::new("hello world!"))
                .run(&precompiled_vector2)
                .unwrap();
        })
    });

    group.bench_function("AES (VM precompiled run)", |b| {
        b.iter(|| {
            StandardInterpreter::new(TestingDevice::new("hello world!"))
                .run(&precompiled_aes)
                .unwrap();
        })
    });
    group.finish();
}

criterion_group!(benches, bench_frontend);
criterion_main!(benches);
