use sage::{lir::Compile, parse::*, vm::*};
use std::{
    fs::{read_dir, read_to_string},
    path::PathBuf,
};

const INPUT: &str = "2 4 8 16 32 64 128 256 512 1024 2048 4096";
const CALL_STACK_SIZE: usize = 1024;

#[test]
fn test_lir_examples() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(24 * 1024 * 1024)
        .spawn(test_lir_examples_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_lir_examples_helper() {
    for entry in read_dir("examples/lir/").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        eprintln!("Starting test for `{path:?}`");
        if path.is_file()
            && matches!(
                path.extension().map(|p| p
                    .to_str()
                    .expect("Couldn't get file extension of example code")
                    .as_bytes()),
                Some(b"sg") | None
            )
        {
            let file_name = path
                .file_name()
                .expect(&format!("Could not get file name of path `{path:?}`"))
                .to_str()
                .expect(&format!("Could not get file name of path `{path:?}`"))
                .to_string();
            let correct_output_path = PathBuf::from("examples/output")
                .join(file_name)
                .with_extension("txt");
            let correct_output = match read_to_string(&correct_output_path) {
                Ok(contents) => contents
                    .replace("\r\n", "\n")
                    .as_bytes()
                    .into_iter()
                    .map(|byte| *byte as isize)
                    .collect::<Vec<_>>(),
                Err(_) => {
                    eprintln!("WARNING: Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
            };

            let lir_src = read_to_string(&path)
                .expect(&format!("Could not read contents of file `{path:?}`"));
            let lir_code = parse_lir(&lir_src).expect(&format!("Could not parse `{path:?}`"));
            drop(lir_src);
            let asm_code = lir_code
                .compile()
                .expect(&format!("Could not compile LIR code in `{path:?}`"));

            let vm_code = match asm_code {
                Ok(core_asm_code) => Ok(core_asm_code
                    .assemble(CALL_STACK_SIZE)
                    .expect(&format!("Could not assemble code in `{path:?}`"))),
                Err(std_asm_code) => Err(std_asm_code
                    .assemble(CALL_STACK_SIZE)
                    .expect(&format!("Could not assemble code in `{path:?}`"))),
            };

            let device = match vm_code {
                Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .expect(&format!("Could not interpret code in `{path:?}`")),
                Err(vm_code) => StandardInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .expect(&format!("Could not interpret code in `{path:?}`")),
            };

            if device.output_vals() != correct_output {
                panic!("{:?} != {correct_output:?}, device output did not match correct output for program {path:?}", device.output)
            }
        }
    }
}

#[test]
fn test_asm_examples() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(test_asm_examples_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_asm_examples_helper() {
    for entry in read_dir("examples/asm/").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file()
            && matches!(
                path.extension().map(|p| p
                    .to_str()
                    .expect("Couldn't get file extension of example code")
                    .as_bytes()),
                Some(b"sg") | None
            )
        {
            let file_name = path
                .file_name()
                .expect(&format!("Could not get file name of path `{path:?}`"))
                .to_str()
                .expect(&format!("Could not get file name of path `{path:?}`"))
                .to_string();
            let correct_output_path = PathBuf::from("examples/output")
                .join(file_name)
                .with_extension("txt");
            let correct_output = match read_to_string(&correct_output_path) {
                Ok(contents) => contents
                    .replace("\r\n", "\n")
                    .as_bytes()
                    .into_iter()
                    .map(|byte| *byte as isize)
                    .collect::<Vec<_>>(),
                Err(_) => {
                    eprintln!("WARNING: Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
            };

            let asm_src = read_to_string(&path)
                .expect(&format!("Could not read contents of file `{path:?}`"));
            let asm_code = parse_asm(&asm_src).expect(&format!("Could not parse `{path:?}`"));
            drop(asm_src);

            let vm_code = match asm_code {
                Ok(core_asm_code) => Ok(core_asm_code
                    .assemble(CALL_STACK_SIZE)
                    .expect(&format!("Could not assemble code in `{path:?}`"))),
                Err(std_asm_code) => Err(std_asm_code
                    .assemble(CALL_STACK_SIZE)
                    .expect(&format!("Could not assemble code in `{path:?}`"))),
            };

            let device = match vm_code {
                Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .expect(&format!("Could not interpret code in `{path:?}`")),
                Err(vm_code) => StandardInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .expect(&format!("Could not interpret code in `{path:?}`")),
            };

            if device.output_vals() != correct_output {
                panic!("{:?} != {correct_output:?}, device output did not match correct output for program {path:?}", device.output)
            }
        }
    }
}
