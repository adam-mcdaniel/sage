use sage::{lir::Compile, parse::*, vm::*};
use std::{
    fs::{read_dir, read_to_string},
    path::PathBuf,
};

use log::warn;

const INPUT: &str = "2 4 8 16 32 64 128 256 512 1024 2048 4096";
const CALL_STACK_SIZE: usize = 8192;

#[test]
fn test_frontend_examples() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(128 * 1024 * 1024)
        .spawn(test_frontend_examples_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_frontend_examples_helper() {
    for entry in read_dir("examples/frontend/").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        warn!("Starting test for `{path:?}`");
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
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_str()
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_string();
            let correct_output_path = PathBuf::from("examples/test-output")
                .join(file_name.clone())
                .with_extension("txt");
            let correct_error_path = PathBuf::from("examples/test-output")
                .join(file_name)
                .with_extension("error.txt");
            let correct_error = match read_to_string(&correct_error_path) {
                Ok(contents) => Some(contents.replace("\r\n", "\n")),
                Err(_) => None
            };
            let correct_output_text = match read_to_string(&correct_output_path) {
                Ok(contents) => contents.replace("\r\n", "\n"),
                Err(_) if correct_error.is_none() => {
                    warn!("Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
                Err(_) => String::new()
            };
            let correct_output = correct_output_text
                .as_bytes()
                .iter()
                .map(|byte| *byte as i64)
                .collect::<Vec<_>>();


            let frontend_src = read_to_string(&path)
                .unwrap_or_else(|_| panic!("Could not read contents of file `{path:?}`"));
            let frontend_code = parse_frontend(&frontend_src, path.to_str())
                .unwrap_or_else(|_| panic!("Could not parse `{path:?}`"));
            drop(frontend_src);
            let asm_code = frontend_code.compile();
            
            if let Err(ref e) = asm_code {
                if let Some(correct_error) = correct_error {
                    let text = e.to_string();
                    if text != correct_error {
                        panic!("{text:?} != {correct_error:?}, error did not match correct error for program {path:?}")
                    } else {
                        continue;
                    }
                } else {
                    panic!("Could not assemble code in `{path:?}`: {e}")
                }
            }
            let asm_code = asm_code.unwrap();

            let vm_code = match asm_code {
                Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
                Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
            }.unwrap();

            let device = match vm_code {
                Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                Err(vm_code) => StandardInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
            };

            let output_text = device.output_str();
            if device.output_vals() != correct_output {
                panic!("{output_text:?} != {correct_output_text:?}, device output did not match correct output for program {path:?}")
            }

            if let Some(correct_error) = correct_error {
                panic!("Expected error `{correct_error:?}` but got output `{output_text:?}` for program `{path:?}`")
            }
        }
    }
}

#[test]
fn test_lir_examples() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(test_lir_examples_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_lir_examples_helper() {
    for entry in read_dir("examples/lir/").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        warn!("Starting test for `{path:?}`");
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
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_str()
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_string();
            let correct_output_path = PathBuf::from("examples/test-output")
                .join(file_name.clone())
                .with_extension("txt");
            let correct_error_path = PathBuf::from("examples/test-output")
                .join(file_name)
                .with_extension("error.txt");

            let correct_error = match read_to_string(&correct_error_path) {
                Ok(contents) => Some(contents.replace("\r\n", "\n")),
                Err(_) => None
            };
            let correct_output_text = match read_to_string(&correct_output_path) {
                Ok(contents) => contents.replace("\r\n", "\n"),
                Err(_) if correct_error.is_none() => {
                    warn!("Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
                Err(_) => String::new()
            };
            let correct_output = correct_output_text
                .as_bytes()
                .iter()
                .map(|byte| *byte as i64)
                .collect::<Vec<_>>();

            let lir_src = read_to_string(&path)
                .unwrap_or_else(|_| panic!("Could not read contents of file `{path:?}`"));
            let lir_code =
                parse_lir(&lir_src)
                .unwrap_or_else(|_| panic!("Could not parse `{path:?}`"));
            drop(lir_src);
            let asm_code = lir_code.compile();
            
            if let Err(ref e) = asm_code {
                let text = e.to_string();
                if let Some(correct_error) = correct_error {
                    if text != correct_error {
                        panic!("{text:?} != {correct_error:?}, error did not match correct error for program {path:?}")
                    } else {
                        continue;
                    }
                } else {
                    panic!("Could not assemble code in `{path:?}`: {text:?}")
                }
            }
            let asm_code = asm_code.unwrap();

            let vm_code = match asm_code {
                Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
                Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
            }.unwrap();

            let device = match vm_code {
                Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                Err(vm_code) => StandardInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
            };

            let output_text = device.output_str();
            if device.output_vals() != correct_output {
                panic!("{output_text:?} != {correct_output_text:?}, device output did not match correct output for program {path:?}")
            }

            if let Some(correct_error) = correct_error {
                panic!("Expected error `{correct_error:?}` but got output `{output_text:?}` for program `{path:?}`")
            }
        }
    }
}

#[test]
fn test_asm_examples() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
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
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_str()
                .unwrap_or_else(|| panic!("Could not get file name of path `{path:?}`"))
                .to_string();
            let correct_output_path = PathBuf::from("examples/test-output")
                .join(file_name.clone())
                .with_extension("txt");
            let correct_error_path = PathBuf::from("examples/test-output")
                .join(file_name)
                .with_extension("error.txt");

            let correct_error = match read_to_string(&correct_error_path) {
                Ok(contents) => Some(contents.replace("\r\n", "\n")),
                Err(_) => None
            };
            let correct_output_text = match read_to_string(&correct_output_path) {
                Ok(contents) => contents.replace("\r\n", "\n"),
                Err(_) if correct_error.is_none() => {
                    warn!("Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
                Err(_) => String::new()
            };
            let correct_output = correct_output_text
                .as_bytes()
                .iter()
                .map(|byte| *byte as i64)
                .collect::<Vec<_>>();

            let asm_src = read_to_string(&path)
                .unwrap_or_else(|_| panic!("Could not read contents of file `{path:?}`"));
            let asm_code =
                parse_asm(&asm_src).unwrap_or_else(|_| panic!("Could not parse `{path:?}`"));
            drop(asm_src);

            let vm_code = match asm_code {
                Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
                Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
            };

            if let Err(ref e) = vm_code {
                let text = e.to_string();
                if let Some(correct_error) = correct_error {
                    if text != correct_error {
                        panic!("{text:?} != {correct_error:?}, error did not match correct error for program {path:?}")
                    } else {
                        continue;
                    }
                } else {
                    panic!("Could not assemble code in `{path:?}`: {text:?}")
                }
            }

            let vm_code = vm_code.unwrap();

            let device = match vm_code {
                Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                Err(vm_code) => StandardInterpreter::new(TestingDevice::new(INPUT))
                    .run(&vm_code)
                    .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
            };

            let output_text = device.output_str();
            if device.output_vals() != correct_output {
                panic!("{output_text:?} != {correct_output_text:?}, device output did not match correct output for program {path:?}")
            }

            if let Some(correct_error) = correct_error {
                panic!("Expected error `{correct_error:?}` but got output `{output_text:?}` for program `{path:?}`")
            }
        }
    }
}
