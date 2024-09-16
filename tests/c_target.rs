use log::warn;
use sage::{lir::Compile, parse::*, targets::*};
use std::{
    fs::{read_dir, read_to_string},
    io::Write,
    path::PathBuf,
};

const INPUT: &str = "2 4 8 16 32 64 128 256 512 1024 2048 4096";
const CALL_STACK_SIZE: usize = 8192;

#[test]
fn test_c_target_frontend_examples() {
    let mut builder = env_logger::Builder::from_default_env();
    builder.format_timestamp(None);
    builder.filter(
        None,
        // LogLevel::Error if args.debug.is_none() => log::LevelFilter::Error,
        // LogLevel::Warn if args.debug.is_none() => log::LevelFilter::Warn,
        // LogLevel::Off if args.debug.is_none() => log::LevelFilter::Error,
        // LogLevel::Info if args.debug.is_none() => log::LevelFilter::Info,
        // LogLevel::Trace => log::LevelFilter::Trace,
        log::LevelFilter::Info,
    );
    builder.init();

    rayon::ThreadPoolBuilder::new()
        .num_threads(16)
        .stack_size(512 * 1024 * 1024)
        .build_global()
        .unwrap();
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(512 * 1024 * 1024)
        .spawn(test_c_target_frontend_examples_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_c_target_frontend_examples_helper() {
    let mut total_failures: i32 = 0;
    let mut total_attempts = 0;

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
                .join(file_name.clone())
                .with_extension("error.txt");
            let correct_error = match read_to_string(&correct_error_path) {
                Ok(contents) => Some(contents.replace("\r\n", "\n")),
                Err(_) => None,
            };
            let correct_output_text = match read_to_string(&correct_output_path) {
                Ok(contents) => contents.replace("\r\n", "\n"),
                Err(_) if correct_error.is_none() => {
                    warn!("Could not read output text file `{correct_output_path:?}` to compare against. Skipping this test.");
                    continue;
                }
                Err(_) => String::new(),
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
            let asm_code = frontend_code.compile(false);

            if let Err(ref e) = asm_code {
                if let Some(correct_error) = correct_error {
                    let text = e.to_string();
                    if text != correct_error {
                        panic!("{text:?} (incorrect) != {correct_error:?} (correct), error did not match correct error for program {path:?}")
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
            }
            .unwrap();

            let c_code = match vm_code {
                Ok(vm_code) => C.build_core(&vm_code.flatten()).unwrap(),
                Err(vm_code) => C.build_std(&vm_code.flatten()).unwrap(),
            };

            // Write the C code to a file.
            let c_code_path = format!("tmp_c_code_{file_name}.c");
            std::fs::write(&c_code_path, c_code).unwrap();

            // Compile the C code.
            let c_exe_path = format!("tmp_c_code_{file_name}.exe");
            let c_compile_output = std::process::Command::new("gcc")
                .arg(&c_code_path)
                .arg("-o")
                .arg(&c_exe_path)
                .arg("-lm")
                .output()
                .unwrap();

            if !c_compile_output.status.success() {
                panic!("Could not compile C code for `{path:?}`: {c_compile_output:?}");
            }

            // Run the C code with the input, and confirm that the output matches the expected output.
            let stdin = std::process::Stdio::piped();
            let stdout = std::process::Stdio::piped();
            let mut c_exe = match std::process::Command::new(&format!("./{c_exe_path}"))
                .stdin(stdin)
                .stdout(stdout)
                .spawn()
            {
                Ok(v) => v,
                Err(e) => panic!("Could not run C code for `{path:?}`: {e}"),
            };

            if c_exe
                .stdin
                .as_mut()
                .unwrap()
                .write_all(INPUT.as_bytes())
                .is_err()
            {
                panic!("Could not write to stdin of program `{path:?}`");
            }

            // Get stdout from the C program.
            let c_output = c_exe.wait_with_output().unwrap().stdout;
            // Convert both to strings
            let correct_output = correct_output
                .iter()
                .map(|byte| *byte as u8)
                .collect::<Vec<_>>();

            let c_output = String::from_utf8(c_output).unwrap();
            let correct_output = String::from_utf8(correct_output).unwrap();

            if c_output != correct_output {
                warn!(
                    "Output did not match correct output for program `{path:?}`:\n{c_output}\n  !=   \n{correct_output}"
                );
                total_failures += 1;
            }
            total_attempts += 1;

            // Remove the C code and executable.
            std::fs::remove_file(&c_code_path).unwrap();
            std::fs::remove_file(&c_exe_path).unwrap();
        }
    }

    // We just use a threshold of 30% failure, because the frontend examples print out pointers
    // And the frontend examples all print pointers differently. Same for floats.
    if total_failures as f64 / total_attempts as f64 > 0.3 {
        panic!(
            "Too many failures in frontend examples: {total_failures} failures out of {total_attempts} attempts"
        );
    }
}
