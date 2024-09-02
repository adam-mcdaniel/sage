use sage::frontend::nom_parse::*;

fn main() {
    // env_logger::builder().filter_level(log::LevelFilter::Trace).init();
    // env_logger::builder().filter_level(log::LevelFilter::Debug).init();
    // env_logger::builder().filter_level(log::LevelFilter::Info).init();
    match compile_and_run(r#"
"#, "hello!!!!") {
        Ok(expr) => {
            // println!("{:#?}", expr)
            // Compile and run
            println!("{}", expr)
        },
        Err(e) => println!("Error: {}", e),
    }
}