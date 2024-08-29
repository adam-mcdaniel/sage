use sage::frontend::nom_parse::compile_and_run;

fn main() {
    
    // env_logger::builder().filter_level(log::LevelFilter::Trace).init();
    match compile_and_run(r#"
module mem {
    extern fun __C_memcpy(dst: &mut Cell, src: &Cell, n: Int);

    fun memcpy<T>(dst: &mut T, src: &T, count: Int) {
        __C_memcpy(dst as &mut Cell, src as &Cell, count * sizeof<T>());
    }
}

let mut x = [1, 2, 3, 4, 5];
let     y = [5, 6, 7, 8, 9];

mem.memcpy<Int>(&mut x,&y, 5);
            "#, "hello!") {
        Ok(expr) => {
            // println!("{:#?}", expr)
            // Compile and run
            println!("{}", expr)
        },
        Err(e) => println!("Error: {}", e),
    }
}