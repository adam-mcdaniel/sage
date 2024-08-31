use sage::frontend::nom_parse::*;

fn main() {
    env_logger::builder().filter_level(log::LevelFilter::Trace).init();
    match compile_and_run(r#"
    
module std {
    module math {
        fun cos(x: Float): Float {
            return 1.0 - x * x / 2.0 + x * x * x * x / 24.0;
        }

        fun sin(x: Float): Float {
            return x - x * x * x / 6.0 + x * x * x * x * x / 120.0;
        }

        fun tan(x: Float): Float {
            return x + x * x * x / 3.0 + 2.0 * x * x * x * x * x / 15.0;
        }
    }

    module io {
        fun println<T>(x: T) {
            print(x);
            print('\n');
        }
    
        fun putchar(c: Char) {
            print(c);
        }

        fun getchar(): Char {
            let mut ch = ' ';
            input(&mut ch);
            return ch;
        }

        impl Char {
            fun put(&self) {
                putchar(*self);
            }

            fun get(): Char {
                return getchar();
            }
        }

        fun test() {
            println<Char>(Char.get());
        }
    }
}


std.io.println<Char>(Char.get());

for let mut i=0; i<5; i+=1; {
    std.io.test();
}

            "#, "hello!") {
        Ok(expr) => {
            // println!("{:#?}", expr)
            // Compile and run
            println!("{}", expr)
        },
        Err(e) => println!("Error: {}", e),
    }
}