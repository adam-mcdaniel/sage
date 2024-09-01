use sage::frontend::nom_parse::*;

fn main() {
    // env_logger::builder().filter_level(log::LevelFilter::Trace).init();
    // env_logger::builder().filter_level(log::LevelFilter::Debug).init();
    env_logger::builder().filter_level(log::LevelFilter::Info).init();
    match compile_and_run(r#"
module std {
    module io {
        fun putln<T>(x: T) {
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
    }

    module testing {
        from io import putln;

        const TESTING = 5;

        struct Point {
            x: Float,
            y: Float,
        }
        
        impl Point {
            fun new(x: Float, y: Float): Point {
                return {x=x, y=y};
            }
        
            fun move(&mut self, dx: Float, dy: Float) {
                self.x += dx;
                self.y += dy;
            }
        }
        
        fun test() {
            println(TESTING);
        }
    }

    module math {
        from testing import TESTING, test;

        fun cos(x: Float): Float {
            println(TESTING, " from math");
            test();
            return 1.0 - x * x / 2.0 + x * x * x * x / 24.0;
        }

        fun sin(x: Float): Float {
            return x - x * x * x / 6.0 + x * x * x * x * x / 120.0;
        }

        fun tan(x: Float): Float {
            return x + x * x * x / 3.0 + 2.0 * x * x * x * x * x / 15.0;
        }
    }

}

from std.io import putln as p;
from std.testing import Point, test;

fun main() {
    let _ = std.math.cos(0.0);
    test();
    
    p<Char>('!');
    let mut x = Point.new(5.0, 6.0);
    p<Point>(x);
    x.move(1.0, 2.0);
    
    p<Point>(x);
    
    test();
}

main();
"#, "hello!!!!") {
        Ok(expr) => {
            // println!("{:#?}", expr)
            // Compile and run
            println!("{}", expr)
        },
        Err(e) => println!("Error: {}", e),
    }
}