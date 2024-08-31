
// module std {
//     module math {
//         fun cos(x: Float): Float {
//             return 1.0 - x * x / 2.0 + x * x * x * x / 24.0;
//         }

//         fun sin(x: Float): Float {
//             return x - x * x * x / 6.0 + x * x * x * x * x / 120.0;
//         }

//         fun tan(x: Float): Float {
//             return x + x * x * x / 3.0 + 2.0 * x * x * x * x * x / 15.0;
//         }
//     }

//     module io {
//         fun println<T>(x: T) {
//             print(x);
//             print('\n');
//         }
    
//         fun putchar(c: Char) {
//             print(c);
//         }

//         fun getchar(): Char {
//             let mut ch = ' ';
//             input(&mut ch);
//             return ch;
//         }

//         impl Char {
//             fun put(&self) {
//                 putchar(*self);
//             }

//             fun get(): Char {
//                 return getchar();
//             }
//         }

//         fun test() {
//             println<Char>(Char.get());
//         }

//         struct Point {
//             x: Float,
//             y: Float,
//         }

//         impl Point {
//             fun new(x: Float, y: Float): Point {
//                 return {x=x, y=y};
//             }

//             fun move(&mut self, dx: Float, dy: Float) {
//                 self.x += dx;
//                 self.y += dy;
//             }
//         }
//     }
// }

// from std import io;
// from io import Point;
// // from io import test;

// let mut p = Point.new(5.0, 6.0);
// p.move(1.0, 2.0);

// // test();

// std.io.println<Char>(Char.get());

// for let mut i=0; i<5; i+=1; {
//     std.io.test();
// }

pub mod standard {
    pub mod math {
        fn cos(x: f32) -> f32 {
            return 1.0 - x * x / 2.0 + x * x * x * x / 24.0;
        }

        fn sin(x: f32) -> f32 {
            return x - x * x * x / 6.0 + x * x * x * x * x / 120.0;
        }

        fn tan(x: f32) -> f32 {
            return x + x * x * x / 3.0 + 2.0 * x * x * x * x * x / 15.0;
        }
    }

    pub mod io {
        // fn println<T>(x: T) {
        //     print(x);
        //     print('\n');
        // }
    
        pub fn putchar(c: char) {
            print!("{}", c);
        }

        pub fn getchar() -> char {
            let mut ch = ' ';
            // input(&mut ch);
            return ch;
        }

        // impl char {
        //     fn put(&self) {
        //         putchar(*self);
        //     }

        //     fn get() -> char {
        //         return getchar();
        //     }
        // }

        pub fn test() {
            // println<char>(char::get());
            print!("{}", getchar());
        }

        pub struct Point {
            x: f32,
            y: f32,
        }

        impl Point {
            pub fn new(x: f32, y: f32) -> Point {
                return Point {x: x, y: y};
            }

            pub fn shift(&mut self, dx: f32, dy: f32) {
                self.x += dx;
                self.y += dy;
            }
        }
    }
}

fn main() {
    let mut p = standard::io::Point::new(5.0, 6.0);
    p.shift(1.0, 2.0);

    standard::io::test();

    // for let mut i=0; i<5; i+=1; {
    //     standard::io::test();
    // }
    for i in 0..5 {
        standard::io::test();
    }
}