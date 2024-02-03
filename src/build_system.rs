use std::collections::VecDeque;
use std::fmt::Pointer;
use maplit::btreemap;

use sage::side_effects::FFIBinding;
use sage::*;
use sage::lir::*;
use sage::targets::*;

fn main() {
    let code = r#"
        extern def deref(x: &Int);

        let x = 5;
        deref(&x);
    "#;

    use lir::Type::*;

    println!("{}", eval(code, "Testing", vec![
        (
            ("deref", Pointer(Mutability::Any, Any.into()), None),
            |channel, tape| {
                let a = channel.pop_front().unwrap() as usize;
                println!("output: {}", tape[a]);
            }
        ),

        (
            // A function add that takes two Ints and returns an Int
            ("add", Tuple(vec![Int, Int]), Int),
            // The function body
            |channel, _| {
                let a = channel.pop_front().unwrap();
                let b = channel.pop_front().unwrap();
                channel.push_back(a + b);
            }
        ),

        (
            // A function sub that takes two Ints and returns an Int
            ("sub", Tuple(vec![Int, Int]), Int),
            // The function body
            |channel, _| {
                let a = channel.pop_front().unwrap();
                let b = channel.pop_front().unwrap();
                channel.push_back(a - b);
            }
        )
    ]).unwrap());
}