# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

*No unreleased changes yet*

## [0.0.0-alpha] - 2022-08-20

### Added

### Changed

### Fixed

## [0.0.1-alpha] - 2023-06-14

### Added

Added stable LIR frontend with type checker and polymorphic types.

### Changed

Expanded standard instruction set.

### Fixed

## [0.0.2-alpha] - 2023-12-20

### Added

Added methods to types, associated constants, and expanded support for foreign functions.

### Changed

Changed type system to accommodate methods.

### Fixed

## [0.0.3-alpha] - 2023-12-27

### Added

### Changed

### Fixed

Fixed bug where unary `-` operator did not type-check on `Float` values.

## [0.0.4-alpha] - 2024-3-22

### Added

Support for typechecking expressions in parallel. Support for SIMD vector instructions.

### Changed

All the VM instructions now take an optional "size" constant parameter which determines the size of the vector they're operating on. The register is now a vector, not a scalar.

Removed `sage-os` and `x86` targets. `x86` will be added back later, but likely using the `C` target as an intermediate step to produced better optimized `x86`.

New VM instructions:
- `Inc`
- `Dec`
- `Offset`
- `And`
- `Or`
- `Not`
- `BitwiseAnd`
- `BitwiseOr`
- `BitwiseXor`
- `BitwiseNot`

### Fixed

Generated code size is now up to 7x smaller than before. 

## [0.1.0-alpha] - 2024-9-4

Sage has just gotten a major upgrade, with a new module system📦 parser📝 and the Sage Lisp Preprocessor🛸!!!

### Added

- [Sage lisp compile time preprocessor language](https://github.com/adam-mcdaniel/sage-lisp)
```rs
// Define some code that modifies the AST of the expression it's called on
#![(defun square_sage_const(x) {
    // Get a constant integer
    (define n x@"ConstExpr"@"Int")
    // Return the constant integer squared
    ["ConstExpr" ["Float" n * n]]
})]

// Call the compile time macro on 5.
// This will cause the compiler to perceive this code as `println(25)`
println(#[square_sage_const] 5);
```

- Module system

```rs
// Import a module from a file named `from_file.sg`
mod from_file;

// Define a module `io` with `getchar`
mod io {
    fun getchar(): Char {
        let mut ch = '\0';
        input(&mut ch);
        return ch;
    }
}

// A module to test importing from other modules
mod testing {
    mod internals {
        // Import getchar from two modules up
        from io import getchar;
        
        // Use it in a function
		fun get_two(): (Char, Char) { (getchar(), getchar()) }
    }

    // Get two chars and print them
    fun get_two_then_print() {
        // Import from a submodule, which imports from a supermodule
        from internals import get_two;
        let (a, b) = get_two();
        print(a, b);
    }
}

from testing import get_two_then_print;
get_two_then_print();

// Get two values and print them
for let mut i=0; i<6; i+=1; {
    // We can also use the full path name
 	testing.get_two_then_print();
}
```

- Standard library

```rs
from std.fallible import Option, Result;

enum Error {
    DivideByZero { numerator: Int },
    Custom(&Char)
}

fun divide(n: Int, d: Int): Option<Int> {
    match d {
        0 => Option<Int> of Nothing,
        _ => Option<Int> of Some(n / d)
    }
}


fun main(): Result<(), Error> {
    println(divide(5, 2));
    println(divide(5, 0));

    return Result<(), Error> of Ok(());
}

println(main());
```

- Better constant evaluation
- `from module.submodule import x, y as alias_for_y;` import statements
- `from module.submodule import *` import statements

### Changed

- The `def` keyword has been changed in favor of `fun`.
- `for` loops now have an extra semicolon on the last statement. `i += 1 {` becomes `i += 1; {`.

### Fixed

The compiler is significantly faster -- about 20 times faster at compiling the AES example.
This is mainly due to the much faster parser implemented with Nom instead of Pest.
There are also several optimizations with constant evaluation I added, along with optimizations
for how declarations are typechecked.