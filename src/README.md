# The Sage Programming Language

This crate implements a compiler and interpreter for the Sage programming language. [The `main` code for the compiler CLI is located here.](cli.rs)

The Sage LIR (Low Intermediate Representation that lowers to the Sage assembly language) is implemented in the [`lir` module](lir/README.md).

The Sage Assembly language (a minimal language for representing common assembly-like constructs on the brainf***-like Sage virtual machine) is implemented in the [`asm` module](asm/README.md).

The Sage Virtual Machine (the IR for lowering to the target backend) is implemented in the [`vm` module](vm/README.md).

The [`targets` module](targets/README.md) contains all the backend target implementations for the Sage programming language. Each target is a separate module that implements the `CompiledTarget` trait.

The [`frontend` module](frontend/README.md) contains the frontend for the Sage programming language. It is responsible for parsing the frontend source code and generating the LIR. Right now, this module is hacked together until more levels of IR are added and the syntax is stabilized.