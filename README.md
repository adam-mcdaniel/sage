<div align="center">
  <h1>🌿🌱<b>sage</b>🪴🍃</h1>
  <strong>Sage advice for your coding conundrums!</strong>
  <p float="left">
    <a href="https://adam-mcdaniel.net/sage"><img src="./assets/aes.png" width="58%"/></a>
    <a href="https://adam-mcdaniel.net/sage"><img src="./assets/anatomy.svg" width="41.5%"/></a>
  </p>
</div>


[***Here's a link to the online compiler with a builtin virtual machine interpreter!***](https://adam-mcdaniel.net/sage)

## The Compiler Caveat

Compilers are one of the most useful tools ever made. They allow us to abstract our algorithms *way* beyond what computers are natively capable of understanding. For compilers to work, though, *they must be able to describe these high level concepts in terms native to the hardware*. This is a **big** problem: each computer is typically very different from the next. Thermostats, desktop computers, phones, and other systems typically all function differently.

## Enter: Intermediate Representations

Here's where LLVM and similar projects come in. LLVM is a great tool for writing compilers because it ***drastically*** simplifies this problem. Instead of writing a compiler for each different system's hardware, you can just write it to compile to LLVM! LLVM will do the heavy lifting and handle dealing with the specific hardware you want to compile to.

<div align="center">
<img alt="LLVM" align="center" src="assets/llvm.png" width="90%"/>
</div>
</br>

This way, a compiler never has to understand the hardware it compiles to.

## The Intermediate Issues

So, if LLVM can do all this already, *why implement an alterative to do the same thing?* Well, there are a few reasons:

1. Most compiler backends are complex.

If you've ever looked at LLVM, you know that it would be a difficult task to implement LLVM for another target. Much simpler than a C compiler for a target platform, but still complex.

2. System calls are not portable.

System calls are a ***huge*** problem in terms of portability in assembly languages. The way you write some data to an output device is different from platform to platform, and *even worse*, these methods of I/O are ***side-effecting***. *Even if* you can manage to write a character to an output device, every platform's "write" system call likely has different side-effects on the state of the program. ***This is poisonous to portable code***. Communicating with the environment is different from platform to platform, and affects program state in nonuniform ways.

3. Unsupported instructions.

Inevitably, there are going to be instructions in many intermediate representations that can't be compiled to every architecture. This isn't necessarily a problem with *the IR*, its just a side-effect of how IRs typically support complex instructions for their frontends' to take advantage of. An unfortunate consequence is just that certain programs using this IR aren't universally portable. This is truly unavoidable, but it should be minimized.

## A Smaller Solution

In order to solve some of these problems, [I've created another virtual machine](https://xkcd.com/927/) with the following ideals:

1. The instruction set must be able to implement algorithms in a reasonable time complexity. [Addition should ***not*** be O(n)](https://esolangs.org/wiki/Brainfuck_algorithms#x_.3D_x_.2B_y).
2. The instruction set must be **small**. This is to minimize the effort to implement another target for the instruction set.
3. Each instruction must take *at most* one integer argument ***known at compile time***. This is to simplify the individual instructions themselves, which makes optimizations easier.
4. The instruction set must be *agnostic to the concept of bit-width*. Casting between different data types in the virtual machine ***immediately*** increases the complexity of the instruction set: you need different instructions for different types. All data should be represented as a `Cell`, a place to store a single unit of data (typically the largest int size supported by the platform).
5. The instruction set must be able to implement abstractions like functions, pointers, and compound data types.
6. ***Interacting with the environment (the outside world) should not change the state of anything but the register***. 

Without further ado, here's the virtual machine instruction set.

## The Forty and Eight Instructions


The instruction set is composed of two halves of 24 instructions each: [the "Core", canonical instructions](CORE.md#the-core-instructions-in-depth), and the "Standard" instructions.

The canonical instructions are required to be implemented by *every target*. These instructions are guaranteed to be supported by every target.

|              | The              | Twenty  | and        | Four       | Canonical | Instructions |
|--------------|------------------|---------|------------|------------|-----------|--------------|
| Memory       | `Move(n: int)`   | `Index` | `Swap`     | `Where?`   | `Deref`   | `Refer`      |
| Control Flow | `While`          | `If`    | `Else`     | `Function` | `Call`    | `Return`     |
| Arithmetic   | `IsNonNegative?` | `Add`   | `Subtract` | `Multiply` | `Divide`  | `Remainder`  |
| Fundamental  | `Set(n: int)`    | `Save`  | `Restore`  | `Get`      | `Put`     | `End`        |

The standard instructions are not guaranteed to be wholly implemented by every target, or at all. A target for Linux on x86 will certainly implement all the standard instructions, but a thermostat might implement only a few or none.

|                        | The              | Twenty    | and             | Four       | Standard    | Instructions |
|------------------------|------------------|-----------|-----------------|------------|-------------|--------------|
| Memory and Fundamental | `Allocate`       | `Free`    | `Set(n: float)` | `ToInt`    | `ToFloat`   | `Power`      |
| Arithmetic             | `IsNonNegative?` | `Add`     | `Subtract`      | `Multiply` | `Divide`    | `Remainder`  |
| Trigonometry           | `Sine`           | `Cosine`  | `Tangent`       | `ArcSine`  | `ArcCosine` | `ArcTangent` |
| Worldly                | `GetChar`        | `PutChar` | `GetInt`        | `PutInt`   | `GetFloat`  | `PutFloat`   |

Every target should provide a chart of the standard instructions showing which are implemented.

#### Examples

- [Factorial](examples/vm/factorial.vm.sg)
- [Cat](examples/vm/cat.vm.sg)

## The Assembly Language

[***Click here to see the documentation!***](https://adam-mcdaniel.github.io/sage/sage/asm/)

![Assembly Language](assets/ASM.png)

While the virtual machine itself is meant to be as small as possible, the stages of IR built on top of it are meant to be as high level as possible. The assembly language has many instructions: *there are seven different instructions for comparisons!!*

The assembly language is also split into two halves as well: one built on the pure, Core variant, and the other built on the Standard variant. This way, programs can be compiled for maximum portability, but use standard instructions if necessary as a fallback.


### Conventions

Here is the memory layout the assembly language uses on the turing tape.

<div align="center">
<img alt="Memory Layout" align="center" src="assets/memory_layout.svg" width="90%"/>
</div>
</br>

The `FP_STACK` is a pointer to a separate stack than the `SP`: the `FP_STACK` begins directly after the `F` register (when there are no items on the `FP_STACK`, it points to `F`). The size of the `FP_STACK` is provided to the assembler at compile time. The size of the `FP_STACK` determines the number of recursive calls the program may make before the frame pointer stack overflows.

In the assembly language, there are a few arbitrary conventions.
1. The stack pointer always points to the top item on the stack (if you push a tuple of `(1, 2, 3)`, the stack pointer will point to the `3`).
2. The frame pointer is equal to the top of the stack when the current function was called. When the program first starts, and no function has been called, the frame pointer is equal to the stack pointer.
3. Function calls push their frame pointers to a private stack *invisible* to the user. **You cannot use the `FP_STACK` register**, which manages this stack.
4. You can push your arguments however you like. In the IR, the arguments are pushed in the order they're given to the function: `f(1, 2, 3)` pushes `1` first, `2` second, and `3` last.
5. Functions return values by popping the function arguments off of the stack, and pushing the return value. There's no need to worry about overwriting the saved frame pointer, because they're stored in a separate stack.

The assembly instructions always take arguments of: constant integers known at compile time, or values of type `Location`. A `Location` can either be a fixed address (registers), a dereferenced `Location`, or a `Location` offset by some cells. The `Location` `[SP + 4]`, for example, dereferences `SP` and moves four cells to the right (`SP.deref().offset(4)`).

#### Examples

- [Factorial](examples/asm/fact.asm.sg)
- [Cat](examples/asm/cat.asm.sg)
- [Comparisons](examples/asm/comparison.asm.sg)

## The Lower Intermediate Representation

[***Click here to see the documentation!***](https://adam-mcdaniel.github.io/sage/sage/lir/)

![Lower Intermediate Representation](assets/LIR.png)

The lower intermediate representation is the type-checked layer of the compiler; all of the intermediate representations constructed on top of this layer are expressed in terms of LIR code before type-checking. LIR is high level enough to be *decently* human readable and usable. It's a bit more terse than C, but not by much. The type system is very expressive, and allows for unions, enums, procedures (as values), structures, arrays, and anonymous recursive types.

[***Click here to use the interactive compiler online!***](https://adam-mcdaniel.net/sage)

- [128, 192, and 256 bit AES encryption and decryption](examples/lir/AES.lir.sg)
- [Quicksort](examples/lir/quicksort.lir.sg)
- [Factorial](examples/lir/fact.lir.sg)
- [Mutually Recursive Types](examples/lir/recursive-types.lir.sg)
- [Tagged Union](examples/lir/option.lir.sg)
- [Units](examples/lir/units.lir.sg)
- [Inline Assembly](examples/lir/inline-assembly.lir.sg)
- [Simulating Variable Capturing Functions](examples/lir/lambda-sim.lir.sg)

## The Higher Intermediate Representation

🚧 🏗️ ⚠️ This is under construction! ⚠️ 🏗️ 🚧
