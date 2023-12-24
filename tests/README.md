# Sage Compiler Tests

This module implements the tests for the Sage compiler.

This is mainly concentrated in [`examples.rs`](examples.rs), which runs all the different frontend, IR, asm, and vm examples in the [`examples`](../examples/README.md) folder, and tests their outputs against the known correct outputs in [`test-output`](../examples/test-output/README.md).