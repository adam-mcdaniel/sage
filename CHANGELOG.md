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