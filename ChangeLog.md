# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.21.0]

### Added

* `fromBools` and `fromField` for replacing `pack` and `toUInt` in the future.

### Changed

* Unsigned integers used to be see as a whole, that means if any of the bits was used, the whole integer will be retained. Now, the compiler will only retain the bits that are used.

## [v0.11.0]

### Added

* Added `pow` operator for calculating exponentiation of field elements. This operator allow for exponentiation by squaring, which is more efficient than repeated multiplication.
* Added helpers like `gf181`, `bn128`, and `b64` for constructing field type information.

### Removed

* Removed fixed field type constructors like `GF181`, `BN128`, and `B64`.

### Fixed 

* [Issue #15](https://github.com/btq-ag/keelung/issues/15): Allow users to use any field they like.
* Compilation of chained conjunection (more than 3) on `UInt`s.

## [v0.10.0]

### Added

* Added `lte`, `lt`, `gte`, and `gt` operators for comparing `UInt`s. These operators can be used in conjunction with `assert` to create assertions about ranges of `UInt`s.

### Fixed 

* Addressed miscellaneous issues with the optimizer.

## [v0.9.5]

### Added

* `assertLTE`, `assertLT`, `assertGTE`, and `assertGT`: assertions for comparing a `UInt` with some constant.

### Fixed 

* Addressed miscellaneous issues with the compiler.

## [v0.9.4]

### Added

* `assertLTE` for asserting that a `UInt` is less than or equal to some constant.
* `modInv` for computing the modular inverse of a `UInt`.

### Fixed 

* [Issue #13](https://github.com/btq-ag/keelung/issues/13): Tutorial Doesn't Load Types

## [v0.9.3]

### Fixed 

* Compilation of addition, multiplication and not-equal-to on Unsigned Integers.
* A bunch of problems with the interpreter.

## [v0.9.2]

### Fixed 

* [Issue #11](https://github.com/btq-ag/keelung/issues/11): Cannot build with Cabal

## [v0.9.1]

### Added

* Docker support for the compiler.

## [v0.9.0]

### Changed

* Search for `aurora_prove` and `aurora_verify` in the environment instead.

### Removed

* Temporarily removed Docker support.

## [v0.8.4]

### Added 

* `Keelung.Syntax.Monad`
    * Functions for requesting fresh varibles: `freshVar`, `freshVarField`, `freshVarBool`, and `freshVarUInt`.

### Removed

* `Keelung.Syntax`
    * Function `uintToBool`
    * Function `fieldToBool`

