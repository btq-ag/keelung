# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

