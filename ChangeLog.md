# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

