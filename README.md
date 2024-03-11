# Keelung

Keelung is a domain-specific language (DSL) embedded in Haskell that is designed for creating zero-knowledge proofs (ZKPs). With Keelung, developers can create high-quality and reliable ZKPs with ease, even without expertise in cryptography.

<img width="1268" alt="keelung5" src="https://user-images.githubusercontent.com/97019448/219329651-d30c0134-f153-4755-979f-c5b77d6d2724.png">

## The Keelung Compiler

Keelung programs are compiled with [the Keelung compiler](https://github.com/btq-ag/keelung-compiler).

## Documentation

Check out our [GitBook](https://btq.gitbook.io/keelung/) for more information.

## Language Reference

The language reference is now available on [Hackage](https://hackage.haskell.org/package/keelung).

## Standard Library

The standard library is available [here](https://github.com/btq-ag/keelung-stdlib).

## Discord
Find us on [Discord](https://discord.gg/nTr2kBmW2q)!

## Codespaces
Get started using Keelung in GitHub codespaces by navigating to `Code` -> `Codespaces` in [Keelung Examples](https://github.com/btq-ag/keelung-examples/)!

## Roadmap 

<img width="2500" alt="Keelung operators" src="https://github.com/btq-ag/keelung/assets/797844/6dc6893f-5d97-428d-80a1-03fae730a81d">

### Current (v0.21.0)

- A more precise method of reference counting on unsigned integers
- Renaming of primitive datatype conversions
- Tons of bug fixes and stability improvements

### Upcoming (v0.22.0)

- Operators for **slicing** and **joining** unsigned integers
- Internal: improvement of representation of polynomials with our new precise reference counter

### Future

- Optimization for the implementation of AES
- Make statement operators (e.g. `performDivMod`, which is only allowed in the `Comp` monad) to pure operators
- Snarkjs/Circom integration
- Optimization for the implementation of BLAKE2
- Speedup compilation of operators on Bytes, Word16, Word32, and Word64
- Include benchmarking in the CI/CD pipeline
- Support PLONKish backend compilation
- Quantitative information flow analysis for measuring privacy leak of programs (working with [Prof. Kohei Suenaga](https://www.fos.kuis.kyoto-u.ac.jp/~ksuenaga/) of Kyoto University)
