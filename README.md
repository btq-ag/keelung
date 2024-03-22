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
![Roadmap](https://github.com/btq-ag/keelung/assets/797844/ddfedd16-703d-4065-a458-b39ef3105dce)

### Current (v0.21.0)

- A more precise method of reference counting on unsigned integers
- Renaming of primitive datatype conversions
- Tons of bug fixes and stability improvements

### Upcoming (v0.22.0)

- Operators for **slicing** and **joining** unsigned integers
- Internal: improvement of representation of polynomials with our new precise reference counter
- Internal: polynomial insertion is now **9x faster**

### Future


- **Snarkjs/Circom Integration**: _Snarkjs/Circom_ is currently the most popular ZK toolchain. We aim to tap into this ecosystem by integrating Keelung with Snarkjs/Circom.
- **Standard Library Optimization**: We plan to heavily optimize ciphers and hash functions such as _AES_ and _Blake2_.
- **PLONKish Backend Compilation Support**: Supporting protocols like _PLONK_ would allow Keelung programs to efficiently manage complex privacy-preserving computations, significantly enhancing their scalability and security across various applications.
- **Quantitative Information Flow Analysis**: We are working with [Prof. Kohei Suenaga](https://www.fos.kuis.kyoto-u.ac.jp/~ksuenaga/) of Kyoto University to enable privacy leakage analysis of Keelung programs.
- **DSL Refinement**: We are transitioning statement operators (e.g. `performDivMod`, which is only allowed in the `Comp` monad) to pure operators to make these operations more versatile and easily integrated into different parts of our system.
- **Performance Improvement**: We aim to speed up the compilation of operators on commonly used types such as Bytes, Word16, Word32, and Word64.
- **Benchmarking CI/CD**: We plan to include benchmarking in the CI/CD pipeline.
