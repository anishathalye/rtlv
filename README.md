**See [anishathalye/knox](https://github.com/anishathalye/knox) for a framework for formally verifying full functional correctness and security of circuits.**

---

# rtlv [![Build Status](https://github.com/anishathalye/rtlv/actions/workflows/ci.yml/badge.svg)](https://github.com/anishathalye/rtlv/actions/workflows/ci.yml)

Tools for reasoning about circuits in [Rosette]/[Racket].

## Collections

### rosutil

Moved to [knox/rosutil](https://github.com/anishathalye/knox).

### yosys

Interpret [Yosys] SMT2 STDT output in Rosette. Moved to [knox/yosys](https://github.com/anishathalye/knox).

### shiva

For proving [deterministic start][notary].

## Setup

Run `raco pkg install` in the top-level directory to install the package.

## Testing

Run `raco test .` to run the tests. When possible, tests should go in the
`test` collection. When writing tests for functionality that is not exported,
they should go in a `test` submodule.

[Rosette]: https://github.com/emina/rosette
[Racket]: https://racket-lang.org/
[Yosys]: https://github.com/YosysHQ/yosys
[notary]: https://github.com/anishathalye/notary
