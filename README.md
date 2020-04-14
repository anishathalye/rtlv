# rtl

## Setup

Run `raco pkg install` in the top-level directory to install the package.

## Testing

Run `raco test .` to run the tests. When possible, tests should go in the
`test` collection. When writing tests for functionality that is not exported,
they should go in a `test` submodule (see `yosys/reader.rkt` for an example).