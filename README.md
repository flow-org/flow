# flow

flow is a representation of "flow concept" using programming language.

For more details, please visit [the documentation website](https://flowlang.vercel.app/).

# Development

This package is managed by `cabal`. You can use `cabal repl` to immediately run the program during development.

# Build

For building WebAssembly binary, the additional GHC backend should be installed. Please refer to the page below:

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html

According to the page, [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) is useful to setup the backend. Note that you may need to set the environment variable `FLAVOUR` to `9.6` before executing `./setup.sh`.
