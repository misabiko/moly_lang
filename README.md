A toy language.

Currently this README serves mostly as personal notes, so it might not make sense.

---

Inspirations
- Rust compiler implementation of Monkey Script
  - Cymbal https://github.com/shuhei/cymbal
- Calling function literals directly: `fn() { 5 }()`
  - Monkey
- Eliding field name on struct initialization
  - Go

Goals
- Elm-style compiler-driven development

Decisions
- When possible, instead of having the compiler optimize redundant code, give messages on how to optimize the code
- TODO Any need for empty static arrays?

Later features
- Removing semicolons, for now it's much easier to parse and make sense of

Wasm bytecode
https://blog.scottlogic.com/2019/05/17/webassembly-compiler.html