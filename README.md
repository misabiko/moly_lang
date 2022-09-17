Inspirations
- Rust compiler implementation of Monkey Script
  - Cymbal https://github.com/shuhei/cymbal
  - Cymbal https://github.com/shuhei/cymbal
- `fn() { 5 }()`
  - Monkey
- Eliding field name on struct initialization
  - Go

Goals
- Elm-style compiler-driven development

Decisions
- When possible, instead of having the compiler optimize redundant code, give messages on how to optimize the code

Later features
- Removing semicolons, for now it's much easier to parse and make sense of