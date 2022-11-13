# The Neon Programming Language

Neon is a programming language that compiles down to Minecraft data packs, while operating at a significantly higher level of abstraction than regular functions.

Neon is work in progress and **very incomplete** at the moment. Some language constructs work already, but there are going to be many more to come.

## Comparison to similar projects

### [Trident](https://www.energyxxer.com/trident/)
Trident is probably the most popular project that fills this nieche. Then again, trident is not a direct competitor to Neon, since both projects have very different goals.

Trident is not as much a standalone language as it is a fairly advanced preprocessor for minecraft functions.

By contrast, neon is an entirely separate language. Neon has (well, is going to have) inline commands to allow interaction with the wider Minecraft ecosystem, but these are more comparable to inline assembly in C/Rust than Trident's commands.

Also, Trident is in an actually usable state at the moment.

## Roadmap

These are some important features that are currently missing from the language. If any of this sounds interesting and you would like to contribute, feel free to open an issue. Any help is appreciated!

- [ ] Source code positions in diagnostics
- [ ] Arithmetic (-, *, //, mod)
- [ ] Remaining comparison operators (<, >, >=, ==, !=)
- [X] If expressions
- [X] Expression blocks (like `{}` expressions in Rust)
- [ ] Inline commands
- [ ] Hindley Milner type inference
- [ ] Structs (and compound shapes in MIR)
- [ ] Zero sized types
- [ ] Variant types
- [ ] Polymorphism (via monomorphization)
- [ ] Traits
- [ ] Function closures

