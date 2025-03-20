# SPRIND
A silly little instruction set architechture.

## Features
- 16-bit registers, 16-bit address space
- Fixed-size 16-bit instructions
- Boolean stack operations (via bit shifting)

## Spec
[Read the spec here](https://github.com/eignnx/SPRIND/blob/main/isa.md)
> RELATIVE LINK TEST: [Read the spec here](isa.md)

## Requirements
To build the documentation you'll need:

- A bash-like shell (I have `GNU bash, version 5.2.15(1)-release (x86_64-pc-msys)`)
- SWI Prolog (I'm using `SWI-Prolog version 9.0.4 for x64-win64`)
- GraphViz (On my system the command is called `dot`: `graphviz version 12.2.1 (20241206.2353)`)

Then to build, run `build.sh`:

```shell
sh build.sh
```