# SPRIND
A silly little instruction set architechture.

## Features
- 16-bit registers, 16-bit address space
- Fixed-size 16-bit instructions
- Boolean stack operations (via bit shifting)

## Specification
[Read the spec here](isa.md)

## Motivation
I've automated the ISA design process as much as possible because:

1. Code is hopefully consistent. Changes should automatically propagate throughout the design.
2. Design decisions are less arbitrary. You can see where I'm using an algorithm to make a decision, and hopefullly where decisions are made arbitrarily.
3. Prolog is fun to write, and is well suited to data modelling. I had been using spreadsheets, but I ran into enough issues and limitations that I decided to switch technologies.

## Development Environment

### Requirements
To build the documentation you'll need:

- A bash-like shell (I have `GNU bash, version 5.2.15(1)-release (x86_64-pc-msys)`)
- SWI Prolog (I'm using `SWI-Prolog version 9.0.4 for x64-win64`)
- GraphViz (On my system the command is called `dot`: `graphviz version 12.2.1 (20241206.2353)`)

### Build
Then to build, run `build.sh`:

```shell
sh build.sh
```