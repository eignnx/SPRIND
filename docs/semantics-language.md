# Explanation of Semantics Language

Each instruction is described by a semantic formula which defines its behavior. This page explains the semantics language. There are two forms of the language: the first is an embedded Prolog DSL which has nice properties for reasoning about the syntax (type checking is easier this way), and the second is the "user facing" form which appears in the Instruction Listing document. This page describes the latter, you may have to read the code to learn the former, but it's not too different.

## `li` Example

Here's an example of the language. This describes the behavior of the `li` (Load Immediate) instruction.

```
[reg(r, rd), simm(simm)]
------------------------
rd ← sxt(simm)
```

### `li` Example: Instruction Syntax

Above the bar is a list of parameters embedded in the instruction. An `li` instruction embeds a destination register (here arbitrarily called `rd`) and a signed-immediate value (arbitrarily called `simm`) which `li` will store in the destination register.

The `reg(_, _)` term describes an embedded register ID. Its first argument is one of `r`, `s`, or `t` which correspond to a specific location in the instruction's bit pattern. `li`'s bit pattern looks like this: `10000iiiiiiiirrr`. In the most complicated case (the `mulstep` instruction) a bit pattern may look like this: `0000100tttsssrrr`. So the `r` in `reg(r, rd)`  binds the variable `rd` to the three bits `rrr` in the instruction's bit pattern, which specify one of the 8 (2^3) possible general purpose registers.

The `simm(_)` term works similarly, but there is only ever one immediate value embedded in an instruction, so `simm(_)`'s argument is just the variable name which represents the immediate value. If the instruction instead interpreted its immediate value as an unsigned number (the `szi` instruction is an example), we would use `imm(_)` instead.

### `li` Example: Instruction Semantics

Below the bar is a formula describing the instruction's semantics. `li` is pretty simple: `rd ← sxt(simm)` means "take the immediate value called `simm`, sign extend (`sxt`) it to register width (16 bits), and store it in the register referred to by `rd`."

## Sizes and Signedness

All semantic formulas are run through a type checker to verify that each value's bit-size and signed or unsigned-ness make sense given the value's use.

### Kinds

For signedness or unsignedness, I use the term **kind**. These are the kinds:

| Kind | Description                                                  |
| ---- | ------------------------------------------------------------ |
| `u`  | unsigned: an unsigned integer                                |
| `s`  | signed: a signed integer                                     |
| `i`  | just an integer: neither unsigned nor signed, it just represents a raw "bit sequence." |

## Sizes

A value's **size** is expressed in number of bits. Some immediate values may be 6 bits wide; general purpose registers in SPRIND are 16 bits wide; a single bit can represent a boolean value; etc.

### Types

A value's **type** is the combination of its kind and its size. I use the `\` (backslash) operator to coerce or specify a value's type, for example:

```
123     % has type `i\_` (bit sequence with unknown size (though we know it can fit in 7 bits)
-3\5    % has type `s\5` (a signed integer occupying 5 bits)
9999999\3 % Invalid: the number can't fit in 3 bits
7\s     % 7 as a signed value with not-yet-specified size
7\s\4   % 7 as a 4 bit signed integer
7\4\s   % Same as above: bit size and kind can be applied in either order
```



## `#<name>`

A specification-time constant value. Refers to a specific integer defined somewhere in the spec.


### Examples

- `#carry_flag_idx`

## `#<integer>`

A literal integer constant. By default has no specific size or [kind](#kinds), unless the literal is negative, in which case it is assumed to be either [kind i](#kind-i) or [kind s](#kind-s).


### Examples

- `#123`

## `- <rval>`

The arithmetic negation of a value. The expression `-X` is equivalent to `1 + ~~X`. If `X` has <type> `s\5`, `-X` has type `s\5`.


### Examples

- `- ?rs`
- `- #some_constant`

## `~ <rval>`

The bitwise complement of a value.


### Examples

- `~123`

## `<rval>\<coersion>`

Coerces the value on the left according to the coercion on the right


### Examples

- `3\s\14 % 3 = 0b11, so evals to 0b00000000000011`
