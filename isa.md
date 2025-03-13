
# SPRIND Instruction Set Architecture Specification


## Machine Overview


### General Purpose Registers


| Register Name | Uses |
|:---:|:---:|
| `sp` | stack_ptr, addr |
| `x` | temp, arg(1), addr |
| `y` | temp, arg(2), addr |
| `z` | temp, arg(3), addr |
| `w` | temp, arg(4) |
| `v` | temp, retval |
| `a` | saved |
| `b` | saved |

### Register Uses and Calling Convention


| Usage Name | Description |
|:---:|:----|
| `stack_ptr` | Register serves as the stack pointer. |
| `addr` | Only some of the registers can be used as the address in a load/store instruction. |
| `temp` | Register may be used to hold temporary values without restriction. |
| `arg(_)` | Register is used as the Nth argument to a subroutine. |
| `retval` | A subroutine's return value is passed in this register. |
| `saved` | A called subroutine must save the content of these registers before using them, but their values persist across subroutine calls. |

## Instruction Specifications


### Instruction Counts by Format


| Generic format | Description | Instr. Count |
|:---:|:----|:---:|
| `lsd` | Load-store with Displacement | 4 |
| `subr` | Subroutine Call | 1 |
| `b` | Branch | 4 |
| `ext` | Reserved for Extension | 4096 |
| `li` | Load Immediate | 2 |
| `ri(_)` | Register-immediate | 28 |
| `rr(_)` | Register-register | 28 |
| `r(_)` | Register | 28 |
| `o` | Opcode | 32 |


Total instructions available (excluding `ext`): 127 (min), 127 (max)


### Format Assignment Availability


| Format | Max Opcodes Available | Opcodes Assigned | Opcodes Reserved |
|:---:|:---:|:---:|:---:|
| `lsd` | 4 | 4 | 0 |
| `subr` | 1 | 1 | 0 |
| `b` | 4 | 3 | 0 |
| `ext` | 4096 | 0 | 0 |
| `li` | 2 | 2 | 0 |
| `ri(1)` | 16 | 16 | 0 |
| `ri(2)` | 8 | 4 | 0 |
| `ri(3)` | 4 | 0 | 0 |
| `rr(1)` | 16 | 14 | 0 |
| `rr(2)` | 8 | 0 | 0 |
| `rr(3)` | 4 | 0 | 0 |
| `r(1)` | 16 | 11 | 0 |
| `r(2)` | 8 | 0 | 0 |
| `r(3)` | 4 | 0 | 0 |
| `o` | 32 | 27 | 0 |

### Instruction Format Breakdown


#### Legend


| Bit Symbol | Description |
|:---:|:----|
| `o` | A bit in the instruction's opcode. |
| `i` | A bit in an immediate value. |
| `r` | A bit in a register specifier. |
| `R` | A bit in a second register specifier. |
| `a` | A bit in an address register specifier. |
| `0` | A literal `0` embedded in the instruction. |
| `1` | A literal `1` embedded in the instruction. |


#### Instruction Format Layouts


| Format | Bit Pattern | \# Opcodes | Range of Immediate |
|:----|:---:|:---:|:---:|
| `lsd` | `00ooiiiiiiiaarrr` | 4 | imm7 in [-64,63] or [0,127] |
| `subr` | `010iiiiiiiiiiiii` | 1 | imm13 in [-4096,4095] or [0,8191] |
| `b` | `0110ooiiiiiiiiii` | 4 | imm10 in [-512,511] or [0,1023] |
| `ext` | `0111oooooooooooo` | 4096 |  |
| `li` | `10oiiiiiiiiiirrr` | 2 | imm10 in [-512,511] or [0,1023] |
| `ri(1)` | `110ooooiiiiiirrr` | 16 | imm6 in [-32,31] or [0,63] |
| `ri(2)` | `1110oooiiiiiirrr` | 8 | imm6 in [-32,31] or [0,63] |
| `ri(3)` | `11110ooiiiiiirrr` | 4 | imm6 in [-32,31] or [0,63] |
| `rr(1)` | `111110ooooRRRrrr` | 16 |  |
| `rr(2)` | `1111110oooRRRrrr` | 8 |  |
| `rr(3)` | `11111110ooRRRrrr` | 4 |  |
| `r(1)` | `111111110oooorrr` | 16 |  |
| `r(2)` | `1111111110ooorrr` | 8 |  |
| `r(3)` | `11111111110oorrr` | 4 |  |
| `o` | `11111111111ooooo` | 32 |  |

### Instruction Specifications


#### Instruction Format `lsd`


##### Load Byte - `lb`

Load a byte from memory into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |

##### Load Word - `lw`

Load a word from memory into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |

##### Store Byte - `sb`

Store a byte from a register into memory.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |

##### Store Word - `sw`

Store a word from a register into memory.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |

#### Instruction Format `subr`


##### Call Subroutine - `call`

Call a subroutine at the specified address.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b010 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | imm13 in [-4096,4095] or [0,8191] |

#### Instruction Format `b`


##### Branch - `b`

Branch to the specified address by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |

##### Branch If True - `bt`

Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |

##### Branch If False - `bf`

Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |

#### Instruction Format `ext`


#### Instruction Format `li`


##### Load Immediate - `li`

Load an immediate value into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100iiiiiiiiiirrr` | 10 | imm10 in [-512,511] or [0,1023] |

##### Shift Zero-extended Immediate - `szi`

Left-shift a zero-extended immediate value into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101iiiiiiiiiirrr` | 10 | imm10 in [-512,511] or [0,1023] |

#### Instruction Format `ri(1)`


##### Load Global Byte - `lgb`

Load a byte from a memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Load Global Word - `lgw`

Load a word from a memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Store Global Byte - `sgb`

Store a byte into memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Store Global Word - `sgw`

Store a word into memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Bit - `tbit`

Test a specific bit in a register, modifying `$TS`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100100iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Clear Bit - `cbit`

Clear a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x5 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100101iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Set Bit - `sbit`

Set a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x6 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100110iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Less-than Immediate - `tli`

Test if a register value is less than an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x7 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100111iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Greater-than or Equal Immediate - `tgei`

Test if a register value is greater than or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x8 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Below Immediate - `tbi`

Test if a register value is below an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x9 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Above or Equal - `taei`

Test if a register value is above or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xA |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Not Equal Immediate - `tnei`

Test if a register value is not equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xB |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Test Equal Immediate - `teqi`

Test if a register value is equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xC |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101100iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Add Immediate - `addi`

Add an immediate value to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xD |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101101iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### AND Immediate - `andi`

Perform a bitwise AND between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101110iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### OR Immediate - `ori`

Perform a bitwise OR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xF |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101111iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

#### Instruction Format `ri(2)`


##### XOR Immediate - `xori`

Perform a bitwise XOR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Logical Shift Right Immediate - `lsri`

Perform a logical shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Logical Shift Left Immediate - `lsli`

Perform a logical shift left on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

##### Arithmetic Shift Right Immediate - `asri`

Perform an arithmetic shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |

#### Instruction Format `ri(3)`


#### Instruction Format `rr(1)`


##### Add - `add`

Add the values of two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111100000RRRrrr` |

##### Subtract - `sub`

Subtract the value of one register from another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111100001RRRrrr` |

##### AND - `and`

Perform a bitwise AND between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111100010RRRrrr` |

##### OR - `or`

Perform a bitwise OR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111100011RRRrrr` |

##### XOR - `xor`

Perform a bitwise XOR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111100100RRRrrr` |

##### Move - `mov`

Move the value from one register to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111100101RRRrrr` |

##### Add with Carry - `addcy`

Add the values of two registers with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111100110RRRrrr` |

##### Subtract with Carry - `subcy`

Subtract the value of one register from another with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111100111RRRrrr` |

##### Test Less-than - `tl`

Test if the value of one register is less than another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111101000RRRrrr` |

##### Test Greater-than or Equal - `tge`

Test if the value of one register is greater than or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111101001RRRrrr` |

##### Test Below - `tb`

Test if the value of one register is below another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xA |

| Bit Layout |
|:---:|
| `1111101010RRRrrr` |

##### Test Above or Equal - `tae`

Test if the value of one register is above or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xB |

| Bit Layout |
|:---:|
| `1111101011RRRrrr` |

##### Test Not Equal - `tne`

Test if the value of one register is not equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xC |

| Bit Layout |
|:---:|
| `1111101100RRRrrr` |

##### Test Equal - `teq`

Test if the value of one register is equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xD |

| Bit Layout |
|:---:|
| `1111101101RRRrrr` |

#### Instruction Format `rr(2)`


#### Instruction Format `rr(3)`


#### Instruction Format `r(1)`


##### Push Byte - `pushb`

Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |

##### Push Word - `pushw`

Push a word from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |

##### Pop Byte - `popb`

Pop a byte from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |

##### Pop Word - `popw`

Pop a word from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |

##### Call Register - `callr`

Call a subroutine at the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |

##### Jump Register - `jr`

Jump to the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |

##### Negate - `neg`

Negate the value in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |

##### Sign Extend Byte - `seb`

Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111111100111rrr` |

##### Read $HI - `r.hi`

Read the value of the system `$HI` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111111101000rrr` |

##### Read $GP - `r.gp`

Read the value of the system `$GP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111111101001rrr` |

##### Write $GP - `w.gp`

Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0xA |

| Bit Layout |
|:---:|
| `1111111101010rrr` |

#### Instruction Format `r(2)`


#### Instruction Format `r(3)`


#### Instruction Format `o`


##### Non-executable (1's Version) - `NONEXE1`

Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x0 |

| Bit Layout |
|:---:|
| `1111111111100000` |

##### Breakpoint - `BREAK`

Trigger a breakpoint.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1 |

| Bit Layout |
|:---:|
| `1111111111100001` |

##### Halt - `HALT`

Halt the processor.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x2 |

| Bit Layout |
|:---:|
| `1111111111100010` |

##### Unimplemented - `UNIMPL`

Unimplemented instruction.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x3 |

| Bit Layout |
|:---:|
| `1111111111100011` |

##### Kernel Return - `kret`

Return from kernel mode.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x4 |

| Bit Layout |
|:---:|
| `1111111111100100` |

##### Kernel Call - `kcall`

Call a kernel function.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x5 |

| Bit Layout |
|:---:|
| `1111111111100101` |

##### Return - `ret`

Return from a subroutine.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x6 |

| Bit Layout |
|:---:|
| `1111111111100110` |

##### Test Overflow - `tov`

Test for overflow.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x7 |

| Bit Layout |
|:---:|
| `1111111111100111` |

##### Test Carry - `tcy`

Test for carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x8 |

| Bit Layout |
|:---:|
| `1111111111101000` |

##### Clear Carry - `cy0`

Clear the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x9 |

| Bit Layout |
|:---:|
| `1111111111101001` |

##### Set Carry - `cy1`

Set the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xA |

| Bit Layout |
|:---:|
| `1111111111101010` |

##### Teststack Push 0 - `tpush0`

Push 0 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xB |

| Bit Layout |
|:---:|
| `1111111111101011` |

##### Teststack Push 1 - `tpush1`

Push 1 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xC |

| Bit Layout |
|:---:|
| `1111111111101100` |

##### Teststack NOT - `tnot`

Perform a NOT operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xD |

| Bit Layout |
|:---:|
| `1111111111101101` |

##### Teststack AND - `tand`

Perform an AND operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xE |

| Bit Layout |
|:---:|
| `1111111111101110` |

##### Teststack OR - `tor`

Perform an OR operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xF |

| Bit Layout |
|:---:|
| `1111111111101111` |

##### Teststack Duplicate - `tdup`

Duplicate the top value on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x10 |

| Bit Layout |
|:---:|
| `1111111111110000` |

##### Preserve $HI - `prsv.hi`

Preserve the value of the `$HI` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x11 |

| Bit Layout |
|:---:|
| `1111111111110001` |

##### Restore $HI - `rstr.hi`

Restore the value of the `$HI` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x12 |

| Bit Layout |
|:---:|
| `1111111111110010` |

##### Preserve $TS - `prsv.ts`

Preserve the value of the `$TS` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x13 |

| Bit Layout |
|:---:|
| `1111111111110011` |

##### Restore $TS - `rstr.ts`

Restore the value of the `$TS` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x14 |

| Bit Layout |
|:---:|
| `1111111111110100` |

##### Preserve $RA - `prsv.ra`

Preserve the value of the `$RA` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x15 |

| Bit Layout |
|:---:|
| `1111111111110101` |

##### Restore $RA - `rstr.ra`

Restore the value of the `$RA` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x16 |

| Bit Layout |
|:---:|
| `1111111111110110` |

##### Preserve $GP - `prsv.gp`

Preserve the value of the `$GP` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x17 |

| Bit Layout |
|:---:|
| `1111111111110111` |

##### Restore $GP - `rstr.gp`

Restore the value of the `$GP` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x18 |

| Bit Layout |
|:---:|
| `1111111111111000` |

##### Preserve $CC - `prsv.cc`

Preserve the value of the `$CC` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x19 |

| Bit Layout |
|:---:|
| `1111111111111001` |

##### Restore $CC - `rstr.cc`

Restore the value of the `$CC` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1A |

| Bit Layout |
|:---:|
| `1111111111111010` |
