
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

### System Registers


| Register | Register Name | Size | Description |
|:---:|:---:|:---:|:----|
| `$pc` | Program Counter | 16-bits | Keeps track of the currently executing instruction. |
| `$ra` | Return Address | 16-bits | Saves the program counter for subroutine return. |
| `$ts` | Test Stack | 16-bits | Stores boolean values in a stack used by branch instructions. |
| `$cc` | Condition Codes | 16-bits | Stores carry and overflow flags. |
| `$gp` | Global Pointer | 16-bits | Points to a region of process memory reserved for global variables. |
| `$kr` | Kernel Return | 16-bits | Holds the value of the program counter during interrupts. |
| `$mp` | Multiplication Product | 32-bits | Holds the accumulating product during a multiplication. |

## Instruction Specifications


### Instruction Counts by Format


| Generic format | Description | Instr. Count |
|:---:|:----|:---:|
| `lsd` | Load-store with Displacement | 4 |
| `subr` | Subroutine Call | 1 |
| `b` | Branch | 4 |
| `ext` | Reserved for Extension | 4096 |
| `li` | Load Immediate | 2 |
| `ri(_)` | Register-immediate | 24 |
| `rrr` | Register-register-register | 4 |
| `rr(_)` | Register-register | 28 |
| `r(_)` | Register | 28 |
| `o` | Opcode | 32 |


Total instructions available (excluding `ext`): 127 (min), 127 (max)


### Format Assignment Availability


| Format | Max Opcodes Available | Opcodes Assigned | Opcodes Reserved | Usage Percent |
|:---:|:---:|:---:|:---:|:---:|
| `lsd` | 4 | 4 | 0 | 100% |
| `subr` | 1 | 1 | 0 | 100% |
| `b` | 4 | 3 | 0 | 75% |
| `ext` | 4096 | 0 | 0 | 0% |
| `li` | 2 | 2 | 0 | 100% |
| `ri(1)` | 16 | 16 | 0 | 100% |
| `ri(2)` | 8 | 6 | 0 | 75% |
| `rrr` | 4 | 1 | 0 | 25% |
| `rr(1)` | 16 | 14 | 0 | 88% |
| `rr(2)` | 8 | 0 | 0 | 0% |
| `rr(3)` | 4 | 0 | 0 | 0% |
| `r(1)` | 16 | 12 | 0 | 75% |
| `r(2)` | 8 | 0 | 0 | 0% |
| `r(3)` | 4 | 0 | 0 | 0% |
| `o` | 32 | 27 | 0 | 84% |

### Instruction Format Breakdown


#### Legend


| Bit Symbol | Description |
|:---:|:----|
| `o` | A bit in the instruction's opcode. |
| `i` | A bit in an immediate value. |
| `r` | A bit in a register specifier. |
| `s` | A bit in a second register specifier. |
| `t` | A bit in a third register specifier. |
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
| `rrr` | `11110ootttsssrrr` | 4 |  |
| `rr(1)` | `111110oooosssrrr` | 16 |  |
| `rr(2)` | `1111110ooosssrrr` | 8 |  |
| `rr(3)` | `11111110oosssrrr` | 4 |  |
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
| `lsd` = 0b00 | 0b00`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |
--------------

##### Load Word - `lw`

Load a word from memory into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b01`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |
--------------

##### Store Byte - `sb`

Store a byte from a register into memory.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b10`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |
--------------

##### Store Word - `sw`

Store a word from a register into memory.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b11`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiiirrr` | 7 | imm7 in [-64,63] or [0,127] |
--------------

#### Instruction Format `subr`


##### Call Subroutine - `call`

Call a subroutine at the specified address.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b010 | NONE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | imm13 in [-4096,4095] or [0,8191] |
--------------

#### Instruction Format `b`


##### Branch - `b`

Branch to the specified address by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b00`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |
--------------

##### Branch If True - `bt`

Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b01`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |
--------------

##### Branch If False - `bf`

Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b10`2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiii` | 10 | imm10 in [-512,511] or [0,1023] |
--------------

#### Instruction Format `ext`


#### Instruction Format `li`


##### Load Immediate - `li`

Load an immediate value into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0b0`1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100iiiiiiiiiirrr` | 10 | imm10 in [-512,511] or [0,1023] |
--------------

##### Shift Zero-extended Immediate - `szi`

Left-shift a zero-extended immediate value into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0b1`1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101iiiiiiiiiirrr` | 10 | imm10 in [-512,511] or [0,1023] |
--------------

#### Instruction Format `ri(1)`


##### Load Global Byte - `lgb`

Load a byte from a memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0000`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Load Global Word - `lgw`

Load a word from a memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0001`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Store Global Byte - `sgb`

Store a byte into memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0010`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Store Global Word - `sgw`

Store a word into memory address offset from `$GP`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0011`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Bit - `tbit`

Test a specific bit in a register, modifying `$TS`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0100`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100100iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Clear Bit - `cbit`

Clear a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0101`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100101iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Set Bit - `sbit`

Set a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0110`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100110iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Less-than Immediate - `tli`

Test if a register value is less than an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0111`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100111iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Greater-than or Equal Immediate - `tgei`

Test if a register value is greater than or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1000`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Below Immediate - `tbi`

Test if a register value is below an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1001`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Above or Equal - `taei`

Test if a register value is above or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1010`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Not Equal Immediate - `tnei`

Test if a register value is not equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1011`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Test Equal Immediate - `teqi`

Test if a register value is equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1100`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101100iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Add Immediate - `addi`

Add an immediate value to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1101`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101101iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### AND Immediate - `andi`

Perform a bitwise AND between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1110`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101110iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### OR Immediate - `ori`

Perform a bitwise OR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1111`4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101111iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

#### Instruction Format `ri(2)`


##### XOR Immediate - `xori`

Perform a bitwise XOR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b000`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110000iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Add Immediate with Carry - `addicy`

Add an immediate value and the carry bit to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b001`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110001iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Subtract Immediate with Carry - `subicy`

Sutract an immediate value and the carry bit from a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b010`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110010iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Logical Shift Right - `lsr`

Perform a logical shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b011`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110011iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Logical Shift Left - `lsl`

Perform a logical shift left on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b100`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110100iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

##### Arithmetic Shift Right - `asr`

Perform an arithmetic shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b101`3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110101iiiiiirrr` | 6 | imm6 in [-32,31] or [0,63] |
--------------

#### Instruction Format `rrr`


##### Multiplication Step - `mulstep`

Computes one step in a full 16-bit by 16-bit multiplication.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rrr` = 0b11110 | 0b00`2 |

| Bit Layout |
|:---:|
| `1111000rrr` |
--------------

#### Instruction Format `rr(1)`


##### Add - `add`

Add the values of two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0000`4 |

| Bit Layout |
|:---:|
| `1111100000rrr` |
--------------

##### Subtract - `sub`

Subtract the value of one register from another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0001`4 |

| Bit Layout |
|:---:|
| `1111100001rrr` |
--------------

##### AND - `and`

Perform a bitwise AND between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0010`4 |

| Bit Layout |
|:---:|
| `1111100010rrr` |
--------------

##### OR - `or`

Perform a bitwise OR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0011`4 |

| Bit Layout |
|:---:|
| `1111100011rrr` |
--------------

##### XOR - `xor`

Perform a bitwise XOR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0100`4 |

| Bit Layout |
|:---:|
| `1111100100rrr` |
--------------

##### Move - `mov`

Move the value from one register to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0101`4 |

| Bit Layout |
|:---:|
| `1111100101rrr` |
--------------

##### Add with Carry - `addcy`

Add the values of two registers with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0110`4 |

| Bit Layout |
|:---:|
| `1111100110rrr` |
--------------

##### Subtract with Carry - `subcy`

Subtract the value of one register from another with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0111`4 |

| Bit Layout |
|:---:|
| `1111100111rrr` |
--------------

##### Test Less-than - `tl`

Test if the value of one register is less than another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1000`4 |

| Bit Layout |
|:---:|
| `1111101000rrr` |
--------------

##### Test Greater-than or Equal - `tge`

Test if the value of one register is greater than or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1001`4 |

| Bit Layout |
|:---:|
| `1111101001rrr` |
--------------

##### Test Below - `tb`

Test if the value of one register is below another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1010`4 |

| Bit Layout |
|:---:|
| `1111101010rrr` |
--------------

##### Test Above or Equal - `tae`

Test if the value of one register is above or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1011`4 |

| Bit Layout |
|:---:|
| `1111101011rrr` |
--------------

##### Test Not Equal - `tne`

Test if the value of one register is not equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1100`4 |

| Bit Layout |
|:---:|
| `1111101100rrr` |
--------------

##### Test Equal - `teq`

Test if the value of one register is equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1101`4 |

| Bit Layout |
|:---:|
| `1111101101rrr` |
--------------

#### Instruction Format `rr(2)`


#### Instruction Format `rr(3)`


#### Instruction Format `r(1)`


##### Push Byte - `pushb`

Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0000`4 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |
--------------

##### Push Word - `pushw`

Push a word from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0001`4 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |
--------------

##### Pop Byte - `popb`

Pop a byte from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0010`4 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |
--------------

##### Pop Word - `popw`

Pop a word from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0011`4 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |
--------------

##### Call Register - `callr`

Call a subroutine at the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0100`4 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |
--------------

##### Jump Register - `jr`

Jump to the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0101`4 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |
--------------

##### Negate - `neg`

Negate the value in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0110`4 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |
--------------

##### Sign Extend Byte - `seb`

Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0111`4 |

| Bit Layout |
|:---:|
| `1111111100111rrr` |
--------------

##### Read $MP.lo - `rd.mp.lo`

Read the low word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1000`4 |

| Bit Layout |
|:---:|
| `1111111101000rrr` |
--------------

##### Read $MP.hi - `rd.mp.hi`

Read the high word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1001`4 |

| Bit Layout |
|:---:|
| `1111111101001rrr` |
--------------

##### Read $GP - `rd.gp`

Read the value of the system `$GP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1010`4 |

| Bit Layout |
|:---:|
| `1111111101010rrr` |
--------------

##### Write $GP - `wr.gp`

Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1011`4 |

| Bit Layout |
|:---:|
| `1111111101011rrr` |
--------------

#### Instruction Format `r(2)`


#### Instruction Format `r(3)`


#### Instruction Format `o`


##### Non-executable (1's Version) - `NONEXE1`

Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00000`5 |

| Bit Layout |
|:---:|
| `1111111111100000` |
--------------

##### Breakpoint - `BREAK`

Trigger a breakpoint.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00001`5 |

| Bit Layout |
|:---:|
| `1111111111100001` |
--------------

##### Halt - `HALT`

Halt the processor.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00010`5 |

| Bit Layout |
|:---:|
| `1111111111100010` |
--------------

##### Unimplemented - `UNIMPL`

Unimplemented instruction.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00011`5 |

| Bit Layout |
|:---:|
| `1111111111100011` |
--------------

##### Kernel Return - `kret`

Return from kernel mode.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00100`5 |

| Bit Layout |
|:---:|
| `1111111111100100` |
--------------

##### Kernel Call - `kcall`

Call a kernel function.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00101`5 |

| Bit Layout |
|:---:|
| `1111111111100101` |
--------------

##### Return - `ret`

Return from a subroutine.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00110`5 |

| Bit Layout |
|:---:|
| `1111111111100110` |
--------------

##### Test Overflow - `tov`

Test for overflow.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00111`5 |

| Bit Layout |
|:---:|
| `1111111111100111` |
--------------

##### Test Carry - `tcy`

Test for carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01000`5 |

| Bit Layout |
|:---:|
| `1111111111101000` |
--------------

##### Clear Carry - `clr.cy`

Clear the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01001`5 |

| Bit Layout |
|:---:|
| `1111111111101001` |
--------------

##### Set Carry - `set.cy`

Set the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01010`5 |

| Bit Layout |
|:---:|
| `1111111111101010` |
--------------

##### Teststack Push 0 - `tpush0`

Push 0 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01011`5 |

| Bit Layout |
|:---:|
| `1111111111101011` |
--------------

##### Teststack Push 1 - `tpush1`

Push 1 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01100`5 |

| Bit Layout |
|:---:|
| `1111111111101100` |
--------------

##### Teststack NOT - `tnot`

Perform a NOT operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01101`5 |

| Bit Layout |
|:---:|
| `1111111111101101` |
--------------

##### Teststack AND - `tand`

Perform an AND operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01110`5 |

| Bit Layout |
|:---:|
| `1111111111101110` |
--------------

##### Teststack OR - `tor`

Perform an OR operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01111`5 |

| Bit Layout |
|:---:|
| `1111111111101111` |
--------------

##### Teststack Duplicate - `tdup`

Duplicate the top value on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10000`5 |

| Bit Layout |
|:---:|
| `1111111111110000` |
--------------

##### Preserve $MP - `prsv.mp`

Preserve the value of the `$MP` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10001`5 |

| Bit Layout |
|:---:|
| `1111111111110001` |
--------------

##### Restore $MP - `rstr.mp`

Restore the value of the `$MP` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10010`5 |

| Bit Layout |
|:---:|
| `1111111111110010` |
--------------

##### Preserve $TS - `prsv.ts`

Preserve the value of the `$TS` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10011`5 |

| Bit Layout |
|:---:|
| `1111111111110011` |
--------------

##### Restore $TS - `rstr.ts`

Restore the value of the `$TS` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10100`5 |

| Bit Layout |
|:---:|
| `1111111111110100` |
--------------

##### Preserve $RA - `prsv.ra`

Preserve the value of the `$RA` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10101`5 |

| Bit Layout |
|:---:|
| `1111111111110101` |
--------------

##### Restore $RA - `rstr.ra`

Restore the value of the `$RA` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10110`5 |

| Bit Layout |
|:---:|
| `1111111111110110` |
--------------

##### Preserve $GP - `prsv.gp`

Preserve the value of the `$GP` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10111`5 |

| Bit Layout |
|:---:|
| `1111111111110111` |
--------------

##### Restore $GP - `rstr.gp`

Restore the value of the `$GP` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11000`5 |

| Bit Layout |
|:---:|
| `1111111111111000` |
--------------

##### Preserve $CC - `prsv.cc`

Preserve the value of the `$CC` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11001`5 |

| Bit Layout |
|:---:|
| `1111111111111001` |
--------------

##### Restore $CC - `rstr.cc`

Restore the value of the `$CC` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11010`5 |

| Bit Layout |
|:---:|
| `1111111111111010` |
--------------

> !!! validation_failed(ill-formed instruction semantics(bt,[invalid_rval(1),invalid_rval(#(_208))]))

