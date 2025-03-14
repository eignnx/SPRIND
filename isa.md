
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
| `0` | A literal `0` embedded in the instruction. |
| `1` | A literal `1` embedded in the instruction. |


#### Instruction Format Layouts


| Format | Bit Pattern | \# Opcodes | Range of Immediate |
|:----|:---:|:---:|:---:|
| `lsd` | `00ooiiiiiisssrrr` | 4 | imm6 in $[-32,31]$ or $[0,63]$ |
| `subr` | `010iiiiiiiiiiiii` | 1 | imm13 in $[-4096,4095]$ or $[0,8191]$ |
| `b` | `0110ooiiiiiiiiii` | 4 | imm10 in $[-512,511]$ or $[0,1023]$ |
| `ext` | `0111oooooooooooo` | 4096 |  |
| `li` | `10oiiiiiiiiiirrr` | 2 | imm10 in $[-512,511]$ or $[0,1023]$ |
| `ri(1)` | `110ooooiiiiiirrr` | 16 | imm6 in $[-32,31]$ or $[0,63]$ |
| `ri(2)` | `1110oooiiiiiirrr` | 8 | imm6 in $[-32,31]$ or $[0,63]$ |
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

###### Examples

- `lb w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rd)]
-------------------------------
?rd <- zxt([?adr + ?simm])
```

--------------

##### Load Word - `lw`

Load a word from memory into a register.

###### Examples

- `lw w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rd)]
----------------------------------
?ptr = b_and(?adr + ?simm,#65534);
?rd <- hi_lo([?ptr + #1],[?ptr])
```

--------------

##### Store Byte - `sb`

Store a byte from a register into memory.

###### Examples

- `sb [sp-20], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rs)]
-------------------------------
[?adr + ?simm] <- ?rs
```

--------------

##### Store Word - `sw`

Store a word from a register into memory.

###### Examples

- `sw [sp-20], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0b11 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rs)]
----------------------------------
?ptr = b_and(?adr + ?simm,#65534);
[?ptr] <- lo(?rs);
[?ptr + #1] <- hi(?rs)
```

--------------

#### Instruction Format `subr`


##### Call Subroutine - `call`

Call a subroutine at the specified address.

###### Examples

- `call SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b010 | NONE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | imm13 in $[-4096,4095]$ or $[0,8191]$ |

###### Semantics

```
[imm(?imm)]
--------------------------------------
$$pc <- $$pc + sxt(?imm)<<#subr_align;
$$ra <- $$pc + #2
```

--------------

#### Instruction Format `b`


##### Branch - `b`

Branch to the specified address by adding the immediate offset to `$PC`.

###### Examples

- `b SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | imm10 in $[-512,511]$ or $[0,1023]$ |

###### Semantics

```
[imm(?offset)]
---------------------------
$$pc <- $$pc + sxt(?offset)
```

--------------

##### Branch If True - `bt`

Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Examples

- `bt SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiiiiii` | 10 | imm10 in $[-512,511]$ or $[0,1023]$ |

###### Semantics

```
[imm(?offset)]
-------------------------------
if b_pop($$ts) == #1 {
    $$pc <- $$pc + sxt(?offset)
}
```

--------------

##### Branch If False - `bf`

Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

###### Examples

- `bf SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiii` | 10 | imm10 in $[-512,511]$ or $[0,1023]$ |

###### Semantics

```
[imm(?offset)]
-------------------------------
if b_pop($$ts) == #0 {
    $$pc <- $$pc + sxt(?offset)
}
```

--------------

#### Instruction Format `ext`


#### Instruction Format `li`


##### Load Immediate - `li`

Load an immediate value into a register.

###### Examples

- `li x, 123`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0b0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100iiiiiiiiiirrr` | 10 | imm10 in $[-512,511]$ or $[0,1023]$ |

###### Semantics

```
[imm(?simm),reg(?rd)]
---------------------
?rd <- sxt(?simm)
```

--------------

##### Shift Zero-extended Immediate - `szi`

Left-shift a zero-extended immediate value into a register.

###### Examples

- `szi x, 0xB3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101iiiiiiiiiirrr` | 10 | imm10 in $[-512,511]$ or $[0,1023]$ |

###### Semantics

```
[imm(?imm),reg(?rd)]
------------------------------
?rd <- b_or(?rd<<#8,zxt(?imm))
```

--------------

#### Instruction Format `ri(1)`


##### Load Global Byte - `lgb`

Load a byte from a memory address offset from `$GP`.

###### Examples

- `lgb x, [gp+8]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100000iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?disp),reg(?rd)]
-------------------------------
?rd <- zxt([$$gp + zxt(?disp)])
```

--------------

##### Load Global Word - `lgw`

Load a word from a memory address offset from `$GP`.

###### Examples

- `lgw x, [gp+8]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100001iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?disp),reg(?rd)]
---------------------------------------
?ptr = b_and($$gp + zxt(?disp),#65534);
?rd <- hi_lo([?ptr + #1],[?ptr])
```

--------------

##### Store Global Byte - `sgb`

Store a byte into memory address offset from `$GP`.

###### Examples

- `sgb [gp+8], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100010iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?disp),reg(?rs)]
--------------------------
[$$gp + zxt(?disp)] <- ?rs
```

--------------

##### Store Global Word - `sgw`

Store a word into memory address offset from `$GP`.

###### Examples

- `sgw [gp+8], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100011iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[imm(?disp),reg(?rs)]
---------------------------------------
?ptr = b_and($$gp + zxt(?disp),#65534);
hi_lo([?ptr + #1],[?ptr]) <- ?rs
```

--------------

##### Test Bit - `tbit`

Test a specific bit in a register, modifying `$TS`.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100100iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Clear Bit - `cbit`

Clear a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100101iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Set Bit - `sbit`

Set a specific bit in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100110iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Less-than Immediate - `tli`

Test if a register value is less than an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b0111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100111iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Greater-than or Equal Immediate - `tgei`

Test if a register value is greater than or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101000iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Below Immediate - `tbi`

Test if a register value is below an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101001iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Above or Equal - `taei`

Test if a register value is above or equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101010iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Not Equal Immediate - `tnei`

Test if a register value is not equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101011iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Equal Immediate - `teqi`

Test if a register value is equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101100iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Add Immediate - `addi`

Add an immediate value to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101101iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### AND Immediate - `andi`

Perform a bitwise AND between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101110iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### OR Immediate - `ori`

Perform a bitwise OR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0b1111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101111iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

#### Instruction Format `ri(2)`


##### XOR Immediate - `xori`

Perform a bitwise XOR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110000iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Add Immediate with Carry - `addicy`

Add an immediate value and the carry bit to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110001iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Subtract Immediate with Carry - `subicy`

Sutract an immediate value and the carry bit from a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110010iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Logical Shift Right - `lsr`

Perform a logical shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110011iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Logical Shift Left - `lsl`

Perform a logical shift left on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110100iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Arithmetic Shift Right - `asr`

Perform an arithmetic shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0b101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110101iiiiiirrr` | 6 | imm6 in $[-32,31]$ or $[0,63]$ |

###### Semantics

```
[_75592]
--------
nop
```

--------------

#### Instruction Format `rrr`


##### Multiplication Step - `mulstep`

Computes one step in a full 16-bit by 16-bit multiplication.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rrr` = 0b11110 | 0b00 |

| Bit Layout |
|:---:|
| `1111000rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

#### Instruction Format `rr(1)`


##### Add - `add`

Add the values of two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0000 |

| Bit Layout |
|:---:|
| `1111100000rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Subtract - `sub`

Subtract the value of one register from another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0001 |

| Bit Layout |
|:---:|
| `1111100001rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### AND - `and`

Perform a bitwise AND between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0010 |

| Bit Layout |
|:---:|
| `1111100010rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### OR - `or`

Perform a bitwise OR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0011 |

| Bit Layout |
|:---:|
| `1111100011rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### XOR - `xor`

Perform a bitwise XOR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0100 |

| Bit Layout |
|:---:|
| `1111100100rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Move - `mov`

Move the value from one register to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0101 |

| Bit Layout |
|:---:|
| `1111100101rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Add with Carry - `addcy`

Add the values of two registers with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0110 |

| Bit Layout |
|:---:|
| `1111100110rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Subtract with Carry - `subcy`

Subtract the value of one register from another with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0111 |

| Bit Layout |
|:---:|
| `1111100111rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Less-than - `tl`

Test if the value of one register is less than another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1000 |

| Bit Layout |
|:---:|
| `1111101000rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Greater-than or Equal - `tge`

Test if the value of one register is greater than or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1001 |

| Bit Layout |
|:---:|
| `1111101001rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Below - `tb`

Test if the value of one register is below another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1010 |

| Bit Layout |
|:---:|
| `1111101010rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Above or Equal - `tae`

Test if the value of one register is above or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1011 |

| Bit Layout |
|:---:|
| `1111101011rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Not Equal - `tne`

Test if the value of one register is not equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1100 |

| Bit Layout |
|:---:|
| `1111101100rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Equal - `teq`

Test if the value of one register is equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b1101 |

| Bit Layout |
|:---:|
| `1111101101rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

#### Instruction Format `rr(2)`


#### Instruction Format `rr(3)`


#### Instruction Format `r(1)`


##### Push Byte - `pushb`

Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0000 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Push Word - `pushw`

Push a word from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0001 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Pop Byte - `popb`

Pop a byte from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0010 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Pop Word - `popw`

Pop a word from the stack into a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0011 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Call Register - `callr`

Call a subroutine at the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0100 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Jump Register - `jr`

Jump to the address in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0101 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Negate - `neg`

Negate the value in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0110 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Sign Extend Byte - `seb`

Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0111 |

| Bit Layout |
|:---:|
| `1111111100111rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Read $MP.lo - `rd.mp.lo`

Read the low word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1000 |

| Bit Layout |
|:---:|
| `1111111101000rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Read $MP.hi - `rd.mp.hi`

Read the high word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1001 |

| Bit Layout |
|:---:|
| `1111111101001rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Read $GP - `rd.gp`

Read the value of the system `$GP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1010 |

| Bit Layout |
|:---:|
| `1111111101010rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Write $GP - `wr.gp`

Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b1011 |

| Bit Layout |
|:---:|
| `1111111101011rrr` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

#### Instruction Format `r(2)`


#### Instruction Format `r(3)`


#### Instruction Format `o`


##### Non-executable (1s Version) - `NONEXE1`

Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00000 |

| Bit Layout |
|:---:|
| `1111111111100000` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Breakpoint - `BREAK`

Trigger a breakpoint.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00001 |

| Bit Layout |
|:---:|
| `1111111111100001` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Halt - `HALT`

Halt the processor.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00010 |

| Bit Layout |
|:---:|
| `1111111111100010` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Unimplemented - `UNIMPL`

Unimplemented instruction.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00011 |

| Bit Layout |
|:---:|
| `1111111111100011` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Kernel Return - `kret`

Return from kernel mode.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00100 |

| Bit Layout |
|:---:|
| `1111111111100100` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Kernel Call - `kcall`

Call a kernel function.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00101 |

| Bit Layout |
|:---:|
| `1111111111100101` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Return - `ret`

Return from a subroutine.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00110 |

| Bit Layout |
|:---:|
| `1111111111100110` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Overflow - `tov`

Test for overflow.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00111 |

| Bit Layout |
|:---:|
| `1111111111100111` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Test Carry - `tcy`

Test for carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01000 |

| Bit Layout |
|:---:|
| `1111111111101000` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Clear Carry - `clr.cy`

Clear the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01001 |

| Bit Layout |
|:---:|
| `1111111111101001` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Set Carry - `set.cy`

Set the carry flag.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01010 |

| Bit Layout |
|:---:|
| `1111111111101010` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack Push 0 - `tpush0`

Push 0 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01011 |

| Bit Layout |
|:---:|
| `1111111111101011` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack Push 1 - `tpush1`

Push 1 onto the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01100 |

| Bit Layout |
|:---:|
| `1111111111101100` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack NOT - `tnot`

Perform a NOT operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01101 |

| Bit Layout |
|:---:|
| `1111111111101101` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack AND - `tand`

Perform an AND operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01110 |

| Bit Layout |
|:---:|
| `1111111111101110` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack OR - `tor`

Perform an OR operation on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b01111 |

| Bit Layout |
|:---:|
| `1111111111101111` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Teststack Duplicate - `tdup`

Duplicate the top value on the test stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10000 |

| Bit Layout |
|:---:|
| `1111111111110000` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Preserve $MP - `prsv.mp`

Preserve the value of the `$MP` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10001 |

| Bit Layout |
|:---:|
| `1111111111110001` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Restore $MP - `rstr.mp`

Restore the value of the `$MP` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10010 |

| Bit Layout |
|:---:|
| `1111111111110010` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Preserve $TS - `prsv.ts`

Preserve the value of the `$TS` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10011 |

| Bit Layout |
|:---:|
| `1111111111110011` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Restore $TS - `rstr.ts`

Restore the value of the `$TS` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10100 |

| Bit Layout |
|:---:|
| `1111111111110100` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Preserve $RA - `prsv.ra`

Preserve the value of the `$RA` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10101 |

| Bit Layout |
|:---:|
| `1111111111110101` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Restore $RA - `rstr.ra`

Restore the value of the `$RA` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10110 |

| Bit Layout |
|:---:|
| `1111111111110110` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Preserve $GP - `prsv.gp`

Preserve the value of the `$GP` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b10111 |

| Bit Layout |
|:---:|
| `1111111111110111` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Restore $GP - `rstr.gp`

Restore the value of the `$GP` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11000 |

| Bit Layout |
|:---:|
| `1111111111111000` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Preserve $CC - `prsv.cc`

Preserve the value of the `$CC` register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11001 |

| Bit Layout |
|:---:|
| `1111111111111001` |

###### Semantics

```
[_75592]
--------
nop
```

--------------

##### Restore $CC - `rstr.cc`

Restore the value of the `$CC` register from the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b11010 |

| Bit Layout |
|:---:|
| `1111111111111010` |

###### Semantics

```
[_75592]
--------
nop
```

--------------
