
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

## Instruction Specifications


### Instruction Counts by Format


| Generic format | Description | Instr. Count Options |
|:---:|:----|----:|
| `lsd` | Load-store with Displacement | 4 |
| `subr` | Subroutine Call | 1 |
| `b` | Branch | 4 |
| `ext` | Reserved for Extension | 4096 |
| `li` | Load Immediate | 2 |
| `ri(_)` | Register-register | 28 |
| `rr(_)` | Register-immediate | 28 |
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

Consequtive rows with the same format represent alternative 
        representations. For example if format `xyz` has two rows in the table 
        then the constraints are not strict enough find a unique layout for 
        `xyz`.

| Format | Bit Pattern | # Opcodes | Range of Immediate | Too Many Instr.s Assigned? |
|:----|:---:|:---:|:---:|:---:|
| `lsd` | `00ooiiiiiiiaarrr` | 4 opcode(s) | imm7 in ..=(-64,63) or ..=(0,127) |  |
| `subr` | `010iiiiiiiiiiiii` | 1 opcode(s) | imm13 in ..=(-4096,4095) or ..=(0,8191) |  |
| `b` | `0110ooiiiiiiiiii` | 4 opcode(s) | imm10 in ..=(-512,511) or ..=(0,1023) |  |
| `ext` | `0111oooooooooooo` | 4096 opcode(s) |  |  |
| `li` | `10oiiiiiiiiiirrr` | 2 opcode(s) | imm10 in ..=(-512,511) or ..=(0,1023) |  |
| `ri(1)` | `110ooooiiiiiirrr` | 16 opcode(s) | imm6 in ..=(-32,31) or ..=(0,63) |  |
| `ri(2)` | `1110oooiiiiiirrr` | 8 opcode(s) | imm6 in ..=(-32,31) or ..=(0,63) |  |
| `ri(3)` | `11110ooiiiiiirrr` | 4 opcode(s) | imm6 in ..=(-32,31) or ..=(0,63) |  |
| `rr(1)` | `111110ooooRRRrrr` | 16 opcode(s) |  |  |
| `rr(2)` | `1111110oooRRRrrr` | 8 opcode(s) |  |  |
| `rr(3)` | `11111110ooRRRrrr` | 4 opcode(s) |  |  |
| `r(1)` | `111111110oooorrr` | 16 opcode(s) |  |  |
| `r(2)` | `1111111110ooorrr` | 8 opcode(s) |  |  |
| `r(3)` | `11111111110oorrr` | 4 opcode(s) |  |  |
| `o` | `11111111111ooooo` | 32 opcode(s) |  |  |

### Instruction Specifications


#### `lb` - Load Byte

Load a byte from memory into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiiirrr` | 7 | imm7 in ..=(-64,63) or ..=(0,127) |

#### `lw` - Load Word

Load a word from memory into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiiirrr` | 7 | imm7 in ..=(-64,63) or ..=(0,127) |

#### `sb` - Store Byte

Store a byte from a register into memory.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiiirrr` | 7 | imm7 in ..=(-64,63) or ..=(0,127) |

#### `sw` - Store Word

Store a word from a register into memory.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiiirrr` | 7 | imm7 in ..=(-64,63) or ..=(0,127) |

#### `call` - Call Subroutine

Call a subroutine at the specified address.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b010 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | imm13 in ..=(-4096,4095) or ..=(0,8191) |

#### `b` - Branch

Branch to the specified address by adding the immediate offset to `$PC`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | imm10 in ..=(-512,511) or ..=(0,1023) |

#### `bt` - Branch If True

Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiiiiii` | 10 | imm10 in ..=(-512,511) or ..=(0,1023) |

#### `bf` - Branch If False

Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiii` | 10 | imm10 in ..=(-512,511) or ..=(0,1023) |

#### `li` - Load Immediate

Load an immediate value into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100iiiiiiiiiirrr` | 10 | imm10 in ..=(-512,511) or ..=(0,1023) |

#### `szi` - Shift Zero-extended Immediate

Left-shift a zero-extended immediate value into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101iiiiiiiiiirrr` | 10 | imm10 in ..=(-512,511) or ..=(0,1023) |

#### `lgb` - Load Global Byte

Load a byte from a memory address offset from `$GP`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100000iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `lgw` - Load Global Word

Load a word from a memory address offset from `$GP`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100001iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `sgb` - Store Global Byte

Store a byte into memory address offset from `$GP`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100010iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `sgw` - Store Global Word

Store a word into memory address offset from `$GP`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100011iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `tbit` - Test Bit

Test a specific bit in a register, modifying `$TS`.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100100iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `cbit` - Clear Bit

Clear a specific bit in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x5 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100101iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `sbit` - Set Bit

Set a specific bit in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x6 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100110iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `tli` - Test Less-than Immediate

Test if a register value is less than an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x7 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100111iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `tgei` - Test Greater-than or Equal Immediate

Test if a register value is greater than or equal to an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x8 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101000iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `tbi` - Test Below Immediate

Test if a register value is below an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x9 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101001iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `taei` - Test Above or Equal

Test if a register value is above or equal to an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xA |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101010iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `tnei` - Test Not Equal Immediate

Test if a register value is not equal to an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xB |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101011iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `teqi` - Test Equal Immediate

Test if a register value is equal to an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xC |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101100iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `addi` - Add Immediate

Add an immediate value to a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xD |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101101iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `andi` - AND Immediate

Perform a bitwise AND between a register and an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101110iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `ori` - OR Immediate

Perform a bitwise OR between a register and an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xF |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101111iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `xori` - XOR Immediate

Perform a bitwise XOR between a register and an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110000iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `lsri` - Logical Shift Right Immediate

Perform a logical shift right on a register by an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110001iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `lsli` - Logical Shift Left Immediate

Perform a logical shift left on a register by an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110010iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `asri` - Arithmetic Shift Right Immediate

Perform an arithmetic shift right on a register by an immediate value.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110011iiiiiirrr` | 6 | imm6 in ..=(-32,31) or ..=(0,63) |

#### `add` - Add

Add the values of two registers.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111100000RRRrrr` |

#### `sub` - Subtract

Subtract the value of one register from another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111100001RRRrrr` |

#### `and` - AND

Perform a bitwise AND between two registers.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111100010RRRrrr` |

#### `or` - OR

Perform a bitwise OR between two registers.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111100011RRRrrr` |

#### `xor` - XOR

Perform a bitwise XOR between two registers.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111100100RRRrrr` |

#### `mov` - Move

Move the value from one register to another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111100101RRRrrr` |

#### `addcy` - Add with Carry

Add the values of two registers with carry.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111100110RRRrrr` |

#### `subcy` - Subtract with Carry

Subtract the value of one register from another with carry.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111100111RRRrrr` |

#### `tl` - Test Less-than

Test if the value of one register is less than another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111101000RRRrrr` |

#### `tge` - Test Greater-than or Equal

Test if the value of one register is greater than or equal to another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111101001RRRrrr` |

#### `tb` - Test Below

Test if the value of one register is below another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xA |

| Bit Layout |
|:---:|
| `1111101010RRRrrr` |

#### `tae` - Test Above or Equal

Test if the value of one register is above or equal to another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xB |

| Bit Layout |
|:---:|
| `1111101011RRRrrr` |

#### `tne` - Test Not Equal

Test if the value of one register is not equal to another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xC |

| Bit Layout |
|:---:|
| `1111101100RRRrrr` |

#### `teq` - Test Equal

Test if the value of one register is equal to another.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xD |

| Bit Layout |
|:---:|
| `1111101101RRRrrr` |

#### `pushb` - Push Byte

Push a byte from a register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |

#### `pushw` - Push Word

Push a word from a register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |

#### `popb` - Pop Byte

Pop a byte from the stack into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |

#### `popw` - Pop Word

Pop a word from the stack into a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |

#### `callr` - Call Register

Call a subroutine at the address in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |

#### `jr` - Jump Register

Jump to the address in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |

#### `neg` - Negate

Negate the value in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |

#### `seb` - Sign Extend Byte

Sign extend a byte in a register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111111100111rrr` |

#### `r.hi` - Read $HI

Read the value of the system `$HI` register into a general purpose register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111111101000rrr` |

#### `r.gp` - Read $GP

Read the value of the system `$GP` register into a general purpose register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111111101001rrr` |

#### `w.gp` - Write $GP

Write a value to the system `$GP` register from a general purpose register.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0xA |

| Bit Layout |
|:---:|
| `1111111101010rrr` |

#### `NONEXE1` - Non-executable (1's Version)

Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x0 |

| Bit Layout |
|:---:|
| `1111111111100000` |

#### `BREAK` - Breakpoint

Trigger a breakpoint.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1 |

| Bit Layout |
|:---:|
| `1111111111100001` |

#### `HALT` - Halt

Halt the processor.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x2 |

| Bit Layout |
|:---:|
| `1111111111100010` |

#### `UNIMPL` - Unimplemented

Unimplemented instruction.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x3 |

| Bit Layout |
|:---:|
| `1111111111100011` |

#### `kret` - Kernel Return

Return from kernel mode.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x4 |

| Bit Layout |
|:---:|
| `1111111111100100` |

#### `kcall` - Kernel Call

Call a kernel function.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x5 |

| Bit Layout |
|:---:|
| `1111111111100101` |

#### `ret` - Return

Return from a subroutine.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x6 |

| Bit Layout |
|:---:|
| `1111111111100110` |

#### `tov` - Test Overflow

Test for overflow.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x7 |

| Bit Layout |
|:---:|
| `1111111111100111` |

#### `tcy` - Test Carry

Test for carry.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x8 |

| Bit Layout |
|:---:|
| `1111111111101000` |

#### `cy0` - Clear Carry

Clear the carry flag.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x9 |

| Bit Layout |
|:---:|
| `1111111111101001` |

#### `cy1` - Set Carry

Set the carry flag.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xA |

| Bit Layout |
|:---:|
| `1111111111101010` |

#### `tpush0` - Teststack Push 0

Push 0 onto the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xB |

| Bit Layout |
|:---:|
| `1111111111101011` |

#### `tpush1` - Teststack Push 1

Push 1 onto the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xC |

| Bit Layout |
|:---:|
| `1111111111101100` |

#### `tnot` - Teststack NOT

Perform a NOT operation on the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xD |

| Bit Layout |
|:---:|
| `1111111111101101` |

#### `tand` - Teststack AND

Perform an AND operation on the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xE |

| Bit Layout |
|:---:|
| `1111111111101110` |

#### `tor` - Teststack OR

Perform an OR operation on the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xF |

| Bit Layout |
|:---:|
| `1111111111101111` |

#### `tdup` - Teststack Duplicate

Duplicate the top value on the test stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x10 |

| Bit Layout |
|:---:|
| `1111111111110000` |

#### `prsv.hi` - Preserve $HI

Preserve the value of the `$HI` register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x11 |

| Bit Layout |
|:---:|
| `1111111111110001` |

#### `rstr.hi` - Restore $HI

Restore the value of the `$HI` register from the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x12 |

| Bit Layout |
|:---:|
| `1111111111110010` |

#### `prsv.ts` - Preserve $TS

Preserve the value of the `$TS` register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x13 |

| Bit Layout |
|:---:|
| `1111111111110011` |

#### `rstr.ts` - Restore $TS

Restore the value of the `$TS` register from the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x14 |

| Bit Layout |
|:---:|
| `1111111111110100` |

#### `prsv.ra` - Preserve $RA

Preserve the value of the `$RA` register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x15 |

| Bit Layout |
|:---:|
| `1111111111110101` |

#### `rstr.ra` - Restore $RA

Restore the value of the `$RA` register from the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x16 |

| Bit Layout |
|:---:|
| `1111111111110110` |

#### `prsv.gp` - Preserve $GP

Preserve the value of the `$GP` register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x17 |

| Bit Layout |
|:---:|
| `1111111111110111` |

#### `rstr.gp` - Restore $GP

Restore the value of the `$GP` register from the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x18 |

| Bit Layout |
|:---:|
| `1111111111111000` |

#### `prsv.cc` - Preserve $CC

Preserve the value of the `$CC` register onto the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x19 |

| Bit Layout |
|:---:|
| `1111111111111001` |

#### `rstr.cc` - Restore $CC

Restore the value of the `$CC` register from the stack.

##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1A |

| Bit Layout |
|:---:|
| `1111111111111010` |
