
### Instruction Counts by Format


| Generic format | Description | Instr. Count Options |
|:---:|:----|----:|
| `lsd` | Load-store with Displacement | 4, 8 |
| `subr` | Subroutine Call | 1 |
| `b` | Branch | 2, 4, 8, 16 |
| `ext` | Reserved for Extension | 4096 |
| `li` | Load Immediate | 1, 2, 4, 8 |
| `ri(_)` | Register-immediate | 28 |
| `rr(_)` | Register-register | 28 |
| `r(_)` | Register | 28 |
| `o` | Opcode | 32 |


Total instructions available (excluding `ext`): 124 (min), 149 (max)


### Format Assignment Availability


| Format | Max Opcodes Available | Opcodes Assigned | Opcodes Reserved |
|:---:|:---:|:---:|:---:|
| `lsd` | 8 | 4 | 0 |
| `subr` | 1 | 1 | 0 |
| `b` | 16 | 3 | 0 |
| `ext` | 4096 | 0 | 0 |
| `li` | 8 | 2 | 0 |
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
| `lsd` | `00ooiiiiiiiaarrr` | 4 opcode(s) | imm7 in -64..=63 or 0..=127 |  |
| `lsd` | `00oooiiiiiiaarrr` | 8 opcode(s) | imm6 in -32..=31 or 0..=63 |  |
| `subr` | `010iiiiiiiiiiiii` | 1 opcode(s) | imm13 in -4096..=4095 or 0..=8191 |  |
| `b` | `0110oiiiiiiiiiii` | 2 opcode(s) | imm11 in -1024..=1023 or 0..=2047 | X |
| `b` | `0110ooiiiiiiiiii` | 4 opcode(s) | imm10 in -512..=511 or 0..=1023 |  |
| `b` | `0110oooiiiiiiiii` | 8 opcode(s) | imm9 in -256..=255 or 0..=511 |  |
| `b` | `0110ooooiiiiiiii` | 16 opcode(s) | imm8 in -128..=127 or 0..=255 |  |
| `ext` | `0111oooooooooooo` | 4096 opcode(s) |  |  |
| `li` | `10iiiiiiiiiiirrr` | 1 opcode(s) | imm11 in -1024..=1023 or 0..=2047 | X |
| `li` | `10oiiiiiiiiiirrr` | 2 opcode(s) | imm10 in -512..=511 or 0..=1023 |  |
| `li` | `10ooiiiiiiiiirrr` | 4 opcode(s) | imm9 in -256..=255 or 0..=511 |  |
| `li` | `10oooiiiiiiiirrr` | 8 opcode(s) | imm8 in -128..=127 or 0..=255 |  |
| `ri(1)` | `110ooooiiiiiirrr` | 16 opcode(s) | imm6 in -32..=31 or 0..=63 |  |
| `ri(2)` | `1110oooiiiiiirrr` | 8 opcode(s) | imm6 in -32..=31 or 0..=63 |  |
| `ri(3)` | `11110ooiiiiiirrr` | 4 opcode(s) | imm6 in -32..=31 or 0..=63 |  |
| `rr(1)` | `111110ooooRRRrrr` | 16 opcode(s) |  |  |
| `rr(2)` | `1111110oooRRRrrr` | 8 opcode(s) |  |  |
| `rr(3)` | `11111110ooRRRrrr` | 4 opcode(s) |  |  |
| `r(1)` | `111111110oooorrr` | 16 opcode(s) |  |  |
| `r(2)` | `1111111110ooorrr` | 8 opcode(s) |  |  |
| `r(3)` | `11111111110oorrr` | 4 opcode(s) |  |  |
| `o` | `11111111111ooooo` | 32 opcode(s) |  |  |

### Instruction Specifications


#### `lb` - Load Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiiirrr` | 7 | imm7 in -64..=63 or 0..=127 |
| `00000iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `lw` - Load Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiiirrr` | 7 | imm7 in -64..=63 or 0..=127 |
| `00001iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `sb` - Store Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiiirrr` | 7 | imm7 in -64..=63 or 0..=127 |
| `00010iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `sw` - Store Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `lsd` = 0b00 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiiirrr` | 7 | imm7 in -64..=63 or 0..=127 |
| `00011iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `call` - Call Subroutine


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b010 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | imm13 in -4096..=4095 or 0..=8191 |

#### `b` - Branch


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01100iiiiiiiiiii` | 11 | imm11 in -1024..=1023 or 0..=2047 |
| `011000iiiiiiiiii` | 10 | imm10 in -512..=511 or 0..=1023 |
| `0110000iiiiiiiii` | 9 | imm9 in -256..=255 or 0..=511 |
| `01100000iiiiiiii` | 8 | imm8 in -128..=127 or 0..=255 |

#### `bt` - Branch If True


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01101iiiiiiiiiii` | 11 | imm11 in -1024..=1023 or 0..=2047 |
| `011001iiiiiiiiii` | 10 | imm10 in -512..=511 or 0..=1023 |
| `0110001iiiiiiiii` | 9 | imm9 in -256..=255 or 0..=511 |
| `01100001iiiiiiii` | 8 | imm8 in -128..=127 or 0..=255 |

#### `bf` - Branch If False


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiiii` | 11 | imm11 in -1024..=1023 or 0..=2047 |
| `011010iiiiiiiiii` | 10 | imm10 in -512..=511 or 0..=1023 |
| `0110010iiiiiiiii` | 9 | imm9 in -256..=255 or 0..=511 |
| `01100010iiiiiiii` | 8 | imm8 in -128..=127 or 0..=255 |

#### `li` - Load Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10iiiiiiiiiiirrr` | 11 | imm11 in -1024..=1023 or 0..=2047 |
| `100iiiiiiiiiirrr` | 10 | imm10 in -512..=511 or 0..=1023 |
| `1000iiiiiiiiirrr` | 9 | imm9 in -256..=255 or 0..=511 |
| `10000iiiiiiiirrr` | 8 | imm8 in -128..=127 or 0..=255 |

#### `szi` - Shift Zero-extended Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b10 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10iiiiiiiiiiirrr` | 11 | imm11 in -1024..=1023 or 0..=2047 |
| `101iiiiiiiiiirrr` | 10 | imm10 in -512..=511 or 0..=1023 |
| `1001iiiiiiiiirrr` | 9 | imm9 in -256..=255 or 0..=511 |
| `10001iiiiiiiirrr` | 8 | imm8 in -128..=127 or 0..=255 |

#### `lgb` - Load Global Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100000iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `lgw` - Load Global Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100001iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `sgb` - Store Global Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100010iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `sgw` - Store Global Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100011iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `tbit` - Test Bit


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x4 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100100iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `cbit` - Clear Bit


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x5 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100101iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `sbit` - Set Bit


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x6 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100110iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `tli` - Test Less-than Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x7 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100111iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `tgei` - Test Greater-than or Equal Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x8 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101000iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `tbi` - Test Below Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0x9 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101001iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `taei` - Test Above or Equal


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xA |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101010iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `tnei` - Test Not Equal Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xB |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101011iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `teqi` - Test Equal Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xC |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101100iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `addi` - Add Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xD |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101101iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `andi` - AND Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101110iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `ori` - OR Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b110 | 0xF |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101111iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `xori` - XOR Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110000iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `lsri` - Logical Shift Right Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110001iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `lsli` - Logical Shift Left Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x2 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110010iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `asri` - Arithmetic Shift Right Immediate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(2)` = 0b1110 | 0x3 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110011iiiiiirrr` | 6 | imm6 in -32..=31 or 0..=63 |

#### `add` - Add


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111100000RRRrrr` |

#### `sub` - Subtract


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111100001RRRrrr` |

#### `and` - AND


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111100010RRRrrr` |

#### `or` - OR


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111100011RRRrrr` |

#### `xor` - XOR


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111100100RRRrrr` |

#### `mov` - Move


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111100101RRRrrr` |

#### `addcy` - Add with Carry


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111100110RRRrrr` |

#### `subcy` - Subtract with Carry


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111100111RRRrrr` |

#### `tl` - Test Less-than


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111101000RRRrrr` |

#### `tge` - Test Greater-than or Equal


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111101001RRRrrr` |

#### `tb` - Test Below


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xA |

| Bit Layout |
|:---:|
| `1111101010RRRrrr` |

#### `tae` - Test Above or Equal


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xB |

| Bit Layout |
|:---:|
| `1111101011RRRrrr` |

#### `tne` - Test Not Equal


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xC |

| Bit Layout |
|:---:|
| `1111101100RRRrrr` |

#### `teq` - Test Equal


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0xD |

| Bit Layout |
|:---:|
| `1111101101RRRrrr` |

#### `pushb` - Push Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x0 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |

#### `pushw` - Push Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x1 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |

#### `popb` - Pop Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x2 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |

#### `popw` - Pop Word


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x3 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |

#### `callr` - Call Register


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x4 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |

#### `jr` - Jump Register


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x5 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |

#### `neg` - Negate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x6 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |

#### `seb` - Sign Extend Byte


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x7 |

| Bit Layout |
|:---:|
| `1111111100111rrr` |

#### `r.hi` - Read $HI


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x8 |

| Bit Layout |
|:---:|
| `1111111101000rrr` |

#### `r.gp` - Read $GP


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0x9 |

| Bit Layout |
|:---:|
| `1111111101001rrr` |

#### `w.gp` - Write $GP


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0xA |

| Bit Layout |
|:---:|
| `1111111101010rrr` |

#### `NONEXE1` - Non-executable (1's Version)


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x0 |

| Bit Layout |
|:---:|
| `1111111111100000` |

#### `BREAK` - Breakpoint


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1 |

| Bit Layout |
|:---:|
| `1111111111100001` |

#### `HALT` - Halt


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x2 |

| Bit Layout |
|:---:|
| `1111111111100010` |

#### `UNIMPL` - Unimplemented


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x3 |

| Bit Layout |
|:---:|
| `1111111111100011` |

#### `kret` - Kernel Return


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x4 |

| Bit Layout |
|:---:|
| `1111111111100100` |

#### `kcall` - Kernel Call


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x5 |

| Bit Layout |
|:---:|
| `1111111111100101` |

#### `ret` - Return


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x6 |

| Bit Layout |
|:---:|
| `1111111111100110` |

#### `tov` - Test Overflow


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x7 |

| Bit Layout |
|:---:|
| `1111111111100111` |

#### `tcy` - Test Carry


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x8 |

| Bit Layout |
|:---:|
| `1111111111101000` |

#### `cy0` - Clear Carry


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x9 |

| Bit Layout |
|:---:|
| `1111111111101001` |

#### `cy1` - Set Carry


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xA |

| Bit Layout |
|:---:|
| `1111111111101010` |

#### `tpush0` - Teststack Push 0


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xB |

| Bit Layout |
|:---:|
| `1111111111101011` |

#### `tpush1` - Teststack Push 1


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xC |

| Bit Layout |
|:---:|
| `1111111111101100` |

#### `tnot` - Teststack NOT


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xD |

| Bit Layout |
|:---:|
| `1111111111101101` |

#### `tand` - Teststack AND


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xE |

| Bit Layout |
|:---:|
| `1111111111101110` |

#### `tor` - Teststack OR


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0xF |

| Bit Layout |
|:---:|
| `1111111111101111` |

#### `tdup` - Teststack Duplicate


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x10 |

| Bit Layout |
|:---:|
| `1111111111110000` |

#### `prsv.hi` - Preserve $HI


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x11 |

| Bit Layout |
|:---:|
| `1111111111110001` |

#### `rstr.hi` - Restore $HI


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x12 |

| Bit Layout |
|:---:|
| `1111111111110010` |

#### `prsv.ts` - Preserve $TS


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x13 |

| Bit Layout |
|:---:|
| `1111111111110011` |

#### `rstr.ts` - Restore $TS


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x14 |

| Bit Layout |
|:---:|
| `1111111111110100` |

#### `prsv.ra` - Preserve $RA


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x15 |

| Bit Layout |
|:---:|
| `1111111111110101` |

#### `rstr.ra` - Restore $RA


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x16 |

| Bit Layout |
|:---:|
| `1111111111110110` |

#### `prsv.gp` - Preserve $GP


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x17 |

| Bit Layout |
|:---:|
| `1111111111110111` |

#### `rstr.gp` - Restore $GP


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x18 |

| Bit Layout |
|:---:|
| `1111111111111000` |

#### `prsv.cc` - Preserve $CC


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x19 |

| Bit Layout |
|:---:|
| `1111111111111001` |

#### `rstr.cc` - Restore $CC


##### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0x1A |

| Bit Layout |
|:---:|
| `1111111111111010` |
