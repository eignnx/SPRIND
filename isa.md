
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


#### Instruction Formats


| Format | Bit Pattern | # Opcodes | Range of Immediate |
|:----|:---:|:---:|:---:|
| `lsd` | `00ooiiiiiiiaarrr` | 4 opcode(s) | imm7 in -64..=63 or 0..=127 |
| `lsd` | `00oooiiiiiiaarrr` | 8 opcode(s) | imm6 in -32..=31 or 0..=63 |
| `subr` | `010iiiiiiiiiiiii` | 1 opcode(s) | imm13 in -4096..=4095 or 0..=8191 |
| `b`[^1] | `0110oiiiiiiiiiii` | 2 opcode(s) | imm11 in -1024..=1023 or 0..=2047 |
| `b` | `0110ooiiiiiiiiii` | 4 opcode(s) | imm10 in -512..=511 or 0..=1023 |
| `b` | `0110oooiiiiiiiii` | 8 opcode(s) | imm9 in -256..=255 or 0..=511 |
| `b` | `0110ooooiiiiiiii` | 16 opcode(s) | imm8 in -128..=127 or 0..=255 |
| `ext` | `0111oooooooooooo` | 4096 opcode(s) |  |
| `li`[^1] | `10iiiiiiiiiiirrr` | 1 opcode(s) | imm11 in -1024..=1023 or 0..=2047 |
| `li` | `10oiiiiiiiiiirrr` | 2 opcode(s) | imm10 in -512..=511 or 0..=1023 |
| `li` | `10ooiiiiiiiiirrr` | 4 opcode(s) | imm9 in -256..=255 or 0..=511 |
| `li` | `10oooiiiiiiiirrr` | 8 opcode(s) | imm8 in -128..=127 or 0..=255 |
| `ri(1)` | `110ooooiiiiiirrr` | 16 opcode(s) | imm6 in -32..=31 or 0..=63 |
| `ri(2)` | `1110oooiiiiiirrr` | 8 opcode(s) | imm6 in -32..=31 or 0..=63 |
| `ri(3)` | `11110ooiiiiiirrr` | 4 opcode(s) | imm6 in -32..=31 or 0..=63 |
| `rr(1)` | `111110ooooRRRrrr` | 16 opcode(s) |  |
| `rr(2)` | `1111110oooRRRrrr` | 8 opcode(s) |  |
| `rr(3)` | `11111110ooRRRrrr` | 4 opcode(s) |  |
| `r(1)` | `111111110oooorrr` | 16 opcode(s) |  |
| `r(2)` | `1111111110ooorrr` | 8 opcode(s) |  |
| `r(3)` | `11111111110oorrr` | 4 opcode(s) |  |
| `o` | `11111111111ooooo` | 32 opcode(s) |  |

[^1]: Not enough opcodes given the assignment of instructions!
### Instruction Counts by Format


| Generic format | Description | Instr. Count Options | Assignment Counts |
|:---:|:----|----:|:---:|
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
