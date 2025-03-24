
# Instructions


## Instruction Counts by Format


| Generic format | Description | Available Opcodes | Assigned | Utilization |
|:---:|:----|:---:|:---:|:---:|
| [`rri`](#instruction-format-rri) | Register-register-immediate | 4 | 4 | 100% |
| [`subr`](#instruction-format-subr) | Subroutine Call | 1 | 1 | 100% |
| [`b`](#instruction-format-b) | Branch | 4 | 3 | 75% |
| [`li`](#instruction-format-li) | Load Immediate | 2 | 2 | 100% |
| [`ri(_)`](#instruction-format-ri_) | Register-immediate | 48 | 22 | 46% |
| [`ext`](#instruction-format-ext) | Reserved for Extension | 4096 | 0 | 0% |
| [`rrr`](#instruction-format-rrr) | Register-register-register | 4 | 1 | 25% |
| [`rr(_)`](#instruction-format-rr_) | Register-register | 28 | 14 | 50% |
| [`r(_)`](#instruction-format-r_) | Register | 28 | 12 | 43% |
| [`o`](#instruction-format-o) | Opcode | 32 | 27 | 84% |
|  | **Totals (excluding `ext`)** | **151** | **86** | **57%** |



## Instruction Listing


| `rri` | `subr` | `b` | `li` | `ri(_)` | `rrr` | `rr(_)` | `r(_)` | `o` |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| [`lb`](#the-lb-instruction) | [`call`](#the-call-instruction) | [`b`](#the-b-instruction) | [`li`](#the-li-instruction) | [`lgb`](#the-lgb-instruction) | [`mulstep`](#the-mulstep-instruction) | [`add`](#the-add-instruction) | [`pushb`](#the-pushb-instruction) | [`NONEXE0`](#the-nonexe0-instruction) |
| [`lw`](#the-lw-instruction) |  | [`bt`](#the-bt-instruction) | [`szi`](#the-szi-instruction) | [`lgw`](#the-lgw-instruction) |  | [`sub`](#the-sub-instruction) | [`pushw`](#the-pushw-instruction) | [`UNIMPL`](#the-unimpl-instruction) |
| [`sb`](#the-sb-instruction) |  | [`bf`](#the-bf-instruction) |  | [`sgb`](#the-sgb-instruction) |  | [`and`](#the-and-instruction) | [`popb`](#the-popb-instruction) | [`HALT`](#the-halt-instruction) |
| [`sw`](#the-sw-instruction) |  |  |  | [`sgw`](#the-sgw-instruction) |  | [`or`](#the-or-instruction) | [`popw`](#the-popw-instruction) | [`BREAK`](#the-break-instruction) |
|  |  |  |  | [`tbit`](#the-tbit-instruction) |  | [`xor`](#the-xor-instruction) | [`callr`](#the-callr-instruction) | [`kret`](#the-kret-instruction) |
|  |  |  |  | [`cbit`](#the-cbit-instruction) |  | [`mov`](#the-mov-instruction) | [`jr`](#the-jr-instruction) | [`kcall`](#the-kcall-instruction) |
|  |  |  |  | [`sbit`](#the-sbit-instruction) |  | [`addcy`](#the-addcy-instruction) | [`neg`](#the-neg-instruction) | [`ret`](#the-ret-instruction) |
|  |  |  |  | [`tli`](#the-tli-instruction) |  | [`subcy`](#the-subcy-instruction) | [`seb`](#the-seb-instruction) | [`tov`](#the-tov-instruction) |
|  |  |  |  | [`tgei`](#the-tgei-instruction) |  | [`tl`](#the-tl-instruction) | [`rd.mp.lo`](#the-rdmplo-instruction) | [`tcy`](#the-tcy-instruction) |
|  |  |  |  | [`tbi`](#the-tbi-instruction) |  | [`tge`](#the-tge-instruction) | [`rd.mp.hi`](#the-rdmphi-instruction) | [`clr.cy`](#the-clrcy-instruction) |
|  |  |  |  | [`taei`](#the-taei-instruction) |  | [`tb`](#the-tb-instruction) | [`rd.gp`](#the-rdgp-instruction) | [`set.cy`](#the-setcy-instruction) |
|  |  |  |  | [`tnei`](#the-tnei-instruction) |  | [`tae`](#the-tae-instruction) | [`wr.gp`](#the-wrgp-instruction) | [`tpush0`](#the-tpush0-instruction) |
|  |  |  |  | [`teqi`](#the-teqi-instruction) |  | [`tne`](#the-tne-instruction) |  | [`tpush1`](#the-tpush1-instruction) |
|  |  |  |  | [`addi`](#the-addi-instruction) |  | [`teq`](#the-teq-instruction) |  | [`tnot`](#the-tnot-instruction) |
|  |  |  |  | [`andi`](#the-andi-instruction) |  |  |  | [`tand`](#the-tand-instruction) |
|  |  |  |  | [`ori`](#the-ori-instruction) |  |  |  | [`tor`](#the-tor-instruction) |
|  |  |  |  | [`xori`](#the-xori-instruction) |  |  |  | [`tdup`](#the-tdup-instruction) |
|  |  |  |  | [`addicy`](#the-addicy-instruction) |  |  |  | [`prsv.mp`](#the-prsvmp-instruction) |
|  |  |  |  | [`subicy`](#the-subicy-instruction) |  |  |  | [`rstr.mp`](#the-rstrmp-instruction) |
|  |  |  |  | [`lsr`](#the-lsr-instruction) |  |  |  | [`prsv.ts`](#the-prsvts-instruction) |
|  |  |  |  | [`lsl`](#the-lsl-instruction) |  |  |  | [`rstr.ts`](#the-rstrts-instruction) |
|  |  |  |  | [`asr`](#the-asr-instruction) |  |  |  | [`prsv.ra`](#the-prsvra-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.ra`](#the-rstrra-instruction) |
|  |  |  |  |  |  |  |  | [`prsv.gp`](#the-prsvgp-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.gp`](#the-rstrgp-instruction) |
|  |  |  |  |  |  |  |  | [`prsv.cc`](#the-prsvcc-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.cc`](#the-rstrcc-instruction) |

## Synthetic Instructions


| Synth. Instr. | Description | Expansion | Reversability |
|:---:|:---:|:---:|:---:|
| `clr r` | Clear a register | `xor r, r` | Reversible |
| `nop` | The no-op instruction | `ori $sp, 0` | Reversible |
| `incr r` | Increment a register | `addi r, 1` | Reversible |
| `decr r` | Increment a register | `subi r, 1` | Reversible |
| `inv r` | Bitwise inversion (complement) | `xori r, -1` | Reversible |
| `not r` | Invert a boolean (0 or 1) | `xori r, 1` | Reversible |
| `tg r1, r2` | Test greater-than | `tl r2, r1` | One Way |
| `tle r1, r2` | Test Less-than or Equal | `tge r2, r1` | One Way |
| `ta r1, r2` | Test Above | `ta r2, r1` | One Way |
| `tbe r1, r2` | Test Below or Equal | `tae r2, r1` | One Way |

## Instruction Format Breakdown


### Instruction Format Layouts


| Format | [Bit Pattern](#legend) | Opcodes Available | Assigned | Utilization | Range of Immediate |
|:----|:---:|:---:|:---:|:---:|:---:|
| [`rri`](#format-rri) | `11ooiiiiiisssrrr` | 4 | 4 | 100% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`subr`](#format-subr) | `101iiiiiiiiiiiii` | 1 | 1 | 100% | `imm13` in `[-4096, 4095]` or `[0, 8191]` |
| [`b`](#format-b) | `1001ooiiiiiiiiii` | 4 | 3 | 75% | `imm10` in `[-512, 511]` or `[0, 1023]` |
| [`li`](#format-li) | `1000oiiiiiiiirrr` | 2 | 2 | 100% | `imm8` in `[-128, 127]` or `[0, 255]` |
| [`ri(1)`](#format-ri1) | `01oooooiiiiiirrr` | 32 | 22 | 69% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`ri(2)`](#format-ri2) | `001ooooiiiiiirrr` | 16 | 0 | 0% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`ext`](#format-ext) | `0001oooooooooooo` | 4096 | 0 | 0% |  |
| [`rrr`](#format-rrr) | `00001ootttsssrrr` | 4 | 1 | 25% |  |
| [`rr(1)`](#format-rr1) | `000001oooosssrrr` | 16 | 8 | 50% |  |
| [`rr(2)`](#format-rr2) | `0000001ooosssrrr` | 8 | 4 | 50% |  |
| [`rr(3)`](#format-rr3) | `00000001oosssrrr` | 4 | 2 | 50% |  |
| [`r(1)`](#format-r1) | `000000001oooorrr` | 16 | 8 | 50% |  |
| [`r(2)`](#format-r2) | `0000000001ooorrr` | 8 | 4 | 50% |  |
| [`r(3)`](#format-r3) | `00000000001oorrr` | 4 | 0 | 0% |  |
| [`o`](#format-o) | `00000000000ooooo` | 32 | 27 | 84% |  |

### Legend


| Bit Symbol | Description |
|:---:|:----|
| `o` | A bit in the instruction's opcode. |
| `i` | A bit in an immediate value. |
| `r` | A bit in a register specifier. |
| `s` | A bit in a second register specifier. |
| `t` | A bit in a third register specifier. |
| `0` | A literal `0` embedded in the instruction. |
| `1` | A literal `1` embedded in the instruction. |


## Instruction Specifications


### Instruction Format `rri`


#### Format `rri`


![../assets/rri.svg](../assets/rri.svg)

##### The `lb` Instruction

**Load Byte** --- Load a byte from memory into a register.

###### Examples

- `lb w, [sp+12]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | 0b11 | `1111iiiiiisssrrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs), reg(rd)]
------------------------------
let ptr := (rs\s+sxt(simm))\u;
rd <- zxt([ptr])
```

--------------

##### The `lw` Instruction

**Load Word** --- Load a word from memory into a register.

###### Examples

- `lw w, [sp+12]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | 0b01 | `1101iiiiiisssrrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs), reg(rd)]
--------------------------------------
let ptr := (rs\s+sxt(simm)and-2\16)\u;
rd <- {[ptr+1], [ptr]}
```

--------------

##### The `sb` Instruction

**Store Byte** --- Store a byte from a register into memory.

###### Examples

- `sb [sp-20], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | 0b10 | `1110iiiiiisssrrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd), reg(rs)]
------------------------------
let ptr := rd\s+sxt(simm);
[ptr\u] <- lo(rs)
```

--------------

##### The `sw` Instruction

**Store Word** --- Store a word from a register into memory.

###### Examples

- `sw [sp-20], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | 0b00 | `1100iiiiiisssrrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd), reg(rs)]
--------------------------------------
let ptr := (rd\s+sxt(simm)and65534)\u;
[ptr] <- lo(rs);
[ptr+1] <- hi(rs)
```

--------------

### Instruction Format `subr`


#### Format `subr`


![../assets/subr.svg](../assets/subr.svg)

##### The `call` Instruction

**Call Subroutine** --- Call a subroutine at the specified address.

###### Examples

- `call SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `subr` = 0b101 | NONE | `101iiiiiiiiiiiii` | 13 | `imm13` in `[-4096, 4095]` or `[0, 8191]` |

###### Semantics

```
[simm(simm)]
-------------------------------------
$PC <- $PC\s+(sxt(simm)<<subr_align);
$RA <- $PC+2
```

--------------

### Instruction Format `b`


#### Format `b`


![../assets/b.svg](../assets/b.svg)

##### The `b` Instruction

**Branch** --- Branch to the specified address by adding the immediate offset to `$PC`.

###### Examples

- `b SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | 0b00 | `100100iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

###### Semantics

```
[simm(offset)]
------------------------
$PC <- $PC\s+sxt(offset)
```

--------------

##### The `bt` Instruction

**Branch If True** --- Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Examples

- `bt SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | 0b10 | `100110iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

###### Semantics

```
[simm(offset)]
----------------------------
if b_pop($TS) {
    $PC <- $PC\s+sxt(offset)
}
```

--------------

##### The `bf` Instruction

**Branch If False** --- Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

###### Examples

- `bf SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | 0b11 | `100111iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

###### Semantics

```
[simm(offset)]
----------------------------
if !(b_pop($TS)) {
    $PC <- $PC\s+sxt(offset)
}
```

--------------

### Instruction Format `li`


#### Format `li`


![../assets/li.svg](../assets/li.svg)

##### The `li` Instruction

**Load Immediate** --- Load an immediate value into a register.

###### Examples

- `li x, 123`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `li` = 0b1000 | 0b0 | `10000iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

###### Semantics

```
[simm(simm), reg(rd)]
---------------------
rd <- sxt(simm)
```

--------------

##### The `szi` Instruction

**Shift Zero-extended Immediate** --- Left-shift a zero-extended immediate value into a register.

###### Examples

- `szi x, 0xB3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `li` = 0b1000 | 0b1 | `10001iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

###### Semantics

```
[imm(imm), reg(rd)]
-----------------------
rd <- rd<<8 or zxt(imm)
```

--------------

### Instruction Format `ri(_)`


#### Format `ri(1)`


![../assets/ri(1).svg](../assets/ri(1).svg)

##### The `lgb` Instruction

**Load Global Byte** --- Load a byte from a memory address offset from `$GP`.

###### Examples

- `lgb x, [gp+8]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00111 | `0100111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rd)]
----------------------------
rd <- zxt([$GP\u+zxt(disp)])
```

--------------

##### The `lgw` Instruction

**Load Global Word** --- Load a word from a memory address offset from `$GP`.

###### Examples

- `lgw x, [gp+8]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00101 | `0100101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rd)]
---------------------------------------
let ptr := ($GP\u+zxt(disp)and65534)\u;
rd <- {[ptr+1], [ptr]}
```

--------------

##### The `sgb` Instruction

**Store Global Byte** --- Store a byte into memory address offset from `$GP`.

###### Examples

- `sgb [gp+8], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00110 | `0100110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rs)]
---------------------------
[$GP\u+zxt(disp)] <- lo(rs)
```

--------------

##### The `sgw` Instruction

**Store Global Word** --- Store a word into memory address offset from `$GP`.

###### Examples

- `sgw [gp+8], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00100 | `0100100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rs)]
---------------------------------------
let ptr := ($GP\u+zxt(disp)and65534)\u;
{[ptr+1], [ptr]} <- rs
```

--------------

##### The `tbit` Instruction

**Test Bit** --- Test a specific bit in a register, modifying `$TS`.

###### Examples

- `tbit 12, w`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01100 | `0101100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rs)]
-------------------------------------
let shamt := bitslice(bit_idx, 3..0);
let bit := rs>>shamt\u and 1;
b_push($TS, bit==1)
```

--------------

##### The `cbit` Instruction

**Clear Bit** --- Clear a specific bit in a register.

###### Examples

- `cbit 9, v`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00111 | `0100111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~ (1<<idx);
rd <- rd and mask
```

--------------

##### The `sbit` Instruction

**Set Bit** --- Set a specific bit in a register.

###### Examples

- `sbit 15, a`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01101 | `0101101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~ (1<<idx);
rd <- rd or mask
```

--------------

##### The `tli` Instruction

**Test Less-than Immediate** --- Test if a register value is less than an immediate value.

###### Examples

- `tli x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01111 | `0101111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs)]
----------------------------------------------
b_push($TS, compare(rs\s, <(s\16), sxt(simm)))
```

--------------

##### The `tgei` Instruction

**Test Greater-than or Equal Immediate** --- Test if a register value is greater than or equal to an immediate value.

###### Examples

- `tgei x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01101 | `0101101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs)]
-----------------------------------------------
b_push($TS, compare(rs\s, >=(s\16), sxt(simm)))
```

--------------

##### The `tbi` Instruction

**Test Below Immediate** --- Test if a register value is below an immediate value.

###### Examples

- `tbi x, 10`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01110 | `0101110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rs)]
---------------------------------------------
b_push($TS, compare(rs\u, <(u\16), zxt(imm)))
```

--------------

##### The `taei` Instruction

**Test Above or Equal** --- Test if a register value is above or equal to an immediate value.

###### Examples

- `taei x, 10`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01100 | `0101100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rs)]
----------------------------------------------
b_push($TS, compare(rs\u, >=(u\16), zxt(imm)))
```

--------------

##### The `tnei` Instruction

**Test Not Equal Immediate** --- Test if a register value is not equal to an immediate value.

###### Examples

- `tnei x, 0`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00101 | `0100101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs)]
----------------------------
b_push($TS, rs\s\=sxt(simm))
```

--------------

##### The `teqi` Instruction

**Test Equal Immediate** --- Test if a register value is equal to an immediate value.

###### Examples

- `teqi x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00100 | `0100100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs)]
----------------------------
b_push($TS, rs\s==sxt(simm))
```

--------------

##### The `addi` Instruction

**Add Immediate** --- Add an immediate value to a register.

###### Examples

- `addi x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00000 | `0100000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
---------------------
rd <- rd\s+sxt(simm)
```

--------------

##### The `andi` Instruction

**AND Immediate** --- Perform a bitwise AND between a register and an immediate value.

###### Examples

- `andi x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01011 | `0101011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
----------------------
rd <- rd and sxt(simm)
```

--------------

##### The `ori` Instruction

**OR Immediate** --- Perform a bitwise OR between a register and an immediate value.

###### Examples

- `ori x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b10100 | `0110100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
---------------------
rd <- rd or sxt(simm)
```

--------------

##### The `xori` Instruction

**XOR Immediate** --- Perform a bitwise XOR between a register and an immediate value.

###### Examples

- `xori x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b10101 | `0110101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
----------------------
rd <- rd xor sxt(simm)
```

--------------

##### The `addicy` Instruction

**Add Immediate with Carry** --- Add an immediate value and the carry bit to a register.

###### Examples

- `addicy x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00011 | `0100011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
-----------------------------------------------------
rd <- rd\s+sxt(simm)+bit($CC, carry_flag_bit)\16\s;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `subicy` Instruction

**Subtract Immediate with Carry** --- Sutract an immediate value and the carry bit from a register.

###### Examples

- `subicy x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b00010 | `0100010iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
-----------------------------------------------------
rd <- rd\s-sxt(simm)-bit($CC, carry_flag_bit)\16\s;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `lsr` Instruction

**Logical Shift Right** --- Perform a logical shift right on a register by an immediate value.

###### Examples

- `lsr x, 15`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b10010 | `0110010iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rd)]
-------------------
rd <- rd>>imm
```

--------------

##### The `lsl` Instruction

**Logical Shift Left** --- Perform a logical shift left on a register by an immediate value.

###### Examples

- `lsl x, 8`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b01000 | `0101000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rd)]
-------------------
rd <- rd<<imm
```

--------------

##### The `asr` Instruction

**Arithmetic Shift Right** --- Perform an arithmetic shift right on a register by an immediate value.

###### Examples

- `asr x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | 0b10011 | `0110011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rd)]
------------------------------------------------------------
let sign_extension := sxt(bit(rd, 15)-1)<<reg_size_bits-imm;
rd <- rd>>imm or sign_extension
```

--------------

#### Format `ri(2)`


![../assets/ri(2).svg](../assets/ri(2).svg)

### Instruction Format `rrr`


#### Format `rrr`


![../assets/rrr.svg](../assets/rrr.svg)

##### The `mulstep` Instruction

**Unsigned Multiplication Step** --- Computes one step in a full 16-bit by 16-bit unsigned multiplication.

###### Examples

- `mulstep x:y, z`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rrr` = 0b00001 | 0b00 | `0000100tttsssrrr` |

###### Semantics

```
[reg(multiplicand_hi), reg(multiplicand_lo), reg(multiplier)]
-----------------------------------------------------------------
let mask := ~ ((multiplier and 1)-1);
let masked_multiplicand_lo := multiplicand_lo and mask;
let masked_multiplicand_hi := multiplicand_hi and mask;
lo($MP) <- lo($MP)+masked_multiplicand_lo;
hi($MP) <- hi($MP)+masked_multiplicand_hi+attr(cpu/alu/carryout);
let shift_cout := bit(multiplicand_lo, reg_size_bits-1);
multiplicand_lo <- multiplicand_lo<<1;
multiplicand_hi <- multiplicand_hi<<1+shift_cout;
multiplier <- multiplier>>1
```

--------------

### Instruction Format `rr(_)`


#### Format `rr(1)`


![../assets/rr(1).svg](../assets/rr(1).svg)

##### The `add` Instruction

**Add** --- Add the values of two registers.

###### Examples

- `add x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0110 | `0000010110sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rd+rs
```

--------------

##### The `sub` Instruction

**Subtract** --- Subtract the value of one register from another.

###### Examples

- `sub x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0100 | `0000010100sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rd-rs
```

--------------

##### The `and` Instruction

**AND** --- Perform a bitwise AND between two registers.

###### Examples

- `and x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0001 | `0000010001sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rd and rs
```

--------------

##### The `or` Instruction

**OR** --- Perform a bitwise OR between two registers.

###### Examples

- `or x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b00010 | `00000100010sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rd or rs
```

--------------

##### The `xor` Instruction

**XOR** --- Perform a bitwise XOR between two registers.

###### Examples

- `xor x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b00011 | `00000100011sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rd xor rs
```

--------------

##### The `mov` Instruction

**Move** --- Move the value from one register to another.

###### Examples

- `mov x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0000 | `0000010000sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- rs
```

--------------

##### The `addcy` Instruction

**Add with Carry** --- Add the values of two registers with carry.

###### Examples

- `addcy x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0111 | `0000010111sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
-----------------------------------------------------
rd <- rd+rs+bit($CC, carry_flag_bit)\16;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `subcy` Instruction

**Subtract with Carry** --- Subtract the value of one register from another with carry.

###### Examples

- `subcy x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0101 | `0000010101sssrrr` |

###### Semantics

```
[reg(rs), reg(rd)]
-----------------------------------------------------
rd <- rd-rs-bit($CC, carry_flag_bit)\16;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

#### Format `rr(2)`


![../assets/rr(2).svg](../assets/rr(2).svg)

##### The `tl` Instruction

**Test Less-than** --- Test if the value of one register is less than another.

###### Examples

- `tl x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b1111 | `00000011111sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
-------------------------------------
b_push($TS, compare(r1, <(s\16), r2))
```

--------------

##### The `tge` Instruction

**Test Greater-than or Equal** --- Test if the value of one register is greater than or equal to another.

###### Examples

- `tge x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b1101 | `00000011101sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
--------------------------------------
b_push($TS, compare(r1, >=(s\16), r2))
```

--------------

##### The `tb` Instruction

**Test Below** --- Test if the value of one register is below another.

###### Examples

- `tb x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b1110 | `00000011110sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
-------------------------------------
b_push($TS, compare(r1, <(u\16), r2))
```

--------------

##### The `tae` Instruction

**Test Above or Equal** --- Test if the value of one register is above or equal to another.

###### Examples

- `tae x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b1100 | `00000011100sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
--------------------------------------
b_push($TS, compare(r1, >=(u\16), r2))
```

--------------

#### Format `rr(3)`


![../assets/rr(3).svg](../assets/rr(3).svg)

##### The `tne` Instruction

**Test Not Equal** --- Test if the value of one register is not equal to another.

###### Examples

- `tne x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(3)` = 0b00000001 | 0b101 | `00000001101sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
-------------------
b_push($TS, r1\=r2)
```

--------------

##### The `teq` Instruction

**Test Equal** --- Test if the value of one register is equal to another.

###### Examples

- `teq x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(3)` = 0b00000001 | 0b100 | `00000001100sssrrr` |

###### Semantics

```
[reg(r1), reg(r2)]
-------------------
b_push($TS, r1==r2)
```

--------------

### Instruction Format `r(_)`


#### Format `r(1)`


![../assets/r(1).svg](../assets/r(1).svg)

##### The `pushb` Instruction

**Push Byte** --- Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0110 | `0000000010110rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `pushw` Instruction

**Push Word** --- Push a word from a register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0100 | `0000000010100rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `popb` Instruction

**Pop Byte** --- Pop a byte from the stack into a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0111 | `0000000010111rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `popw` Instruction

**Pop Word** --- Pop a word from the stack into a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0101 | `0000000010101rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `callr` Instruction

**Call Register** --- Call a subroutine at the address in a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0001 | `0000000010001rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `jr` Instruction

**Jump Register** --- Jump to the address in a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0000 | `0000000010000rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `neg` Instruction

**Negate** --- Negate the value in a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0010 | `0000000010010rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `seb` Instruction

**Sign Extend Byte** --- Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0011 | `0000000010011rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Format `r(2)`


![../assets/r(2).svg](../assets/r(2).svg)

##### The `rd.mp.lo` Instruction

**Read $MP.lo** --- Read the low word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b100 | `0000000001100rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rd.mp.hi` Instruction

**Read $MP.hi** --- Read the high word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b101 | `0000000001101rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rd.gp` Instruction

**Read $GP** --- Read the value of the system `$GP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b111 | `0000000001111rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `wr.gp` Instruction

**Write $GP** --- Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b110 | `0000000001110rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Format `r(3)`


![../assets/r(3).svg](../assets/r(3).svg)

### Instruction Format `o`


#### Format `o`


![../assets/o.svg](../assets/o.svg)

##### The `NONEXE0` Instruction

**Non-executable (0s Version)** --- Triggers a "non-executable instruction" exception. The entire instruction is 16 `0`s.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00000 | `0000000000000000` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `UNIMPL` Instruction

**Unimplemented** --- Unimplemented instruction.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00110 | `0000000000000110` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `HALT` Instruction

**Halt** --- Halt the processor.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00010 | `0000000000000010` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `BREAK` Instruction

**Breakpoint** --- Trigger a breakpoint.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00111 | `0000000000000111` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `kret` Instruction

**Kernel Return** --- Return from kernel mode.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b000110 | `00000000000000110` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `kcall` Instruction

**Kernel Call** --- Call a kernel function.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b000111 | `00000000000000111` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `ret` Instruction

**Return** --- Return from a subroutine.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00010 | `0000000000000010` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tov` Instruction

**Test Overflow** --- Test for overflow.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01010 | `0000000000001010` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tcy` Instruction

**Test Carry** --- Test for carry.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01011 | `0000000000001011` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `clr.cy` Instruction

**Clear Carry** --- Clear the carry flag.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b000011 | `00000000000000011` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `set.cy` Instruction

**Set Carry** --- Set the carry flag.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b000010 | `00000000000000010` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tpush0` Instruction

**Teststack Push 0** --- Push 0 onto the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b010011 | `00000000000010011` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tpush1` Instruction

**Teststack Push 1** --- Push 1 onto the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b010010 | `00000000000010010` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tnot` Instruction

**Teststack NOT** --- Perform a NOT operation on the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01101 | `0000000000001101` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tand` Instruction

**Teststack AND** --- Perform an AND operation on the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00111 | `0000000000000111` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tor` Instruction

**Teststack OR** --- Perform an OR operation on the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01100 | `0000000000001100` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `tdup` Instruction

**Teststack Duplicate** --- Duplicate the top value on the test stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01000 | `0000000000001000` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `prsv.mp` Instruction

**Preserve $MP** --- Preserve the value of the `$MP` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b11001 | `0000000000011001` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rstr.mp` Instruction

**Restore $MP** --- Restore the value of the `$MP` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b10001 | `0000000000010001` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `prsv.ts` Instruction

**Preserve $TS** --- Preserve the value of the `$TS` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b110000 | `00000000000110000` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rstr.ts` Instruction

**Restore $TS** --- Restore the value of the `$TS` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b100000 | `00000000000100000` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `prsv.ra` Instruction

**Preserve $RA** --- Preserve the value of the `$RA` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b110001 | `00000000000110001` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rstr.ra` Instruction

**Restore $RA** --- Restore the value of the `$RA` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b100001 | `00000000000100001` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `prsv.gp` Instruction

**Preserve $GP** --- Preserve the value of the `$GP` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01101 | `0000000000001101` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rstr.gp` Instruction

**Restore $GP** --- Restore the value of the `$GP` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b01001 | `0000000000001001` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `prsv.cc` Instruction

**Preserve $CC** --- Preserve the value of the `$CC` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00111 | `0000000000000111` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rstr.cc` Instruction

**Restore $CC** --- Restore the value of the `$CC` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b00000000000 | 0b00101 | `0000000000000101` |

###### Semantics

```
[]
----
todo
```

--------------
