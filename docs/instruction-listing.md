
# Instructions


## Instruction Counts by Format


| Generic format | Description | Available Opcodes | Assigned | Utilization |
|:---:|:----|:---:|:---:|:---:|
| [`rri`](#instruction-format-rri) | Register-register-immediate | 4 | 4 | 100% |
| [`subr`](#instruction-format-subr) | Subroutine Call | 1 | 1 | 100% |
| [`b`](#instruction-format-b) | Branch | 4 | 3 | 75% |
| [`li`](#instruction-format-li) | Load Immediate | 2 | 2 | 100% |
| [`ri(_)`](#instruction-format-ri) | Register-immediate | 48 | 22 | 46% |
| [`ext`](#instruction-format-ext) | Reserved for Extension | 4096 | 0 | 0% |
| [`rrr`](#instruction-format-rrr) | Register-register-register | 4 | 1 | 25% |
| [`rr(_)`](#instruction-format-rr) | Register-register | 28 | 14 | 50% |
| [`r(_)`](#instruction-format-r) | Register | 28 | 12 | 43% |
| [`o`](#instruction-format-o) | Opcode | 32 | 27 | 84% |
|  | **Totals (excluding `ext`)** | **151** | **86** | **57%** |



## Instruction Listing


| `rri` | `subr` | `b` | `li` | `ri(_)` | `rrr` | `rr(_)` | `r(_)` | `o` |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| [`lb`](#the-lb-instruction) | [`call`](#the-call-instruction) | [`b`](#the-b-instruction) | [`li`](#the-li-instruction) | [`lgb`](#the-lgb-instruction) | [`mulstep`](#the-mulstep-instruction) | [`add`](#the-add-instruction) | [`pushb`](#the-pushb-instruction) | [`NONEXE0`](#the-NONEXE0-instruction) |
| [`lw`](#the-lw-instruction) |  | [`bt`](#the-bt-instruction) | [`szi`](#the-szi-instruction) | [`lgw`](#the-lgw-instruction) |  | [`sub`](#the-sub-instruction) | [`pushw`](#the-pushw-instruction) | [`UNIMPL`](#the-UNIMPL-instruction) |
| [`sb`](#the-sb-instruction) |  | [`bf`](#the-bf-instruction) |  | [`sgb`](#the-sgb-instruction) |  | [`and`](#the-and-instruction) | [`popb`](#the-popb-instruction) | [`HALT`](#the-HALT-instruction) |
| [`sw`](#the-sw-instruction) |  |  |  | [`sgw`](#the-sgw-instruction) |  | [`or`](#the-or-instruction) | [`popw`](#the-popw-instruction) | [`BREAK`](#the-BREAK-instruction) |
|  |  |  |  | [`tbit`](#the-tbit-instruction) |  | [`xor`](#the-xor-instruction) | [`callr`](#the-callr-instruction) | [`kret`](#the-kret-instruction) |
|  |  |  |  | [`cbit`](#the-cbit-instruction) |  | [`mov`](#the-mov-instruction) | [`jr`](#the-jr-instruction) | [`kcall`](#the-kcall-instruction) |
|  |  |  |  | [`sbit`](#the-sbit-instruction) |  | [`addcy`](#the-addcy-instruction) | [`neg`](#the-neg-instruction) | [`ret`](#the-ret-instruction) |
|  |  |  |  | [`tli`](#the-tli-instruction) |  | [`subcy`](#the-subcy-instruction) | [`seb`](#the-seb-instruction) | [`tov`](#the-tov-instruction) |
|  |  |  |  | [`tgei`](#the-tgei-instruction) |  | [`tl`](#the-tl-instruction) | [`rd.mp.lo`](#the-rd.mp.lo-instruction) | [`tcy`](#the-tcy-instruction) |
|  |  |  |  | [`tbi`](#the-tbi-instruction) |  | [`tge`](#the-tge-instruction) | [`rd.mp.hi`](#the-rd.mp.hi-instruction) | [`clr.cy`](#the-clr.cy-instruction) |
|  |  |  |  | [`taei`](#the-taei-instruction) |  | [`tb`](#the-tb-instruction) | [`rd.gp`](#the-rd.gp-instruction) | [`set.cy`](#the-set.cy-instruction) |
|  |  |  |  | [`tnei`](#the-tnei-instruction) |  | [`tae`](#the-tae-instruction) | [`wr.gp`](#the-wr.gp-instruction) | [`tpush0`](#the-tpush0-instruction) |
|  |  |  |  | [`teqi`](#the-teqi-instruction) |  | [`tne`](#the-tne-instruction) |  | [`tpush1`](#the-tpush1-instruction) |
|  |  |  |  | [`addi`](#the-addi-instruction) |  | [`teq`](#the-teq-instruction) |  | [`tnot`](#the-tnot-instruction) |
|  |  |  |  | [`andi`](#the-andi-instruction) |  |  |  | [`tand`](#the-tand-instruction) |
|  |  |  |  | [`ori`](#the-ori-instruction) |  |  |  | [`tor`](#the-tor-instruction) |
|  |  |  |  | [`xori`](#the-xori-instruction) |  |  |  | [`tdup`](#the-tdup-instruction) |
|  |  |  |  | [`addicy`](#the-addicy-instruction) |  |  |  | [`prsv.mp`](#the-prsv.mp-instruction) |
|  |  |  |  | [`subicy`](#the-subicy-instruction) |  |  |  | [`rstr.mp`](#the-rstr.mp-instruction) |
|  |  |  |  | [`lsr`](#the-lsr-instruction) |  |  |  | [`prsv.ts`](#the-prsv.ts-instruction) |
|  |  |  |  | [`lsl`](#the-lsl-instruction) |  |  |  | [`rstr.ts`](#the-rstr.ts-instruction) |
|  |  |  |  | [`asr`](#the-asr-instruction) |  |  |  | [`prsv.ra`](#the-prsv.ra-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.ra`](#the-rstr.ra-instruction) |
|  |  |  |  |  |  |  |  | [`prsv.gp`](#the-prsv.gp-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.gp`](#the-rstr.gp-instruction) |
|  |  |  |  |  |  |  |  | [`prsv.cc`](#the-prsv.cc-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.cc`](#the-rstr.cc-instruction) |

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
| [`r(1)`](#format-r1) | `000000001oooorrr` | 16 | 7 | 44% |  |
| [`r(2)`](#format-r2) | `0000000001ooorrr` | 8 | 4 | 50% |  |
| [`r(3)`](#format-r3) | `00000000001oorrr` | 4 | 1 | 25% |  |
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


![../assets/rri.svg](../assets/rri.svg)

#### Format `rri`


##### The `lb` Instruction

**Load Byte** --- Load a byte from memory into a register.

###### Examples

- `lb w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b11 | 0b11 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs), reg(rd)]
------------------------------
let ptr := rs\s+sxt(simm)\u;
rd <- zxt([ptr])
```

--------------

##### The `lw` Instruction

**Load Word** --- Load a word from memory into a register.

###### Examples

- `lw w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b11 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rs), reg(rd)]
----------------------------------------
let ptr := and(rs\s+sxt(simm), 65534)\u;
rd <- {[ptr+1], [ptr]}
```

--------------

##### The `sb` Instruction

**Store Byte** --- Store a byte from a register into memory.

###### Examples

- `sb [sp-20], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b11 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b11 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd), reg(rs)]
----------------------------------------
let ptr := and(rd\s+sxt(simm), 65534)\u;
[ptr] <- lo(rs);
[ptr+1] <- hi(rs)
```

--------------

### Instruction Format `subr`


![../assets/subr.svg](../assets/subr.svg)

#### Format `subr`


##### The `call` Instruction

**Call Subroutine** --- Call a subroutine at the specified address.

###### Examples

- `call SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `subr` = 0b101 | NONE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101iiiiiiiiiiiii` | 13 | `imm13` in `[-4096, 4095]` or `[0, 8191]` |

###### Semantics

```
[simm(simm)]
-----------------------------------
$PC <- $PC\s+sxt(simm)<<subr_align;
$RA <- $PC+2
```

--------------

### Instruction Format `b`


![../assets/b.svg](../assets/b.svg)

#### Format `b`


##### The `b` Instruction

**Branch** --- Branch to the specified address by adding the immediate offset to `$PC`.

###### Examples

- `b SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b1001 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10011iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b1001 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100100iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b1001 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100110iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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


![../assets/li.svg](../assets/li.svg)

#### Format `li`


##### The `li` Instruction

**Load Immediate** --- Load an immediate value into a register.

###### Examples

- `li x, 123`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b1000 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10001iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b1000 | 0b0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10000iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

###### Semantics

```
[imm(imm), reg(rd)]
-------------------------
rd <- or(rd<<8, zxt(imm))
```

--------------

### Instruction Format `ri(_)`


![../assets/ri(_).svg](../assets/ri(_).svg)

#### Format `ri(1)`


##### The `lgb` Instruction

**Load Global Byte** --- Load a byte from a memory address offset from `$GP`.

###### Examples

- `lgb x, [gp+8]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b11011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0111011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b10011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0110011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rd)]
-----------------------------------------
let ptr := and($GP\u+zxt(disp), 65534)\u;
rd <- {[ptr+1], [ptr]}
```

--------------

##### The `sgb` Instruction

**Store Global Byte** --- Store a byte into memory address offset from `$GP`.

###### Examples

- `sgb [gp+8], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b01011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0101011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b00011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0100011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(disp), reg(rs)]
-----------------------------------------
let ptr := and($GP\u+zxt(disp), 65534)\u;
{[ptr+1], [ptr]} <- rs
```

--------------

##### The `tbit` Instruction

**Test Bit** --- Test a specific bit in a register, modifying `$TS`.

###### Examples

- `tbit 12, w`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01010iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rs)]
-------------------------------------
let shamt := bitslice(bit_idx, 3..0);
let bit := and(rs>>shamt\u, 1);
b_push($TS, bit==1)
```

--------------

##### The `cbit` Instruction

**Clear Bit** --- Clear a specific bit in a register.

###### Examples

- `cbit 9, v`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b10110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0110110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~(1<<idx);
rd <- and(rd, mask)
```

--------------

##### The `sbit` Instruction

**Set Bit** --- Set a specific bit in a register.

###### Examples

- `sbit 15, a`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b00110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0100110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~(1<<idx);
rd <- or(rd, mask)
```

--------------

##### The `tli` Instruction

**Test Less-than Immediate** --- Test if a register value is less than an immediate value.

###### Examples

- `tli x, -5`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b1001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b1101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b00001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0100001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b10001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0110001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b00101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0100101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b10101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0110101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b1111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b01100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0101100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
------------------------
rd <- and(rd, sxt(simm))
```

--------------

##### The `ori` Instruction

**OR Immediate** --- Perform a bitwise OR between a register and an immediate value.

###### Examples

- `ori x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b11100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0111100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[simm(simm), reg(rd)]
-----------------------
rd <- or(rd, sxt(simm))
```

--------------

##### The `xori` Instruction

**XOR Immediate** --- Perform a bitwise XOR between a register and an immediate value.

###### Examples

- `xori x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b0100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b00111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0100111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b10111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0110111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b0000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b1000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b01 | 0b1110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rd)]
--------------------------------------------------------------
let sign_extension := sxt(bit(rd, 15)-1)<<(reg_size_bits-imm);
rd <- or(rd>>imm, sign_extension)
```

--------------

#### Format `ri(2)`


### Instruction Format `rrr`


![../assets/rrr.svg](../assets/rrr.svg)

#### Format `rrr`


##### The `mulstep` Instruction

**Unsigned Multiplication Step** --- Computes one step in a full 16-bit by 16-bit unsigned multiplication.

###### Examples

- `mulstep x:y, z`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rrr` = 0b00001 | 0b |

| Bit Layout |
|:---:|
| `00001rrr` |

###### Semantics

```
[reg(multiplicand_hi), reg(multiplicand_lo), reg(multiplier)]
-----------------------------------------------------------------
let mask := ~(and(multiplier, 1)-1);
masked_multiplicand_lo <- and(multiplicand_lo, mask);
masked_multiplicand_hi <- and(multiplicand_hi, mask);
lo($MP) <- lo($MP)+masked_multiplicand_lo;
hi($MP) <- hi($MP)+masked_multiplicand_hi+attr(cpu/alu/carryout);
let shift_cout := bit(multiplicand_lo, reg_size_bits-1);
multiplicand_lo <- multiplicand_lo<<1;
multiplicand_hi <- multiplicand_hi<<1+shift_cout;
multiplier <- multiplier>>1
```

--------------

### Instruction Format `rr(_)`


![../assets/rr(_).svg](../assets/rr(_).svg)

#### Format `rr(1)`


##### The `add` Instruction

**Add** --- Add the values of two registers.

###### Examples

- `add x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0101 |

| Bit Layout |
|:---:|
| `0000010101rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b1101 |

| Bit Layout |
|:---:|
| `0000011101rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b00111 |

| Bit Layout |
|:---:|
| `00000100111rrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- and(rd, rs)
```

--------------

##### The `or` Instruction

**OR** --- Perform a bitwise OR between two registers.

###### Examples

- `or x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b10111 |

| Bit Layout |
|:---:|
| `00000110111rrr` |

###### Semantics

```
[reg(rs), reg(rd)]
------------------
rd <- or(rd, rs)
```

--------------

##### The `xor` Instruction

**XOR** --- Perform a bitwise XOR between two registers.

###### Examples

- `xor x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b011 |

| Bit Layout |
|:---:|
| `000001011rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b1111 |

| Bit Layout |
|:---:|
| `0000011111rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b0001 |

| Bit Layout |
|:---:|
| `0000010001rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b000001 | 0b1001 |

| Bit Layout |
|:---:|
| `0000011001rrr` |

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


##### The `tl` Instruction

**Test Less-than** --- Test if the value of one register is less than another.

###### Examples

- `tl x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b100 |

| Bit Layout |
|:---:|
| `0000001100rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b110 |

| Bit Layout |
|:---:|
| `0000001110rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b0000 |

| Bit Layout |
|:---:|
| `00000010000rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b0000001 | 0b1000 |

| Bit Layout |
|:---:|
| `00000011000rrr` |

###### Semantics

```
[reg(r1), reg(r2)]
--------------------------------------
b_push($TS, compare(r1, >=(u\16), r2))
```

--------------

#### Format `rr(3)`


##### The `tne` Instruction

**Test Not Equal** --- Test if the value of one register is not equal to another.

###### Examples

- `tne x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b00000001 | 0b0010 |

| Bit Layout |
|:---:|
| `000000010010rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b00000001 | 0b1010 |

| Bit Layout |
|:---:|
| `000000011010rrr` |

###### Semantics

```
[reg(r1), reg(r2)]
-------------------
b_push($TS, r1==r2)
```

--------------

### Instruction Format `r(_)`


![../assets/r(_).svg](../assets/r(_).svg)

#### Format `r(1)`


##### The `pushb` Instruction

**Push Byte** --- Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b010 |

| Bit Layout |
|:---:|
| `000000001010rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b000 |

| Bit Layout |
|:---:|
| `000000001000rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b110 |

| Bit Layout |
|:---:|
| `000000001110rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b100 |

| Bit Layout |
|:---:|
| `000000001100rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b0011 |

| Bit Layout |
|:---:|
| `0000000010011rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b1011 |

| Bit Layout |
|:---:|
| `0000000011011rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b000000001 | 0b1111 |

| Bit Layout |
|:---:|
| `0000000011111rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Format `r(2)`


##### The `seb` Instruction

**Sign Extend Byte** --- Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b0111 |

| Bit Layout |
|:---:|
| `00000000010111rrr` |

###### Semantics

```
[]
----
todo
```

--------------

##### The `rd.mp.lo` Instruction

**Read $MP.lo** --- Read the low word in the system `$MP` register into a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b0001 |

| Bit Layout |
|:---:|
| `00000000010001rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b1001 |

| Bit Layout |
|:---:|
| `00000000011001rrr` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `r(2)` = 0b0000000001 | 0b1101 |

| Bit Layout |
|:---:|
| `00000000011101rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Format `r(3)`


##### The `wr.gp` Instruction

**Write $GP** --- Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(3)` = 0b00000000001 | 0b0101 |

| Bit Layout |
|:---:|
| `000000000010101rrr` |

###### Semantics

```
[]
----
todo
```

--------------

### Instruction Format `o`


![../assets/o.svg](../assets/o.svg)

#### Format `o`


##### The `NONEXE0` Instruction

**Non-executable (0s Version)** --- Triggers a "non-executable instruction" exception. The entire instruction is 16 `0`s.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b11011 |

| Bit Layout |
|:---:|
| `0000000000011011` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b00011 |

| Bit Layout |
|:---:|
| `0000000000000011` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b01011 |

| Bit Layout |
|:---:|
| `0000000000001011` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b10011 |

| Bit Layout |
|:---:|
| `0000000000010011` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b101111 |

| Bit Layout |
|:---:|
| `00000000000101111` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b11111 |

| Bit Layout |
|:---:|
| `0000000000011111` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b001111 |

| Bit Layout |
|:---:|
| `00000000000001111` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b01110 |

| Bit Layout |
|:---:|
| `0000000000001110` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b11110 |

| Bit Layout |
|:---:|
| `0000000000011110` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b10111 |

| Bit Layout |
|:---:|
| `0000000000010111` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b00111 |

| Bit Layout |
|:---:|
| `0000000000000111` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b1000 |

| Bit Layout |
|:---:|
| `000000000001000` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b0000 |

| Bit Layout |
|:---:|
| `000000000000000` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b01010 |

| Bit Layout |
|:---:|
| `0000000000001010` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b11010 |

| Bit Layout |
|:---:|
| `0000000000011010` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b0010 |

| Bit Layout |
|:---:|
| `000000000000010` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b100 |

| Bit Layout |
|:---:|
| `00000000000100` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b01101 |

| Bit Layout |
|:---:|
| `0000000000001101` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b01001 |

| Bit Layout |
|:---:|
| `0000000000001001` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b10110 |

| Bit Layout |
|:---:|
| `0000000000010110` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b00110 |

| Bit Layout |
|:---:|
| `0000000000000110` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b0101 |

| Bit Layout |
|:---:|
| `000000000000101` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b0001 |

| Bit Layout |
|:---:|
| `000000000000001` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b011101 |

| Bit Layout |
|:---:|
| `00000000000011101` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b011001 |

| Bit Layout |
|:---:|
| `00000000000011001` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b111101 |

| Bit Layout |
|:---:|
| `00000000000111101` |

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


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b00000000000 | 0b111001 |

| Bit Layout |
|:---:|
| `00000000000111001` |

###### Semantics

```
[]
----
todo
```

--------------
