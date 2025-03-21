
# Instructions


## Instruction Counts by Format


| Generic format | Description | Available Opcodes | Assigned | Utilization |
|:---:|:----|:---:|:---:|:---:|
| `rri` | Register-register-immediate | 4 | 4 | 100% |
| `subr` | Subroutine Call | 1 | 1 | 100% |
| `b` | Branch | 4 | 3 | 75% |
| `li` | Load Immediate | 2 | 2 | 100% |
| `ri(_)` | Register-immediate | 48 | 22 | 46% |
| `ext` | Reserved for Extension | 4096 | 0 | 0% |
| `rrr` | Register-register-register | 4 | 1 | 25% |
| `rr(_)` | Register-register | 28 | 14 | 50% |
| `r(_)` | Register | 28 | 12 | 43% |
| `o` | Opcode | 32 | 27 | 84% |
|  | **Totals (excluding `ext`)** | **151** | **86** | **57%** |



## Instruction Listing


| `rri` | `subr` | `b` | `li` | `ri(_)` | `rrr` | `rr(_)` | `r(_)` | `o` |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| [`lb`](#the-lb-instruction) | [`call`](#the-call-instruction) | [`b`](#the-b-instruction) | [`li`](#the-li-instruction) | [`lgb`](#the-lgb-instruction) | [`mulstep`](#the-mulstep-instruction) | [`add`](#the-add-instruction) | [`pushb`](#the-pushb-instruction) | [`kret`](#the-kret-instruction) |
| [`lw`](#the-lw-instruction) |  | [`bt`](#the-bt-instruction) | [`szi`](#the-szi-instruction) | [`lgw`](#the-lgw-instruction) |  | [`sub`](#the-sub-instruction) | [`pushw`](#the-pushw-instruction) | [`kcall`](#the-kcall-instruction) |
| [`sb`](#the-sb-instruction) |  | [`bf`](#the-bf-instruction) |  | [`sgb`](#the-sgb-instruction) |  | [`and`](#the-and-instruction) | [`popb`](#the-popb-instruction) | [`ret`](#the-ret-instruction) |
| [`sw`](#the-sw-instruction) |  |  |  | [`sgw`](#the-sgw-instruction) |  | [`or`](#the-or-instruction) | [`popw`](#the-popw-instruction) | [`tov`](#the-tov-instruction) |
|  |  |  |  | [`tbit`](#the-tbit-instruction) |  | [`xor`](#the-xor-instruction) | [`callr`](#the-callr-instruction) | [`tcy`](#the-tcy-instruction) |
|  |  |  |  | [`cbit`](#the-cbit-instruction) |  | [`mov`](#the-mov-instruction) | [`jr`](#the-jr-instruction) | [`clr.cy`](#the-clr.cy-instruction) |
|  |  |  |  | [`sbit`](#the-sbit-instruction) |  | [`addcy`](#the-addcy-instruction) | [`neg`](#the-neg-instruction) | [`set.cy`](#the-set.cy-instruction) |
|  |  |  |  | [`tli`](#the-tli-instruction) |  | [`subcy`](#the-subcy-instruction) | [`seb`](#the-seb-instruction) | [`tpush0`](#the-tpush0-instruction) |
|  |  |  |  | [`tgei`](#the-tgei-instruction) |  | [`tl`](#the-tl-instruction) | [`rd.mp.lo`](#the-rd.mp.lo-instruction) | [`tpush1`](#the-tpush1-instruction) |
|  |  |  |  | [`tbi`](#the-tbi-instruction) |  | [`tge`](#the-tge-instruction) | [`rd.mp.hi`](#the-rd.mp.hi-instruction) | [`tnot`](#the-tnot-instruction) |
|  |  |  |  | [`taei`](#the-taei-instruction) |  | [`tb`](#the-tb-instruction) | [`rd.gp`](#the-rd.gp-instruction) | [`tand`](#the-tand-instruction) |
|  |  |  |  | [`tnei`](#the-tnei-instruction) |  | [`tae`](#the-tae-instruction) | [`wr.gp`](#the-wr.gp-instruction) | [`tor`](#the-tor-instruction) |
|  |  |  |  | [`teqi`](#the-teqi-instruction) |  | [`tne`](#the-tne-instruction) |  | [`tdup`](#the-tdup-instruction) |
|  |  |  |  | [`addi`](#the-addi-instruction) |  | [`teq`](#the-teq-instruction) |  | [`prsv.mp`](#the-prsv.mp-instruction) |
|  |  |  |  | [`andi`](#the-andi-instruction) |  |  |  | [`rstr.mp`](#the-rstr.mp-instruction) |
|  |  |  |  | [`ori`](#the-ori-instruction) |  |  |  | [`prsv.ts`](#the-prsv.ts-instruction) |
|  |  |  |  | [`xori`](#the-xori-instruction) |  |  |  | [`rstr.ts`](#the-rstr.ts-instruction) |
|  |  |  |  | [`addicy`](#the-addicy-instruction) |  |  |  | [`prsv.ra`](#the-prsv.ra-instruction) |
|  |  |  |  | [`subicy`](#the-subicy-instruction) |  |  |  | [`rstr.ra`](#the-rstr.ra-instruction) |
|  |  |  |  | [`lsr`](#the-lsr-instruction) |  |  |  | [`prsv.gp`](#the-prsv.gp-instruction) |
|  |  |  |  | [`lsl`](#the-lsl-instruction) |  |  |  | [`rstr.gp`](#the-rstr.gp-instruction) |
|  |  |  |  | [`asr`](#the-asr-instruction) |  |  |  | [`prsv.cc`](#the-prsv.cc-instruction) |
|  |  |  |  |  |  |  |  | [`rstr.cc`](#the-rstr.cc-instruction) |
|  |  |  |  |  |  |  |  | [`BREAK`](#the-BREAK-instruction) |
|  |  |  |  |  |  |  |  | [`HALT`](#the-HALT-instruction) |
|  |  |  |  |  |  |  |  | [`UNIMPL`](#the-UNIMPL-instruction) |
|  |  |  |  |  |  |  |  | [`NONEXE1`](#the-NONEXE1-instruction) |

## Instruction Format Breakdown


### Instruction Format Layouts


| Format | [Bit Pattern](#legend) | Opcodes Available | Assigned | Utilization | Range of Immediate |
|:----|:---:|:---:|:---:|:---:|:---:|
| `rri` | `00ooiiiiiisssrrr` | 4 | 4 | 100% | `imm6` in `[-32, 31]` or `[0, 63]` |
| `subr` | `010iiiiiiiiiiiii` | 1 | 1 | 100% | `imm13` in `[-4096, 4095]` or `[0, 8191]` |
| `b` | `0110ooiiiiiiiiii` | 4 | 3 | 75% | `imm10` in `[-512, 511]` or `[0, 1023]` |
| `li` | `0111oiiiiiiiirrr` | 2 | 2 | 100% | `imm8` in `[-128, 127]` or `[0, 255]` |
| `ri(1)` | `10oooooiiiiiirrr` | 32 | 22 | 69% | `imm6` in `[-32, 31]` or `[0, 63]` |
| `ri(2)` | `110ooooiiiiiirrr` | 16 | 0 | 0% | `imm6` in `[-32, 31]` or `[0, 63]` |
| `ext` | `1110oooooooooooo` | 4096 | 0 | 0% |  |
| `rrr` | `11110ootttsssrrr` | 4 | 1 | 25% |  |
| `rr(1)` | `111110oooosssrrr` | 16 | 8 | 50% |  |
| `rr(2)` | `1111110ooosssrrr` | 8 | 4 | 50% |  |
| `rr(3)` | `11111110oosssrrr` | 4 | 2 | 50% |  |
| `r(1)` | `111111110oooorrr` | 16 | 7 | 44% |  |
| `r(2)` | `1111111110ooorrr` | 8 | 4 | 50% |  |
| `r(3)` | `11111111110oorrr` | 4 | 1 | 25% |  |
| `o` | `11111111111ooooo` | 32 | 27 | 84% |  |

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
| `rri` = 0b00 | 0b11 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `rri` = 0b00 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `rri` = 0b00 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `rri` = 0b00 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `subr` = 0b010 | NONE |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `010iiiiiiiiiiiii` | 13 | `imm13` in `[-4096, 4095]` or `[0, 8191]` |

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
| `b` = 0b0110 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01101iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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
| `b` = 0b0110 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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
| `b` = 0b0110 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011010iiiiiiiiii` | 10 | `imm10` in `[-512, 511]` or `[0, 1023]` |

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
| `li` = 0b0111 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01111iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

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
| `li` = 0b0111 | 0b0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01110iiiiiiiirrr` | 8 | `imm8` in `[-128, 127]` or `[0, 255]` |

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
| `ri(1)` = 0b10 | 0b11011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1011011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b10011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b01011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b00011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000011iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `10010iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b10110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b00110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b1001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b1101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b00001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b10001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010001iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b00101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b10101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010101iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b1111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b01100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b11100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1011100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b0100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100100iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b00111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b10111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010111iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b0000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `100000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b1000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101000iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

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
| `ri(1)` = 0b10 | 0b1110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `101110iiiiiirrr` | 6 | `imm6` in `[-32, 31]` or `[0, 63]` |

###### Semantics

```
[imm(imm), reg(rd)]
--------------------------------------------------------------
let sign_extension := sxt(bit(rd, 15)-1)<<(reg_size_bits-imm);
rd <- or(rd>>imm, sign_extension)
```

--------------

#### Format `ri(2)`


### Instruction Format `ext`


![../assets/ext.svg](../assets/ext.svg)
