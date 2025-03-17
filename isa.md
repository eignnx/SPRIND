
# SPRIND Instruction Set Architecture Specification


## Machine Overview


### General Purpose Registers


| Register Name | [Uses](#register-uses-and-calling-convention) |
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

## Instructions


### Instruction Counts by Format


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



### Instruction Listing


| `rri` | `subr` | `b` | `li` | `ri(_)` | `rrr` | `rr(_)` | `r(_)` | `o` |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| [`lb`](#the-lb-instruction) | [`call`](#the-call-instruction) | [`b`](#the-b-instruction) | [`li`](#the-li-instruction) | [`lgb`](#the-lgb-instruction) | [`mulstep`](#the-mulstep-instruction) | [`add`](#the-add-instruction) | [`pushb`](#the-pushb-instruction) | [`NONEXE1`](#the-NONEXE1-instruction) |
| [`lw`](#the-lw-instruction) |  | [`bt`](#the-bt-instruction) | [`szi`](#the-szi-instruction) | [`lgw`](#the-lgw-instruction) |  | [`sub`](#the-sub-instruction) | [`pushw`](#the-pushw-instruction) | [`BREAK`](#the-BREAK-instruction) |
| [`sb`](#the-sb-instruction) |  | [`bf`](#the-bf-instruction) |  | [`sgb`](#the-sgb-instruction) |  | [`and`](#the-and-instruction) | [`popb`](#the-popb-instruction) | [`HALT`](#the-HALT-instruction) |
| [`sw`](#the-sw-instruction) |  |  |  | [`sgw`](#the-sgw-instruction) |  | [`or`](#the-or-instruction) | [`popw`](#the-popw-instruction) | [`UNIMPL`](#the-UNIMPL-instruction) |
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

### Instruction Format Breakdown


#### Instruction Format Layouts


| Format | [Bit Pattern](#legend) | Opcodes Available | Assigned | Utilization | Range of Immediate |
|:----|:---:|:---:|:---:|:---:|:---:|
| `rri` | `00ooiiiiiisssrrr` | 4 | 4 | 100% | `imm6` in `[-32,31]` or `[0,63]` |
| `subr` | `010iiiiiiiiiiiii` | 1 | 1 | 100% | `imm13` in `[-4096,4095]` or `[0,8191]` |
| `b` | `0110ooiiiiiiiiii` | 4 | 3 | 75% | `imm10` in `[-512,511]` or `[0,1023]` |
| `li` | `0111oiiiiiiiirrr` | 2 | 2 | 100% | `imm8` in `[-128,127]` or `[0,255]` |
| `ri(1)` | `10oooooiiiiiirrr` | 32 | 22 | 69% | `imm6` in `[-32,31]` or `[0,63]` |
| `ri(2)` | `110ooooiiiiiirrr` | 16 | 0 | 0% | `imm6` in `[-32,31]` or `[0,63]` |
| `ext` | `1110oooooooooooo` | 4096 | 0 | 0% |  |
| `rrr` | `11110ootttsssrrr` | 4 | 1 | 25% |  |
| `rr(1)` | `111110oooosssrrr` | 16 | 8 | 50% |  |
| `rr(2)` | `1111110ooosssrrr` | 8 | 4 | 50% |  |
| `rr(3)` | `11111110oosssrrr` | 4 | 2 | 50% |  |
| `r(1)` | `111111110oooorrr` | 16 | 7 | 44% |  |
| `r(2)` | `1111111110ooorrr` | 8 | 4 | 50% |  |
| `r(3)` | `11111111110oorrr` | 4 | 1 | 25% |  |
| `o` | `11111111111ooooo` | 32 | 27 | 84% |  |

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


### Instruction Specifications


#### Instruction Format `rri`


##### The `lb` Instruction

**Load Byte** --- Load a byte from memory into a register.

###### Examples

- `lb w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b00 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0000iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs),reg(?rd)]
-------------------------------
?ptr = ?rs/s + sxt(?simm)/u;
?rd <- zxt([?ptr])
```

--------------

##### The `lw` Instruction

**Load Word** --- Load a word from memory into a register.

###### Examples

- `lw w, [sp+12]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b00 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0001iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs),reg(?rd)]
-----------------------------------------
?ptr = ?rs/s + sxt(?simm)/u  and  #65534;
?rd <- {[?ptr + #1],[?ptr]}
```

--------------

##### The `sb` Instruction

**Store Byte** --- Store a byte from a register into memory.

###### Examples

- `sb [sp-20], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b00 | 0b10 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0010iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rs)]
-------------------------------
[?adr + ?simm] <- ?rs
```

--------------

##### The `sw` Instruction

**Store Word** --- Store a word from a register into memory.

###### Examples

- `sw [sp-20], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rri` = 0b00 | 0b11 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `0011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?simm),reg(?adr),reg(?rs)]
---------------------------------
?ptr = ?adr + ?simm  and  #65534;
[?ptr] <- lo(?rs);
[?ptr + #1] <- hi(?rs)
```

--------------

#### Instruction Format `subr`


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
| `010iiiiiiiiiiiii` | 13 | `imm13` in `[-4096,4095]` or `[0,8191]` |

###### Semantics

```
[imm(?imm)]
----------------------------------------
$$pc <- $$pc + sxt(?imm) << #subr_align;
$$ra <- $$pc + #2
```

--------------

#### Instruction Format `b`


##### The `b` Instruction

**Branch** --- Branch to the specified address by adding the immediate offset to `$PC`.

###### Examples

- `b SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b00 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011000iiiiiiiiii` | 10 | `imm10` in `[-512,511]` or `[0,1023]` |

###### Semantics

```
[imm(?offset)]
---------------------------
$$pc <- $$pc + sxt(?offset)
```

--------------

##### The `bt` Instruction

**Branch If True** --- Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Examples

- `bt SOME_LABEL`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `b` = 0b0110 | 0b01 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `011001iiiiiiiiii` | 10 | `imm10` in `[-512,511]` or `[0,1023]` |

###### Semantics

```
[imm(?offset)]
-------------------------------
if b_pop($$ts) == #1 {
    $$pc <- $$pc + sxt(?offset)
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
| `011010iiiiiiiiii` | 10 | `imm10` in `[-512,511]` or `[0,1023]` |

###### Semantics

```
[imm(?offset)]
-------------------------------
if b_pop($$ts) == #0 {
    $$pc <- $$pc + sxt(?offset)
}
```

--------------

#### Instruction Format `li`


##### The `li` Instruction

**Load Immediate** --- Load an immediate value into a register.

###### Examples

- `li x, 123`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b0111 | 0b0 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01110iiiiiiiirrr` | 8 | `imm8` in `[-128,127]` or `[0,255]` |

###### Semantics

```
[imm(?simm),reg(?rd)]
---------------------
?rd <- sxt(?simm)
```

--------------

##### The `szi` Instruction

**Shift Zero-extended Immediate** --- Left-shift a zero-extended immediate value into a register.

###### Examples

- `szi x, 0xB3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `li` = 0b0111 | 0b1 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `01111iiiiiiiirrr` | 8 | `imm8` in `[-128,127]` or `[0,255]` |

###### Semantics

```
[imm(?imm),reg(?rd)]
-------------------------------
?rd <- ?rd << #8  or  zxt(?imm)
```

--------------

#### Instruction Format `ri(1)`


##### The `lgb` Instruction

**Load Global Byte** --- Load a byte from a memory address offset from `$GP`.

###### Examples

- `lgb x, [gp+8]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000000iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?disp),reg(?rd)]
-------------------------------
?rd <- zxt([$$gp + zxt(?disp)])
```

--------------

##### The `lgw` Instruction

**Load Global Word** --- Load a word from a memory address offset from `$GP`.

###### Examples

- `lgw x, [gp+8]`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000001iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?disp),reg(?rd)]
--------------------------------------
?ptr = $$gp + zxt(?disp)  and  #65534;
?rd <- hi_lo([?ptr + #1],[?ptr])
```

--------------

##### The `sgb` Instruction

**Store Global Byte** --- Store a byte into memory address offset from `$GP`.

###### Examples

- `sgb [gp+8], x`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000010iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?disp),reg(?rs)]
--------------------------
[$$gp + zxt(?disp)] <- ?rs
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
| `1000011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?disp),reg(?rs)]
--------------------------------------
?ptr = $$gp + zxt(?disp)  and  #65534;
hi_lo([?ptr + #1],[?ptr]) <- ?rs
```

--------------

##### The `tbit` Instruction

**Test Bit** --- Test a specific bit in a register, modifying `$TS`.

###### Examples

- `tbit 12, w`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000100iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?bit_idx),reg(?rs)]
------------------------------------------------------------
b_push($$ts,?rs >> bitslice(?bit_idx,#3..#0)  and  #1 == #1)
```

--------------

##### The `cbit` Instruction

**Clear Bit** --- Clear a specific bit in a register.

###### Examples

- `cbit 9, v`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000101iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?bit_idx),reg(?rd)]
---------------------------------------------------
?rd <- ?rd  and  ~(#1 << bitslice(?bit_idx,#3..#0))
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
| `1000110iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?bit_idx),reg(?rd)]
-----------------------------------------------
?rd <- ?rd  or  #1 << bitslice(?bit_idx,#3..#0)
```

--------------

##### The `tli` Instruction

**Test Less-than Immediate** --- Test if a register value is less than an immediate value.

###### Examples

- `tli x, -5`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b00111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1000111iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs)]
---------------------------------------------
b_push($$ts,compare(?rs,<(s(16)),sxt(?simm)))
```

--------------

##### The `tgei` Instruction

**Test Greater-than or Equal Immediate** --- Test if a register value is greater than or equal to an immediate value.

###### Examples

- `tgei x, -5`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001000iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs)]
----------------------------------------------
b_push($$ts,compare(?rs,>=(s(16)),sxt(?simm)))
```

--------------

##### The `tbi` Instruction

**Test Below Immediate** --- Test if a register value is below an immediate value.

###### Examples

- `tbi x, 10`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001001iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?imm),reg(?rs)]
--------------------------------------------
b_push($$ts,compare(?rs,<(u(16)),zxt(?imm)))
```

--------------

##### The `taei` Instruction

**Test Above or Equal** --- Test if a register value is above or equal to an immediate value.

###### Examples

- `taei x, 10`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001010iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?imm),reg(?rs)]
---------------------------------------------
b_push($$ts,compare(?rs,>=(u(16)),zxt(?imm)))
```

--------------

##### The `tnei` Instruction

**Test Not Equal Immediate** --- Test if a register value is not equal to an immediate value.

###### Examples

- `tnei x, 0`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs)]
------------------------------
b_push($$ts,?rs != sxt(?simm))
```

--------------

##### The `teqi` Instruction

**Test Equal Immediate** --- Test if a register value is equal to an immediate value.

###### Examples

- `teqi x, -5`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001100iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rs)]
------------------------------
b_push($$ts,?rs == sxt(?simm))
```

--------------

##### The `addi` Instruction

**Add Immediate** --- Add an immediate value to a register.

###### Examples

- `addi x, -5`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001101iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
-----------------------
?rd <- ?rd + sxt(?simm)
```

--------------

##### The `andi` Instruction

**AND Immediate** --- Perform a bitwise AND between a register and an immediate value.

###### Examples

- `andi x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001110iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
---------------------------
?rd <- ?rd  and  sxt(?simm)
```

--------------

##### The `ori` Instruction

**OR Immediate** --- Perform a bitwise OR between a register and an immediate value.

###### Examples

- `ori x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001111iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
--------------------------
?rd <- ?rd  or  sxt(?simm)
```

--------------

##### The `xori` Instruction

**XOR Immediate** --- Perform a bitwise XOR between a register and an immediate value.

###### Examples

- `xori x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010000iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
---------------------------
?rd <- ?rd  xor  sxt(?simm)
```

--------------

##### The `addicy` Instruction

**Add Immediate with Carry** --- Add an immediate value and the carry bit to a register.

###### Examples

- `addicy x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010001iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
------------------------------------------------------
?rd <- ?rd + sxt(?simm) + bit($$cc,#carry_flag_bit);
bit($$cc,#carry_flag_bit) <- attr(cpu/alu/carryout);
bit($$cc,#overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `subicy` Instruction

**Subtract Immediate with Carry** --- Sutract an immediate value and the carry bit from a register.

###### Examples

- `subicy x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010010iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[simm(?simm),reg(?rd)]
------------------------------------------------------
?rd <- ?rd - sxt(?simm) - bit($$cc,#carry_flag_bit);
bit($$cc,#carry_flag_bit) <- attr(cpu/alu/carryout);
bit($$cc,#overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `lsr` Instruction

**Logical Shift Right** --- Perform a logical shift right on a register by an immediate value.

###### Examples

- `lsr x, 15`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?imm),reg(?rd)]
--------------------
?rd <- ?rd >> ?imm
```

--------------

##### The `lsl` Instruction

**Logical Shift Left** --- Perform a logical shift left on a register by an immediate value.

###### Examples

- `lsl x, 8`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010100iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?imm),reg(?rd)]
--------------------
?rd <- ?rd << ?imm
```

--------------

##### The `asr` Instruction

**Arithmetic Shift Right** --- Perform an arithmetic shift right on a register by an immediate value.

###### Examples

- `asr x, 3`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010101iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[imm(?imm),reg(?rd)]
------------------------------------------------------------------
?sign_extension = sxt(bit(?rd,#15) - #1) << #reg_size_bits - ?imm;
?rd <- ?rd >> ?imm  or  ?sign_extension
```

--------------

#### Instruction Format `ri(2)`


#### Instruction Format `ext`


#### Instruction Format `rrr`


##### The `mulstep` Instruction

**Unsigned Multiplication Step** --- Computes one step in a full 16-bit by 16-bit unsigned multiplication.

###### Examples

- `mulstep x:y, z`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rrr` = 0b11110 | 0b00 |

| Bit Layout |
|:---:|
| `1111000rrr` |

###### Semantics

```
[reg(?multiplicand_hi),reg(?multiplicand_lo),reg(?multiplier)]
------------------------------------------------------------------------
?mask = ~(?multiplier  and  #1 - #1);
?masked_multiplicand_lo <- ?multiplicand_lo  and  ?mask;
?masked_multiplicand_hi <- ?multiplicand_hi  and  ?mask;
lo($$mp) <- lo($$mp) + ?masked_multiplicand_lo;
hi($$mp) <- hi($$mp) + ?masked_multiplicand_hi + attr(cpu/alu/carryout);
?shift_cout = bit(?multiplicand_lo,#reg_size_bits - #1);
?multiplicand_lo <- ?multiplicand_lo << #1;
?multiplicand_hi <- ?multiplicand_hi << #1 + ?shift_cout;
?multiplier <- ?multiplier >> #1
```

--------------

#### Instruction Format `rr(1)`


##### The `add` Instruction

**Add** --- Add the values of two registers.

###### Examples

- `add x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0000 |

| Bit Layout |
|:---:|
| `1111100000rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
-------------------
?rd <- ?rd + ?rs
```

--------------

##### The `sub` Instruction

**Subtract** --- Subtract the value of one register from another.

###### Examples

- `sub x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0001 |

| Bit Layout |
|:---:|
| `1111100001rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
-------------------
?rd <- ?rd - ?rs
```

--------------

##### The `and` Instruction

**AND** --- Perform a bitwise AND between two registers.

###### Examples

- `and x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0010 |

| Bit Layout |
|:---:|
| `1111100010rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
--------------------
?rd <- ?rd  and  ?rs
```

--------------

##### The `or` Instruction

**OR** --- Perform a bitwise OR between two registers.

###### Examples

- `or x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0011 |

| Bit Layout |
|:---:|
| `1111100011rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
-------------------
?rd <- ?rd  or  ?rs
```

--------------

##### The `xor` Instruction

**XOR** --- Perform a bitwise XOR between two registers.

###### Examples

- `xor x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0100 |

| Bit Layout |
|:---:|
| `1111100100rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
--------------------
?rd <- ?rd  xor  ?rs
```

--------------

##### The `mov` Instruction

**Move** --- Move the value from one register to another.

###### Examples

- `mov x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0101 |

| Bit Layout |
|:---:|
| `1111100101rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
-------------------
?rd <- ?rs
```

--------------

##### The `addcy` Instruction

**Add with Carry** --- Add the values of two registers with carry.

###### Examples

- `addcy x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0110 |

| Bit Layout |
|:---:|
| `1111100110rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
------------------------------------------------------
?rd <- ?rd + ?rs + bit($$cc,#carry_flag_bit);
bit($$cc,#carry_flag_bit) <- attr(cpu/alu/carryout);
bit($$cc,#overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

##### The `subcy` Instruction

**Subtract with Carry** --- Subtract the value of one register from another with carry.

###### Examples

- `subcy x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0111 |

| Bit Layout |
|:---:|
| `1111100111rrr` |

###### Semantics

```
[reg(?rs),reg(?rd)]
------------------------------------------------------
?rd <- ?rd - ?rs - bit($$cc,#carry_flag_bit);
bit($$cc,#carry_flag_bit) <- attr(cpu/alu/carryout);
bit($$cc,#overflow_flag_bit) <- attr(cpu/alu/overflow)
```

--------------

#### Instruction Format `rr(2)`


##### The `tl` Instruction

**Test Less-than** --- Test if the value of one register is less than another.

###### Examples

- `tl x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b000 |

| Bit Layout |
|:---:|
| `1111110000rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
--------------------------------------
b_push($$ts,compare(?r1,<(s(16)),?r2))
```

--------------

##### The `tge` Instruction

**Test Greater-than or Equal** --- Test if the value of one register is greater than or equal to another.

###### Examples

- `tge x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b001 |

| Bit Layout |
|:---:|
| `1111110001rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
---------------------------------------
b_push($$ts,compare(?r1,>=(s(16)),?r2))
```

--------------

##### The `tb` Instruction

**Test Below** --- Test if the value of one register is below another.

###### Examples

- `tb x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b010 |

| Bit Layout |
|:---:|
| `1111110010rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
--------------------------------------
b_push($$ts,compare(?r1,<(u(16)),?r2))
```

--------------

##### The `tae` Instruction

**Test Above or Equal** --- Test if the value of one register is above or equal to another.

###### Examples

- `tae x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b011 |

| Bit Layout |
|:---:|
| `1111110011rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
---------------------------------------
b_push($$ts,compare(?r1,>=(u(16)),?r2))
```

--------------

#### Instruction Format `rr(3)`


##### The `tne` Instruction

**Test Not Equal** --- Test if the value of one register is not equal to another.

###### Examples

- `tne x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b11111110 | 0b00 |

| Bit Layout |
|:---:|
| `1111111000rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
-----------------------
b_push($$ts,?r1 != ?r2)
```

--------------

##### The `teq` Instruction

**Test Equal** --- Test if the value of one register is equal to another.

###### Examples

- `teq x, y`

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b11111110 | 0b01 |

| Bit Layout |
|:---:|
| `1111111001rrr` |

###### Semantics

```
[reg(?r1),reg(?r2)]
-----------------------
b_push($$ts,?r1 == ?r2)
```

--------------

#### Instruction Format `r(1)`


##### The `pushb` Instruction

**Push Byte** --- Push a byte from a register onto the stack.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(1)` = 0b111111110 | 0b0000 |

| Bit Layout |
|:---:|
| `1111111100000rrr` |

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
| `r(1)` = 0b111111110 | 0b0001 |

| Bit Layout |
|:---:|
| `1111111100001rrr` |

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
| `r(1)` = 0b111111110 | 0b0010 |

| Bit Layout |
|:---:|
| `1111111100010rrr` |

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
| `r(1)` = 0b111111110 | 0b0011 |

| Bit Layout |
|:---:|
| `1111111100011rrr` |

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
| `r(1)` = 0b111111110 | 0b0100 |

| Bit Layout |
|:---:|
| `1111111100100rrr` |

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
| `r(1)` = 0b111111110 | 0b0101 |

| Bit Layout |
|:---:|
| `1111111100101rrr` |

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
| `r(1)` = 0b111111110 | 0b0110 |

| Bit Layout |
|:---:|
| `1111111100110rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Instruction Format `r(2)`


##### The `seb` Instruction

**Sign Extend Byte** --- Sign extend a byte in a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(2)` = 0b1111111110 | 0b000 |

| Bit Layout |
|:---:|
| `1111111110000rrr` |

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
| `r(2)` = 0b1111111110 | 0b001 |

| Bit Layout |
|:---:|
| `1111111110001rrr` |

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
| `r(2)` = 0b1111111110 | 0b010 |

| Bit Layout |
|:---:|
| `1111111110010rrr` |

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
| `r(2)` = 0b1111111110 | 0b011 |

| Bit Layout |
|:---:|
| `1111111110011rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Instruction Format `r(3)`


##### The `wr.gp` Instruction

**Write $GP** --- Write a value to the system `$GP` register from a general purpose register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `r(3)` = 0b11111111110 | 0b00 |

| Bit Layout |
|:---:|
| `1111111111000rrr` |

###### Semantics

```
[]
----
todo
```

--------------

#### Instruction Format `o`


##### The `NONEXE1` Instruction

**Non-executable (1s Version)** --- Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `o` = 0b11111111111 | 0b00000 |

| Bit Layout |
|:---:|
| `1111111111100000` |

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
| `o` = 0b11111111111 | 0b00001 |

| Bit Layout |
|:---:|
| `1111111111100001` |

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
| `o` = 0b11111111111 | 0b00010 |

| Bit Layout |
|:---:|
| `1111111111100010` |

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
| `o` = 0b11111111111 | 0b00011 |

| Bit Layout |
|:---:|
| `1111111111100011` |

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
| `o` = 0b11111111111 | 0b00100 |

| Bit Layout |
|:---:|
| `1111111111100100` |

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
| `o` = 0b11111111111 | 0b00101 |

| Bit Layout |
|:---:|
| `1111111111100101` |

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
| `o` = 0b11111111111 | 0b00110 |

| Bit Layout |
|:---:|
| `1111111111100110` |

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
| `o` = 0b11111111111 | 0b00111 |

| Bit Layout |
|:---:|
| `1111111111100111` |

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
| `o` = 0b11111111111 | 0b01000 |

| Bit Layout |
|:---:|
| `1111111111101000` |

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
| `o` = 0b11111111111 | 0b01001 |

| Bit Layout |
|:---:|
| `1111111111101001` |

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
| `o` = 0b11111111111 | 0b01010 |

| Bit Layout |
|:---:|
| `1111111111101010` |

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
| `o` = 0b11111111111 | 0b01011 |

| Bit Layout |
|:---:|
| `1111111111101011` |

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
| `o` = 0b11111111111 | 0b01100 |

| Bit Layout |
|:---:|
| `1111111111101100` |

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
| `o` = 0b11111111111 | 0b01101 |

| Bit Layout |
|:---:|
| `1111111111101101` |

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
| `o` = 0b11111111111 | 0b01110 |

| Bit Layout |
|:---:|
| `1111111111101110` |

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
| `o` = 0b11111111111 | 0b01111 |

| Bit Layout |
|:---:|
| `1111111111101111` |

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
| `o` = 0b11111111111 | 0b10000 |

| Bit Layout |
|:---:|
| `1111111111110000` |

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
| `o` = 0b11111111111 | 0b10001 |

| Bit Layout |
|:---:|
| `1111111111110001` |

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
| `o` = 0b11111111111 | 0b10010 |

| Bit Layout |
|:---:|
| `1111111111110010` |

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
| `o` = 0b11111111111 | 0b10011 |

| Bit Layout |
|:---:|
| `1111111111110011` |

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
| `o` = 0b11111111111 | 0b10100 |

| Bit Layout |
|:---:|
| `1111111111110100` |

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
| `o` = 0b11111111111 | 0b10101 |

| Bit Layout |
|:---:|
| `1111111111110101` |

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
| `o` = 0b11111111111 | 0b10110 |

| Bit Layout |
|:---:|
| `1111111111110110` |

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
| `o` = 0b11111111111 | 0b10111 |

| Bit Layout |
|:---:|
| `1111111111110111` |

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
| `o` = 0b11111111111 | 0b11000 |

| Bit Layout |
|:---:|
| `1111111111111000` |

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
| `o` = 0b11111111111 | 0b11001 |

| Bit Layout |
|:---:|
| `1111111111111001` |

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
| `o` = 0b11111111111 | 0b11010 |

| Bit Layout |
|:---:|
| `1111111111111010` |

###### Semantics

```
[]
----
todo
```

--------------

> !!! validation_failed(incompatible bit sizes(instruction(sb),memory access must produce a `u(16)` address(?(adr)+ ?(simm))))

