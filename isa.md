
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
| [`lb`](#`lb`) | [`call`](#`call`) | [`b`](#`b`) | [`li`](#`li`) | [`lgb`](#`lgb`) | [`mulstep`](#`mulstep`) | [`add`](#`add`) | [`pushb`](#`pushb`) | [`NONEXE1`](#`NONEXE1`) |
| [`lw`](#`lw`) |  | [`bt`](#`bt`) | [`szi`](#`szi`) | [`lgw`](#`lgw`) |  | [`sub`](#`sub`) | [`pushw`](#`pushw`) | [`BREAK`](#`BREAK`) |
| [`sb`](#`sb`) |  | [`bf`](#`bf`) |  | [`sgb`](#`sgb`) |  | [`and`](#`and`) | [`popb`](#`popb`) | [`HALT`](#`HALT`) |
| [`sw`](#`sw`) |  |  |  | [`sgw`](#`sgw`) |  | [`or`](#`or`) | [`popw`](#`popw`) | [`UNIMPL`](#`UNIMPL`) |
|  |  |  |  | [`tbit`](#`tbit`) |  | [`xor`](#`xor`) | [`callr`](#`callr`) | [`kret`](#`kret`) |
|  |  |  |  | [`cbit`](#`cbit`) |  | [`mov`](#`mov`) | [`jr`](#`jr`) | [`kcall`](#`kcall`) |
|  |  |  |  | [`sbit`](#`sbit`) |  | [`addcy`](#`addcy`) | [`neg`](#`neg`) | [`ret`](#`ret`) |
|  |  |  |  | [`tli`](#`tli`) |  | [`subcy`](#`subcy`) | [`seb`](#`seb`) | [`tov`](#`tov`) |
|  |  |  |  | [`tgei`](#`tgei`) |  | [`tl`](#`tl`) | [`rd.mp.lo`](#`rd.mp.lo`) | [`tcy`](#`tcy`) |
|  |  |  |  | [`tbi`](#`tbi`) |  | [`tge`](#`tge`) | [`rd.mp.hi`](#`rd.mp.hi`) | [`clr.cy`](#`clr.cy`) |
|  |  |  |  | [`taei`](#`taei`) |  | [`tb`](#`tb`) | [`rd.gp`](#`rd.gp`) | [`set.cy`](#`set.cy`) |
|  |  |  |  | [`tnei`](#`tnei`) |  | [`tae`](#`tae`) | [`wr.gp`](#`wr.gp`) | [`tpush0`](#`tpush0`) |
|  |  |  |  | [`teqi`](#`teqi`) |  | [`tne`](#`tne`) |  | [`tpush1`](#`tpush1`) |
|  |  |  |  | [`addi`](#`addi`) |  | [`teq`](#`teq`) |  | [`tnot`](#`tnot`) |
|  |  |  |  | [`andi`](#`andi`) |  |  |  | [`tand`](#`tand`) |
|  |  |  |  | [`ori`](#`ori`) |  |  |  | [`tor`](#`tor`) |
|  |  |  |  | [`xori`](#`xori`) |  |  |  | [`tdup`](#`tdup`) |
|  |  |  |  | [`addicy`](#`addicy`) |  |  |  | [`prsv.mp`](#`prsv.mp`) |
|  |  |  |  | [`subicy`](#`subicy`) |  |  |  | [`rstr.mp`](#`rstr.mp`) |
|  |  |  |  | [`lsr`](#`lsr`) |  |  |  | [`prsv.ts`](#`prsv.ts`) |
|  |  |  |  | [`lsl`](#`lsl`) |  |  |  | [`rstr.ts`](#`rstr.ts`) |
|  |  |  |  | [`asr`](#`asr`) |  |  |  | [`prsv.ra`](#`prsv.ra`) |
|  |  |  |  |  |  |  |  | [`rstr.ra`](#`rstr.ra`) |
|  |  |  |  |  |  |  |  | [`prsv.gp`](#`prsv.gp`) |
|  |  |  |  |  |  |  |  | [`rstr.gp`](#`rstr.gp`) |
|  |  |  |  |  |  |  |  | [`prsv.cc`](#`prsv.cc`) |
|  |  |  |  |  |  |  |  | [`rstr.cc`](#`rstr.cc`) |

### Instruction Format Breakdown


#### Instruction Format Layouts


| Format | Bit Pattern | Opcodes Available | Assigned | Utilization | Range of Immediate |
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


##### `lb`

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
[imm(?simm),reg(?adr),reg(?rd)]
-------------------------------
?rd <- zxt([?adr + ?simm])
```

--------------

##### `lw`

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
[imm(?simm),reg(?adr),reg(?rd)]
---------------------------------
?ptr = ?adr + ?simm  and  #65534;
?rd <- hi_lo([?ptr + #1],[?ptr])
```

--------------

##### `sb`

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

##### `sw`

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


##### `call`

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


##### `b`

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

##### `bt`

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

##### `bf`

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


##### `li`

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

##### `szi`

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


##### `lgb`

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

##### `lgw`

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

##### `sgb`

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

##### `sgw`

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

##### `tbit`

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
-------------------------------------------------------------
b_push($$ts,?rs >> bitslice(?bit_idx,..(3,0))  and  #1 == #1)
```

--------------

##### `cbit`

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
----------------------------------------------------
?rd <- ?rd  and  ~(#1 << bitslice(?bit_idx,..(3,0)))
```

--------------

##### `sbit`

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
------------------------------------------------
?rd <- ?rd  or  #1 << bitslice(?bit_idx,..(3,0))
```

--------------

##### `tli`

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
-------------------------------------------
b_push($$ts,compare(?rs,s16(<),sxt(?simm)))
```

--------------

##### `tgei`

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
--------------------------------------------
b_push($$ts,compare(?rs,s16(>=),sxt(?simm)))
```

--------------

##### `tbi`

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
------------------------------------------
b_push($$ts,compare(?rs,u16(<),zxt(?imm)))
```

--------------

##### `taei`

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
-------------------------------------------
b_push($$ts,compare(?rs,u16(>=),zxt(?imm)))
```

--------------

##### `tnei`

**Test Not Equal Immediate** --- Test if a register value is not equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `teqi`

**Test Equal Immediate** --- Test if a register value is equal to an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001100iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `addi`

**Add Immediate** --- Add an immediate value to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001101iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `andi`

**AND Immediate** --- Perform a bitwise AND between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01110 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001110iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `ori`

**OR Immediate** --- Perform a bitwise OR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b01111 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1001111iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `xori`

**XOR Immediate** --- Perform a bitwise XOR between a register and an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10000 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010000iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `addicy`

**Add Immediate with Carry** --- Add an immediate value and the carry bit to a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10001 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010001iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `subicy`

**Subtract Immediate with Carry** --- Sutract an immediate value and the carry bit from a register.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10010 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010010iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `lsr`

**Logical Shift Right** --- Perform a logical shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10011 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010011iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `lsl`

**Logical Shift Left** --- Perform a logical shift left on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10100 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010100iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `asr`

**Arithmetic Shift Right** --- Perform an arithmetic shift right on a register by an immediate value.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `ri(1)` = 0b10 | 0b10101 |

| Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|
| `1010101iiiiiirrr` | 6 | `imm6` in `[-32,31]` or `[0,63]` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

#### Instruction Format `ri(2)`


#### Instruction Format `ext`


#### Instruction Format `rrr`


##### `mulstep`

**Multiplication Step** --- Computes one step in a full 16-bit by 16-bit multiplication.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rrr` = 0b11110 | 0b00 |

| Bit Layout |
|:---:|
| `1111000rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

#### Instruction Format `rr(1)`


##### `add`

**Add** --- Add the values of two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0000 |

| Bit Layout |
|:---:|
| `1111100000rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `sub`

**Subtract** --- Subtract the value of one register from another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0001 |

| Bit Layout |
|:---:|
| `1111100001rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `and`

**AND** --- Perform a bitwise AND between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0010 |

| Bit Layout |
|:---:|
| `1111100010rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `or`

**OR** --- Perform a bitwise OR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0011 |

| Bit Layout |
|:---:|
| `1111100011rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `xor`

**XOR** --- Perform a bitwise XOR between two registers.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0100 |

| Bit Layout |
|:---:|
| `1111100100rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `mov`

**Move** --- Move the value from one register to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0101 |

| Bit Layout |
|:---:|
| `1111100101rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `addcy`

**Add with Carry** --- Add the values of two registers with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0110 |

| Bit Layout |
|:---:|
| `1111100110rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `subcy`

**Subtract with Carry** --- Subtract the value of one register from another with carry.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(1)` = 0b111110 | 0b0111 |

| Bit Layout |
|:---:|
| `1111100111rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

#### Instruction Format `rr(2)`


##### `tl`

**Test Less-than** --- Test if the value of one register is less than another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b000 |

| Bit Layout |
|:---:|
| `1111110000rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `tge`

**Test Greater-than or Equal** --- Test if the value of one register is greater than or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b001 |

| Bit Layout |
|:---:|
| `1111110001rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `tb`

**Test Below** --- Test if the value of one register is below another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b010 |

| Bit Layout |
|:---:|
| `1111110010rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `tae`

**Test Above or Equal** --- Test if the value of one register is above or equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(2)` = 0b1111110 | 0b011 |

| Bit Layout |
|:---:|
| `1111110011rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

#### Instruction Format `rr(3)`


##### `tne`

**Test Not Equal** --- Test if the value of one register is not equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b11111110 | 0b00 |

| Bit Layout |
|:---:|
| `1111111000rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

##### `teq`

**Test Equal** --- Test if the value of one register is equal to another.

###### Layout


| Format Prefix | Opcode |
|:---:|:---:|
| `rr(3)` = 0b11111110 | 0b01 |

| Bit Layout |
|:---:|
| `1111111001rrr` |

###### Semantics

```
[_61432]
--------
nop
```

--------------

#### Instruction Format `r(1)`


##### `pushb`

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
[_61432]
--------
nop
```

--------------

##### `pushw`

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
[_61432]
--------
nop
```

--------------

##### `popb`

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
[_61432]
--------
nop
```

--------------

##### `popw`

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
[_61432]
--------
nop
```

--------------

##### `callr`

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
[_61432]
--------
nop
```

--------------

##### `jr`

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
[_61432]
--------
nop
```

--------------

##### `neg`

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
[_61432]
--------
nop
```

--------------

#### Instruction Format `r(2)`


##### `seb`

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
[_61432]
--------
nop
```

--------------

##### `rd.mp.lo`

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
[_61432]
--------
nop
```

--------------

##### `rd.mp.hi`

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
[_61432]
--------
nop
```

--------------

##### `rd.gp`

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
[_61432]
--------
nop
```

--------------

#### Instruction Format `r(3)`


##### `wr.gp`

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
[_61432]
--------
nop
```

--------------

#### Instruction Format `o`


##### `NONEXE1`

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
[_61432]
--------
nop
```

--------------

##### `BREAK`

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
[_61432]
--------
nop
```

--------------

##### `HALT`

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
[_61432]
--------
nop
```

--------------

##### `UNIMPL`

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
[_61432]
--------
nop
```

--------------

##### `kret`

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
[_61432]
--------
nop
```

--------------

##### `kcall`

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
[_61432]
--------
nop
```

--------------

##### `ret`

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
[_61432]
--------
nop
```

--------------

##### `tov`

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
[_61432]
--------
nop
```

--------------

##### `tcy`

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
[_61432]
--------
nop
```

--------------

##### `clr.cy`

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
[_61432]
--------
nop
```

--------------

##### `set.cy`

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
[_61432]
--------
nop
```

--------------

##### `tpush0`

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
[_61432]
--------
nop
```

--------------

##### `tpush1`

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
[_61432]
--------
nop
```

--------------

##### `tnot`

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
[_61432]
--------
nop
```

--------------

##### `tand`

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
[_61432]
--------
nop
```

--------------

##### `tor`

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
[_61432]
--------
nop
```

--------------

##### `tdup`

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
[_61432]
--------
nop
```

--------------

##### `prsv.mp`

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
[_61432]
--------
nop
```

--------------

##### `rstr.mp`

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
[_61432]
--------
nop
```

--------------

##### `prsv.ts`

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
[_61432]
--------
nop
```

--------------

##### `rstr.ts`

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
[_61432]
--------
nop
```

--------------

##### `prsv.ra`

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
[_61432]
--------
nop
```

--------------

##### `rstr.ra`

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
[_61432]
--------
nop
```

--------------

##### `prsv.gp`

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
[_61432]
--------
nop
```

--------------

##### `rstr.gp`

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
[_61432]
--------
nop
```

--------------

##### `prsv.cc`

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
[_61432]
--------
nop
```

--------------

##### `rstr.cc`

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
[_61432]
--------
nop
```

--------------
