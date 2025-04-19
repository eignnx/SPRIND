
# Instructions


## Instruction Counts by Format


| Generic format | Description | Available Opcodes | Assigned | Utilization |
|:---:|:----|:---:|:---:|:---:|
| [`rri`](#instruction-format-rri) | Register-register-immediate | 4 | 4 | 100% |
| [`subr`](#instruction-format-subr) | Subroutine Call | 1 | 1 | 100% |
| [`b`](#instruction-format-b) | Branch | 4 | 3 | 75% |
| [`li`](#instruction-format-li) | Load Immediate | 2 | 2 | 100% |
| [`ri(_)`](#instruction-format-ri_) | Register-immediate | 48 | 26 | 54% |
| [`ext`](#instruction-format-ext) | Reserved for Extension | 4096 | 0 | 0% |
| [`rrr`](#instruction-format-rrr) | Register-register-register | 4 | 1 | 25% |
| [`rr(_)`](#instruction-format-rr_) | Register-register | 28 | 14 | 50% |
| [`r(_)`](#instruction-format-r_) | Register | 24 | 12 | 50% |
| [`o`](#instruction-format-o) | Opcode | 64 | 28 | 44% |
|  | **Totals (excluding `ext`)** | **179** | **91** | **51%** |



## Instruction Listing


| `rri` | `subr` | `b` | `li` | `ri(_)` | `rrr` | `rr(_)` | `r(_)` | `o` |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| [`lb`](#the-lb-instruction) | [`call`](#the-call-instruction) | [`b`](#the-b-instruction) | [`li`](#the-li-instruction) | [`lgb`](#the-lgb-instruction) | [`mulstep`](#the-mulstep-instruction) | [`add`](#the-add-instruction) | [`pushb`](#the-pushb-instruction) | [`NONEXE0`](#the-nonexe0-instruction) |
| [`lw`](#the-lw-instruction) |  | [`bt`](#the-bt-instruction) | [`szi`](#the-szi-instruction) | [`lgw`](#the-lgw-instruction) |  | [`sub`](#the-sub-instruction) | [`pushw`](#the-pushw-instruction) | [`UNIMPL`](#the-unimpl-instruction) |
| [`sb`](#the-sb-instruction) |  | [`bf`](#the-bf-instruction) |  | [`sgb`](#the-sgb-instruction) |  | [`and`](#the-and-instruction) | [`popb`](#the-popb-instruction) | [`BREAK`](#the-break-instruction) |
| [`sw`](#the-sw-instruction) |  |  |  | [`sgw`](#the-sgw-instruction) |  | [`or`](#the-or-instruction) | [`popw`](#the-popw-instruction) | [`kret`](#the-kret-instruction) |
|  |  |  |  | [`tbit`](#the-tbit-instruction) |  | [`xor`](#the-xor-instruction) | [`callr`](#the-callr-instruction) | [`kcall`](#the-kcall-instruction) |
|  |  |  |  | [`cbit`](#the-cbit-instruction) |  | [`mov`](#the-mov-instruction) | [`jr`](#the-jr-instruction) | [`ret`](#the-ret-instruction) |
|  |  |  |  | [`sbit`](#the-sbit-instruction) |  | [`addcy`](#the-addcy-instruction) | [`neg`](#the-neg-instruction) | [`tov`](#the-tov-instruction) |
|  |  |  |  | [`tli`](#the-tli-instruction) |  | [`subcy`](#the-subcy-instruction) | [`seb`](#the-seb-instruction) | [`tcy`](#the-tcy-instruction) |
|  |  |  |  | [`tgei`](#the-tgei-instruction) |  | [`tl`](#the-tl-instruction) | [`rd.mp.lo`](#the-rdmplo-instruction) | [`clr.cy`](#the-clrcy-instruction) |
|  |  |  |  | [`tbi`](#the-tbi-instruction) |  | [`tge`](#the-tge-instruction) | [`rd.mp.hi`](#the-rdmphi-instruction) | [`set.cy`](#the-setcy-instruction) |
|  |  |  |  | [`taei`](#the-taei-instruction) |  | [`tb`](#the-tb-instruction) | [`rd.gp`](#the-rdgp-instruction) | [`tpush0`](#the-tpush0-instruction) |
|  |  |  |  | [`tnei`](#the-tnei-instruction) |  | [`tae`](#the-tae-instruction) | [`wr.gp`](#the-wrgp-instruction) | [`tpush1`](#the-tpush1-instruction) |
|  |  |  |  | [`teqi`](#the-teqi-instruction) |  | [`tne`](#the-tne-instruction) |  | [`tnot`](#the-tnot-instruction) |
|  |  |  |  | [`addi`](#the-addi-instruction) |  | [`teq`](#the-teq-instruction) |  | [`tand`](#the-tand-instruction) |
|  |  |  |  | [`subi`](#the-subi-instruction) |  |  |  | [`tor`](#the-tor-instruction) |
|  |  |  |  | [`andi`](#the-andi-instruction) |  |  |  | [`tdup`](#the-tdup-instruction) |
|  |  |  |  | [`ori`](#the-ori-instruction) |  |  |  | [`prsv.mp`](#the-prsvmp-instruction) |
|  |  |  |  | [`xori`](#the-xori-instruction) |  |  |  | [`rstr.mp`](#the-rstrmp-instruction) |
|  |  |  |  | [`addicy`](#the-addicy-instruction) |  |  |  | [`prsv.ts`](#the-prsvts-instruction) |
|  |  |  |  | [`subicy`](#the-subicy-instruction) |  |  |  | [`rstr.ts`](#the-rstrts-instruction) |
|  |  |  |  | [`lsr`](#the-lsr-instruction) |  |  |  | [`prsv.ra`](#the-prsvra-instruction) |
|  |  |  |  | [`lsl`](#the-lsl-instruction) |  |  |  | [`rstr.ra`](#the-rstrra-instruction) |
|  |  |  |  | [`asr`](#the-asr-instruction) |  |  |  | [`prsv.gp`](#the-prsvgp-instruction) |
|  |  |  |  | [`tbitm`](#the-tbitm-instruction) |  |  |  | [`rstr.gp`](#the-rstrgp-instruction) |
|  |  |  |  | [`cbitm`](#the-cbitm-instruction) |  |  |  | [`prsv.cc`](#the-prsvcc-instruction) |
|  |  |  |  | [`sbitm`](#the-sbitm-instruction) |  |  |  | [`rstr.cc`](#the-rstrcc-instruction) |
|  |  |  |  |  |  |  |  | [`sleep`](#the-sleep-instruction) |
|  |  |  |  |  |  |  |  | [`vijt`](#the-vijt-instruction) |

## Synthetic Instructions


| Synth. Instr. | Description | Expansion | Reversability |
|:---:|:---:|:---:|:---:|
| `clr r` | Clear a register | `xor r, r` | Reversable |
| `nop` | The no-op instruction | `ori $sp, 0` | Reversable |
| `incr r` | Increment a register | `addi r, 1` | Reversable |
| `decr r` | Increment a register | `addi r, -1` | Reversable |
| `inv r` | Bitwise inversion (complement) | `xori r, -1` | Reversable |
| `not r` | Invert a boolean (0 or 1) | `xori r, 1` | Reversable |
| `tg r1, r2` | Test greater-than | `tl r2, r1` | One Way |
| `tle r1, r2` | Test Less-than or Equal | `tge r2, r1` | One Way |
| `ta r1, r2` | Test Above | `ta r2, r1` | One Way |
| `tbe r1, r2` | Test Below or Equal | `tae r2, r1` | One Way |
| `HALT` | Halt the processor (infinite loop) | `b 0` | Reversable |

## Instruction Format Breakdown


### Instruction Format Layouts


| Format | [Bit Pattern](#legend) | Opcodes Available | Assigned | Utilization | Range of Immediate |
|:----|:---:|:---:|:---:|:---:|:---:|
| [`rri`](#format-rri) | `11ooiiiiiisssrrr` | 4 | 4 | 100% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`subr`](#format-subr) | `101iiiiiiiiiiiii` | 1 | 1 | 100% | `imm13` in `[-4096, 4095]` or `[0, 8191]` |
| [`b`](#format-b) | `1001ooiiiiiiiiii` | 4 | 3 | 75% | `imm10` in `[-512, 511]` or `[0, 1023]` |
| [`li`](#format-li) | `1000oiiiiiiiirrr` | 2 | 2 | 100% | `imm8` in `[-128, 127]` or `[0, 255]` |
| [`ri(1)`](#format-ri1) | `01oooooiiiiiirrr` | 32 | 26 | 81% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`ri(2)`](#format-ri2) | `001ooooiiiiiirrr` | 16 | 0 | 0% | `imm6` in `[-32, 31]` or `[0, 63]` |
| [`ext`](#format-ext) | `0001oooooooooooo` | 4096 | 0 | 0% |  |
| [`rrr`](#format-rrr) | `00001ootttsssrrr` | 4 | 1 | 25% |  |
| [`rr(1)`](#format-rr1) | `000001oooosssrrr` | 16 | 8 | 50% |  |
| [`rr(2)`](#format-rr2) | `0000001ooosssrrr` | 8 | 4 | 50% |  |
| [`rr(3)`](#format-rr3) | `00000001oosssrrr` | 4 | 2 | 50% |  |
| [`r(1)`](#format-r1) | `000000001oooorrr` | 16 | 8 | 50% |  |
| [`r(2)`](#format-r2) | `0000000001ooorrr` | 8 | 4 | 50% |  |
| [`o`](#format-o) | `0000000000oooooo` | 64 | 28 | 44% |  |

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

--------------------------------------------------------------------------------
byte                      100%  ################################################################################
load                      100%  ################################################################################
store                     100%  ################################################################################
word                      100%  ################################################################################
instr(lb)                  38%  ##############################
instr(lw)                  38%  ##############################
instr(sb)                  38%  ##############################
instr(sw)                  38%  ##############################
mem                         0%  
trying splittag `byte`...
Left: [lw,sw]
Right: [lb,sb]

--------------------------------------------------------------------------------
load                      100%  ################################################################################
store                     100%  ################################################################################
instr(lw)                  75%  ############################################################
instr(sw)                  75%  ############################################################
mem                         0%  
word                        0%  
trying splittag `load`...
Left: [sw]
Right: [lw]

--------------------------------------------------------------------------------
load                      100%  ################################################################################
store                     100%  ################################################################################
instr(lb)                  75%  ############################################################
instr(sb)                  75%  ############################################################
byte                        0%  
mem                         0%  
trying splittag `load`...
Left: [sb]
Right: [lb]


#### Format `rri`


![../assets/rri.svg](../assets/rri.svg)

##### The `lb` Instruction

**Load Byte** --- Load a byte from memory into a register.

###### Examples

- `lb w, [sp+12]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | `lb` = 0b11 | `1111iiiiiisssrrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs), reg(s, rd)]
------------------------------------
let ptr := (rs\s+sxt(simm))\u;
rd <- zxt([ptr])
```

###### Module

**`base`**

--------------

##### The `lw` Instruction

**Load Word** --- Load a word from memory into a register.

###### Examples

- `lw w, [sp+12]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | `lw` = 0b01 | `1101iiiiiisssrrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs), reg(s, rd)]
--------------------------------------
let ptr := (rs\s+sxt(simm)and-2\16)\u;
rd <- {[ptr+1], [ptr]}
```

###### Module

**`base`**

--------------

##### The `sb` Instruction

**Store Byte** --- Store a byte from a register into memory.

###### Examples

- `sb [sp-20], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | `sb` = 0b10 | `1110iiiiiisssrrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd), reg(s, rs)]
------------------------------------
let ptr := rd\s+sxt(simm);
[ptr\u] <- lo(rs)
```

###### Module

**`base`**

--------------

##### The `sw` Instruction

**Store Word** --- Store a word from a register into memory.

###### Examples

- `sw [sp-20], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `rri` = 0b11 | `sw` = 0b00 | `1100iiiiiisssrrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd), reg(s, rs)]
--------------------------------------
let ptr := (rd\s+sxt(simm)and65534)\u;
[ptr] <- lo(rs);
[ptr+1] <- hi(rs)
```

###### Module

**`base`**

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
| `subr` = 0b101 | `call` = NONE | `101iiiiiiiiiiiii` | 13 | `[-4096, 4095]` |

###### Semantics

```
[simm(simm)]
-------------------------------------
$PC <- $PC\s+(sxt(simm)<<subr_align);
$RA <- $PC+2
```

###### Module

**`base`**

--------------

### Instruction Format `b`

--------------------------------------------------------------------------------
cond                       67%  #####################################################
instr(b)                   50%  ########################################
instr(bf)                  50%  ########################################
instr(bt)                  50%  ########################################
pc                          0%  
trying splittag `cond`...
Left: [b]
Right: [bt,bf]

--------------------------------------------------------------------------------
instr(bf)                  75%  ############################################################
instr(bt)                  75%  ############################################################
cond                        0%  
pc                          0%  
trying splittag `instr(bf)`...
Left: [bt]
Right: [bf]


#### Format `b`


![../assets/b.svg](../assets/b.svg)

##### The `b` Instruction

**Branch** --- Branch to the specified address by adding the immediate offset to `$PC`.

###### Examples

- `b SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | `b` = 0b00 | `100100iiiiiiiiii` | 10 | `[-512, 511]` |

###### Semantics

```
[simm(offset)]
------------------------
$PC <- $PC\s+sxt(offset)
```

###### Module

**`base`**

--------------

##### The `bt` Instruction

**Branch If True** --- Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.

###### Examples

- `bt SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | `bt` = 0b10 | `100110iiiiiiiiii` | 10 | `[-512, 511]` |

###### Semantics

```
[simm(offset)]
----------------------------
if b_pop($TS) {
    $PC <- $PC\s+sxt(offset)
}
```

###### Module

**`base`**

--------------

##### The `bf` Instruction

**Branch If False** --- Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.

###### Examples

- `bf SOME_LABEL`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `b` = 0b1001 | `bf` = 0b11 | `100111iiiiiiiiii` | 10 | `[-512, 511]` |

###### Semantics

```
[simm(offset)]
----------------------------
if !(b_pop($TS)) {
    $PC <- $PC\s+sxt(offset)
}
```

###### Module

**`base`**

--------------

### Instruction Format `li`

--------------------------------------------------------------------------------
shift                     100%  ################################################################################
sxt                       100%  ################################################################################
zxt                       100%  ################################################################################
instr(li)                  75%  ############################################################
instr(szi)                 75%  ############################################################
data                        0%  
trying splittag `shift`...
Left: [li]
Right: [szi]


#### Format `li`


![../assets/li.svg](../assets/li.svg)

##### The `li` Instruction

**Load Immediate** --- Load an immediate value into a register.

###### Examples

- `li x, 123`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `li` = 0b1000 | `li` = 0b0 | `10000iiiiiiiirrr` | 8 | `[-128, 127]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- sxt(simm)
```

###### Module

**`base`**

--------------

##### The `szi` Instruction

**Shift Zero-extended Immediate** --- Left-shift a zero-extended immediate value into a register.

###### Examples

- `szi x, 0xB3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `li` = 0b1000 | `szi` = 0b1 | `10001iiiiiiiirrr` | 8 | `[0, 255]` |

###### Semantics

```
[imm(imm), reg(r, rd)]
-----------------------
rd <- rd<<8 or zxt(imm)
```

###### Module

**`base`**

--------------

### Instruction Format `ri(_)`

--------------------------------------------------------------------------------
bitwise                   100%  ################################################################################
ts                         90%  ########################################################################
mem                        80%  ################################################################
bit                        70%  ########################################################
cmp                        70%  ########################################################
global                     50%  ########################################
inequality                 50%  ########################################
shift                      40%  ################################
<                          30%  ########################
>=                         30%  ########################
byte                       30%  ########################
clear                      30%  ########################
equality                   30%  ########################
load                       30%  ########################
right                      30%  ########################
set                        30%  ########################
signed                     30%  ########################
store                      30%  ########################
unsigned                   30%  ########################
word                       30%  ########################
left                       20%  ################
not                        20%  ################
sxt                        20%  ################
zxt                        20%  ################
add                        10%  ########
and                        10%  ########
arith                      10%  ########
boolean                    10%  ########
carry                      10%  ########
logical                    10%  ########
or                         10%  ########
sub                        10%  ########
xor                        10%  ########
instr(asr)                  7%  ######
instr(cbit)                 7%  ######
instr(cbitm)                7%  ######
instr(lgb)                  7%  ######
instr(lgw)                  7%  ######
instr(lsl)                  7%  ######
instr(lsr)                  7%  ######
instr(sbit)                 7%  ######
instr(sbitm)                7%  ######
instr(sgb)                  7%  ######
instr(sgw)                  7%  ######
instr(taei)                 7%  ######
instr(tbi)                  7%  ######
instr(tbit)                 7%  ######
instr(tbitm)                7%  ######
instr(teqi)                 7%  ######
instr(tgei)                 7%  ######
instr(tli)                  7%  ######
instr(tnei)                 7%  ######
instr(addi)                 0%  
instr(addicy)               0%  
instr(andi)                 0%  
instr(ori)                  0%  
instr(subi)                 0%  
instr(subicy)               0%  
instr(xori)                 0%  
trying splittag `bitwise`...
Left: [lgb,lgw,sgb,sgw,tli,tgei,tbi,taei,tnei,teqi]
Right: [tbit,cbit,sbit,lsr,lsl,asr,tbitm,cbitm,sbitm,subcat_tags_instrs(alu,[<,>=,add,and,arith,bit,bitwise,boolean,byte,carry,clear,cmp,equality,global,inequality,left,load,logical,mem,not,or,right,set,shift,signed,store,sub,sxt,ts,unsigned,word,xor,zxt],[addi,subi,andi,ori,xori,addicy,subicy])]

--------------------------------------------------------------------------------
cmp                        80%  ################################################################
global                     80%  ################################################################
inequality                 80%  ################################################################
mem                        80%  ################################################################
ts                         80%  ################################################################
<                          40%  ################################
>=                         40%  ################################
byte                       40%  ################################
equality                   40%  ################################
load                       40%  ################################
signed                     40%  ################################
store                      40%  ################################
unsigned                   40%  ################################
word                       40%  ################################
not                        20%  ################
instr(lgb)                 15%  ############
instr(lgw)                 15%  ############
instr(sgb)                 15%  ############
instr(sgw)                 15%  ############
instr(taei)                15%  ############
instr(tbi)                 15%  ############
instr(teqi)                15%  ############
instr(tgei)                15%  ############
instr(tli)                 15%  ############
instr(tnei)                15%  ############
trying splittag `cmp`...
Left: [lgb,lgw,sgb,sgw]
Right: [tli,tgei,tbi,taei,tnei,teqi]

--------------------------------------------------------------------------------
byte                      100%  ################################################################################
load                      100%  ################################################################################
store                     100%  ################################################################################
word                      100%  ################################################################################
instr(lgb)                 38%  ##############################
instr(lgw)                 38%  ##############################
instr(sgb)                 38%  ##############################
instr(sgw)                 38%  ##############################
global                      0%  
mem                         0%  
trying splittag `byte`...
Left: [lgw,sgw]
Right: [lgb,sgb]

--------------------------------------------------------------------------------
load                      100%  ################################################################################
store                     100%  ################################################################################
instr(lgw)                 75%  ############################################################
instr(sgw)                 75%  ############################################################
global                      0%  
mem                         0%  
word                        0%  
trying splittag `load`...
Left: [sgw]
Right: [lgw]

--------------------------------------------------------------------------------
load                      100%  ################################################################################
store                     100%  ################################################################################
instr(lgb)                 75%  ############################################################
instr(sgb)                 75%  ############################################################
byte                        0%  
global                      0%  
mem                         0%  
trying splittag `load`...
Left: [sgb]
Right: [lgb]

--------------------------------------------------------------------------------
inequality                 67%  #####################################################
<                          67%  #####################################################
>=                         67%  #####################################################
equality                   67%  #####################################################
signed                     67%  #####################################################
unsigned                   67%  #####################################################
not                        33%  ###########################
instr(taei)                25%  ####################
instr(tbi)                 25%  ####################
instr(teqi)                25%  ####################
instr(tgei)                25%  ####################
instr(tli)                 25%  ####################
instr(tnei)                25%  ####################
cmp                         0%  
ts                          0%  
trying splittag `inequality`...
Left: [tnei,teqi]
Right: [tli,tgei,tbi,taei]

--------------------------------------------------------------------------------
not                       100%  ################################################################################
instr(teqi)                75%  ############################################################
instr(tnei)                75%  ############################################################
cmp                         0%  
equality                    0%  
ts                          0%  
trying splittag `not`...
Left: [teqi]
Right: [tnei]

--------------------------------------------------------------------------------
<                         100%  ################################################################################
>=                        100%  ################################################################################
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(taei)                38%  ##############################
instr(tbi)                 38%  ##############################
instr(tgei)                38%  ##############################
instr(tli)                 38%  ##############################
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `<`...
Left: [tgei,taei]
Right: [tli,tbi]

--------------------------------------------------------------------------------
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(taei)                75%  ############################################################
instr(tgei)                75%  ############################################################
>=                          0%  
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `signed`...
Left: [taei]
Right: [tgei]

--------------------------------------------------------------------------------
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(tbi)                 75%  ############################################################
instr(tli)                 75%  ############################################################
<                           0%  
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `signed`...
Left: [tbi]
Right: [tli]

--------------------------------------------------------------------------------
ts                        100%  ################################################################################
mem                        80%  ################################################################
shift                      80%  ################################################################
bit                        60%  ################################################
clear                      60%  ################################################
right                      60%  ################################################
set                        60%  ################################################
left                       40%  ################################
sxt                        40%  ################################
zxt                        40%  ################################
<                          20%  ################
>=                         20%  ################
add                        20%  ################
and                        20%  ################
arith                      20%  ################
boolean                    20%  ################
byte                       20%  ################
carry                      20%  ################
cmp                        20%  ################
equality                   20%  ################
global                     20%  ################
inequality                 20%  ################
load                       20%  ################
logical                    20%  ################
not                        20%  ################
or                         20%  ################
signed                     20%  ################
store                      20%  ################
sub                        20%  ################
unsigned                   20%  ################
word                       20%  ################
xor                        20%  ################
instr(asr)                 15%  ############
instr(cbit)                15%  ############
instr(cbitm)               15%  ############
instr(lsl)                 15%  ############
instr(lsr)                 15%  ############
instr(sbit)                15%  ############
instr(sbitm)               15%  ############
instr(tbit)                15%  ############
instr(tbitm)               15%  ############
bitwise                     0%  
trying splittag `ts`...
Left: [cbit,sbit,lsr,lsl,asr]
Right: [tbit,tbitm,cbitm,sbitm,subcat_tags_instrs(alu,[<,>=,add,and,arith,bit,bitwise,boolean,byte,carry,clear,cmp,equality,global,inequality,left,load,logical,mem,not,or,right,set,shift,signed,store,sub,sxt,ts,unsigned,word,xor,zxt],[addi,subi,andi,ori,xori,addicy,subicy])]

--------------------------------------------------------------------------------
bit                        80%  ################################################################
right                      80%  ################################################################
shift                      80%  ################################################################
clear                      40%  ################################
left                       40%  ################################
set                        40%  ################################
sxt                        40%  ################################
zxt                        40%  ################################
instr(asr)                 30%  ########################
instr(cbit)                30%  ########################
instr(lsl)                 30%  ########################
instr(lsr)                 30%  ########################
instr(sbit)                30%  ########################
bitwise                     0%  
trying splittag `bit`...
Left: [lsr,lsl,asr]
Right: [cbit,sbit]

--------------------------------------------------------------------------------
right                      67%  #####################################################
left                       67%  #####################################################
sxt                        67%  #####################################################
zxt                        67%  #####################################################
instr(asr)                 50%  ########################################
instr(lsl)                 50%  ########################################
instr(lsr)                 50%  ########################################
bitwise                     0%  
shift                       0%  
trying splittag `right`...
Left: [lsl]
Right: [lsr,asr]

--------------------------------------------------------------------------------
sxt                       100%  ################################################################################
instr(asr)                 75%  ############################################################
instr(lsr)                 75%  ############################################################
bitwise                     0%  
right                       0%  
shift                       0%  
trying splittag `sxt`...
Left: [lsr]
Right: [asr]

--------------------------------------------------------------------------------
clear                     100%  ################################################################################
set                       100%  ################################################################################
instr(cbit)                75%  ############################################################
instr(sbit)                75%  ############################################################
bit                         0%  
bitwise                     0%  
trying splittag `clear`...
Left: [sbit]
Right: [cbit]

--------------------------------------------------------------------------------
clear                      80%  ################################################################
set                        80%  ################################################################
<                          40%  ################################
>=                         40%  ################################
add                        40%  ################################
and                        40%  ################################
arith                      40%  ################################
boolean                    40%  ################################
byte                       40%  ################################
carry                      40%  ################################
cmp                        40%  ################################
equality                   40%  ################################
global                     40%  ################################
inequality                 40%  ################################
left                       40%  ################################
load                       40%  ################################
logical                    40%  ################################
not                        40%  ################################
or                         40%  ################################
right                      40%  ################################
shift                      40%  ################################
signed                     40%  ################################
store                      40%  ################################
sub                        40%  ################################
sxt                        40%  ################################
unsigned                   40%  ################################
word                       40%  ################################
xor                        40%  ################################
zxt                        40%  ################################
mem                        40%  ################################
instr(cbitm)               30%  ########################
instr(sbitm)               30%  ########################
instr(tbit)                30%  ########################
instr(tbitm)               30%  ########################
bit                         0%  
bitwise                     0%  
ts                          0%  
trying splittag `clear`...
Left: [tbit,tbitm,sbitm]
Right: [cbitm,subcat_tags_instrs(alu,[<,>=,add,and,arith,bit,bitwise,boolean,byte,carry,clear,cmp,equality,global,inequality,left,load,logical,mem,not,or,right,set,shift,signed,store,sub,sxt,ts,unsigned,word,xor,zxt],[addi,subi,andi,ori,xori,addicy,subicy])]

--------------------------------------------------------------------------------
mem                        67%  #####################################################
set                        67%  #####################################################
instr(sbitm)               50%  ########################################
instr(tbit)                50%  ########################################
instr(tbitm)               50%  ########################################
bit                         0%  
bitwise                     0%  
ts                          0%  
trying splittag `mem`...
Left: [tbit]
Right: [tbitm,sbitm]

--------------------------------------------------------------------------------
set                       100%  ################################################################################
instr(sbitm)               75%  ############################################################
instr(tbitm)               75%  ############################################################
bit                         0%  
bitwise                     0%  
mem                         0%  
ts                          0%  
trying splittag `set`...
Left: [tbitm]
Right: [sbitm]

--------------------------------------------------------------------------------
<                         100%  ################################################################################
>=                        100%  ################################################################################
add                       100%  ################################################################################
and                       100%  ################################################################################
arith                     100%  ################################################################################
boolean                   100%  ################################################################################
byte                      100%  ################################################################################
carry                     100%  ################################################################################
cmp                       100%  ################################################################################
equality                  100%  ################################################################################
global                    100%  ################################################################################
inequality                100%  ################################################################################
left                      100%  ################################################################################
load                      100%  ################################################################################
logical                   100%  ################################################################################
not                       100%  ################################################################################
or                        100%  ################################################################################
right                     100%  ################################################################################
set                       100%  ################################################################################
shift                     100%  ################################################################################
signed                    100%  ################################################################################
store                     100%  ################################################################################
sub                       100%  ################################################################################
sxt                       100%  ################################################################################
unsigned                  100%  ################################################################################
word                      100%  ################################################################################
xor                       100%  ################################################################################
zxt                       100%  ################################################################################
instr(cbitm)               75%  ############################################################
bit                         0%  
bitwise                     0%  
clear                       0%  
mem                         0%  
ts                          0%  
trying splittag `<`...
Left: [cbitm]
Right: [subcat_tags_instrs(alu,[<,>=,add,and,arith,bit,bitwise,boolean,byte,carry,clear,cmp,equality,global,inequality,left,load,logical,mem,not,or,right,set,shift,signed,store,sub,sxt,ts,unsigned,word,xor,zxt],[addi,subi,andi,ori,xori,addicy,subicy])]

--------------------------------------------------------------------------------
arith                      86%  #####################################################################
bitwise                    86%  #####################################################################
boolean                    86%  #####################################################################
logical                    86%  #####################################################################
add                        57%  ##############################################
carry                      57%  ##############################################
sub                        57%  ##############################################
and                        29%  #######################
or                         29%  #######################
xor                        29%  #######################
instr(addi)                21%  #################
instr(addicy)              21%  #################
instr(andi)                21%  #################
instr(ori)                 21%  #################
instr(subi)                21%  #################
instr(subicy)              21%  #################
instr(xori)                21%  #################
aligned_subcat(alu)         0%  
trying splittag `arith`...
Left: [andi,ori,xori]
Right: [addi,subi,addicy,subicy]

--------------------------------------------------------------------------------
and                        67%  #####################################################
or                         67%  #####################################################
xor                        67%  #####################################################
instr(andi)                50%  ########################################
instr(ori)                 50%  ########################################
instr(xori)                50%  ########################################
bitwise                     0%  
boolean                     0%  
logical                     0%  
aligned_subcat(alu)         0%  
trying splittag `and`...
Left: [ori,xori]
Right: [andi]

--------------------------------------------------------------------------------
or                        100%  ################################################################################
xor                       100%  ################################################################################
instr(ori)                 75%  ############################################################
instr(xori)                75%  ############################################################
bitwise                     0%  
boolean                     0%  
logical                     0%  
aligned_subcat(alu)         0%  
trying splittag `or`...
Left: [xori]
Right: [ori]

--------------------------------------------------------------------------------
add                       100%  ################################################################################
carry                     100%  ################################################################################
sub                       100%  ################################################################################
instr(addi)                38%  ##############################
instr(addicy)              38%  ##############################
instr(subi)                38%  ##############################
instr(subicy)              38%  ##############################
arith                       0%  
aligned_subcat(alu)         0%  
trying splittag `add`...
Left: [subi,subicy]
Right: [addi,addicy]

--------------------------------------------------------------------------------
carry                     100%  ################################################################################
instr(subi)                75%  ############################################################
instr(subicy)              75%  ############################################################
arith                       0%  
sub                         0%  
aligned_subcat(alu)         0%  
trying splittag `carry`...
Left: [subi]
Right: [subicy]

--------------------------------------------------------------------------------
carry                     100%  ################################################################################
instr(addi)                75%  ############################################################
instr(addicy)              75%  ############################################################
add                         0%  
arith                       0%  
aligned_subcat(alu)         0%  
trying splittag `carry`...
Left: [addi]
Right: [addicy]


#### Format `ri(1)`


![../assets/ri(1).svg](../assets/ri(1).svg)

##### The `lgb` Instruction

**Load Global Byte** --- Load a byte from a memory address offset from `$GP`.

###### Examples

- `lgb x, [gp+8]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `lgb` = 0b00011 | `0100011iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(disp), reg(r, rd)]
----------------------------
rd <- zxt([$GP\u+zxt(disp)])
```

###### Module

**`globals`**

--------------

##### The `lgw` Instruction

**Load Global Word** --- Load a word from a memory address offset from `$GP`.

###### Examples

- `lgw x, [gp+8]`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `lgw` = 0b00001 | `0100001iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(disp), reg(r, rd)]
---------------------------------------
let ptr := ($GP\u+zxt(disp)and65534)\u;
rd <- {[ptr+1], [ptr]}
```

###### Module

**`globals`**

--------------

##### The `sgb` Instruction

**Store Global Byte** --- Store a byte into memory address offset from `$GP`.

###### Examples

- `sgb [gp+8], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `sgb` = 0b00010 | `0100010iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(disp), reg(r, rs)]
---------------------------
[$GP\u+zxt(disp)] <- lo(rs)
```

###### Module

**`globals`**

--------------

##### The `sgw` Instruction

**Store Global Word** --- Store a word into memory address offset from `$GP`.

###### Examples

- `sgw [gp+8], x`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `sgw` = 0b00000 | `0100000iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(disp), reg(r, rs)]
---------------------------------------
let ptr := ($GP\u+zxt(disp)and65534)\u;
{[ptr+1], [ptr]} <- rs
```

###### Module

**`globals`**

--------------

##### The `tbit` Instruction

**Test Bit** --- Test a specific bit in a register, modifying `$TS`.

###### Examples

- `tbit 12, w`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tbit` = 0b01100 | `0101100iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(r, rs)]
-------------------------------------
let shamt := bitslice(bit_idx, 3..0);
let bit := rs>>shamt\u and 1;
b_push($TS, bit==1)
```

###### Module

**`bittests`**

--------------

##### The `cbit` Instruction

**Clear Bit** --- Clear a specific bit in a register.

###### Examples

- `cbit 9, v`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `cbit` = 0b01011 | `0101011iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(r, rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~ (1<<idx);
rd <- rd and mask
```

###### Module

**`bittests`**

--------------

##### The `sbit` Instruction

**Set Bit** --- Set a specific bit in a register.

###### Examples

- `sbit 15, a`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `sbit` = 0b01010 | `0101010iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(bit_idx), reg(r, rd)]
-------------------------------------
let idx := bitslice(bit_idx, 3..0)\u;
let mask := ~ (1<<idx);
rd <- rd or mask
```

###### Module

**`bittests`**

--------------

##### The `tli` Instruction

**Test Less-than Immediate** --- Test if a register value is less than an immediate value.

###### Examples

- `tli x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tli` = 0b01111 | `0101111iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs)]
----------------------------------------------
b_push($TS, compare(rs\s, <(s\16), sxt(simm)))
```

###### Module

**`imms`**

--------------

##### The `tgei` Instruction

**Test Greater-than or Equal Immediate** --- Test if a register value is greater than or equal to an immediate value.

###### Examples

- `tgei x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tgei` = 0b01101 | `0101101iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs)]
-----------------------------------------------
b_push($TS, compare(rs\s, >=(s\16), sxt(simm)))
```

###### Module

**`imms`**

--------------

##### The `tbi` Instruction

**Test Below Immediate** --- Test if a register value is below an immediate value.

###### Examples

- `tbi x, 10`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tbi` = 0b01110 | `0101110iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rs)]
---------------------------------------------
b_push($TS, compare(rs\u, <(u\16), zxt(imm)))
```

###### Module

**`imms`**

--------------

##### The `taei` Instruction

**Test Above or Equal** --- Test if a register value is above or equal to an immediate value.

###### Examples

- `taei x, 10`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `taei` = 0b01100 | `0101100iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rs)]
----------------------------------------------
b_push($TS, compare(rs\u, >=(u\16), zxt(imm)))
```

###### Module

**`imms`**

--------------

##### The `tnei` Instruction

**Test Not Equal Immediate** --- Test if a register value is not equal to an immediate value.

###### Examples

- `tnei x, 0`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tnei` = 0b00101 | `0100101iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs)]
----------------------------
b_push($TS, rs\s\=sxt(simm))
```

###### Module

**`imms`**

--------------

##### The `teqi` Instruction

**Test Equal Immediate** --- Test if a register value is equal to an immediate value.

###### Examples

- `teqi x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `teqi` = 0b00100 | `0100100iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rs)]
----------------------------
b_push($TS, rs\s==sxt(simm))
```

###### Module

**`imms`**

--------------

##### The `addi` Instruction

**Add Immediate** --- Add an immediate value to a register.

###### Examples

- `addi x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `addi` = 0b1111110 | `011111110iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- rd\s+sxt(simm)
```

###### Module

**`imms`**

--------------

##### The `subi` Instruction

**Subtract Immediate** --- Subtract an immediate value from a register.

###### Examples

- `subi x, -5`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `subi` = 0b1111100 | `011111100iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- rd\s-sxt(simm)
```

###### Module

**`imms`**

--------------

##### The `andi` Instruction

**AND Immediate** --- Perform a bitwise AND between a register and an immediate value.

###### Examples

- `andi x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `andi` = 0b111101 | `01111101iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- rd and sxt(simm)
```

###### Module

**`imms`**

--------------

##### The `ori` Instruction

**OR Immediate** --- Perform a bitwise OR between a register and an immediate value.

###### Examples

- `ori x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `ori` = 0b1111001 | `011111001iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- rd or sxt(simm)
```

###### Module

**`imms`**

--------------

##### The `xori` Instruction

**XOR Immediate** --- Perform a bitwise XOR between a register and an immediate value.

###### Examples

- `xori x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `xori` = 0b1111000 | `011111000iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
------------------------
rd <- rd xor sxt(simm)
```

###### Module

**`imms`**

--------------

##### The `addicy` Instruction

**Add Immediate with Carry** --- Add an immediate value and the carry bit to a register.

###### Examples

- `addicy x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `addicy` = 0b1111111 | `011111111iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
-----------------------------------------------------
rd <- rd\s+sxt(simm)+bit($CC, carry_flag_bit)\16\s;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

###### Module

**`imms`**

--------------

##### The `subicy` Instruction

**Subtract Immediate with Carry** --- Sutract an immediate value and the carry bit from a register.

###### Examples

- `subicy x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `subicy` = 0b1111101 | `011111101iiiiiirrr` | 6 | `[-32, 31]` |

###### Semantics

```
[simm(simm), reg(r, rd)]
-----------------------------------------------------
rd <- rd\s-sxt(simm)-bit($CC, carry_flag_bit)\16\s;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

###### Module

**`imms`**

--------------

##### The `lsr` Instruction

**Logical Shift Right** --- Perform a logical shift right on a register by an immediate value.

###### Examples

- `lsr x, 15`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `lsr` = 0b10010 | `0110010iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rd)]
----------------------
rd <- rd>>imm
```

###### Module

**`base`**

--------------

##### The `lsl` Instruction

**Logical Shift Left** --- Perform a logical shift left on a register by an immediate value.

###### Examples

- `lsl x, 8`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `lsl` = 0b01000 | `0101000iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rd)]
----------------------
rd <- rd<<imm
```

###### Module

**`base`**

--------------

##### The `asr` Instruction

**Arithmetic Shift Right** --- Perform an arithmetic shift right on a register by an immediate value.

###### Examples

- `asr x, 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `asr` = 0b10011 | `0110011iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rd)]
------------------------------------------------------------
let sign_extension := sxt(bit(rd, 15)-1)<<reg_size_bits-imm;
rd <- rd>>imm or sign_extension
```

###### Module

**`base`**

--------------

##### The `tbitm` Instruction

**Test Bit in Memory** --- 

###### Examples

- `tbitm [x], 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `tbitm` = 0b11010 | `0111010iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rs)]
---------------------------
b_push($TS, bit([rs], imm))
```

###### Module

**`bittests`**

--------------

##### The `cbitm` Instruction

**Clear Bit in Memory** --- 

###### Examples

- `cbitm [x], 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `cbitm` = 0b01110 | `0101110iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rs)]
-------------------------
[rs] <- [rs]and~ (1<<imm)
```

###### Module

**`bittests`**

--------------

##### The `sbitm` Instruction

**Set Bit in Memory** --- 

###### Examples

- `sbitm [x], 3`

###### Layout


| Format Prefix | Opcode | Bit Layout | Immediate Bits | Immediate Range |
|:---:|:---:|:---:|:---:|:---:|
| `ri(1)` = 0b01 | `sbitm` = 0b11011 | `0111011iiiiiirrr` | 6 | `[0, 63]` |

###### Semantics

```
[imm(imm), reg(r, rs)]
----------------------
[rs] <- [rs]or(1<<imm)
```

###### Module

**`bittests`**

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
| `rrr` = 0b00001 | `mulstep` = 0b00 | `0000100tttsssrrr` |

###### Semantics

```
[reg(r, multiplicand_hi), reg(s, multiplicand_lo), reg(t, multiplier)]
----------------------------------------------------------------------
let mask := ~ ((multiplier and 1)-1);
let masked_lo := multiplicand_lo and mask;
let masked_hi := multiplicand_hi and mask;
lo($MP) <- lo($MP)+masked_lo;
hi($MP) <- hi($MP)+masked_hi+attr(cpu/alu/carryout);
let shift_cout := bit(multiplicand_lo, reg_size_bits-1);
multiplicand_lo <- multiplicand_lo<<1;
multiplicand_hi <- multiplicand_hi<<1+shift_cout;
multiplier <- multiplier>>1
```

###### Module

**`mul`**

--------------

### Instruction Format `rr(_)`

--------------------------------------------------------------------------------
add                       100%  ################################################################################
and                       100%  ################################################################################
arith                     100%  ################################################################################
bitwise                   100%  ################################################################################
boolean                   100%  ################################################################################
carry                     100%  ################################################################################
logical                   100%  ################################################################################
or                        100%  ################################################################################
sub                       100%  ################################################################################
xor                       100%  ################################################################################
instr(mov)                 75%  ############################################################
data                        0%  
instr(add)                  0%  
instr(addcy)                0%  
instr(and)                  0%  
instr(or)                   0%  
instr(sub)                  0%  
instr(subcy)                0%  
instr(xor)                  0%  
trying splittag `add`...
Left: [mov]
Right: [subcat_tags_instrs(alu,[add,and,arith,bitwise,boolean,carry,data,logical,or,sub,xor],[add,sub,and,or,xor,addcy,subcy])]

--------------------------------------------------------------------------------
arith                      86%  #####################################################################
bitwise                    86%  #####################################################################
boolean                    86%  #####################################################################
logical                    86%  #####################################################################
add                        57%  ##############################################
carry                      57%  ##############################################
sub                        57%  ##############################################
and                        29%  #######################
or                         29%  #######################
xor                        29%  #######################
instr(add)                 21%  #################
instr(addcy)               21%  #################
instr(and)                 21%  #################
instr(or)                  21%  #################
instr(sub)                 21%  #################
instr(subcy)               21%  #################
instr(xor)                 21%  #################
aligned_subcat(alu)         0%  
trying splittag `arith`...
Left: [and,or,xor]
Right: [add,sub,addcy,subcy]

--------------------------------------------------------------------------------
and                        67%  #####################################################
or                         67%  #####################################################
xor                        67%  #####################################################
instr(and)                 50%  ########################################
instr(or)                  50%  ########################################
instr(xor)                 50%  ########################################
bitwise                     0%  
boolean                     0%  
logical                     0%  
aligned_subcat(alu)         0%  
trying splittag `and`...
Left: [or,xor]
Right: [and]

--------------------------------------------------------------------------------
or                        100%  ################################################################################
xor                       100%  ################################################################################
instr(or)                  75%  ############################################################
instr(xor)                 75%  ############################################################
bitwise                     0%  
boolean                     0%  
logical                     0%  
aligned_subcat(alu)         0%  
trying splittag `or`...
Left: [xor]
Right: [or]

--------------------------------------------------------------------------------
add                       100%  ################################################################################
carry                     100%  ################################################################################
sub                       100%  ################################################################################
instr(add)                 38%  ##############################
instr(addcy)               38%  ##############################
instr(sub)                 38%  ##############################
instr(subcy)               38%  ##############################
arith                       0%  
aligned_subcat(alu)         0%  
trying splittag `add`...
Left: [sub,subcy]
Right: [add,addcy]

--------------------------------------------------------------------------------
carry                     100%  ################################################################################
instr(sub)                 75%  ############################################################
instr(subcy)               75%  ############################################################
arith                       0%  
sub                         0%  
aligned_subcat(alu)         0%  
trying splittag `carry`...
Left: [sub]
Right: [subcy]

--------------------------------------------------------------------------------
carry                     100%  ################################################################################
instr(add)                 75%  ############################################################
instr(addcy)               75%  ############################################################
add                         0%  
arith                       0%  
aligned_subcat(alu)         0%  
trying splittag `carry`...
Left: [add]
Right: [addcy]


#### Format `rr(1)`


![../assets/rr(1).svg](../assets/rr(1).svg)

##### The `add` Instruction

**Add** --- Add the values of two registers.

###### Examples

- `add x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `add` = 0b1110 | `0000011110sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rd+rs
```

###### Module

**`base`**

--------------

##### The `sub` Instruction

**Subtract** --- Subtract the value of one register from another.

###### Examples

- `sub x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `sub` = 0b1100 | `0000011100sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rd-rs
```

###### Module

**`base`**

--------------

##### The `and` Instruction

**AND** --- Perform a bitwise AND between two registers.

###### Examples

- `and x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `and` = 0b0101 | `0000010101sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rd and rs
```

###### Module

**`base`**

--------------

##### The `or` Instruction

**OR** --- Perform a bitwise OR between two registers.

###### Examples

- `or x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `or` = 0b1001 | `0000011001sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rd or rs
```

###### Module

**`base`**

--------------

##### The `xor` Instruction

**XOR** --- Perform a bitwise XOR between two registers.

###### Examples

- `xor x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `xor` = 0b1000 | `0000011000sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rd xor rs
```

###### Module

**`base`**

--------------

##### The `mov` Instruction

**Move** --- Move the value from one register to another.

###### Examples

- `mov x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `mov` = 0b0000 | `0000010000sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
------------------------
rd <- rs
```

###### Module

**`base`**

--------------

##### The `addcy` Instruction

**Add with Carry** --- Add the values of two registers with carry.

###### Examples

- `addcy x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `addcy` = 0b1111 | `0000011111sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
-----------------------------------------------------
rd <- rd+rs+bit($CC, carry_flag_bit)\16;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

###### Module

**`base`**

--------------

##### The `subcy` Instruction

**Subtract with Carry** --- Subtract the value of one register from another with carry.

###### Examples

- `subcy x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(1)` = 0b000001 | `subcy` = 0b1101 | `0000011101sssrrr` |

###### Semantics

```
[reg(r, rs), reg(s, rd)]
-----------------------------------------------------
rd <- rd-rs-bit($CC, carry_flag_bit)\16;
bit($CC, carry_flag_bit) <- attr(cpu/alu/carryout);
bit($CC, overflow_flag_bit) <- attr(cpu/alu/overflow)
```

###### Module

**`base`**

--------------
--------------------------------------------------------------------------------
<                         100%  ################################################################################
>=                        100%  ################################################################################
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(tae)                 38%  ##############################
instr(tb)                  38%  ##############################
instr(tge)                 38%  ##############################
instr(tl)                  38%  ##############################
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `<`...
Left: [tge,tae]
Right: [tl,tb]

--------------------------------------------------------------------------------
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(tae)                 75%  ############################################################
instr(tge)                 75%  ############################################################
>=                          0%  
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `signed`...
Left: [tae]
Right: [tge]

--------------------------------------------------------------------------------
signed                    100%  ################################################################################
unsigned                  100%  ################################################################################
instr(tb)                  75%  ############################################################
instr(tl)                  75%  ############################################################
<                           0%  
cmp                         0%  
inequality                  0%  
ts                          0%  
trying splittag `signed`...
Left: [tb]
Right: [tl]


#### Format `rr(2)`


![../assets/rr(2).svg](../assets/rr(2).svg)

##### The `tl` Instruction

**Test Less-than** --- Test if the value of one register is less than another.

###### Examples

- `tl x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | `tl` = 0b011 | `0000001011sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
-------------------------------------
b_push($TS, compare(r1, <(s\16), r2))
```

###### Module

**`base`**

--------------

##### The `tge` Instruction

**Test Greater-than or Equal** --- Test if the value of one register is greater than or equal to another.

###### Examples

- `tge x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | `tge` = 0b001 | `0000001001sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
--------------------------------------
b_push($TS, compare(r1, >=(s\16), r2))
```

###### Module

**`base`**

--------------

##### The `tb` Instruction

**Test Below** --- Test if the value of one register is below another.

###### Examples

- `tb x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | `tb` = 0b010 | `0000001010sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
-------------------------------------
b_push($TS, compare(r1, <(u\16), r2))
```

###### Module

**`base`**

--------------

##### The `tae` Instruction

**Test Above or Equal** --- Test if the value of one register is above or equal to another.

###### Examples

- `tae x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(2)` = 0b0000001 | `tae` = 0b000 | `0000001000sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
--------------------------------------
b_push($TS, compare(r1, >=(u\16), r2))
```

###### Module

**`base`**

--------------
--------------------------------------------------------------------------------
not                       100%  ################################################################################
instr(teq)                 75%  ############################################################
instr(tne)                 75%  ############################################################
cmp                         0%  
equality                    0%  
ts                          0%  
trying splittag `not`...
Left: [teq]
Right: [tne]


#### Format `rr(3)`


![../assets/rr(3).svg](../assets/rr(3).svg)

##### The `tne` Instruction

**Test Not Equal** --- Test if the value of one register is not equal to another.

###### Examples

- `tne x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(3)` = 0b00000001 | `tne` = 0b01 | `0000000101sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
------------------------
b_push($TS, r1\=r2)
```

###### Module

**`base`**

--------------

##### The `teq` Instruction

**Test Equal** --- Test if the value of one register is equal to another.

###### Examples

- `teq x, y`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `rr(3)` = 0b00000001 | `teq` = 0b00 | `0000000100sssrrr` |

###### Semantics

```
[reg(r, r1), reg(s, r2)]
------------------------
b_push($TS, r1==r2)
```

###### Module

**`base`**

--------------

### Instruction Format `r(_)`

--------------------------------------------------------------------------------
sp                        100%  ################################################################################
arith                      50%  ########################################
byte                       50%  ########################################
indirect                   50%  ########################################
jump                       50%  ########################################
pc                         50%  ########################################
pop                        50%  ########################################
push                       50%  ########################################
word                       50%  ########################################
ra                         25%  ####################
sxt                        25%  ####################
instr(callr)               19%  ###############
instr(jr)                  19%  ###############
instr(neg)                 19%  ###############
instr(popb)                19%  ###############
instr(popw)                19%  ###############
instr(pushb)               19%  ###############
instr(pushw)               19%  ###############
instr(seb)                 19%  ###############
trying splittag `sp`...
Left: [callr,jr,neg,seb]
Right: [pushb,pushw,popb,popw]

--------------------------------------------------------------------------------
arith                     100%  ################################################################################
indirect                  100%  ################################################################################
jump                      100%  ################################################################################
pc                        100%  ################################################################################
ra                         50%  ########################################
sxt                        50%  ########################################
instr(callr)               38%  ##############################
instr(jr)                  38%  ##############################
instr(neg)                 38%  ##############################
instr(seb)                 38%  ##############################
trying splittag `arith`...
Left: [callr,jr]
Right: [neg,seb]

--------------------------------------------------------------------------------
ra                        100%  ################################################################################
instr(callr)               75%  ############################################################
instr(jr)                  75%  ############################################################
indirect                    0%  
jump                        0%  
pc                          0%  
trying splittag `ra`...
Left: [jr]
Right: [callr]

--------------------------------------------------------------------------------
sxt                       100%  ################################################################################
instr(neg)                 75%  ############################################################
instr(seb)                 75%  ############################################################
arith                       0%  
trying splittag `sxt`...
Left: [neg]
Right: [seb]

--------------------------------------------------------------------------------
byte                      100%  ################################################################################
pop                       100%  ################################################################################
push                      100%  ################################################################################
word                      100%  ################################################################################
instr(popb)                38%  ##############################
instr(popw)                38%  ##############################
instr(pushb)               38%  ##############################
instr(pushw)               38%  ##############################
sp                          0%  
trying splittag `byte`...
Left: [pushw,popw]
Right: [pushb,popb]

--------------------------------------------------------------------------------
pop                       100%  ################################################################################
push                      100%  ################################################################################
instr(popw)                75%  ############################################################
instr(pushw)               75%  ############################################################
sp                          0%  
word                        0%  
trying splittag `pop`...
Left: [pushw]
Right: [popw]

--------------------------------------------------------------------------------
pop                       100%  ################################################################################
push                      100%  ################################################################################
instr(popb)                75%  ############################################################
instr(pushb)               75%  ############################################################
byte                        0%  
sp                          0%  
trying splittag `pop`...
Left: [pushb]
Right: [popb]


#### Format `r(1)`


![../assets/r(1).svg](../assets/r(1).svg)

##### The `pushb` Instruction

**Push Byte** --- Push a byte from a register onto the stack.

###### Examples

- `pushb x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `pushb` = 0b0110 | `0000000010110rrr` |

###### Semantics

```
[reg(r, rs)]
------------
todo
```

###### Module

**`stack`**

--------------

##### The `pushw` Instruction

**Push Word** --- Push a word from a register onto the stack.

###### Examples

- `pushw x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `pushw` = 0b0100 | `0000000010100rrr` |

###### Semantics

```
[reg(r, rs)]
------------
todo
```

###### Module

**`stack`**

--------------

##### The `popb` Instruction

**Pop Byte** --- Pop a byte from the stack into a register.

###### Examples

- `popb x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `popb` = 0b0111 | `0000000010111rrr` |

###### Semantics

```
[reg(r, rd)]
------------
todo
```

###### Module

**`stack`**

--------------

##### The `popw` Instruction

**Pop Word** --- Pop a word from the stack into a register.

###### Examples

- `popw x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `popw` = 0b0101 | `0000000010101rrr` |

###### Semantics

```
[reg(r, rd)]
------------
todo
```

###### Module

**`stack`**

--------------

##### The `callr` Instruction

**Call Register** --- Call a subroutine at the address in a register.

###### Examples

- `callr x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `callr` = 0b0001 | `0000000010001rrr` |

###### Semantics

```
[reg(r, abs_lbl)]
-----------------
$PC <- abs_lbl;
$RA <- $PC+2
```

###### Module

**`base`**

--------------

##### The `jr` Instruction

**Jump Register** --- Jump to the address in a register.

###### Examples

- `jr x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `jr` = 0b0000 | `0000000010000rrr` |

###### Semantics

```
[reg(r, abs_lbl)]
-----------------
$PC <- abs_lbl
```

###### Module

**`base`**

--------------

##### The `neg` Instruction

**Negate** --- Negate the value in a register.

###### Examples

- `neg x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `neg` = 0b0010 | `0000000010010rrr` |

###### Semantics

```
[reg(r, rd)]
------------
rd <- -rd
```

###### Module

**`imms`**

--------------

##### The `seb` Instruction

**Sign Extend Byte** --- Sign extend a byte in a register.

###### Examples

- `seb x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(1)` = 0b000000001 | `seb` = 0b0011 | `0000000010011rrr` |

###### Semantics

```
[reg(r, rd)]
---------------
rd <- sxt(rd\8)
```

###### Module

**`base`**

--------------
--------------------------------------------------------------------------------
gp                        100%  ################################################################################
mp                        100%  ################################################################################
hi                         50%  ########################################
lo                         50%  ########################################
rd                         50%  ########################################
wr                         50%  ########################################
instr('rd.gp')             38%  ##############################
instr('rd.mp.hi')          38%  ##############################
instr('rd.mp.lo')          38%  ##############################
instr('wr.gp')             38%  ##############################
data                        0%  
trying splittag `gp`...
Left: ['rd.mp.lo','rd.mp.hi']
Right: ['rd.gp','wr.gp']

--------------------------------------------------------------------------------
hi                        100%  ################################################################################
lo                        100%  ################################################################################
instr('rd.mp.hi')          75%  ############################################################
instr('rd.mp.lo')          75%  ############################################################
data                        0%  
mp                          0%  
rd                          0%  
trying splittag `hi`...
Left: ['rd.mp.lo']
Right: ['rd.mp.hi']

--------------------------------------------------------------------------------
rd                        100%  ################################################################################
wr                        100%  ################################################################################
instr('rd.gp')             75%  ############################################################
instr('wr.gp')             75%  ############################################################
data                        0%  
gp                          0%  
trying splittag `rd`...
Left: ['wr.gp']
Right: ['rd.gp']


#### Format `r(2)`


![../assets/r(2).svg](../assets/r(2).svg)

##### The `rd.mp.lo` Instruction

**Read $MP.lo** --- Read the low word in the system `$MP` register into a general purpose register.

###### Examples

- `rd.mp.lo x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | `'rd.mp.lo'` = 0b000 | `0000000001000rrr` |

###### Semantics

```
[reg(r, rd)]
-------------
rd <- lo($MP)
```

###### Module

**`mul`**

--------------

##### The `rd.mp.hi` Instruction

**Read $MP.hi** --- Read the high word in the system `$MP` register into a general purpose register.

###### Examples

- `rd.mp.hi x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | `'rd.mp.hi'` = 0b001 | `0000000001001rrr` |

###### Semantics

```
[reg(r, rd)]
-------------
rd <- hi($MP)
```

###### Module

**`mul`**

--------------

##### The `rd.gp` Instruction

**Read $GP** --- Read the value of the system `$GP` register into a general purpose register.

###### Examples

- `rd.gp x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | `'rd.gp'` = 0b011 | `0000000001011rrr` |

###### Semantics

```
[reg(r, rd)]
------------
rd <- $GP
```

###### Module

**`globals`**

--------------

##### The `wr.gp` Instruction

**Write $GP** --- Write a value to the system `$GP` register from a general purpose register.

###### Examples

- `wr.gp x`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `r(2)` = 0b0000000001 | `'wr.gp'` = 0b010 | `0000000001010rrr` |

###### Semantics

```
[reg(r, rs)]
------------
$GP <- rs
```

###### Module

**`globals`**

--------------

### Instruction Format `o`

--------------------------------------------------------------------------------
prsv_rstr                  71%  #########################################################
sp                         71%  #########################################################
ts                         71%  #########################################################
cc                         36%  #############################
prsv                       36%  #############################
rstr                       36%  #############################
pc                         29%  #######################
boolean                    21%  #################
cy                         21%  #################
ra                         21%  #################
dbg                        14%  ###########
exc                        14%  ###########
gp                         14%  ###########
kernel                     14%  ###########
kr                         14%  ###########
mp                         14%  ###########
push                       14%  ###########
ret                        14%  ###########
wr                         14%  ###########
call                        7%  ######
data                        7%  ######
indirect                    7%  ######
interrupts                  7%  ######
jump                        7%  ######
ov                          7%  ######
security                    7%  ######
sleep                       7%  ######
instr('BREAK')              5%  ####
instr('NONEXE0')            5%  ####
instr('UNIMPL')             5%  ####
instr('clr.cy')             5%  ####
instr(kcall)                5%  ####
instr(kret)                 5%  ####
instr('prsv.cc')            5%  ####
instr('prsv.gp')            5%  ####
instr('prsv.mp')            5%  ####
instr('prsv.ra')            5%  ####
instr('prsv.ts')            5%  ####
instr(ret)                  5%  ####
instr('rstr.cc')            5%  ####
instr('rstr.gp')            5%  ####
instr('rstr.mp')            5%  ####
instr('rstr.ra')            5%  ####
instr('rstr.ts')            5%  ####
instr('set.cy')             5%  ####
instr(sleep)                5%  ####
instr(tand)                 5%  ####
instr(tcy)                  5%  ####
instr(tdup)                 5%  ####
instr(tnot)                 5%  ####
instr(tor)                  5%  ####
instr(tov)                  5%  ####
instr(tpush0)               5%  ####
instr(tpush1)               5%  ####
instr(vijt)                 5%  ####
trying splittag `prsv_rstr`...
Left: ['NONEXE0','UNIMPL','BREAK',kret,kcall,ret,tov,tcy,'clr.cy','set.cy',tpush0,tpush1,tnot,tand,tor,tdup,sleep,vijt]
Right: ['prsv.mp','rstr.mp','prsv.ts','rstr.ts','prsv.ra','rstr.ra','prsv.gp','rstr.gp','prsv.cc','rstr.cc']

--------------------------------------------------------------------------------
ts                         89%  #######################################################################
pc                         44%  ####################################
boolean                    33%  ###########################
cc                         33%  ###########################
cy                         33%  ###########################
dbg                        22%  ##################
exc                        22%  ##################
kernel                     22%  ##################
kr                         22%  ##################
push                       22%  ##################
ret                        22%  ##################
wr                         22%  ##################
call                       11%  #########
data                       11%  #########
indirect                   11%  #########
interrupts                 11%  #########
jump                       11%  #########
ov                         11%  #########
ra                         11%  #########
security                   11%  #########
sleep                      11%  #########
instr('BREAK')              8%  #######
instr('NONEXE0')            8%  #######
instr('UNIMPL')             8%  #######
instr('clr.cy')             8%  #######
instr(kcall)                8%  #######
instr(kret)                 8%  #######
instr(ret)                  8%  #######
instr('set.cy')             8%  #######
instr(sleep)                8%  #######
instr(tand)                 8%  #######
instr(tcy)                  8%  #######
instr(tdup)                 8%  #######
instr(tnot)                 8%  #######
instr(tor)                  8%  #######
instr(tov)                  8%  #######
instr(tpush0)               8%  #######
instr(tpush1)               8%  #######
instr(vijt)                 8%  #######
trying splittag `ts`...
Left: ['NONEXE0','UNIMPL','BREAK',kret,kcall,ret,'clr.cy','set.cy',sleep,vijt]
Right: [tov,tcy,tpush0,tpush1,tnot,tand,tor,tdup]

--------------------------------------------------------------------------------
pc                         80%  ################################################################
cy                         40%  ################################
dbg                        40%  ################################
exc                        40%  ################################
kernel                     40%  ################################
kr                         40%  ################################
ret                        40%  ################################
wr                         40%  ################################
call                       20%  ################
cc                         20%  ################
indirect                   20%  ################
interrupts                 20%  ################
jump                       20%  ################
ra                         20%  ################
security                   20%  ################
sleep                      20%  ################
instr('BREAK')             15%  ############
instr('NONEXE0')           15%  ############
instr('UNIMPL')            15%  ############
instr('clr.cy')            15%  ############
instr(kcall)               15%  ############
instr(kret)                15%  ############
instr(ret)                 15%  ############
instr('set.cy')            15%  ############
instr(sleep)               15%  ############
instr(vijt)                15%  ############
trying splittag `pc`...
Left: ['NONEXE0','UNIMPL','BREAK','clr.cy','set.cy',sleep]
Right: [kret,kcall,ret,vijt]

--------------------------------------------------------------------------------
cy                         67%  #####################################################
dbg                        67%  #####################################################
exc                        67%  #####################################################
wr                         67%  #####################################################
cc                         33%  ###########################
interrupts                 33%  ###########################
sleep                      33%  ###########################
instr('BREAK')             25%  ####################
instr('NONEXE0')           25%  ####################
instr('UNIMPL')            25%  ####################
instr('clr.cy')            25%  ####################
instr('set.cy')            25%  ####################
instr(sleep)               25%  ####################
trying splittag `cy`...
Left: ['NONEXE0','UNIMPL','BREAK',sleep]
Right: ['clr.cy','set.cy']

--------------------------------------------------------------------------------
dbg                       100%  ################################################################################
exc                       100%  ################################################################################
cc                         50%  ########################################
interrupts                 50%  ########################################
sleep                      50%  ########################################
instr('BREAK')             38%  ##############################
instr('NONEXE0')           38%  ##############################
instr('UNIMPL')            38%  ##############################
instr(sleep)               38%  ##############################
trying splittag `dbg`...
Left: ['NONEXE0',sleep]
Right: ['UNIMPL','BREAK']

--------------------------------------------------------------------------------
cc                        100%  ################################################################################
interrupts                100%  ################################################################################
sleep                     100%  ################################################################################
instr('NONEXE0')           75%  ############################################################
instr(sleep)               75%  ############################################################
trying splittag `cc`...
Left: ['NONEXE0']
Right: [sleep]

--------------------------------------------------------------------------------
instr('BREAK')             75%  ############################################################
instr('UNIMPL')            75%  ############################################################
dbg                         0%  
exc                         0%  
trying splittag `instr('BREAK')`...
Left: ['UNIMPL']
Right: ['BREAK']

--------------------------------------------------------------------------------
instr('clr.cy')            75%  ############################################################
instr('set.cy')            75%  ############################################################
cy                          0%  
wr                          0%  
trying splittag `instr('clr.cy')`...
Left: ['set.cy']
Right: ['clr.cy']

--------------------------------------------------------------------------------
kernel                    100%  ################################################################################
kr                        100%  ################################################################################
ret                       100%  ################################################################################
call                       50%  ########################################
indirect                   50%  ########################################
jump                       50%  ########################################
ra                         50%  ########################################
security                   50%  ########################################
instr(kcall)               38%  ##############################
instr(kret)                38%  ##############################
instr(ret)                 38%  ##############################
instr(vijt)                38%  ##############################
pc                          0%  
trying splittag `kernel`...
Left: [ret,vijt]
Right: [kret,kcall]

--------------------------------------------------------------------------------
indirect                  100%  ################################################################################
jump                      100%  ################################################################################
ra                        100%  ################################################################################
ret                       100%  ################################################################################
security                  100%  ################################################################################
instr(ret)                 75%  ############################################################
instr(vijt)                75%  ############################################################
pc                          0%  
trying splittag `indirect`...
Left: [ret]
Right: [vijt]

--------------------------------------------------------------------------------
call                      100%  ################################################################################
ret                       100%  ################################################################################
instr(kcall)               75%  ############################################################
instr(kret)                75%  ############################################################
kernel                      0%  
kr                          0%  
pc                          0%  
trying splittag `call`...
Left: [kret]
Right: [kcall]

--------------------------------------------------------------------------------
boolean                    75%  ############################################################
cc                         50%  ########################################
push                       50%  ########################################
cy                         25%  ####################
data                       25%  ####################
ov                         25%  ####################
instr(tand)                19%  ###############
instr(tcy)                 19%  ###############
instr(tdup)                19%  ###############
instr(tnot)                19%  ###############
instr(tor)                 19%  ###############
instr(tov)                 19%  ###############
instr(tpush0)              19%  ###############
instr(tpush1)              19%  ###############
ts                          0%  
trying splittag `boolean`...
Left: [tov,tcy,tpush0,tpush1,tdup]
Right: [tnot,tand,tor]

--------------------------------------------------------------------------------
cc                         80%  ################################################################
push                       80%  ################################################################
cy                         40%  ################################
data                       40%  ################################
ov                         40%  ################################
instr(tcy)                 30%  ########################
instr(tdup)                30%  ########################
instr(tov)                 30%  ########################
instr(tpush0)              30%  ########################
instr(tpush1)              30%  ########################
ts                          0%  
trying splittag `cc`...
Left: [tpush0,tpush1,tdup]
Right: [tov,tcy]

--------------------------------------------------------------------------------
push                       67%  #####################################################
data                       67%  #####################################################
instr(tdup)                50%  ########################################
instr(tpush0)              50%  ########################################
instr(tpush1)              50%  ########################################
ts                          0%  
trying splittag `push`...
Left: [tdup]
Right: [tpush0,tpush1]

--------------------------------------------------------------------------------
instr(tpush0)              75%  ############################################################
instr(tpush1)              75%  ############################################################
push                        0%  
ts                          0%  
trying splittag `instr(tpush0)`...
Left: [tpush1]
Right: [tpush0]

--------------------------------------------------------------------------------
cy                        100%  ################################################################################
ov                        100%  ################################################################################
instr(tcy)                 75%  ############################################################
instr(tov)                 75%  ############################################################
cc                          0%  
ts                          0%  
trying splittag `cy`...
Left: [tov]
Right: [tcy]

--------------------------------------------------------------------------------
instr(tand)                50%  ########################################
instr(tnot)                50%  ########################################
instr(tor)                 50%  ########################################
boolean                     0%  
ts                          0%  
trying splittag `instr(tand)`...
Left: [tnot,tor]
Right: [tand]

--------------------------------------------------------------------------------
instr(tnot)                75%  ############################################################
instr(tor)                 75%  ############################################################
boolean                     0%  
ts                          0%  
trying splittag `instr(tnot)`...
Left: [tor]
Right: [tnot]

--------------------------------------------------------------------------------
prsv                      100%  ################################################################################
rstr                      100%  ################################################################################
cc                         40%  ################################
gp                         40%  ################################
mp                         40%  ################################
ra                         40%  ################################
ts                         40%  ################################
instr('prsv.cc')           15%  ############
instr('prsv.gp')           15%  ############
instr('prsv.mp')           15%  ############
instr('prsv.ra')           15%  ############
instr('prsv.ts')           15%  ############
instr('rstr.cc')           15%  ############
instr('rstr.gp')           15%  ############
instr('rstr.mp')           15%  ############
instr('rstr.ra')           15%  ############
instr('rstr.ts')           15%  ############
prsv_rstr                   0%  
sp                          0%  
trying splittag `prsv`...
Left: ['rstr.mp','rstr.ts','rstr.ra','rstr.gp','rstr.cc']
Right: ['prsv.mp','prsv.ts','prsv.ra','prsv.gp','prsv.cc']

--------------------------------------------------------------------------------
cc                         40%  ################################
gp                         40%  ################################
mp                         40%  ################################
ra                         40%  ################################
ts                         40%  ################################
instr('rstr.cc')           30%  ########################
instr('rstr.gp')           30%  ########################
instr('rstr.mp')           30%  ########################
instr('rstr.ra')           30%  ########################
instr('rstr.ts')           30%  ########################
prsv_rstr                   0%  
rstr                        0%  
sp                          0%  
trying splittag `cc`...
Left: ['rstr.mp','rstr.ts','rstr.ra','rstr.gp']
Right: ['rstr.cc']

--------------------------------------------------------------------------------
gp                         50%  ########################################
mp                         50%  ########################################
ra                         50%  ########################################
ts                         50%  ########################################
instr('rstr.gp')           38%  ##############################
instr('rstr.mp')           38%  ##############################
instr('rstr.ra')           38%  ##############################
instr('rstr.ts')           38%  ##############################
prsv_rstr                   0%  
rstr                        0%  
sp                          0%  
trying splittag `gp`...
Left: ['rstr.mp','rstr.ts','rstr.ra']
Right: ['rstr.gp']

--------------------------------------------------------------------------------
mp                         67%  #####################################################
ra                         67%  #####################################################
ts                         67%  #####################################################
instr('rstr.mp')           50%  ########################################
instr('rstr.ra')           50%  ########################################
instr('rstr.ts')           50%  ########################################
prsv_rstr                   0%  
rstr                        0%  
sp                          0%  
trying splittag `mp`...
Left: ['rstr.ts','rstr.ra']
Right: ['rstr.mp']

--------------------------------------------------------------------------------
ra                        100%  ################################################################################
ts                        100%  ################################################################################
instr('rstr.ra')           75%  ############################################################
instr('rstr.ts')           75%  ############################################################
prsv_rstr                   0%  
rstr                        0%  
sp                          0%  
trying splittag `ra`...
Left: ['rstr.ts']
Right: ['rstr.ra']

--------------------------------------------------------------------------------
cc                         40%  ################################
gp                         40%  ################################
mp                         40%  ################################
ra                         40%  ################################
ts                         40%  ################################
instr('prsv.cc')           30%  ########################
instr('prsv.gp')           30%  ########################
instr('prsv.mp')           30%  ########################
instr('prsv.ra')           30%  ########################
instr('prsv.ts')           30%  ########################
prsv                        0%  
prsv_rstr                   0%  
sp                          0%  
trying splittag `cc`...
Left: ['prsv.mp','prsv.ts','prsv.ra','prsv.gp']
Right: ['prsv.cc']

--------------------------------------------------------------------------------
gp                         50%  ########################################
mp                         50%  ########################################
ra                         50%  ########################################
ts                         50%  ########################################
instr('prsv.gp')           38%  ##############################
instr('prsv.mp')           38%  ##############################
instr('prsv.ra')           38%  ##############################
instr('prsv.ts')           38%  ##############################
prsv                        0%  
prsv_rstr                   0%  
sp                          0%  
trying splittag `gp`...
Left: ['prsv.mp','prsv.ts','prsv.ra']
Right: ['prsv.gp']

--------------------------------------------------------------------------------
mp                         67%  #####################################################
ra                         67%  #####################################################
ts                         67%  #####################################################
instr('prsv.mp')           50%  ########################################
instr('prsv.ra')           50%  ########################################
instr('prsv.ts')           50%  ########################################
prsv                        0%  
prsv_rstr                   0%  
sp                          0%  
trying splittag `mp`...
Left: ['prsv.ts','prsv.ra']
Right: ['prsv.mp']

--------------------------------------------------------------------------------
ra                        100%  ################################################################################
ts                        100%  ################################################################################
instr('prsv.ra')           75%  ############################################################
instr('prsv.ts')           75%  ############################################################
prsv                        0%  
prsv_rstr                   0%  
sp                          0%  
trying splittag `ra`...
Left: ['prsv.ts']
Right: ['prsv.ra']


#### Format `o`


![../assets/o.svg](../assets/o.svg)

##### The `NONEXE0` Instruction

**Non-executable (0s Version)** --- Triggers a "non-executable instruction" exception. The entire instruction is 16 `0`s.

###### Examples

- `NONEXE0`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'NONEXE0'` = 0b000000 | `0000000000000000` |

###### Semantics

```
[]
------------------
$PC <- nonexe0_isr
```

###### Module

**`base`**

--------------

##### The `UNIMPL` Instruction

**Unimplemented** --- Unimplemented instruction.

###### Examples

- `UNIMPL`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'UNIMPL'` = 0b000010 | `0000000000000010` |

###### Semantics

```
[]
-----------------
$PC <- unimpl_isr
```

###### Module

**`dbg`**

--------------

##### The `BREAK` Instruction

**Breakpoint** --- Trigger a breakpoint.

###### Examples

- `BREAK`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'BREAK'` = 0b000011 | `0000000000000011` |

###### Semantics

```
[]
----------------
$PC <- break_isr
```

###### Module

**`dbg`**

--------------

##### The `kret` Instruction

**Kernel Return** --- Return from kernel mode.

###### Examples

- `kret`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `kret` = 0b000110 | `0000000000000110` |

###### Semantics

```
[]
----------
$PC <- $KR
```

###### Module

**`interrupts`**

--------------

##### The `kcall` Instruction

**Kernel Call** --- Call a kernel function. The function index must be stored in `v`.

###### Examples

- `kcall`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `kcall` = 0b000111 | `0000000000000111` |

###### Semantics

```
[]
-------------
$KR <- $PC+2;
$PC <- $v;
todo
```

###### Module

**`interrupts`**

--------------

##### The `ret` Instruction

**Return** --- Return from a subroutine.

###### Examples

- `ret`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `ret` = 0b000100 | `0000000000000100` |

###### Semantics

```
[]
----------
$PC <- $RA
```

###### Module

**`base`**

--------------

##### The `tov` Instruction

**Test Overflow** --- Test for overflow.

###### Examples

- `tov`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tov` = 0b001010 | `0000000000001010` |

###### Semantics

```
[]
----------------------------------------
b_push($TS, bit($CC, overflow_flag_idx))
```

###### Module

**`base`**

--------------

##### The `tcy` Instruction

**Test Carry** --- Test for carry.

###### Examples

- `tcy`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tcy` = 0b001011 | `0000000000001011` |

###### Semantics

```
[]
-------------------------------------
b_push($TS, bit($CC, carry_flag_idx))
```

###### Module

**`base`**

--------------

##### The `clr.cy` Instruction

**Clear Carry** --- Clear the carry flag.

###### Examples

- `clr.cy`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'clr.cy'` = 0b000011 | `0000000000000011` |

###### Semantics

```
[]
-----------------------------
bit($CC, carry_flag_idx) <- 0
```

###### Module

**`base`**

--------------

##### The `set.cy` Instruction

**Set Carry** --- Set the carry flag.

###### Examples

- `set.cy`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'set.cy'` = 0b000010 | `0000000000000010` |

###### Semantics

```
[]
-----------------------------
bit($CC, carry_flag_idx) <- 1
```

###### Module

**`base`**

--------------

##### The `tpush0` Instruction

**Teststack Push 0** --- Push 0 onto the test stack.

###### Examples

- `tpush0`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tpush0` = 0b010011 | `0000000000010011` |

###### Semantics

```
[]
--------------
b_push($TS, 0)
```

###### Module

**`tsops`**

--------------

##### The `tpush1` Instruction

**Teststack Push 1** --- Push 1 onto the test stack.

###### Examples

- `tpush1`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tpush1` = 0b010010 | `0000000000010010` |

###### Semantics

```
[]
--------------
b_push($TS, 1)
```

###### Module

**`tsops`**

--------------

##### The `tnot` Instruction

**Teststack NOT** --- Perform a NOT operation on the test stack.

###### Examples

- `tnot`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tnot` = 0b001101 | `0000000000001101` |

###### Semantics

```
[]
------------------------
b_push($TS, ~b_pop($TS))
```

###### Module

**`tsops`**

--------------

##### The `tand` Instruction

**Teststack AND** --- Perform an AND operation on the test stack.

###### Examples

- `tand`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tand` = 0b000111 | `0000000000000111` |

###### Semantics

```
[]
----
todo
```

###### Module

**`tsops`**

--------------

##### The `tor` Instruction

**Teststack OR** --- Perform an OR operation on the test stack.

###### Examples

- `tor`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tor` = 0b001100 | `0000000000001100` |

###### Semantics

```
[]
----
todo
```

###### Module

**`tsops`**

--------------

##### The `tdup` Instruction

**Teststack Duplicate** --- Duplicate the top value on the test stack.

###### Examples

- `tdup`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `tdup` = 0b001000 | `0000000000001000` |

###### Semantics

```
[]
----
todo
```

###### Module

**`tsops`**

--------------

##### The `prsv.mp` Instruction

**Preserve $MP** --- Preserve the value of the `$MP` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'prsv.mp'` = 0b011001 | `0000000000011001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`mul`**

--------------

##### The `rstr.mp` Instruction

**Restore $MP** --- Restore the value of the `$MP` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'rstr.mp'` = 0b010001 | `0000000000010001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`mul`**

--------------

##### The `prsv.ts` Instruction

**Preserve $TS** --- Preserve the value of the `$TS` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'prsv.ts'` = 0b110000 | `0000000000110000` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `rstr.ts` Instruction

**Restore $TS** --- Restore the value of the `$TS` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'rstr.ts'` = 0b100000 | `0000000000100000` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `prsv.ra` Instruction

**Preserve $RA** --- Preserve the value of the `$RA` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'prsv.ra'` = 0b110001 | `0000000000110001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `rstr.ra` Instruction

**Restore $RA** --- Restore the value of the `$RA` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'rstr.ra'` = 0b100001 | `0000000000100001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `prsv.gp` Instruction

**Preserve $GP** --- Preserve the value of the `$GP` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'prsv.gp'` = 0b001101 | `0000000000001101` |

###### Semantics

```
[]
----
todo
```

###### Module

**`globals`**

--------------

##### The `rstr.gp` Instruction

**Restore $GP** --- Restore the value of the `$GP` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'rstr.gp'` = 0b001001 | `0000000000001001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`globals`**

--------------

##### The `prsv.cc` Instruction

**Preserve $CC** --- Preserve the value of the `$CC` register onto the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'prsv.cc'` = 0b000111 | `0000000000000111` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `rstr.cc` Instruction

**Restore $CC** --- Restore the value of the `$CC` register from the stack.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `'rstr.cc'` = 0b000101 | `0000000000000101` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `sleep` Instruction

**Sleep** --- Puts processor into low-power sleep mode.

###### Examples

- `sleep`

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `sleep` = 0b000001 | `0000000000000001` |

###### Semantics

```
[]
----
todo
```

###### Module

**`interrupts`**

--------------

##### The `vijt` Instruction

**Valid Indirect Jump Target** --- When `$CC.jt` is `1`, the `callr` and `jr` instructions must jump to one of these instructions or an exception is raised.

###### Layout


| Format Prefix | Opcode | Bit Layout |
|:---:|:---:|:---:|
| `o` = 0b0000000000 | `vijt` = 0b000101 | `0000000000000101` |

###### Semantics

```
[]
------------------------------------------------------
if bit($CC, jmp_tgt_validation_en_flag_bit) {
    if bit($CC, jmp_tgt_validation_req_flag_bit) {
        bit($CC, jmp_tgt_validation_req_flag_bit) <- 0
    } else {
        exception(ILLINSTR)
    }
}
```

###### Module

**`security`**

--------------
