
# SPRIND Abstract Machine Overview


## General Purpose Registers


| Register Name | [Uses](#register-uses-and-calling-convention) |
|:---:|:---:|
| `sp` | stack_ptr |
| `x` | temp, arg(1) |
| `y` | temp, arg(2) |
| `z` | temp, arg(3) |
| `w` | temp, arg(4) |
| `v` | temp, retval |
| `a` | saved |
| `b` | saved |

## Register Uses and Calling Convention


| Usage Name | Description |
|:---:|:----|
| `stack_ptr` | Register serves as the stack pointer. |
| `temp` | Register may be used to hold temporary values without restriction. |
| `arg(_)` | Register is used as the Nth argument to a subroutine. |
| `retval` | A subroutine's return value is passed in this register. |
| `saved` | A called subroutine must save the content of these registers before using them, but their values persist across subroutine calls. |

## System Registers


| Register | Register Name | Size | Description |
|:---:|:---:|:---:|:----|
| `$PC` | Program Counter | 16-bits | Keeps track of the currently executing instruction. |
| `$RA` | Return Address | 16-bits | Saves the program counter for subroutine return. |
| `$TS` | Test Stack | 16-bits | Stores boolean values in a stack used by branch instructions. |
| `$CC` | Condition Codes | 16-bits | Stores carry and overflow flags. |
| `$GP` | Global Pointer | 16-bits | Points to a region of process memory reserved for global variables. |
| `$KR` | Kernel Return | 16-bits | Holds the value of the program counter during interrupts. |
| `$MP` | Multiplication Product | 32-bits | Holds the accumulating product during a multiplication. |

## Extension Modules

Extension modules are groups of instructions which add functionality to the processor.
They allow the processor to be built up in stages, or can be omitted for a simpler processor.

### `base`

**Base**

The minimal set of features needed for general computation.


#### Dependencies

`[]`

#### Instructions

[`lb`](instruction-listing#the-lb-instruction), [`lw`](instruction-listing#the-lw-instruction), [`sb`](instruction-listing#the-sb-instruction), [`sw`](instruction-listing#the-sw-instruction), [`call`](instruction-listing#the-call-instruction), [`b`](instruction-listing#the-b-instruction), [`bt`](instruction-listing#the-bt-instruction), [`bf`](instruction-listing#the-bf-instruction), [`li`](instruction-listing#the-li-instruction), [`szi`](instruction-listing#the-szi-instruction), [`lsr`](instruction-listing#the-lsr-instruction), [`lsl`](instruction-listing#the-lsl-instruction), [`asr`](instruction-listing#the-asr-instruction), [`add`](instruction-listing#the-add-instruction), [`sub`](instruction-listing#the-sub-instruction), [`and`](instruction-listing#the-and-instruction), [`or`](instruction-listing#the-or-instruction), [`xor`](instruction-listing#the-xor-instruction), [`mov`](instruction-listing#the-mov-instruction), [`addcy`](instruction-listing#the-addcy-instruction), [`subcy`](instruction-listing#the-subcy-instruction), [`tl`](instruction-listing#the-tl-instruction), [`tge`](instruction-listing#the-tge-instruction), [`tb`](instruction-listing#the-tb-instruction), [`tae`](instruction-listing#the-tae-instruction), [`tne`](instruction-listing#the-tne-instruction), [`teq`](instruction-listing#the-teq-instruction), [`callr`](instruction-listing#the-callr-instruction), [`jr`](instruction-listing#the-jr-instruction), [`seb`](instruction-listing#the-seb-instruction), [`NONEXE0`](instruction-listing#the-nonexe0-instruction), [`HALT`](instruction-listing#the-halt-instruction), [`ret`](instruction-listing#the-ret-instruction), [`tov`](instruction-listing#the-tov-instruction), [`tcy`](instruction-listing#the-tcy-instruction), [`clr.cy`](instruction-listing#the-clrcy-instruction), [`set.cy`](instruction-listing#the-setcy-instruction), 
### `globals`

**Globals**

Instructions related to the `$GP` global variable pointer register and gloal variables.


#### Dependencies

`[base]`

#### Instructions

[`lgb`](instruction-listing#the-lgb-instruction), [`lgw`](instruction-listing#the-lgw-instruction), [`sgb`](instruction-listing#the-sgb-instruction), [`sgw`](instruction-listing#the-sgw-instruction), [`rd.gp`](instruction-listing#the-rdgp-instruction), [`wr.gp`](instruction-listing#the-wrgp-instruction), [`prsv.gp`](instruction-listing#the-prsvgp-instruction), [`rstr.gp`](instruction-listing#the-rstrgp-instruction), 
### `bittests`

**Bit Tests**

Instructions related to testing specific bits of a register or memory.


#### Dependencies

`[base]`

#### Instructions

[`tbit`](instruction-listing#the-tbit-instruction), [`cbit`](instruction-listing#the-cbit-instruction), [`sbit`](instruction-listing#the-sbit-instruction), [`tbitm`](instruction-listing#the-tbitm-instruction), [`cbitm`](instruction-listing#the-cbitm-instruction), [`sbitm`](instruction-listing#the-sbitm-instruction), 
### `dbg`

**Debugging**

Instructions which communicate with a debugger.


#### Dependencies

`[base]`

#### Instructions

[`BREAK`](instruction-listing#the-break-instruction), [`UNIMPL`](instruction-listing#the-unimpl-instruction), 
### `imms`

**Immediates**

Instructions contain embedded (immediate) values. Generally duplicates of instructions which operate on registers.


#### Dependencies

`[base]`

#### Instructions

[`tli`](instruction-listing#the-tli-instruction), [`tgei`](instruction-listing#the-tgei-instruction), [`tbi`](instruction-listing#the-tbi-instruction), [`taei`](instruction-listing#the-taei-instruction), [`tnei`](instruction-listing#the-tnei-instruction), [`teqi`](instruction-listing#the-teqi-instruction), [`addi`](instruction-listing#the-addi-instruction), [`andi`](instruction-listing#the-andi-instruction), [`ori`](instruction-listing#the-ori-instruction), [`xori`](instruction-listing#the-xori-instruction), [`addicy`](instruction-listing#the-addicy-instruction), [`subicy`](instruction-listing#the-subicy-instruction), [`neg`](instruction-listing#the-neg-instruction), 
### `interrupts`

**Interrupts**

Instructions for handling and operating during hardware/software interrupts.


#### Dependencies

`[base, stack]`

#### Instructions

[`kret`](instruction-listing#the-kret-instruction), [`kcall`](instruction-listing#the-kcall-instruction), [`prsv.ts`](instruction-listing#the-prsvts-instruction), [`rstr.ts`](instruction-listing#the-rstrts-instruction), [`prsv.ra`](instruction-listing#the-prsvra-instruction), [`rstr.ra`](instruction-listing#the-rstrra-instruction), [`prsv.cc`](instruction-listing#the-prsvcc-instruction), [`rstr.cc`](instruction-listing#the-rstrcc-instruction), [`sleep`](instruction-listing#the-sleep-instruction), 
### `mul`

**Multiply**

Instructions related to integer multiplication.


#### Dependencies

`[base, stack]`

#### Instructions

[`mulstep`](instruction-listing#the-mulstep-instruction), [`rd.mp.lo`](instruction-listing#the-rdmplo-instruction), [`rd.mp.hi`](instruction-listing#the-rdmphi-instruction), [`prsv.mp`](instruction-listing#the-prsvmp-instruction), [`rstr.mp`](instruction-listing#the-rstrmp-instruction), 
### `security`

**Security**

Instructions related to computer security.


#### Dependencies

`[base]`

#### Instructions

[`vijt`](instruction-listing#the-vijt-instruction), 
### `stack`

**Stack**

Instructions for manipulating the subroutine stack.


#### Dependencies

`[base]`

#### Instructions

[`pushb`](instruction-listing#the-pushb-instruction), [`pushw`](instruction-listing#the-pushw-instruction), [`popb`](instruction-listing#the-popb-instruction), [`popw`](instruction-listing#the-popw-instruction), 
### `tsops`

**Test-stack Operations**

Instructions for manipulating the test-stack (`$TS`).


#### Dependencies

`[base]`

#### Instructions

[`tpush0`](instruction-listing#the-tpush0-instruction), [`tpush1`](instruction-listing#the-tpush1-instruction), [`tnot`](instruction-listing#the-tnot-instruction), [`tand`](instruction-listing#the-tand-instruction), [`tor`](instruction-listing#the-tor-instruction), [`tdup`](instruction-listing#the-tdup-instruction), 