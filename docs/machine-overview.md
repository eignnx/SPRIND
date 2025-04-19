
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

[`lb`](instruction-listing.md#the-lb-instruction), [`lw`](instruction-listing.md#the-lw-instruction), [`sb`](instruction-listing.md#the-sb-instruction), [`sw`](instruction-listing.md#the-sw-instruction), [`call`](instruction-listing.md#the-call-instruction), [`b`](instruction-listing.md#the-b-instruction), [`bt`](instruction-listing.md#the-bt-instruction), [`bf`](instruction-listing.md#the-bf-instruction), [`li`](instruction-listing.md#the-li-instruction), [`szi`](instruction-listing.md#the-szi-instruction), [`lsr`](instruction-listing.md#the-lsr-instruction), [`lsl`](instruction-listing.md#the-lsl-instruction), [`asr`](instruction-listing.md#the-asr-instruction), [`add`](instruction-listing.md#the-add-instruction), [`sub`](instruction-listing.md#the-sub-instruction), [`and`](instruction-listing.md#the-and-instruction), [`or`](instruction-listing.md#the-or-instruction), [`xor`](instruction-listing.md#the-xor-instruction), [`mov`](instruction-listing.md#the-mov-instruction), [`addcy`](instruction-listing.md#the-addcy-instruction), [`subcy`](instruction-listing.md#the-subcy-instruction), [`tl`](instruction-listing.md#the-tl-instruction), [`tge`](instruction-listing.md#the-tge-instruction), [`tb`](instruction-listing.md#the-tb-instruction), [`tae`](instruction-listing.md#the-tae-instruction), [`tne`](instruction-listing.md#the-tne-instruction), [`teq`](instruction-listing.md#the-teq-instruction), [`callr`](instruction-listing.md#the-callr-instruction), [`jr`](instruction-listing.md#the-jr-instruction), [`seb`](instruction-listing.md#the-seb-instruction), [`NONEXE0`](instruction-listing.md#the-nonexe0-instruction), [`ret`](instruction-listing.md#the-ret-instruction), [`tov`](instruction-listing.md#the-tov-instruction), [`tcy`](instruction-listing.md#the-tcy-instruction), [`clr.cy`](instruction-listing.md#the-clrcy-instruction), [`set.cy`](instruction-listing.md#the-setcy-instruction), 
### `globals`

**Globals**

Instructions related to the `$GP` global variable pointer register and gloal variables.


#### Dependencies

`[base]`

#### Instructions

[`lgb`](instruction-listing.md#the-lgb-instruction), [`lgw`](instruction-listing.md#the-lgw-instruction), [`sgb`](instruction-listing.md#the-sgb-instruction), [`sgw`](instruction-listing.md#the-sgw-instruction), [`rd.gp`](instruction-listing.md#the-rdgp-instruction), [`wr.gp`](instruction-listing.md#the-wrgp-instruction), [`prsv.gp`](instruction-listing.md#the-prsvgp-instruction), [`rstr.gp`](instruction-listing.md#the-rstrgp-instruction), 
### `bittests`

**Bit Tests**

Instructions related to testing specific bits of a register or memory.


#### Dependencies

`[base]`

#### Instructions

[`tbit`](instruction-listing.md#the-tbit-instruction), [`cbit`](instruction-listing.md#the-cbit-instruction), [`sbit`](instruction-listing.md#the-sbit-instruction), [`tbitm`](instruction-listing.md#the-tbitm-instruction), [`cbitm`](instruction-listing.md#the-cbitm-instruction), [`sbitm`](instruction-listing.md#the-sbitm-instruction), [`tpopm`](instruction-listing.md#the-tpopm-instruction), 
### `dbg`

**Debugging**

Instructions which communicate with a debugger.


#### Dependencies

`[base]`

#### Instructions

[`BREAK`](instruction-listing.md#the-break-instruction), [`UNIMPL`](instruction-listing.md#the-unimpl-instruction), 
### `imms`

**Immediates**

Instructions contain embedded (immediate) values. Generally duplicates of instructions which operate on registers.


#### Dependencies

`[base]`

#### Instructions

[`tli`](instruction-listing.md#the-tli-instruction), [`tgei`](instruction-listing.md#the-tgei-instruction), [`tbi`](instruction-listing.md#the-tbi-instruction), [`taei`](instruction-listing.md#the-taei-instruction), [`tnei`](instruction-listing.md#the-tnei-instruction), [`teqi`](instruction-listing.md#the-teqi-instruction), [`addi`](instruction-listing.md#the-addi-instruction), [`andi`](instruction-listing.md#the-andi-instruction), [`ori`](instruction-listing.md#the-ori-instruction), [`xori`](instruction-listing.md#the-xori-instruction), [`addicy`](instruction-listing.md#the-addicy-instruction), [`subicy`](instruction-listing.md#the-subicy-instruction), [`neg`](instruction-listing.md#the-neg-instruction), 
### `interrupts`

**Interrupts**

Instructions for handling and operating during hardware/software interrupts.


#### Dependencies

`[base, stack]`

#### Instructions

[`kret`](instruction-listing.md#the-kret-instruction), [`kcall`](instruction-listing.md#the-kcall-instruction), [`prsv.ts`](instruction-listing.md#the-prsvts-instruction), [`rstr.ts`](instruction-listing.md#the-rstrts-instruction), [`prsv.ra`](instruction-listing.md#the-prsvra-instruction), [`rstr.ra`](instruction-listing.md#the-rstrra-instruction), [`prsv.cc`](instruction-listing.md#the-prsvcc-instruction), [`rstr.cc`](instruction-listing.md#the-rstrcc-instruction), [`sleep`](instruction-listing.md#the-sleep-instruction), 
### `mul`

**Multiply**

Instructions related to integer multiplication.


#### Dependencies

`[base, stack]`

#### Instructions

[`mulstep`](instruction-listing.md#the-mulstep-instruction), [`rd.mp.lo`](instruction-listing.md#the-rdmplo-instruction), [`rd.mp.hi`](instruction-listing.md#the-rdmphi-instruction), [`prsv.mp`](instruction-listing.md#the-prsvmp-instruction), [`rstr.mp`](instruction-listing.md#the-rstrmp-instruction), 
### `security`

**Security**

Instructions related to computer security.


#### Dependencies

`[base]`

#### Instructions

[`vijt`](instruction-listing.md#the-vijt-instruction), 
### `stack`

**Stack**

Instructions for manipulating the subroutine stack.


#### Dependencies

`[base]`

#### Instructions

[`pushb`](instruction-listing.md#the-pushb-instruction), [`pushw`](instruction-listing.md#the-pushw-instruction), [`popb`](instruction-listing.md#the-popb-instruction), [`popw`](instruction-listing.md#the-popw-instruction), 
### `tsops`

**Test-stack Operations**

Instructions for manipulating the test-stack (`$TS`).


#### Dependencies

`[base]`

#### Instructions

[`rd.ts`](instruction-listing.md#the-rdts-instruction), [`wr.ts`](instruction-listing.md#the-wrts-instruction), [`tpush0`](instruction-listing.md#the-tpush0-instruction), [`tpush1`](instruction-listing.md#the-tpush1-instruction), [`tnot`](instruction-listing.md#the-tnot-instruction), [`tand`](instruction-listing.md#the-tand-instruction), [`tor`](instruction-listing.md#the-tor-instruction), [`tdup`](instruction-listing.md#the-tdup-instruction), 