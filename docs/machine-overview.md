
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
