## Study on RISC-V Compressed Instruction Set

Source: https://www2.eecs.berkeley.edu/Pubs/TechRpts/2011/EECS-2011-63.pdf

### Major Findings

Static use is the most important in my case. I don't care about cache efficiency (my CPU core will not really have caches), but I do care about ease of use from the programmer's perspective. If some feature is used in writing 90% of programs, but is only run 10% of the time, I think that's a reasonable trade off for SPRIND.

#### Branch Offsets

- > RISC-V conditional branch offsets are at most 12 bits. Unconditional jump
  > offsets can be up to 25 bits, but offsets longer than 20 bits (±256K instructions) did not occur.

  - 9 bits (+/-256) is adequate for ~92.5% of branch instructions called dynamically
  - 8 bits (+/-128) is adequate for ~90% of branch instructions called dynamically
  - 7 bits (+/-64) is adequate for ~77.5% of branch instructions called dynamically

#### Immediates

| # Bits | Range  | Dynamic Use Coverage | Static Use Coverage |
| :----: | :----: | :------------------: | :-----------------: |
|   3    |  +/-4  |         50%          |         37%         |
|   4    |  +/-8  |         55%          |         45%         |
|   5    | +/-16  |         65%          |         55%         |
|   6    | +/-32  |         73%          |         70%         |
|   7    | +/-64  |         86%          |         75%         |
|   8    | +/-128 |         90%          |         77%         |

#### Register Use

| Register | Static Use |  Description  |                          Relevance                           |
| :------: | :--------: | :-----------: | :----------------------------------------------------------: |
|    v0    |    18%     |    ret val    |                           Relevant                           |
|    sp    |    11%     | stack pointer |                           Relevant                           |
|    a0    |    10%     |      arg      |                           Relevant                           |
|    v1    |    9.5%    |    ret val    | Irrelevant; ret vals larger than 1 word not supported directly |
|   zero   |     9%     | constant zero |       Irrelevant; SPRIND does not have a zero register       |
|    a1    |     7%     |      arg      |                           Relevant                           |
|    s0    |     5%     |     saved     |                           Relevant                           |
|    a2    |     4%     |      arg      |                           Relevant                           |
|    s1    |    3.5%    |     saved     |                           Relevant                           |
|    a3    |     3%     |      arg      |                           Relevant                           |
|    s2    |    2.5%    |     saved     |                           Relevant                           |

This is good justification for the following layout:

| Register ID | Register Name |  Description   | RVC Register Name |
| :---------: | :-----------: | :------------: | :---------------: |
|      0      |      sp       | stack pointer  |        sp         |
|      1      |      rv       | ret val / temp |        v0         |
|      2      |       x       |   arg / temp   |        a0         |
|      3      |       y       |   arg / temp   |        a1         |
|      4      |       z       |   arg / temp   |        a2         |
|      5      |       w       |   arg / temp   |        a3         |
|      6      |       a       |     saved      |        s0         |
|      7      |       b       |     saved      |        s1         |



### RISC-V Compressed Instruction Set

Here is the final RISC-V compressed instruction set shown in the paper:

| **RVC Instr** | **RISC-V Equivalent** | **Description**                      | **Static** | **Dynamic** |
| ------------- | --------------------- | ------------------------------------ | ---------- | ----------- |
| C.ADDI        | ADDI rd,rd,imm6       | Increment register.                  | X          | X           |
| C.ADDIW       | ADDIW rd,rd,imm6      | Increment register (as 32-bit word). | X          | X           |
| C.LI          | ADDI rd,x0,imm6       | Load immediate.                      | X          |             |
| C.LWSP        | LW rd,imm6×4(sp)      | Load word, stack-relative.           | X          |             |
| C.LDSP        | LD rd,imm6×8(sp)      | Load double-word, stack-relative.    | X          |             |
| C.SWSP        | SW rs2,imm6×4(sp)     | Store word, stack-relative.          | X          |             |
| C.SDSP        | SD rs2,imm6×8(sp)     | Store double-word, stack-relative.   | X          |             |
| C.LW0         | LW rd,0(rs1)          | Load word, register-indirect.        | X          |             |
| C.LD0         | LD rd,0(rs1)          | Load double-word, register-indirect. | X          |             |
| C.ADD         | ADD rd,rs1,rd         | Add register, destructive.           | X          | X           |
| C.SUB         | SUB rd,rs1,rd         | Subtract register, destructive.      |            | X           |
| C.MOVE        | ADDI rd,rs1,          | Move register.                       | X          | X           |
| C.ADD3        | ADD rda,rs1a,rs2a     | Add register.                        |            | X           |
| C.SUB3        | SUB rda,rs1a,rs2a     | Subtract register.                   |            | X           |
| C.OR3         | OR rda,rs1a,rs2a      | Bitwise-OR register.                 |            | X           |
| C.AND3        | AND rda,rs1a,rs2a     | Bitwise-AND register.                |            | X           |
| C.SLLI        | SLLI rda,rda,shamt    | Shift left logical.                  | X          | X           |
| C.SRLI        | SRLI rda,rda,shamt    | Shift right logical.                 | X          | X           |
| C.SRAI        | SRAI rda,rda,shamt    | Shift right arithmetic.              | X          | X           |
| C.SLLIW       | SLLIW rda,rda,shamt   | Shift left logical (as 32-bit word). | X          | X           |
| C.LW          | LW rda,imm5×4(rs1a)   | Load word.                           | X          | X           |
| C.LD          | LD rda,imm5×8(rs1a)   | Load double-word.                    | X          | X           |
| C.SW          | SW rs2b,imm5×4(rs1a)  | Store word.                          | X          | X           |
| C.SD          | SD rs2b,imm5×8(rs1a)  | Store double-word.                   | X          | X           |
| C.FLW         | FLW rda,imm5×4(rs1a)  | Load floating-point word.            |            | X           |
| C.FLD         | FLD rda,imm5×8(rs1a)  | Load floating-point double-word.     |            | X           |
| C.FSW         | FSW rs2b,imm5×4(rs1a) | Store floating-point word.           |            | X           |
| C.FSD         | FSD rs2b,imm5×8(rs1a) | Store floating-point double-word.    |            | X           |
| C.BEQ         | BEQ rs1a,rs2b,imm5    | Branch if equal.                     | X          | X           |
| C.BNE         | BNE rs1a,rs2b,imm5    | Branch if not equal.                 | X          | X           |
| C.JR          | JALR x0,rs1a          | Indirect jump/subroutine return.     | X          |             |
| C.JALR        | JALR ra,rs1a          | Indirect subroutine call.            | X          |             |
| C.J           | J imm10               | Unconditional jump.                  | X          |             |

![image-20250308003742609](C:\Users\gideo\AppData\Roaming\Typora\typora-user-images\image-20250308003742609.png)

Interesting how small the immediates for branch instructions are; only 5 bits which means jumping  with a range of +/-16 compressed instructions, or +/-8 non-compressed instructions.