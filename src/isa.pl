:- module(isa, [
    fmt_operands_description/3,
    fmt_huffman_enc/1,
    fmt_instr_title_description/4,
    fmt_immsizeconstraint/2,
    fmt_opcodesizeconstraint/2,
    instr_size/1,
    register_size/1,
    gpr_count_bits/1,
    regname_uses/2,
    reguse_description/2,
    sysregname_name_size_description/4,
    instr/1,
    gfmt/1,
    gprreg/1,
    sysreg/1,
    fmt_instr/2
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).
:- op(20, fx, #).

:- use_module(library(clpfd)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_operands_description(rri,   [i, s, r], 'Register-register-immediate').
fmt_operands_description(subr,  [i],       'Subroutine Call').        
fmt_operands_description(li,    [i, r],    'Load Immediate').         
fmt_operands_description(b,     [i],       'Branch').                 
fmt_operands_description(ri(_), [i, r],    'Register-immediate').     
fmt_operands_description(rrr,   [t, s, r], 'Register-register-register').
fmt_operands_description(rr(_), [s, r],    'Register-register').
fmt_operands_description(r(_),  [r],       'Register').               
fmt_operands_description(o,     [],        'Opcode').                 
fmt_operands_description(ext,   [],        'Reserved for Extension'). 

% A cons-cell based tree representation of the encoding space. The program uses
% this tree to construct prefix codes for the different instruction formats.
%
% The tree `(a + ((b + c) + d))` would be represented as `[a | [[b | c] | d]]`.
%
% See: https://en.wikipedia.org/wiki/Huffman_coding
fmt_huffman_enc([
    [
        rri
    |
        [
            subr
        |
            [
                b
            |
                li
            ]
        ]
    ]
|
    % Note: `[a | [b | [c | d]]] == [a, b, c | d]`
    [
        ri(1),
        ri(2),
        ext,
        rrr,
        rr(1),
        rr(2),
        rr(3),
        r(1),
        r(2),
        r(3)
    |
        o
    ]
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_immsizeconstraint(rri, Bits) :-
    2 ^ (#Bits - 1) #>= 32. % I'd like to be able to in one load/store instruction index at least 32 bytes in a stackframe.
fmt_immsizeconstraint(subr, Bits) :-
    addrsize_maxalignment(Bits, 32). % Alignment of 32-bytes is definitely too much wasted space (16 instructions wasted in worst case, 8 in average case).
fmt_immsizeconstraint(b, Bits) :-
    #Bits #>= 7. % See `design.md` for RISC-V study. They found 77.5% of conditional jumps use only 7 bit immediates.
fmt_immsizeconstraint(li, Bits) :-
    register_size(RegBits),
    #Bits #>= #RegBits div 2. % 8-bit immediates allow 16-bit immediates to be loaded in two instructions.
fmt_immsizeconstraint(ri(_), Bits) :-
    #Bits #>= 4, % Allows shift and individual bit manipulation instructions to refer to any of the 16 bits.
    % Size bits seems to be a sweet spot for evenly distributing opcode space among formats. According 
    % to the RISC-V study (see `design.md`), it should be enough 70% of the time.
    #Bits #= 6.

fmt_opcodesizeconstraint(rri, Bits) :-
    2 ^ #Bits #>= 4, % We need at least 4 = |{load,store}x{byte,word}|
    2 ^ #Bits #= 4. % I can only think of 4 instructions for this category at this time.
fmt_opcodesizeconstraint(li, Bits) :-
    2 ^ #Bits #= 2. % I think we can get away with only two instructions here.
fmt_opcodesizeconstraint(b, Bits) :-
    2 ^ #Bits #>= 2, % Need at least 2 opcodes for conditional/unconditional branch instrs.
    2 ^ #Bits #=< 16, % As an upper bound: |{<,>=}x{signed,unsigned} U {==,!=}| = 6, flag tests may mean we want a few more.
    #Bits #= 2. % Let's do 4 opcodes for unconditional branch, branch if true/false, and one extra for expansion.
fmt_opcodesizeconstraint(subr, Bits) :-
    2 ^ #Bits #= 1. % We only need one call instruction (with embedded fn address). This allows for a larger immediate.

%%%%%%%%%%%%%%%%%%%%%%%%%% Instruction Assignments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_instr_title_description(rri, lb, 'Load Byte', 'Load a byte from memory into a register.').
fmt_instr_title_description(rri, lw, 'Load Word', 'Load a word from memory into a register.').
fmt_instr_title_description(rri, sb, 'Store Byte', 'Store a byte from a register into memory.').
fmt_instr_title_description(rri, sw, 'Store Word', 'Store a word from a register into memory.').

fmt_instr_title_description(subr, call, 'Call Subroutine', 'Call a subroutine at the specified address.').

fmt_instr_title_description(b, b, 'Branch', 'Branch to the specified address by adding the immediate offset to `$PC`.').
fmt_instr_title_description(b, bt, 'Branch If True', 'Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.').
fmt_instr_title_description(b, bf, 'Branch If False', 'Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.').

fmt_instr_title_description(li, li, 'Load Immediate', 'Load an immediate value into a register.').
fmt_instr_title_description(li, szi, 'Shift Zero-extended Immediate', 'Left-shift a zero-extended immediate value into a register.').

fmt_instr_title_description(ri(1), lgb, 'Load Global Byte', 'Load a byte from a memory address offset from `$GP`.').
fmt_instr_title_description(ri(1), lgw, 'Load Global Word', 'Load a word from a memory address offset from `$GP`.').
fmt_instr_title_description(ri(1), sgb, 'Store Global Byte', 'Store a byte into memory address offset from `$GP`.').
fmt_instr_title_description(ri(1), sgw, 'Store Global Word', 'Store a word into memory address offset from `$GP`.').
fmt_instr_title_description(ri(1), tbit, 'Test Bit', 'Test a specific bit in a register, modifying `$TS`.').
fmt_instr_title_description(ri(1), cbit, 'Clear Bit', 'Clear a specific bit in a register.').
fmt_instr_title_description(ri(1), sbit, 'Set Bit', 'Set a specific bit in a register.').
fmt_instr_title_description(ri(1), tli, 'Test Less-than Immediate', 'Test if a register value is less than an immediate value.').
fmt_instr_title_description(ri(1), tgei, 'Test Greater-than or Equal Immediate', 'Test if a register value is greater than or equal to an immediate value.').
fmt_instr_title_description(ri(1), tbi, 'Test Below Immediate', 'Test if a register value is below an immediate value.').
fmt_instr_title_description(ri(1), taei, 'Test Above or Equal', 'Test if a register value is above or equal to an immediate value.').
fmt_instr_title_description(ri(1), tnei, 'Test Not Equal Immediate', 'Test if a register value is not equal to an immediate value.').
fmt_instr_title_description(ri(1), teqi, 'Test Equal Immediate', 'Test if a register value is equal to an immediate value.').
fmt_instr_title_description(ri(1), addi, 'Add Immediate', 'Add an immediate value to a register.').
fmt_instr_title_description(ri(1), andi, 'AND Immediate', 'Perform a bitwise AND between a register and an immediate value.').
fmt_instr_title_description(ri(1), ori, 'OR Immediate', 'Perform a bitwise OR between a register and an immediate value.').

fmt_instr_title_description(ri(1), xori, 'XOR Immediate', 'Perform a bitwise XOR between a register and an immediate value.').
fmt_instr_title_description(ri(1), addicy, 'Add Immediate with Carry', 'Add an immediate value and the carry bit to a register.').
fmt_instr_title_description(ri(1), subicy, 'Subtract Immediate with Carry', 'Sutract an immediate value and the carry bit from a register.').
fmt_instr_title_description(ri(1), lsr, 'Logical Shift Right', 'Perform a logical shift right on a register by an immediate value.').
fmt_instr_title_description(ri(1), lsl, 'Logical Shift Left', 'Perform a logical shift left on a register by an immediate value.').
fmt_instr_title_description(ri(1), asr, 'Arithmetic Shift Right', 'Perform an arithmetic shift right on a register by an immediate value.').

fmt_instr_title_description(rr(1), add, 'Add', 'Add the values of two registers.').
fmt_instr_title_description(rr(1), sub, 'Subtract', 'Subtract the value of one register from another.').
fmt_instr_title_description(rr(1), and, 'AND', 'Perform a bitwise AND between two registers.').
fmt_instr_title_description(rr(1), or, 'OR', 'Perform a bitwise OR between two registers.').
fmt_instr_title_description(rr(1), xor, 'XOR', 'Perform a bitwise XOR between two registers.').
fmt_instr_title_description(rr(1), mov, 'Move', 'Move the value from one register to another.').
fmt_instr_title_description(rr(1), addcy, 'Add with Carry', 'Add the values of two registers with carry.').
fmt_instr_title_description(rr(1), subcy, 'Subtract with Carry', 'Subtract the value of one register from another with carry.').
fmt_instr_title_description(rr(2), tl, 'Test Less-than', 'Test if the value of one register is less than another.').
fmt_instr_title_description(rr(2), tge, 'Test Greater-than or Equal', 'Test if the value of one register is greater than or equal to another.').
fmt_instr_title_description(rr(2), tb, 'Test Below', 'Test if the value of one register is below another.').
fmt_instr_title_description(rr(2), tae, 'Test Above or Equal', 'Test if the value of one register is above or equal to another.').
fmt_instr_title_description(rr(3), tne, 'Test Not Equal', 'Test if the value of one register is not equal to another.').
fmt_instr_title_description(rr(3), teq, 'Test Equal', 'Test if the value of one register is equal to another.').

fmt_instr_title_description(rrr, mulstep, 'Multiplication Step', 'Computes one step in a full 16-bit by 16-bit multiplication.').

fmt_instr_title_description(r(1), pushb, 'Push Byte', 'Push a byte from a register onto the stack.').
fmt_instr_title_description(r(1), pushw, 'Push Word', 'Push a word from a register onto the stack.').
fmt_instr_title_description(r(1), popb, 'Pop Byte', 'Pop a byte from the stack into a register.').
fmt_instr_title_description(r(1), popw, 'Pop Word', 'Pop a word from the stack into a register.').
fmt_instr_title_description(r(1), callr, 'Call Register', 'Call a subroutine at the address in a register.').
fmt_instr_title_description(r(1), jr, 'Jump Register', 'Jump to the address in a register.').
fmt_instr_title_description(r(1), neg, 'Negate', 'Negate the value in a register.').
fmt_instr_title_description(r(2), seb, 'Sign Extend Byte', 'Sign extend a byte in a register.').
fmt_instr_title_description(r(2), 'rd.mp.lo', 'Read $MP.lo', 'Read the low word in the system `$MP` register into a general purpose register.').
fmt_instr_title_description(r(2), 'rd.mp.hi', 'Read $MP.hi', 'Read the high word in the system `$MP` register into a general purpose register.').
fmt_instr_title_description(r(2), 'rd.gp', 'Read $GP', 'Read the value of the system `$GP` register into a general purpose register.').
fmt_instr_title_description(r(3), 'wr.gp', 'Write $GP', 'Write a value to the system `$GP` register from a general purpose register.').

fmt_instr_title_description(o, 'NONEXE1', 'Non-executable (1''s Version)', 'Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.').
fmt_instr_title_description(o, 'BREAK', 'Breakpoint', 'Trigger a breakpoint.').
fmt_instr_title_description(o, 'HALT', 'Halt', 'Halt the processor.').
fmt_instr_title_description(o, 'UNIMPL', 'Unimplemented', 'Unimplemented instruction.').
fmt_instr_title_description(o, kret, 'Kernel Return', 'Return from kernel mode.').
fmt_instr_title_description(o, kcall, 'Kernel Call', 'Call a kernel function.').
fmt_instr_title_description(o, ret, 'Return', 'Return from a subroutine.').
fmt_instr_title_description(o, tov, 'Test Overflow', 'Test for overflow.').
fmt_instr_title_description(o, tcy, 'Test Carry', 'Test for carry.').
fmt_instr_title_description(o, 'clr.cy', 'Clear Carry', 'Clear the carry flag.').
fmt_instr_title_description(o, 'set.cy', 'Set Carry', 'Set the carry flag.').
fmt_instr_title_description(o, tpush0, 'Teststack Push 0', 'Push 0 onto the test stack.').
fmt_instr_title_description(o, tpush1, 'Teststack Push 1', 'Push 1 onto the test stack.').
fmt_instr_title_description(o, tnot, 'Teststack NOT', 'Perform a NOT operation on the test stack.').
fmt_instr_title_description(o, tand, 'Teststack AND', 'Perform an AND operation on the test stack.').
fmt_instr_title_description(o, tor, 'Teststack OR', 'Perform an OR operation on the test stack.').
fmt_instr_title_description(o, tdup, 'Teststack Duplicate', 'Duplicate the top value on the test stack.').
fmt_instr_title_description(o, 'prsv.mp', 'Preserve $MP', 'Preserve the value of the `$MP` register onto the stack.').
fmt_instr_title_description(o, 'rstr.mp', 'Restore $MP', 'Restore the value of the `$MP` register from the stack.').
fmt_instr_title_description(o, 'prsv.ts', 'Preserve $TS', 'Preserve the value of the `$TS` register onto the stack.').
fmt_instr_title_description(o, 'rstr.ts', 'Restore $TS', 'Restore the value of the `$TS` register from the stack.').
fmt_instr_title_description(o, 'prsv.ra', 'Preserve $RA', 'Preserve the value of the `$RA` register onto the stack.').
fmt_instr_title_description(o, 'rstr.ra', 'Restore $RA', 'Restore the value of the `$RA` register from the stack.').
fmt_instr_title_description(o, 'prsv.gp', 'Preserve $GP', 'Preserve the value of the `$GP` register onto the stack.').
fmt_instr_title_description(o, 'rstr.gp', 'Restore $GP', 'Restore the value of the `$GP` register from the stack.').
fmt_instr_title_description(o, 'prsv.cc', 'Preserve $CC', 'Preserve the value of the `$CC` register onto the stack.').
fmt_instr_title_description(o, 'rstr.cc', 'Restore $CC', 'Restore the value of the `$CC` register from the stack.').

%%%%%%%%%%%%%%%%%%%%%%%%% Synthetic Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

synthinstr_descr_expansion_reversability(clr(Reg),    'Clear a register',               xor(Reg, Reg),  reversible).
synthinstr_descr_expansion_reversability(nop,         'The no-op instruction',          ori(sp, 0),     reversible).
synthinstr_descr_expansion_reversability(incr(Reg),   'Increment a register',           addi(Reg, 1),   reversible).
synthinstr_descr_expansion_reversability(decr(Reg),   'Increment a register',           subi(Reg, 1),   reversible).
synthinstr_descr_expansion_reversability(inv(Reg),    'Bitwise inversion (complement)', xori(Reg, -1),  reversible).
synthinstr_descr_expansion_reversability(not(Reg),    'Invert a boolean (0 or 1)',      xori(Reg, 1),   reversible).
synthinstr_descr_expansion_reversability(tg(R1, R2),  'Test greater-than',              tl(R2, R1),     one_way).
synthinstr_descr_expansion_reversability(tle(R1, R2), 'Test Less-than or Equal',        tge(R2, R1),    one_way).
synthinstr_descr_expansion_reversability(ta(R1, R2),  'Test Above',                     ta(R2, R1),     one_way).
synthinstr_descr_expansion_reversability(tbe(R1, R2), 'Test Below or Equal',            tae(R2, R1),    one_way).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instr_size(16).
register_size(16).
gpr_count_bits(3).
addr_reg_count_bits(2).

regname_uses(sp, [stack_ptr, addr]).
regname_uses(x,  [temp, arg(1), addr]).
regname_uses(y,  [temp, arg(2), addr]).
regname_uses(z,  [temp, arg(3), addr]).
regname_uses(w,  [temp, arg(4)]).
regname_uses(v,  [temp, retval]).
regname_uses(a,  [saved]).
regname_uses(b,  [saved]).

reguse_description(stack_ptr, 'Register serves as the stack pointer.').
reguse_description(addr,      'Only some of the registers can be used as the address in a load/store instruction.').
reguse_description(temp,      'Register may be used to hold temporary values without restriction.').
reguse_description(arg(_),    'Register is used as the Nth argument to a subroutine.').
reguse_description(retval,    'A subroutine''s return value is passed in this register.').
reguse_description(saved,     'A called subroutine must save the content of these registers before using them, but their values persist across subroutine calls.').

sysregname_name_size_description(pc, 'Program Counter', 16, 'Keeps track of the currently executing instruction.').
sysregname_name_size_description(ra, 'Return Address',  16, 'Saves the program counter for subroutine return.').
sysregname_name_size_description(ts, 'Test Stack',      16, 'Stores boolean values in a stack used by branch instructions.').
sysregname_name_size_description(cc, 'Condition Codes', 16, 'Stores carry and overflow flags.').
sysregname_name_size_description(gp, 'Global Pointer',  16, 'Points to a region of process memory reserved for global variables.').
sysregname_name_size_description(kr, 'Kernel Return',   16, 'Holds the value of the program counter during interrupts.').
% To multiply two words together in software, you need 4 or 5 words worth of space.
% I considered having two 32-bit system registers for doing multiplication, but instead I'd like to just use one.
% You could eliminate this one too, but `mulstep` would become a 4-register instruction (i.e. `mulstep x:y, w:v`).
sysregname_name_size_description(mp, 'Multiplication Product', 32, 'Holds the accumulating product during a multiplication.').

%%%%%%%%%%%%%%%%%%%%%%% Instruction Details %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addrsize_maxalignment(Bits, MaxAlign) :-
    instr_size(InstrSizeBits),
    2 ^ #Bits #>= (2 ^ #InstrSizeBits) div #MaxAlign.

instr(Instr) :- fmt_instr_title_description(_, Instr, _, _).
gfmt(GFmt) :- fmt_operands_description(GFmt, _, _).

gprreg(R) :- regname_uses(R, _).
sysreg(Name) :- sysregname_name_size_description(Name, _, _, _).

fmt_instr(Fmt, Instr) :- fmt_instr_title_description(Fmt, Instr, _, _).

