:- module(isa, [
    fmt_operands/2,
    genericfmt_description/2,
    fmt_huffman_enc/1,
    fmt_instr_title_description/4,
    fmt_immsizeconstraint/2,
    fmt_opcodesizeconstraint/2,
    register_size/1,
    gpr_count_bits/1,
    addr_reg_count_bits/1,
    regid_name_uses/2
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).
:- op(20, fx, #).

:- use_module(library(clpfd)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- det(fmt_operands/2).

fmt_operands(lsd, [i, a, r]).
fmt_operands(subr, [i]).
fmt_operands(li, [i, r]).
fmt_operands(b, [i]).
fmt_operands(ri(_), [i, r]).
fmt_operands(rr(_), ['R', r]).
fmt_operands(r(_), [r]).
fmt_operands(o, []).
fmt_operands(ext, []).

:- det(genericfmt_description/2).

genericfmt_description(lsd,  'Load-store with Displacement').
genericfmt_description(subr,  'Subroutine Call').
genericfmt_description(li,    'Load Immediate').
genericfmt_description(b,     'Branch').
genericfmt_description(lsx,   'Load-store with Index').
genericfmt_description(rr(_), 'Register-register').
genericfmt_description(ri(_), 'Register-immediate').
genericfmt_description(r(_),  'Register').
genericfmt_description(o,     'Opcode').
genericfmt_description(ext,   'Reserved for Extension').

% A cons-cell based tree representation of the encoding space. The program uses
% this tree to construct prefix codes for the different instruction formats.
%
% The tree `(a + ((b + c) + d))` would be represented as `[a | [[b | c] | d]]`.
%
% See: https://en.wikipedia.org/wiki/Huffman_coding
fmt_huffman_enc([
    [
        lsd
    |
        [
            subr
        |
            [
                b
            |
                ext
            ]
        ]
    ]
|
    % Note: `[a | [b | [c | d]]] == [a, b, c | d]`
    [
        li,
        ri(1),
        ri(2),
        ri(3),
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

fmt_immsizeconstraint(lsd, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(subr, Bits) :- addrsize_maxalignment(Bits, 32).
fmt_immsizeconstraint(b, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(li, Bits) :- #Bits #>= 8.
fmt_immsizeconstraint(ri(_), Bits) :- #Bits #= 6. % #Bits #>= 4. %

fmt_opcodesizeconstraint(lsd, Bits) :- 2 ^ #Bits #>= 4.
fmt_opcodesizeconstraint(b, Bits) :- #Bits #>= 1, 2 ^ #Bits #=< 16.
fmt_opcodesizeconstraint(subr, Bits) :- 2 ^ #Bits #= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%% Instruction Assignments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_instr_title_description(lsd, lb, 'Load Byte', 'Load a byte from memory into a register.').
fmt_instr_title_description(lsd, lw, 'Load Word', 'Load a word from memory into a register.').
fmt_instr_title_description(lsd, sb, 'Store Byte', 'Store a byte from a register into memory.').
fmt_instr_title_description(lsd, sw, 'Store Word', 'Store a word from a register into memory.').

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

fmt_instr_title_description(ri(2), xori, 'XOR Immediate', 'Perform a bitwise XOR between a register and an immediate value.').
fmt_instr_title_description(ri(2), lsri, 'Logical Shift Right Immediate', 'Perform a logical shift right on a register by an immediate value.').
fmt_instr_title_description(ri(2), lsli, 'Logical Shift Left Immediate', 'Perform a logical shift left on a register by an immediate value.').
fmt_instr_title_description(ri(2), asri, 'Arithmetic Shift Right Immediate', 'Perform an arithmetic shift right on a register by an immediate value.').

fmt_instr_title_description(rr(1), add, 'Add', 'Add the values of two registers.').
fmt_instr_title_description(rr(1), sub, 'Subtract', 'Subtract the value of one register from another.').
fmt_instr_title_description(rr(1), and, 'AND', 'Perform a bitwise AND between two registers.').
fmt_instr_title_description(rr(1), or, 'OR', 'Perform a bitwise OR between two registers.').
fmt_instr_title_description(rr(1), xor, 'XOR', 'Perform a bitwise XOR between two registers.').
fmt_instr_title_description(rr(1), mov, 'Move', 'Move the value from one register to another.').
fmt_instr_title_description(rr(1), addcy, 'Add with Carry', 'Add the values of two registers with carry.').
fmt_instr_title_description(rr(1), subcy, 'Subtract with Carry', 'Subtract the value of one register from another with carry.').
fmt_instr_title_description(rr(1), tl, 'Test Less-than', 'Test if the value of one register is less than another.').
fmt_instr_title_description(rr(1), tge, 'Test Greater-than or Equal', 'Test if the value of one register is greater than or equal to another.').
fmt_instr_title_description(rr(1), tb, 'Test Below', 'Test if the value of one register is below another.').
fmt_instr_title_description(rr(1), tae, 'Test Above or Equal', 'Test if the value of one register is above or equal to another.').
fmt_instr_title_description(rr(1), tne, 'Test Not Equal', 'Test if the value of one register is not equal to another.').
fmt_instr_title_description(rr(1), teq, 'Test Equal', 'Test if the value of one register is equal to another.').

fmt_instr_title_description(r(1), pushb, 'Push Byte', 'Push a byte from a register onto the stack.').
fmt_instr_title_description(r(1), pushw, 'Push Word', 'Push a word from a register onto the stack.').
fmt_instr_title_description(r(1), popb, 'Pop Byte', 'Pop a byte from the stack into a register.').
fmt_instr_title_description(r(1), popw, 'Pop Word', 'Pop a word from the stack into a register.').
fmt_instr_title_description(r(1), callr, 'Call Register', 'Call a subroutine at the address in a register.').
fmt_instr_title_description(r(1), jr, 'Jump Register', 'Jump to the address in a register.').
fmt_instr_title_description(r(1), neg, 'Negate', 'Negate the value in a register.').
fmt_instr_title_description(r(1), seb, 'Sign Extend Byte', 'Sign extend a byte in a register.').
fmt_instr_title_description(r(1), 'r.hi', 'Read $HI', 'Read the value of the system `$HI` register into a general purpose register.').
fmt_instr_title_description(r(1), 'r.gp', 'Read $GP', 'Read the value of the system `$GP` register into a general purpose register.').
fmt_instr_title_description(r(1), 'w.gp', 'Write $GP', 'Write a value to the system `$GP` register from a general purpose register.').

fmt_instr_title_description(o, 'NONEXE1', 'Non-executable (1''s Version)', 'Non-executable instruction (1''s version).').
fmt_instr_title_description(o, 'BREAK', 'Breakpoint', 'Trigger a breakpoint.').
fmt_instr_title_description(o, 'HALT', 'Halt', 'Halt the processor.').
fmt_instr_title_description(o, 'UNIMPL', 'Unimplemented', 'Unimplemented instruction.').
fmt_instr_title_description(o, kret, 'Kernel Return', 'Return from kernel mode.').
fmt_instr_title_description(o, kcall, 'Kernel Call', 'Call a kernel function.').
fmt_instr_title_description(o, ret, 'Return', 'Return from a subroutine.').
fmt_instr_title_description(o, tov, 'Test Overflow', 'Test for overflow.').
fmt_instr_title_description(o, tcy, 'Test Carry', 'Test for carry.').
fmt_instr_title_description(o, cy0, 'Clear Carry', 'Clear the carry flag.').
fmt_instr_title_description(o, cy1, 'Set Carry', 'Set the carry flag.').
fmt_instr_title_description(o, tpush0, 'Teststack Push 0', 'Push 0 onto the test stack.').
fmt_instr_title_description(o, tpush1, 'Teststack Push 1', 'Push 1 onto the test stack.').
fmt_instr_title_description(o, tnot, 'Teststack NOT', 'Perform a NOT operation on the test stack.').
fmt_instr_title_description(o, tand, 'Teststack AND', 'Perform an AND operation on the test stack.').
fmt_instr_title_description(o, tor, 'Teststack OR', 'Perform an OR operation on the test stack.').
fmt_instr_title_description(o, tdup, 'Teststack Duplicate', 'Duplicate the top value on the test stack.').
fmt_instr_title_description(o, 'prsv.hi', 'Preserve $HI', 'Preserve the value of the `$HI` register onto the stack.').
fmt_instr_title_description(o, 'rstr.hi', 'Restore $HI', 'Restore the value of the `$HI` register from the stack.').
fmt_instr_title_description(o, 'prsv.ts', 'Preserve $TS', 'Preserve the value of the `$TS` register onto the stack.').
fmt_instr_title_description(o, 'rstr.ts', 'Restore $TS', 'Restore the value of the `$TS` register from the stack.').
fmt_instr_title_description(o, 'prsv.ra', 'Preserve $RA', 'Preserve the value of the `$RA` register onto the stack.').
fmt_instr_title_description(o, 'rstr.ra', 'Restore $RA', 'Restore the value of the `$RA` register from the stack.').
fmt_instr_title_description(o, 'prsv.gp', 'Preserve $GP', 'Preserve the value of the `$GP` register onto the stack.').
fmt_instr_title_description(o, 'rstr.gp', 'Restore $GP', 'Restore the value of the `$GP` register from the stack.').
fmt_instr_title_description(o, 'prsv.cc', 'Preserve $CC', 'Preserve the value of the `$CC` register onto the stack.').
fmt_instr_title_description(o, 'rstr.cc', 'Restore $CC', 'Restore the value of the `$CC` register from the stack.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_size(16).
gpr_count_bits(3).
addr_reg_count_bits(2).

regid_name_uses(sp, [stack_ptr, addr]).
regid_name_uses(x,  [temp, arg(1), addr]).
regid_name_uses(y,  [temp, arg(2), addr]).
regid_name_uses(z,  [temp, arg(3), addr]).
regid_name_uses(w,  [temp, arg(4)]).
regid_name_uses(v,  [temp, retval]).
regid_name_uses(a,  [saved]).
regid_name_uses(b,  [saved]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addrsize_maxalignment(Bits, MaxAlign) :-
    2 ^ #Bits #>= 2^16 div #MaxAlign.
