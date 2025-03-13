:- module(isa, [
    fmt_operands_description/3,
    fmt_huffman_enc/1,
    fmt_instr_title_description/4,
    fmt_immsizeconstraint/2,
    fmt_opcodesizeconstraint/2,
    instr_size/1,
    register_size/1,
    gpr_count_bits/1,
    addr_reg_count_bits/1,
    regname_uses/2,
    reguse_description/2,
    sysregname_name_size_description/4,
    instr_info/2,
    valid_semantics//1
]).

:- use_module(library(ctypes), [upper_lower/2]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).
:- op(20, fx, #).
:- op(20, fx, &).
:- op(20, fx, $$).
:- op(1050, xfy, <-).

:- use_module(library(clpfd)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- det(fmt_operands_description/3).

fmt_operands_description(lsd,   [i, a, r], 'Load-store with Displacement').
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

fmt_immsizeconstraint(lsd, Bits) :-
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

fmt_opcodesizeconstraint(lsd, Bits) :-
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
fmt_instr_title_description(ri(2), addicy, 'Add Immediate with Carry', 'Add an immediate value and the carry bit to a register.').
fmt_instr_title_description(ri(2), subicy, 'Subtract Immediate with Carry', 'Sutract an immediate value and the carry bit from a register.').
fmt_instr_title_description(ri(2), lsr, 'Logical Shift Right', 'Perform a logical shift right on a register by an immediate value.').
fmt_instr_title_description(ri(2), lsl, 'Logical Shift Left', 'Perform a logical shift left on a register by an immediate value.').
fmt_instr_title_description(ri(2), asr, 'Arithmetic Shift Right', 'Perform an arithmetic shift right on a register by an immediate value.').

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

fmt_instr_title_description(rrr, mulstep, 'Multiplication Step', 'Computes one step in a full 16-bit by 16-bit multiplication.').

fmt_instr_title_description(r(1), pushb, 'Push Byte', 'Push a byte from a register onto the stack.').
fmt_instr_title_description(r(1), pushw, 'Push Word', 'Push a word from a register onto the stack.').
fmt_instr_title_description(r(1), popb, 'Pop Byte', 'Pop a byte from the stack into a register.').
fmt_instr_title_description(r(1), popw, 'Pop Word', 'Pop a word from the stack into a register.').
fmt_instr_title_description(r(1), callr, 'Call Register', 'Call a subroutine at the address in a register.').
fmt_instr_title_description(r(1), jr, 'Jump Register', 'Jump to the address in a register.').
fmt_instr_title_description(r(1), neg, 'Negate', 'Negate the value in a register.').
fmt_instr_title_description(r(1), seb, 'Sign Extend Byte', 'Sign extend a byte in a register.').
fmt_instr_title_description(r(1), 'rd.mp.lo', 'Read $MP.lo', 'Read the low word in the system `$MP` register into a general purpose register.').
fmt_instr_title_description(r(1), 'rd.mp.hi', 'Read $MP.hi', 'Read the high word in the system `$MP` register into a general purpose register.').
fmt_instr_title_description(r(1), 'rd.gp', 'Read $GP', 'Read the value of the system `$GP` register into a general purpose register.').
fmt_instr_title_description(r(1), 'wr.gp', 'Write $GP', 'Write a value to the system `$GP` register from a general purpose register.').

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

sysregname_name_size_description('PC', 'Program Counter', 16, 'Keeps track of the currently executing instruction.').
sysregname_name_size_description('RA', 'Return Address',  16, 'Saves the program counter for subroutine return.').
sysregname_name_size_description('TS', 'Test Stack',      16, 'Stores boolean values in a stack used by branch instructions.').
sysregname_name_size_description('CC', 'Condition Codes', 16, 'Stores carry and overflow flags.').
sysregname_name_size_description('GP', 'Global Pointer',  16, 'Points to a region of process memory reserved for global variables.').
sysregname_name_size_description('KR', 'Kernel Return',   16, 'Holds the value of the program counter during interrupts.').
% To multiply two words together in software, you need 4 or 5 words worth of space.
% I considered having two 32-bit system registers for doing multiplication, but instead I'd like to just use one.
% You could eliminate this one too, but `mulstep` would become a 4-register instruction (i.e. `mulstep x:y, w:v`).
sysregname_name_size_description('MP', 'Multiplication Product', 32, 'Holds the accumulating product during a multiplication.').

%%%%%%%%%%%%%%%%%%%%%%% Instruction Details %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semanticfn_title_description(Integer,     'Integer Literal',    '') --> { integer(Integer) }.
semanticfn_title_description($$S,           'Value in System Register',                '') --> { can_be(S, sysreg) }.
semanticfn_title_description($R,                'Value in General-purpose Register',                '') --> { can_be(R, gprreg) }.
semanticfn_title_description(&A,                'Value in Address Register',                '') --> { can_be(A, adrreg) }.
semanticfn_title_description(#Imm,                'Value in Immediate Field',         '') --> { can_be(Imm, integer) }.
semanticfn_title_description([Addr],               'Byte in memory at given address', '') --> sem(Addr).
semanticfn_title_description(Dest <- Src,     'Parallel Assign',    'Assigns `Src` to `Dest`. All `_<-_` operations are executed simultaneously.') --> sem(Dest), sem(Src).
semanticfn_title_description(Binding = Expr,  'Let-assignment',     'Gives name `Binding` to the semantics expression `Expr`.') --> sem(Expr), { Binding = Expr }.
semanticfn_title_description(Stmt1 ; Stmt2,   'Joint Eval',         'Evaluates all let-assignments, then parallel-assignments in `Stmt1` and `Stmt2`') --> sem(Stmt1), sem(Stmt2).
semanticfn_title_description(A + B,           'Addition',           'Adds two expressions.') --> sem(A), sem(B).
semanticfn_title_description(A - B,           'Subtraction',        'Subtracts two expressions') --> sem(A), sem(B).
semanticfn_title_description(A >> B,          'Right Shift',        'Performs logical right shift') --> sem(A), sem(B).
semanticfn_title_description(A << B,          'Logical Left Shift', 'Performs logical left shift') --> sem(A), sem(B).
semanticfn_title_description(A == B,          'Equality',           'Evaluates to `true` or `false` if operands are equal or not equal respectively.') --> sem(A), sem(B).
semanticfn_title_description(zxt(A),            'Zero Extend',        'Extend a binary value from A bits to B bits, left-padding with zeros.') --> sem(A).
semanticfn_title_description(sxt(A),            'Sign Extend',        'Extend a binary value from A bits to B bits, left-padding with the sign bit of the original number.') --> sem(A).
semanticfn_title_description(b_and(A, B),     'Bitwise AND',        'Perform bitwise AND between two values.') --> sem(A), sem(B).
semanticfn_title_description(b_or(A, B),      'Bitwise OR',         'Perform bitwise OR between two values.') --> sem(A), sem(B).
semanticfn_title_description(b_xor(A, B),     'Bitwise XOR',        'Perform bitwise XOR between two values.') --> sem(A), sem(B).
semanticfn_title_description(hi_lo(Hi, Lo),   'Concat Bytes',       'Concatenate bytes `Hi` and `Lo` into a word where the most significant byte is `Hi` and least significant byte is `Lo`.') --> sem(Hi), sem(Lo).
semanticfn_title_description(hi(A),             'High Byte',          'Extracts the high byte from a word.') --> sem(A).
semanticfn_title_description(lo(A),             'Low Byte',           'Extracts the low byte from a word.') --> sem(A).
semanticfn_title_description(const(Sym),          'Lookup Constant',    'Retrieves the value of the named, specification constant.') --> { atom(Sym) }.
semanticfn_title_description(b_pop($$Reg),       'Pop Bit',            'Pops the LSB off from `Reg`, and shifts `Reg` right by 1. Yields the popped bit.') --> { can_be(Reg, sysreg) }.
semanticfn_title_description(if(Cond, Consq), 'If-then',            'If the condition evaluates to `true`, executes the consequent.') --> sem(Cond), sem(Consq).
semanticfn_title_description(nop,               'Does nothing.', '') --> [].

valid_semantics(Sem) --> semanticfn_title_description(Sem, _, _) -> [] ; ['non-well-formed semantic expression'(Sem)].
sem(Sem) --> valid_semantics(Sem).

can_be(Term, TypeTest) :- var(Term) ; call(TypeTest, Term).
gprreg(R) :- regname_uses(R, _).
adrreg(A) :- regname_uses(A, Uses), member(addr, Uses).
sysreg(Name) :-
    atom_codes(Name, LoCodes),
    maplist(upper_lower, UpCodes, LoCodes),
    atom_codes(UpName, UpCodes),
    sysregname_name_size_description(UpName, _, _, _).

instr_info(lb, info{
	title: 'Load Byte',
	descr: 'Load a byte from memory into a register.',
    ex: ['lb w, [sp+12]'],
    syn: lb(Rd, [Adr + Imm]),
	sem: $Rd <- zxt([&Adr + #Imm])
}).
instr_info(lw, info{
	title: 'Load Word',
	descr: 'Load a word from memory into a register.',
	ex: ['lw w, [sp+12]'],
	syn: lw(Rd, [Adr + Imm]),
	sem: (
        Ptr = b_and(&Adr + #Imm, 0b1111111111111110);
        $Rd <- hi_lo([Ptr + 1], [Ptr])
    )
}).
instr_info(sb, info{
	title: 'Store Byte',
	descr: 'Store a byte from a register into memory.',
	ex: ['sb [sp-20], x'],
	syn: sb([Adr + Imm], Rs),
	sem: [&Adr + #Imm] <- $Rs
}).
instr_info(sw, info{
	title: 'Store Word',
	descr: 'Store a word from a register into memory.',
	ex: ['sw [sp-20], x'],
	syn: sw([Adr + Imm], Rs),
	sem: (
        Ptr = b_and(&Adr + #Imm, 0b1111111111111110);
        [Ptr] <- lo($Rs);
        [Ptr + 1] <- hi($Rs)
    )
}).

instr_info(call, info{
	title: 'Call Subroutine',
	descr: 'Call a subroutine at the specified address.',
	ex: ['call SOME_LABEL'],
	syn: call(Imm),
	sem: (
        $$pc <- $$pc + (sxt(#Imm) << const(subr_align));
        $$ra <- $$pc + 2
    )
}).

instr_info(b, info{
	title: 'Branch',
	descr: 'Branch to the specified address by adding the immediate offset to `$PC`.',
	ex: ['b SOME_LABEL'],
	syn: b(Imm),
	sem: $$pc <- $$pc + sxt(#Imm)
}).
instr_info(bt, info{
	title: 'Branch If True',
	descr: 'Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.',
	ex: ['bt SOME_LABEL'],
	syn: bt(Imm),
	sem: if(b_pop($$ts) == 1, $$pc <- $$pc + sxt(#Imm))
}).
instr_info(bf, info{
	title: 'Branch If False',
	descr: 'Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.',
	ex: ['bf SOME_LABEL'],
	syn: bf(Imm),
	sem: if(b_pop($$ts) == 0, $$pc <- $$pc + sxt(#Imm))
}).

instr_info(li, info{
	title: 'Load Immediate',
	descr: 'Load an immediate value into a register.',
	ex: ['li x, 123'],
	syn: li(Rd, Imm),
	sem: $Rd <- sxt(#Imm)
}).
instr_info(szi, info{
	title: 'Shift Zero-extended Immediate',
	descr: 'Left-shift a zero-extended immediate value into a register.',
	ex: ['szi x, 0xB3'],
	syn: szi(Rd, Imm),
	sem: $Rd <- b_or($Rd << 8, zxt(#Imm))
}).

instr_info(lgb, info{
	title: 'Load Global Byte',
	descr: 'Load a byte from a memory address offset from `$GP`.',
	ex: ['lgb x, [gp+8]'],
	syn: lgb(Rd, Imm),
	sem: $Rd <- zxt([$$gp + zxt(#Imm)])
}).
instr_info(lgw, info{
	title: 'Load Global Word',
	descr: 'Load a word from a memory address offset from `$GP`.',
	ex: ['lgw x, [gp+8]'],
	syn: lgw(Rd, Imm),
	sem: (
        Ptr = b_and($$gp + zxt(#Imm), 0b1111111111111110);
        $Rd <- hi_lo([Ptr + 1], [Ptr])
    )
}).
instr_info(sgb, info{
	title: 'Store Global Byte',
	descr: 'Store a byte into memory address offset from `$GP`.',
	ex: ['sgb [gp+8], x'],
	syn: sgb(Rs, Imm),
	sem: [$$gp + zxt(#Imm)] <- $Rs
}).
instr_info(sgw, info{
	title: 'Store Global Word',
	descr: 'Store a word into memory address offset from `$GP`.',
	ex: ['sgw [gp+8], x'],
	syn: sgw(Rs, Imm),
	sem: (
        Ptr = b_and($$gp + zxt(#Imm), 0b1111111111111110);
        hi_lo([Ptr + 1], [Ptr]) <- $Rs
    )
}).
instr_info(tbit, info{
	title: 'Test Bit',
	descr: 'Test a specific bit in a register, modifying `$TS`.',
	ex: [],
	syn: tbit,
	sem: nop
}).
instr_info(cbit, info{
	title: 'Clear Bit',
	descr: 'Clear a specific bit in a register.',
	ex: [],
	syn: cbit,
	sem: nop
}).
instr_info(sbit, info{
	title: 'Set Bit',
	descr: 'Set a specific bit in a register.',
	ex: [],
	syn: sbit,
	sem: nop
}).
instr_info(tli, info{
	title: 'Test Less-than Immediate',
	descr: 'Test if a register value is less than an immediate value.',
	ex: [],
	syn: tli,
	sem: nop
}).
instr_info(tgei, info{
	title: 'Test Greater-than or Equal Immediate',
	descr: 'Test if a register value is greater than or equal to an immediate value.',
	ex: [],
	syn: tgei,
	sem: nop
}).
instr_info(tbi, info{
	title: 'Test Below Immediate',
	descr: 'Test if a register value is below an immediate value.',
	ex: [],
	syn: tbi,
	sem: nop
}).
instr_info(taei, info{
	title: 'Test Above or Equal',
	descr: 'Test if a register value is above or equal to an immediate value.',
	ex: [],
	syn: taei,
	sem: nop
}).
instr_info(tnei, info{
	title: 'Test Not Equal Immediate',
	descr: 'Test if a register value is not equal to an immediate value.',
	ex: [],
	syn: tnei,
	sem: nop
}).
instr_info(teqi, info{
	title: 'Test Equal Immediate',
	descr: 'Test if a register value is equal to an immediate value.',
	ex: [],
	syn: teqi,
	sem: nop
}).
instr_info(addi, info{
	title: 'Add Immediate',
	descr: 'Add an immediate value to a register.',
	ex: [],
	syn: addi,
	sem: nop
}).
instr_info(andi, info{
	title: 'AND Immediate',
	descr: 'Perform a bitwise AND between a register and an immediate value.',
	ex: [],
	syn: andi,
	sem: nop
}).
instr_info(ori, info{
	title: 'OR Immediate',
	descr: 'Perform a bitwise OR between a register and an immediate value.',
	ex: [],
	syn: ori,
	sem: nop
}).

instr_info(xori, info{
	title: 'XOR Immediate',
	descr: 'Perform a bitwise XOR between a register and an immediate value.',
	ex: [],
	syn: xori,
	sem: nop
}).
instr_info(addicy, info{
	title: 'Add Immediate with Carry',
	descr: 'Add an immediate value and the carry bit to a register.',
	ex: [],
	syn: addicy,
	sem: nop
}).
instr_info(subicy, info{
	title: 'Subtract Immediate with Carry',
	descr: 'Sutract an immediate value and the carry bit from a register.',
	ex: [],
	syn: subicy,
	sem: nop
}).
instr_info(lsr, info{
	title: 'Logical Shift Right',
	descr: 'Perform a logical shift right on a register by an immediate value.',
	ex: [],
	syn: lsr,
	sem: nop
}).
instr_info(lsl, info{
	title: 'Logical Shift Left',
	descr: 'Perform a logical shift left on a register by an immediate value.',
	ex: [],
	syn: lsl,
	sem: nop
}).
instr_info(asr, info{
	title: 'Arithmetic Shift Right',
	descr: 'Perform an arithmetic shift right on a register by an immediate value.',
	ex: [],
	syn: asr,
	sem: nop
}).

instr_info(add, info{
	title: 'Add',
	descr: 'Add the values of two registers.',
	ex: [],
	syn: add,
	sem: nop
}).
instr_info(sub, info{
	title: 'Subtract',
	descr: 'Subtract the value of one register from another.',
	ex: [],
	syn: sub,
	sem: nop
}).
instr_info(and, info{
	title: 'AND',
	descr: 'Perform a bitwise AND between two registers.',
	ex: [],
	syn: and,
	sem: nop
}).
instr_info(or, info{
	title: 'OR',
	descr: 'Perform a bitwise OR between two registers.',
	ex: [],
	syn: or,
	sem: nop
}).
instr_info(xor, info{
	title: 'XOR',
	descr: 'Perform a bitwise XOR between two registers.',
	ex: [],
	syn: xor,
	sem: nop
}).
instr_info(mov, info{
	title: 'Move',
	descr: 'Move the value from one register to another.',
	ex: [],
	syn: mov,
	sem: nop
}).
instr_info(addcy, info{
	title: 'Add with Carry',
	descr: 'Add the values of two registers with carry.',
	ex: [],
	syn: addcy,
	sem: nop
}).
instr_info(subcy, info{
	title: 'Subtract with Carry',
	descr: 'Subtract the value of one register from another with carry.',
	ex: [],
	syn: subcy,
	sem: nop
}).
instr_info(tl, info{
	title: 'Test Less-than',
	descr: 'Test if the value of one register is less than another.',
	ex: [],
	syn: tl,
	sem: nop
}).
instr_info(tge, info{
	title: 'Test Greater-than or Equal',
	descr: 'Test if the value of one register is greater than or equal to another.',
	ex: [],
	syn: tge,
	sem: nop
}).
instr_info(tb, info{
	title: 'Test Below',
	descr: 'Test if the value of one register is below another.',
	ex: [],
	syn: tb,
	sem: nop
}).
instr_info(tae, info{
	title: 'Test Above or Equal',
	descr: 'Test if the value of one register is above or equal to another.',
	ex: [],
	syn: tae,
	sem: nop
}).
instr_info(tne, info{
	title: 'Test Not Equal',
	descr: 'Test if the value of one register is not equal to another.',
	ex: [],
	syn: tne,
	sem: nop
}).
instr_info(teq, info{
	title: 'Test Equal',
	descr: 'Test if the value of one register is equal to another.',
	ex: [],
	syn: teq,
	sem: nop
}).

instr_info(mulstep, info{
	title: 'Multiplication Step',
	descr: 'Computes one step in a full 16-bit by 16-bit multiplication.',
	ex: [],
	syn: mulstep,
	sem: nop
}).

instr_info(pushb, info{
	title: 'Push Byte',
	descr: 'Push a byte from a register onto the stack.',
	ex: [],
	syn: pushb,
	sem: nop
}).
instr_info(pushw, info{
	title: 'Push Word',
	descr: 'Push a word from a register onto the stack.',
	ex: [],
	syn: pushw,
	sem: nop
}).
instr_info(popb, info{
	title: 'Pop Byte',
	descr: 'Pop a byte from the stack into a register.',
	ex: [],
	syn: popb,
	sem: nop
}).
instr_info(popw, info{
	title: 'Pop Word',
	descr: 'Pop a word from the stack into a register.',
	ex: [],
	syn: popw,
	sem: nop
}).
instr_info(callr, info{
	title: 'Call Register',
	descr: 'Call a subroutine at the address in a register.',
	ex: [],
	syn: callr,
	sem: nop
}).
instr_info(jr, info{
	title: 'Jump Register',
	descr: 'Jump to the address in a register.',
	ex: [],
	syn: jr,
	sem: nop
}).
instr_info(neg, info{
	title: 'Negate',
	descr: 'Negate the value in a register.',
	ex: [],
	syn: neg,
	sem: nop
}).
instr_info(seb, info{
	title: 'Sign Extend Byte',
	descr: 'Sign extend a byte in a register.',
	ex: [],
	syn: seb,
	sem: nop
}).
instr_info('rd.mp.lo', info{
	title: 'Read $MP.lo',
	descr: 'Read the low word in the system `$MP` register into a general purpose register.',
	ex: [],
	syn: 'rd.mp.lo',
	sem: nop
}).
instr_info('rd.mp.hi', info{
	title: 'Read $MP.hi',
	descr: 'Read the high word in the system `$MP` register into a general purpose register.',
	ex: [],
	syn: 'rd.mp.hi',
	sem: nop
}).
instr_info('rd.gp', info{
	title: 'Read $GP',
	descr: 'Read the value of the system `$GP` register into a general purpose register.',
	ex: [],
	syn: 'rd.gp',
	sem: nop
}).
instr_info('wr.gp', info{
	title: 'Write $GP',
	descr: 'Write a value to the system `$GP` register from a general purpose register.',
	ex: [],
	syn: 'wr.gp',
	sem: nop
}).

instr_info('NONEXE1', info{
	title: 'Non-executable (1s Version)',
	descr: 'Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.',
	ex: [],
	syn: 'NONEXE1',
	sem: nop
}).
instr_info('BREAK', info{
	title: 'Breakpoint',
	descr: 'Trigger a breakpoint.',
	ex: [],
	syn: 'BREAK',
	sem: nop
}).
instr_info('HALT', info{
	title: 'Halt',
	descr: 'Halt the processor.',
	ex: [],
	syn: 'HALT',
	sem: nop
}).
instr_info('UNIMPL', info{
	title: 'Unimplemented',
	descr: 'Unimplemented instruction.',
	ex: [],
	syn: 'UNIMPL',
	sem: nop
}).
instr_info(kret, info{
	title: 'Kernel Return',
	descr: 'Return from kernel mode.',
	ex: [],
	syn: kret,
	sem: nop
}).
instr_info(kcall, info{
	title: 'Kernel Call',
	descr: 'Call a kernel function.',
	ex: [],
	syn: kcall,
	sem: nop
}).
instr_info(ret, info{
	title: 'Return',
	descr: 'Return from a subroutine.',
	ex: [],
	syn: ret,
	sem: nop
}).
instr_info(tov, info{
	title: 'Test Overflow',
	descr: 'Test for overflow.',
	ex: [],
	syn: tov,
	sem: nop
}).
instr_info(tcy, info{
	title: 'Test Carry',
	descr: 'Test for carry.',
	ex: [],
	syn: tcy,
	sem: nop
}).
instr_info('clr.cy', info{
	title: 'Clear Carry',
	descr: 'Clear the carry flag.',
	ex: [],
	syn: 'clr.cy',
	sem: nop
}).
instr_info('set.cy', info{
	title: 'Set Carry',
	descr: 'Set the carry flag.',
	ex: [],
	syn: 'set.cy',
	sem: nop
}).
instr_info(tpush0, info{
	title: 'Teststack Push 0',
	descr: 'Push 0 onto the test stack.',
	ex: [],
	syn: tpush0,
	sem: nop
}).
instr_info(tpush1, info{
	title: 'Teststack Push 1',
	descr: 'Push 1 onto the test stack.',
	ex: [],
	syn: tpush1,
	sem: nop
}).
instr_info(tnot, info{
	title: 'Teststack NOT',
	descr: 'Perform a NOT operation on the test stack.',
	ex: [],
	syn: tnot,
	sem: nop
}).
instr_info(tand, info{
	title: 'Teststack AND',
	descr: 'Perform an AND operation on the test stack.',
	ex: [],
	syn: tand,
	sem: nop
}).
instr_info(tor, info{
	title: 'Teststack OR',
	descr: 'Perform an OR operation on the test stack.',
	ex: [],
	syn: tor,
	sem: nop
}).
instr_info(tdup, info{
	title: 'Teststack Duplicate',
	descr: 'Duplicate the top value on the test stack.',
	ex: [],
	syn: tdup,
	sem: nop
}).
instr_info('prsv.mp', info{
	title: 'Preserve $MP',
	descr: 'Preserve the value of the `$MP` register onto the stack.',
	ex: [],
	syn: 'prsv.mp',
	sem: nop
}).
instr_info('rstr.mp', info{
	title: 'Restore $MP',
	descr: 'Restore the value of the `$MP` register from the stack.',
	ex: [],
	syn: 'rstr.mp',
	sem: nop
}).
instr_info('prsv.ts', info{
	title: 'Preserve $TS',
	descr: 'Preserve the value of the `$TS` register onto the stack.',
	ex: [],
	syn: 'prsv.ts',
	sem: nop
}).
instr_info('rstr.ts', info{
	title: 'Restore $TS',
	descr: 'Restore the value of the `$TS` register from the stack.',
	ex: [],
	syn: 'rstr.ts',
	sem: nop
}).
instr_info('prsv.ra', info{
	title: 'Preserve $RA',
	descr: 'Preserve the value of the `$RA` register onto the stack.',
	ex: [],
	syn: 'prsv.ra',
	sem: nop
}).
instr_info('rstr.ra', info{
	title: 'Restore $RA',
	descr: 'Restore the value of the `$RA` register from the stack.',
	ex: [],
	syn: 'rstr.ra',
	sem: nop
}).
instr_info('prsv.gp', info{
	title: 'Preserve $GP',
	descr: 'Preserve the value of the `$GP` register onto the stack.',
	ex: [],
	syn: 'prsv.gp',
	sem: nop
}).
instr_info('rstr.gp', info{
	title: 'Restore $GP',
	descr: 'Restore the value of the `$GP` register from the stack.',
	ex: [],
	syn: 'rstr.gp',
	sem: nop
}).
instr_info('prsv.cc', info{
	title: 'Preserve $CC',
	descr: 'Preserve the value of the `$CC` register onto the stack.',
	ex: [],
	syn: 'prsv.cc',
	sem: nop
}).
instr_info('rstr.cc', info{
	title: 'Restore $CC',
	descr: 'Restore the value of the `$CC` register from the stack.',
	ex: [],
	syn: 'rstr.cc',
	sem: nop
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addrsize_maxalignment(Bits, MaxAlign) :-
    instr_size(InstrSizeBits),
    2 ^ #Bits #>= (2 ^ #InstrSizeBits) div #MaxAlign.
