:- module(isa, [
    version/3,
    fmt_operands_description/3,
    fmt_huffman_enc/1,
    fmt_instr_title/3,
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
    fmt/1,
    gprreg/1,
    sysreg/1,
    fmt_instr/2,
    fmt_genericfmt/2
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).
:- op(20, fx, #).

:- use_module(library(clpfd)).
:- use_module(library(solution_sequences)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

version(0, 2, 1).

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
        r(2)
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

fmt_instr_title(rri, lb, 'Load Byte').
fmt_instr_title(rri, lw, 'Load Word').
fmt_instr_title(rri, sb, 'Store Byte').
fmt_instr_title(rri, sw, 'Store Word').

fmt_instr_title(subr, call, 'Call Subroutine').

fmt_instr_title(b, b, 'Branch').
fmt_instr_title(b, bt, 'Branch If True').
fmt_instr_title(b, bf, 'Branch If False').

fmt_instr_title(li, li, 'Load Immediate').
fmt_instr_title(li, szi, 'Shift Zero-extended Immediate').

fmt_instr_title(ri(1), lgb, 'Load Global Byte').
fmt_instr_title(ri(1), lgw, 'Load Global Word').
fmt_instr_title(ri(1), sgb, 'Store Global Byte').
fmt_instr_title(ri(1), sgw, 'Store Global Word').
fmt_instr_title(ri(1), tbit, 'Test Bit').
fmt_instr_title(ri(1), cbit, 'Clear Bit').
fmt_instr_title(ri(1), sbit, 'Set Bit').
fmt_instr_title(ri(1), tli, 'Test Less-than Immediate').
fmt_instr_title(ri(1), tgei, 'Test Greater-than or Equal Immediate').
fmt_instr_title(ri(1), tbi, 'Test Below Immediate').
fmt_instr_title(ri(1), taei, 'Test Above or Equal').
fmt_instr_title(ri(1), tnei, 'Test Not Equal Immediate').
fmt_instr_title(ri(1), teqi, 'Test Equal Immediate').
fmt_instr_title(ri(1), addi, 'Add Immediate').
fmt_instr_title(ri(1), andi, 'AND Immediate').
fmt_instr_title(ri(1), ori, 'OR Immediate').
fmt_instr_title(ri(1), xori, 'XOR Immediate').
fmt_instr_title(ri(1), addicy, 'Add Immediate with Carry').
fmt_instr_title(ri(1), subicy, 'Subtract Immediate with Carry').
fmt_instr_title(ri(1), lsr, 'Logical Shift Right').
fmt_instr_title(ri(1), lsl, 'Logical Shift Left').
fmt_instr_title(ri(1), asr, 'Arithmetic Shift Right').
fmt_instr_title(ri(1), tbitm, 'Test Bit in Memory').
fmt_instr_title(ri(1), cbitm, 'Clear Bit in Memory').
fmt_instr_title(ri(1), sbitm, 'Set Bit in Memory').

fmt_instr_title(rr(1), add, 'Add').
fmt_instr_title(rr(1), sub, 'Subtract').
fmt_instr_title(rr(1), and, 'AND').
fmt_instr_title(rr(1), or, 'OR').
fmt_instr_title(rr(1), xor, 'XOR').
fmt_instr_title(rr(1), mov, 'Move').
fmt_instr_title(rr(1), addcy, 'Add with Carry').
fmt_instr_title(rr(1), subcy, 'Subtract with Carry').
fmt_instr_title(rr(2), tl, 'Test Less-than').
fmt_instr_title(rr(2), tge, 'Test Greater-than or Equal').
fmt_instr_title(rr(2), tb, 'Test Below').
fmt_instr_title(rr(2), tae, 'Test Above or Equal').
fmt_instr_title(rr(3), tne, 'Test Not Equal').
fmt_instr_title(rr(3), teq, 'Test Equal').

fmt_instr_title(rrr, mulstep, 'Multiplication Step').

fmt_instr_title(r(1), pushb, 'Push Byte').
fmt_instr_title(r(1), pushw, 'Push Word').
fmt_instr_title(r(1), popb, 'Pop Byte').
fmt_instr_title(r(1), popw, 'Pop Word').
fmt_instr_title(r(1), callr, 'Call Register').
fmt_instr_title(r(1), jr, 'Jump Register').
fmt_instr_title(r(1), neg, 'Negate').
fmt_instr_title(r(1), seb, 'Sign Extend Byte').
fmt_instr_title(r(2), 'rd.mp.lo', 'Read $MP.lo').
fmt_instr_title(r(2), 'rd.mp.hi', 'Read $MP.hi').
fmt_instr_title(r(2), 'rd.gp', 'Read $GP').
fmt_instr_title(r(2), 'wr.gp', 'Write $GP').

fmt_instr_title(o, 'NONEXE0', 'Non-executable (0''s Version)').
fmt_instr_title(o, 'UNIMPL', 'Unimplemented').
fmt_instr_title(o, 'HALT', 'Halt').
fmt_instr_title(o, 'BREAK', 'Breakpoint').
fmt_instr_title(o, kret, 'Kernel Return').
fmt_instr_title(o, kcall, 'Kernel Call').
fmt_instr_title(o, ret, 'Return').
fmt_instr_title(o, tov, 'Test Overflow').
fmt_instr_title(o, tcy, 'Test Carry').
fmt_instr_title(o, 'clr.cy', 'Clear Carry').
fmt_instr_title(o, 'set.cy', 'Set Carry').
fmt_instr_title(o, tpush0, 'Teststack Push 0').
fmt_instr_title(o, tpush1, 'Teststack Push 1').
fmt_instr_title(o, tnot, 'Teststack NOT').
fmt_instr_title(o, tand, 'Teststack AND').
fmt_instr_title(o, tor, 'Teststack OR').
fmt_instr_title(o, tdup, 'Teststack Duplicate').
fmt_instr_title(o, 'prsv.mp', 'Preserve $MP').
fmt_instr_title(o, 'rstr.mp', 'Restore $MP').
fmt_instr_title(o, 'prsv.ts', 'Preserve $TS').
fmt_instr_title(o, 'rstr.ts', 'Restore $TS').
fmt_instr_title(o, 'prsv.ra', 'Preserve $RA').
fmt_instr_title(o, 'rstr.ra', 'Restore $RA').
fmt_instr_title(o, 'prsv.gp', 'Preserve $GP').
fmt_instr_title(o, 'rstr.gp', 'Restore $GP').
fmt_instr_title(o, 'prsv.cc', 'Preserve $CC').
fmt_instr_title(o, 'rstr.cc', 'Restore $CC').
fmt_instr_title(o, sleep, 'Sleep').
fmt_instr_title(o, vijt, 'Valid Indirect Jump Target').

%%%%%%%%%%%%%%%%%%%%%%%%% Synthetic Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

synthinstr_descr_expansion_reversability(clr(r),      'Clear a register',               xor(r, r),   reversible).
synthinstr_descr_expansion_reversability(nop,         'The no-op instruction',          ori('$sp', 0),  reversible).
synthinstr_descr_expansion_reversability(incr(r),     'Increment a register',           addi(r, 1),  reversible).
synthinstr_descr_expansion_reversability(decr(r),     'Increment a register',           subi(r, 1),  reversible).
synthinstr_descr_expansion_reversability(inv(r),      'Bitwise inversion (complement)', xori(r, -1), reversible).
synthinstr_descr_expansion_reversability(not(r),      'Invert a boolean (0 or 1)',      xori(r, 1),  reversible).
synthinstr_descr_expansion_reversability(tg(r1, r2),  'Test greater-than',              tl(r2, r1),  one_way).
synthinstr_descr_expansion_reversability(tle(r1, r2), 'Test Less-than or Equal',        tge(r2, r1), one_way).
synthinstr_descr_expansion_reversability(ta(r1, r2),  'Test Above',                     ta(r2, r1),  one_way).
synthinstr_descr_expansion_reversability(tbe(r1, r2), 'Test Below or Equal',            tae(r2, r1), one_way).

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

conditioncode_info(cy, info{
    title: 'Carry',
    descr: 'After an `addcy` or `subcy` instruction, this flag is set if there was an arithmetic carry-out.'
}).
conditioncode_info(jt, info{
    title: 'Jump Target Validation',
    descr: 'When enabled, processor raises exception if an indirect jump does not land on a `vijt` instruction.'
}).

%%%%%%%%%%%%%%%%%%%%%%% Instruction Details %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addrsize_maxalignment(Bits, MaxAlign) :-
    instr_size(InstrSizeBits),
    2 ^ #Bits #>= (2 ^ #InstrSizeBits) div #MaxAlign.

instr(Instr) :- fmt_instr_title(_, Instr, _).
fmt(Fmt) :-
    fmt_huffman_enc(Tree),
    tree_leaf(Tree, Fmt).

tree_leaf([A | B], X) :- !, ( tree_leaf(A, X) ; tree_leaf(B, X) ).
tree_leaf(Atom, Atom).

gfmt(GFmt) :-
    solution_sequences:reduced(
        GFmt,
        isa:fmt_genericfmt(_Fmt, GFmt),
        [size_limit(32)]
    ).

gprreg(R) :- regname_uses(R, _).
sysreg(Name) :- sysregname_name_size_description(Name, _, _, _).

fmt_instr(Fmt, Instr) :- fmt_instr_title(Fmt, Instr, _).

fmt_genericfmt(Fmt, GFmt) :-
    fmt(Fmt),
    Fmt =.. [Functor | Args],
    same_length(Args, FreeArgs),
    GFmt =.. [Functor | FreeArgs].