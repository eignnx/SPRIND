:- module(sem, [
    instr_info/2,
    valid_semantics//1,
    def/2,
	emit_semantics_codeblock/1,
	all_modules/1,
	module_instrs/2,
	module_info/2
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(isa).
:- use_module(library(clpfd)).
:- use_module(library(aggregate)).

:- op(20, fx, #).
:- op(20, fx, $$).
:- op(20, fx, ?).
:- op(200, fx, ~).
:- op(1050, xfx, <-).
:- op(600, yfx, xor).
:- op(600, yfx, and).
:- op(600, yfx, or).
:- op(600, yfx, <<).
:- op(600, yfx, >>).
:- op(150, yfx, \).


valid_semantics(Stmt) --> stmt(Stmt).

rval_(LVal) --> lval(LVal).
rval_(#Term) --> % Constant or Immediate
    ( { atom(Term) } -> [constant(#Term)] % Specification-time named constant
    ; { integer(Term) } -> [] % Numeric Literal
    ).
rval_(~(A)) --> rval(A).
rval_(A + B) --> rval(A), rval(B).
rval_(A - B) --> rval(A), rval(B).
rval_(A << B) --> rval(A), rval(B).
rval_(A >> B) --> rval(A), rval(B).
rval_(A == B) --> rval(A), rval(B).
rval_(A \= B) --> rval(A), rval(B).
rval_(A and B) --> rval(A), rval(B).
rval_(A or B) --> rval(A), rval(B).
rval_(A xor B) --> rval(A), rval(B).
rval_(!(A)) --> rval(A).
rval_(~(A)) --> rval(A).
rval_(b_pop(LVal)) --> lval(LVal).
rval_(zxt(RVal)) --> rval(RVal).
rval_(sxt(RVal)) --> rval(RVal).
rval_(compare(A, RelOp, B)) --> { relop(RelOp) }, rval(A), rval(B).
rval_(RVal\Coersion) --> rval(RVal), { member(Coersion, [u, s, i]) ; integer(Coersion) }.

relop(<(IntTy)) :- int_ty(IntTy).
relop(>(IntTy)) :- int_ty(IntTy).
relop(<=(IntTy)) :- int_ty(IntTy).
relop(>=(IntTy)) :- int_ty(IntTy).

int_ty(u\N) :- N in 1 .. sup.
int_ty(s\N) :- N in 1 .. sup.
int_ty(i\N) :- N in 1 .. sup.

rval(RVal) --> rval_(RVal) -> [] ; [error(invalid_rval(RVal))].

lval([Term]) --> rval(Term).
lval($Reg) --> { isa:gprreg(Reg) }.
lval($$Reg) --> { isa:sysreg(Reg) }.
lval(?Var) --> { atom(Var) }.
lval(attr(Path)) --> path(Path), [constant(attr(Path))].
lval(hi(RVal)) --> rval(RVal).
lval(lo(RVal)) --> rval(RVal).
lval(hi_lo(A, B)) --> rval(A), rval(B).
lval(bitslice(LVal, Bound1 .. Bound2)) --> lval(LVal), rval(Bound1), rval(Bound2).
lval(bit(LVal, Index)) --> lval(LVal), rval(Index).
lval({ Elements }) --> { comma_list(Elements, EleList) }, all_lvals(EleList).

all_lvals([]) --> [].
all_lvals([X | Xs]) --> lval(X), all_lvals(Xs).

path(Atom) --> { atom(Atom) }.
path(Parent/Child) --> { atom(Child) }, path(Parent).

stmt_(todo) --> [].
stmt_(exception(E)) --> { isa:cpu_exception(E) }.
stmt_(b_push(LVal, RVal)) --> lval(LVal), rval(RVal).
stmt_(if(Cond, Consq)) --> rval(Cond), stmt(Consq).
stmt_(if(Cond, Consq, Alt)) --> rval(Cond), stmt(Consq), stmt(Alt).
stmt_(Dst <- Src) --> lval(Dst), rval(Src).
stmt_(S1 ; S2) -->
    { S1 = (?Var = RVal) } ->
        { atom(Var) }, rval(RVal), stmt(S2)
    ;
        stmt(S1), stmt(S2).

stmt(Stmt) --> stmt_(Stmt) -> [] ; [error(invalid_stmt(Stmt))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def(attr(cpu/alu/carryout), signal).
def(attr(cpu/alu/overflow), signal).
def(#carry_flag_bit, _).
def(#overflow_flag_bit, _).
def(#jmp_tgt_validation_req_flag_bit, _).
def(#jmp_tgt_validation_en_flag_bit, _).
def(#subr_align, _).
def(#reg_size_bits, Size) :- isa:register_size(Size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(print_write_options, [
    portrayed(true),
    spacing(next_argument)
]).

user:portray(Dst <- Src) :- print(Dst), format(' <- '), print(Src).
user:portray(Dst = Src) :- format('let '), print(Dst), format(' := '), print(Src).
% user:portray(A\B) :- print(A), format('\\'), print(B).
% user:portray(A + B) :- print(A), format(' + '), print(B).
% user:portray(A - B) :- print(A), format(' - '), print(B).
% user:portray(A == B) :- print(A), format(' == '), print(B).
% user:portray(A \= B) :- print(A), format(' != '), print(B).
% user:portray(A >> B) :- print(A), format(' >> '), print(B).
% user:portray(A << B) :- print(A), format(' << '), print(B).
% user:portray(A and B) :- write_term(A and B, [])
% user:portray(A or B) :- print(A), format(' or '), print(B).
% user:portray(A xor B) :- print(A), format(' xor '), print(B).
user:portray(S1 ; S2) :- print(S1), format(';~n'), print(S2).
user:portray(S1 .. S2) :- print(S1), format('..'), print(S2).
user:portray(#X) :- print(X).
user:portray($X) :- format('$'), print(X).
user:portray($$X) :- upcase_atom(X, XUpper), format('$~w', [XUpper]).
user:portray(?X) :- print(X).
user:portray(if(Cond, Consq)) :-
    format('if ~p {~n', [Cond]),
    format(codes(ConsqCodes), '~p', [Consq]),
    indent_lines('    ', ConsqCodes, ConsqIndented),
    format('~w~n', [ConsqIndented]),
    format('}').

user:portray(if(Cond, Consq, Alt)) :-
    format('if ~p {~n', [Cond]),
    format(codes(ConsqCodes), '~p', [Consq]),
    indent_lines('    ', ConsqCodes, ConsqIndented),
    format('~w~n', [ConsqIndented]),
    format('} else {~n'),
    format(codes(AltCodes), '~p', [Alt]),
    indent_lines('    ', AltCodes, AltIndented),
    format('~w~n', [AltIndented]),
    format('}').

emit_semantics_codeblock(Info) :-
	op(200, fx, ~),
	op(1050, xfx, <-),
	op(600, yfx, xor),
	op(600, yfx, and),
	op(600, yfx, or),
	op(600, yfx, <<),
	op(600, yfx, >>),
	op(150, yfx, \),

    format(codes(OperandsCodes), '~p', [Info.operands]),
    length(OperandsCodes, OLen),
    format(codes(SemanticsCodes), '~p', [Info.sem]),
    string_lines(SemanticsCodes, SLines),
    maplist(string_length, SLines, SLens),
    max_member(MaxLen, [OLen | SLens]),
    format('```~n'),
    format('~s~n', [OperandsCodes]),
    format('~`-t~*|~n', [MaxLen]),
    format('~s~n', [SemanticsCodes]),
    format('```~n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


instr_info(lb, info{
	title: 'Load Byte',
	descr: 'Load a byte from memory into a register.',
    ex: ['lb w, [sp+12]'],
    operands: [simm(?simm), reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
	sem: (
        ?ptr = (?rs\s + sxt(?simm))\u;
        ?rd <- zxt([?ptr])
    ),
    tags: [mem, load, byte],
	module: [base]
}).
instr_info(lw, info{
	title: 'Load Word',
	descr: 'Load a word from memory into a register.',
	ex: ['lw w, [sp+12]'],
    operands: [simm(?simm), reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
	sem: (
        ?ptr = ((?rs\s + sxt(?simm)) and #(-2)\16)\u;
        ?rd <- {[?ptr + #1], [?ptr]}
    ),
    tags: [mem, load, word],
	module: [base]
}).
instr_info(sb, info{
	title: 'Store Byte',
	descr: 'Store a byte from a register into memory.',
	ex: ['sb [sp-20], x'],
    operands: [simm(?simm), reg(r, ?rd), reg(s, ?rs)],
	syntax: { [reg(r, ?rd) + simm(?simm)], reg(s, ?rs) },
	sem: (
        ?ptr = ?rd\s + sxt(?simm);
        [?ptr\u] <- lo(?rs)
    ),
    tags: [mem, store, byte],
	module: [base]
}).
instr_info(sw, info{
	title: 'Store Word',
	descr: 'Store a word from a register into memory.',
	ex: ['sw [sp-20], x'],
    operands: [simm(?simm), reg(r, ?rd), reg(s, ?rs)],
	syntax: { [reg(r, ?rd) + simm(?simm)], reg(s, ?rs) },
	sem: (
        ?ptr = ((?rd\s + sxt(?simm)) and #0b1111111111111110)\u;
        [?ptr] <- lo(?rs);
        [?ptr + #1] <- hi(?rs)
    ),
    tags: [mem, store, word],
	module: [base]
}).

instr_info(call, info{
	title: 'Call Subroutine',
	descr: 'Call a subroutine at the specified address.',
	ex: ['call SOME_LABEL'],
	operands: [simm(?simm)],
	syntax: { simm(?abs_lbl) } -> (
		?rel_lbl = ?abs_lbl - #asm_pc;
		{ ?rel_lbl }
	),
	sem: (
        $$pc <- $$pc\s + (sxt(?simm) << #subr_align);
        $$ra <- $$pc + #2
    ),
    tags: [pc, ra],
	module: [base]
}).

instr_info(b, info{
	title: 'Branch',
	descr: 'Branch to the specified address by adding the immediate offset to `$PC`.',
	ex: ['b SOME_LABEL'],
	operands: [simm(?offset)],
	syntax: { simm(?abs_lbl) } -> (
		?rel_lbl = ?abs_lbl - #asm_pc;
		{ ?rel_lbl }
	),
	sem: $$pc <- $$pc\s + sxt(?offset),
    tags: [pc],
	module: [base]
}).
instr_info(bt, info{
	title: 'Branch If True',
	descr: 'Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.',
	ex: ['bt SOME_LABEL'],
	operands: [simm(?offset)],
	syntax: { simm(?abs_lbl) } -> (
		?rel_lbl = ?abs_lbl - #asm_pc;
		{ ?rel_lbl }
	),
	sem: if(b_pop($$ts),
        $$pc <- $$pc\s + sxt(?offset)
    ),
    tags: [pc, cond],
	module: [base]
}).
instr_info(bf, info{
	title: 'Branch If False',
	descr: 'Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.',
	ex: ['bf SOME_LABEL'],
	operands: [simm(?offset)],
	syntax: { simm(?abs_lbl) } -> (
		?rel_lbl = ?abs_lbl - #asm_pc;
		{ ?rel_lbl }
	),
	sem: if(!(b_pop($$ts)),
        $$pc <- $$pc\s + sxt(?offset)
    ),
    tags: [pc, cond],
	module: [base]
}).

instr_info(li, info{
	title: 'Load Immediate',
	descr: 'Load an immediate value into a register.',
	ex: ['li x, 123'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: ?rd <- sxt(?simm),
    tags: [sxt, data],
	module: [base]
}).
instr_info(szi, info{
	title: 'Shift Zero-extended Immediate',
	descr: 'Left-shift a zero-extended immediate value into a register.',
	ex: ['szi x, 0xB3'],
	operands: [imm(?imm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), imm(?imm) },
	sem: ?rd <- (?rd << #8) or zxt(?imm),
    tags: [zxt, data, shift],
	module: [base]
}).

instr_info(lgb, info{
	title: 'Load Global Byte',
	descr: 'Load a byte from a memory address offset from `$GP`.',
	ex: ['lgb x, [gp+8]'],
	operands: [imm(?disp), reg(r, ?rd)],
	syntax: { reg(r, ?rd), [reg(s, ?rs) + imm(?disp)] },
	sem: ?rd <- zxt([$$gp\u + zxt(?disp)]),
    tags: [mem, load, global, byte],
	module: [globals]
}).
instr_info(lgw, info{
	title: 'Load Global Word',
	descr: 'Load a word from a memory address offset from `$GP`.',
	ex: ['lgw x, [gp+8]'],
	operands: [imm(?disp), reg(r, ?rd)],
	syntax: { reg(r, ?rd), [reg(s, ?rs) + imm(?disp)] },
	sem: (
        ?ptr = (($$gp\u + zxt(?disp)) and #0b1111111111111110)\u;
        ?rd <- {[?ptr + #1], [?ptr]}
    ),
    tags: [mem, load, global, word],
	module: [globals]
}).
instr_info(sgb, info{
	title: 'Store Global Byte',
	descr: 'Store a byte into memory address offset from `$GP`.',
	ex: ['sgb [gp+8], x'],
	operands: [imm(?disp), reg(r, ?rs)],
	syntax: { [reg(r, ?rd) + imm(?disp)], reg(s, ?rs) },
	sem: [$$gp\u + zxt(?disp)] <- lo(?rs),
    tags: [mem, store, global, byte],
	module: [globals]
}).
instr_info(sgw, info{
	title: 'Store Global Word',
	descr: 'Store a word into memory address offset from `$GP`.',
	ex: ['sgw [gp+8], x'],
	operands: [imm(?disp), reg(r, ?rs)],
	syntax: { [reg(r, ?rd) + imm(?disp)], reg(s, ?rs) },
	sem: (
        ?ptr = (($$gp\u + zxt(?disp)) and #0b1111111111111110)\u;
        {[?ptr + #1], [?ptr]} <- ?rs
    ),
    tags: [mem, store, global, word],
	module: [globals]
}).
instr_info(tbit, info{
	title: 'Test Bit',
	descr: 'Test a specific bit in a register, modifying `$TS`.',
	ex: ['tbit 12, w'],
	operands: [imm(?bit_idx), reg(r, ?rs)],
	syntax: { imm(?bit_idx), reg(r, ?rs) },
	sem: (
        ?shamt = bitslice(?bit_idx, #3 .. #0);
        ?bit = (?rs >> ?shamt\u) and #1;
        b_push($$ts, ?bit == #1)
    ),
    tags: [ts, bit, bitwise],
	module: [bittests]
}).
instr_info(cbit, info{
	title: 'Clear Bit',
	descr: 'Clear a specific bit in a register.',
	ex: ['cbit 9, v'],
	operands: [imm(?bit_idx), reg(r, ?rd)],
	syntax: { imm(?bit_idx), reg(r, ?rd) },
	sem: (
        ?idx = bitslice(?bit_idx, #3 .. #0)\u;
        ?mask = ~(#1 << ?idx);
        ?rd <- ?rd and ?mask
    ),
    tags: [bit, bitwise, clear],
	module: [bittests]
}).
instr_info(sbit, info{
	title: 'Set Bit',
	descr: 'Set a specific bit in a register.',
	ex: ['sbit 15, a'],
	operands: [imm(?bit_idx), reg(r, ?rd)],
	syntax: { imm(?bit_idx), reg(r, ?rd) },
	sem: (
        ?idx = bitslice(?bit_idx, #3 .. #0)\u;
        ?mask = ~(#1 << ?idx);
        ?rd <- ?rd or ?mask
    ),
    tags: [bit, bitwise, set],
	module: [bittests]
}).
instr_info(tli, info{
	title: 'Test Less-than Immediate',
	descr: 'Test if a register value is less than an immediate value.',
	ex: ['tli x, -5'],
	operands: [simm(?simm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), simm(?simm) },
	sem: b_push($$ts, compare(?rs\s, <(s\16), sxt(?simm))),
    tags: [ts, cmp, inequality, signed, '<'],
	module: [imms]
}).
instr_info(tgei, info{
	title: 'Test Greater-than or Equal Immediate',
	descr: 'Test if a register value is greater than or equal to an immediate value.',
	ex: ['tgei x, -5'],
	operands: [simm(?simm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), simm(?simm) },
	sem: b_push($$ts, compare(?rs\s, >=(s\16), sxt(?simm))),
    tags: [ts, cmp, inequality, signed, '>='],
	module: [imms]
}).
instr_info(tbi, info{
	title: 'Test Below Immediate',
	descr: 'Test if a register value is below an immediate value.',
	ex: ['tbi x, 10'],
	operands: [imm(?imm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), imm(?imm) },
	sem: b_push($$ts, compare(?rs\u, <(u\16), zxt(?imm))),
    tags: [ts, cmp, inequality, unsigned, '<'],
	module: [imms]
}).
instr_info(taei, info{
	title: 'Test Above or Equal',
	descr: 'Test if a register value is above or equal to an immediate value.',
	ex: ['taei x, 10'],
	operands: [imm(?imm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), imm(?imm) },
	sem: b_push($$ts, compare(?rs\u, >=(u\16), zxt(?imm))),
    tags: [ts, cmp, inequality, unsigned, '>='],
	module: [imms]
}).
instr_info(tnei, info{
	title: 'Test Not Equal Immediate',
	descr: 'Test if a register value is not equal to an immediate value.',
	ex: ['tnei x, 0'],
	operands: [simm(?simm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), simm(?simm) },
	sem: b_push($$ts, ?rs\s \= sxt(?simm)),
    tags: [ts, cmp, equality, not],
	module: [imms]
}).
instr_info(teqi, info{
	title: 'Test Equal Immediate',
	descr: 'Test if a register value is equal to an immediate value.',
	ex: ['teqi x, -5'],
	operands: [simm(?simm), reg(r, ?rs)],
	syntax: { reg(r, ?rs), simm(?simm) },
	sem: b_push($$ts, ?rs\s == sxt(?simm)),
    tags: [ts, cmp, equality],
	module: [imms]
}).
instr_info(addi, info{
	title: 'Add Immediate',
	descr: 'Add an immediate value to a register.',
	ex: ['addi x, -5'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: ?rd <- ?rd\s + sxt(?simm),
    tags: [arith],
	module: [imms]
}).
instr_info(andi, info{
	title: 'AND Immediate',
	descr: 'Perform a bitwise AND between a register and an immediate value.',
	ex: ['andi x, 3'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: ?rd <- ?rd and sxt(?simm),
    tags: [logical, boolean, bitwise, and],
	module: [imms]
}).
instr_info(ori, info{
	title: 'OR Immediate',
	descr: 'Perform a bitwise OR between a register and an immediate value.',
	ex: ['ori x, 3'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: ?rd <- ?rd or sxt(?simm),
    tags: [logical, boolean, bitwise],
	module: [imms]
}).

instr_info(xori, info{
	title: 'XOR Immediate',
	descr: 'Perform a bitwise XOR between a register and an immediate value.',
	ex: ['xori x, 3'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: ?rd <- ?rd xor sxt(?simm),
    tags: [logical, boolean, bitwise, xor],
	module: [imms]
}).
instr_info(addicy, info{
	title: 'Add Immediate with Carry',
	descr: 'Add an immediate value and the carry bit to a register.',
	ex: ['addicy x, 3'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: (
        ?rd <- ?rd\s + sxt(?simm) + bit($$cc, #carry_flag_bit)\16\s;
        bit($$cc, #carry_flag_bit) <- attr(cpu/alu/carryout);
        bit($$cc, #overflow_flag_bit) <- attr(cpu/alu/overflow)
    ),
    tags: [arith, carry, add],
	module: [imms]
}).
instr_info(subicy, info{
	title: 'Subtract Immediate with Carry',
	descr: 'Sutract an immediate value and the carry bit from a register.',
	ex: ['subicy x, 3'],
	operands: [simm(?simm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), simm(?simm) },
	sem: (
        ?rd <- ?rd\s - sxt(?simm) - bit($$cc, #carry_flag_bit)\16\s;
        bit($$cc, #carry_flag_bit) <- attr(cpu/alu/carryout);
        bit($$cc, #overflow_flag_bit) <- attr(cpu/alu/overflow)
    ),
    tags: [arith, carry],
	module: [imms]
}).
instr_info(lsr, info{
	title: 'Logical Shift Right',
	descr: 'Perform a logical shift right on a register by an immediate value.',
	ex: ['lsr x, 15'],
	operands: [imm(?imm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), imm(?imm) },
	sem: ?rd <- ?rd >> ?imm,
    tags: [bitwise, shift, right],
	module: [base]
}).
instr_info(lsl, info{
	title: 'Logical Shift Left',
	descr: 'Perform a logical shift left on a register by an immediate value.',
	ex: ['lsl x, 8'],
	operands: [imm(?imm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), imm(?imm) },
	sem: ?rd <- ?rd << ?imm,
    tags: [zxt, bitwise, shift, left],
	module: [base]
}).
instr_info(asr, info{
	title: 'Arithmetic Shift Right',
	descr: 'Perform an arithmetic shift right on a register by an immediate value.',
	ex: ['asr x, 3'],
	operands: [imm(?imm), reg(r, ?rd)],
	syntax: { reg(r, ?rd), imm(?imm) },
	sem: (
        ?sign_extension = (sxt(bit(?rd, #15) - #1)) << (#reg_size_bits - ?imm);
        ?rd <- (?rd >> ?imm) or ?sign_extension 
    ),
    tags: [sxt, bitwise, shift, right],
	module: [base]
}).
instr_info(tbitm, info{
	title: 'Test Bit in Memory',
	descr: '',
	ex: ['tbitm [x], 3'],
	operands: [imm(?imm), reg(r, ?rs)],
	syntax: { [reg(r, ?rs)], imm(?imm) },
	sem: b_push($$ts, bit([?rs], ?imm)),
    tags: [ts, bit, bitwise, mem],
	module: [bittests]
}).
instr_info(cbitm, info{
	title: 'Clear Bit in Memory',
	descr: '',
	ex: ['cbitm [x], 3'],
	operands: [imm(?imm), reg(r, ?rs)],
	syntax: { [reg(r, ?rs)], imm(?imm) },
	sem: (
		[?rs] <- [?rs] and ~(#1 << ?imm)
    ),
    tags: [ts, bit, bitwise, clear, mem],
	module: [bittests]
}).
instr_info(sbitm, info{
	title: 'Set Bit in Memory',
	descr: '',
	ex: ['sbitm [x], 3'],
	operands: [imm(?imm), reg(r, ?rs)],
	syntax: { [reg(r, ?rs)], imm(?imm) },
	sem: (
		[?rs] <- [?rs] or (#1 << ?imm)
    ),
    tags: [ts, bit, bitwise, set, mem],
	module: [bittests]
}).

instr_info(add, info{
	title: 'Add',
	descr: 'Add the values of two registers.',
	ex: ['add x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rd + ?rs,
    tags: [arith, add],
	module: [base]
}).
instr_info(sub, info{
	title: 'Subtract',
	descr: 'Subtract the value of one register from another.',
	ex: ['sub x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rd - ?rs,
    tags: [arith],
	module: [base]
}).
instr_info(and, info{
	title: 'AND',
	descr: 'Perform a bitwise AND between two registers.',
	ex: ['and x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rd and ?rs,
    tags: [logical, boolean, bitwise, and],
	module: [base]
}).
instr_info(or, info{
	title: 'OR',
	descr: 'Perform a bitwise OR between two registers.',
	ex: ['or x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rd or ?rs,
    tags: [logical, boolean, bitwise],
	module: [base]
}).
instr_info(xor, info{
	title: 'XOR',
	descr: 'Perform a bitwise XOR between two registers.',
	ex: ['xor x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rd xor ?rs,
    tags: [logical, boolean, bitwise, xor],
	module: [base]
}).
instr_info(mov, info{
	title: 'Move',
	descr: 'Move the value from one register to another.',
	ex: ['mov x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: ?rd <- ?rs,
    tags: [data],
	module: [base]
}).
instr_info(addcy, info{
	title: 'Add with Carry',
	descr: 'Add the values of two registers with carry.',
	ex: ['addcy x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: (
        ?rd <- ?rd + ?rs + bit($$cc, #carry_flag_bit)\16;
        bit($$cc, #carry_flag_bit) <- attr(cpu/alu/carryout);
        bit($$cc, #overflow_flag_bit) <- attr(cpu/alu/overflow)
    ),
    tags: [arith, carry, add],
	module: [base]
}).
instr_info(subcy, info{
	title: 'Subtract with Carry',
	descr: 'Subtract the value of one register from another with carry.',
	ex: ['subcy x, y'],
	operands: [reg(r, ?rs), reg(s, ?rd)],
	syntax: { reg(r, ?rd), reg(s, ?rs) },
	sem: (
        ?rd <- ?rd - ?rs - bit($$cc, #carry_flag_bit)\16;
        bit($$cc, #carry_flag_bit) <- attr(cpu/alu/carryout);
        bit($$cc, #overflow_flag_bit) <- attr(cpu/alu/overflow)
    ),
    tags: [arith, carry],
	module: [base]
}).
instr_info(tl, info{
	title: 'Test Less-than',
	descr: 'Test if the value of one register is less than another.',
	ex: ['tl x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, compare(?r1, <(s\16), ?r2)),
    tags: [ts, cmp, inequality, signed, '<'],
	module: [base]
}).
instr_info(tge, info{
	title: 'Test Greater-than or Equal',
	descr: 'Test if the value of one register is greater than or equal to another.',
	ex: ['tge x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, compare(?r1, >=(s\16), ?r2)),
    tags: [ts, cmp, inequality, signed, '>='],
	module: [base]
}).
instr_info(tb, info{
	title: 'Test Below',
	descr: 'Test if the value of one register is below another.',
	ex: ['tb x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, compare(?r1, <(u\16), ?r2)),
    tags: [ts, cmp, inequality, unsigned, '<'],
	module: [base]
}).
instr_info(tae, info{
	title: 'Test Above or Equal',
	descr: 'Test if the value of one register is above or equal to another.',
	ex: ['tae x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, compare(?r1, >=(u\16), ?r2)),
    tags: [ts, cmp, inequality, unsigned, '>='],
	module: [base]
}).
instr_info(tne, info{
	title: 'Test Not Equal',
	descr: 'Test if the value of one register is not equal to another.',
	ex: ['tne x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, ?r1 \= ?r2),
    tags: [ts, cmp, equality, not],
	module: [base]
}).
instr_info(teq, info{
	title: 'Test Equal',
	descr: 'Test if the value of one register is equal to another.',
	ex: ['teq x, y'],
	operands: [reg(r, ?r1), reg(s, ?r2)],
	syntax: { reg(r, ?r1), reg(s, ?r2) },
	sem: b_push($$ts, ?r1 == ?r2),
    tags: [ts, cmp, equality],
	module: [base]
}).

instr_info(mulstep, info{
	title: 'Unsigned Multiplication Step',
	descr: 'Computes one step in a full 16-bit by 16-bit unsigned multiplication.',
	ex: ['mulstep x:y, z'],
	operands: [reg(r, ?multiplicand_hi), reg(s, ?multiplicand_lo), reg(t, ?multiplier)],
	syntax: { reg(t, ?multiplicand_hi):reg(s, ?multiplicand_lo), reg(r, ?multiplier) },
	sem: (
        ?mask = ~((?multiplier and #1) - #1);
        ?masked_lo = ?multiplicand_lo and ?mask;
        ?masked_hi = ?multiplicand_hi and ?mask;
        lo($$mp) <- lo($$mp) + ?masked_lo;
        hi($$mp) <- hi($$mp) + ?masked_hi + attr(cpu/alu/carryout);
        ?shift_cout = bit(?multiplicand_lo, (#reg_size_bits - #1));
        ?multiplicand_lo <- ?multiplicand_lo << #1;
        ?multiplicand_hi <- ?multiplicand_hi << #1 + ?shift_cout;
        ?multiplier <- ?multiplier >> #1
    ),
    tags: [arith, shift],
	module: [mul]
}).

instr_info(pushb, info{
	title: 'Push Byte',
	descr: 'Push a byte from a register onto the stack.',
	ex: ['pushb x'],
	operands: [reg(r, ?rs)],
	syntax: { reg(r, ?rs) },
	sem: todo,
    tags: [sp, push, byte],
	module: [stack]
}).
instr_info(pushw, info{
	title: 'Push Word',
	descr: 'Push a word from a register onto the stack.',
	ex: ['pushw x'],
	operands: [reg(r, ?rs)],
	syntax: { reg(r, ?rs) },
	sem: todo,
    tags: [sp, push, word],
	module: [stack]
}).
instr_info(popb, info{
	title: 'Pop Byte',
	descr: 'Pop a byte from the stack into a register.',
	ex: ['popb x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: todo,
    tags: [sp, pop, byte],
	module: [stack]
}).
instr_info(popw, info{
	title: 'Pop Word',
	descr: 'Pop a word from the stack into a register.',
	ex: ['popw x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: todo,
    tags: [sp, pop, word],
	module: [stack]
}).
instr_info(callr, info{
	title: 'Call Register',
	descr: 'Call a subroutine at the address in a register.',
	ex: ['callr x'],
	operands: [reg(r, ?abs_lbl)],
	syntax: { reg(r, ?abs_lbl) },
	sem: (
		$$pc <- ?abs_lbl;
		$$ra <- $$pc + #2
	),
    tags: [pc, ra, jump, indirect],
	module: [base]
}).
instr_info(jr, info{
	title: 'Jump Register',
	descr: 'Jump to the address in a register.',
	ex: ['jr x'],
	operands: [reg(r, ?abs_lbl)],
	syntax: { reg(r, ?abs_lbl) },
	sem: $$pc <- ?abs_lbl,
    tags: [pc, jump, indirect],
	module: [base]
}).
instr_info(neg, info{
	title: 'Negate',
	descr: 'Negate the value in a register.',
	ex: ['neg x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: ?rd <- -(?rd),
    tags: [arith],
	module: [imms]
}).
instr_info(seb, info{
	title: 'Sign Extend Byte',
	descr: 'Sign extend a byte in a register.',
	ex: ['seb x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: ?rd <- sxt(?rd\8),
    tags: [arith, sxt],
	module: [base]
}).
instr_info('rd.mp.lo', info{
	title: 'Read $MP.lo',
	descr: 'Read the low word in the system `$MP` register into a general purpose register.',
	ex: ['rd.mp.lo x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: ?rd <- lo($$mp),
    tags: [rd, data, mp, lo],
	module: [mul]
}).
instr_info('rd.mp.hi', info{
	title: 'Read $MP.hi',
	descr: 'Read the high word in the system `$MP` register into a general purpose register.',
	ex: ['rd.mp.hi x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: ?rd <- hi($$mp),
    tags: [rd, data, mp, hi],
	module: [mul]
}).
instr_info('rd.gp', info{
	title: 'Read $GP',
	descr: 'Read the value of the system `$GP` register into a general purpose register.',
	ex: ['rd.gp x'],
	operands: [reg(r, ?rd)],
	syntax: { reg(r, ?rd) },
	sem: ?rd <- $$gp,
    tags: [rd, data, gp],
	module: [globals]
}).
instr_info('wr.gp', info{
	title: 'Write $GP',
	descr: 'Write a value to the system `$GP` register from a general purpose register.',
	ex: ['wr.gp x'],
	operands: [reg(r, ?rs)],
	syntax: { reg(r, ?rs) },
	sem: $$gp <- ?rs,
    tags: [wr, data, gp],
	module: [globals]
}).

instr_info('NONEXE0', info{
	title: 'Non-executable (0s Version)',
	descr: 'Triggers a "non-executable instruction" exception. The entire instruction is 16 `0`s.',
	ex: ['NONEXE0'],
	operands: [],
	syntax: {},
	sem: $$pc <- #nonexe0_isr,
    tags: [], % Giving this no tags ensures it gets sorted into the 0-most branch of the optree.
	module: [base]
}).
instr_info('BREAK', info{
	title: 'Breakpoint',
	descr: 'Trigger a breakpoint.',
	ex: ['BREAK'],
	operands: [],
	syntax: {},
	sem: $$pc <- #break_isr,
    tags: [exc, dbg],
	module: [dbg]
}).
instr_info('HALT', info{
	title: 'Halt',
	descr: 'Halt the processor.',
	ex: ['HALT'],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [exc, cmd],
	module: [base]
}).
instr_info('UNIMPL', info{
	title: 'Unimplemented',
	descr: 'Unimplemented instruction.',
	ex: ['UNIMPL'],
	operands: [],
	syntax: {},
	sem: $$pc <- #unimpl_isr, 
    tags: [exc, dbg],
	module: [dbg]
}).
instr_info(kret, info{
	title: 'Kernel Return',
	descr: 'Return from kernel mode.',
	ex: ['kret'],
	operands: [],
	syntax: {},
	sem: $$pc <- $$kr,
    tags: [kernel, kr, pc, ret],
	module: [interrupts]
}).
instr_info(kcall, info{
	title: 'Kernel Call',
	descr: 'Call a kernel function. The function index must be stored in `v`.',
	ex: ['kcall'],
	operands: [],
	syntax: {},
	sem: (
		$$kr <- $$pc + #2;
		$$pc <- $v;
		todo
	),
    tags: [kernel, kr, pc, call],
	module: [interrupts]
}).
instr_info(ret, info{
	title: 'Return',
	descr: 'Return from a subroutine.',
	ex: ['ret'],
	operands: [],
	syntax: {},
	sem: $$pc <- $$ra,
    tags: [pc, ra, ret],
	module: [base]
}).
instr_info(tov, info{
	title: 'Test Overflow',
	descr: 'Test for overflow.',
	ex: ['tov'],
	operands: [],
	syntax: {},
	sem: b_push($$ts, bit($$cc, #overflow_flag_idx)),
    tags: [ts, cc, ov],
	module: [base]
}).
instr_info(tcy, info{
	title: 'Test Carry',
	descr: 'Test for carry.',
	ex: ['tcy'],
	operands: [],
	syntax: {},
	sem: b_push($$ts, bit($$cc, #carry_flag_idx)),
    tags: [ts, cc, cy],
	module: [base]
}).
instr_info('clr.cy', info{
	title: 'Clear Carry',
	descr: 'Clear the carry flag.',
	ex: ['clr.cy'],
	operands: [],
	syntax: {},
	sem: bit($$cc, #carry_flag_idx) <- #0,
    tags: [wr, cy],
	module: [base]
}).
instr_info('set.cy', info{
	title: 'Set Carry',
	descr: 'Set the carry flag.',
	ex: ['set.cy'],
	operands: [],
	syntax: {},
	sem: bit($$cc, #carry_flag_idx) <- #1,
    tags: [wr, cy],
	module: [base]
}).
instr_info(tpush0, info{
	title: 'Teststack Push 0',
	descr: 'Push 0 onto the test stack.',
	ex: ['tpush0'],
	operands: [],
	syntax: {},
	sem: b_push($$ts, #0),
    tags: [ts, push],
	module: [tsops]
}).
instr_info(tpush1, info{
	title: 'Teststack Push 1',
	descr: 'Push 1 onto the test stack.',
	ex: ['tpush1'],
	operands: [],
	syntax: {},
	sem: b_push($$ts, #1),
    tags: [ts, push],
	module: [tsops]
}).
instr_info(tnot, info{
	title: 'Teststack NOT',
	descr: 'Perform a NOT operation on the test stack.',
	ex: ['tnot'],
	operands: [],
	syntax: {},
	sem: b_push($$ts, ~(b_pop($$ts))),
    tags: [ts, boolean],
	module: [tsops]
}).
instr_info(tand, info{
	title: 'Teststack AND',
	descr: 'Perform an AND operation on the test stack.',
	ex: ['tand'],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [ts, boolean],
	module: [tsops]
}).
instr_info(tor, info{
	title: 'Teststack OR',
	descr: 'Perform an OR operation on the test stack.',
	ex: ['tor'],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [ts, boolean],
	module: [tsops]
}).
instr_info(tdup, info{
	title: 'Teststack Duplicate',
	descr: 'Duplicate the top value on the test stack.',
	ex: ['tdup'],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [ts, data],
	module: [tsops]
}).
instr_info('prsv.mp', info{
	title: 'Preserve $MP',
	descr: 'Preserve the value of the `$MP` register onto the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, prsv, mp],
	module: [mul]
}).
instr_info('rstr.mp', info{
	title: 'Restore $MP',
	descr: 'Restore the value of the `$MP` register from the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, rstr, mp],
	module: [mul]
}).
instr_info('prsv.ts', info{
	title: 'Preserve $TS',
	descr: 'Preserve the value of the `$TS` register onto the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, prsv, ts],
	module: [interrupts]
}).
instr_info('rstr.ts', info{
	title: 'Restore $TS',
	descr: 'Restore the value of the `$TS` register from the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, rstr, ts],
	module: [interrupts]
}).
instr_info('prsv.ra', info{
	title: 'Preserve $RA',
	descr: 'Preserve the value of the `$RA` register onto the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, prsv, ra],
	module: [interrupts]
}).
instr_info('rstr.ra', info{
	title: 'Restore $RA',
	descr: 'Restore the value of the `$RA` register from the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, rstr, ra],
	module: [interrupts]
}).
instr_info('prsv.gp', info{
	title: 'Preserve $GP',
	descr: 'Preserve the value of the `$GP` register onto the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, prsv, gp],
	module: [globals]
}).
instr_info('rstr.gp', info{
	title: 'Restore $GP',
	descr: 'Restore the value of the `$GP` register from the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, rstr, gp],
	module: [globals]
}).
instr_info('prsv.cc', info{
	title: 'Preserve $CC',
	descr: 'Preserve the value of the `$CC` register onto the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, prsv, cc],
	module: [interrupts]
}).
instr_info('rstr.cc', info{
	title: 'Restore $CC',
	descr: 'Restore the value of the `$CC` register from the stack.',
	ex: [],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [sp, prsv_rstr, rstr, cc],
	module: [interrupts]
}).
instr_info(sleep, info{
	title: 'Sleep',
	descr: 'Puts processor into low-power sleep mode.',
	ex: ['sleep'],
	operands: [],
	syntax: {},
	sem: todo,
    tags: [cc, sleep, interrupts],
	module: [interrupts]
}).
instr_info(vijt, info{
	title: 'Valid Indirect Jump Target',
	descr: 'When `$CC.jt` is `1`, the `callr` and `jr` instructions must jump to one of these instructions or an exception is raised.',
	ex: [],
	operands: [],
	syntax: {},
	sem: (
		if(bit($$cc, #jmp_tgt_validation_en_flag_bit),
			if(bit($$cc, #jmp_tgt_validation_req_flag_bit),
				bit($$cc, #jmp_tgt_validation_req_flag_bit) <- #0,
				exception('ILLINSTR')
			)
		)
	),
    tags: [pc, indirect, jump, security],
	module: [security]
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_modules(Modules) :-
	aggregate_all(set(M), (
		instr_info(_, Info),
		[M] = Info.module
	), Modules).

module_instrs(Module, Instrs) :-
	bagof(I, instr_module(I, Module), Instrs).

instr_module(Instr, Module) :-
	instr_info(Instr, Info),
	[Module] = Info.module.


module_info(base, info{
	title: 'Base',
	descr: 'The minimal set of features needed for general computation.',
	deps: []
}).

module_info(globals, info{
	title: 'Globals',
	descr: 'Instructions related to the `$GP` global variable pointer register and gloal variables.',
	deps: [base]
}).

module_info(bittests, info{
	title: 'Bit Tests',
	descr: 'Instructions related to testing specific bits of a register or memory.',
	deps: [base]
}).

module_info(dbg, info{
	title: 'Debugging',
	descr: 'Instructions which communicate with a debugger.',
	deps: [base]
}).

module_info(imms, info{
	title: 'Immediates',
	descr: 'Instructions contain embedded (immediate) values. Generally duplicates of instructions which operate on registers.',
	deps: [base]
}).

module_info(interrupts, info{
	title: 'Interrupts',
	descr: 'Instructions for handling and operating during hardware/software interrupts.',
	deps: [base, stack]
}).

module_info(mul, info{
	title: 'Multiply',
	descr: 'Instructions related to integer multiplication.',
	deps: [base, stack]
}).

module_info(security, info{
	title: 'Security',
	descr: 'Instructions related to computer security.',
	deps: [base]
}).

module_info(stack, info{
	title: 'Stack',
	descr: 'Instructions for manipulating the subroutine stack.',
	deps: [base]
}).

module_info(tsops, info{
	title: 'Test-stack Operations',
	descr: 'Instructions for manipulating the test-stack (`$TS`).',
	deps: [base]
}).
