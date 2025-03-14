:- module(sem, [
    instr_info/2,
    valid_semantics//1
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(isa).
:- use_module(library(clpfd)).
:- op(20, fx, #).
:- op(20, fx, $$).
:- op(20, fx, ?).
:- op(1050, xfx, <-).
:- op(400, yfx, xor).
:- op(400, yfx, and).
:- op(400, yfx, or).


valid_semantics(Stmt) --> stmt(Stmt).

rval_(LVal) --> lval(LVal).
rval_(#Term) --> % Constant or Immediate
    ( { atom(Term) } -> [] % Specification-time named constant
    ; { integer(Term) } -> [] % Numeric Literal
    ; { Term = ?Var, atom(Var) } -> [] % Immediate
    ).
rval_(A + B) --> rval(A), rval(B).
rval_(A - B) --> rval(A), rval(B).
rval_(A << B) --> rval(A), rval(B).
rval_(A >> B) --> rval(A), rval(B).
rval_(A == B) --> rval(A), rval(B).
rval_(A and B) --> rval(A), rval(B).
rval_(A or B) --> rval(A), rval(B).
rval_(A xor B) --> rval(A), rval(B).
rval_(b_pop(LVal)) --> lval(LVal).
rval_(const(Symbol)) --> { atom(Symbol) }.
rval_(zxt(RVal)) --> rval(RVal).
rval_(sxt(RVal)) --> rval(RVal).

rval(RVal) --> rval_(RVal) -> [] ; [invalid_rval(RVal)].

lval([Term]) --> rval(Term).
lval($Reg) --> { isa:gprreg(Reg) }.
lval($$Reg) --> { isa:sysreg(Reg) }.
lval(?Var) --> { atom(Var) }.
lval(hi(RVal)) --> rval(RVal).
lval(lo(RVal)) --> rval(RVal).
lval(hi_lo(A, B)) --> rval(A), rval(B).

stmt_(nop) --> [].
stmt_(if(Cond, Consq)) --> rval(Cond), stmt(Consq).
stmt_(if(Cond, Consq, Alt)) --> rval(Cond), stmt(Consq), stmt(Alt).
stmt_(Dst <- Src) --> lval(Dst), rval(Src).
stmt_(S1 ; S2) -->
    { S1 = (?Var = RVal) } ->
        { atom(Var) }, rval(RVal), stmt(S2)
    ;
        stmt(S1), stmt(S2).

stmt(Stmt) --> stmt_(Stmt) -> [] ; [invalid_stmt(Stmt)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:portray(---(Hypothesis, Conclusion)) :- print(Hypothesis), format('~n-----------------------~n'), print(Conclusion).
user:portray(Dst <- Src) :- print(Dst), format(' <- '), print(Src).
user:portray(Dst = Src) :- print(Dst), format(' = '), print(Src).
user:portray(A + B) :- print(A), format(' + '), print(B).
user:portray(A - B) :- print(A), format(' - '), print(B).
user:portray(A == B) :- print(A), format(' == '), print(B).
user:portray(A >> B) :- print(A), format(' >> '), print(B).
user:portray(A << B) :- print(A), format(' << '), print(B).
user:portray(A and B) :- print(A), format('  and  '), print(B).
user:portray(A or B) :- print(A), format('  or  '), print(B).
user:portray(A xor B) :- print(A), format('  xor  '), print(B).
user:portray(S1 ; S2) :- print(S1), format(';~n'), print(S2).
user:portray(#X) :- format('#'), print(X).
user:portray($X) :- format('$'), print(X).
user:portray($$X) :- format('$$'), print(X).
user:portray(?X) :- format('?'), print(X).
user:portray(if(Cond, Consq)) :-
    format('if ~p {~n', [Cond]),
    format(codes(ConsqCodes), '~p', [Consq]),
    indent_lines('    ', ConsqCodes, ConsqIndented),
    format('~w~n', [ConsqIndented]),
    format('}').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


instr_info(lb, info{
	title: 'Load Byte',
	descr: 'Load a byte from memory into a register.',
    ex: ['lb w, [sp+12]'],
    operands: [imm(?simm), reg(?adr), reg(?rd)],
	sem: ?rd <- zxt([?adr + ?simm])
}).
instr_info(lw, info{
	title: 'Load Word',
	descr: 'Load a word from memory into a register.',
	ex: ['lw w, [sp+12]'],
    operands: [imm(?simm), reg(?adr), reg(?rd)],
	sem: (
        ?ptr = (?adr + ?simm) and #0b1111111111111110;
        ?rd <- hi_lo([?ptr + #1], [?ptr])
    )
}).
instr_info(sb, info{
	title: 'Store Byte',
	descr: 'Store a byte from a register into memory.',
	ex: ['sb [sp-20], x'],
    operands: [imm(?simm), reg(?adr), reg(?rs)],
	sem: [?adr + ?simm] <- ?rs
}).
instr_info(sw, info{
	title: 'Store Word',
	descr: 'Store a word from a register into memory.',
	ex: ['sw [sp-20], x'],
    operands: [imm(?simm), reg(?adr), reg(?rs)],
	sem: (
        ?ptr = (?adr + ?simm) and #0b1111111111111110;
        [?ptr] <- lo(?rs);
        [?ptr + #1] <- hi(?rs)
    )
}).

instr_info(call, info{
	title: 'Call Subroutine',
	descr: 'Call a subroutine at the specified address.',
	ex: ['call SOME_LABEL'],
	operands: [imm(?imm)],
	sem: (
        $$pc <- $$pc + (sxt(?imm) << #subr_align);
        $$ra <- $$pc + #2
    )
}).

instr_info(b, info{
	title: 'Branch',
	descr: 'Branch to the specified address by adding the immediate offset to `$PC`.',
	ex: ['b SOME_LABEL'],
	operands: [imm(?offset)],
	sem: $$pc <- $$pc + sxt(?offset)
}).
instr_info(bt, info{
	title: 'Branch If True',
	descr: 'Branch to the specified address if the condition is true by adding the immediate offset to `$PC`.',
	ex: ['bt SOME_LABEL'],
	operands: [imm(?offset)],
	sem: if(b_pop($$ts) == #1,
        $$pc <- $$pc + sxt(?offset)
    )
}).
instr_info(bf, info{
	title: 'Branch If False',
	descr: 'Branch to the specified address if the condition is false by adding the immediate offset to `$PC`.',
	ex: ['bf SOME_LABEL'],
	operands: [imm(?offset)],
	sem: if(b_pop($$ts) == #0,
        $$pc <- $$pc + sxt(?offset)
    )
}).

instr_info(li, info{
	title: 'Load Immediate',
	descr: 'Load an immediate value into a register.',
	ex: ['li x, 123'],
	operands: [imm(?simm), reg(?rd)],
	sem: ?rd <- sxt(?simm)
}).
instr_info(szi, info{
	title: 'Shift Zero-extended Immediate',
	descr: 'Left-shift a zero-extended immediate value into a register.',
	ex: ['szi x, 0xB3'],
	operands: [imm(?imm), reg(?rd)],
	sem: ?rd <- (?rd << #8) or zxt(?imm)
}).

instr_info(lgb, info{
	title: 'Load Global Byte',
	descr: 'Load a byte from a memory address offset from `$GP`.',
	ex: ['lgb x, [gp+8]'],
	operands: [imm(?disp), reg(?rd)],
	sem: ?rd <- zxt([$$gp + zxt(?disp)])
}).
instr_info(lgw, info{
	title: 'Load Global Word',
	descr: 'Load a word from a memory address offset from `$GP`.',
	ex: ['lgw x, [gp+8]'],
	operands: [imm(?disp), reg(?rd)],
	sem: (
        ?ptr = ($$gp + zxt(?disp)) and #0b1111111111111110;
        ?rd <- hi_lo([?ptr + #1], [?ptr])
    )
}).
instr_info(sgb, info{
	title: 'Store Global Byte',
	descr: 'Store a byte into memory address offset from `$GP`.',
	ex: ['sgb [gp+8], x'],
	operands: [imm(?disp), reg(?rs)],
	sem: [$$gp + zxt(?disp)] <- ?rs
}).
instr_info(sgw, info{
	title: 'Store Global Word',
	descr: 'Store a word into memory address offset from `$GP`.',
	ex: ['sgw [gp+8], x'],
	operands: [imm(?disp), reg(?rs)],
	sem: (
        ?ptr = ($$gp + zxt(?disp)) and #0b1111111111111110;
        hi_lo([?ptr + #1], [?ptr]) <- ?rs
    )
}).
instr_info(tbit, info{
	title: 'Test Bit',
	descr: 'Test a specific bit in a register, modifying `$TS`.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(cbit, info{
	title: 'Clear Bit',
	descr: 'Clear a specific bit in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(sbit, info{
	title: 'Set Bit',
	descr: 'Set a specific bit in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tli, info{
	title: 'Test Less-than Immediate',
	descr: 'Test if a register value is less than an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tgei, info{
	title: 'Test Greater-than or Equal Immediate',
	descr: 'Test if a register value is greater than or equal to an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tbi, info{
	title: 'Test Below Immediate',
	descr: 'Test if a register value is below an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(taei, info{
	title: 'Test Above or Equal',
	descr: 'Test if a register value is above or equal to an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tnei, info{
	title: 'Test Not Equal Immediate',
	descr: 'Test if a register value is not equal to an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(teqi, info{
	title: 'Test Equal Immediate',
	descr: 'Test if a register value is equal to an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(addi, info{
	title: 'Add Immediate',
	descr: 'Add an immediate value to a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(andi, info{
	title: 'AND Immediate',
	descr: 'Perform a bitwise AND between a register and an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(ori, info{
	title: 'OR Immediate',
	descr: 'Perform a bitwise OR between a register and an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).

instr_info(xori, info{
	title: 'XOR Immediate',
	descr: 'Perform a bitwise XOR between a register and an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(addicy, info{
	title: 'Add Immediate with Carry',
	descr: 'Add an immediate value and the carry bit to a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(subicy, info{
	title: 'Subtract Immediate with Carry',
	descr: 'Sutract an immediate value and the carry bit from a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(lsr, info{
	title: 'Logical Shift Right',
	descr: 'Perform a logical shift right on a register by an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(lsl, info{
	title: 'Logical Shift Left',
	descr: 'Perform a logical shift left on a register by an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(asr, info{
	title: 'Arithmetic Shift Right',
	descr: 'Perform an arithmetic shift right on a register by an immediate value.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).

instr_info(add, info{
	title: 'Add',
	descr: 'Add the values of two registers.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(sub, info{
	title: 'Subtract',
	descr: 'Subtract the value of one register from another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(and, info{
	title: 'AND',
	descr: 'Perform a bitwise AND between two registers.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(or, info{
	title: 'OR',
	descr: 'Perform a bitwise OR between two registers.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(xor, info{
	title: 'XOR',
	descr: 'Perform a bitwise XOR between two registers.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(mov, info{
	title: 'Move',
	descr: 'Move the value from one register to another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(addcy, info{
	title: 'Add with Carry',
	descr: 'Add the values of two registers with carry.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(subcy, info{
	title: 'Subtract with Carry',
	descr: 'Subtract the value of one register from another with carry.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tl, info{
	title: 'Test Less-than',
	descr: 'Test if the value of one register is less than another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tge, info{
	title: 'Test Greater-than or Equal',
	descr: 'Test if the value of one register is greater than or equal to another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tb, info{
	title: 'Test Below',
	descr: 'Test if the value of one register is below another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tae, info{
	title: 'Test Above or Equal',
	descr: 'Test if the value of one register is above or equal to another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tne, info{
	title: 'Test Not Equal',
	descr: 'Test if the value of one register is not equal to another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(teq, info{
	title: 'Test Equal',
	descr: 'Test if the value of one register is equal to another.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).

instr_info(mulstep, info{
	title: 'Multiplication Step',
	descr: 'Computes one step in a full 16-bit by 16-bit multiplication.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).

instr_info(pushb, info{
	title: 'Push Byte',
	descr: 'Push a byte from a register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(pushw, info{
	title: 'Push Word',
	descr: 'Push a word from a register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(popb, info{
	title: 'Pop Byte',
	descr: 'Pop a byte from the stack into a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(popw, info{
	title: 'Pop Word',
	descr: 'Pop a word from the stack into a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(callr, info{
	title: 'Call Register',
	descr: 'Call a subroutine at the address in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(jr, info{
	title: 'Jump Register',
	descr: 'Jump to the address in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(neg, info{
	title: 'Negate',
	descr: 'Negate the value in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(seb, info{
	title: 'Sign Extend Byte',
	descr: 'Sign extend a byte in a register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rd.mp.lo', info{
	title: 'Read $MP.lo',
	descr: 'Read the low word in the system `$MP` register into a general purpose register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rd.mp.hi', info{
	title: 'Read $MP.hi',
	descr: 'Read the high word in the system `$MP` register into a general purpose register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rd.gp', info{
	title: 'Read $GP',
	descr: 'Read the value of the system `$GP` register into a general purpose register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('wr.gp', info{
	title: 'Write $GP',
	descr: 'Write a value to the system `$GP` register from a general purpose register.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).

instr_info('NONEXE1', info{
	title: 'Non-executable (1s Version)',
	descr: 'Triggers a "non-executable instruction" exception. The entire instruction is 16 `1`s.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('BREAK', info{
	title: 'Breakpoint',
	descr: 'Trigger a breakpoint.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('HALT', info{
	title: 'Halt',
	descr: 'Halt the processor.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('UNIMPL', info{
	title: 'Unimplemented',
	descr: 'Unimplemented instruction.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(kret, info{
	title: 'Kernel Return',
	descr: 'Return from kernel mode.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(kcall, info{
	title: 'Kernel Call',
	descr: 'Call a kernel function.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(ret, info{
	title: 'Return',
	descr: 'Return from a subroutine.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tov, info{
	title: 'Test Overflow',
	descr: 'Test for overflow.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tcy, info{
	title: 'Test Carry',
	descr: 'Test for carry.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('clr.cy', info{
	title: 'Clear Carry',
	descr: 'Clear the carry flag.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('set.cy', info{
	title: 'Set Carry',
	descr: 'Set the carry flag.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tpush0, info{
	title: 'Teststack Push 0',
	descr: 'Push 0 onto the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tpush1, info{
	title: 'Teststack Push 1',
	descr: 'Push 1 onto the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tnot, info{
	title: 'Teststack NOT',
	descr: 'Perform a NOT operation on the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tand, info{
	title: 'Teststack AND',
	descr: 'Perform an AND operation on the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tor, info{
	title: 'Teststack OR',
	descr: 'Perform an OR operation on the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info(tdup, info{
	title: 'Teststack Duplicate',
	descr: 'Duplicate the top value on the test stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('prsv.mp', info{
	title: 'Preserve $MP',
	descr: 'Preserve the value of the `$MP` register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rstr.mp', info{
	title: 'Restore $MP',
	descr: 'Restore the value of the `$MP` register from the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('prsv.ts', info{
	title: 'Preserve $TS',
	descr: 'Preserve the value of the `$TS` register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rstr.ts', info{
	title: 'Restore $TS',
	descr: 'Restore the value of the `$TS` register from the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('prsv.ra', info{
	title: 'Preserve $RA',
	descr: 'Preserve the value of the `$RA` register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rstr.ra', info{
	title: 'Restore $RA',
	descr: 'Restore the value of the `$RA` register from the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('prsv.gp', info{
	title: 'Preserve $GP',
	descr: 'Preserve the value of the `$GP` register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rstr.gp', info{
	title: 'Restore $GP',
	descr: 'Restore the value of the `$GP` register from the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('prsv.cc', info{
	title: 'Preserve $CC',
	descr: 'Preserve the value of the `$CC` register onto the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).
instr_info('rstr.cc', info{
	title: 'Restore $CC',
	descr: 'Restore the value of the `$CC` register from the stack.',
	ex: [],
	operands: [OPERANDS],
	sem: nop
}).