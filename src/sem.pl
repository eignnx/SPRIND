:- module(sem, [
    instr_info/2,
    valid_semantics//1
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(isa).
:- use_module(library(clpfd)).
:- op(20, fx, #).
:- op(20, fx, &).
:- op(20, fx, $$).
:- op(20, fx, ?).
:- op(1050, xfy, <-).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


valid_semantics(Stmt) --> stmt(Stmt).

rval_(LVal) --> lval(LVal).
rval_(#Term) --> % Specification-time constant
    ( { atom(Term) } -> []
    ; { integer(Term) } -> []
    ).
rval_(?Var) --> { atom(Var) }. % Local semantic binding. Aliases a semantic value.
rval_(imm(N, Imm)) --> { #Hi #= 2 ^ #N, Imm in 0 .. Hi }.
rval_(simm(N, Simm)) --> { #Lo #= -1 * 2 ^ (#N - 1), #Hi #= 2 ^ (#N - 1), Simm in Lo .. Hi }.
rval_(A + B) --> rval(A), rval(B).
rval_(A - B) --> rval(A), rval(B).
rval_(A << B) --> rval(A), rval(B).
rval_(A >> B) --> rval(A), rval(B).
rval_(A == B) --> rval(A), rval(B).
rval_(b_and(A, B)) --> rval(A), rval(B).
rval_(b_or(A, B)) --> rval(A), rval(B).
rval_(b_xor(A, B)) --> rval(A), rval(B).
rval_(b_pop(LVal)) --> lval(LVal).
rval_(const(Symbol)) --> { atom(Symbol) }.
rval_(zxt(RVal)) --> rval(RVal).
rval_(sxt(RVal)) --> rval(RVal).
rval_(hi(RVal)) --> rval(RVal).
rval_(lo(RVal)) --> rval(RVal).
rval_(hi_lo(A, B)) --> rval(A), rval(B).

rval(RVal) --> rval_(RVal) -> [] ; [invalid_rval(RVal)].

lval([Term]) --> rval(Term).
lval($Reg) --> { isa:gprreg(Reg) }.
lval(&Reg) --> { isa:adrreg(Reg) }.
lval($$Reg) --> { isa:sysreg(Reg) }.

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


instr_info(lb, info{
	title: 'Load Byte',
	descr: 'Load a byte from memory into a register.',
    ex: ['lb w, [sp+12]'],
    syn: lb(Rd, [Adr + Imm]),
	sem: $Rd <- zxt([&Adr + simm(7, Imm)])
}).
instr_info(lw, info{
	title: 'Load Word',
	descr: 'Load a word from memory into a register.',
	ex: ['lw w, [sp+12]'],
	syn: lw(Rd, [Adr + Imm]),
	sem: (
        ?ptr = b_and(&Adr + simm(7, Imm), #0b1111111111111110);
        $Rd <- hi_lo([?ptr + #1], [?ptr])
    )
}).
instr_info(sb, info{
	title: 'Store Byte',
	descr: 'Store a byte from a register into memory.',
	ex: ['sb [sp-20], x'],
	syn: sb([Adr + Imm], Rs),
	sem: [&Adr + simm(7, Imm)] <- $Rs
}).
instr_info(sw, info{
	title: 'Store Word',
	descr: 'Store a word from a register into memory.',
	ex: ['sw [sp-20], x'],
	syn: sw([Adr + Imm], Rs),
	sem: (
        ?ptr = b_and(&Adr + simm(7, Imm), #0b1111111111111110);
        [?ptr] <- lo($Rs);
        [?ptr + #1] <- hi($Rs)
    )
}).

instr_info(call, info{
	title: 'Call Subroutine',
	descr: 'Call a subroutine at the specified address.',
	ex: ['call SOME_LABEL'],
	syn: call(Imm),
	sem: (
        $$pc <- $$pc + (sxt(imm(7, Imm)) << #subr_align);
        $$ra <- $$pc + #2
    )
}).

instr_info(b, info{
	title: 'Branch',
	descr: 'Branch to the specified address by adding the immediate offset to `$PC`.',
	ex: ['b SOME_LABEL'],
	syn: b(Imm),
	sem: $$pc <- $$pc + sxt(simm(7, Imm))
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