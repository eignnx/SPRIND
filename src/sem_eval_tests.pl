:- module(sem_eval_tests, []).

:- use_module(querilog_syntax).
:- use_module(querilog_eval, [
    bv_signed/2,
    bv_signed/3,
    bv_unsigned/2,
    bv_unsigned/3
]).
:- use_module(querilog_tck_driver, [typecheck_instr/2]).
:- use_module(sem, [instr_info/2]).
:- use_module(utils).

run_instrs(Setup, Instrs, FinalStateSimplified) :-
    setup_initialstate(Setup, InitialState),
    run_instrs_(Instrs, InitialState, FinalState),
    prevstate_nextstate(FinalState, FinalStateAdvanced),
    state_simplified(FinalStateAdvanced, FinalStateSimplified).

setup_initialstate([], S) :- querilog_eval:init_state(S).
setup_initialstate([$Reg=Value | Rest], S) :-
    setup_initialstate(Rest, S0),
    bv_unsigned(ValueBv, Value, 16),
    S = S0.put(curr/regs/Reg, ValueBv).
setup_initialstate([$$Reg=Value | Rest], S) :-
    setup_initialstate(Rest, S0),
    isa:sysreg_size(Reg, Size),
    bv_unsigned(ValueBv, Value, Size),
    S = S0.put(curr/sysregs/Reg, ValueBv).
setup_initialstate([mem(AddrU)=Value | Rest], S) :-
    setup_initialstate(Rest, S0),
    bv_unsigned(ValueBv, Value, 16),
    S = S0.put(curr/mem/AddrU, ValueBv).

run_instrs_([], Current, Current).
run_instrs_([Instr | Instrs], Before0, After) :-
    % First typecheck the instruction:
    functor(Instr, InstrName, _Arity),
    querilog_tck_driver:typecheck_instr(InstrName, TypeCheckedSem),

    % Advance the state:
    prevstate_nextstate(Before0, Before1),

    % Next generate bindings (assign actual arguments to formal parameters).
    instrcall_bindings(Instr, Bindings),
    Before2 = Before1.put(bindings, Bindings),

    % Finally run the instr with the updated state:
    phrase(querilog_eval:stmt_eval(TypeCheckedSem), [Before2], [After0]),

    % Run the remaining instrs:
    run_instrs_(Instrs, After0, After).

prevstate_nextstate(S0, S) :-
    querilog_eval:init_state(Fresh),
    S = interpstate{
        bindings: [], % Reset bindings for each instr invocation
        curr: #{
            regs: S0.curr.regs.put(S0.next.regs),
            sysregs: S0.curr.sysregs.put(S0.next.sysregs),
            mem: S0.curr.mem.put(S0.next.mem)
        },
        next: Fresh.next
    }.


/*
interpstate{ bindings: Bs, curr: Curr, next: Next }
==>
interpstate{ bindings: Bs, ...Curr }
(also all bitvectors have been reinterpreted as Prolog integers)
*/
state_simplified(S0, S) :-
    maplist(simplify_binding, S0.bindings, Bindings),
    mapdict(simplify_kv, S0.curr.regs, Regs), is_dict(Regs, #),
    mapdict(simplify_kv, S0.curr.sysregs, SysRegs), is_dict(SysRegs, #),
    S = interpstate{
        bindings: Bindings,
        regs: Regs,
        sysregs: SysRegs,
        mem: S0.next.mem
    }.

simplify_kv(_RegName, Bv, U) :-
    bv_unsigned(Bv, U).

simplify_binding(Var = $Reg, Var = $Reg) :- !.
simplify_binding(Var = bv(Bv), Var = U) :- bv_unsigned(bv(Bv), U).


:- det(instrcall_bindings/2).
instrcall_bindings(InstrCall, Bindings) :-
    InstrCall =.. [InstrName | Args],
    once(sem:instr_info(InstrName, Info)),
    params_args_bindings_instr(Info.syntax, Args, Bindings, InstrName).

params_args_bindings_instr({}, [], [], _InstrName).
params_args_bindings_instr(({ParamsCommaList} -> _), Args, Bindings, InstrName) :-
    params_args_bindings_instr({ParamsCommaList}, Args, Bindings, InstrName).
params_args_bindings_instr({ParamsCommaList}, Args, Bindings, InstrName) :-
    comma_list(ParamsCommaList, Params),
    maplist(
        instr_param_arg_bindings_checked(InstrName),
        Params, Args, BindingsNested
    ),
    flatten(BindingsNested, Bindings).

:- det(instr_param_arg_bindings_checked/4).
instr_param_arg_bindings_checked(InstrName, Param, Arg, Bindings) :-
    ( param_arg_instr_bindings(Param, Arg, InstrName, Bindings) -> true
    ;
        isa:fmt_instr(Fmt, InstrName),
        once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
        throw_error(invalid_instruction_call, #{
            instr: InstrName,
            imm_bits: ImmBits,
            expected_parameter: Param,
            actual_argument: Arg
        })
    ).

:- det(param_arg_instr_bindings/4).
param_arg_instr_bindings(reg(_, ?Var), $Reg, _I, [Var = $Reg]).
param_arg_instr_bindings(imm(?Var), #Int, Instr, [Var=Bv]) :-
    isa:fmt_instr(Fmt, Instr),
    once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
    bv_unsigned(Bv, Int, ImmBits).
param_arg_instr_bindings(simm(?Var), #Int, Instr, [Var=Bv]) :-
    isa:fmt_instr(Fmt, Instr),
    once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
    bv_signed(Bv, Int, ImmBits).
param_arg_instr_bindings([Param], [Arg], I, Bindings) :-
    param_arg_instr_bindings(Param, Arg, I, Bindings).
param_arg_instr_bindings(P1+P2, A1+A2, I, [B1, B2]) :-
    param_arg_instr_bindings(P1, A1, I, B1Nested), flatten(B1Nested, B1),
    param_arg_instr_bindings(P2, A2, I, B2Nested), flatten(B2Nested, B2).
param_arg_instr_bindings(P1:P2, A1:A2, [B1, B2]) :-
    param_arg_instr_bindings(P1, A1, I, B1Nested), flatten(B1Nested, B1),
    param_arg_instr_bindings(P2, A2, I, B2Nested), flatten(B2Nested, B2).


:- begin_tests(sem_eval_tests_).

test(li_instr, [true(X == 127)]) :-
    run_instrs([$x=99], [
        li($x, #127)
    ], State),
    #{ x: X } :< State.regs,
true.

test(li_instr_x2, [
    X == 34,
    Y == 56
]) :-
    run_instrs([$x=12], [
        li($x, #34),
        li($y, #56)
    ], State),
    #{ x: X, y: Y } :< State.regs,
true.

test(szi_instr, [
    X == 0x1234
]) :-
    run_instrs([$x=0x12], [
        szi($x, #0x34)
    ], State),
    #{ x: X } :< State.regs,
true.

test(li_then_szi, [X == 0x1234]) :-
    run_instrs([$x=12], [
        li($x, #0x12),
        szi($x, #0x34)
    ], State),
    #{ x: X } :< State.regs,
true.

test(add_instr, [X == 46, Y == 34]) :-
    run_instrs([$x=12, $y=34], [
        add($x, $y)
    ], State),
    #{ x: X, y: Y } :< State.regs,
true.

test(b_instr, [Pc == 2026]) :-
    run_instrs([$$pc=2000], [
        b(#26)
    ], State),
    #{ pc: Pc } :< State.sysregs,
true.

test(lb_instr, [W == 0]) :-
    run_instrs([], [
        lb($w, [$sp + #12])
    ], State),
    #{ w: W } :< State.regs,
true.

test(lb_instr_nonzero, [W == 45]) :-
    run_instrs([$sp=120, mem(123)=45], [
        lb($w, [$sp + #3])
    ], State),
    #{ w: W } :< State.regs,
true.

test(or_instr, [X == 0xAABB]) :-
    run_instrs([$x=0xAA00, $y=0x00BB], [
        or($x, $y)
    ], State),
    #{ x: X } :< State.regs,
true.

test('set.cy instr', [Cc == 0x0001]) :-
    run_instrs([$$cc=0], [
        'set.cy'
    ], State),
    #{ cc: Cc } :< State.sysregs,
true.

:- end_tests(sem_eval_tests_).
