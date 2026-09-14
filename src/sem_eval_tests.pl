:- module(sem_eval_tests, []).

:- use_module(querilog_syntax).
:- use_module(querilog_eval, [
    bv_signed/2,
    bv_signed/3,
    bv_unsigned/2,
    bv_unsigned/3
]).
:- use_module(sem, [instr_info/2]).
:- use_module(utils).

run_instrs(Setup, Instrs, FinalStateSimplified) :-
    setup_initialstate(Setup, InitialState),
    run_instrs_(Instrs, InitialState, FinalState),
    state_simplified(FinalState, FinalStateSimplified).

setup_initialstate([], S) :- querilog_eval:init_state(S).
setup_initialstate([$Reg=Value | Rest], S) :-
    setup_initialstate(Rest, S0),
    bv_signed(ValueBv, Value, 16),
    S = S0.put(curr/regs/Reg, ValueBv).
setup_initialstate([$$Reg=Value | Rest], S) :-
    setup_initialstate(Rest, S0),
    isa:sysreg_size(Reg, Size),
    bv_signed(ValueBv, Value, Size),
    S = S0.put(curr/sysregs/Reg, ValueBv).

run_instrs_([], Current, Current).
run_instrs_([Instr | Instrs], Before0, After) :-
    functor(Instr, InstrName, _Arity),
    instr_info(InstrName, Info),
    instrcall_bindings(Instr, Bindings),
    Before1 = Before0.put(bindings, Bindings),
    phrase(querilog_eval:stmt_eval(Info.sem), [Before1], [After0]),
    run_instrs_(Instrs, After0, After).


/*
interpstate{ bindings: Bs, curr: Curr, next: Next }
==>
interpstate{ bindings: Bs, ...Next}
*/
state_simplified(S0, S) :-
    maplist(simplify_binding, S0.bindings, Bindings),
    mapdict(simplify_kv, S0.next.regs, Regs),
    mapdict(simplify_kv, S0.next.sysregs, SysRegs),
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
    sem:instr_info(InstrName, Info),
    params_args_bindings(Info.syntax, Args, Bindings).

params_args_bindings(({ParamsCommaList} -> _), Args, Bindings) :-
    params_args_bindings(ParamsCommaList, Args, Bindings).
params_args_bindings({ParamsCommaList}, Args, Bindings) :-
    comma_list(ParamsCommaList, Params),
    maplist(param_arg_bindings_checked, Params, Args, BindingsNested),
    flatten(BindingsNested, Bindings).

param_arg_bindings_checked(Param, Arg, Bindings) :-
    ( param_arg_bindings(Param, Arg, Bindings) -> true
    ; throw_error(invalid_instruction_call, #{
            expected_parameter: Param,
            actual_argument: Arg
        })
    ).

param_arg_bindings(reg(_, ?Var), $Reg, [Var = $Reg]).
param_arg_bindings(imm(?Var), #Int, [Var=Bv]) :- bv_unsigned(Bv, Int).
param_arg_bindings(simm(?Var), #Int, [Var=Bv]) :- bv_signed(Bv, Int).
param_arg_bindings([Param], [Arg], Bindings) :-
    param_arg_bindings(Param, Arg, Bindings).
param_arg_bindings(P1+P2, A1+A2, [B1, B2]) :-
    param_arg_bindings(P1, A1, B1Nested), flatten(B1Nested, B1),
    param_arg_bindings(P2, A2, B2Nested), flatten(B2Nested, B2).
param_arg_bindings(P1:P2, A1:A2, [B1, B2]) :-
    param_arg_bindings(P1, A1, B1Nested), flatten(B1Nested, B1),
    param_arg_bindings(P2, A2, B2Nested), flatten(B2Nested, B2).


:- begin_tests(sem_eval_tests_).

test(li_instr, [true(X == 999)]) :-
    run_instrs(
        [$x=12],
        [
            li($x, #999)
        ],
        State
    ),
    X = State.regs.x.

:- end_tests(sem_eval_tests_).
