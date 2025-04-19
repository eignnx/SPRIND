
:- module(validate, [
    op(20, fx, #),
    op(20, fx, $$),
    op(20, fx, ?),
    op(1050, xfx, <-),
    op(400, yfx, xor),
    op(400, yfx, and),
    op(400, yfx, or),
    run_validations/0
]).

:- op(20, fx, #).
:- op(20, fx, $$).
:- op(20, fx, ?).
:- op(1050, xfx, <-).
:- op(400, yfx, xor).
:- op(400, yfx, and).
:- op(400, yfx, or).

:- use_module(library(clpfd)).
:- use_module(isa).
:- use_module(derive).
:- use_module(sem).
:- use_module(tyck).

run_validations :-
    disprove('instr_info is not one-to-one with fmt_instr'(_)),
    disprove('ill-formed instruction semantics'(_, _)),
    disprove('undefined constant in semantic definition'(_, _)),
    disprove(tyck:'incompatible bit sizes'(_, _)),
    disprove('use of undefined module'(_)),
    disprove('use of undefined format'(_, _)),
end.

disprove(NegativeCheck) :-
    NegativeCheck ->
        throw(error(validation_failed(NegativeCheck)))
    ;
        true.

'instr_info is not one-to-one with fmt_instr'(instruction(Instr)) :-
    (
        isa:instr(Instr), \+ sem:instr_info(Instr, _)
    )
    ;
    (
        sem:instr_info(Instr, _), \+ isa:instr(Instr)
    ).

'ill-formed instruction semantics'(instruction(Instr), Errors) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Emissions),
    convlist([error(E), E]>>true, Emissions, Errors),
    Errors = [_|_].

'undefined constant in semantic definition'(instruction(Instr), undefined_constant(Constant)) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Emissions),
    member(constant(Constant), Emissions),
    \+ sem:def(Constant, _).

'use of undefined module'(Module) :-
    sem:instr_info(_Instr, Info),
    [Module] = Info.module,
    \+ sem:module_info(Module, _).

'use of undefined format'(fmt(Fmt), instr(Instr)) :-
    isa:fmt_instr_title(Fmt, Instr, _),
    \+ fmt(Fmt).


end.
