:- module(validate, [
    run_validations/0
]).

:- use_module(isa).
:- use_module(sem).

run_validations :-
    disprove('instr_info is not one-to-one with fmt_instr'(_)),
    disprove('ill-formed instruction semantics'(_, _)),
    disprove('undefined constant in semantic definition'(_, _)),
    true.

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
    include([error(E), E]>>true, Emissions, Errors),
    Errors = [_|_].

'undefined constant in semantic definition'(instruction(Instr), undefined_constant(Constant)) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Emissions),
    member(constant(Constant), Emissions),
    \+ sem:def(Constant, _).

    