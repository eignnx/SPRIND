:- module(validate, [
    run_validations/0
]).

:- use_module(isa).
:- use_module(sem).

run_validations :-
    disprove('instr_info is not one-to-one with fmt_instr'(_)),
    disprove('ill-formed instruction semantics'(_, _)),
    true.

disprove(NegativeCheck) :-
    NegativeCheck ->
        throw(error(validation_failed(NegativeCheck)))
    ;
        true.

'instr_info is not one-to-one with fmt_instr'(Instr) :-
    (
        isa:instr(Instr), \+ sem:instr_info(Instr, _)
    )
    ;
    (
        sem:instr_info(Instr, _), \+ isa:instr(Instr)
    ).

'ill-formed instruction semantics'(Instr, Errors) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Errors),
    Errors = [_|_].