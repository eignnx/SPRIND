:- module(validate, [
    run_validations/0
]).

:- use_module(isa).

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
        isa:fmt_instr_title_description(_, Instr, _, _),
        \+ isa:instr_info(Instr, _)
    )
    ;
    (
        isa:instr_info(Instr, _),
        \+ isa:fmt_instr_title_description(_, Instr, _, _)
    ).

'ill-formed instruction semantics'(Instr, Errors) :-
    isa:instr_info(Instr, Info),
    phrase(isa:valid_semantics(Info.sem), Errors),
    Errors = [_|_].