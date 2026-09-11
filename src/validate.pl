
:- module(validate, [
    run_validations/0
]).


:- use_module(library(clpfd)).
:- use_module(isa).
:- use_module(derive).
:- use_module(sem).
:- use_module(querilog_tck_driver).

run_validations :-
    disprove('instr_info is not one-to-one with fmt_instr'(_)),
    disprove('ill-formed instruction semantics'(_, _)),
    disprove('use of undefined module'(_)),
    disprove('use of undefined format'(_, _)),
true.

disprove(NegativeCheck) :-
    NegativeCheck ->
        throw(error(validation_failed(NegativeCheck)))
    ;
        true.

'instr_info is not one-to-one with fmt_instr'(instruction(Instr)) :-
    ( isa:instr(Instr), \+ sem:instr_info(Instr, _) )
    ;
    ( sem:instr_info(Instr, _), \+ isa:instr(Instr) ).

'ill-formed instruction semantics'(instruction(Instr), Error) :-
    sem:instr_info(Instr, Info),
    catch(
        ( querilog_tck_driver:typecheck_instr(Instr) -> fail ;
            Error = typechecking_failed(Instr, Info.sem)
        ),
        error(E, _),
        Error = typecheck_exception(Instr, E)
    ).

'use of undefined module'(Module) :-
    sem:instr_info(_Instr, Info),
    [Module] = Info.module,
    \+ sem:module_info(Module, _).

'use of undefined format'(fmt(Fmt), instr(Instr)) :-
    isa:fmt_instr_title(Fmt, Instr, _),
    \+ fmt(Fmt).

