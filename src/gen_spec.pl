:- module(gen_spec, [generate_spec/0]).

:- use_module(validate).
:- use_module(utils).
:- use_module(markdown).

:- use_module(gen_machine_overview, [report/1 as gen_machine_overview_report]).
:- use_module(gen_instr_listing, [report/1 as gen_instr_listing_report]).

generate_spec :-
    catch(validate:run_validations, error(E), format('~n> !!! ~w~n~n', [E])),
    % validate:run_validations,
    utils:warn_if_nondet(gen_spec:generate_spec_),
end.

generate_spec_ :-
    utils:peano_decimal(LvlOne, 1),

    markdown:emit_heading(LvlOne, 'SPRIND Instruction Set Architecture Specification'),
    gen_machine_overview_report(s(LvlOne)),
    gen_instr_listing_report(s(LvlOne)),
end.


end.
