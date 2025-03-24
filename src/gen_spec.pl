:- module(gen_spec, [generate_spec/0]).

:- use_module(validate).
:- use_module(utils, [output_to_file/2]).
:- use_module(markdown).

:- use_module(gen_machine_overview, [report/1 as gen_machine_overview_report]).
:- use_module(gen_instr_listing, [report/1 as gen_instr_listing_report]).
:- use_module(gen_asm_spec, [report/0 as gen_asm_spec_report]).

generate_spec :-
    call_time(
        utils:warn_if_nondet(gen_spec:generate_spec_),
        Time
    ),
    format('[`gen_spec` ran in ~3fs]~n', [Time.wall]),
end.

generate_spec_ :-
    utils:peano_decimal(LvlOne, 1),
    output_to_file('docs/spec-outline.md', gen_spec_outline(LvlOne)),
    output_to_file('docs/machine-overview.md', gen_machine_overview_report(LvlOne)),
    output_to_file('docs/instruction-listing.md', gen_instr_listing_report(LvlOne)),
    output_to_file('SPRIND.customasm', gen_asm_spec_report),
end.

gen_spec_outline(Lvl) :-
    catch(validate:run_validations, error(E), (
        format('~n> !!! '),
        numbervars(E),
        write_term(E, [numbervars(true)]),
        format('~n~n')
    )), % validate:run_validations,
    markdown:emit_heading(Lvl, 'SPRIND Instruction Set Architecture Specification'),
    markdown:emit_heading(s(Lvl), 'Outline'),
    format('- [Machine Overview](machine-overview.md): Describes registers and instruction formats.~n'),
    format('- [Instruction Listing](instruction-listing.md): Lists and defines all instructions.~n'),
end.


end.
