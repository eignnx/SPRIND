:- module(gen_machine_overview, [report/1]).

:- use_module(markdown).
:- use_module(isa).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


report(Lvl) :-
    markdown:emit_heading(Lvl, 'SPRIND Abstract Machine Overview'),
    display_gprs(s(Lvl)),
    display_register_uses(s(Lvl)),
    display_sysregs(s(Lvl)),
    true.

display_gprs(Lvl) :-
    markdown:emit_heading(Lvl, 'General Purpose Registers'),
    markdown:emit_table_header(['Register Name', '[Uses](#register-uses-and-calling-convention)']),
    foreach(
        isa:regname_uses(Reg, Uses),
        display_gpr_info(Reg, Uses)
    ).

display_gpr_info(Reg, Uses) :-
    phrase(sequence(atom, `, `, Uses), UsesList),
    markdown:emit_table_row([code(a(Reg)), s(UsesList)]).


display_register_uses(Lvl) :-
    markdown:emit_heading(Lvl, 'Register Uses and Calling Convention'),
    markdown:emit_table_header(['Usage Name', left('Description')]),
    foreach(
        isa:reguse_description(RegUse, Descr),
        markdown:emit_table_row([code(fmt('~k', RegUse)), a(Descr)])
    ).

display_sysregs(Lvl) :-
    markdown:emit_heading(Lvl, 'System Registers'),
    markdown:emit_table_header(['Register', 'Register Name', 'Size', left('Description')]),
    foreach(
        (
            isa:sysregname_name_size_description(Reg, Name, Size, Descr),
            upcase_atom(Reg, RegUpcase)
        ),
        markdown:emit_table_row([code(fmt('$~w', RegUpcase)), a(Name), fmt('~d-bits', Size), a(Descr)])
    ).