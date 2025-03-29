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
    display_modules(s(Lvl)),
end.

display_gprs(Lvl) :-
    markdown:emit_heading(Lvl, 'General Purpose Registers'),
    markdown:emit_table_header(['Register Name', '[Uses](#register-uses-and-calling-convention)']),
    foreach(
        isa:regname_uses(Reg, Uses),
        display_gpr_info(Reg, Uses)
    ),
end.

display_gpr_info(Reg, Uses) :-
    phrase(sequence(atom, `, `, Uses), UsesList),
    markdown:emit_table_row([code(a(Reg)), s(UsesList)]),
end.


display_register_uses(Lvl) :-
    markdown:emit_heading(Lvl, 'Register Uses and Calling Convention'),
    markdown:emit_table_header(['Usage Name', left('Description')]),
    foreach(
        isa:reguse_description(RegUse, Descr),
        markdown:emit_table_row([code(fmt('~k', RegUse)), a(Descr)])
    ),
end.

display_sysregs(Lvl) :-
    markdown:emit_heading(Lvl, 'System Registers'),
    markdown:emit_table_header(['Register', 'Register Name', 'Size', left('Description')]),
    foreach(
        (
            isa:sysregname_name_size_description(Reg, Name, Size, Descr),
            upcase_atom(Reg, RegUpcase)
        ),
        markdown:emit_table_row([code(fmt('$~w', RegUpcase)), a(Name), fmt('~d-bits', Size), a(Descr)])
    ),
end.

display_modules(Lvl) :-
    markdown:emit_heading(Lvl, 'Extension Modules'),
    format('Extension modules are groups of instructions which add functionality to the processor.~n'),
    format('They allow the processor to be built up in stages, or can be omitted for a simpler processor.~n'),
    foreach(
        sem:module_info(Module, Info),
        display_module(s(Lvl), Module, Info)
    ),
end.

display_module(Lvl, Module, Info) :-
    markdown:emit_heading(Lvl, '`~w`', [Module]),
    format('**~w**~n~n', [Info.title]),
    format('~w~n~n', [Info.descr]),
    markdown:emit_heading(s(Lvl), 'Dependencies'),
    format('`~p`~n', [Info.deps]),
    markdown:emit_heading(s(Lvl), 'Instructions'),
    sem:module_instrs(Module, Instrs),
    maplist(
        [Instr]>>(
            format(codes(SectionNameRendered), 'The `~w` Instruction', [Instr]),
            utils:codes_slugified(SectionNameRendered, Slug),
            format('[`~p`](instruction-listing.md#~s), ', [Instr, Slug])
        ),
        Instrs
    ),
end.


end.