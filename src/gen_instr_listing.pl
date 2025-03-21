:- module(gen_instr_listing, [report/1]).

:- use_module(isa).
:- use_module(derive).
:- use_module(sem).
:- use_module(markdown).
:- use_module(utils).
:- use_module(optree).
:- use_module(library(clpfd)).
:- op(20, fx, #).


report(Lvl) :-
    markdown:emit_heading(Lvl, 'Instructions'),
    display_instruction_counts_by_format(s(Lvl)),
    display_instruction_listing(s(Lvl)),
    display_instr_format_breakdown(s(Lvl)),
    display_instr_specifications(s(Lvl)),
end.


display_instruction_counts_by_format(Lvl) :-
    markdown:emit_heading(Lvl, 'Instruction Counts by Format'),
    markdown:emit_table_header(['Generic format', left('Description'), 'Available Opcodes', 'Assigned', 'Utilization']),
    foreach(
        genericfmt(GFmt),
        display_genericfmt_instr_count(GFmt)
    ),

    derive:total_opcode_count_minmax(TotalMin, TotalMax),

    ( TotalMin == TotalMax -> TotalAvailable = TotalMin
    ; throw(error(non_unique_total_available_opcode_count(TotalMax, TotalMin)))
    ),

    aggregate_all(count, (
        isa:fmt_instr(_Fmt, Instr),
        dif(Instr, ???)
    ), TotalAssigned),

    TotalUtilization is 100 * TotalAssigned / TotalAvailable,

    markdown:emit_table_row([a(''), bold(a('Totals (excluding `ext`)')), bold(d(TotalAvailable)), bold(d(TotalAssigned)), bold(fmt('~0f%', TotalUtilization))]),
    format('~n~n').


display_genericfmt_instr_count(GFmt) :-
    (
        bagof(Opcodes, GFmt^(
            genericfmt_opcodes(GFmt, Opcodes),
            labeling([up, bisect], [Opcodes])
        ), Counts) -> true ; throw(error(could_not_solve, _))
    ),
    genericfmt_description(GFmt, Descr),

    aggregate_all(count, (
        isa:fmt_instr(GFmt, Instr),
        dif(Instr, ???)
    ), Assigned),
    ( [Available] = Counts -> true ; throw(error(non_unique_availableopcodecount(GFmt, Counts)))),
    Utilization is 100 * Assigned / Available,
    markdown:emit_table_row([
        sectionlink(fmt('`~k`', GFmt), fmt('Instruction Format `~k`', GFmt)),
        a(Descr),
        d(Available),
        d(Assigned),
        fmt('~0f%', Utilization)
    ]),
end.



display_instruction_listing(Lvl) :-
    markdown:emit_heading(Lvl, 'Instruction Listing'),
    bagof(GFmt, (
        derive:genericfmt(GFmt),
        dif(GFmt, ext)
    ), GFmts),
    maplist([GF0, GFAtom]>>(format(atom(GFAtom), '`~k`', [GF0])), GFmts, GFmtsFormatted),
    markdown:emit_table_header(GFmtsFormatted),

    maplist(
        [GF, Instrs]>>(
            findall(
                sectionlink(fmt('`~w`', Instr), fmt('The `~w` Instruction', Instr)),
                isa:fmt_instr(GF, Instr),
                Instrs
            )
        ),
        GFmts,
        Cols
    ),
    nzip_longest(Cols, Rows, a('')),
    maplist(markdown:emit_table_row, Rows).


display_instr_format_breakdown(Lvl) :-
    markdown:emit_heading(Lvl, 'Instruction Format Breakdown'),
    markdown:emit_heading(s(Lvl), 'Instruction Format Layouts'),
    markdown:emit_table_header([left('Format'), '[Bit Pattern](#legend)', 'Opcodes Available', 'Assigned', 'Utilization', 'Range of Immediate']),
    foreach(
        derive:fmt(Fmt),
        format_section(Fmt)
    ),
    display_bitformat_legend(s(Lvl)),
end.

format_section(Fmt) :-
    foreach(
        derive:fmt_layout(Fmt, Layout),
        format_layout_row(Fmt, Layout)
    ).

format_layout_row(Fmt, Layout) :-
    list_item_occurrances(Layout, i, IBits),
    immbits_immdescription(IBits, ImmDescr),
    derive:validate_instr_assignments_or_throw(Fmt, _Ops),
    derive:fmt_assignedinstrcount(Fmt, AssignedCount, _ReservedCount),
    derive:fmt_maxopcodes(Fmt, MaxAvail),
    UsagePct is 100 * AssignedCount / MaxAvail,
    markdown:emit_table_row([
        sectionlink(fmt('`~k`', Fmt), fmt('Format `~k`', Fmt)),
        code(chars(Layout)),
        d(MaxAvail),
        d(AssignedCount),
        fmt('~0f%', UsagePct),
        a(ImmDescr)
    ]),
end.


display_bitformat_legend(Lvl) :-
    markdown:emit_heading(Lvl, 'Legend'),
    markdown:emit_table_header(['Bit Symbol', left('Description')]),
    foreach(
        bitformatchar_description(Char, Descr),
        markdown:emit_table_row([fmt('`~w`', Char), a(Descr)])
    ),
    format('~n').


bitformatchar_description(o, 'A bit in the instruction''s opcode.').
bitformatchar_description(i, 'A bit in an immediate value.').
bitformatchar_description(r, 'A bit in a register specifier.').
bitformatchar_description(s, 'A bit in a second register specifier.').
bitformatchar_description(t, 'A bit in a third register specifier.').
bitformatchar_description('0', 'A literal `0` embedded in the instruction.').
bitformatchar_description('1', 'A literal `1` embedded in the instruction.').


display_instr_specifications(Lvl) :-
    markdown:emit_heading(Lvl, 'Instruction Specifications'),
    forall(
        ( derive:genericfmt(GFmt), GFmt \= ext ),
        display_instr_specification_under_gfmt(s(Lvl), GFmt)
    ).

display_instr_specification_under_gfmt(Lvl, GFmt) :-
    markdown:emit_heading(Lvl, 'Instruction Format `~k`', [GFmt]),
    markdown:emit_image('../assets/~k.svg', [GFmt]),
    
    GFmt \= ext,
    optree:fmt_tree(GFmt, OpTree), % Format being ext makes fmt_tree throw error about empty pool

    forall(
        isa:fmt_genericfmt(Fmt, GFmt),
        display_instr_specification_under_fmt(s(Lvl), Fmt, OpTree)
    ).

display_instr_specification_under_fmt(Lvl, Fmt, OpTree) :-
    markdown:emit_heading(Lvl, 'Format `~k`', [Fmt]),
    forall(
        isa:fmt_instr(Fmt, Instr),
        display_instr_specification(s(Lvl), Fmt, Instr, OpTree)
    ).

display_instr_specification(Lvl, Fmt, Instr, OpTree) :-
    sem:instr_info(Instr, Info),

    markdown:emit_heading(Lvl, 'The `~w` Instruction', [Instr]),
    format('**~w** --- ~w~n', [Info.title, Info.descr]),

    ( Info.ex = [_|_] ->
        markdown:emit_heading(s(Lvl), 'Examples'),
        maplist(
            [Ex]>>format('- `~w`~n', [Ex]),
            Info.ex
        )
    ;
        true
    ),

    markdown:emit_heading(s(Lvl), 'Layout'),
    once(derive:fmt_prefix(Fmt, Prefix)),
    % bagof(I, Fmt^fmt_instr(Fmt, I), InstrsInFmt),
    % once(nth0(OpcodeIndex, InstrsInFmt, Instr)),
    ( Fmt \= ext ->
        optree:optree_instr_prefix(OpTree, Instr, Opcode)
    ;
        Opcode = 'NONE'
    ),

    ( derive:fmt_operands(Fmt, Operands), member(i, Operands) ->
        MaybeImmRange = ['Immediate Bits', 'Immediate Range']
    ;
        MaybeImmRange = []
    ),

    markdown:emit_table_header(['Format Prefix', 'Opcode', 'Bit Layout' | MaybeImmRange]),

    foreach(
        derive:fmt_layout(Fmt, Layout),
        display_detailed_instr_layout(Fmt, Prefix, Opcode, Layout)
    ),

    markdown:emit_heading(s(Lvl), 'Semantics'),

    format(codes(OperandsCodes), '~p', [Info.operands]),
    length(OperandsCodes, OLen),
    format(codes(SemanticsCodes), '~p', [Info.sem]),
    string_lines(SemanticsCodes, SLines),
    maplist(string_length, SLines, SLens),
    max_member(MaxLen, [OLen | SLens]),

    format('```~n'),
    format('~s~n', [OperandsCodes]),
    format('~`-t~*|~n', [MaxLen]),
    format('~s~n', [SemanticsCodes]),
    format('```~n'),

    format('~n--------------~n').

display_detailed_instr_layout(Fmt, Prefix, Opcode, Layout) :-
    bitlayout_opcodebits(Layout, OBits),
    bitlayout_immbits(Layout, IBits),
    bitlayout_operands(Layout, OperandsBits),
    ( #OBits #> 0 ->
        format(atom(OpcodeBits), '~s', [Opcode])
    ;
        OpcodeBits = ''
    ),
    format(atom(RenderedLayout), '`~s~w~s`', [Prefix, OpcodeBits, OperandsBits]),

    (
        OBits > 0 ->
            format(atom(OpcodeBin), '0b~s', [Opcode])
        ;
            OpcodeBin = 'NONE'
    ),

    ( fmt_operands(Fmt, Operands), member(i, Operands) ->
        immbits_immdescription(IBits, ImmRange),
        MaybeImmRange = [d(IBits), a(ImmRange)]
    ;
        MaybeImmRange = []
    ),

    markdown:emit_table_row([
        fmt('`~q` = 0b~s', Fmt, Prefix),
        a(OpcodeBin),
        fmt('~w', RenderedLayout)
        | MaybeImmRange
    ]),
end.


bitlayout_operands(BitLayout, Operands) :-
    partition([Bit]>>memberchk(Bit, [i, r, 'R']), BitLayout, Operands, _NonOperands).

bitlayout_immbits(BitLayout, Count) :-
    include(=(i), BitLayout, ImmBits),
    length(ImmBits, Count).

bitlayout_opcodebits(BitLayout, Count) :-
    include(=(o), BitLayout, OpcodeBits),
    length(OpcodeBits, Count).


immbits_simmrange(Bits, [Low, High]) :-
    #Bits #> 0,
    #Low #= -1 * 2 ^ (#Bits - 1),
    #High #= 2 ^ (#Bits - 1) - 1.

immbits_immrange(Bits, [0 , High]) :-
    #Bits #> 0,
    #High #= 2 ^ #Bits - 1.

immbits_immdescription(Bits, Descr) :-
    immbits_simmrange(Bits, SimmRange),
    immbits_immrange(Bits, ImmRange) ->
        format(atom(Descr), '`imm~d` in `~p` or `~p`', [Bits, SimmRange, ImmRange])
    ;
        Descr = ''.


genericfmt_description(Fmt, Descr) :-
    isa:fmt_operands_description(Fmt, _, Descr).

fmt_description(Fmt, Descr) :-
    Fmt =.. [_Functor],
    !,
    genericfmt_description(Fmt, Descr).
fmt_description(Fmt, Descr) :-
    Fmt =.. [_Functor, Arg],
    genericfmt_description(Fmt, Descr0),
    format(atom(Descr), '~w #~d', [Descr0, Arg]).

end.