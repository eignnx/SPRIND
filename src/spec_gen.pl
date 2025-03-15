:- module(spec_gen, [show_table/0]).

:- use_module(isa).
:- use_module(utils).
:- use_module(validate).
:- use_module(sem).
:- use_module(derive).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


:- op(20, fx, #).
:- op(50, xfx, ..=).
:- op(500, xfy, ++).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).


show_table :-
    utils:warn_if_nondet(spec_gen:show_table_),
    catch(validate:run_validations, error(E), format('~n> !!! ~w~n~n', [E])),
    true.

show_table_ :-
    emit_heading(1, 'SPRIND Instruction Set Architecture Specification'),
    display_machine_overview,
    display_instructions_spec,
    true.

display_machine_overview :-
    emit_heading(2, 'Machine Overview'),
    display_gprs,
    display_register_uses,
    display_sysregs,

    true.

display_gprs :-
    emit_heading(3, 'General Purpose Registers'),
    emit_table_header(['Register Name', 'Uses']),
    foreach(
        isa:regname_uses(Reg, Uses),
        display_gpr_info(Reg, Uses)
    ).

display_gpr_info(Reg, Uses) :-
    phrase(sequence(atom, `, `, Uses), UsesList),
    emit_table_row([code(a(Reg)), s(UsesList)]).


display_register_uses :-
    emit_heading(3, 'Register Uses and Calling Convention'),
    emit_table_header(['Usage Name', left('Description')]),
    foreach(
        isa:reguse_description(RegUse, Descr),
        emit_table_row([code(fmt('~k', RegUse)), a(Descr)])
    ).

display_sysregs :-
    emit_heading(3, 'System Registers'),
    emit_table_header(['Register', 'Register Name', 'Size', left('Description')]),
    foreach(
        isa:sysregname_name_size_description(Reg, Name, Size, Descr),
        emit_table_row([code(fmt('$~w', Reg)), a(Name), fmt('~d-bits', Size), a(Descr)])
    ).



display_instructions_spec :-
    emit_heading(2, 'Instruction Specifications'),

    emit_heading(3, 'Instruction Counts by Format'),
    display_instruction_counts_by_format,

    emit_heading(3, 'Instruction Listing'),
    display_instruction_listing,

    emit_heading(3, 'Instruction Format Breakdown'),
    display_instr_format_breakdown,
    display_bitformat_legend,

    display_instr_specifications.



display_instr_specifications :-
    emit_heading(3, 'Instruction Specifications'),
    forall(
        fmt(Fmt),
        display_instr_specification_under_fmt(Fmt)
    ).

display_instr_specification_under_fmt(Fmt) :-
    emit_heading(4, 'Instruction Format `~k`', [Fmt]),
    forall(
        fmt_instr(Fmt, Instr),
        display_instr_specification(Fmt, Instr)
    ).

display_instr_specification(Fmt, Instr) :-
    sem:instr_info(Instr, Info),

    emit_heading(5, '`~w`', [Instr]),
    format('**~w** --- ~w~n', [Info.title, Info.descr]),

    ( Info.ex = [_|_] ->
        emit_heading(6, 'Examples'),
        maplist(
            [Ex]>>format('- `~w`~n', [Ex]),
            Info.ex
        )
    ;
        true
    ),

    emit_heading(6, 'Layout'),
    once(derive:fmt_prefix(Fmt, Prefix)),
    bagof(I, Fmt^fmt_instr(Fmt, I), InstrsInFmt),
    once(nth0(OpcodeIndex, InstrsInFmt, Instr)),

    emit_table_header(['Format Prefix', 'Opcode']),

    derive:fmt_opcodebits_immbits(Fmt, OBits, _),
    label([OBits]),
    (
        OBits > 0 ->
            format(atom(OpcodeBin), '0b~|~`0t~2r~*+', [OpcodeIndex, OBits])
        ;
            OpcodeBin = 'NONE'
    ),
    emit_table_row([fmt('`~q`', Fmt)++a(=)++fmt('0b~s', Prefix), a(OpcodeBin)]),

    ( derive:fmt_operands(Fmt, Operands), member(i, Operands) ->
        MaybeImmRange = ['Immediate Bits', 'Immediate Range']
    ;
        MaybeImmRange = []
    ),

    emit_table_header(['Bit Layout' | MaybeImmRange]),

    foreach(
        derive:fmt_layout(Fmt, Layout),
        display_detailed_instr_layout(Fmt, Prefix, OpcodeIndex, Layout)
    ),

    emit_heading(6, 'Semantics'),

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
        format(atom(OpcodeBits), '~`0t~2r~*+', [Opcode, OBits])
    ;
        OpcodeBits = ''
    ),
    format(atom(RenderedLayout), '`~s~w~s`', [Prefix, OpcodeBits, OperandsBits]),

    ( fmt_operands(Fmt, Operands), member(i, Operands) ->
        immbits_immdescription(IBits, ImmRange),
        MaybeImmRange = [d(IBits), a(ImmRange)]
    ;
        MaybeImmRange = []
    ),

    emit_table_row([fmt('~w', RenderedLayout) | MaybeImmRange]).


bitlayout_operands(BitLayout, Operands) :-
    partition([Bit]>>memberchk(Bit, [i, r, 'R']), BitLayout, Operands, _NonOperands).

bitlayout_immbits(BitLayout, Count) :-
    include(=(i), BitLayout, ImmBits),
    length(ImmBits, Count).

bitlayout_opcodebits(BitLayout, Count) :-
    include(=(o), BitLayout, OpcodeBits),
    length(OpcodeBits, Count).

display_instruction_counts_by_format :-
    emit_table_header(['Generic format', left('Description'), 'Available Opcodes', 'Assigned', 'Utilization']),
    foreach(
        genericfmt(GFmt),
        display_genericfmt_instr_count(GFmt)
    ),

    derive:total_opcode_count_minmax(TotalMin, TotalMax),

    ( TotalMin == TotalMax -> TotalAvailable = TotalMin
    ; throw(error(non_unique_total_available_opcode_count(TotalMax, TotalMin)))
    ),

    aggregate_all(count, (
        derive:fmt_instr(_Fmt, Instr),
        dif(Instr, ???)
    ), TotalAssigned),

    TotalUtilization is 100 * TotalAssigned / TotalAvailable,

    emit_table_row([a(''), bold(a('Totals (excluding `ext`)')), bold(d(TotalAvailable)), bold(d(TotalAssigned)), bold(fmt('~0f%', TotalUtilization))]),
    format('~n~n').


display_genericfmt_instr_count(GFmt) :-
    (
        bagof(Opcodes, GFmt^(
            genericfmt_opcodes(GFmt, Opcodes),
            labeling([up, bisect], [Opcodes])
        ), Counts) -> true ; throw(error(could_not_solve))
    ),
    genericfmt_description(GFmt, Descr),

    aggregate_all(count, (
        derive:fmt_instr(GFmt, Instr),
        dif(Instr, ???)
    ), Assigned),
    ( [Available] = Counts -> true ; throw(error(non_unique_availableopcodecount(GFmt, Counts)))),
    Utilization is 100 * Assigned / Available,
    emit_table_row([code(fmt('~k', GFmt)), a(Descr), d(Available), d(Assigned), fmt('~0f%', Utilization)]).


display_instruction_listing :-
    bagof(GFmt, (
        derive:genericfmt(GFmt),
        dif(GFmt, ext)
    ), GFmts),
    maplist([GF0, GFAtom]>>(format(atom(GFAtom), '`~k`', [GF0])), GFmts, GFmtsFormatted),
    emit_table_header(GFmtsFormatted),

    maplist(
        [GF, Instrs]>>(
            findall(
                (fmt('[`~w`]', Instr)+fmt('(#`~w`)', Instr)),
                fmt_instr(GF, Instr),
                Instrs
            )
        ),
        GFmts,
        Cols
    ),
    nzip_longest(Cols, Rows, a('')),
    maplist(emit_table_row, Rows).


display_instr_format_breakdown :-
    emit_heading(4, 'Instruction Format Layouts'),

    emit_table_header([left('Format'), 'Bit Pattern', 'Opcodes Available', 'Assigned', 'Utilization', 'Range of Immediate']),
    foreach(
        derive:fmt(Fmt),
        format_section(Fmt)
    ).

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
    emit_table_row([code(a(Fmt)), code(chars(Layout)), d(MaxAvail), d(AssignedCount), fmt('~0f%', UsagePct), a(ImmDescr)]).


display_bitformat_legend :-
    emit_heading(4, 'Legend'),
    emit_table_header(['Bit Symbol', left('Description')]),
    foreach(
        bitformatchar_description(Char, Descr),
        emit_table_row([fmt('`~w`', Char), a(Descr)])
    ),
    format('~n').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


bitformatchar_description(o, 'A bit in the instruction''s opcode.').
bitformatchar_description(i, 'A bit in an immediate value.').
bitformatchar_description(r, 'A bit in a register specifier.').
bitformatchar_description(s, 'A bit in a second register specifier.').
bitformatchar_description(t, 'A bit in a third register specifier.').
bitformatchar_description('0', 'A literal `0` embedded in the instruction.').
bitformatchar_description('1', 'A literal `1` embedded in the instruction.').

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emit_heading(Level, Content) :-
    emit_heading(Level, '~w', [Content]).
emit_heading(Level, FString, FParams) :-
    item_count_replication('#', Level, Hashes),
    format(atom(Content), FString, FParams),
    format('~n~s ~w~n~n', [Hashes, Content]).

emit_table_header(ColSpecs) :-
    maplist([Spec, Name, Align]>>(
        Spec = center(Name)  -> Align = ':---:'
        ; Spec = left(Name)  -> Align = ':----'
        ; Spec = right(Name) -> Align = '----:'
        ; Name = Spec, Align = ':---:'
    ), ColSpecs, ColNames, ColAligns),
    phrase(sequence(`| `, atom, ` | `, ` |`, ColNames), ColNamesRow),
    phrase(sequence(`|`, atom, `|`, `|`, ColAligns), HorizontalRule),
    format('~n'),
    format('~s~n', [ColNamesRow]),
    format('~s~n', [HorizontalRule]).

emit_table_row(ColumnData) :-
    phrase(sequence(`| `, variant_type, ` | `, ` |`, ColumnData), RowText),
    format('~s~n', [RowText]).

variant_type(s(Content)) --> !, string(Content).
variant_type(chars(Content)) --> !, { format(codes(Codes), '~s', [Content]) }, Codes.
variant_type(a(Content)) --> !, atom(Content).
variant_type(d(Content)) --> !, integer(Content).
variant_type(code(Inner)) --> !, `\``, variant_type(Inner), `\``.
variant_type(bold(Inner)) --> !, `**`, variant_type(Inner), `**`.
variant_type(fmt(FormatString, Content)) --> !,
    { format(atom(Formatted), FormatString, [Content]) },
    atom(Formatted).
variant_type(First + Second) --> !, variant_type(First), variant_type(Second).
variant_type(First ++ Second) --> !, variant_type(First), ` `, variant_type(Second).
variant_type(Other) --> { throw(error(unknown_format_command(Other))) }.