:- module(spec_gen, [show_table/0]).

:- use_module(isa).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(20, fx, #).
:- op(50, xfx, ..=).
:- op(500, xfy, ++).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).
                              

fmt_operands(Fmt, Operands) :-
    isa:fmt_operands_description(Fmt, Operands, _).

genericfmt_description(Fmt, Descr) :-
    isa:fmt_operands_description(Fmt, _, Descr).



fmt_instr_title(Fmt, Instr, Title) :- isa:fmt_instr_title_description(Fmt, Instr, Title, _).
fmt_instr(Fmt, Instr) :- fmt_instr_title(Fmt, Instr, _).

fmt_assignedinstrcount(Fmt, AssignedCount) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, _ReservedCount).
fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount) :-
    aggregate_all(count, (
            fmt_instr(Fmt, Instr),
            dif(Instr, ???) % For spacing out instructions in opcode space.
        ),
        AssignedCount
    ),
    aggregate_all(count, (
            fmt_instr(Fmt, ???) % For spacing out instructions in opcode space.
        ),
        ReservedCount
    ).

validate_instr_assignments(Fmt, OpcodeCount) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount),
    #OpcodeCount #>= #AssignedCount + #ReservedCount.

fmt_description(Fmt, Descr) :-
    Fmt =.. [_Functor],
    !,
    genericfmt_description(Fmt, Descr).
fmt_description(Fmt, Descr) :-
    Fmt =.. [_Functor, Arg],
    genericfmt_description(Fmt, Descr0),
    format(atom(Descr), '~w #~d', [Descr0, Arg]).

tree_leaf(Tree, Leaf) :-
    Tree = [Left | Right] ->
        ( tree_leaf(Left, Leaf) ; tree_leaf(Right, Leaf) )
    ;
        Leaf = Tree.

:- 
    retractall(fmt(_)),
    fmt_huffman_enc(Tree),
    foreach(
        tree_leaf(Tree, Leaf),
        assertz(fmt(Leaf))
    ).

:-
    retractall(genericfmt(_)),
    bagof(Fmt, fmt(Fmt), Fmts),
    maplist([Fmt, Functor-Arity]>>(
        Fmt =.. [Functor | Args],
        length(Args, Arity)
    ), Fmts, FunctorsAritiesDup),
    list_to_set(FunctorsAritiesDup, FunctorsArities),
    maplist([Functor-Arity, GFmt]>>(
        length(FreeArgs, Arity),
        GFmt =.. [Functor | FreeArgs]
    ), FunctorsArities, GFmts),
    maplist([GFmt]>>assertz(genericfmt(GFmt)), GFmts).


fmt_prefix(Fmt, Prefix) :-
    fmt_huffman_enc(Tree),
    huffmantree_item_prefix(Tree, Fmt, Prefix).


operand_size(r, Bits) :- isa:gpr_count_bits(Bits).
operand_size('R', Bits) :- isa:gpr_count_bits(Bits).
operand_size(a, Bits) :- isa:addr_reg_count_bits(Bits).
operand_size(i, Size) :- isa:register_size(RegBits), Size in 0 .. RegBits.


fmt_opcodebits_immbits(Fmt, OpcodeBits, ImmBits) :-
    isa:instr_size(InstrSize),
    OpcodeBits in 0 .. InstrSize,
    ImmBits in 0 .. InstrSize,
    fmt_prefix(Fmt, Prefix),
    length(Prefix, PrefixLen),
    fmt_operands(Fmt, Operands),
    maplist([O, S, O-S]>>operand_size(O, S), Operands, Sizes, OperandsSizes),
    list_to_assoc(OperandsSizes, Assoc),
    ( get_assoc(i, Assoc, ImmBits) -> true ; ImmBits = 0 ),
    ( isa:fmt_immsizeconstraint(Fmt, ImmBits) -> true ; true ),
    ( isa:fmt_opcodesizeconstraint(Fmt, OpcodeBits) -> true ; true ),
    sum(Sizes, #=, #OperandsTotalSize),
    #PrefixLen + #OpcodeBits + #OperandsTotalSize #= InstrSize.


fmt_maxopcodes(Fmt, MaxOpcodes) :-
    fmt_opcodebits_immbits(Fmt, OpcodeBits, _),
    #MaxOpcodes #= 2 ^ #OpcodeBits,
    once(labeling([bisect, max(MaxOpcodes)], [MaxOpcodes])).


fmt_layout(Fmt, Layout) :-
    fmt_operands(Fmt, Operands),
    fmt_opcodebits_immbits(Fmt, OBits, IBits),
    label([OBits, IBits]),
    maplist({IBits}/[Operand, OperandReplicated]>>(
        ( Operand = i -> Count = IBits ; operand_size(Operand, Count) ),
        item_count_replication(Operand, Count, OperandReplicated)
    ), Operands, OperandBits),
    fmt_prefix(Fmt, Prefix),
    item_count_replication(o, OBits, Opcode),
    append([Prefix, Opcode | OperandBits], Layout),
    true.


genericfmt_opcodes(GFmt, Opcodes) :-
    bagof(OBits, GFmt^fmt_opcodebits_immbits(GFmt, OBits, _), OBitRows),
    maplist([Bits, Count]>>(#Count #= 2 ^ #Bits), OBitRows, OpcodeCounts),
    sum(OpcodeCounts, #=, Opcodes).


total_opcode_count(Count) :-
    total_opcode_count(Count, _Counts).
total_opcode_count(Count, TotalCounts) :-
    bagof(GFmt, (genericfmt(GFmt), dif(GFmt, ext)), GFmts),
    maplist([GFmt, Ops]>>genericfmt_opcodes(GFmt, Ops), GFmts, TotalCounts),
    sum(TotalCounts, #=, Count).

total_opcode_count_minmax(MinCount, MaxCount) :-
    total_opcode_count_minmax(MinCount, MaxCount, bisect).
total_opcode_count_minmax(MinCount, MaxCount, BranchingStrat) :-
    total_opcode_count(MinCount, TotalCounts0),
    once(labeling([BranchingStrat, min(MinCount)], TotalCounts0)),
    total_opcode_count(MaxCount, TotalCounts1),
    once(labeling([BranchingStrat, max(MaxCount)], TotalCounts1)).


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
        format(atom(Descr), 'imm~d in ~p or ~p', [Bits, SimmRange, ImmRange])
    ;
        Descr = ''.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


show_table :-
    warn_if_nondet(show_table_).

show_table_ :-
    emit_heading(1, 'SPRIND Instruction Set Architecture Specification'),
    display_machine_overview,
    display_instructions_spec,
    true.

display_machine_overview :-
    emit_heading(2, 'Machine Overview'),
    display_gprs,

    display_register_uses,

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



display_instructions_spec :-
    emit_heading(2, 'Instruction Specifications'),

    emit_heading(3, 'Instruction Counts by Format'),
    display_instruction_counts_by_format,

    emit_heading(3, 'Format Assignment Availability'),
    display_opcode_availability_by_format,

    emit_heading(3, 'Instruction Format Breakdown'),
    display_bitformat_legend,
    display_instr_format_breakdown,

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
        fmt_instr_title_description(Fmt, Instr, Title, Descr),
        display_instr_specification(Fmt, Instr, Title, Descr)
    ).

display_instr_specification(Fmt, Instr, Title, Descr) :-
    emit_heading(5, '~w - `~w`', [Title, Instr]),
    format('~w~n', [Descr]),

    emit_heading(6, 'Layout'),
    once(fmt_prefix(Fmt, Prefix)),
    bagof(I, Fmt^fmt_instr(Fmt, I), InstrsInFmt),
    once(nth0(OpcodeIndex, InstrsInFmt, Instr)),

    emit_table_header(['Format Prefix', 'Opcode']),
    emit_table_row([fmt('`~q`', Fmt)++a(=)++fmt('0b~s', Prefix), fmt('0x~16R', OpcodeIndex)]),

    ( fmt_operands(Fmt, Operands), member(i, Operands) ->
        MaybeImmRange = ['Immediate Bits', 'Immediate Range']
    ;
        MaybeImmRange = []
    ),

    emit_table_header(['Bit Layout' | MaybeImmRange]),

    foreach(
        fmt_layout(Fmt, Layout),
        display_detailed_instr_layout(Fmt, Prefix, OpcodeIndex, Layout)
    ),

    true.
    % emit_heading(6, 'Operation'),
    % Operation = "RD <- RD + RS;\c
    %              RS <- RS + 1;",
    % format('```~n'),
    % format('~s~n', [Operation]),
    % format('```~n').

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

display_opcode_availability_by_format :-
    emit_table_header(['Format', 'Max Opcodes Available', 'Opcodes Assigned', 'Opcodes Reserved']),
    foreach(
        fmt(Fmt),
        display_opcode_availability(Fmt)
    ).

display_opcode_availability(Fmt) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount),
    fmt_maxopcodes(Fmt, MaxAvail),
    emit_table_row([fmt('`~k`', Fmt), d(MaxAvail), d(AssignedCount), d(ReservedCount)]).

display_instruction_counts_by_format :-
    emit_table_header(['Generic format', left('Description'), 'Instr. Count']),
    foreach(
        genericfmt(GFmt),
        display_genericfmt_instr_count(GFmt)
    ),

    total_opcode_count_minmax(TotalMin, TotalMax),
    format('~n~n'),
    format(
        'Total instructions available (excluding `ext`): ~d (min), ~d (max)~n',
        [TotalMin, TotalMax]
    ),
    format('~n').


display_genericfmt_instr_count(GFmt) :-
    bagof(Opcodes, GFmt^(
        genericfmt_opcodes(GFmt, Opcodes),
        labeling([up, bisect], [Opcodes])
    ), Counts),
    phrase(sequence(integer, `, `, Counts), CountsList),
    genericfmt_description(GFmt, Descr),

    emit_table_row([code(fmt('~k', GFmt)), a(Descr), s(CountsList)]).


display_instr_format_breakdown :-
    emit_heading(4, 'Instruction Format Layouts'),

    emit_table_header([left('Format'), 'Bit Pattern', '\\# Opcodes', 'Range of Immediate']),
    foreach(
        fmt(Fmt),
        format_section(Fmt)
    ).

format_section(Fmt) :-
    foreach(
        fmt_layout(Fmt, Layout),
        format_layout_row(Fmt, Layout)
    ).

format_layout_row(Fmt, Layout) :-
    list_item_occurrances(Layout, o, OBits),
    list_item_occurrances(Layout, i, IBits),
    immbits_immdescription(IBits, ImmDescr),
    #Ops #= 2 ^ #OBits,
    validate_instr_assignments(Fmt, Ops),
    emit_table_row([code(a(Fmt)), code(chars(Layout)), d(Ops), a(ImmDescr)]).


display_bitformat_legend :-
    emit_heading(4, 'Legend'),
    emit_table_header(['Bit Symbol', left('Description')]),
    foreach(
        bitformatchar_description(Char, Descr),
        emit_table_row([fmt('`~w`', Char), a(Descr)])
    ),
    format('~n').

bitformatchar_description(o, 'A bit in the instruction''s opcode.').
bitformatchar_description(i, 'A bit in an immediate value.').
bitformatchar_description(r, 'A bit in a register specifier.').
bitformatchar_description('R', 'A bit in a second register specifier.').
bitformatchar_description(a, 'A bit in an address register specifier.').
bitformatchar_description('0', 'A literal `0` embedded in the instruction.').
bitformatchar_description('1', 'A literal `1` embedded in the instruction.').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


huffmantree_item_prefix(Fmt, Fmt, []).
huffmantree_item_prefix([Left | _Right], Fmt, ['0' | Prefix]) :-
    huffmantree_item_prefix(Left, Fmt, Prefix).
huffmantree_item_prefix([_Left | Right], Fmt, ['1' | Prefix]) :-
    huffmantree_item_prefix(Right, Fmt, Prefix).


item_count_replication(Item, Count, Replication) :-
    length(Replication, Count),
    maplist(=(Item), Replication).


list_item_occurrances([], _, 0).
list_item_occurrances([X | Xs], Y, N) :-
    dif(X, Y) ->
        list_item_occurrances(Xs, Y, N)
    ;
        list_item_occurrances(Xs, Y, M),
        #N #= #M + 1.

warn_if_nondet(Goal) :-
    aggregate(count, Goal, Count),
    Count > 1 -> throw(error(redundant_choicepoint_for_goal(Goal))) ; true.

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

variant_type(s(Content)) --> string(Content).
variant_type(chars(Content)) --> { format(codes(Codes), '~s', [Content]) }, Codes.
variant_type(a(Content)) --> atom(Content).
variant_type(d(Content)) --> integer(Content).
variant_type(code(Inner)) --> `\``, variant_type(Inner), `\``.
variant_type(fmt(FormatString, Content)) -->
    { format(atom(Formatted), FormatString, [Content]) },
    atom(Formatted).
variant_type(First + Second) --> variant_type(First), variant_type(Second).
variant_type(First ++ Second) --> variant_type(First), ` `, variant_type(Second).