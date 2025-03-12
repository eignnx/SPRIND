:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(20, fx, #).
:- op(50, xfx, ..=).
:- op(500, xfy, ++).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- det(fmt_operands/2).

fmt_operands(lsd, [i, a, r]).
fmt_operands(subr, [i]).
fmt_operands(li, [i, r]).
fmt_operands(b, [i]).
fmt_operands(ri(_), [i, r]).
fmt_operands(rr(_), ['R', r]).
fmt_operands(r(_), [r]).
fmt_operands(o, []).
fmt_operands(ext, []).

:- det(genericfmt_description/2).

genericfmt_description(lsd,  'Load-store with Displacement').
genericfmt_description(subr,  'Subroutine Call').
genericfmt_description(li,    'Load Immediate').
genericfmt_description(b,     'Branch').
genericfmt_description(lsx,   'Load-store with Index').
genericfmt_description(rr(_), 'Register-register').
genericfmt_description(ri(_), 'Register-immediate').
genericfmt_description(r(_),  'Register').
genericfmt_description(o,     'Opcode').
genericfmt_description(ext,   'Reserved for Extension').

% A cons-cell based tree representation of the encoding space. The program uses
% this tree to construct prefix codes for the different instruction formats.
%
% The tree `(a + ((b + c) + d))` would be represented as `[a | [[b | c] | d]]`.
%
% See: https://en.wikipedia.org/wiki/Huffman_coding
huffman_enc([
    [
        lsd
    |
        [
            subr
        |
            [
                b
            |
                ext
            ]
        ]
    ]
|
    % Note: `[a | [b | [c | d]]] == [a, b, c | d]`
    [
        li,
        ri(1),
        ri(2),
        ri(3),
        rr(1),
        rr(2),
        rr(3),
        r(1),
        r(2),
        r(3)
    |
        o
    ]
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Constraints %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_immsizeconstraint(lsd, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(subr, Bits) :- addrsize_maxalignment(Bits, 32).
fmt_immsizeconstraint(b, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(li, Bits) :- #Bits #>= 8.
fmt_immsizeconstraint(ri(_), Bits) :- #Bits #= 6. % #Bits #>= 4. %

fmt_opcodesizeconstraint(lsd, Bits) :- 2 ^ #Bits #>= 4.
fmt_opcodesizeconstraint(b, Bits) :- #Bits #>= 1, 2 ^ #Bits #=< 16.
fmt_opcodesizeconstraint(subr, Bits) :- 2 ^ #Bits #= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%% Instruction Assignments %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_instr_title(lsd, lb, 'Load Byte').
fmt_instr_title(lsd, lw, 'Load Word').
fmt_instr_title(lsd, sb, 'Store Byte').
fmt_instr_title(lsd, sw, 'Store Word').


fmt_instr_title(subr, call, 'Call Subroutine').


fmt_instr_title(b, b, 'Branch').
fmt_instr_title(b, bt, 'Branch If True').
fmt_instr_title(b, bf, 'Branch If False').


fmt_instr_title(li, li, 'Load Immediate').
fmt_instr_title(li, szi, 'Shift Zero-extended Immediate').


fmt_instr_title(ri(1), lgb, 'Load Global Byte').
fmt_instr_title(ri(1), lgw, 'Load Global Word').
fmt_instr_title(ri(1), sgb, 'Store Global Byte').
fmt_instr_title(ri(1), sgw, 'Store Global Word').
fmt_instr_title(ri(1), tbit, 'Test Bit').
fmt_instr_title(ri(1), cbit, 'Clear Bit').
fmt_instr_title(ri(1), sbit, 'Set Bit').
fmt_instr_title(ri(1), tli, 'Test Less-than Immediate').
fmt_instr_title(ri(1), tgei, 'Test Greater-than or Equal Immediate').
fmt_instr_title(ri(1), tbi, 'Test Below Immediate').
fmt_instr_title(ri(1), taei, 'Test Above or Equal').
fmt_instr_title(ri(1), tnei, 'Test Not Equal Immediate').
fmt_instr_title(ri(1), teqi, 'Test Equal Immediate').
fmt_instr_title(ri(1), addi, 'Add Immediate').
fmt_instr_title(ri(1), andi, 'AND Immediate').
fmt_instr_title(ri(1), ori, 'OR Immediate').

fmt_instr_title(ri(2), xori, 'XOR Immediate').
fmt_instr_title(ri(2), lsri, 'Logical Shift Right Immediate').
fmt_instr_title(ri(2), lsli, 'Logical Shift Left Immediate').
fmt_instr_title(ri(2), asri, 'Arithmetic Shift Right Immediate').


fmt_instr_title(rr(1), add, 'Add').
fmt_instr_title(rr(1), sub, 'Subtract').
fmt_instr_title(rr(1), and, 'AND').
fmt_instr_title(rr(1), or, 'OR').
fmt_instr_title(rr(1), xor, 'XOR').
fmt_instr_title(rr(1), mov, 'Move').
fmt_instr_title(rr(1), addcy, 'Add with Carry').
fmt_instr_title(rr(1), subcy, 'Subtract with Carry').
fmt_instr_title(rr(1), tl, 'Test Less-than').
fmt_instr_title(rr(1), tge, 'Test Greater-than or Equal').
fmt_instr_title(rr(1), tb, 'Test Below').
fmt_instr_title(rr(1), tae, 'Test Above or Equal').
fmt_instr_title(rr(1), tne, 'Test Not Equal').
fmt_instr_title(rr(1), teq, 'Test Equal').


fmt_instr_title(r(1), pushb, 'Push Byte').
fmt_instr_title(r(1), pushw, 'Push Word').
fmt_instr_title(r(1), popb, 'Pop Byte').
fmt_instr_title(r(1), popw, 'Pop Word').
fmt_instr_title(r(1), callr, 'Call Register').
fmt_instr_title(r(1), jr, 'Jump Register').
fmt_instr_title(r(1), neg, 'Negate').
fmt_instr_title(r(1), seb, 'Sign Extend Byte').
fmt_instr_title(r(1), 'r.hi', 'Read $HI').
fmt_instr_title(r(1), 'r.gp', 'Read $GP').
fmt_instr_title(r(1), 'w.gp', 'Write $GP').


fmt_instr_title(o, 'NONEXE1', 'Non-executable (1''s Version)').
fmt_instr_title(o, 'BREAK', 'Breakpoint').
fmt_instr_title(o, 'HALT', 'Halt').
fmt_instr_title(o, 'UNIMPL', 'Unimplemented').
fmt_instr_title(o, kret, 'Kernel Return').
fmt_instr_title(o, kcall, 'Kernel Call').
fmt_instr_title(o, ret, 'Return').
fmt_instr_title(o, tov, 'Test Overflow').
fmt_instr_title(o, tcy, 'Test Carry').
fmt_instr_title(o, cy0, 'Clear Carry').
fmt_instr_title(o, cy1, 'Set Carry').
fmt_instr_title(o, tpush0, 'Teststack Push 0').
fmt_instr_title(o, tpush1, 'Teststack Push 1').
fmt_instr_title(o, tnot, 'Teststack NOT').
fmt_instr_title(o, tand, 'Teststack AND').
fmt_instr_title(o, tor, 'Teststack OR').
fmt_instr_title(o, tdup, 'Teststack Duplicate').
fmt_instr_title(o, 'prsv.hi', 'Preserve $HI').
fmt_instr_title(o, 'rstr.hi', 'Restore $HI').
fmt_instr_title(o, 'prsv.ts', 'Preserve $TS').
fmt_instr_title(o, 'rstr.ts', 'Restore $TS').
fmt_instr_title(o, 'prsv.ra', 'Preserve $RA').
fmt_instr_title(o, 'rstr.ra', 'Restore $RA').
fmt_instr_title(o, 'prsv.gp', 'Preserve $GP').
fmt_instr_title(o, 'rstr.gp', 'Restore $GP').
fmt_instr_title(o, 'prsv.cc', 'Preserve $CC').
fmt_instr_title(o, 'rstr.cc', 'Restore $CC').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_size(16).
gpr_count_bits(3).
addr_reg_count_bits(2).

regid_name_uses(sp, [stack_ptr, addr]).
regid_name_uses(x,  [temp, arg(1), addr]).
regid_name_uses(y,  [temp, arg(2), addr]).
regid_name_uses(z,  [temp, arg(3), addr]).
regid_name_uses(w,  [temp, arg(4)]).
regid_name_uses(v,  [temp, retval]).
regid_name_uses(a,  [saved]).
regid_name_uses(b,  [saved]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    huffman_enc(Tree),
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


addrsize_maxalignment(Bits, MaxAlign) :-
    2 ^ #Bits #>= 2^16 div #MaxAlign.

fmt_prefix(Fmt, Prefix) :-
    huffman_enc(Tree),
    huffmantree_item_prefix(Tree, Fmt, Prefix).


operand_size(r, Bits) :- gpr_count_bits(Bits).
operand_size('R', Bits) :- gpr_count_bits(Bits).
operand_size(a, Bits) :- addr_reg_count_bits(Bits).
operand_size(i, Size) :- Size in 0 .. 16.


fmt_opcodebits_immbits(Fmt, OpcodeBits, ImmBits) :-
    OpcodeBits in 0 .. 16,
    ImmBits in 0 .. 16,
    fmt_prefix(Fmt, Prefix),
    length(Prefix, PrefixLen),
    fmt_operands(Fmt, Operands),
    maplist([O, S, O-S]>>operand_size(O, S), Operands, Sizes, OperandsSizes),
    list_to_assoc(OperandsSizes, Assoc),
    ( get_assoc(i, Assoc, ImmBits) -> true ; ImmBits = 0 ),
    ( fmt_immsizeconstraint(Fmt, ImmBits) -> true ; true ),
    ( fmt_opcodesizeconstraint(Fmt, OpcodeBits) -> true ; true ),
    sum(Sizes, #=, #OperandsTotalSize),
    #PrefixLen + #OpcodeBits + #OperandsTotalSize #= 16.


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


immbits_simmrange(Bits, (Low ..= High)) :-
    #Bits #> 0,
    #Low #= -1 * 2 ^ (#Bits - 1),
    #High #= 2 ^ (#Bits - 1) - 1.

immbits_immrange(Bits, (0 ..= High)) :-
    #Bits #> 0,
    #High #= 2 ^ #Bits - 1.

immbits_immdescription(Bits, Descr) :-
    immbits_simmrange(Bits, SimmRange),
    immbits_immrange(Bits, ImmRange) ->
        format(atom(Descr), 'imm~d in ~q or ~q', [Bits, SimmRange, ImmRange])
    ;
        Descr = ''.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


show_table :-
    warn_if_nondet(show_table_).

show_table_ :-
    emit_heading(3, 'Instruction Counts by Format'),
    display_instruction_counts_by_format,

    emit_heading(3, 'Format Assignment Availability'),
    display_opcode_availability_by_format,

    emit_heading(3, 'Instruction Format Breakdown'),
    display_bitformat_legend,
    display_instr_format_breakdown,

    emit_heading(3, 'Instruction Specifications'),
    display_instr_specifications,

    true.


display_instr_specifications :-
    forall(
        fmt_instr_title(Fmt, Instr, Title),
        display_instr_specification(Fmt, Instr, Title)
    ).

display_instr_specification(Fmt, Instr, Title) :-
    emit_heading(4, '`~w` - ~w', [Instr, Title]),
    % format('Description of the instruction goes here. Expand on title to give more information.~n~n'),

    emit_heading(5, 'Layout'),
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
    % emit_heading(5, 'Operation'),
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
    emit_table_header(['Generic format', left('Description'), right('Instr. Count Options')]),
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

    format(
        'Consequtive rows with the same format represent alternative 
        representations. For example if format `xyz` has two rows in the table 
        then the constraints are not strict enough find a unique layout for 
        `xyz`.~n'
    ),

    emit_table_header([left('Format'), 'Bit Pattern', '# Opcodes', 'Range of Immediate', 'Too Many Instr.s Assigned?']),
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
    ( \+ validate_instr_assignments(Fmt, Ops) ->
        TooManyInstrs = 'X'
    ;
        TooManyInstrs = ''
    ),
    emit_table_row([code(a(Fmt)), code(chars(Layout)), fmt('~d opcode(s)', Ops), a(ImmDescr), a(TooManyInstrs)]).


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
