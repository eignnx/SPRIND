:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(20, fx, #).
:- op(50, xfx, ..=).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER EDITABLE SECTION %%%%%%%%%%%%%%%%%%%%%

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

:- det(fmt_description/2).

fmt_description(lsd,  'Load-store with Displacement').
fmt_description(subr,  'Subroutine Call').
fmt_description(li,    'Load Immediate').
fmt_description(b,     'Branch').
fmt_description(lsx,   'Load-store with Index').
fmt_description(rr(N), Descr) :- format(atom(Descr), 'Register-register ~d', [N]).
fmt_description(ri(N), Descr) :- format(atom(Descr), 'Register-immediate ~d', [N]).
fmt_description(r(N),  Descr) :- format(atom(Descr), 'Register ~d', [N]).
fmt_description(o,     'Opcode').
fmt_description(ext,   'Reserved for Extension').

huffman_enc([
    [
        lsd
    |
        [
            subr
        |
            [
                ext
            |
                b
            ]
        ]
    ]
|
    [
        li,
        ri(1),
        ri(2),
        % ri(3),
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

fmt_immsizeconstraint(lsd, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(subr, Bits) :- addrsize_maxalignment(Bits, 32).
fmt_immsizeconstraint(b, Bits) :- #Bits #>= 6.
fmt_immsizeconstraint(li, Bits) :- #Bits #>= 8.
fmt_immsizeconstraint(ri(_), Bits) :- #Bits #= 5. % #Bits #>= 4. %

fmt_opcodesizeconstraint(lsd, Bits) :- 2 ^ #Bits #>= 4.
fmt_opcodesizeconstraint(b, Bits) :- #Bits #>= 1, 2 ^ #Bits #=< 16.
fmt_opcodesizeconstraint(subr, Bits) :- 2 ^ #Bits #= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

huffmantree_item_prefix(Fmt, Fmt, []).
huffmantree_item_prefix([Left | _Right], Fmt, ['1' | Prefix]) :-
    huffmantree_item_prefix(Left, Fmt, Prefix).
huffmantree_item_prefix([_Left | Right], Fmt, ['0' | Prefix]) :-
    huffmantree_item_prefix(Right, Fmt, Prefix).

fmt_prefix(Fmt, Prefix) :-
    huffman_enc(Tree),
    huffmantree_item_prefix(Tree, Fmt, Prefix).


operand_size(r, 3).
operand_size('R', 3).
operand_size(a, 2).
operand_size(i, Size) :- Size in 0 .. 16.


:- table fmt_opcodebits_immbits/3.

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


:- table fmt_layout/2.

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


:- table genericfmt_opcodes/2.

genericfmt_opcodes(GFmt, Opcodes) :-
    bagof(OBits, GFmt^fmt_opcodebits_immbits(GFmt, OBits, _), OBitRows),
    maplist([Bits, Count]>>(#Count #= 2 ^ #Bits), OBitRows, OpcodeCounts),
    sum(OpcodeCounts, #=, Opcodes).


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
        format(atom(Descr), 'imm(~d) in ~q \\/ ~q', [Bits, SimmRange, ImmRange])
    ;
        Descr = ''.


show_table :-
    format('~n~n~`=t Instruction Counts by Format ~`=t~80|~n~n'),
    format('|~`-t~20||~`-t~80||~n'),
    format('|~t Generic Format ~t~20||~t Instr Count Possibilities  ~t~80||~n'),
    format('|~`-t~20||~`-t~80||~n'),
    foreach(
        genericfmt(GFmt),
        display_genericfmt_instr_count(GFmt)
    ),
    format('|~`-t~80||~n'),

    format('~n~n~`=t Instruction Format Breakdown ~`=t~80|~n~n'),
    display_bitformat_legend,
    foreach(
        fmt(Fmt),
        format_section(Fmt)
    ),
    format('~`-t~80|~n~n'),
    true.

display_genericfmt_instr_count(GFmt) :-
    bagof(Opcodes, GFmt^(
        genericfmt_opcodes(GFmt, Opcodes),
        labeling([down], [Opcodes])
    ), DescCounts),
    sort(DescCounts, Counts),
    phrase(sequence(integer, `, `, Counts), CountsList),
    format('|~t~8|~k  ~`.t~20|~`.t  ~s~72|~t~80||~n', [GFmt, CountsList]).

format_section(Fmt) :-
    fmt_description(Fmt, Descr),
    format('|~`-t ~w ~`-t|~80|~n', [Descr]),
    format('|~80|~n'),
    foreach(
        fmt_layout(Fmt, Layout),
        format_layout_row(Fmt, Layout)
    ),
    format('|~80|~n').

format_layout_row(Fmt, Layout) :-
    list_item_occurrances(Layout, o, OBits),
    #Ops #= 2 ^ OBits,
    list_item_occurrances(Layout, i, IBits),
    immbits_immdescription(IBits, ImmDescr),
    format(
        '|~|~t~k~t~8|~s~+~t~d opcode(s)~16+~t~w~t|~40+~n',
        [Fmt, Layout, Ops, ImmDescr]
    ).

bitformatchar_description('0, 1', 'A literal `0` or `1` embedded in the instruction.').
bitformatchar_description(o, 'A bit in the instruction''s opcode.').
bitformatchar_description(i, 'A bit in an immediate value.').
bitformatchar_description(r, 'A bit in a register specifier.').
bitformatchar_description('R', 'A bit in a second register specifier.').

display_bitformat_legend :-
    format('|~`-t~80||~n'),
    format('|~tLegend~t~80||~n'),
    format('|~`-t~15||~`-t~80||~n'),
    format('|~tBit Symbol~t~15||~tDescription~t~80||~n'),
    format('|~`-t~15||~`-t~80||~n'),
    foreach(
        bitformatchar_description(Char, Descr),
        format('|~t~w~t~15||~t~w~t~80||~n', [Char, Descr])
    ),
    format('|~`-t~80||~n'),
    format('~n').