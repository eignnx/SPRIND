:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(20, fx, #).
:- op(50, xfx, ..=).


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

fmt_instr(lsd, lb).
fmt_instr(lsd, lw).
fmt_instr(lsd, sb).
fmt_instr(lsd, sw).


fmt_instr(subr, call).


fmt_instr(b, b).
fmt_instr(b, bt).
fmt_instr(b, bf).


fmt_instr(li, li).
fmt_instr(li, szi).


fmt_instr(ri(1), lgb).
fmt_instr(ri(1), lgw).
fmt_instr(ri(1), sgb).
fmt_instr(ri(1), sgw).
fmt_instr(ri(1), tbit).
fmt_instr(ri(1), cbit).
fmt_instr(ri(1), sbit).
fmt_instr(ri(1), tli).
fmt_instr(ri(1), tgei).
fmt_instr(ri(1), tbi).
fmt_instr(ri(1), taei).
fmt_instr(ri(1), tnei).
fmt_instr(ri(1), teqi).
fmt_instr(ri(1), addi).
fmt_instr(ri(1), andi).
fmt_instr(ri(1), ori).

fmt_instr(ri(2), xori).
fmt_instr(ri(2), lsri).
fmt_instr(ri(2), lsli).
fmt_instr(ri(2), asri).


fmt_instr(rr(1), add).
fmt_instr(rr(1), sub).
fmt_instr(rr(1), and).
fmt_instr(rr(1), or).
fmt_instr(rr(1), xor).
fmt_instr(rr(1), mov).
fmt_instr(rr(1), addcy).
fmt_instr(rr(1), subcy).
fmt_instr(rr(1), tl).
fmt_instr(rr(1), tge).
fmt_instr(rr(1), tb).
fmt_instr(rr(1), tae).
fmt_instr(rr(1), tne).
fmt_instr(rr(1), teq).


fmt_instr(r(1), pushb).
fmt_instr(r(1), pushw).
fmt_instr(r(1), popb).
fmt_instr(r(1), popw).
fmt_instr(r(1), callr).
fmt_instr(r(1), jr).
fmt_instr(r(1), neg).
fmt_instr(r(1), seb).
fmt_instr(r(1), 'r.hi').
fmt_instr(r(1), 'r.gp').
fmt_instr(r(1), 'w.gp').


fmt_instr(o, 'NONEXE1').
fmt_instr(o, 'BREAK').
fmt_instr(o, 'HALT').
fmt_instr(o, 'UNIMPL').
fmt_instr(o, kret).
fmt_instr(o, kcall).
fmt_instr(o, ret).
fmt_instr(o, tov).
fmt_instr(o, tcy).
fmt_instr(o, cy0).
fmt_instr(o, cy1).
fmt_instr(o, tpush0).
fmt_instr(o, tpush1).
fmt_instr(o, tnot).
fmt_instr(o, tand).
fmt_instr(o, tor).
fmt_instr(o, tdup).
fmt_instr(o, 'prsv.hi').
fmt_instr(o, 'rstr.hi').
fmt_instr(o, 'prsv.ts').
fmt_instr(o, 'rstr.ts').
fmt_instr(o, 'prsv.ra').
fmt_instr(o, 'rstr.ra').
fmt_instr(o, 'prsv.gp').
fmt_instr(o, 'rstr.gp').
fmt_instr(o, 'prsv.cc').
fmt_instr(o, 'rstr.cc').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Registers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_size(16).
gpr_count_bits(3).
addr_reg_count_bits(2).

regid_name_uses(0, sp, [stack_ptr, addr]).
regid_name_uses(1, x,  [temp, arg(1), addr]).
regid_name_uses(2, y,  [temp, arg(2), addr]).
regid_name_uses(3, z,  [temp, arg(3), addr]).
regid_name_uses(4, w,  [temp, arg(4)]).
regid_name_uses(5, v,  [temp, retval]).
regid_name_uses(6, a,  [saved]).
regid_name_uses(7, b,  [saved]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
        format(atom(Descr), 'imm~d in ~q | ~q', [Bits, SimmRange, ImmRange])
    ;
        Descr = ''.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


show_table :-
    warn_if_nondet(show_table_).

show_table_ :-
    display_heading('Instruction Format Breakdown'),
    display_bitformat_legend,
    display_instr_format_breakdown,

    display_heading('Instruction Counts by Format'),
    display_instruction_counts_by_format,

    true.

display_instruction_counts_by_format :-
    format('|~`-t~20||~`-t~45||~`-t~80||~n'),
    format('|~tGeneric Format~t~20||~tInstr. Count Opts.~t~45||~tDescription~t~80||~n'),
    format('|~`-t~20||~`-t~45||~`-t~80||~n'),
    foreach(
        genericfmt(GFmt),
        display_genericfmt_instr_count(GFmt)
    ),
    format('|~`-t~45||~`-t~80||~n'),
    total_opcode_count_minmax(TotalMin, TotalMax),
    format(
        '|~t~5|~w  ~`.t  ~d, ~d~40|~t~45||~t~48|~t~80||~n',
        ['Total (excl. `ext`)', TotalMin, TotalMax]
    ),
    format('|~`-t~45||~`-t~80||~n').


display_genericfmt_instr_count(GFmt) :-
    bagof(Opcodes, GFmt^(
        genericfmt_opcodes(GFmt, Opcodes),
        labeling([up, bisect], [Opcodes])
    ), Counts),
    phrase(sequence(integer, `, `, Counts), CountsList),
    genericfmt_description(GFmt, Descr),
    format(
        '|~t~5|~k  ~`.t~20|~`.t  ~s~40|~t~45||~t~48|~w~t~80||~n',
        [GFmt, CountsList, Descr]
    ).


display_instr_format_breakdown :-
    display_table_header('Instruction Formats'),
    foreach(
        fmt(Fmt),
        format_section(Fmt)
    ),
    format('|~`-t~80||~n').

format_section(Fmt) :-
    fmt_description(Fmt, Descr),
    fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount),
    fmt_maxopcodes(Fmt, MaxAvail),
    % format('|~t~80||~n'),
    format(atom(CountSummary), '[ ~d max avail., ~d assigned, ~d reserved ]', [MaxAvail, AssignedCount, ReservedCount]),
    format('|~`-t[ ~w ]~`-t~35|~`-t~w~`-t~80||~n', [Descr, CountSummary]),
    format('|~t~80||~n'),
    foreach(
        fmt_layout(Fmt, Layout),
        format_layout_row(Fmt, Layout)
    ),
    format('|~t~80||~n').

format_layout_row(Fmt, Layout) :-
    list_item_occurrances(Layout, o, OBits),
    #Ops #= 2 ^ #OBits,
    ( \+ validate_instr_assignments(Fmt, Ops) ->
        AnsiStyle = [fg(red)]
    ;
        AnsiStyle = []
    ),
    list_item_occurrances(Layout, i, IBits),
    immbits_immdescription(IBits, ImmDescr),
    ansi_format(
        AnsiStyle,
        '|~|~t~k~t~8|~s~+~t~d opcode(s)~16+~t~w~t~40+|~n',
        [Fmt, Layout, Ops, ImmDescr]
    ).


display_bitformat_legend :-
    display_table_header('Legend'),
    format('|~`-t~15||~`-t~80||~n'),
    format('|~tBit Symbol~t~15||~tDescription~t~80||~n'),
    format('|~`-t~15||~`-t~80||~n'),
    foreach(
        bitformatchar_description(Char, Descr),
        format('|~t~w~t~15||~t~w~t~80||~n', [Char, Descr])
    ),
    format('|~`-t~80||~n'),
    format('~n').

bitformatchar_description('0, 1', 'A literal `0` or `1` embedded in the instruction.').
bitformatchar_description(o, 'A bit in the instruction''s opcode.').
bitformatchar_description(i, 'A bit in an immediate value.').
bitformatchar_description(r, 'A bit in a register specifier.').
bitformatchar_description('R', 'A bit in a second register specifier.').
bitformatchar_description(a, 'A bit in an address register specifier.').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


huffmantree_item_prefix(Fmt, Fmt, []).
huffmantree_item_prefix([Left | _Right], Fmt, ['0' | Prefix]) :-
    huffmantree_item_prefix(Left, Fmt, Prefix).
huffmantree_item_prefix([_Left | Right], Fmt, ['1' | Prefix]) :-
    huffmantree_item_prefix(Right, Fmt, Prefix).


display_heading(Content) :-
    format('~n~n~`=t ~w ~`=t~80|~n~n', [Content]).

display_table_header(Content) :-
    format('|~`-t~80||~n'),
    format('|~t~80||~n'),
    format('|~t ~w ~t~80||~n', [Content]),
    format('|~t~80||~n').


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