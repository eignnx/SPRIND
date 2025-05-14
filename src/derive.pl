%
% Derives bit formats based on ISA constraints.
%
:- module(derive, [
    genericfmt/1,
    fmt_prefix/2,
    fmt_operands/2,
    fmt_maxopcodes/2,
    total_opcode_count_minmax/2,
    total_opcode_count_minmax/3,
    genericfmt_opcodes/2,
    fmt_assignedinstrcount/2,
    fmt_assignedinstrcount/3,
    validate_instr_assignments_or_throw/2,
    fmt_opcodebits_immbits/3,
    fmt_labelledimmbits/2,
    fmt_labelledopcodebits/2,
    subr_byte_alignment/1
]).

:- set_prolog_flag(double_quotes, chars).
:- encoding(utf8).

:- use_module(isa).
:- use_module(utils).
:- use_module(library(clpfd)).
:- op(20, fx, #).


genericfmt(GFmt) :-
    bagof(Fmt, isa:fmt(Fmt), Fmts),
    maplist([Fmt, Functor-Arity]>>(
        Fmt =.. [Functor | Args],
        length(Args, Arity)
    ), Fmts, FunctorsAritiesDup),
    list_to_set(FunctorsAritiesDup, FunctorsArities),
    maplist([Functor-Arity, GF]>>(
        length(FreeArgs, Arity),
        GF =.. [Functor | FreeArgs]
    ), FunctorsArities, GFmts),
    member(GFmt, GFmts).


fmt_operands(Fmt, Operands) :-
    isa:fmt_operands_description(Fmt, Operands, _).


fmt_assignedinstrcount(Fmt, AssignedCount) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, _ReservedCount).
fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount) :-
    aggregate_all(count, (
            isa:fmt_instr(Fmt, Instr),
            dif(Instr, ???) % For spacing out instructions in opcode space.
        ),
        AssignedCount
    ),
    aggregate_all(count, (
            isa:fmt_instr(Fmt, ???) % For spacing out instructions in opcode space.
        ),
        ReservedCount
    ).

validate_instr_assignments(Fmt, AvailableCount) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount),
    #AvailableCount #>= #AssignedCount + #ReservedCount.

validate_instr_assignments_or_throw(Fmt, AvailableCount) :-
    fmt_assignedinstrcount(Fmt, AssignedCount, ReservedCount),
    (
        validate_instr_assignments(Fmt, AvailableCount) ->
            true
        ;
            throw(error(too_many_opcodes_assigned(
                Fmt,
                available(AvailableCount),
                assigned(AssignedCount),
                reserved(ReservedCount)
            )))
    ).


huffmantree_item_prefix(Fmt, Fmt, []).
huffmantree_item_prefix([Left | _Right], Fmt, ['1' | Prefix]) :-
    huffmantree_item_prefix(Left, Fmt, Prefix).
huffmantree_item_prefix([_Left | Right], Fmt, ['0' | Prefix]) :-
    huffmantree_item_prefix(Right, Fmt, Prefix).


tree_leaf(Tree, Leaf) :-
    Tree = [Left | Right] ->
        ( tree_leaf(Left, Leaf) ; tree_leaf(Right, Leaf) )
    ;
        Leaf = Tree.



fmt_prefix(Fmt, Prefix) :-
    isa:fmt_huffman_enc(Tree),
    huffmantree_item_prefix(Tree, Fmt, Prefix).


operand_size(r, Bits) :- isa:gpr_count_bits(Bits).
operand_size(s, Bits) :- isa:gpr_count_bits(Bits).
operand_size(t, Bits) :- isa:gpr_count_bits(Bits).
operand_size(i, Size) :- isa:instr_size(RegBits), Size in 0 .. RegBits.


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

fmt_labelledimmbits(Fmt, ImmBits) :-
    fmt_opcodebits_immbits(Fmt, _, ImmBits),
    label([ImmBits]).

fmt_labelledopcodebits(Fmt, OpcodeBits) :-
    fmt_opcodebits_immbits(Fmt, OpcodeBits, _),
    label([OpcodeBits]).

subr_byte_alignment(ByteAlignment) :-
    fmt_labelledimmbits(subr, ImmBits),
    #ByteAlignment #= 2 ^ (16 - #ImmBits - 1).


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
