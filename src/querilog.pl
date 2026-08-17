/** <module> querilog
A Verilog-like specification language embedded in Prolog syntax.
*/

:- module(querilog).

:- use_module(library(clpfd)).

:- op(400, yfx, >>>).
:- op(500, yfx, and).
:- op(500, yfx, or).

%! bv(?Bv:nonempty_list(oneof([0, 1]))).
%
% Describes a bit vector; a non-empty fixed-length sequence of bits.
bv([B | Bs]) :-
    B in 0..1,
    maplist([X]>>(X in 0..1), Bs).

must_be_bv(Bv) :- bv(Bv) -> true ; type_error(nonempty_list(oneof([0,1])), Bv).

bv_same_len(A, B) :-
    same_length(A, B) -> true ;
    domain_error('two bitvectors of the same length', A-B).

bv_bitwise_unop(UnaryOp, A, B) :-
    must_be_bv(A),
    maplist(UnaryOp, A, B).
bv_bitwise_complement(A, B) :-
    bv_bitwise_unop([X, Y]>>(Y #= 1 - X), A, B).

bv_bitwise_binop(BinOp, A, B, C) :-
    ( must_be_bv(A), must_be_bv(B), bv_same_len(A, B) ),
    maplist(BinOp, A, B, C).

bv_bitwise_and(A, B, C) :-
    bv_bitwise_binop([X, Y, Z]>>(Z #= X /\ Y), A, B, C).
bv_bitwise_or(A, B, C) :-
    bv_bitwise_binop([X, Y, Z]>>(Z #= X \/ Y), A, B, C).


bv_logical_shift_right(A, Shamt, C) :-
    bv_shift_right_signexttype(A, Shamt, C, 0).
bv_arithmetic_shift_right(A, Shamt, C) :-
    bv_shift_right_signexttype(A, Shamt, C, sign).

bv_shift_right_signexttype(A, Shamt, C, SExtTy) :-
    ( must_be_bv(A), must_be_bv(Shamt) ),
    bv_unsigned(Shamt, ShamtInt),
    [SignBit|_] = A,
    signexttype_signbit_extbit(SExtTy, SignBit, ExtBit),
    length(A, ALen),

    % Allow `3'b111 >> 999` (it would equal 0)
    ShamtIntClamped #= min(ShamtInt, ALen),
    length(Prefix, ShamtIntClamped),
    maplist(=(ExtBit), Prefix),

    n_list_firstn_rest(ShamtIntClamped, A, TrimmedA, _),

    append(Prefix, TrimmedA, C).

signexttype_signbit_extbit(0, _, 0).
signexttype_signbit_extbit(sign, SignBit, SignBit).


n_list_firstn_rest(N, List, FirstN, Rest) :-
    length(List, L),
    zcompare(Ord, N, L),
    ord_n_list_firstn_rest(Ord, N, List, FirstN, Rest).
ord_n_list_firstn_rest(<, N, [X|Tail], [X|FirstN], Rest) :-
    n_list_firstn_rest(N, Tail, FirstN, Rest).
ord_n_list_firstn_rest(=, _, Rest, [], Rest).


bv_unsigned(Bv, U) :-
    U #>= 0,
    bv(Bv),
    U in 0 .. sup,
    bv_unsigned_(Bv, _, U).
bv_unsigned_([], 0, 0).
bv_unsigned_([B|Bs], N, U) :-
    U #= B * 2^N0 + U0,
    N #= N0 + 1,
    bv_unsigned_(Bs, N0, U0).

bv_signed([SignBit|Bv], S) :-
    bv([SignBit|Bv]),
    length(Bv, N),
    bv_unsigned_(Bv, _, U),
    S #= (-1 * SignBit * 2^N) + U.


full_adder(A, B, Cin, Sum, Cout) :-
    Sum #= A #\ B #\ Cin,
    Cout #= (A /\ B) \/ (Cin /\ (A #\ B)).

ql_op_info(>>>, #{
    title: "Arithmetic Right Shift",
    descr: "Preserves the sign of the value during right shift"
}).
ql_op_info(>>, #{
    title: "Logical Right Shift"
}).
ql_op_info(<<, #{
    title: "Logical Left Shift"
}).



/*
sem(
    idx := bit_idx(3:0);
    mask := ~(1 << idx);
    rd <- rd \/ mask
).
*/
