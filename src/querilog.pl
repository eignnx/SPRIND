/** <module> querilog
A Verilog-like specification language embedded in Prolog syntax.
*/

:- module(querilog).

:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order)).

:- op(400, yfx, >>>).
:- op(500, yfx, and).
:- op(500, yfx, or).
:- op(50, fx, #).
:- op(25, xfx, \).

%! bv(?Bv:compound(bv(nonempty_list(oneof([0, 1]))))).
%
% Describes a bit vector; a non-empty fixed-length sequence of bits.
bv(bv([B | Bs])) :- bit(B), maplist(bit, Bs).

bit(B) :- B in 0..1.
bv_size(bv(Bv), Size) :- length(Bv, Size).

must_be_bv(Bv) :- bv(Bv) -> true ; type_error(compound(bv(nonempty_list(oneof([0,1])))), Bv).

bv_same_len(bv(A), bv(B)) :-
    same_length(A, B) -> true ;
    domain_error('two bitvectors of the same length', bv(A)-bv(B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_list_front_lastn(N, List, FirstN, Rest) :-
    length(List, L),
    zcompare(Ord, N, L),
    ord_n_list_front_lastn(Ord, N, List, FirstN, Rest).
ord_n_list_front_lastn(<, N, [X|Tail], [X|FirstN], Rest) :-
    n_list_front_lastn(N, Tail, FirstN, Rest).
ord_n_list_front_lastn(=, _, Rest, [], Rest).


bv_unsigned(bv(Bv), U, Size) :-
    length(Bv, Size),
    bv_unsigned(bv(Bv), U).
bv_unsigned(bv(Bv), U) :-
    bv(bv(Bv)),
    U in 0 .. sup,
    bv_unsigned_(Bv, _, U).
bv_unsigned_([], 0, 0).
bv_unsigned_([B|Bs], N, U) :-
    U #= B * 2^N0 + U0,
    N #= N0 + 1,
    bv_unsigned_(Bs, N0, U0).

bv_signed(Bv, S) :- bv_signed(Bv, S, _SizeS).
bv_signed(bv(BvS), S, SizeS) :-
    length(BvS, SizeS),
    bv(bv(BvS)),
    [SignBit|BvU] = BvS,
    SizeS #= SizeU + 1,
    bv_unsigned_(BvU, SizeU, U),
    S #= -1 * SignBit * 2^SizeU + U.

signbit_compare(SignBit, N) :-
    SignBit #<==> 0 #< N,
    label([SignBit]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bv_bitwise_unop(UnaryOp, bv(A), bv(B)) :-
    must_be_bv(bv(A)),
    maplist(UnaryOp, A, B).
bv_bitwise_complement(A, B) :-
    bv_bitwise_unop([X, Y]>>(Y #= 1 - X), A, B).

bv_bitwise_binop(BinOp, bv(A), bv(B), bv(C)) :-
    ( must_be_bv(bv(A)), must_be_bv(bv(B)), bv_same_len(bv(A), bv(B)) ),
    maplist(BinOp, A, B, C).

bv_bitwise_and(A, B, C) :-
    bv_bitwise_binop([X, Y, Z]>>(Z #= X /\ Y), A, B, C).
bv_bitwise_or(A, B, C) :-
    bv_bitwise_binop([X, Y, Z]>>(Z #= X \/ Y), A, B, C).


bv_logical_shift_right(A, Shamt, C) :-
    bv_shift_right_signexttype(A, Shamt, C, 0).
bv_arithmetic_shift_right(A, Shamt, C) :-
    bv_shift_right_signexttype(A, Shamt, C, sign).

bv_shift_right_signexttype(bv(A), bv(Shamt), bv(C), SExtTy) :-
    ( must_be_bv(bv(A)), must_be_bv(bv(Shamt)) ),
    bv_unsigned(bv(Shamt), ShamtInt),
    [SignBit|_] = A,
    signexttype_signbit_extbit(SExtTy, SignBit, ExtBit),
    length(A, ALen),

    % Allow `3'b111 >> 999` (it would equal 0)
    ShamtIntClamped #= min(ShamtInt, ALen),
    length(Prefix, ShamtIntClamped),
    maplist(=(ExtBit), Prefix),

    n_list_front_lastn(ShamtIntClamped, A, TrimmedA, _),

    append(Prefix, TrimmedA, C).

signexttype_signbit_extbit(0, _, 0).
signexttype_signbit_extbit(sign, SignBit, SignBit).

full_adder(A, B, Cin, Sum, Cout) :-
    Sum #= A xor B xor Cin,
    Cout #= (A /\ B) \/ (Cin /\ (A xor B)).

bv_add(A, B, C) :- bv_add(A, B, 0, C, _).
bv_add(A, B, C, Cout) :- bv_add(A, B, 0, C, Cout).
bv_add(bv(A), bv(B), Cin, bv(C), Cout) :-
    bv_add_(A, B, Cin, C, Cout).
bv_add_([], [], Cinout, [], Cinout).
bv_add_([A|As],[B|Bs],Cin,[C|Cs],Cout) :-
    bv_add_(As, Bs, Cin, Cs, Cout0),
    full_adder(A, B, Cout0, C, Cout).

bv_sub(A, B, C) :-
    bv_sub(A, B, C, _Carry).
bv_sub(A, B, C, Cout) :-
    bv_bitwise_complement(B, NotB),
    bv_add(A, NotB, 1, C, Cout).

bv_concat(bv(A), bv(B), bv(C)) :- append(A, B, C).

bv_concat(Es0, E) :-
    reverse(Es0, [E1|Es1]),
    foldl(bv_concat, Es1, E1, E).

bv_slice(bv(B), Start, End, bv(Slice)) :-
    length(B, N),
    EndDropCount #= N - End,
    n_list_front_lastn(EndDropCount, B, EndDropped, _),
    Len #= End - Start,
    n_list_front_lastn(Len, EndDropped, _, Slice),
true.

bv_zero_extend(Bv0, NewSize, Bv) :-
    bv_size(Bv0, Bv0Size),
    PadSize #= NewSize - Bv0Size, PadSize #>= 0,
    length(Padding, PadSize),
    maplist(=(0), Padding),
    bv_concat(bv(Padding), Bv0, Bv).

bv_sign_extend(Bv0, NewSize, Bv) :-
    bv([SignBit|_]) = Bv0,
    bv_size(Bv0, Bv0Size),
    PadSize #= NewSize - Bv0Size, PadSize #>= 0,
    length(Padding, PadSize),
    maplist(=(SignBit), Padding),
    bv_concat(bv(Padding), Bv0, Bv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATE MONAD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(#{
    bindings: [],
    curr: #{
        regs: #{
            sp: Zero16,
            x:  Zero16,
            y:  Zero16,
            z:  Zero16,
            w:  Zero16,
            v:  Zero16,
            a:  Zero16,
            b:  Zero16
        },
        sysregs: #{
            pc: Zero16,
            ra: Zero16,
            ts: Zero16,
            cc: Zero16,
            gp: Zero16,
            kr: Zero16,
            mp: Zero32
        },
        mem: MemCurr
    },
    next: #{
        regs: #{},
        sysregs: #{},
        mem: MemNext
    }
}) :-
    bv_unsigned(Zero16, 0, 16),
    bv_unsigned(Zero32, 0, 32),
    list_to_assoc([], MemCurr),
    list_to_assoc([], MemNext).

interpretation(Program, NextState) :-
    init_state(InitState),
    phrase(Program, [InitState], [NextState]).

before_after(Old, New), [New] --> [Old].
get(State) --> before_after(State, State).
put(State) --> before_after(_, State).

set_reg(Reg, Value) -->
    get(Before),
    { AlreadySet = Before.get(next/regs/Reg) ->
        throw(error(signal_already_set(next/regs/Reg, AlreadySet), _))
    ;
        After = Before.put(next/regs/Reg, Value)
    },
    put(After).

set_sysreg(Reg, Value) -->
    get(Before),
    { AlreadySet = Before.get(next/sysregs/Reg) ->
        throw(error(signal_already_set(next/sysregs/Reg, AlreadySet), _))
    ;
        After = Before.put(next/sysregs/Reg, Value)
    },
    put(After).

set_memaddr(AddrBv, Value) -->
    get(Before),
    { MemNext = Before.get(next/mem) },
    { bv_unsigned(AddrBv, Addr, 16) },
    { get_assoc(Addr, Before.next.mem, AlreadySet) ->
        throw(error(signal_already_set(next/mem/Addr, AlreadySet), _))
    ;
        put_assoc(Addr, MemNext, Value, NewMemNext),
        After = Before.put(next/mem, NewMemNext)
    },
    put(After).

add_binding(Var, Val, Ty) -->
    get(Before),
    { After = Before.put(bindings, [Var-Val-Ty | Before.bindings]) },
    put(After).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EVALUATOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- det(term_eval_type//3).

term_eval_type(#N\Size, Bv, bv(Size)) -->
    { N #>= 0 ->
        bv_unsigned(Bv, N, Size)
    ;
        bv_signed(Bv, N, Size)
    }.

term_eval_type(sxt(E0), E, bv(Size)) -->
    term_eval_type(E0, E1, bv(_E1Size)),
    { bv_sign_extend(E1, Size, E) }.

term_eval_type(zxt(E0), E, bv(Size)) -->
    term_eval_type(E0, E1, bv(_E1Size)),
    { bv_zero_extend(E1, Size, E) }.

term_eval_type({Es0}, E, bv(Size)) -->
    { comma_list(Es0, Es1) },
    eval_all(Es1, Es2),
    { bv_concat(Es2, E) },
    { bv_size(E, Size) }.

term_eval_type(A0 + B0, Sum, bv(Size)) -->
    term_eval_type(A0, A, bv(ASize)),
    term_eval_type(B0, B, bv(BSize)),
    { ASize = BSize -> true ; throw(error(incompatible_sizes(+, ASize, BSize), _)) },
    { Size = ASize },
    { bv_add(A, B, Sum) }.

eval_all([], []) --> [].
eval_all([E0|Es0], [E|Es]) -->
    term_eval_type(E0, E, _Ty),
    eval_all(Es0, Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%% PORTRAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- create_prolog_flag(bv_portray_base, bin, [type(oneof([
    dec,
    signed_dec,
    hex,
    bin
]))]).

:- dynamic portray/1.
:- multifile portray/1.

portray(bv(Bv)) :-
    Bv = [_|_],
    ( maplist(integer, Bv) ->
        length(Bv, N),
        current_prolog_flag(bv_portray_base, Base),
        ( Base = signed_dec ->
            bv_signed(bv(Bv), Int),
            portray_ground_bv_base(dec, Int, N)
        ;
            bv_unsigned(bv(Bv), Int),
            portray_ground_bv_base(Base, Int, N)
        )
    ;
        format('bv(~w)', [Bv])
    ).

portray_ground_bv_base(dec, U, N) :- format('#~I\\~d', [U, N]).
portray_ground_bv_base(hex, U, N) :- format('#0x~16R\\~d', [U, N]).
portray_ground_bv_base(bin, U, N) :- format('#0b~|~`0t~2r~*+\\~d', [U, N, N]).

bv_portray_dec :- set_prolog_flag(bv_portray_base, dec).
bv_portray_signed :- set_prolog_flag(bv_portray_base, signed_dec).
bv_portray_hex :- set_prolog_flag(bv_portray_base, hex).
bv_portray_bin :- set_prolog_flag(bv_portray_base, bin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
