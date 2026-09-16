/** <module> querilog_eval
Interpreter for querilog.
*/
:- module(querilog_eval, [
    interpretation/2,
    bv_signed/2,
    bv_signed/3,
    bv_unsigned/2,
    bv_unsigned/3
]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order)).
:- use_module(querilog_syntax).
:- use_module(isa).
:- use_module(sem).
:- use_module(consts).
:- use_module(utils).

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

n_list_front_lastn(N, List, Front, LastN) :-
    length(List, L),
    zcompare(Ord, N, L),
    ord_n_list_front_lastn(Ord, N, List, Front, LastN).
ord_n_list_front_lastn(<, N, [X|Tail], [X|FirstN], Rest) :-
    n_list_front_lastn(N, Tail, FirstN, Rest).
ord_n_list_front_lastn(=, _, Rest, [], Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bv_unsigned(bv(Bv), U, Size) :-
    %( (var(Size) ; var(Bv) ) -> U #< 2^Size ; true ),
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

bv_logical_shift_left(bv(A), bv(Shamt), bv(C)) :-
    ( must_be_bv(bv(A)), must_be_bv(bv(Shamt)) ),
    bv_unsigned(bv(Shamt), ShamtInt),
    length(A, ALen),

    % Allow `#0b111\3 << 999` (it would equal 0)
    ShamtIntClamped #= max(0, ALen - ShamtInt),
    length(Suffix, ShamtIntClamped),
    maplist(=(0), Suffix),

    n_list_front_lastn(ShamtIntClamped, A, _, TrimmedA),

    append(TrimmedA, Suffix, C).

full_adder(A, B, Cin, Sum, Cout) :-
    Sum #= A xor B xor Cin,
    Cout #= (A /\ B) \/ (Cin /\ (A xor B)).

bv_add(A, B, C) :- bv_add(A, B, C, #{}).
%bv_add(A, B, C, Cout) :- bv_add(A, B, 0, C, Cout, _).
%bv_add(bv([A|As]), bv([B|Bs]), Cin, bv([C|Cs]), Cout, SignIn) :-
%    bv_add_(As, Bs, Cin, Cs, SignIn),
%    full_adder(A, B, Cout, C, SignIn).
bv_add_([], [], Cinout, [], Cinout).
bv_add_([A|As],[B|Bs],Cin,[C|Cs],Cout) :-
    bv_add_(As, Bs, Cin, Cs, Cout0),
    full_adder(A, B, Cout0, C, Cout).

% Options:
%   - cin: The carry-in bit. The sum will be A + B + Cin.
%   - cout: The carry-out bit. 0b10 + 0b10 = 0b100 with a carry-out of 1.
%   - signin: The carry-in to the sign bit.
%             ex: 0b0100 + 0b0100 has a sign-in of 1.
%             ex: 0b0100 + 0b0000 has a sign-in of 0.
bv_add(bv([A|As]), bv([B|Bs]), bv([C|Cs]), Options) :-
    #{cout: Cout, signin: SignIn} >:< Options,
    Cin = Options.get(cin, 0),
    bv_add_(As, Bs, Cin, Cs, SignIn),
    full_adder(A, B, SignIn, C, Cout).

bv_sub(A, B, C) :-
    bv_sub(A, B, C, _Carry).
bv_sub(A, B, C, Cout) :-
    bv_bitwise_complement(B, NotB),
    bv_add(A, NotB, C, #{cin: 1, cout: Cout}).

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

% We require that `size(Tgt) == 2^size(Idx)`.
% We could return a default (either 0 or 1) when the index is out of
% bounds, but that choice would be arbitrary. If Tgt was an unsigned value, 0
% should be the default, while if it represents a signed value, 1 should be the
% default. So basically, you should just use `zxt_log2` or `sxt_log2` on Tgt
% before using this predicate.
% Note: index 0 is the least significant bit of Tgt.
bv_bit(bv(Tgt), bv(Idx), bv([Bit])) :-
    % RevIdx = Size - 1 - Idx = ~Idx (if Size is a power of 2, and Idx has
    % log2(Size) bits).
    bv_bitwise_complement(bv(Idx), bv(IdxCompl)),
    bv_unsigned(bv(IdxCompl), IdxU),
    ( nth0(IdxU, Tgt, Bit) -> true ;
        bv_size(bv(Tgt), TgtSz),
        succ(MaxIdx, TgtSz),
        type_error(between(0, MaxIdx), IdxU)
    ).

bv_zero_extend(Bv0, NewSize, Bv) :-
    bv_size(Bv0, Bv0Size),
    PadSize #= NewSize - Bv0Size, PadSize #>= 0,
    length(Padding, PadSize),
    maplist(=(0), Padding),
    bv_concat(bv(Padding), Bv0, Bv).

bv_sign_extend(Bv0, NewSize, Bv) :-
    ( var(Bv0) -> instantiation_error(Bv0) ; true ),
    bv([SignBit|_]) = Bv0,
    bv_size(Bv0, Bv0Size),
    PadSize #= NewSize - Bv0Size, PadSize #>= 0,
    length(Padding, PadSize),
    maplist(=(SignBit), Padding),
    bv_concat(bv(Padding), Bv0, Bv).

% Zero-extends the value until it's size is a power of 2. If already a power of
% 2, do nothing.
bv_zero_extend_log2(Bv0, NewSize, Bv) :-
    bv_signbit_extend_log2(Bv0, NewSize, Bv, 0).

bv_sign_extend_log2(Bv0, NewSize, Bv) :-
    bv([SignBit|_]) = Bv0,
    bv_signbit_extend_log2(Bv0, NewSize, Bv, SignBit).

:- det(bv_signbit_extend_log2/4).
bv_signbit_extend_log2(Bv0, NewSize, Bv, SignBit) :-
    bv_size(Bv0, OldSize),
    round_up_to_next_pow2(OldSize, NewSize),
    PaddingSz in 0..sup,
    PaddingSz #= NewSize - OldSize,
    label([PaddingSz]),
    length(Padding, PaddingSz),
    maplist(=(SignBit), Padding),
    bv_concat(bv(Padding), Bv0, Bv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATE MONAD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(interpstate{
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
        mem: mem{}
    },
    next: #{
        regs: #{},
        sysregs: #{},
        mem: mem{}
    }
}) :-
    bv_unsigned(Zero16, 0, 16),
    bv_unsigned(Zero32, 0, 32).

%! interpretation(+Program:typechecked(querilog_program), -Next:interpstate) is det.
%
% Requires a *typechecked* program as input.
interpretation(Program, NextState) :-
    init_state(InitState),
    phrase(Program, [InitState], [NextState]).

get_reg(Reg, Value) -->
    get_state(State),
    { Value = State.get(curr/regs/Reg) }.

set_reg(Reg, Value) -->
    get_state(Before),
    { AlreadySet = Before.get(next/regs/Reg) ->
        throw(error(signal_already_set(next/regs/Reg, AlreadySet), _))
    ;
        After = Before.put(next/regs/Reg, Value)
    },
    put_state(After).

get_sysreg(Reg, Value) -->
    get_state(State),
    { Value = State.get(curr/sysregs/Reg) }.

set_sysreg(Reg, Value) -->
    get_state(Before),
    { AlreadySet = Before.get(next/sysregs/Reg) ->
        throw(error(signal_already_set(next/sysregs/Reg, AlreadySet), _))
    ;
        After = Before.put(next/sysregs/Reg, Value)
    },
    put_state(After).

set_memaddr(AddrBv, Value) -->
    get_state(Before),
    { MemNext = Before.get(next/mem) },
    { bv_unsigned(AddrBv, Addr, 16) },
    { AlreadySet = Before.next.mem.get(Addr) ->
        throw(error(signal_already_set(next/mem/Addr, AlreadySet), _))
    ;
        NewMemNext = MemNext.put(Addr, Value),
        After = Before.put(next/mem, NewMemNext)
    },
    put_state(After).

get_memaddr(AddrBv, Value) -->
    get_state(State),
    { bv_unsigned(AddrBv, Addr, 16) },
    { Value = State.curr.mem.get(Addr) -> true ;
        bv_unsigned(Value, 0, 16)
    }.

add_binding(VarName, Value) -->
    { must_be_bv(Value) },
    get_state(Before),
    { After = Before.put(bindings, [VarName=Value | Before.bindings]) },
    put_state(After).

lookup_binding(VarName, Value) -->
    get_state(State),
    { memberchk(VarName=Value, State.bindings) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EVALUATOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- det(term_eval_size//3).
:- discontiguous(term_eval_size//3).

term_eval_size(#Term, Eval, Size) --> !,
    poundsign_eval_size(Term, Eval, Size).

poundsign_eval_size(N, Bv, Size) -->
    { integer(N) }, !,
    { N #>= 0 -> bv_unsigned(Bv, N, Size) ; bv_signed(Bv, N, Size) }.
poundsign_eval_size(N\Size, Bv, Size) -->
    { integer(N) }, !,
    { ( N #>= 0 -> bv_unsigned(Bv, N, Size) ; bv_signed(Bv, N, Size) ) -> true ;
        format(atom(Msg), 'Integer ~d does not fit in ~d bits', [N, Size]),
        throw(error(syntax_error(Msg, #N\Size), _))
    }.
poundsign_eval_size(Const, Bv, Size) -->
    { consts:def_const(Const, Val) }, !,
    { bv_unsigned(Bv, Val, Size) }.

term_eval_size($Reg, Value, RegSize) --> !,
    { isa:register_size(RegSize) },
    get_state(State),
    { Value = State.get(curr/regs/Reg) }.

term_eval_size($$SysReg, Value, RegSize) --> !,
    { isa:sysreg_size(SysReg, RegSize) },
    get_state(State),
    { Value = State.get(curr/sysregs/SysReg) }.

term_eval_size(?Var, Value, Size) --> !,
    lookup_binding(Var, V0),
    ( { bv(_) = V0 } ->
        { bv_size(V0, Size) },
        { Value = V0 }
    ; { $Reg = V0 } ->
        term_eval_size($Reg, Value, Size)
    ;
        { throw_error(unknown_value_in_bindings(V0)) }
    ).

term_eval_size(sxt(E0), E, Size) --> !,
    term_eval_size(E0, E1, E1Sz),
    { E1Sz #< Size },
    { bv_sign_extend(E1, Size, E) }.

term_eval_size(zxt(E0), E, Size) --> !,
    term_eval_size(E0, E1, E1Sz),
    { E1Sz #< Size },
    { bv_zero_extend(E1, Size, E) }.

term_eval_size({Es0}, E, Size) --> !,
    { comma_list(Es0, Es1) },
    eval_all(Es1, Es2),
    { bv_concat(Es2, E) },
    { bv_size(E, Size) }.

term_eval_size(A0 + B0, Sum, Size) --> !,
    term_eval_size(A0, A, Size),
    term_eval_size(B0, B, Size),
    { bv_add(A, B, Sum) }.

term_eval_size(A0 and B0, Sum, Size) --> !,
    term_eval_size(A0, A, Size),
    term_eval_size(B0, B, Size),
    { bv_bitwise_and(A, B, Sum) }.

term_eval_size(A0 or B0, Sum, Size) --> !,
    term_eval_size(A0, A, Size),
    term_eval_size(B0, B, Size),
    { bv_bitwise_or(A, B, Sum) }.

term_eval_size(A0 << B0, C, Size) --> !,
    term_eval_size(A0, A, Size),
    { 2^ShamtSz #= Size },
    term_eval_size(B0, B, ShamtSz),
    { bv_logical_shift_left(A, B, C) }.

term_eval_size(mem(A0), B, 8) --> !,
    { isa:register_size(RegSz) },
    term_eval_size(A0, A, RegSz),
    get_memaddr(A, B).


:- det(stmt_eval//1).
:- discontiguous(stmt_eval//1).

stmt_eval(todo) --> [].
stmt_eval(todo(_)) --> [].
stmt_eval( (A ; B) ) --> stmt_eval(A), stmt_eval(B).

stmt_eval(Lhs := Rhs0) -->
    contassign_lhs(Lhs, Rhs0).

contassign_lhs($Reg, _Rhs0) --> !,
    { throw_error( cannot_contassign_to($Reg)) }.
contassign_lhs($$Reg, _Rhs0) --> !,
    { throw_error(cannot_contassign_to($$Reg)) }.
contassign_lhs(mem(Addr), _Rhs0) --> !,
    { throw_error(cannot_contassign_to(mem(Addr))) }.
contassign_lhs(?Var, Rhs0) --> !,
    term_eval_size(Rhs0, Rhs, _Size), % Size must be inferred here
    % Assume typechecking ensures ?Var is not a reg
    add_binding(Var, Rhs).

stmt_eval(Lhs <- Rhs) -->
    clkassign_lhs(Lhs, Rhs).

clkassign_lhs(?Var, Rhs0) -->
    % Assume typechecking ensures ?Var is a reg
    lookup_binding(Var, ActualLhs),
    { $_ = ActualLhs -> true ;
        throw_error(expected_var_to_refer_to_reg, #{
            var: ?Var,
            actual_value: ActualLhs
        })
    },
    clkassign_lhs(ActualLhs, Rhs0).

clkassign_lhs($Reg, Rhs0) -->
    { isa:register_size(RegSz) },
    term_eval_size(Rhs0, Rhs, RegSz),
    set_reg(Reg, Rhs).
clkassign_lhs($$SysReg, Rhs0) -->
    { isa:sysreg_size(SysReg, RegSz) },
    term_eval_size(Rhs0, Rhs, RegSz),
    set_sysreg(SysReg, Rhs).
clkassign_lhs(m(Addr0), Rhs0) -->
    term_eval_size(Addr0, Addr, 16),
    { isa:register_size(RegSz) },
    term_eval_size(Rhs0, Rhs, RegSz),
    set_memaddr(Addr, Rhs).
clkassign_lhs(bit(Tgt, Idx0), Rhs0) -->
    { [IdxSz, TgtSz] ins 1..sup },
    { 2^IdxSz #= TgtSz },
    term_eval_size(Rhs0, Rhs, 1),
    term_eval_size(Idx0, Idx, IdxSz),
    ( { Tgt = $Reg } ->
        { isa:register_size(TgtSz) },
        get_reg(Reg, OldVal),
        { set_bit(OldVal, Idx, Rhs, NewVal) },
        set_reg(Reg, NewVal)
    ; { Tgt = $$Reg } ->
        { isa:sysreg_size(Reg, TgtSz) },
        get_sysreg(Reg, OldVal),
        { set_bit(OldVal, Idx, Rhs, NewVal) },
        set_sysreg(Reg, NewVal)
    ; { Tgt = mem(Addr0) } ->
        { TgtSz = 8 },
        term_eval_size(Addr0, Addr, _AddrSz),
        get_memaddr(Addr, OldVal),
        { set_bit(OldVal, Idx, Rhs, NewVal) },
        set_memaddr(Addr, NewVal)
    ;
        { throw_error(bad_target_of_bit_lhs(bit(Tgt, _))) }
    ).

set_bit(OldVal, Idx, Rhs, NewVal) :-
    bv_unsigned(OldVal, OldValU),
    bv_unsigned(Idx, IdxU),
    bv_unsigned(Rhs, RhsU),
    NewValU #= OldValU /\ \(1 << IdxU) \/ ((RhsU /\ 1) << IdxU),
    bv_unsigned(NewVal, NewValU),
    !.


eval_all([], []) --> [].
eval_all([E0|Es0], [E|Es]) -->
    term_eval_size(E0, E, _Sz),
    eval_all(Es0, Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%% PORTRAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- create_prolog_flag(bv_portray_base, bin, [type(oneof([
    dec,
    signed_dec,
    hex,
    bin
]))]).

% :- dynamic user:portray/1.
:- multifile user:portray/1.

user:portray(bv(Bv)) :-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNIT TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(test_querilog_eval).

test(eval_simple_store, [X == bv([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1])]) :-
    interpretation(stmt_eval($x <- #3\16), State),
    X = State.next.regs.x.

:- end_tests(test_querilog_eval).
