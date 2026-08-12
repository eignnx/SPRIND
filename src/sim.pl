:- module(sim, [
    op(5, fx, $),
    op(5, fx, $$),
    op(5, fx, #),
    op(5, fx, ?),
    op(1100, xfx, <-),
    op(15, xfx, \)
]).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(dcg/high_order)).

:- encoding(utf8).

:- op(5, fx, $).
:- op(5, fx, $$).
:- op(5, fx, #).
:- op(5, fx, ?).
:- op(1100, xfx, <-).
:- op(15, xfx, \).

interpretation(Program, StateAfter) :-
    init_state(StateBefore),
    catch(
        phrase(Program, [StateBefore], [StateAfter]),
        Error,
        (
            print_message(error, Error),
            fail
        )
    ).

init_state(#{
    regs: #{
        sp: 0,
        x: 0,
        y: 0,
        z: 0,
        w: 0,
        v: 0,
        a: 0,
        b: 0
    },
    sysregs: #{
        pc: 0,
        ra: 0,
        ts: 0,
        cc: 0,
        gp: 0,
        kr: 0,
        mp: 0
    },
    mem: Mem,
    bindings: []
}) :-
    list_to_assoc([], Mem).

before_after(Old, New), [New] --> [Old].
get(State) --> before_after(State, State).
put(State) --> before_after(_, State).

set_reg(Reg, Value) -->
    get(Before),
    { After = Before.put(regs/Reg, Value) },
    put(After).

set_sysreg(Reg, Value) -->
    get(Before),
    { After = Before.put(sysregs/Reg, Value) },
    put(After).

bitpattern_unsigned(B-Size, U) :-
    U in 0 .. sup,
    Size in 1 .. sup,
    B #>= 0, B #< 2^Size,
    B #= U.




% :- det(<- // 2).

% X is set to the bit pattern for which u(X) = Rhs.
( u($Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(u($Reg) <- Rhs0, u\16, RhsTy),
    get(State), { New = State.put(regs/Reg, Rhs) }, put(New).

( u($$Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(u($$Reg) <- Rhs0, u\16, RhsTy),
    get(State), { New = State.put(sysregs/Reg, Rhs) }, put(New).

( s($Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(s($Reg) <- Rhs0, s\16, RhsTy),
    get(State), { New = State.put(regs/Reg, Rhs) }, put(New).

( s($$Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(s($$Reg) <- Rhs0, s\16, RhsTy),
    get(State), { New = State.put(sysregs/Reg, Rhs) }, put(New).


( m(Addr0) <- Rhs0 ) -->
    term_eval_type(Addr0, Addr, AddrTy),
    term_compatible_types(m(Addr0), AddrTy, u\16),
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(m(Addr0) <- Rhs0, _\8, RhsTy),
    get(State),
    { OldMem = State.mem },
    { put_assoc(Addr, OldMem, Rhs, NewMem) },
    { NewState = State.put(mem, NewMem) },
    put(NewState).


let(?Var, Rhs0, Rest) -->
    term_eval_type(Rhs0, Rhs, RhsSize),
    get(State),
    { NewState = State.put(bindings, [Var-Rhs-RhsSize | State.bindings]) },
    put(NewState),
    call(Rest).

% :- det(term_compatible_types//3).

term_compatible_types(SrcExpr, KindA\SizeA, KindB\SizeB) -->
    { KindA = KindB ->
        ( SizeA #= SizeB ->
            true
        ;
            SrcExpr =.. [Op, _A, B],
            copy_term(SrcExpr-SizeA-SizeB, SrcExpr𞁞-SizeA𞁞-SizeB𞁞, Constraints),
            numbervars(SrcExpr𞁞-SizeA𞁞-SizeB𞁞-Constraints, 0, End, [singletons(true)]),
            format(
                atom(Msg),
                'The subterms in `~p` have incompatible sizes: `_\\~p ~p _\\~p` is invalid.~n\c
                 Additional constraints: ~p',
                [SrcExpr𞁞, SizeA𞁞, Op, SizeB𞁞, Constraints]
            ),
            AType = _\SizeA𞁞,
            numbervars(AType, End, _EndEnd, [singletons(true)]),
            throw(error(domain_error(AType, B), context(interpretation_of_operator(Op), Msg)))
        )
    ;
        SrcExpr =.. [Op, _A, B],
        copy_term(SrcExpr-KindA-KindB, SrcExpr𞁞-KindA𞁞-KindB𞁞, Constraints),
        numbervars(SrcExpr𞁞-KindA𞁞-KindB𞁞-Constraints, 0, End, [singletons(true)]),
        format(
            atom(Msg),
            'The subterms in `~p` have incompatible kinds: `~p\\_ ~p ~p\\_` is invalid.~n\c
             Additional constraints: ~p',
            [SrcExpr𞁞, KindA𞁞, Op, KindB𞁞, Constraints]
        ),
        AType = KindA𞁞\SizeA,
        numbervars(AType, 0, _End, [singletons(true)]),
        throw(error(domain_error(AType, B), context(interpretation_of_operator(Op), Msg)))
    }.

% :- det(term_eval_type//3).

term_eval_type(#N, N, Type) --> { possible_type(N, Type) }.
term_eval_type(?Var, Val, Type) --> get(State), { memberchk(Var-Val-Type, State.bindings) }.
term_eval_type($Reg, Value, i\16) --> get(State), { Value = State.get(regs/Reg) }.
term_eval_type($$Reg, Value, i\16) --> get(State), { Value = State.get(sysregs/Reg) }. % TODO: handle 32-bit sysregs
term_eval_type(u(A0), A, u\Size) -->
    term_eval_type(A0, A1, OldKind\Size),
    { OldKind = u -> true
    ; OldKind = i -> true
    ; OldKind = s -> signed_unsigned(A1, A, Size)
    }.
term_eval_type(s(A0), A, s\Size) -->
    term_eval_type(A0, A1, OldKind\Size),
    { OldKind = s -> true
    ; OldKind = i -> signed_unsigned(A, A1, Size)
    ; OldKind = u -> signed_unsigned(A, A1, Size)
    }.
term_eval_type(m(Addr0), Value, i\8) -->
    term_eval_type(Addr0, Addr, AddrSize),
    term_compatible_types(m(Addr0), AddrSize, u\16),
    get(State),
    { get_assoc(Addr, State.mem, Value) -> true ; Value = 0 }.
term_eval_type(A0 + B0, Sum, Kind\Size) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    term_compatible_types(A0 + B0, TA, TB),
    { Kind\Size = TA },
    { Sum #= A + B }.
term_eval_type(A0 /\ B0, Sum, Kind\Size) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    term_compatible_types(A0 /\ B0, TA, TB),
    { Kind\Size = TA },
    { Sum #= A /\ B }.
term_eval_type(A0 \/ B0, Sum, Kind\Size) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    term_compatible_types(A0 \/ B0, TA, TB),
    { Kind\Size = TA },
    { Sum #= A \/ B }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kind(X) :-
    ( get_attr(X, sim, _Kind) -> true
    ;
        list_to_ord_set([i, s, u], Domain),
        put_attr(Y, sim, Domain),
        X = Y
    ).

kind(X, Kind) :-
    var(Kind), !,
    get_attr(X, sim, Kind).
kind(X, List) :-
    list_to_ord_set(List, Domain),
    put_attr(Y, sim, Domain),
    X = Y.

attr_unify_hook(OldDomain, Y) :-
    ( get_attr(Y, sim, Dom2) ->
        ord_intersection(OldDomain, Dom2, NewDomain),
        ( NewDomain == [] -> fail
        ; NewDomain = [Value] -> Y = Value
        ; put_attr(Y, sim, NewDomain)
        )
    ; var(Y) ->
        put_attr(Y, sim, OldDomain)
    ;
        ord_memberchk(Y, OldDomain)
    ).

attribute_goals(X) -->
    { get_attr(X, sim, List) },
    [kind(X, List)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible_type(N, Ty) :-
    integer_sign(N, Sign),
    possible_type_(Sign, N, Ty).

possible_type_(+, N, Kind\Size) :- Size in 1 .. sup, 2^Size #> N, kind(Kind).
possible_type_(-, N, s\Size) :- -1 * 2^(Size - 1) #=< N.

integer_sign(N, Sign) :-
    SignBit #<==> N #< 0,
    label([SignBit]),
    bit_sign(SignBit, Sign).

bit_sign(0, +).
bit_sign(1, -).

signed_size(S, Size) :- -1 * 2^(Size-1) #=< S, S #< 2^(Size-1).
unsigned_size(U, Size) :- 0 #=< U, U #< 2^Size.

signed_unsigned(S, U, Size) :-
    signed_size(S, Size),
    unsigned_size(U, Size),
    SignBit #<==> S #< 0,
    label([SignBit]),
    signed_unsigned_(SignBit, S, U, Size).
signed_unsigned_(1, S, U, Size) :- U #= S + 2^Size.
signed_unsigned_(0, U, U, _Size).


instr_info(lb, info{
    title: 'Load Byte',
    descr: 'Load a byte from memory into a register.',
    ex: ['lb w, [sp+12]'],
    syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
    sem: (
        let(?ptr, s(?rs) + s(?simm),
        u(?rd) <- m1(?ptr))
    ),
    tags: [mem, load, byte],
    module: [base]
}).
/*
instr_info(lw, info{
    title: 'Load Word',
    descr: 'Load a word from memory into a register.',
    ex: ['lw w, [sp+12]'],
    syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
    sem: (
        ?ptr := u((s(?rs) + s(?simm)) and #(-2)\16);
        ?rd <- {[?ptr + #1], [?ptr]}
    ),
    tags: [mem, load, word],
    module: [base]
}).
instr_info(li, info{
    title: 'Load Immediate',
    descr: 'Load an immediate value into a register.',
    ex: ['li x, 123'],
    syntax: { reg(r, ?rd), simm(?simm) },
    sem: s(?rd) <- s(?simm),
    tags: [sxt, data],
    module: [base]
}).
*/
