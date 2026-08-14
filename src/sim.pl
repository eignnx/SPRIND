:- module(sim, [
    op(5, fx, $),
    op(5, fx, $$),
    op(5, fx, #),
    op(5, fx, ?),
    op(950, xfx, <-),
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
:- op(950, xfx, <-).
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
    bindings: [],
    curr: #{
        regs: #{
            sp: 0,
            x:  0,
            y:  0,
            z:  0,
            w:  0,
            v:  0,
            a:  0,
            b:  0
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
        mem: MemCurr
    },
    next: #{
        regs: #{},
        sysregs: #{},
        mem: MemNext
    }
}) :-
    list_to_assoc([], MemCurr),
    list_to_assoc([], MemNext).

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

set_memaddr(Addr, Value) -->
    get(Before),
    { MemNext = Before.get(next/mem) },
    { get_assoc(Addr, Before.next.mem, AlreadySet) ->
        throw(error(signal_already_set(next/mem/Addr, AlreadySet), _))
    ;
        put_assoc(Addr, MemNext, Value, NewMemNext),
        After = Before.put(next/mem, NewMemNext)
    },
    put(After).


% :- det(<- // 2).

% X is set to the bit pattern for which u(X) = Rhs.
( u($Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(u($Reg) <- Rhs0, u\16, RhsTy),
    set_reg(Reg, Rhs).

( u($$Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(u($$Reg) <- Rhs0, u\16, RhsTy),
    set_sysreg(Reg, Rhs).

( s($Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(s($Reg) <- Rhs0, s\16, RhsTy),
    set_reg(Reg, Rhs).

( s($$Reg) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(s($$Reg) <- Rhs0, s\16, RhsTy),
    set_sysreg(Reg, Rhs).


( m(Addr0) <- Rhs0 ) -->
    term_eval_type(Addr0, Addr, AddrTy),
    term_compatible_types(m(Addr0), u\16, AddrTy),
    term_eval_type(Rhs0, Rhs, RhsTy),
    term_compatible_types(m(Addr0) <- Rhs0, _\8, RhsTy),
    set_memaddr(Addr, Rhs).


let(?Var, Rhs0) -->
    term_eval_type(Rhs0, Rhs, RhsSize),
    get(State),
    { NewState = State.put(bindings, [Var-Rhs-RhsSize | State.bindings]) },
    put(NewState).

% :- det(term_compatible_types//3).

term_compatible_types(SrcExpr, KindA\SizeA, KindB\SizeB) -->
    { KindA = KindB ->
        ( SizeA #= SizeB ->
            true
        ;
            ( SrcExpr =.. [Op, _A, B], ! ; SrcExpr =.. [Op, B] ),
            copy_term(SrcExpr-SizeA-SizeB, SrcExpr𞁞-SizeA𞁞-SizeB𞁞, Constraints),
            numbervars(SrcExpr𞁞-SizeA𞁞-SizeB𞁞-Constraints, 0, End, [singletons(true)]),
            format(
                atom(Msg),
                'Type mis-match in `~p`: `_\\~p` vs `_\\~p`~n\c
                 Additional constraints: ~p',
                [SrcExpr𞁞, SizeA𞁞, SizeB𞁞, Constraints]
            ),
            AType = _\SizeA𞁞,
            numbervars(AType, End, _EndEnd, [singletons(true)]),
            throw(error(domain_error(AType, B), context(interpretation_of_operator(Op), Msg)))
        )
    ;
        ( SrcExpr =.. [Op, _A, B], ! ; SrcExpr =.. [Op, B] ),
        copy_term(SrcExpr-KindA-KindB, SrcExpr𞁞-KindA𞁞-KindB𞁞, Constraints),
        numbervars(SrcExpr𞁞-KindA𞁞-KindB𞁞-Constraints, 0, _End1, [singletons(true)]),
        format(
            atom(Msg),
            'Type mis-match in `~p`: `~p\\_` vs `~p\\_`~n\c
             Additional constraints: ~p',
            [SrcExpr𞁞, KindA𞁞, KindB𞁞, Constraints]
        ),
        AType = KindA𞁞\SizeA,
        numbervars(AType, 0, _End2, [singletons(true)]),
        throw(error(domain_error(AType, B), context(interpretation_of_operator(Op), Msg)))
    }.

% :- det(term_eval_type//3).

term_eval_type(#N, N, Type) --> { possible_type(N, Type) }.
term_eval_type(?Var, Val, Type) --> get(State), { memberchk(Var-Val-Type, State.bindings) }.
term_eval_type($Reg, Value, i\16) --> get(State), { Value = State.get(curr/regs/Reg) }.
term_eval_type($$Reg, Value, i\16) --> get(State), { Value = State.get(curr/sysregs/Reg) }. % TODO: handle 32-bit sysregs
term_eval_type(u(A0), A, u\Size) -->
    term_eval_type(A0, A1, OldKind\Size),
    { OldKind = u -> A = A1
    ; OldKind = i -> A = A1
    ; OldKind = s -> signed_unsigned(A1, A, Size)
    }.
term_eval_type(s(A0), A, s\Size) -->
    term_eval_type(A0, A1, OldKind\Size),
    { OldKind = s -> A = A1
    ; OldKind = i -> signed_unsigned(A, A1, Size)
    ; OldKind = u -> signed_unsigned(A, A1, Size)
    }.
term_eval_type(m(Addr0), Value, i\8) -->
    term_eval_type(Addr0, Addr, AddrSize),
    term_compatible_types(m(Addr0), AddrSize, u\16),
    get(State),
    { get_assoc(Addr, State.curr.mem, Value) -> true ; Value = 0 }.
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

kind(X) :- kind(X, [i, s, u]).

kind(X, Possibilities) :-
    ( var(Possibilities) ->
        get_attr(X, sim, Possibilities)
    ;
        list_to_ord_set(Possibilities, PossibilitiesSet),
        put_attr(Y, sim, PossibilitiesSet),
        X = Y % merge (via intersection) the given possibilities in
    ).

% Handle the unification `X = Y` like so:
attr_unify_hook(XDomain, Y) :-
    ( get_attr(Y, sim, YDomain) -> % Y is already an attributed variable
        ord_intersection(XDomain, YDomain, NewDomain),
        ( NewDomain == [] -> fail % No possible values in domain -> impossible to succeed
        ; NewDomain = [Value] -> Y = Value % Unique possibility -> bind it to the var
        ; put_attr(Y, sim, NewDomain) % otherwise update Y with the merged domain
        )
    ; var(Y) -> % Y is a non-attributed variable
        put_attr(Y, sim, XDomain) % Its domain ought to be just X's domain
    ; % Else, Y is a nonvar
        ord_memberchk(Y, XDomain) % Succeed if Y's value is in X's domain of possibilities
    ).

attribute_goals(X) -->
    { get_attr(X, sim, Domain) },
    ( { Domain = [i, s, u] } -> [kind(X)]
    ; [kind(X, Domain)]
    ).

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
        let(?ptr, s(?rs) + s(?simm)),
        u(?rd) <- m1(?ptr)
    ),
    tags: [mem, load, byte],
    module: [base]
}).
instr_info(lw, info{
    title: 'Load Word',
    descr: 'Load a word from memory into a register.',
    ex: ['lw w, [sp+12]'],
    syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
    sem: (
        let(?ptr, u((s(?rs) + s(?simm)) /\ #(-2)\16)),
        u(?rd) <- u(m(?ptr + #1)) * #256 \/ u(m(?ptr)) % Defined as little-endian
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


term_evaluation(Lhs <- Rhs) -->
    get(S0),
    { Prev = S0.get(signals/Lhs) ->
        throw(error('signal assigned more than once'(Lhs, Prev), _))
    ;
        S1 = S0.put(signals/Lhs, Rhs)
    },
    put(S1).

term_evaluation(A ; B) -->
    term_evaluation(A),
    term_evaluation(B).

