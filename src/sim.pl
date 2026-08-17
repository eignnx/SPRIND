/** <module> sim
Based on the semantics language used in Donald Knuth's "MMIX - A RISC Computer
for the New Millenium."
*/

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
:- use_module(isa).

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
    { AddrFit #= Addr mod 2^16 },
    { get_assoc(AddrFit, Before.next.mem, AlreadySet) ->
        throw(error(signal_already_set(next/mem/AddrFit, AlreadySet), _))
    ;
        put_assoc(AddrFit, MemNext, Value, NewMemNext),
        After = Before.put(next/mem, NewMemNext)
    },
    put(After).

add_binding(Var, Val, Ty) -->
    get(Before),
    { After = Before.put(bindings, [Var-Val-Ty | Before.bindings]) },
    put(After).


% :- det(<- // 2).

/*
In `u(X) <- u(A) + #1`,

                '<-':ZZ->ZZ->{State}Void
      u:?                 +:ZZ->ZZ->ZZ
      X:Bits\_       u:Bits\_->ZZ    #1:ZZ
                   A:Bits\_

That is, arithmetic expressions are only allowed on integers (`ZZ`). Bit strings
must undergo some transformation to be turned into integers, and integers must
undergo some (fallible) transformation to be turned back into a bit string.
*/

my_type_error(Expected, SrcTerm, Type) :-
    throw(error(wrong_type_error(#{
        expected_type: Expected,
        received_type: Type,
        src_term: SrcTerm
    }), _)).

must_be_type(Expected, SrcTerm, Type) :-
    ( Type = Expected -> true ;
        my_type_error(Expected, SrcTerm, Type)
    ).

must_be_z(SrcTerm, Type) :- must_be_type(z, SrcTerm, Type).
must_be_i(SrcTerm, Type) :- must_be_type(i(_), SrcTerm, Type).

:- det(lhs_size/2).
lhs_size($_, Size) :- isa:register_size(Size).
lhs_size($$Reg, Size) :- isa:sysregname_name_size_description(Reg, _, Size, _).
lhs_size(m(_), 8).

assign_lhs($Reg, Val) --> set_reg(Reg, Val).
assign_lhs($$SysReg, Val) --> set_sysreg(SysReg, Val).
assign_lhs(m(Addr0), Val) -->
    term_eval_type(Addr0, Addr, AddrTy),
    { must_be_z(Addr0, AddrTy) },
    set_memaddr(Addr, Val).

:- det(<- // 2).

% X is set to the bit pattern for which u(X) = Rhs. `Err` is an `i(1)` boolean.
% If Rhs does not fit in Lhs, set Err to 1, set Lhs to Rhs mod 2^sizeof(Lhs).
( u(Lhs, ?ErrVar) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    { must_be_z(Rhs0, RhsTy) },
    { lhs_size(Lhs, LhsSize) },
    { unsigned_size(Rhs, LhsSize) -> Err = 0 ; Err = 1 },
    { RhsFit #= Rhs mod 2^LhsSize },
    assign_lhs(Lhs, RhsFit),
    add_binding(ErrVar, Err, i(1)).

( s(Lhs, ?ErrVar) <- Rhs0 ) -->
    term_eval_type(Rhs0, Rhs, RhsTy),
    { must_be_z(Rhs0, RhsTy) },
    { lhs_size(Lhs, LhsSize) },
    { signed_size(Rhs, LhsSize) -> Err = 0 ; Err = 1 },
    { RhsFit #= Rhs mod 2^LhsSize },
    assign_lhs(Lhs, RhsFit),
    add_binding(ErrVar, Err, i(1)).


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


:- det(term_eval_type//3).

% Construct a "number", allow context to infer if it's a bit-string or integer.
term_eval_type(#N, N, Ty) --> { possible_type(N, Ty) }.

% Construct a bit-string with a specified size.
term_eval_type(i(N, Size), N, i(Size)) --> { unsigned_size(N, Size) }.

term_eval_type(?Var, Val, Ty) -->
    get(State),
    % Use memberchk/2 to allow variable shadowing
    { memberchk(Var-Val-Ty, State.bindings) }.

term_eval_type($Reg, Value, i(Bits)) -->
    { isa:gprreg(Reg) -> true ;
        bagof(R, isa:gprreg(R), Rs),
        type_error(oneof(Rs), Reg)
    },
    { isa:register_size(Bits) },
    get(State), { Value = State.get(curr/regs/Reg) }.

term_eval_type($$Reg, Value, i(Bits)) -->
    { isa:sysregname_name_size_description(Reg, _, Bits, _) -> true ;
        bagof(R, isa:sysreg(R), Rs),
        type_error(oneof(Rs), Reg)
    },
    get(State), { Value = State.get(curr/sysregs/Reg) }.

term_eval_type(u(A0), A, z) -->
    term_eval_type(A0, A, OldTy),
    { OldTy = i(_) -> true ; my_type_error(i(_), A0, OldTy) }.

term_eval_type(s(A0), A, z) -->
    term_eval_type(A0, A1, OldTy),
    { OldTy = i(_) -> true ; my_type_error(i(_), A0, OldTy) },
    % I'm representing a bit string as a Prolog integer which ought to always
    % be non-negative (therefore, convert from "unsigned" to signed).
    { i(Size) = OldTy, once(signed_unsigned(A, A1, Size)) }.

term_eval_type(m(Addr0), Value, i(8)) -->
    term_eval_type(Addr0, Addr, AddrTy),
    { must_be_z(Addr0, AddrTy) },
    get(State),
    { get_assoc(Addr, State.curr.mem, Value) -> true ; Value = 0 }.

term_eval_type(A0 + B0, Sum, z) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    { must_be_z(A0, TA), must_be_z(B0, TB) },
    { Sum #= A + B }.

term_eval_type(A0 /\ B0, Sum, i(Size)) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    { must_be_i(A0, TA), must_be_i(B0, TB) },
    { TA = i(Size), i(Size) = TB -> true ;
        throw(error('arguments to /\\ must be same size bit strings'(TA, TB), _))
    },
    { Sum #= A /\ B }.

term_eval_type(A0 \/ B0, Sum, i(Size)) -->
    term_eval_type(A0, A, TA),
    term_eval_type(B0, B, TB),
    { must_be_i(A0, TA), must_be_i(B0, TB) },
    { TA = i(Size), i(Size) = TB -> true ;
        throw(error('arguments to \\/ must be same size bit strings'(TA, TB), _))
    },
    { Sum #= A \/ B }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ty(X) :- ty(X, [
    i(_N), % The type of length-N bit-strings ({0, 1}^N).
    z      % The type of mathematical integers (ℤ).
]).

ty(X, Possibilities) :-
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
        % Succeed iff Y's value is in X's domain of possibilities
        memberchk(Y, XDomain)
        % NOTE: We're not using ord_memberchk/2 since it compares via == which
        %       will not bind variables (if `i(N)` is in the set, `i(2)` is
        %       considered not in the set).
        % NOTE: It's safe to use memberchk instead of member since Y is a nonvar
        %       and since the only possible values (z and i(_)) are non-unifiable
        %       with each other. We won't be missing out on additional solutions.
    ).

attribute_goals(X) -->
    { get_attr(X, sim, Domain) },
    [ty(X, Domain)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible_type(N, Ty) :-
    integer_sign(N, Sign),
    possible_type_(Sign, N, Ty).

integer_sign(N, Sign) :-
    SignBit #<==> N #< 0,
    label([SignBit]),
    bit_sign(SignBit, Sign).

bit_sign(0, +).
bit_sign(1, -).

possible_type_(-, _, z).
possible_type_(+, N, Ty) :-
    Size in 1 .. sup,
    N #< 2^Size,
    ty(Ty, [i(Size), z]).


signed_size(S, Size) :-
    must_be((integer;var), S),
    -1 * 2^(Size-1) #=< S, S #< 2^(Size-1).
unsigned_size(U, Size) :-
    must_be((integer;var), U),
    0 #=< U, U #< 2^Size.

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
        u(?rd, ?e1) <- u(m(?ptr + #1)) * #256 \/ u(m(?ptr)) % Defined as little-endian
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
instr_info(mulstep, info{
    title: 'Unsigned Multiplication Step',
    descr: 'Computes one step in a full 16-bit by 16-bit unsigned multiplication.',
    ex: ['mulstep x:y, z'],
    syntax: { reg(t, ?multiplicand_hi):reg(s, ?multiplicand_lo), reg(r, ?multiplier) },
    sem: (
        let(?mask, ~((?multiplier /\ #1) - #1)),
        let(?masked_lo, ?multiplicand_lo /\ ?mask),
        let(?masked_hi, ?multiplicand_hi /\ ?mask),
        lo($$mp) <- lo($$mp) + ?masked_lo,
        hi($$mp) <- hi($$mp) + ?masked_hi + attr(cpu/alu/carryout),
        let(?shift_cout, bit(?multiplicand_lo, (#reg_size_bits - #1))),
        ?multiplicand_lo <- ?multiplicand_lo << #1,
        ?multiplicand_hi <- ?multiplicand_hi << #1 + ?shift_cout,
        ?multiplier <- ?multiplier >> #1
    ),
    tags: [arith, shift],
    module: [mul]
}).
instr_info(lw, info{
    title: 'Load Word',
    descr: 'Load a word from memory into a register.',
    ex: ['lw w, [sp+12]'],
    syntax: { reg(r, ?rs), [reg(s, ?rd) + simm(?simm)] },
    sem: (
        let(?ptr, ((s(?rs) + s(?simm)) /\ #(-2)\16)\u),
        ?rd <- {m(?ptr + #1), m(?ptr)}
    ),
    tags: [mem, load, word],
    module: [base]
}).
