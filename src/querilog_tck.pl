/** <module> querilog_tck
The static type checker for querilog. Transforms a raw ast by ensuring all
values with implicit sizes (like `#123`) are explicitly sized after type
checking.
*/
:- module(querilog_tck, [
    stmt_typechecked//2,
    typecheck_instr_sems/0
]).

:- use_module(library(clpfd)).
:- use_module(querilog_syntax).
:- use_module(isa, [register_size/1]).
:- use_module(utils).
:- use_module(derive).
:- use_module(consts).

:- use_module(sem_querilog, [instr_info/2]).
typecheck_instr_sems :-
    findall(Status, typecheck_some_instr_sem(Status), Statuses),
    exclude(=(success), Statuses, Failures),
    length(Statuses, NTotal),
    length(Failures, NFail),
    utils:list_enumerated1(Failures, FailuresEnum),
    maplist([N-F]>>(
        arg(1, F, Instr),
        format('~t~d.~3| ~p:~t~14|~p~n', [N, Instr, F])
    ), FailuresEnum),
    format('RESULTS: ~d failures out of ~d instructions~n', [NFail, NTotal]).
typecheck_some_instr_sem(Status) :-
    sem_querilog:instr_info(Instr, Info),
    Sem = Info.sem,
    catch(
        ( typecheck_instr(Instr) ->
            Status = success
        ;
            Status = typechecking_failed(Instr, Sem)
        ),
        error(E, _),
        Status = exception(Instr, E)
    ).

typecheck_instr(Instr) :-
    sem_querilog:instr_info(Instr, Info),
    isa:fmt_instr(Fmt, Instr),
    once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
    syntax_operands(Info.syntax, Operands),
    maplist(tcx_binding_from_syn_operands(ImmBits), Operands, Tcx),
    init_state(S0, Tcx),
    phrase(stmt_typechecked(Info.sem, _TypeChecked), [S0], [_S]).

typecheck(Statements) :- typecheck([], Statements).
typecheck(Tcx, Statements) :-
    init_state(S0, Tcx),
    phrase(stmt_typechecked(Statements, _TypeChecked), [S0], [_S]).
term_size(Term, Size) :-
    init_state(S0),
    phrase(term_size_resolved(Term, Size, _Resolved), [S0], [_S]).

tcx_binding_from_syn_operands(ImmBits, Operand, ?VarName-Size) :-
    operand_immbits_name_size(Operand, ImmBits, VarName, Size).

operand_immbits_name_size(   imm(?Name), ImmBits, Name, ImmBits).
operand_immbits_name_size(  simm(?Name), ImmBits, Name, ImmBits).
operand_immbits_name_size(reg(_, ?Name),       _, Name,    Bits) :-
    isa:register_size(Bits).

syntax_operands({}, []).
syntax_operands({CommaList}, VarDecls) :-
    comma_list(CommaList, Operands),
    phrase(operand_vardecl(Operands), VarDecls).
syntax_operands(Lhs -> _Rhs, Operands) :- syntax_operands(Lhs, Operands).

operand_vardecl([]) --> [].
operand_vardecl([X|Xs]) -->
    ( { [Inner] = X } -> expand_bracket_content(Inner) ; [X]),
    operand_vardecl(Xs).
expand_bracket_content(A + B) --> !, [A], [B].
expand_bracket_content(A) --> [A].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bound_integer_size(N, Size) :-
    Size in 1..sup,
    signbit_compare(Sign, N),
    bound_integer_size_(Sign, N, Size).
bound_integer_size_(0, N, Size) :- 0 #=< N, N #< 2^Size.
bound_integer_size_(1, N, Size) :- -1 * 2^(Size-1) #=< N, N #< 2^(Size-1).


:- det(term_size_resolved//3).
:- discontiguous(term_size_resolved//3).

%! term_size_resolved(+Term0, -Size:nonneg, -Term) is det.
%
% Removes unsized integer literals like `#123` by inferring their size. Also
% performs type checking.
%
term_size_resolved(#Term0, Size, Term) --> !,
    { N\Size = Term0 ->
        ( bound_integer_size(N, Size) -> Term = #N\Size ;
            format(atom(Msg), 'Integer ~d does not fit in ~d bits', [N, Size]),
            throw(error(syntax_error(Msg, #N\Size), _))
        )
    ; integer(Term0), N = Term0 ->
        bound_integer_size(N, Size),
        Term = #N\Size % Defer size inference for later
    ; ( atom(Term0) ; _:_ = Term0 ), Const = Term0 ->
        once(consts:def_const(Const, N)),
        !,
        Size in 1..sup,
        % Widest possible bounds -> Size approx(>=) lg(|N|)
        -1 * 2^(Size - 1) #=< N, N #< 2^Size,
        Term = #N\Size % Defer size inference for later
    }.

term_size_resolved($Reg, Size, $Reg) --> !,
    ( { isa:gprreg(Reg) } ->
        { isa:register_size(Size) }
    ; { Reg = ?Var } ->
        { isa:gpr_count_bits(GprCountBits) },
        term_size_resolved(?Var, GprCountBits, _)
    ).

term_size_resolved($$Reg, Size, $$Reg) --> !, { isa:sysreg_size(Reg, Size) }.

term_size_resolved(?Var, Size, ?Var) --> !,
    get_state(State),
    { memberchk(?Var-Size, State.bindings) -> true ;
        throw(error(unbound_identifier(?Var, State.bindings), _))
    }.

term_size_resolved(~E0, Size, ~E) --> !,
    term_size_resolved(E0, Size, E).

term_size_resolved(mem(Addr0), 8, mem(Addr)) --> !,
    term_size_resolved(Addr0, AddrSz, Addr),
    { AddrSz = 16 -> true ;
        throw(error(incompatible_size(#{
            op: mem,
            subterm: [Addr0],
            expected_size: 16,
            actual_size: [AddrSz]
        }), _))
    }.

term_size_resolved(lo(E0), Size, lo(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size * 2 #= ESz -> true ;
        throw(error(invalid_argument(#{
            op: lo,
            subterm:  [E0],
            actual_size: [ESz],
            note: 'lo(..) requires an argument with an even number of bits'
        }), _))
    }.

term_size_resolved(hi(E0), Size, hi(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size * 2 #= ESz -> true ;
        throw(error(invalid_argument(#{
            op: lo,
            subterm:  [E0],
            actual_size: [ESz],
            note: 'hi(..) requires an argument with an even number of bits'
        }), _))
    }.

term_size_resolved(sxt(E0), Size, sxt(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size in 1..sup },
    { ESz #< Size -> true ;
        % Unreachable?
        throw(error(unsatisfiable_size_constraint(#{
            constraint: ESz #< Size,
            term: sxt(E0)
        }), _))
    }.

term_size_resolved(zxt(E0), Size, zxt(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size in 1..sup },
    { ESz #< Size -> true ;
        % Unreachable?
        throw(error(unsatisfiable_size_constraint(#{
            constraint: ESz #< Size,
            term: zxt(E0)
        }), _))
    }.

term_size_resolved(zxt_log2(E0), Size, zxt_log2(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { round_up_to_next_pow2(ESz, Size) }.

term_size_resolved({Es0}, Size, {Es}) --> !,
    { comma_list(Es0, Es1) },
    terms_sizes_resolveds(Es1, [S|Sizes], Es),
    { foldl([A, B, C]>>(A + B #= C), Sizes, S, Size) }.

binopterm_size_resolved(Op, A0, B0, Size, Term) -->
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz = BSz -> true ;
        term_clpfd_goals(ASz-BSz-Size, Goals),
        Err0 = incompatible_sizes(#{
            op: Op,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            goals: Goals
        }),
        copy_term(Err0, Err, _),
        numbervars(Err),
        throw(error(Err, _))
    },
    { Size = ASz },
    { Term =.. [Op, A, B] }.

term_size_resolved(A0 + B0, Size, Term) --> !,
    binopterm_size_resolved(+, A0, B0, Size, Term).
term_size_resolved(A0 - B0, Size, Term) --> !,
    binopterm_size_resolved(-, A0, B0, Size, Term).
term_size_resolved(A0 and B0, Size, Term) --> !,
    binopterm_size_resolved(and, A0, B0, Size, Term).
term_size_resolved(A0 or B0, Size, Term) --> !,
    binopterm_size_resolved(or, A0, B0, Size, Term).
term_size_resolved(A0 xor B0, Size, Term) --> !,
    binopterm_size_resolved(xor, A0, B0, Size, Term).

term_size_resolved(A0 << B0, Size, A << B) --> !,
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz #= 2^BSz -> true ;
        term_clpfd_goals(ASz-BSz, Goals),
        Err0 = incompatible_sizes(#{
            op: <<,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            violation: not_power_of_2(\+ ASz = 2^BSz),
            goals: Goals
        }),
        copy_term(Err0, Err, _),
        numbervars(Err),
        throw(error(Err, _))
    },
    { Size = ASz }.

term_size_resolved(A0 >> B0, Size, A >> B) --> !,
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz #= 2^BSz -> true ;
        term_clpfd_goals(ASz-BSz, Goals),
        Err0 = incompatible_sizes(#{
            op: >>,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            violation: not_power_of_2(\+ ASz = 2^BSz),
            goals: Goals
        }),
        copy_term(Err0, Err, _),
        numbervars(Err),
        throw(error(Err, _))
    },
    { Size = ASz }.

term_size_resolved(A0\Size, Size, A\Size) --> !,
    term_size_resolved(A0, ASz, A),
    { Size #=< ASz -> true ;
        throw(error(slice_can_only_truncate_not_extend(A0\Size), _))
    }.

/*
[_,_,x1,x2,x3,x4,x5,x6] << [s1,s2,s3]
*/
term_size_resolved(bit(Tgt0, Idx0), 1, bit(Tgt, Idx)) --> !,
    term_size_resolved(Tgt0, TgtSz, Tgt),
    term_size_resolved(Idx0, IdxSz, Idx),
    { TgtSz #= 2^IdxSz -> true ;
        term_clpfd_goals(TgtSz-IdxSz, Goals),
        Err0 = incompatible_sizes(#{
            op: bit,
            subterms: [Tgt0, Idx0],
            subterm_sizes: [TgtSz, IdxSz],
            violation: not_power_of_2(\+ TgtSz = 2^IdxSz),
            goals: Goals
        }),
        copy_term(Err0, Err, _),
        numbervars(Err),
        throw(error(Err, _))
    }.
term_size_resolved(bitslice(Tgt0, Lo..Hi), Size, bitslice(Tgt, Lo..Hi)) --> !,
    term_size_resolved(Tgt0, TgtSz, Tgt),
    { integer(Lo) -> true ; throw(error(non_const_bitslice_index(Lo), _)) },
    { integer(Hi) -> true ; throw(error(non_const_bitslice_index(Hi), _)) },
    { Lo < Hi -> true ; throw(error(reversed_bitslice_bounds(Lo..Hi), _)) },
    { TgtSz >= Hi -> true ;
        throw(error(incompatible_sizes(#{
            op: bitslice,
            subterms: [Tgt0, Lo..Hi],
            subterm_sizes: [TgtSz, Lo..Hi],
            violation: Hi =< TgtSz
        }), _))
    },
    { Size #= Hi - Lo },
[].

term_size_resolved(if(Cond0, Consq0, Alt0), Size, if(Cond, Consq, Alt)) --> !,
    term_size_resolved(Cond0, CondSz, Cond),
    term_size_resolved(Consq0, ConsqSz, Consq),
    term_size_resolved(Alt0, AltSz, Alt),
    { CondSz = 1 -> true ;
        throw(error(incompatible_size(#{
            op: if(cond, _, _),
            subterm: [Cond0],
            expected_size: 1,
            actual_size: [CondSz]
        }), _))
    },
    { ConsqSz = AltSz -> true ;
        throw(error(incompatible_sizes(#{
            op: if(_, consq, alt),
            subterms: [Consq0, Alt0],
            subterm_sizes: [ConsqSz, AltSz]
        }), _))
    },
    { Size = ConsqSz }.

% Catchall error case:
term_size_resolved(Term, _, _) -->
    { Term =.. [Functor|_] },
    !,
    { throw(error(unimplemented(Functor, term_size_resolved(Term, _, _)), _)) }.
term_size_resolved(Term, _, _) -->
    { throw(error(unimplemented(Term, term_size_resolved(Term, _, _)), _)) }.

terms_sizes_resolveds([], [], []) --> [].
terms_sizes_resolveds([T|Ts], [S|Ss], [R|Rs]) -->
    term_size_resolved(T, S, R),
    terms_sizes_resolveds(Ts, Ss, Rs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPECHECKER STATE MONAD %%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(State) :- init_state(State, []).
init_state(tck_state{
    bindings: Bindings,
    pending_assignments: []
}, Bindings).

add_binding(?VarName, Size) -->
    get_state(Before),
    { OldBindings = Before.bindings },
    { After = Before.put(bindings, [?VarName - Size | OldBindings]) },
    put_state(After).

add_pending_assignment(Lhs) -->
    get_state(Before),
    { memberchk(Lhs, Before.pending_assignments) ->
        throw(error(duplicate_assignment(#{target: Lhs}), _))
    ; true },
    { NewBindings = [Lhs | Before.pending_assignments] },
    { After = Before.put(pending_assignments, NewBindings) },
    put_state(After).


:- det(stmt_typechecked//2).
:- discontiguous(stmt_typechecked//2).

%! stmt_typechecked(+P0:statements, -P:typechecked(statements)) is det.
%
% Same as `term_size_resolved` except for statements.
stmt_typechecked(todo, todo) --> !.

stmt_typechecked((A0 ; B0), (A ; B)) --> !,
    stmt_typechecked(A0, A),
    stmt_typechecked(B0, B).

stmt_typechecked((?VarName := Rhs0), (?VarName := Rhs)) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    add_binding(?VarName, RhsSz).

stmt_typechecked(Lhs0 <- Rhs0, Lhs <- Rhs) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    lhs_size_typechecked(Lhs0, LhsSz, Lhs),
    { LhsSz = RhsSz  -> true ;
        throw(error(incompatible_sizes(#{
            op: <-,
            subterms: [Lhs0, Rhs0],
            subterm_sizes: [LhsSz, RhsSz]
        }), _))
    }.

stmt_typechecked(AdderDict0, AdderDict) --> { is_dict(AdderDict0, adder) }, !,
    { adder{x: X0, y: Y0, sum: Sum0} :< AdderDict0 -> true ;
        throw(error(incorrect_adder_spec(
            must_include_keys([x, y, sum]),
            got(AdderDict0)
        ), _))
    },
    term_size_resolved(X0, XSz, X),
    term_size_resolved(Y0, YSz, Y),
    { XSz = YSz -> true ;
        throw(error(incompatible_sizes(#{
            op: adder{x:_, y:_},
            subterms: [X0, Y0],
            subterm_sizes: [XSz, YSz]
        }), _))
    },
    lhs_size_typechecked(Sum0, SumSz, Sum),
    { XSz = SumSz -> true ;
        throw(error(incompatible_sizes(#{
            op: adder{x:_, sum: _},
            subterms: [X0, Sum0],
            subterm_sizes: [XSz, SumSz]
        }), _))
    },
    { AdderDictData = [x=X, y=Y, sum=Sum | Rest0] },
    ( { Cin0 = AdderDict0.get(carryin) } ->
        term_size_resolved(Cin0, CinSz, Cin),
        { CinSz = 1 -> true ;
            throw(error(must_be_one_bit(carry_in, got_size(CinSz)), _))
        },
        { Rest0 = [carryin=Cin | Rest1] }
    ;
        { Rest0 = Rest1 }
    ),
    ( { Cout0 = AdderDict0.get(carryout) } ->
        lhs_size_typechecked(Cout0, CoutSz, Cout),
        { CoutSz = 1 -> true ;
            throw(error(must_be_one_bit(carry_out, got_size(CoutSz)), _))
        },
        { Rest1 = [carryout=Cout | Rest2] }
    ;
        { Rest1 = Rest2 }
    ),
    ( { SignIn0 = AdderDict0.get(signin) } ->
        lhs_size_typechecked(SignIn0, SignInSz, SignIn),
        { SignInSz = 1 -> true ;
            throw(error(must_be_one_bit(sign_in, got_size(SignInSz)), _))
        },
        { Rest2 = [signin=SignIn] }
    ;
        { Rest2 = [] }
    ),
    { dict_create(AdderDict, adder, AdderDictData) },
[].


stmt_typechecked(Other, _) -->
    { Other =.. [Functor|_] },
    { throw(error(unimplemented(Functor, stmt_typechecked(Other, _)), _)) }.

stmts_typecheckeds([], []) --> [].
stmts_typecheckeds([S0|Ss0], [S|Ss]) -->
    stmt_typechecked(S0, S),
    stmts_typecheckeds(Ss0, Ss).

lhs_size_typechecked($Reg, RegSz, $Reg) --> !,
    { isa:register_size(RegSz) },
    add_pending_assignment($Reg).
lhs_size_typechecked($$Reg, RegSz, $$Reg) --> !,
    { isa:sysreg_size(Reg, RegSz) },
    add_pending_assignment($$Reg).
lhs_size_typechecked(?Var, UnknownSize, ?Var) --> !,
    add_binding(?Var, UnknownSize).
lhs_size_typechecked(bit(Tgt0, Idx0), 1, bit(Tgt, Idx)) --> !,
    term_size_resolved(bit(Tgt0, Idx0), _One, bit(Tgt, Idx)),
    add_pending_assignment(Tgt).
lhs_size_typechecked(mem(Addr0), 8, mem(Addr)) --> !,
    term_size_resolved(mem(Addr0), _AddrSz, mem(Addr)).
lhs_size_typechecked({ Components0 }, ComponentsSz, { Components }) --> !,
    { comma_list(Components0, Components1) },
    terms_sizes_resolveds(Components1, Sizes, Components2),
    { sumlist(Sizes, ComponentsSz) },
    { maplist([Comp, Comp <- #0]>>true, Components2, Stmts) },
    stmts_typecheckeds(Stmts, _),
    { comma_list(Components, Components2) }.
lhs_size_typechecked(Lhs0, _, _) -->
    { Lhs0 =.. [Functor|_] },
    { LhsErr =.. [Functor, '...'] },
    { throw(error(unimplemented(
            LhsErr <- '...',
            stmt_typechecked(Lhs0 <- '...', _)
        ), _))
    }.
