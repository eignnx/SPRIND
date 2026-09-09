/** <module> querilog_tck
The static type checker for querilog. Transforms a raw ast by ensuring all
values with implicit sizes (like `#123`) are explicitly sized after type
checking.

Glossary:
- Contassign, Cont-assignment: Continuous assignment (the := operator)
- Clkassign, Clk-assignment: Clock assignment (the <- operator)
- Varkind: The kind of a variable (like ?x). See `varkind/1` for possibilities.
*/
:- module(querilog_tck, [
    stmt_typechecked//2,
    typecheck_instr_sems/0,
    querilog_mod/2
]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(querilog_syntax).
:- use_module(isa, [register_size/1]).
:- use_module(utils).
:- use_module(derive).
:- use_module(consts).

:- multifile(querilog_mod/2).

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

tcx_binding_from_syn_operands(ImmBits, Operand, ?VarName-Dir-Size) :-
    operand_immbits_name_size_dir(Operand, ImmBits, VarName, Size, Dir).

operand_immbits_name_size_dir(   imm(?Name), ImmBits, Name, ImmBits, net(param(in))).
operand_immbits_name_size_dir(  simm(?Name), ImmBits, Name, ImmBits, net(param(in))).
operand_immbits_name_size_dir(reg(_, ?Name),       _, Name,    Bits, reg) :-
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
            throw_error(syntax_error(Msg, #N\Size))
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
    lookup_var_decl(?Var, Dir, Size),
    { Dir == reg ->
        throw_error(cannot_read_from_out_port(?Var))
    ; true }.

term_size_resolved(~E0, Size, ~E) --> !,
    term_size_resolved(E0, Size, E).

term_size_resolved(mem(Addr0), 8, mem(Addr)) --> !,
    term_size_resolved(Addr0, AddrSz, Addr),
    { AddrSz = 16 -> true ;
        throw_error(incompatible_size, #{
            op: mem,
            subterm: [Addr0],
            expected_size: 16,
            actual_size: [AddrSz]
        })
    }.

term_size_resolved(lo(E0), Size, lo(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size * 2 #= ESz -> true ;
        throw_error(invalid_argument, #{
            op: lo,
            subterm:  [E0],
            actual_size: [ESz],
            note: 'lo(..) requires an argument with an even number of bits'
        })
    }.

term_size_resolved(hi(E0), Size, hi(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size * 2 #= ESz -> true ;
        throw_error(invalid_argument, #{
            op: lo,
            subterm:  [E0],
            actual_size: [ESz],
            note: 'hi(..) requires an argument with an even number of bits'
        })
    }.

term_size_resolved(sxt(E0), Size, sxt(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size in 1..sup },
    { ESz #< Size -> true ;
        % Unreachable?
        throw_error(unsatisfiable_size_constraint, #{
            constraint: ESz #< Size,
            term: sxt(E0)
        })
    }.

term_size_resolved(zxt(E0), Size, zxt(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { Size in 1..sup },
    { ESz #< Size -> true ;
        % Unreachable?
        throw_error(unsatisfiable_size_constraint, #{
            constraint: ESz #< Size,
            term: zxt(E0)
        })
    }.

term_size_resolved(zxt_log2(E0), Size, zxt_log2(E)) --> !,
    term_size_resolved(E0, ESz, E),
    { round_up_to_next_pow2(ESz, Size) }.

term_size_resolved({Es0}, Size, {Es}) --> !,
    { comma_list(Es0, Es1) },
    terms_sizes_resolveds(Es1, Sizes, Es),
    { clpfd_sumlist(Sizes, Size) }.

binopterm_size_resolved(Op, A0, B0, Size, Term) -->
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz = BSz -> true ;
        throw_error(incompatible_sizes, #{
            op: Op,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz]
        })
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
term_size_resolved(A0 == B0, 1, Term) --> !,
    binopterm_size_resolved(==, A0, B0, _, Term).

term_size_resolved(A0 << B0, Size, A << B) --> !,
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz #= 2^BSz -> true ;
        throw_error(incompatible_sizes, #{
            op: <<,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            violation: not_power_of_2(\+ ASz = 2^BSz)
        })
    },
    { Size = ASz }.

term_size_resolved(A0 >> B0, Size, A >> B) --> !,
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz #= 2^BSz -> true ;
        throw_error(incompatible_sizes, #{
            op: >>,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            violation: not_power_of_2(\+ ASz = 2^BSz)
        })
    },
    { Size = ASz }.

term_size_resolved(A0\Size, Size, A\Size) --> !,
    term_size_resolved(A0, ASz, A),
    { Size #=< ASz -> true ;
        throw_error(slice_can_only_truncate_not_extend(A0\Size))
    }.

/*
[_,_,x1,x2,x3,x4,x5,x6] << [s1,s2,s3]
*/
term_size_resolved(bit(Tgt0, Idx0), 1, bit(Tgt, Idx)) --> !,
    term_size_resolved(Tgt0, TgtSz, Tgt),
    term_size_resolved(Idx0, IdxSz, Idx),
    { TgtSz #= 2^IdxSz -> true ;
        throw_error(incompatible_sizes, #{
            op: bit,
            subterms: [Tgt0, Idx0],
            subterm_sizes: [TgtSz, IdxSz],
            violation: not_power_of_2(\+ TgtSz = 2^IdxSz)
        })
    }.
term_size_resolved(bitslice(Tgt0, Lo..Hi), Size, bitslice(Tgt, Lo..Hi)) --> !,
    term_size_resolved(Tgt0, TgtSz, Tgt),
    { integer(Lo) -> true ; throw_error(non_const_bitslice_index(Lo)) },
    { integer(Hi) -> true ; throw_error(non_const_bitslice_index(Hi)) },
    { Lo < Hi -> true ; throw_error(reversed_bitslice_bounds(Lo..Hi)) },
    { TgtSz >= Hi -> true ;
        throw_error(incompatible_sizes, #{
            op: bitslice,
            subterms: [Tgt0, Lo..Hi],
            subterm_sizes: [TgtSz, Lo..Hi],
            violation: Hi =< TgtSz
        })
    },
    { Size #= Hi - Lo },
[].

term_size_resolved(if(Cond0, Consq0, Alt0), Size, if(Cond, Consq, Alt)) --> !,
    term_size_resolved(Cond0, CondSz, Cond),
    term_size_resolved(Consq0, ConsqSz, Consq),
    term_size_resolved(Alt0, AltSz, Alt),
    { CondSz = 1 -> true ;
        throw_error(incompatible_size, #{
            op: if(cond, _, _),
            subterm: [Cond0],
            expected_size: 1,
            actual_size: [CondSz]
        })
    },
    { ConsqSz = AltSz -> true ;
        throw_error(incompatible_sizes, #{
            op: if(_, consq, alt),
            subterms: [Consq0, Alt0],
            subterm_sizes: [ConsqSz, AltSz]
        })
    },
    { Size = ConsqSz }.

% Catchall error case:
term_size_resolved(Term, _, _) -->
    { Term =.. [Functor|_] },
    !,
    { throw_error(unimplemented(Functor, term_size_resolved(Term, _, _))) }.
term_size_resolved(Term, _, _) -->
    { throw_error(unimplemented(Term, term_size_resolved(Term, _, _))) }.

terms_sizes_resolveds([], [], []) --> [].
terms_sizes_resolveds([T|Ts], [S|Ss], [R|Rs]) -->
    term_size_resolved(T, S, R),
    terms_sizes_resolveds(Ts, Ss, Rs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VARKIND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varkind(reg). % A memory element; may be read from and/or clkassigned to.
varkind(net(local)). % A locally-defined wire; must be contassigned to, then
                     % may be read from.
varkind(net(param(in))). % A module parameter that can only be read from.
varkind(net(param(out))). % A module parameter that can only be contassigned to.


%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPECHECKER STATE MONAD %%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(State) :- init_state(State, []).
init_state(tck_state{
    % Holds info about variables: Name-VarKind-Size
    var_decls: Decls,

    % Ensures a net is not cont-assigned to more than once. Contains variable
    % names.
    contassigns: [],

    % Ensures a reg is not clk-assigned to more than once. Contains variables
    % (like ?x), registers ($a), sysregs ($$ts), mem references (mem(#123)),
    % bit values (bit($a, #13)).
    pending_clkassigns: []
}, Decls).

decl_var(?VarName, VarKind, Size) -->
    ( is_var_decl(?VarName, PrevKind, PrevSize) ->
        { throw_error(duplicate_decl, #{
            target: ?VarName,
            prev_kind: PrevKind,
            prev_size: PrevSize
        }) }
    ; [] ),
    get_state(S0),
    { S = S0.put(var_decls, [?VarName-VarKind-Size | S0.var_decls]) },
    put_state(S).

lookup_var_decl(?VarName, VarKind, Size) -->
    ( is_var_decl(?VarName, VarKind, Size) -> [] ;
        { throw_error(undeclared_var(?VarName)) }
    ).

is_var_decl(?VarName, VarKind, Size) -->
    get_state(S0),
    % Disallow variable shadowing.
    { memberchk(?VarName-VarKind-Size, S0.var_decls) }.

add_contassign(?VarName) -->
    get_state(S0),
    lookup_var_decl(?VarName, VarKind, _Size),
    { memberchk(VarKind, [net(local), net(param(out))]) -> true ;
        throw_error(contassign_to_invalid_varkind, #{
            target: ?VarName,
            varkind: VarKind,
            expected_varkind: [net(local), net(param(out))]
        })
    },
    { memberchk(?VarName, S0.contassigns) ->
        throw_error(multiple_contassigns(?VarName))
    ; true },
    { S = S0.put(contassigns, [?VarName | S0.contassigns]) },
    put_state(S).

is_contassign(?VarName) -->
    get_state(State),
    { memberchk(?VarName, State.contassigns) }.

is_pending_clkassign(?VarName) -->
    get_state(State),
    { memberchk(?VarName, State.pending_clkassigns) }.

add_pending_clkassign(?VarName) --> !,
    lookup_var_decl(?VarName, VarKind, _Size),
    { reg = VarKind -> true ;
        throw_error(clkassign_to_invalid_varkind, #{
            target: ?VarName,
            varkind: VarKind,
            expected_varkind: reg
        })
    },
    get_state(S0),
    { After = S0.put(pending_clkassigns, [?VarName | S0.pending_clkassigns]) },
    put_state(After).
add_pending_clkassign(bit(Lhs, _)) --> !,
    % Register the underlying reg kind as having a pending assignment.
    add_pending_clkassign(Lhs).
add_pending_clkassign({Components}) --> !,
    { comma_list(Components, Cs) },
    % Register each component as having a pending assignment.
    sequence(add_pending_clkassign, Cs).
add_pending_clkassign(Lhs) --> !,
    { memberchk(Lhs, [$_, $$_, mem(_)]) },
    get_state(S0),
    { After = S0.put(pending_clkassigns, [Lhs | S0.pending_clkassigns]) },
    put_state(After).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STMT_TYPECHECKED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    ( is_var_decl(?VarName, _VarKind, VarSz) -> 
        { RhsSz = VarSz -> true ;
            throw_error(incompatible_sizes, #{
                op: (:=),
                subterms: [?VarName, Rhs0],
                subterm_sizes: [VarSz, RhsSz]
            })
        }
    ;
        decl_var(?VarName, net(local), RhsSz)
    ),
    add_contassign(?VarName).
stmt_typechecked((Lhs0 := Rhs0), (Lhs := Rhs)) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    contassign_lhs_size_typechecked(Lhs0, LhsSz, Lhs),
    { RhsSz = LhsSz -> true ;
        throw_error(incompatible_sizes, #{
            op: (:=),
            subterms: [Lhs0, Rhs0],
            subterm_sizes: [LhsSz, RhsSz]
        })
    }.

stmt_typechecked((Lhs0 <- Rhs0), (Lhs <- Rhs)) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    clkassign_lhs_size_typechecked(Lhs0, LhsSz, Lhs),
    { LhsSz = RhsSz  -> true ;
        throw_error(incompatible_sizes, #{
            op: (<-),
            subterms: [Lhs0, Rhs0],
            subterm_sizes: [LhsSz, RhsSz]
        })
    }.

stmt_typechecked(AdderDict0, AdderDict) --> { is_dict(AdderDict0, adder) }, !,
    { adder{x: X0, y: Y0, sum: Sum0} :< AdderDict0 -> true ;
        throw_error(incorrect_adder_spec(
            must_include_keys([x, y, sum]),
            got(AdderDict0)
        ))
    },
    term_size_resolved(X0, XSz, X),
    term_size_resolved(Y0, YSz, Y),
    { XSz = YSz -> true ;
        throw_error(incompatible_sizes, #{
            op: adder{x:_, y:_},
            subterms: [X0, Y0],
            subterm_sizes: [XSz, YSz]
        })
    },
    contassign_lhs_size_typechecked(Sum0, SumSz, Sum),
    { XSz = SumSz -> true ;
        throw_error(incompatible_sizes, #{
            op: adder{x:_, sum: _},
            subterms: [X0, Sum0],
            subterm_sizes: [XSz, SumSz]
        })
    },
    { AdderDictData = [x=X, y=Y, sum=Sum | Rest0] },
    ( { Cin0 = AdderDict0.get(carryin) } ->
        term_size_resolved(Cin0, CinSz, Cin),
        { CinSz = 1 -> true ;
            throw_error(must_be_one_bit(carry_in, got_size(CinSz)))
        },
        { Rest0 = [carryin=Cin | Rest1] }
    ;
        { Rest0 = Rest1 }
    ),
    ( { Cout0 = AdderDict0.get(carryout) } ->
        contassign_lhs_size_typechecked(Cout0, CoutSz, Cout),
        { CoutSz = 1 -> true ;
            throw_error(must_be_one_bit(carry_out, got_size(CoutSz)))
        },
        { Rest1 = [carryout=Cout | Rest2] }
    ;
        { Rest1 = Rest2 }
    ),
    ( { SignIn0 = AdderDict0.get(signin) } ->
        contassign_lhs_size_typechecked(SignIn0, SignInSz, SignIn),
        { SignInSz = 1 -> true ;
            throw_error(must_be_one_bit(sign_in, got_size(SignInSz)))
        },
        { Rest2 = [signin=SignIn] }
    ;
        { Rest2 = [] }
    ),
    { dict_create(AdderDict, adder, AdderDictData) },
[].

stmt_typechecked(Dict0, Dict) --> { is_dict(Dict0, ModName) }, !,
    { mod_sig_body(ModName, Sig, _Body) -> true ;
        throw_error(instantiation_of_undefined_module(ModName))
    },
    { dict_pairs(Dict0, ModName, Pairs0) },
    sig_kwargs_typechecked(Pairs0, Sig, Pairs),
    { dict_pairs(Dict, ModName, Pairs) },
[].

% Typechecks a module instantiation (module use/when one is wired up).
sig_kwargs_typechecked([], _Sig, []) --> [].
sig_kwargs_typechecked(
    [Kwarg-Val0|Kws0],
    Sig,
    [Kwarg-Val|Kws]
) -->
    { PortSpec = Sig.get(Kwarg) -> true ;
        throw_error(invalid_module_kwarg(Kwarg, Sig))
    },
    { portspec_dir_size(PortSpec, PortDir, ExpectedSize) },
    kwarg_dispatch_on_portdir(PortDir, Sig, Kwarg, ExpectedSize, Val0, Val),
    sig_kwargs_typechecked(Kws0, Sig, Kws).

kwarg_dispatch_on_portdir(out, Sig, Kwarg, ExpectedSize, Val0, Val) -->
    contassign_lhs_size_typechecked(Val0, ValSz, Val),
    { ValSz = ExpectedSize -> true ;
        throw_error(incompatible_sizes, #{
            op: mod(Sig),
            subterms: [Kwarg, Val0],
            subterm_sizes: [ExpectedSize, ValSz]
        })
    }.
kwarg_dispatch_on_portdir(in, Sig, Kwarg, ExpectedSize, Val0, Val) -->
    term_size_resolved(Val0, ValSz, Val),
    { ValSz = ExpectedSize -> true ;
        throw_error(incompatible_sizes, #{
            op: mod(Sig),
            subterms: [Kwarg, Val0],
            subterm_sizes: [ExpectedSize, ValSz]
        })
    }.


stmt_typechecked(Other, _) -->
    { Other =.. [Functor|_] },
    { throw_error(unimplemented(Functor, stmt_typechecked(Other, _))) }.

stmts_typecheckeds([], []) --> [].
stmts_typecheckeds([S0|Ss0], [S|Ss]) -->
    stmt_typechecked(S0, S),
    stmts_typecheckeds(Ss0, Ss).

/*
There are two kinds of assignment in Querilog: continuous assignment (:=) and
clocked assignment (<-).

A continuous assignment like `?a := ?b` is identical to the Verilog statement
`assign a = b`.

Clocked assignment is delayed until the end of the current "cycle". The series
of statements `?a <- ?b; ?b <- ?c` is analogous to the Verilog block:

```verilog
always @ (posedge clk) begin
    a <= b;
    b <= c;
end
```

For convenience, you can interleave continous and clocked assignments however
you like. The order only matters for scope reasons i.e. `?y := ?x; ?x := #1` is
problematic since the typechecker and evaluator assume a variable will be
assigned to before use.

Inside modules, an out-param is assumed to be a net type meaning it may be
continuous-assigned to, but cannot be clock-assigned to.

```querilog
defmodule(lessthan { x: in, y: in, bool: out(1) },
    subtr {
        x: ?x, y: ?y,
        diff: ?diff, signin: ?sin, carryout: ?cout
    };
    ?overflow := ?sin xor ?cout;
    ?sign := bit(?diff, #15);
    ?bool := ?overflow xor ?sign
).
```

In fact for now, let's say no clocked-assignments are allowed inside modules.

The instantiator of a module also must provide net variables to any 
out-params, but may subsequently clock-assign that net to a reg:

```querilog
adder { x: ?x, y: ?y, sum: ?new_var };
$some_reg <- ?new_var
```
*/


contassign_lhs_size_typechecked(?VarName, Size, ?VarName) --> !,
    ( is_var_decl(?VarName, _VarKind, Size) -> [] ;
        decl_var(?VarName, net(local), Size)
    ),
    add_contassign(?VarName).
contassign_lhs_size_typechecked(Lhs\Size, Size, Lhs\Size) --> !,
    { integer(Size) -> true ; throw_error(var_size_must_be_const_int(Size)) },
    contassign_lhs_size_typechecked(Lhs, Size, Lhs).
contassign_lhs_size_typechecked({Components0}, Size, {Components}) --> !,
    { comma_list(Components0, Cs0) },
    contassign_lhs_all(Cs0, Sizes, Cs),
    { clpfd_sumlist(Sizes, Size) },
    { comma_list(Components, Cs) }.
contassign_lhs_size_typechecked(Other, _, _) -->
    { throw_error(contassign_may_only_assign_to_net(Other)) }.

contassign_lhs_all([], [], []) --> [].
contassign_lhs_all([C0|Cs0], [CSz|CsSz], [C|Cs]) -->
    contassign_lhs_size_typechecked(C0, CSz, C),
    contassign_lhs_all(Cs0, CsSz, Cs).


clkassign_lhs_size_typechecked($Reg, RegSz, $Reg) --> !,
    { isa:register_size(RegSz) },
    add_pending_clkassign($Reg).
clkassign_lhs_size_typechecked($$Reg, RegSz, $$Reg) --> !,
    { isa:sysreg_size(Reg, RegSz) },
    add_pending_clkassign($$Reg).
clkassign_lhs_size_typechecked(?Var, Size, ?Var) --> !,
    /*
    my_mod{ o1: out, o2: out, i1: in } :=
        ?o1 <- #1;      % Invalid! Can't clkassign to net(local(out)).
        ?newvar1 <- #1; % Invalid! Not a known reg varkind.
        ?newvar2 := #1; % Valid. Introduces a new net(local) variable.
        other_mod{
            out_param_1: ?o2,      % Valid. Forward value outside my_mod.
            out_param_2: ?newvar3, % Valid. Introduces a net(local) var.
            out_param_3: ?i1       % Invalid! net(param(out)) assigned to a
                                   % net(param(in)).
        }
    */
    ( is_var_decl(?Var, VarKind, Size) ->
        ( { VarKind = reg } ->
            % Just ensure it's not already a pending assignment.
            ( is_pending_clkassign(?Var) ->
                { throw_error(multiple_clkassigns(?Var)) }
            ;
                add_pending_clkassign(?Var)
            )
        ; % Wrong varkind
            { throw_error(wrong_varkind, #{
                target: ?Var,
                actual_varkind: VarKind,
                expected_varkind: reg
            }) }
        )
    ; % If Var is undeclared, error
        { throw_error(undeclared_var_in_clkassign(?Var)) }
    ).
clkassign_lhs_size_typechecked(bit(Tgt0, Idx0), 1, bit(Tgt, Idx)) --> !,
    clkassign_lhs_size_typechecked(Tgt0, TgtSz, Tgt),
    term_size_resolved(Idx0, IdxSz, Idx),
    { TgtSz #= 2^IdxSz -> true ;
        throw_error(incompatible_sizes, #{
            op: (bit(_, _) <- _),
            subterms: [Tgt0, Idx0],
            subterm_sizes: [TgtSz, IdxSz],
            violation: not_power_of_2(\+ TgtSz = 2^IdxSz)
        })
    }.
clkassign_lhs_size_typechecked(mem(Addr0), Size, mem(Addr)) --> !,
    term_size_resolved(mem(Addr0), Size, mem(Addr)),
    add_pending_clkassign(mem(Addr)).
clkassign_lhs_size_typechecked({ Components0 }, ComponentsSz, { Components }) --> !,
    { comma_list(Components0, Components1) },
    terms_sizes_resolveds(Components1, Sizes, Components2),
    { sumlist(Sizes, ComponentsSz) },
    { maplist([Comp, Comp <- #0]>>true, Components2, Stmts) },
    stmts_typecheckeds(Stmts, _),
    { comma_list(Components, Components2) }.
clkassign_lhs_size_typechecked(Lhs0, _, _) -->
    { Lhs0 =.. [Functor|_] },
    { LhsErr =.. [Functor, '...'] },
    { throw_error(unimplemented(
            LhsErr <- '...',
            stmt_typechecked(Lhs0 <- '...', _)
        ))
    }.


:- dynamic(mod_sig_body/3).

/*
                     module
              |------------------|
              |input       output|
reg or net -->|-->            -->|--> net
              |net     reg or net|
              |------------------|
*/
mod_sig_typechecked(ModName, Sig, Body) -->
    get_state(Before),
    { init_state(FreshState) },
    put_state(FreshState),
    { querilog_mod(Sig, Body0), is_dict(Sig, ModName) },
    { dict_pairs(Sig, ModName, PortNamesSpecs) },
    define_ports(PortNamesSpecs),
    stmt_typechecked(Body0, Body),
    { format('Typechecked mod `~p`.~n', [ModName]) },
    { asserta(mod_sig_body(ModName, Sig, Body)) },
    put_state(Before),
[].

define_ports([]) --> [].
define_ports([PortName-PortSpec|Ps]) -->
    { portspec_dir_size(PortSpec, Dir, Size) },
    decl_var(?PortName, net(param(Dir)), Size),
    define_ports(Ps).

portspec_dir_size(       in,  in, Size) :- Size in 1..sup.
portspec_dir_size(      out, out, Size) :- Size in 1..sup.
portspec_dir_size( in(Size),  in, Size) :- Size in 1..sup.
portspec_dir_size(out(Size), out, Size) :- Size in 1..sup.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% THROW_ERROR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

throw_error(ErrNameAndPayload) :-
    throw(error(ErrNameAndPayload, _)).
throw_error(ErrName, ErrPayload) :-
        term_clpfd_goals(ErrPayload, Goals),
        ( Goals = [] ->
            Err0 =.. [ErrName, ErrPayload]
        ;
            Err0 =.. [ErrName, ErrPayload, att_goals(Goals)]
        ),
        copy_term(Err0, Err, _),
        numbervars(Err),
        throw(error(Err, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNIT TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(test_querilog_tck).

test(save_lit_to_reg) :-
    typecheck((
        $x <- #123\16
    )).

test(save_inferred_size_lit_to_reg) :-
    typecheck((
        $x <- #123
    )).

test(lit_too_big_for_reg, [error(incompatible_sizes(_, _))]) :-
    typecheck((
        $x <- #1_000_000
    )).

test(reg_to_reg) :-
    typecheck((
        $x <- $y
    )).

test(sysreg_too_big_for_reg, [error(incompatible_sizes(_))]) :-
    typecheck((
        $x <- $$mp
    )).

test(multi_statement) :-
    typecheck((
        $x <- $y;
        $y <- $x
    )).

test(equality_relop) :-
    typecheck((
        bit($x, #0) <- #12\8 == #9\8
    )).

test(bit_select_good) :-
    typecheck((
        $x <- zxt(bit($y, #15\4))
    )).

test(bit_select_bad, [error(incompatible_sizes(_))]) :-
    typecheck((
        $x <- zxt(bit($y, #1_000_000\32))
    )).

test(clkassign_to_bit) :-
    typecheck((
        bit($y, #5\4) <- #1\1
    )).

test(clkassign_to_bit_aliased_reg) :-
    typecheck([?my_reg-reg-16], (
        bit(?my_reg, #5\4) <- #1\1
    )).

test('clkassign to local-net fails', [error(wrong_varkind(_))]) :-
    typecheck([?local_net-net(local)-8], (
        ?local_net <- #0
    )).

test('clkassign to in-param-net fails', [error(wrong_varkind(_))]) :-
    typecheck([?local_net-net(param(in))-8], (
        ?local_net <- #0
    )).

test('clkassign to out-param-net fails', [error(wrong_varkind(_))]) :-
    typecheck([?local_net-net(param(out))-8], (
        ?local_net <- #0
    )).

test('contassign to out-param-net') :-
    typecheck([?local_net-net(param(out))-8], (
        ?local_net := #0
    )).

test(alias_def_alias_use) :-
    typecheck((
        ?asdf := #0xFF00AA\24;
        $x <- ?asdf\16 + #77
    )).

test(contassign_to_reg_fails, [error(contassign_may_only_assign_to_net($x))]) :-
    typecheck((
        $x := $y
    )).

test(adder_simple) :-
    typecheck((
        adder{ x: #123, y: $$pc\8, sum: ?tmp };
        $z <- zxt(?tmp)
    )).

test(adder_all_features) :-
    typecheck((
        adder{
            x: #123, y: $$pc\8, sum: ?tmp, carryin: #1,
            carryout: ?cout, signin: ?sin
        };
        bit($$ts, #0) <- ?cout;
        $z <- zxt(?tmp)
    )).

test(contassign_to_sized_var) :-
    typecheck((
        ?asdf\3 := #(-1)
    )).

test(contassign_to_brace_components) :-
    typecheck((
        {?asdf\3, ?qwer\13} := #(-1)
    )).

test(instantiation_of_custom_module) :-
    typecheck((
        subtr{ x: #123, y: #456, diff: ?d\10, signin: ?sin, carryout: ?cout }
    )).

:- end_tests(test_querilog_tck).
