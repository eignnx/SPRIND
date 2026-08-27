/** <module> querilog_tck
The static type checker for querilog. Transforms a raw ast by ensuring all
values with implicit sizes (like `#123`) are explicitly sized after type
checking.
*/
:- module(querilog_tck, [
    program_typechecked//2,
    typecheck_instr_sems/0
]).

:- use_module(library(clpfd)).
:- use_module(querilog_syntax).
:- use_module(sem, [def/2]).
:- use_module(isa, [register_size/1]).
:- use_module(utils).


:- use_module(sem_querilog, [instr_info/2]).
typecheck_instr_sems :-
    findall(Status, typecheck_some_instr_sem(Status), Statuses),
    exclude(=(success), Statuses, Failures),
    length(Statuses, NTotal),
    length(Failures, NFail),
    maplist([F]>>format('~p~n', [F]), Failures),
    format('RESULTS: ~d failures out of ~d instructions~n', [NFail, NTotal]).
typecheck_some_instr_sem(Status) :-
    sem_querilog:instr_info(Instr, Info),
    Sem = Info.sem,
    catch(
        ( typecheck(Instr) ->
            Status = success
        ;
            Status = typechecking_failed(Instr, Sem)
        ),
        error(E, _),
        Status = exception(Instr, E)
    ).

typecheck(Instr) :-
    sem_querilog:instr_info(Instr, Info),
    isa:fmt_instr(Fmt, Instr),
    once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
    syntax_operands(Info.syntax, Operands),
    maplist(tcx_binding_from_syn_operands(ImmBits), Operands, Tcx),
    init_state(S0, Tcx),
    phrase(program_typechecked(Info.sem, _TypeChecked), [S0], [_S]).

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
    ( { [Inner] = X } -> [Inner] ; [X]),
    operand_vardecl(Xs).

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
        ( guess_integer_size(N, Size) -> Term = #N\Size ;
            format(atom(Msg), 'Integer ~d does not fit in ~d bits', [N, Size]),
            throw(error(syntax_error(Msg, #N\Size), _))
        )
    ; integer(Term0), N = Term0 ->
        bound_integer_size(N, Size),
        Term = #N\Size % Defer size inference for later
    ; atom(Term0), Const = Term0 ->
        sem:def(#Const, N),
        Size in 1..sup,
        -1 * 2^(Size - 1) #=< N, N #< 2^Size, % Widest possible bounds -> Size approx(>=) lg(|N|)
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

term_size_resolved(mem(Addr0), 8, mem(Addr)) --> !,
    term_size_resolved(Addr0, AddrSz, Addr),
    { AddrSz = 16 -> true ;
        throw(error(incompatible_size(#{
            op: m,
            subterm: [Addr0],
            expected_size: 16,
            actual_size: [AddrSz]
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

term_size_resolved({Es0}, Size, {Es}) --> !,
    { comma_list(Es0, Es1) },
    terms_sizes_resolveds(Es1, [S|Sizes], Es),
    { foldl([A, B, C]>>(A + B #= C), Sizes, S, Size) }.

binopterm_size_resolved(Op, A0, B0, Size, Term) -->
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz = BSz -> true ;
        throw(error(incompatible_sizes(#{
            op: Op,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz]
        }), _))
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

term_size_resolved(A0 << B0, Size, A << B) --> !,
    term_size_resolved(A0, ASz, A),
    term_size_resolved(B0, BSz, B),
    { ASz >= BSz -> true ;
        throw(error(incompatible_sizes(#{
            op: <<,
            subterms: [A0, B0],
            subterm_sizes: [ASz, BSz],
            violation: BSz =< ASz
        }), _))
    },
    { Size = ASz }.

/*
[_,_,x1,x2,x3,x4,x5,x6] << [s1,s2,s3]
*/
term_size_resolved(bit(Tgt0, Idx0), 1, bit(Tgt, Idx)) --> !,
    term_size_resolved(Tgt0, TgtSz, Tgt),
    term_size_resolved(Idx0, IdxSz, Idx),
    { TgtSz >= 2^IdxSz -> true ;
        throw(error(incompatible_sizes(#{
            op: bit,
            subterms: [Tgt0, Idx0],
            subterm_sizes: [TgtSz, IdxSz],
            violation: log2(TgtSz) >= IdxSz
        }), _))
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

% Catchall error case:
term_size_resolved(Term, _, _) -->
    { Term =.. [Functor|_] },
    { throw(error(unimplemented(Functor, term_size_resolved(Term, _, _)), _)) }.

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


:- det(program_typechecked//2).
:- discontiguous(program_typechecked//2).

%! program_typechecked(+P0:statements, -P:typechecked(statements)) is det.
%
% Same as `term_size_resolved` except for statements.
program_typechecked(todo, todo) --> !.

program_typechecked((A0 ; B0), (A ; B)) --> !,
    program_typechecked(A0, A),
    program_typechecked(B0, B).

program_typechecked((?VarName := Rhs0), (?VarName := Rhs)) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    add_binding(?VarName, RhsSz).

program_typechecked(Lhs0 <- Rhs0, Lhs <- Rhs) --> !,
    term_size_resolved(Rhs0, RhsSz, Rhs),
    ( { $Reg = Lhs0 } ->
        { isa:register_size(RegSz) },
        { RegSz = RhsSz -> true ;
            throw(error(incompatible_sizes(#{
                op: <-,
                subterms: [$Reg, Rhs0],
                subterm_sizes: [RegSz, RhsSz]
            }), _))
        },
        add_pending_assignment($Reg),
        { Lhs = Lhs0 }
    ; { $$Reg = Lhs0} ->
        { isa:sysreg_size(Reg, RegSz) },
        { RegSz = RhsSz -> true ;
            throw(error(incompatible_sizes(#{
                op: <-,
                subterms: [$$Reg, Rhs0],
                subterm_sizes: [RegSz, RhsSz]
            }), _))
        },
        add_pending_assignment($$Reg),
        { Lhs = Lhs0 }
    ; { ?Var = Lhs0 } ->
        get_state(State),
        { memberchk(?Var-VarSz, State.bindings) -> true ;
            throw(error(unbound_identifier(?Var, State.bindings), _))
        },
        { VarSz = RhsSz -> true ;
            throw(error(incompatible_sizes(#{
                op: <-,
                subterms: [?Var, Rhs0],
                subterm_sizes: [VarSz, RhsSz]
            }), _))
        },
        { Lhs = Lhs0 }
    ; { bit(Tgt0, Idx0) = Lhs0 } ->
        { RhsSz = 1 -> true ;
            throw(error(incompatible_sizes(#{
                op: <-,
                subterms: [bit(Tgt0, Idx0), Rhs0],
                subterm_sizes: [1, RhsSz]
            }), _))
        },
        term_size_resolved(Tgt0, TgtSz, Tgt),
        term_size_resolved(Idx0, IdxSz, Idx),
        { Lhs = bit(Tgt, Idx) }
    ;
        { Lhs0 =.. [Functor|_] },
        { LhsErr =.. [Functor, '...'] },
        { throw(error(unimplemented(
                LhsErr <- '...',
                program_typechecked(Lhs0 <- Rhs0, _)
            ), _))
        }
    ).

program_typechecked(Other, _) -->
    { Other =.. [Functor|_] },
    { throw(error(unimplemented(Functor, program_typechecked(Other, _)), _)) }.



