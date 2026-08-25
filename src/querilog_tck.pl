/** <module> querilog_tck
The static type checker for querilog. Transforms a raw ast by ensuring all
values with implicit sizes (like `#123`) are explicitly sized after type
checking.
*/
:- module(querilog_tck, [
    program_typechecked//2
]).

:- use_module(library(clpfd)).
:- use_module(querilog_syntax).
:- use_module(sem, [def/2]).
:- use_module(isa, [register_size/1]).
:- use_module(utils).


guess_integer_size(N, Size) :-
    ( N #>= 0 ->
        % Assume unsigned
        Size in 1..sup,
        0 #=< N, N < 2^Size
    ;
        % Assume signed
        Size in 1..sup,
        -1 * 2^(Size-1) #=< N, N < 2^(Size-1)
    ).


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
        guess_integer_size(N, Size),
        Term = #N\Size % Defer size inference for later
    ; atom(Term0), Const = Term0 ->
        sem:def(#Const, N),
        Size in 0..sup,
        -1 * 2^(Size - 1) #=< N, N #< 2^Size, % Widest possible bounds -> Size approx(>=) lg(|N|)
        Term = #N\Size % Defer size inference for later
    }.

term_size_resolved(A0 + B0, Size, A + B) --> !,
    term_size_resolved(A0, ZA, A),
    term_size_resolved(B0, ZB, B),
    { ZA = ZB -> true ;
        throw(error(incompatible_sizes(#{
            op: +,
            subterms: [A0, B0],
            subterm_sizes: [ZA, ZB]
        }), _))
    },
    { Size = ZA }.

term_size_resolved(m(Addr0), 8, m(Addr)) --> !,
    term_size_resolved(Addr0, ZAddr, Addr),
    { ZAddr = 16 -> true ;
        throw(error(incompatible_size(#{
            op: m,
            subterm: [Addr0],
            expected_size: 16,
            actual_size: [ZAddr]
        }), _))
    }.

term_size_resolved(sxt(E0), Size, sxt(E)) --> !,
    term_size_resolved(E0, ZE, E),
    { Size in 1..sup },
    { ZE #< Size -> true ;
        % Unreachable?
        throw(error(unsatisfiable_size_constraint(#{
            constraint: ZE #< Size,
            term: sxt(E0)
        }), _))
    }.

term_size_resolved(zxt(E0), Size, zxt(E)) --> !,
    term_size_resolved(E0, ZE, E),
    { Size in 1..sup },
    { ZE #< Size -> true ;
        % Unreachable?
        throw(error(unsatisfiable_size_constraint(#{
            constraint: ZE #< Size,
            term: zxt(E0)
        }), _))
    }.

term_size_resolved({Es0}, Size, {Es}) --> !,
    { comma_list(Es0, Es1) },
    { maplist(term_size_resolved, Es1, [S|Sizes], Es) },
    { foldl([A, B, C]>>(A + B #= C), Sizes, S, Size) }.

term_size_resolved(Term, _, _) -->
    { throw(error(unimplemented(term_size_resolved(Term, _, _)), _)) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPECHECKER STATE MONAD %%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(tck_state{
    bindings: [],
    pending_assignments: []
}).

add_binding(VarName, Size) -->
    get(Before),
    { After = Before.put(bindings, [VarName-Size | Before.bindings]) },
    put(After).

add_pending_assignment(Lhs) -->
    get(Before),
    { memberchk(Lhs, Before.pending_assignments) ->
        throw(error(duplicate_assignment(#{target: Lhs}), _))
    ; true },
    { NewBindings = [Lhs | Before.pending_assignments] },
    { After = Before.put(pending_assignments, NewBindings) },
    put(After).



%! program_typechecked(+P0:statements, -P:typechecked(statements)) is det.
%
% Same as `term_size_resolved` except for statements.
program_typechecked(?VarName := Rhs0, ?VarName := Rhs) --> !,
    term_size_resolved(Rhs0, ZRhs, Rhs),
    add_binding(VarName, ZRhs).
program_typechecked($Reg <- Rhs0, $Reg <- Rhs) --> !,
    { isa:register_size(ZReg) },
    term_size_resolved(Rhs0, ZRhs, Rhs),
    { ZReg = ZRhs -> true ;
        throw(error(incompatible_sizes(#{
            op: <-,
            subterms: [$Reg, Rhs0],
            subterm_sizes: [ZReg, ZRhs]
        }), _))
    },
    add_pending_assignment($Reg).
program_typechecked(Other, _) -->
    { throw(error(unimplemented(program_typechecked(Other)), _)) }.



