:- module(utils, [
    item_count_replication/3,
    list_item_occurrances/3,
    warn_if_nondet/1,
    nzip_longest/3,
    peano_decimal/2,
    output_to_file/2,
    atom_slugified/2,
    codes_slugified/2,
    write_phrase/1,
    before_after//2,
    get_state//1,
    put_state//1,
    signbit_compare/2,
    round_up_to_next_pow2/2,
    term_clpfd_goals/2,
    list_enumerated0/2,
    list_enumerated1/2,
    template_goal_condition_index/4,
    clpfd_sumlist/2
]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order)).
:- op(20, fx, #).


item_count_replication(Item, Count, Replication) :-
    length(Replication, Count),
    maplist(=(Item), Replication).


list_item_occurrances([], _, 0).
list_item_occurrances([X | Xs], Y, N) :-
    dif(X, Y) ->
        list_item_occurrances(Xs, Y, N)
    ;
        list_item_occurrances(Xs, Y, M),
        #N #= #M + 1.


:- meta_predicate warn_if_nondet(0).
warn_if_nondet(Goal) :-
    aggregate(count, Goal, Count),
    Count > 1 -> throw(error(redundant_choicepoint_for_goal(Goal))) ; true.


nzip_longest(ColumnsRagged, Rows, Default) :-
    raggedlist_matrix(ColumnsRagged, Columns, Default),
    transpose(Columns, Rows).

raggedlist_matrix(ColumnsRagged, Columns, Default) :-
    foldl(
        [List,Max,NewMax]>>(length(List, Len), #NewMax #= max(#Max, #Len)),
        ColumnsRagged, 0, MaxColLen
    ),
    maplist(
        {MaxColLen, Default}/[RaggedCol, PaddedCol]>>(
            list_padded(RaggedCol, PaddedCol, MaxColLen, Default)
        ),
        ColumnsRagged,
        Columns
    ).


list_padded(List, Padded, PadLen, Fill) :-
    zcompare(RelOp, PadLen, 0),
    relop_list_padded(RelOp, List, Padded, PadLen, Fill).

relop_list_padded(=, [], [], _, _Fill).
relop_list_padded(>, List, Padding, N, Fill) :-
    gt_list_padded(List, Padding, N, Fill).

gt_list_padded([], Padding, N, Fill) :-
    item_count_replication(Fill, N, Padding).
gt_list_padded([X | Xs0], [X | Xs], N0, Fill) :-
    #N #= #N0 - 1,
    zcompare(RelOp, N, 0),
    relop_list_padded(RelOp, Xs0, Xs, N, Fill).


peano_decimal(z, 0).
peano_decimal(s(P), N) :-
    N in 0 .. sup,
    #N #= #N0 + 1,
    peano_decimal(P, N0).


:- meta_predicate output_to_file(?, 0).

output_to_file(Path, Goal) :-
    ( atom(Path) -> true ; type_error('a file path as an atom', Path) ),
    setup_call_cleanup(
        (
            format('Writing to `~w`...', [Path]),
            open(Path, write, S, [
                create([read, write]),
                encoding(utf8)
            ])
        ),
        (
            call_time(with_output_to(S, Goal), Time),
            format(' Done. (~3fs)~n', [Time.wall])
        ),
        close(S)
    ).

atom_slugified(Atom, Slug) :-
    downcase_atom(Atom, AtomDown),
    atom_chars(AtomDown, Chars0),
    maplist([In, Out]>>(
        In = ' ' -> Out = '-'
        ; Out = In
    ),
        Chars0,
        Chars1
    ),
    include([Ch]>>(char_type(Ch, alnum) ; Ch = '-' ; Ch = '_'), Chars1, Chars2),
    atom_chars(Slug, Chars2),
end.

codes_slugified(Codes, Slug) :-
    atom_codes(Atom, Codes),
    atom_slugified(Atom, SlugAtom),
    atom_codes(SlugAtom, Slug),
end.


:- meta_predicate write_phrase(//).

write_phrase(Nonterminal) :-
    phrase(Nonterminal, Codes),
    format('~s', [Codes]),
end.


before_after(Old, New), [New] --> [Old].
get_state(State) --> before_after(State, State).
put_state(State) --> before_after(_, State).


%! signbit_compare(SignBit:oneof([0,1]), N:integer) is det.
%
% Like `zcompare` but for negative/nonnegative only.
signbit_compare(SignBit, N) :-
    SignBit #<==> N #< 0,
    label([SignBit]).

%! round_up_to_next_pow2(Old:positive_integer, New:positive_integer) is det.
%
round_up_to_next_pow2(OldSize, NewSize) :-
    N in 1..sup,
    2^(N-1) #< OldSize, OldSize #=< 2^N,
    NewSize #= 2^N.

term_clpfd_goals(Term, Goals) :-
    term_variables(Term, Vars0),
    phrase(vars_clpfd_goals_(Vars0), Goals0),
    term_variables(Goals0, Vars),
    phrase((Goals0, vars_clpfd_goals_(Vars)), Goals1),
    sort(Goals1, Goals).

vars_clpfd_goals_([]) --> [].
vars_clpfd_goals_([V|Vs]) -->
    ( clpfd:attribute_goals(V), ! | [] ),
    vars_clpfd_goals_(Vs).

list_enumerated0(List, Enumerated) :-
    list_enumerated_(List, Enumerated, 0).
list_enumerated1(List, Enumerated) :-
    list_enumerated_(List, Enumerated, 1).

list_enumerated_([], [], _).
list_enumerated_([X|Xs], [N-X|NXs], N) :-
    PredN #= N + 1,
    list_enumerated_(Xs, NXs, PredN).


:- meta_predicate(template_goal_condition_index(?, 0, +, -)).

template_goal_condition_index(Templ, Goal, Cond, Index) :-
    setup_call_cleanup(
        engine_create(Templ, Goal, E),
        catch(
            get_answers_(E, Cond, Index, 0),
            % If engine is finished (no more solns), fail rather than throw.
            error(existence_error(engine, E), _),
            fail
        ),
        engine_destroy(E)
    ).

get_answers_(E, Cond, Index, Acc) :-
    ( engine_next(E, Cond) ->
        Index = Acc
    ;
        Acc1 #= Acc + 1,
        get_answers_(E, Cond, Index, Acc1)
    ).


clpfd_sumlist([], 0).
clpfd_sumlist([X|Xs], Total) :-
    foldl([A, B, C]>>(A + B #= C), Xs, X, Total).


end.
