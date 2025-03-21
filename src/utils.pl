:- module(utils, [
    item_count_replication/3,
    list_item_occurrances/3,
    warn_if_nondet/1,
    nzip_longest/3,
    peano_decimal/2,
    output_to_file/2,
    atom_slugified/2
]).

:- use_module(library(clpfd)).
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
        open(Path, write, S, [create([read, write])]),
        with_output_to(S, Goal),
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
    include([Ch]>>(char_type(Ch, alnum) ; Ch = '-'), Chars1, Chars2),
    atom_chars(Slug, Chars2),
end.


end.