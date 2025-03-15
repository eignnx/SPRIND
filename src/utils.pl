:- module(utils, [
    item_count_replication/3,
    list_item_occurrances/3,
    warn_if_nondet/1,
    nzip_longest/3
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


