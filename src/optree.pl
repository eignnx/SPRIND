%%
%% Builds a decision tree to categorize by tag instructions within a generic 
%% instruction format. Uses the decision tree to assign opcodes within a
%% generic instruction format.
%%
:- module(optree, [
    print_report/0,
    print_dottrees/0,
    print_dottree/1
]).

:- use_module(isa, [fmt_instr/2]).
:- use_module(sem, [instr_info/2]).
:- use_module(utils).

instr_tag(Instr, Tag) :-
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).

tag_instr(Tag, Instr) :- instr_tag(Instr, Tag).

fmt_instr_tag(Fmt, Instr, Tag) :-
    fmt_instr(Fmt, Instr),
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).

fmt_all_tags(Fmt, Tags) :-
    aggregate_all(set(Tag), fmt_instr_tag(Fmt, _Instr, Tag), Tags).

fmt_all_instrs(Fmt, Instrs) :-
    aggregate_all(bag(Instr), fmt_instr(Fmt, Instr), Instrs).


fmt_tag_occurrances(Fmt, Tag, Count) :-
    aggregate_all(count, fmt_instr_tag(Fmt, _Instr, Tag), Count).


fmt_total_instr_count(Fmt, Count) :-
    aggregate_all(count, fmt_instr(Fmt, _Instr), Count).


fmt_tag_frequency(Fmt, Tag, Freq) :-
    fmt_tag_occurrances(Fmt, Tag, TagCount),
    fmt_total_instr_count(Fmt, TotalCount),
    Freq is TagCount / TotalCount.


fmt_tag_splittability(Fmt, Tag, Score) :-
    fmt_tag_frequency(Fmt, Tag, Freq),
    ( fmt_instr(Fmt, Tag) -> Factor = 0.75 ; Factor = 1.0 ),
    Score is (1.0 - 2.0 * abs(0.5 - Freq)) * Factor.

print_scores(Fmt) :-
    fmt_all_tags(Fmt, Tags),
    maplist([Tag, Score-Tag]>>(
        fmt_tag_splittability(Fmt, Tag, S),
        Score is 100 * S
    ), Tags, TagsScores0),
    sort(0, @>, TagsScores0, TagsScores),
    maplist([Score-Tag]>>(
        BarLength is round(Score),
        format('~p~t~0f%~20|  ~|~`#t~*+~n', [Tag, Score, BarLength])
    ), TagsScores).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pool_instr_tag(Pool, Instr, Tag) :-
    member(Instr, Pool),
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).

pool_all_tags(Pool, Tags) :-
    aggregate_all(set(Tag), pool_instr_tag(Pool, _Instr, Tag), Tags).

pool_tag_occurrances(Pool, Tag, Occurrances) :-
    aggregate_all(count, pool_instr_tag(Pool, _Instr, Tag), Occurrances).

pool_tag_frequency(Pool, Tag, Freq) :-
    pool_tag_occurrances(Pool, Tag, Occurrances),
    length(Pool, PoolSize),
    Freq is Occurrances / PoolSize.

pool_tag_score(Pool, Tag, Score) :-
    pool_tag_frequency(Pool, Tag, Freq),
    ( Tag = instr(_) -> Factor = 0.75 ; Factor = 1.0 ),
    Score is (1.0 - 2.0 * abs(0.5 - Freq)) * Factor.

print_scores_pooled(Fmt) :-
    fmt_all_instrs(Fmt, Pool),
    pool_all_tags(Pool, Tags),
    maplist({Pool}/[Tag, Score-Tag]>>(
        pool_tag_score(Pool, Tag, Score)
    ), Tags, TagsScores0),
    sort(0, @>, TagsScores0, TagsScores),
    catch(tty_size(_Rows, Cols), _, Cols = 80), 
    maplist({Cols}/[Score-Tag]>>(
        BarLength is floor((Cols - 25) * Score),
        Pct is Score * 100,
        format('~p~t~0f%~20|  ~|~`#t~*+~n', [Tag, Pct, BarLength])
    ), TagsScores).

print_scores :-
    foreach(
        isa:gfmt(Fmt),
        (
            format('-------------------~n'),
            format('| ~k~n', [Fmt]),
            format('-------------------~n'),
            print_scores_pooled(Fmt),
            format('~n')
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fmt_tree(Fmt, Tree) :-
    fmt_all_instrs(Fmt, Pool),
    instrpool_tree(Pool, Tree).

instrpool_tree([], '$ERROR') :- throw(error('cannot construct tree from empty pool')).
instrpool_tree([Instr], leaf(Instr)).
instrpool_tree([I0, I1 | Is], node(LeftTree, SplitTag, RightTree)) :-
    Pool = [I0, I1 | Is],
    pool_all_tags(Pool, Tags),
    maplist({Pool}/[Tag, Score-Tag]>>(
        pool_tag_score(Pool, Tag, Score)
    ), Tags, ScoresTags),
    predsort(cmp_score_ascending, ScoresTags, SortedScoresTags),
    member(_SplitScore-SplitTag, SortedScoresTags), % Generate many solns here...
    partition(tag_instr(SplitTag), Pool, RightPool, LeftPool),
    LeftPool \= Pool, RightPool \= Pool, % ...because some split tags may need to be skipped.
    instrpool_tree(LeftPool, LeftTree),
    instrpool_tree(RightPool, RightTree).


cmp_score_ascending(Ord, Score1-Tag1, Score2-Tag2) :-
    Score1 > Score2 ->
        Ord = '<'
    ; Tag1 @> Tag2 ->
        Ord = '<'
    ;
        Ord = '>'.


print_tree(Tree) :- 
    format('~`-t~40|~n'),
    print_tree(Tree, 0, ``),
    format('~`-t~40|~n').

print_tree(leaf(Instr), Lvl, Prefix) :-
    length(Prefix, PLen),
    format('~t~*|~k = 0b~s`~d~n', [Lvl, Instr, Prefix, PLen]).
print_tree(node(Left, Split, Right), Lvl, Prefix) :-
    format(codes(SplitLine), '~t~*|~k?', [Lvl, Split]),
    length(SplitLine, Indent),
    NextLvl is Indent + 1,

    append(Prefix, `0`, LeftPrefix),
    append(Prefix, `1`, RightPrefix),

    print_tree(Left, NextLvl, LeftPrefix),
    format('~s~n', [SplitLine]),
    print_tree(Right, NextLvl, RightPrefix).


dotprint_tree(Fmt, Tree) :-
    format('digraph "Format ~k" {~n', [Fmt]),
    dotprint_tree_(Tree, ``),
    format('}~n').

dotprint_tree_(leaf(Instr), Prefix) :-
    node_id_label(leaf(Instr), Id, _),
    instr_info(Instr, Info),
    length(Prefix, PLen),
    format(atom(Label), '~k~n~w~n0b~s`~d', [Instr, Info.title, Prefix, PLen]),
    format('  ~w [label = "~w", shape = rectangle];~n', [Id, Label]).
dotprint_tree_(node(Left, Split, Right), Prefix) :-
    node_id_label(node(Left, Split, Right), Id, Label),
    node_id_label(Left, LeftId, _),
    node_id_label(Right, RightId, _),
    format('  ~w [label = ~w, shape = ellipse];~n', [Id, Label]),
    format('  ~w -> ~w [label = "1"];~n', [Id, RightId]),
    format('  ~w -> ~w [label = "0"];~n', [Id, LeftId]),
    append(Prefix, `0`, LeftPrefix),
    append(Prefix, `1`, RightPrefix),
    dotprint_tree_(Right, RightPrefix),
    dotprint_tree_(Left, LeftPrefix).

node_id_label(leaf(Instr), Id, Label) :-
    term_hash(leaf(Instr), Hash),
    format(atom(Id), '"~k_~d"', [Instr, Hash]),
    format(atom(Label), '"~k"', [Instr]).
node_id_label(node(Left, Split, Right), Id, Label) :-
    term_hash(node(Left, Split, Right), Hash),
    format(atom(Id), '"~k_~d"', [Split, Hash]),
    format(atom(Label), '"~k?"', [Split]).

print_dottree(Fmt) :-
    once(fmt_tree(Fmt, Tree)),
    dotprint_tree(Fmt, Tree).

print_dottrees :-
    foreach(
        (
            isa:gfmt(Fmt),
            Fmt \= ext,
            format(atom(Path), 'src/graphs/~k.dot', [Fmt])
        ),
        utils:output_to_file(print_dottree(Fmt), Path)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_report :-
    print_scores,
    print_dottrees,
    true.