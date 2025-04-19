% If an instruction contains a `aligned_subcat(_)` tag, it should be excluded from the pool.
% All instructions with tag `aligned_subcat(X)` should be replaced by a single pseudo-
% instruction called `subcat(X)` whose tags are the union of the tags of all instructions 
% in the subcategory.

:- module(optree, [
    fmt_tree/2,
    fmt_tree_maxbits/3,
    optree_instr_prefix/3
]).

:- use_module(isa, [fmt_instr/2]).
:- use_module(sem, [instr_info/2]).
:- use_module(utils).
:- use_module(library(clpfd)).
:- op(20, fx, #).


fmt_tree(Fmt, Tree) :-
    fmt_tree_maxbits(Fmt, Tree, _MaxBits).

fmt_tree_maxbits(Fmt, Tree, MaxBits) :-
    fmt_all_instrs(Fmt, Pool),
    ( Pool = [] ->
        Tree = empty
    ;
        catch(
            instrpool_tree_maxdepth(Pool, Tree, MaxBits),
            error(empty_pool, Loc),
            throw(error(while_building_optree(
                fmt(Fmt),
                'empty pool passed to `isntrpool_tree_maxbits`'
            ), Loc))
        )
    ),
end.


instrpool_tree_maxdepth(Pool, Tree, MaxBits) :-
    instrpool_tree_maxdepth_mode(Pool, Tree, MaxBits, group_subcats).

instrpool_tree_maxdepth_mode([], _, _, _) :- throw(error(empty_pool, _)).
instrpool_tree_maxdepth_mode(SubcatInstr, subtree(Subcat, Tree), Depth, _) :-
    SubcatInstr = [subcat_tags_instrs(Subcat, _Tags, Instrs)],
    !,
    instrpool_tree_maxdepth_mode(Instrs, Tree, Depth, nogroup_subcats),
end.
instrpool_tree_maxdepth_mode([Instr], leaf(Instr), 0, _).
instrpool_tree_maxdepth_mode(Pool, node(LeftTree, SplitTag, RightTree), MaxBits, Mode) :-
    Pool = [_, _ | _],
    !,
    pool_split_left_right_mode(Pool, SplitTag, LeftPool, RightPool, Mode),
    instrpool_tree_maxdepth_mode(LeftPool, LeftTree, MaxBitsLeft, Mode),
    instrpool_tree_maxdepth_mode(RightPool, RightTree, MaxBitsRight, Mode),
    #MaxBits #= max(#MaxBitsLeft, #MaxBitsRight) + 1,
end.

pool_split_left_right_mode(Pool0, SplitTag, LeftPool, RightPool, Mode) :-
    ( Mode = group_subcats ->
        pool_consolidated(Pool0, Pool-Tags)
    ;
        Pool = Pool0,
        pool_all_tags(Pool, Tags)
    ),
    pool_tags_splittag(Pool, Tags, SplitTag),
    partition(tag_instr(SplitTag), Pool, RightPool, LeftPool),
    LeftPool \= Pool, RightPool \= Pool, % <1> ...because some split tags may need to be skipped.
    format('trying splittag `~q`...~n', [SplitTag]),
    format('Left: ~q~nRight: ~q~n~n', [LeftPool, RightPool]),
end.

%! pool_tags_splittag(+Pool, +Tags, -SplitTag) is multi.
pool_tags_splittag(Pool, Tags, SplitTag) :-
    pool_tags_scored(Pool, Tags, ScoresTags),
    predsort(cmp_score_ascending, ScoresTags, SortedScoresTags),
    member(_SplitScore-SplitTag, SortedScoresTags), % Generate many solns here... <1>
    print_scorestags(SortedScoresTags), %<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<DEBUG
end.

pool_tags_scored(Pool, Tags, ScoresTags) :-
    maplist({Pool}/[Tag, Score-Tag]>>(
        pool_tag_score(Pool, Tag, Score)
    ), Tags, ScoresTags),
end.


fmt_all_instrs(Fmt, Instrs) :-
    aggregate_all(bag(Instr), fmt_instr(Fmt, Instr), Instrs).


pool_instr_tag(Pool, Instr, Tag) :-
    member(Instr, Pool),
    instr_tag(Instr, Tag).

pool_all_tags(Pool, Tags) :-
       aggregate_all(set(Tag), (
        pool_instr_tag(Pool, _Instr, Tag)
    ), Tags).

dedupe(Original, Deduped) :- sort(Original, Deduped).

pool_consolidated(Pool0, Pool-Tags) :-
    pool_all_tags(Pool0, Tags0),
    pool_tags_consolidated(Pool0, Tags0, Pool-Tags),
end.

% Remove all instrs in `Pool` that are members of any subcat; replace with
% one psuedo-instr `subcat_tags_instrs(_,_,_)` for each subcat.
% Also remove all `aligned_subcat(_)` tags.
pool_tags_consolidated(Pool0, Tags0, Pool-Tags) :-
    partition(=(aligned_subcat(_)), Tags0, SubcatTags, NonSubcatTags0), 
    maplist([aligned_subcat(Subcat), Subcat]>>true, SubcatTags, Subcats0),
    dedupe(Subcats0, Subcats),
    dedupe(NonSubcatTags0, NonSubcatTags),
    Tags = NonSubcatTags,

    exclude([I]>>instr_tag(I, aligned_subcat(_)), Pool0, NonSubcatInstrs),
    maplist(pool_subcat_consolidated(Pool0), Subcats, ConsolidatedInstrs),
    append(NonSubcatInstrs, ConsolidatedInstrs, Pool),
end.

%% Remove all instrs in `Pool` that are members of `Subcat`; replace with one
%% psuedo-instr `subcat_tags_instrs(_,_,_)`.
pool_subcat_consolidated(Pool, Subcat, subcat_tags_instrs(Subcat, Tags, Instrs)) :-
    maplist(instr_normaltags, Pool, TagLists),
    flatten(TagLists, TagsWithDups),
    dedupe(TagsWithDups, Tags),
    include(
        {Subcat}/[Instr]>>instr_tag(Instr, aligned_subcat(Subcat)),
        Pool,
        Instrs
    ),
end.


instr_normaltags(Instr, Tags) :-
    instr_info(Instr, Info),
    exclude(=(aligned_subcat(_)), Info.tags, Tags).


% `Occurrances` is the number of instructions in `Pool` that have the tag `Tag`.
pool_tag_occurrances(Pool, Tag, Occurrances) :-
    aggregate_all(count, Instr,
        pool_instr_tag(Pool, Instr, Tag),
    Occurrances),
end.


pool_subcat_instrs([], _Subcat, []).
pool_subcat_instrs([I | Is0], Subcat, [I | Is]) :-
    instr_tag(I, aligned_subcat(Subcat)), !,
    pool_subcat_instrs(Is0, Subcat, Is).
pool_subcat_instrs([_ | Is0], Subcat, Is) :-
    pool_subcat_instrs(Is0, Subcat, Is).


pool_tag_frequency(Pool, Tag, Freq) :-
    pool_tag_occurrances(Pool, Tag, Occurrances),
    length(Pool, PoolSize),
    Freq is Occurrances / PoolSize.


pool_tag_score(Pool, instr(I), Score) :- !,
    pool_tag_frequency(Pool, instr(I), Freq),
    Score is 0.75 * (1.0 - 2.0 * abs(0.5 - Freq)).
pool_tag_score(Pool, subcat_tags(_Subcat, Tags), Score) :- !,
    maplist(pool_tag_score(Pool), Tags, Scores),
    foldl(average, Scores, 0, Score).
pool_tag_score(Pool, Tag, Score) :-
    pool_tag_frequency(Pool, Tag, Freq),
    Score is (1.0 - 2.0 * abs(0.5 - Freq)).

cmp_score_ascending(Ord, Score1-Tag1, Score2-Tag2) :-
    ( Score1 > Score2 ->
        Ord = '<'
    ; Score1 < Score2 ->
        Ord = '>'
    ; Tag1 @< Tag2 ->
        Ord = '<'
    ;
        Ord = '>'
    ).

average(X, Y, Average) :- Average is (X + Y) / 2.


tag_instr(Tag, Instr) :- instr_tag(Instr, Tag).

instr_tag(subcat_tags_instrs(_, Tags, _), Tag) :- !, member(Tag, Tags).
instr_tag(Instr, Tag) :-
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


print_scorestags(ScoresTags) :-
    format('~|~`-t~80|~n', []),
    maplist([Score-Tag]>>(
        ScorePct is round(100 * Score),
        BarLength is round(80 * Score),
        format('~q~t~0f%~30|  ~|~`#t~*+~n', [Tag, ScorePct, BarLength])
    ), ScoresTags),
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% optree_instr_prefix(+Tree, +Instr, -Prefix:list) is det
%
optree_instr_prefix(empty, _, _) :- throw(error(empty_tree, _)).
optree_instr_prefix(Tree, Instr, Prefix) :-
    phrase(optree_instr_prefix_(Tree, Instr), Prefix).

optree_instr_prefix_(leaf(Instr), Instr) --> [].
optree_instr_prefix_(subtree(_SplitTag, Tree), Instr) -->
    optree_instr_prefix_(Tree, Instr).
optree_instr_prefix_(node(Left, _SplitTag, Right), Instr) -->
    ( ['0'], optree_instr_prefix_(Left, Instr) -> []
    ; ['1'], optree_instr_prefix_(Right, Instr) -> []
    ).

