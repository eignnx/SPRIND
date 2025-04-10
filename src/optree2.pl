% If an instruction contains a `aligned_subcat(_)` tag, it should be excluded from the pool.
% All instructions with tag `aligned_subcat(X)` should be replaced by a single pseudo-
% instruction called `subcat(X)` whose tags are the union of the tags of all instructions 
% in the subcategory.

:- module(optree2, [
    % print_report/0,
    % print_dottrees/0,
    % print_dottree/1,
    % fmt_tree/2,
    % optree_instr_prefix/3
]).

:- use_module(isa, [fmt_instr/2]).
:- use_module(sem, [instr_info/2]).
:- use_module(utils).
:- use_module(library(clpfd)).
:- op(20, fx, #).

fmt_tree_maxbits(Fmt, Tree, MaxBits) :-
	fmt_all_instrs(Fmt, Pool),
	catch(
		instrpool_tree(Pool, Tree, MaxBits),
		error(empty_pool, Loc),
		throw(error(while_building_optree(
			fmt(Fmt),
			'empty pool passed to `isntrpool_tree`'
		), Loc))
	),
end.

instrpool_tree([], _, _) :- throw(error(empty_pool, _)).
instrpool_tree([Instr], Tree, Depth) :-
	( Instr = subcat_tags_instrs(Subcat, _Tags, Instrs) ->
		instrpool_tree(Instrs, Tree0, Depth),
		Tree = subtree(Subcat, Tree0)
	;
		Tree = leaf(Instr), Depth = 0
	).
instrpool_tree(Pool, node(LeftTree, SplitTag, RightTree), MaxBits) :-
	Pool = [_, _ | _],
	pool_all_tags(Pool, Tags0),
	pool_tags_consolidated(Pool, Tags0, Tags),
	maplist({Pool}/[Tag, Score-Tag]>>(
		pool_tag_score(Pool, Tag, Score)
	), Tags, ScoresTags),
	predsort(cmp_score_ascending, ScoresTags, SortedScoresTags),
	member(_SplitScore-SplitTag, SortedScoresTags), % Generate many solns here... <1> 
    partition(tag_instr(SplitTag), Pool, RightPool, LeftPool),
	LeftPool \= Pool, RightPool \= Pool, % <1> ...because some split tags may need to be skipped.
    instrpool_tree(LeftPool, LeftTree, MaxBitsLeft),
    instrpool_tree(RightPool, RightTree, MaxBitsRight),
	#MaxBits #= max(#MaxBitsLeft, #MaxBitsRight) + 1,
end.

fmt_all_instrs(Fmt, Instrs) :-
    aggregate_all(bag(Instr), fmt_instr(Fmt, Instr), Instrs).


pool_instr_tag(Pool, Instr, Tag) :-
    member(Instr, Pool),
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).

pool_all_tags(Pool, Tags) :-
    aggregate_all(set(Tag), (
		pool_instr_tag(Pool, _Instr, Tag)
	), Tags).

dedupe(Original, Deduped) :- sort(Original, Deduped).

pool_tags_consolidated(Pool, Tags0, Tags) :-
	partition(=(aligned_subcat(_)), Tags0, SubcatTags, NonSubcatTags0), 
	maplist([aligned_subcat(Subcat), Subcat]>>true, SubcatTags, Subcats0),
	dedupe(Subcats0, Subcats),
	dedupe(NonSubcatTags0, NonSubcatTags),
	maplist(pool_subcat_consolidated(Pool), Subcats, ConsolidatedSubcats),
	append(NonSubcatTags, ConsolidatedSubcats, Tags).

pool_subcat_consolidated(Pool, Subcat, subcat_tags_instrs(Subcat, Tags, Instrs)) :-
	maplist(instr_normaltags, Pool, TagLists),
	flatten(TagLists, TagsWithDups),
	dedupe(TagsWithDups, Tags),
	include(
		{Subcat}/[Instr]>>(instr_tag(Instr, aligned_subcat(Subcat))),
		% Using `@-lambda` library this would be:
		% @instr_tag(@0, aligned_subcat(Subcat))
		Pool,
		Instrs
	),
end.


instr_normaltags(Instr, Tags) :-
	instr_info(Instr, Info),
	exclude(=(aligned_subcat(_)), Info.tags, Tags).


pool_tag_occurrances(Pool, Tag, Occurrances) :-
    aggregate_all(count, pool_instr_tag(Pool, _Instr, Tag), Occurrances).


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

instr_tag(Instr, Tag) :-
    instr_info(Instr, Info),
    member(Tag, [instr(Instr) | Info.tags]).

end.
