:- module(optree, [

]).

:- use_module(sem).

instr_tag(Instr, Tag) :-
    sem:instr_info(Instr, Info),
    member(Tag, Info.tags).

all_tags(Tags) :-
    aggregate_all(set(Tag), instr_tag(_, Tag), Tags).

tag_occurrances(Tag, Count) :-
    aggregate_all(count, instr_tag(_, Tag), Count).

total_instr_count(Count) :-
    aggregate_all(count, sem:instr_info(_, _), Count).

tag_frequency(Tag, Freq) :-
    tag_occurrances(Tag, TagCount),
    total_instr_count(TotalCount),
    Freq is TagCount / TotalCount.

tag_splitability(Tag, Score) :-
    tag_frequency(Tag, Freq),
    Score is 1.0 - 2.0 * abs(0.5 - Freq).

print_scores :-
    all_tags(Tags),
    maplist([Tag, Score-Tag]>>(
        tag_splitability(Tag, S),
        Score is 100 * S
    ), Tags, TagsScores),
    sort(TagsScores, TagsScoresSort),
    maplist([Score-Tag]>>(
        format('~p~t~0f%~15|~n', [Tag, Score])
    ), TagsScoresSort).