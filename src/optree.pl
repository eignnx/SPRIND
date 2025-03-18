:- module(optree, [

]).

:- use_module(isa, [
    fmt_instr/2
]).
:- use_module(sem, [
    instr_info/2
]).


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
        BarLength is ceil(Score),
        format('~p~t~0f%~20|  ~|~`#t~*+~n', [Tag, Score, BarLength])
    ), TagsScores).

print_scores :-
    foreach(
        isa:gfmt(Fmt),
        (
            format('-------------------~n'),
            format('| ~k~n', [Fmt]),
            format('-------------------~n'),
            print_scores(Fmt),
            format('~n')
        )
    ).