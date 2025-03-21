:- module(markdown, [
    op(500, xfy, ++),
    emit_heading/2,
    emit_table_header/1,
    emit_table_row/1,
    emit_image/2
]).

:- use_module(utils).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(500, xfy, ++).

emit_heading(PeanoLevel, Content) :-
    emit_heading(PeanoLevel, '~w', [Content]).
emit_heading(PeanoLevel, FString, FParams) :-
    utils:peano_decimal(PeanoLevel, Level),
    utils:item_count_replication('#', Level, Hashes),
    format(atom(Content), FString, FParams),
    format('~n~s ~w~n~n', [Hashes, Content]),
end.


emit_table_header(ColSpecs) :-
    maplist([Spec, Name, Align]>>(
        Spec = center(Name)  -> Align = ':---:'
        ; Spec = left(Name)  -> Align = ':----'
        ; Spec = right(Name) -> Align = '----:'
        ; Name = Spec, Align = ':---:'
    ), ColSpecs, ColNames, ColAligns),
    phrase(sequence(`| `, atom, ` | `, ` |`, ColNames), ColNamesRow),
    phrase(sequence(`|`, atom, `|`, `|`, ColAligns), HorizontalRule),
    format('~n'),
    format('~s~n', [ColNamesRow]),
    format('~s~n', [HorizontalRule]).

emit_table_row(ColumnData) :-
    phrase(sequence(`| `, interpolation_cmd, ` | `, ` |`, ColumnData), RowText),
    format('~s~n', [RowText]).

emit_image(PathFmt, FmtArgs) :-
    format(chars(Chars), PathFmt, FmtArgs),
    format('~n![~s](~s)~n', [Chars, Chars]).


:- det(interpolation_cmd//1).

interpolation_cmd(s(Content)) --> !, string(Content).
interpolation_cmd(chars(Content)) --> !, { format(codes(Codes), '~s', [Content]) }, Codes.
interpolation_cmd(a(Content)) --> !, atom(Content).
interpolation_cmd(d(Content)) --> !, integer(Content).
interpolation_cmd(code(Inner)) --> !, `\``, interpolation_cmd(Inner), `\``.
interpolation_cmd(bold(Inner)) --> !, `**`, interpolation_cmd(Inner), `**`.
interpolation_cmd(First + Second) --> !, interpolation_cmd(First), interpolation_cmd(Second).
interpolation_cmd(First ++ Second) --> !, interpolation_cmd(First), ` `, interpolation_cmd(Second).

interpolation_cmd(sectionlink(SectionName)) --> !, interpolation_cmd(sectionlink(SectionName, SectionName)).
interpolation_cmd(sectionlink(Content, SectionName)) --> !,
    { phrase(interpolation_cmd(SectionName), SectionNameRendered) },
    { utils:codes_slugified(SectionNameRendered, Slug) },
    `[`, interpolation_cmd(Content), `](#`, Slug, `)`.

interpolation_cmd(fmt(FormatString, Arg1)) --> !,
    { format(atom(Formatted), FormatString, [Arg1]) },
    atom(Formatted).
interpolation_cmd(fmt(FormatString, Arg1, Arg2)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2]) },
    atom(Formatted).
interpolation_cmd(fmt(FormatString, Arg1, Arg2, Arg3)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3]) },
    atom(Formatted).
interpolation_cmd(fmt(FormatString, Arg1, Arg2, Arg3, Arg4)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4]) },
    atom(Formatted).
interpolation_cmd(fmt(FormatString, Arg1, Arg2, Arg3, Arg4, Arg5)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4, Arg5]) },
    atom(Formatted).
interpolation_cmd(fmt(FormatString, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]) },
    atom(Formatted).

interpolation_cmd(Other) --> { throw(error(unknown_format_command(Other))) }.

end.