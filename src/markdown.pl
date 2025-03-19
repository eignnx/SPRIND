:- module(markdown, [
    op(500, xfy, ++),
    emit_heading/2,
    emit_table_header/1,
    emit_table_row/1
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
    format('~n~s ~w~n~n', [Hashes, Content]).


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
    phrase(sequence(`| `, variant_type, ` | `, ` |`, ColumnData), RowText),
    format('~s~n', [RowText]).


:- det(variant_type//1).

variant_type(s(Content)) --> !, string(Content).
variant_type(chars(Content)) --> !, { format(codes(Codes), '~s', [Content]) }, Codes.
variant_type(a(Content)) --> !, atom(Content).
variant_type(d(Content)) --> !, integer(Content).
variant_type(code(Inner)) --> !, `\``, variant_type(Inner), `\``.
variant_type(bold(Inner)) --> !, `**`, variant_type(Inner), `**`.
variant_type(First + Second) --> !, variant_type(First), variant_type(Second).
variant_type(First ++ Second) --> !, variant_type(First), ` `, variant_type(Second).

variant_type(fmt(FormatString, Arg1)) --> !,
    { format(atom(Formatted), FormatString, [Arg1]) },
    atom(Formatted).
variant_type(fmt(FormatString, Arg1, Arg2)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2]) },
    atom(Formatted).
variant_type(fmt(FormatString, Arg1, Arg2, Arg3)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3]) },
    atom(Formatted).
variant_type(fmt(FormatString, Arg1, Arg2, Arg3, Arg4)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4]) },
    atom(Formatted).
variant_type(fmt(FormatString, Arg1, Arg2, Arg3, Arg4, Arg5)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4, Arg5]) },
    atom(Formatted).
variant_type(fmt(FormatString, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)) --> !,
    { format(atom(Formatted), FormatString, [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]) },
    atom(Formatted).

variant_type(Other) --> { throw(error(unknown_format_command(Other))) }.