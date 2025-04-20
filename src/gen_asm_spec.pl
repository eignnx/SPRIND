:- module(gen_asm_spec, [report/0]).

:- use_module(isa).
:- use_module(derive).
:- use_module(optree).
:- use_module(sem, [instr_info/2]).
:- use_module(utils, [write_phrase/1]).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(20, fx, #).
:- op(20, fx, ?).
:- op(200, fx, ~).
:- op(1050, xfx, <-).
:- op(600, yfx, xor).
:- op(600, yfx, and).
:- op(600, yfx, or).
:- op(600, yfx, <<).
:- op(600, yfx, >>).
:- op(150, yfx, \).

report :-
    heading,
    register_definitions,
    instruction_definitions,
    constant_definitions,
end.

heading :-
    format(';~n'),
    format('; SPRIND ISA Assembly Language Specification~n'),
    format(';~n~n'),
    format('#once~n~n'),
    isa:version(VMajor, VMinor, VPatch),
    format('#const ISA_VERSION_MAJOR = ~d~n', [VMajor]),
    format('#const ISA_VERSION_MINOR = ~d~n', [VMinor]),
    format('#const ISA_VERSION_PATCH = ~d~n~n', [VPatch]),
end.

register_definitions :-
    findall(Reg, isa:regname_uses(Reg, _), Regs),
    format('#subruledef Reg {~n'),
    maplist({Regs}/[Reg]>>(
        nth0(Idx, Regs, Reg),
        format('  ~|~w~t~3+=> 0b~|~`0t~2r~3+~n', [Reg, Idx])
    ), Regs),
    format('}~n~n'),
end.

constant_definitions :-
    derive:subr_byte_alignment(SubrAlign),
    format('#const SPRIND_SUBR_ALIGN = ~d~n~n', SubrAlign),
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instruction_definitions :-
    write_phrase(instruction_ruledef), nl, nl,
end.

instruction_ruledef -->
    `#ruledef Instruction {`, nl,
    foreach(instr_info(Instr, Info), instr_arm(Instr, Info)),
    nl,
    synthetic_instruction_definitions,
    `}`,
end.

instr_arm(Instr, Info) -->
    { instr_syntax_ctx(Instr, Info.syntax, Ctx) },
    `\t`, expand_syntax(Info.syntax, Ctx), nl,
end.


instr_syntax_ctx(Instr, Syntax, Ctx) :-
    isa:fmt_instr(Fmt, Instr),

    % Get prefix
    derive:fmt_prefix(Fmt, PrefixChars),
    atom_chars(Prefix, PrefixChars),

    % Get opcode
    once(optree:fmt_tree(Fmt, OpTree)),
    optree:optree_instr_prefix(OpTree, Instr, OpcodeChars),
    atom_chars(Opcode, OpcodeChars),

    % Get imm and opcode bit counts
    derive:fmt_opcodebits_immbits(Fmt, OBits, IBits),
    once(clpfd:label([OBits, IBits])),

    % Get name of immediate variable (if any)
    syntax_components(Syntax, Lhs, _),
    (
        member(imm(?ImmVar), Lhs), ! ;
        member(simm(?ImmVar), Lhs), ! ;
        ImmVar = '<none>'
    ),


    Ctx = ctx{
        fmt: Fmt,
        instr: Instr,
        prefix: Prefix,
        opcode: Opcode,
        obits: OBits,
        ibits: IBits,
        immvar: ImmVar
    },
end.

expand_syntax(Syntax, Ctx) -->
    { syntax_components(Syntax, Lhs, Rhs) },
    expand_syntax_ast(Lhs -> Rhs, Ctx),
end.

syntax_components({}, [], auto).
syntax_components({LhsCommas}, Lhs, auto) :- comma_list(LhsCommas, Lhs).
syntax_components({LhsCommas} -> RhsSemis, Lhs, Rhs) :-
    comma_list(LhsCommas, Lhs),
    semicolon_list(RhsSemis, Rhs),
end.

expand_syntax_ast(Lhs -> auto, Ctx) --> !,
    atom(Ctx.instr), ` `, sequence(flip(expand_lhs(Ctx)), `, `, Lhs),
    ` => `,
    autoexpand_rhs_args(Lhs, Ctx),
end.
expand_syntax_ast(Lhs -> Rhs, Ctx) -->
    atom(Ctx.instr), ` `, sequence(flip(expand_lhs(Ctx)), `, `, Lhs),
    ` => `, `{`, nl,
        expand_rhs_stmts(Rhs, Ctx),
    `\t}`,
end.

expand_lhs([Syntax], Ctx) --> `[`, expand_lhs(Syntax, Ctx), `]`.
expand_lhs(A:B, Ctx) --> `(`, expand_lhs(A, Ctx), `,`, expand_lhs(B, Ctx), `)`.
expand_lhs(Syn1 + Syn2, Ctx) --> expand_lhs(Syn1, Ctx), ` + `, expand_lhs(Syn2, Ctx).
expand_lhs(reg(_RegField, ?Ident), _Ctx) --> decl_reg(Ident).
expand_lhs(imm(?Ident), Ctx) -->
    decl(Ident, u\Ctx.ibits).
expand_lhs(simm(?Ident), Ctx) -->
    decl(Ident, s\Ctx.ibits).

autoexpand_rhs_args(Args, Ctx) -->
    { phrase(flatten_args(Args), ArgsFlat) },
    { predsort(order_args, ArgsFlat, ArgsSorted) },
    { Items = [prefix(Ctx.prefix), opcode(Ctx.opcode) | ArgsSorted] },
    sequence(flip(expand_rhs_from_lhs(Ctx)), ` @ `, Items),
end.

flatten_args([]) --> [].
flatten_args([Term | Rest]) --> flatten_arg(Term), flatten_args(Rest).

flatten_arg(reg(R, Ident)) --> [reg(R, Ident)].
flatten_arg(imm(Ident)) --> [imm(Ident)].
flatten_arg(simm(Ident)) --> [simm(Ident)].
flatten_arg([Inner]) --> flatten_arg(Inner).
flatten_arg(A + B) --> flatten_arg(A), flatten_arg(B).
flatten_arg(A - B) --> flatten_arg(A), flatten_arg(B).
flatten_arg(A:B) --> flatten_arg(A), flatten_arg(B).

order_args(Delta, A, B) :-
    List = [
        imm(_),
        simm(_),
        reg(t, _),
        reg(s, _),
        reg(r, _)
    ],
    nth0(IdxA, List, A),
    nth0(IdxB, List, B),
    compare(Delta, IdxA, IdxB),
end.


:- det(expand_rhs_from_lhs//2).

expand_rhs_from_lhs(reg(_R, ?Ident), _Ctx) --> atom(Ident).
expand_rhs_from_lhs(imm(?Ident), _Ctx) --> atom(Ident).
expand_rhs_from_lhs(simm(?Ident), _Ctx) --> atom(Ident).
expand_rhs_from_lhs(prefix(Bits), _Ctx) --> `0b`, atom(Bits).
expand_rhs_from_lhs(opcode(Bits), Ctx) -->
    { format(codes(Padded), '~`0t~w~*|', [Bits, Ctx.obits]) },
    `0b`, Padded, `\``, integer(Ctx.obits),
end.

expand_rhs_stmts([], _) --> ``.
expand_rhs_stmts([Line | Lines], Ctx) -->
    `\t\t`, expand_rhs_stmt(Ctx, Line), nl,
    expand_rhs_stmts(Lines, Ctx),
end.

expand_rhs_stmt(Ctx, (?Ident := Rhs)) -->
    atom(Ident), ` = `, expand_rhs(Ctx, Rhs),
end.
expand_rhs_stmt(Ctx, {CommaList}) -->
    { comma_list(CommaList, List) },
    expand_rhs_concat_expr(List, Ctx),
end.

expand_rhs(Ctx, A - B) -->
    expand_rhs(Ctx, A), ` - `, expand_rhs(Ctx, B),
end.
expand_rhs(Ctx, A + B) -->
    expand_rhs(Ctx, A), ` + `, expand_rhs(Ctx, B),
end.
expand_rhs(Ctx, ?Ident) -->
    atom(Ident),
    ( { Ctx.immvar = Ident } -> `\``, integer(Ctx.ibits) ; [] ),
end.
expand_rhs(_Ctx, #Value) -->
    ( { integer(Value) } ->
        integer(Value)
    ; { Value = asm_pc } ->
        `$`
    ),
end.
expand_rhs(Ctx, {CommaList}) -->
    { comma_list(CommaList, Items) },
    sequence(expand_rhs(Ctx), ` @ `, Items),
end.


expand_rhs_concat_expr(Items0, Ctx) -->
    { dif(Ctx.obits, 0) ->
        Items1 = [opcode(Ctx.opcode) | Items0]
    ;
        Items1 = Items0
    },
    { Items = [prefix(Ctx.prefix) | Items1] },
    sequence(flip(expand_concat_item(Ctx)), ` @ `, Items),
end.

expand_concat_item(?Ident, _Ctx) --> atom(Ident).
expand_concat_item(prefix(Bits), _Ctx) --> `0b`, atom(Bits).
expand_concat_item(opcode(Bits), Ctx) --> `0b`, atom(Bits), `\``, integer(Ctx.obits).
expand_concat_item(?Ident\size(imm), Ctx) --> atom(Ident), `\``, integer(Ctx.ibits).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

synthetic_instruction_definitions -->
    foreach(isa:synthinstr_info(Instr, Info), synthinstr(Instr-Info), nl),
    nl,
end.

synthinstr(InstrTerm-Info) -->
    { InstrTerm =.. [Instr | Args] },
    { Info.expansion =.. [ExpInstr | ExpArgs] },
    `\t`, atom(Instr), ` `, sequence(synthinstr_arg, `, `, Args),
    ` => `,
    `asm { `,
    atom(ExpInstr), ` `, sequence(synthinstr_exparg, `, `, ExpArgs),
    ` }`,
end.

synthinstr_arg(Symbol) --> decl_reg(Symbol).

synthinstr_exparg($(Reg)) --> atom(Reg).
synthinstr_exparg(Symbol) --> { atom(Symbol), ! }, braces(atom(Symbol)).
synthinstr_exparg(N) --> { integer(N), !}, integer(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nl --> `\n`.
end --> ``.
braces(Content) --> `{`, Content, `}`.
bracks(Content) --> `[`, Content, `]`.
parens(Content) --> `(`, Content, `)`.

decl(Ident, Type) --> braces((atom(Ident), `: `, type(Type))).
type('Reg') --> `Reg`.
type(s\N) --> `s`, number(N).
type(u\N) --> `u`, number(N).
type(i\N) --> `i`, number(N).
decl_reg(Ident) --> decl(Ident, 'Reg').

instr(Name, []) --> atom(Name).
instr(Name, [A | As]) --> atom(Name), ` `, args([A | As]).
args([]) --> ``.
args([A]) --> A.
args([A, B | Rest]) --> A, `, `, args([B | Rest]).

joined([]) --> ``.
joined([A]) --> A.
joined([A, B | Rest]) --> A, ` @ `, joined([B | Rest]).

#Literal --> atom(Literal).
A\B --> A, `\``, integer(B).
?A --> atom(A).


:- meta_predicate(flip(1, ?)).
:- meta_predicate(flip(3, ?)).

flip(Expr0, Arg1) :-
    Expr0 =.. [Functor, Arg2],
    Expr =.. [Functor, Arg1, Arg2],
    call(Expr).

flip(Dcg0, Arg1) -->
    { Dcg0 =.. [Functor, Arg2] },
    { Dcg =.. [Functor, Arg1, Arg2] },
    Dcg.

% #bankdef rom {
%       #bits 8
%       #outp 0
% }

end.
