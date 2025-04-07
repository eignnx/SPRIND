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

% Five things I hate about this code:
% - disorganization of predicate definitions (largely unordered)
% 	- dcg rules aren't structured like a grammar, they're kinda spaghetti
% 		- `{}` vs `{} -> ()` syntax is not discriminated upon very explicitly

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

instruction_definitions :-
    write_phrase(instruction_ruledef), nl, nl,
end.

constant_definitions :-
    derive:subr_byte_alignment(SubrAlign),
    format('#const SPRIND_SUBR_ALIGN = ~d~n~n', SubrAlign),
end.


instruction_ruledef -->
    `#ruledef Instruction {`, nl,
	foreach(instr_info(Instr, Info), instr_arm(Instr, Info)),
    `}`,
end.

instr_arm(Instr, Info) -->
	{ instr_ctx(Instr, Ctx) },
	`\t`, expand_syntax(Info.syntax, Ctx), nl,
end.


instr_ctx(Instr, Ctx) :-
	isa:fmt_instr(Fmt, Instr),
	derive:fmt_prefix(Fmt, PrefixChars),
	atom_chars(Prefix, PrefixChars),
	once(optree:fmt_tree(Fmt, OpTree)),
	optree:optree_instr_prefix(OpTree, Instr, OpcodeChars),
	atom_chars(Opcode, OpcodeChars),
	derive:fmt_opcodebits_immbits(Fmt, OBits, IBits),
	once(clpfd:label([OBits, IBits])),
	Ctx = ctx{
		fmt: Fmt,
		instr: Instr,
		prefix: Prefix,
		opcode: Opcode,
		obits: OBits,
		ibits: IBits
	},
end.


expand_syntax({}, Ctx) -->
	expand_syntax_ast([] -> auto, Ctx),
end.
expand_syntax({LhsCommas}, Ctx) -->
	{ comma_list(LhsCommas, Lhs) },
	expand_syntax_ast(Lhs -> auto, Ctx),
end.
expand_syntax({LhsCommas} -> RhsSemis, Ctx) -->
	{ comma_list(LhsCommas, Lhs) },
	{ semicolon_list(RhsSemis, Rhs) },
	expand_syntax_ast(Lhs -> Rhs, Ctx),
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

%%%%%%%%%%%%

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
	`0b`, Padded, `\``, integer(Ctx.obits).

expand_rhs_stmt(Ctx, (?Ident = Rhs)) -->
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
expand_rhs(_Ctx, ?Ident) --> atom(Ident).
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

expand_rhs_stmts([], _) --> ``.
expand_rhs_stmts([Line | Lines], Ctx) -->
	`\t\t`, expand_rhs_stmt(Ctx, Line), nl,
	expand_rhs_stmts(Lines, Ctx),
end.

expand_rhs_concat_expr(Items0, Ctx) -->
	{ dif(Ctx.obits, 0) ->
		Items1 = [opcode(Ctx.opcode) | Items0]
	;
		Items1 = Items0
	},
	sequence(
		flip(expand_concat_item(Ctx)),
		` @ `,
		[prefix(Ctx.prefix) | Items1]
	),
end.
expand_concat_item(?Ident, _Ctx) --> atom(Ident).
expand_concat_item(prefix(Bits), _Ctx) --> `0b`, atom(Bits).
expand_concat_item(opcode(Bits), Ctx) --> `0b`, atom(Bits), `\``, integer(Ctx.obits).

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


flip(Expr0, Arg1) :-
	Expr0 =.. [Functor, Arg2],
	Expr =.. [Functor, Arg1, Arg2],
	call(Expr).

flip(Dcg0, Arg1) -->
	{ Dcg0 =.. [Functor, Arg2] },
	{ Dcg =.. [Functor, Arg1, Arg2] },
	Dcg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*


#ruledef Instruction {
      call {addr: u16} => {
            assert(addr != 0xFFFF, "address 0xFFFF may not store the start of a subroutine")
            0b11 @ (addr << 3)`14
      }

      ; RRI
      lw {dst: Reg}, [{base: AdrReg}][{disp: s7}] => 0b10 @ 0`2 @ disp @ base @ dst
      lb {dst: Reg}, [{base: AdrReg}][{disp: s7}] => 0b10 @ 1`2 @ disp @ base @ dst
      sw [{base: AdrReg}][{disp: s7}], {src: Reg} => 0b10 @ 2`2 @ disp @ base @ src
      sb [{base: AdrReg}][{disp: s7}], {src: Reg} => 0b10 @ 3`2 @ disp @ base @ src

      ; RI
      lzi   {dst: Reg}, {imm: u8} => 0b001 @ 0`2 @ imm @ dst
      lsi   {dst: Reg}, {imm: s8} => 0b001 @ 1`2 @ imm @ dst
      lui   {dst: Reg}, {imm: i8} => 0b001 @ 2`2 @ imm @ dst
      auipc {dst: Reg}, {imm: i8} => 0b001 @ 3`2 @ imm @ dst

      ; I
      b {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 0`3 @ rel_lbl`11
      }
      bl {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 1`3 @ rel_lbl`11
      }
      be {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 2`3 @ rel_lbl`11
      }
      bg {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 3`3 @ rel_lbl`11
      }
      ; __ {abs_lbl: u12} => {
      ;       rel_lbl = (abs_lbl - $) >> 1
      ;       0b01 @ 4`3 @ rel_lbl`11
      ; }
      bge {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 5`3 @ rel_lbl`11
      }
      bne {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 6`3 @ rel_lbl`11
      }
      ble {abs_lbl: u12} => {
            rel_lbl = (abs_lbl - $) >> 1
            0b01 @ 7`3 @ rel_lbl`11
      }

      ; RRR
      lw {dst: Reg}, [{base: Reg}][{index: Reg}] => 0b0001 @ 0`3 @ dst @ base @ index
      lb {dst: Reg}, [{base: Reg}][{index: Reg}] => 0b0001 @ 1`3 @ dst @ base @ index
      sw [{base: Reg}][{index: Reg}], {src: Reg} => 0b0001 @ 2`3 @ src @ base @ index
      sb [{base: Reg}][{index: Reg}], {src: Reg} => 0b0001 @ 3`3 @ src @ base @ index
      add {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 4`3 @ dst @ src1 @ src2
      sub {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 5`3 @ dst @ src1 @ src2
      and {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 6`3 @ dst @ src1 @ src2
      or  {dst: Reg}, {src1: Reg}, {src2: Reg}    => 0b0001 @ 7`3 @ dst @ src1 @ src2

      ; Ri
      cmpi {src: Reg}, {imm: i4} => 0b00001 @  0`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  1`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  2`4 @ imm @ src
      ; __ {src: Reg}, {imm: i4} => 0b00001 @  3`4 @ imm @ src
      addi {dst: Reg}, {imm: u4} => 0b00001 @  4`4 @ imm @ dst
      subi {dst: Reg}, {imm: u4} => 0b00001 @  5`4 @ imm @ dst
      tbit {src: Reg}, {imm: u4} => 0b00001 @  6`4 @ imm @ src
      sbit {dst: Reg}, {imm: u4} => 0b00001 @  7`4 @ imm @ dst
      xori {dst: Reg}, {imm: u4} => 0b00001 @  8`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @  9`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @ 10`4 @ imm @ dst
      ; __ {dst: Reg}, {imm: u4} => 0b00001 @ 11`4 @ imm @ dst
      lsli {dst: Reg}, {imm: u4} => 0b00001 @ 12`4 @ imm @ dst
      lsri {dst: Reg}, {imm: u4} => 0b00001 @ 13`4 @ imm @ dst
      asri {dst: Reg}, {imm: u4} => 0b00001 @ 14`4 @ imm @ dst
      roti {dst: Reg}, {imm: s4} => 0b00001 @ 15`4 @ imm @ dst

      ; RR1
      cmp  {src1: Reg}, {src2: Reg}    => 0b000001 @  0`4 @ src1 @ src2
      jral {jmp_tgt: Reg}, {link: Reg} => 0b000001 @  1`4 @ jmp_tgt @ link
      exch {reg1: Reg}, {reg2: Reg}    => 0b000001 @  2`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @  3`4 @ reg1 @ reg2
      add  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  4`4 @ reg1 @ reg2
      sub  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  5`4 @ reg1 @ reg2
      and  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  6`4 @ reg1 @ reg2
      or   {reg1: Reg}, {reg2: Reg}    => 0b000001 @  7`4 @ reg1 @ reg2
      xor  {reg1: Reg}, {reg2: Reg}    => 0b000001 @  8`4 @ reg1 @ reg2
      xnor {reg1: Reg}, {reg2: Reg}    => 0b000001 @  9`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 10`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 11`4 @ reg1 @ reg2
      mulstep {reg1: Reg}, {reg2: Reg} => 0b000001 @ 12`4 @ reg1 @ reg2
      ; __ {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 13`4 @ reg1 @ reg2
      nand {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 14`4 @ reg1 @ reg2
      nor  {reg1: Reg}, {reg2: Reg}    => 0b000001 @ 15`4 @ reg1 @ reg2

      ; RR2
      pushw [{addr: Reg}], {src: Reg} => 0b0000001 @ 0`3 @ addr @ src
      pushb [{addr: Reg}], {src: Reg} => 0b0000001 @ 1`3 @ addr @ src
      popw  {dst: Reg}, [{addr: Reg}] => 0b0000001 @ 2`3 @ addr @ dst
      popw  {dst: Reg}, [{addr: Reg}] => 0b0000001 @ 3`3 @ addr @ dst
      ; __
      ; __
      ; __
      ; __

      ; R
      seb {reg: Reg}   => 0b00000001 @ 0`5 @ reg
      rvend {reg: Reg} => 0b00000001 @ 1`5 @ reg
      rf {reg: Reg}    => 0b00000001 @ 2`5 @ reg
      wf {reg: Reg}    => 0b00000001 @ 3`5 @ reg
      ; ...

      ; O
      NONEXE0 => 0b00000000 @ 0`8
      BREAK   => 0b00000000 @ 1`8
      HALT    => 0b00000000 @ 2`8
      UNIMPL  => 0b00000000 @ 3`8
      kcall   => 0b00000000 @ 4`8
      kret    => 0b00000000 @ 5`8
      inrd    => 0b00000000 @ 6`8
      inre    => 0b00000000 @ 7`8

      ; SYNTHETIC INSTRUCTIONS
      mov {dst: Reg}, {src: Reg} => asm{ or {dst}, {src} }
      clear {reg: Reg} => asm{ xor {reg}, {reg} }
      nop => asm{ or zero, zero }
      ret => asm{ jral ra, zero }
      jr {reg: Reg} => asm{ jral {reg}, zero }
      incr {reg: Reg} => asm{ addi {reg}, 1 }
      decr {reg: Reg} => asm{ subi {reg}, 1 }
      neg {reg: Reg} => asm{ sub {reg}, zero, {reg} }
      inv {reg: Reg} => asm{ xnor {reg}, zero }
      not {reg: Reg} => asm{ xori {reg}, 1 }
      NONEXE1 => 0xFFFF
}


#bankdef rom {
      #bits 8
      #outp 0
}

#const SPRIND_SUBR_ALIGN = 4
*/

end.
