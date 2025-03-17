
:- module(validate, [
    op(20, fx, #),
    op(20, fx, $$),
    op(20, fx, ?),
    op(1050, xfx, <-),
    op(400, yfx, xor),
    op(400, yfx, and),
    op(400, yfx, or),
    run_validations/0
]).

:- op(20, fx, #).
:- op(20, fx, $$).
:- op(20, fx, ?).
:- op(1050, xfx, <-).
:- op(400, yfx, xor).
:- op(400, yfx, and).
:- op(400, yfx, or).

:- use_module(library(clpfd)).
:- use_module(isa).
:- use_module(sem).

run_validations :-
    disprove('instr_info is not one-to-one with fmt_instr'(_)),
    disprove('ill-formed instruction semantics'(_, _)),
    disprove('undefined constant in semantic definition'(_, _)),
    disprove('incompatible bit sizes'(_, _, _)),
    true.

disprove(NegativeCheck) :-
    NegativeCheck ->
        throw(error(validation_failed(NegativeCheck)))
    ;
        true.

'instr_info is not one-to-one with fmt_instr'(instruction(Instr)) :-
    (
        isa:instr(Instr), \+ sem:instr_info(Instr, _)
    )
    ;
    (
        sem:instr_info(Instr, _), \+ isa:instr(Instr)
    ).

'ill-formed instruction semantics'(instruction(Instr), Errors) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Emissions),
    include([error(E), E]>>true, Emissions, Errors),
    Errors = [_|_].

'undefined constant in semantic definition'(instruction(Instr), undefined_constant(Constant)) :-
    sem:instr_info(Instr, Info),
    phrase(sem:valid_semantics(Info.sem), Emissions),
    member(constant(Constant), Emissions),
    \+ sem:def(Constant, _).

'incompatible bit sizes'(instruction(Instr), val1(V1), val2(V2)) :-
    sem:instr_info(Instr, Info),
    isa:fmt_instr_title_description(Fmt, Instr, _, _),
    derive:fmt_opcodebits_immbits(Fmt, _, ImmBits),
    maplist(
        {ImmBits}/[Op, OpName-OpTy]>>operand_immbits_name_type(Op, ImmBits, OpName, OpTy),
        Info.operands,
        OpNamesOpTys
    ),
    false.

operand_immbits_name_type(reg(?Name), _, Name, i(Bits)) :- isa:register_size(Bits).
operand_immbits_name_type(imm(?Name), ImmBits, Name, u(ImmBits)).
operand_immbits_name_type(simm(?Name), ImmBits, Name, s(ImmBits)).

supertype_subtype(i(Bits), s(Bits)).
supertype_subtype(i(Bits), u(Bits)).
supertype_subtype(Ty, Ty).
% supertype_subtype(u(A), u(B)) :- #A #>= #B.
% supertype_subtype(s(A), s(B)) :- #A #>= #B.

ty(u(N), N) :- N in 1 .. 32.
ty(s(N), N) :- N in 1 .. 32.
ty(i(N), N) :- N in 1 .. 32.

:- discontiguous inference/3.

inference(_Tcx, #N, Ty) :-
    number(N),
    ty(Ty, Bits),
    zcompare(Ordering, N, 0),
    inference_bit_ord(Ordering, N, Bits, Ty).

inference_bit_ord(>, N, Bits, u(Bits)) :- 2 ^ #Bits #> #N.
inference_bit_ord(>, N, Bits, s(Bits)) :- 2 ^ (#Bits - 1) #> #N.
inference_bit_ord(>, N, Bits, i(Bits)) :- 2 ^ #Bits #> #N.
inference_bit_ord(<, N, Bits, s(Bits)) :- -1 * 2 ^ (#Bits - 1) #< #N.
inference_bit_ord(<, N, Bits, i(Bits)) :- -1 * 2 ^ (#Bits - 1) #< #N.
    

inference(Tcx, A + B, Ty) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.

inference(Tcx, ?Symbol, Ty) :-
    member(Symbol-Ty, Tcx).

inference(Tcx, zxt(Expr), i(ZxtBits)) :-
    inference(Tcx, Expr, i(ExprBits)),
    #ZxtBits #>= #ExprBits.
inference(Tcx, zxt(Expr), u(ZxtBits)) :-
    inference(Tcx, Expr, u(ExprBits)),
    #ZxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), i(SxtBits)) :-
    inference(Tcx, Expr, i(ExprBits)),
    #SxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), s(SxtBits)) :-
    inference(Tcx, Expr, s(ExprBits)),
    #SxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), _) :-
    inference(Tcx, Expr, u(ExprBits)),
    throw(error('`sxt` cannot accept a `u(_)` argument.'(expression(Expr)))).
inference(Tcx, zxt(Expr), _) :-
    inference(Tcx, Expr, s(ExprBits)),
    throw(error('`zxt` cannot accept a `s(_)` argument.'(expression(Expr)))).

stmt_inference(Tcx0, ?Var = Expr ; Rest0, Tcx) :-
    inference(Tcx0, Expr, ExprTy),
    TcxExt = [Var-ExprTy | Tcx0],
    stmt_inference(TcxExt, Rest0, Rest, Tcx).

stmt_inference(Tcx, Dst <- Src, Tcx) :-
    inference(Tcx, Dst, DstTy),
    inference(Tcx, Src, SrcTy),
    supertype_subtype(DstTy, SrcTy).

stmt_inference(Tcx, todo, todo, Tcx).



