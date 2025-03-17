
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
:- use_module(derive).
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

'incompatible bit sizes'(instruction(Instr), val1(_V1), val2(_V2)) :-
    sem:instr_info(Instr, Info),
    isa:fmt_instr_title_description(Fmt, Instr, _, _),
    derive:fmt_opcodebits_immbits(Fmt, _, ImmBits),
    maplist(
        {ImmBits}/[Op, OpName-OpTy]>>operand_immbits_name_type(Op, ImmBits, OpName, OpTy),
        Info.operands,
        _OpNamesOpTys
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

ty(u(N), N) :- N in 1 .. sup.
ty(s(N), N) :- N in 1 .. sup.
ty(i(N), N) :- N in 1 .. sup.

ty_newsize(u(_), N, u(N)) :- N in 1 .. sup.
ty_newsize(s(_), N, s(N)) :- N in 1 .. sup.
ty_newsize(i(_), N, i(N)) :- N in 1 .. sup.

:- discontiguous inference/3.

inference(_Tcx, #Term, Ty) :-
    ( number(Term) ->
        N = Term,
        ty(Ty, Bits),
        zcompare(Ordering, N, 0),
        inference_bit_ord(Ordering, N, Bits, Ty)
    ; atom(Term) ->
        Constant = Term,
        sem:def(#Constant, N),
        zcompare(Ordering, N, 0),
        inference_bit_ord(Ordering, N, _Bits, Ty)
    ).

% Type coercion sytax.
% `#123/9` -> 9-bit integer
% `#123/s` -> signed integer
% `#123/i/32` -> 32-bit bitvector
% `#123/8/u` -> 8-bit unsigned integer
% Note: `#9999999999999999/8` is invalid, cause the number doesn't fit in 8 bits.
inference(Tcx, Expr/Term, Ty) :-
    inference(Tcx, Expr, ETy),
    ( integer(Term) ->
        Bits = Term,
        ( rval_consteval(Expr, #Number) ->
            (
                (ETy = i(_) ; ETy = u(_)),
                #Number #< 2 ^ #Bits
            ;
                ETy = s(_),
                #Number #< 2 ^ (#Bits - 1),
                #Number #>= -1 * 2 ^ (#Bits - 1)
            )
        ;
            true
        ),
        ty_newsize(ETy, Bits, Ty)
    ; member(Term, [u, s, i]) ->
        IntType = Term,
        ty(ETy, EBits),
        Ty =.. [IntType, EBits]
    ).

inference_bit_ord(>, N, Bits, u(Bits)) :- 2 ^ #Bits #> #N.
inference_bit_ord(>, N, Bits, s(Bits)) :- 2 ^ (#Bits - 1) #> #N.
inference_bit_ord(>, N, Bits, i(Bits)) :- 2 ^ #Bits #> #N.
inference_bit_ord(<, N, Bits, s(Bits)) :- -1 * 2 ^ (#Bits - 1) #< #N.
inference_bit_ord(<, N, Bits, i(Bits)) :- -1 * 2 ^ (#Bits - 1) #< #N.
    

inference(Tcx, A + B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A - B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A == B, bool) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A \= B, bool) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A and B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A or B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A xor B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A << B, TyA) :-
    inference(Tcx, A, TyA),
    ty(TyA, TyABits),
    inference(Tcx, B, u(IdxBits)),
    2 ^ #IdxBits #>= #TyABits.
inference(Tcx, A >> B, TyA) :-
    inference(Tcx, A, TyA),
    ty(TyA, TyABits),
    inference(Tcx, B, u(IdxBits)),
    2 ^ #IdxBits #>= #TyABits.

inference(Tcx, compare(A, <(Ty), B), bool) :-
    inference(Tcx, A, Ty),
    inference(Tcx, B, Ty).
inference(Tcx, compare(A, >(Ty), B), bool) :-
    inference(Tcx, A, Ty),
    inference(Tcx, B, Ty).
inference(Tcx, compare(A, <=(Ty), B), bool) :-
    inference(Tcx, A, Ty),
    inference(Tcx, B, Ty).
inference(Tcx, compare(A, >=(Ty), B), bool) :-
    inference(Tcx, A, Ty),
    inference(Tcx, B, Ty).

inference(Tcx, ?Symbol, Ty) :-
    member(Symbol-Ty, Tcx).
inference(Tcx, [RVal], u(8)) :-
    inference(Tcx, RVal, u(16)) -> true
    ; throw(error('memory access must produce a `u(16)` address'(RVal))).
inference(_, $Reg, i(Bits)) :- isa:regname_uses(Reg, _), isa:register_size(Bits).
inference(_, $$SysReg, i(Bits)) :- isa:sysregname_name_size_description(SysReg, _, Bits, _).
inference(Tcx, attr(Path), Ty) :-
    sem:def(attr(Path), Value),
    ( Value = signal -> Ty = i(1)
    ; inference(Tcx, Value, Ty)
    ).

inference(Tcx, b_pop(LVal), bool) :-
    inference(Tcx, LVal, Ty),
    ty(Ty, _).

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
    inference(Tcx, Expr, u(_)),
    throw(error('`sxt` cannot accept a `u(_)` argument.'(expression(Expr)))).
inference(Tcx, zxt(Expr), _) :-
    inference(Tcx, Expr, s(_)),
    throw(error('`zxt` cannot accept a `s(_)` argument.'(expression(Expr)))).

inference(Tcx, hi(X), Ty) :-
    inference(Tcx, X, XTy),
    ty(XTy, XTyBits),
    XTyBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XTyBits div 2,
    ty_newsize(XTy, HalfBits, Ty).

inference(Tcx, lo(X), Ty) :-
    inference(Tcx, X, XTy),
    ty(XTy, XTyBits),
    XTyBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XTyBits div 2,
    ty_newsize(XTy, HalfBits, Ty).

inference(Tcx, hi_lo(Hi, Lo), Ty) :-
    inference(Tcx, Hi, HiTy),
    inference(Tcx, Lo, LoTy),
    HiTy = LoTy,
    ty(HiTy, Bits),
    #WholeBits #= 2 * #Bits,
    ty_newsize(HiTy, WholeBits, Ty).

inference(Tcx, { Elements }, Ty) :-
    comma_list(Elements, EleList),
    fold_concat_element_tys(Tcx, EleList, Ty).

fold_concat_element_tys(_, [], i(0)).
fold_concat_element_tys(Tcx, [X | Xs], i(N)) :-
    inference(Tcx, X, XTy),
    ty(XTy, XBits),
    #N #= #N0 + #XBits,
    fold_concat_element_tys(Tcx, Xs, i(N0)).
    

inference(Tcx, bit(LVal, Index), Ty) :-
    inference(Tcx, Index, u(IndexBits)),
    inference(Tcx, LVal, LValTy),
    ty(LValTy, LValTyBits),
    2 ^ #IndexBits #>= #LValTyBits,
    ty_newsize(LValTy, 1, Ty).

inference(Tcx, bitslice(LVal, HiExpr .. LoExpr), Ty) :-
    inference(Tcx, LVal, LValTy),
    inference(Tcx, HiExpr, u(IndexBits)),
    inference(Tcx, LoExpr, u(IndexBits)),
    rval_consteval(HiExpr, #Hi),
    rval_consteval(LoExpr, #Lo),
    #Hi #> #Lo,
    ty(LValTy, LValTyBits),
    2 ^ #IndexBits #> #LValTyBits,
    #NewSize #= #Hi - #Lo,
    ty_newsize(LValTy, NewSize, Ty).

rval_consteval(#N, #N) :- integer(N).
rval_consteval(#Sym, #N) :-
    atom(Sym),
    ( sem:def(#Sym, N) -> true
    ; throw(error('undefined constant symbol'(#Sym)))
    ).

stmt_inference(Tcx, todo, Tcx).

stmt_inference(Tcx, b_push(LVal, RVal), Tcx) :-
    inference(Tcx, LVal, LValTy),
    inference(Tcx, RVal, bool),
    ty(LValTy, _).

stmt_inference(Tcx, if(Cond, Consq), Tcx) :-
    (inference(Tcx, Cond, bool) -> true ; throw(error('if condition must be type bool'))),
    stmt_inference(Tcx, Consq, _).

stmt_inference(Tcx, if(Cond, Consq, Alt), Tcx) :-
    (inference(Tcx, Cond, bool) -> true ; throw(error('if condition must be type bool'))),
    stmt_inference(Tcx, Consq, _),
    stmt_inference(Tcx, Alt, _).

stmt_inference(Tcx0, First ; Rest, Tcx) :-
    ( First = (?Var = Expr) ->
        inference(Tcx0, Expr, ExprTy),
        TcxExt = [Var-ExprTy | Tcx0],
        stmt_inference(TcxExt, Rest, Tcx)
    ;
        stmt_inference(Tcx0, First, Tcx1),
        stmt_inference(Tcx1, Rest, Tcx)
    ).

stmt_inference(Tcx, Dst <- Src, Tcx) :-
    inference(Tcx, Dst, DstTy),
    inference(Tcx, Src, SrcTy),
    supertype_subtype(DstTy, SrcTy).




