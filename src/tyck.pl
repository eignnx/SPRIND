%
% Type Checking
%
% Predicates to perform type checking on instruction semantics specifications
% found in `src/sem.pl`.
%
:- module(tyck, [
    'incompatible bit sizes'/2
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


'incompatible bit sizes'(instruction(Instr), Error) :-
    sem:instr_info(Instr, Info),
    isa:fmt_instr_title_description(Fmt, Instr, _, _),
    derive:fmt_opcodebits_immbits(Fmt, _, ImmBits),
    maplist(
        {ImmBits}/[Op, OpName-OpTy]>>operand_immbits_name_type(Op, ImmBits, OpName, OpTy),
        Info.operands,
        Tcx
    ),
    catch(
        (
            \+ stmt_inference(Tcx, Info.sem, _TcxOut),
            Error = expression(Info.sem)
        ),
        error(Error),
        true
    ).

operand_immbits_name_type(reg(?Name), _, Name, i/Bits) :- isa:register_size(Bits).
operand_immbits_name_type(imm(?Name), ImmBits, Name, u/ImmBits).
operand_immbits_name_type(simm(?Name), ImmBits, Name, s/ImmBits).

supertype_subtype(i/Bits, s/Bits).
supertype_subtype(i/Bits, u/Bits).
supertype_subtype(Ty, Ty).
% supertype_subtype(bool, i/1).
% supertype_subtype(bool, u/1).
% supertype_subtype(bool, s/1).

int_ty(u/N) :- N in 1 .. sup.
int_ty(s/N) :- N in 1 .. sup.
int_ty(i/N) :- N in 1 .. sup.

:- discontiguous inference/3.

inference(_Tcx, #Term, Ty) :-
    ( number(Term) ->
        N = Term,
        _/Bits = Ty,
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
    inference(Tcx, Expr, EKind/EBits),
    ( integer(Term) ->
        Bits = Term,
        ( rval_consteval(Expr, #Number) ->
            (
                member(EKind, [u, i]),
                #Number #< 2 ^ #Bits
            ;
                EKind = s,
                #Number #< 2 ^ (#Bits - 1),
                #Number #>= -1 * 2 ^ (#Bits - 1)
            )
        ;
            true
        ),
        Ty = EKind/Bits
    ; member(Term, [u, s, i]) ->
        IntType = Term,
        Ty = IntType/EBits
    ).

inference_bit_ord(>, N, Bits, u/Bits) :- 2 ^ #Bits #> #N.
inference_bit_ord(>, N, Bits, s/Bits) :- 2 ^ (#Bits - 1) #> #N.
inference_bit_ord(>, N, Bits, i/Bits) :- 2 ^ #Bits #> #N.
inference_bit_ord(<, N, Bits, s/Bits) :- -1 * 2 ^ (#Bits - 1) #< #N.
inference_bit_ord(<, N, Bits, i/Bits) :- -1 * 2 ^ (#Bits - 1) #< #N.
    

inference(Tcx, A + B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    (TyA = TyB -> true
    ; throw(error('bad binop'(A/TyA + B/TyB)))
    ).
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
    _/TyABits = TyA,
    inference(Tcx, B, u/IdxBits),
    2 ^ #IdxBits #>= #TyABits.
inference(Tcx, A >> B, TyA) :-
    inference(Tcx, A, TyA),
    _/TyABits = TyA,
    inference(Tcx, B, u/IdxBits),
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
inference(Tcx, [RVal], u/8) :-
    inference(Tcx, RVal, u/16) -> true
    ; throw(error('memory access must produce a `u/16` address'(RVal))).
inference(_, $Reg, i/Bits) :- isa:regname_uses(Reg, _), isa:register_size(Bits).
inference(_, $$SysReg, i/Bits) :- isa:sysregname_name_size_description(SysReg, _, Bits, _).
inference(Tcx, attr(Path), Ty) :-
    sem:def(attr(Path), Value),
    ( Value = signal -> Ty = i/1
    ; inference(Tcx, Value, Ty)
    ).

inference(Tcx, b_pop(LVal), bool) :-
    inference(Tcx, LVal, Ty),
    int_ty(Ty).

inference(Tcx, zxt(Expr), i/ZxtBits) :-
    inference(Tcx, Expr, i/ExprBits),
    #ZxtBits #>= #ExprBits.
inference(Tcx, zxt(Expr), u/ZxtBits) :-
    inference(Tcx, Expr, u/ExprBits),
    #ZxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), i/SxtBits) :-
    inference(Tcx, Expr, i/ExprBits),
    #SxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), s/SxtBits) :-
    inference(Tcx, Expr, s/ExprBits),
    #SxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), _) :-
    inference(Tcx, Expr, u/_),
    throw(error('`sxt` cannot accept a `u/_` argument.'(expression(Expr)))).
inference(Tcx, zxt(Expr), _) :-
    inference(Tcx, Expr, s/_),
    throw(error('`zxt` cannot accept a `s/_` argument.'(expression(Expr)))).

inference(Tcx, hi(X), XKind/HalfBits) :-
    inference(Tcx, X, XKind/XBits),
    XBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XBits div 2.

inference(Tcx, lo(X), XKind/HalfBits) :-
    inference(Tcx, X, XKind/XBits),
    XBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XBits div 2.

inference(Tcx, hi_lo(Hi, Lo), Kind/WholeBits) :-
    inference(Tcx, Hi, HiTy),
    inference(Tcx, Lo, LoTy),
    HiTy = LoTy,
    Kind/Bits = HiTy,
    #WholeBits #= 2 * #Bits.

inference(Tcx, { Elements }, Ty) :-
    comma_list(Elements, EleList),
    fold_concat_element_tys(Tcx, EleList, Ty).

fold_concat_element_tys(_, [], i/0).
fold_concat_element_tys(Tcx, [X | Xs], i/N) :-
    inference(Tcx, X, XTy),
    _/XBits = XTy,
    #N #= #N0 + #XBits,
    fold_concat_element_tys(Tcx, Xs, i/N0).
    

inference(Tcx, bit(LVal, Index), bool) :-
    inference(Tcx, Index, u/IndexBits),
    inference(Tcx, LVal, LValTy),
    _/LValTyBits = LValTy,
    2 ^ #IndexBits #>= #LValTyBits.

inference(Tcx, bitslice(LVal, HiExpr .. LoExpr), LValKind/NewSize) :-
    inference(Tcx, LVal, LValTy),
    inference(Tcx, HiExpr, u/IndexBits),
    inference(Tcx, LoExpr, u/IndexBits),
    rval_consteval(HiExpr, #Hi),
    rval_consteval(LoExpr, #Lo),
    #Hi #> #Lo,
    LValKind/LValBits = LValTy,
    2 ^ #IndexBits #> #LValBits,
    #NewSize #= #Hi - #Lo.

rval_consteval(#N, #N) :- integer(N).
rval_consteval(#Sym, #N) :-
    atom(Sym),
    ( sem:def(#Sym, N) -> true
    ; throw(error('undefined constant symbol'(#Sym)))
    ).

stmt_inference(Tcx, todo, Tcx).

stmt_inference(Tcx, b_push(LVal, RVal), Tcx) :-
    inference(Tcx, LVal, LValTy),
    int_ty(LValTy),
    inference(Tcx, RVal, RValTy),
    ( RValTy = bool -> true
    ; throw(error('`b_push` accepts a boolean as 2nd arg'(RVal/RValTy)))
    ).

stmt_inference(Tcx, if(Cond, Consq), Tcx) :-
    (inference(Tcx, Cond, bool) -> true ; throw(error('if condition must be type bool'(Cond)))),
    stmt_inference(Tcx, Consq, _).

stmt_inference(Tcx, if(Cond, Consq, Alt), Tcx) :-
    (inference(Tcx, Cond, bool) -> true ; throw(error('if condition must be type bool'(Cond)))),
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
    ( supertype_subtype(DstTy, SrcTy) -> true
    ; throw(error('bad assignment stmt'(Dst/DstTy <- Src/SrcTy)))
    ).



