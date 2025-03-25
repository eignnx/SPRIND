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
:- op(250, yfx, \).

:- use_module(library(clpfd)).
:- use_module(isa).
:- use_module(derive).
:- use_module(sem).


'incompatible bit sizes'(instruction(Instr), Error) :-
    sem:instr_info(Instr, Info),
    isa:fmt_instr(Fmt, Instr),
    derive:fmt_opcodebits_immbits(Fmt, _, ImmBits),
    maplist(
        {ImmBits}/[Op, OpName-OpTy]>>operand_immbits_name_type(Op, ImmBits, OpName, OpTy),
        Info.operands,
        Tcx
    ),
    catch(
        (
            \+ stmt_inference(Tcx, Info.sem, _TcxOut),
            Error = stmt(Info.sem)
        ),
        error(Error),
        true
    ).

operand_immbits_name_type(reg(?Name), _, Name, i\Bits) :- isa:register_size(Bits).
operand_immbits_name_type(imm(?Name), ImmBits, Name, u\ImmBits).
operand_immbits_name_type(simm(?Name), ImmBits, Name, s\ImmBits).

supertype_subtype(i\Bits, s\Bits).
supertype_subtype(i\Bits, u\Bits).
supertype_subtype(Ty, Ty).

int_ty(u\N) :- N in 1 .. sup.
int_ty(s\N) :- N in 1 .. sup.
int_ty(i\N) :- N in 1 .. sup.

:- discontiguous inference/3.

inference(_Tcx, #Term, Ty) :-
    ( integer(Term) ->
        N = Term,
        integer_inference(N, Ty)
    ; atom(Term) ->
        Constant = Term,
        sem:def(#Constant, N),
        integer_inference(N, Ty)
    ).

integer_inference(N, Ty) :-
    zcompare(Ordering, N, 0),
    ord_inference(Ordering, Ty, N).


ord_inference(>, u\Bits, N) :- 2 ^ #Bits #> #N.
ord_inference(>, s\Bits, N) :- 2 ^ (#Bits - 1) #> #N.
ord_inference(>, i\Bits, N) :- 2 ^ #Bits #> #N.
ord_inference(<, s\Bits, N) :- -1 * 2 ^ (#Bits - 1) #< #N.
ord_inference(<, i\Bits, N) :- -1 * 2 ^ (#Bits - 1) #< #N.
ord_inference(=, u\Bits, 0) :- Bits in 1 .. sup.
ord_inference(=, s\Bits, 0) :- Bits in 1 .. sup.
ord_inference(=, i\Bits, 0) :- Bits in 1 .. sup.
    
kind(u).
kind(s).
kind(i).

% Type coercion sytax.
% `#123\9` -> 9-bit integer
% `#123\s` -> signed integer
% `#123\i\32` -> 32-bit bitvector
% `#123\8\u` -> 8-bit unsigned integer
% Note: `#9999999999999999\8` is invalid, cause the number doesn't fit in 8 bits.
inference(_Tcx, #N\Kind, Kind\Bits) :-
    integer(N), kind(Kind), !,
    integer_inference(N, Kind\Bits).
inference(_Tcx, #N\Bits, Kind\Bits) :-
    integer(N), integer(Bits), !,
    integer_inference(N, Kind\Bits).
inference(Tcx, Expr\Kind\Bits, Kind\Bits) :-
    kind(Kind), integer(Bits), !,
    inference(Tcx, Expr, Kind\Bits).
inference(Tcx, Expr\Bits, Kind\Bits) :- integer(Bits), inference(Tcx, Expr, Kind\_).
inference(Tcx, Expr\u, u\Bits) :- inference(Tcx, Expr, _\Bits).
inference(Tcx, Expr\s, s\Bits) :- inference(Tcx, Expr, _\Bits).
inference(Tcx, Expr\i, i\Bits) :- inference(Tcx, Expr, _\Bits).

inference(Tcx, A + B, Ty) :-
    inference(Tcx, A, Ty),
    inference(Tcx, B, Ty).
inference(Tcx, A - B, TyA) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A == B, i\1) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A \= B, i\1) :-
    inference(Tcx, A, TyA),
    inference(Tcx, B, TyB),
    TyA = TyB.
inference(Tcx, A and B, i\Bits) :-
    inference(Tcx, A, _\Bits),
    inference(Tcx, B, _\Bits).
inference(Tcx, A or B, i\Bits) :-
    inference(Tcx, A, _\Bits),
    inference(Tcx, B, _\Bits).
inference(Tcx, A xor B, i\Bits) :-
    inference(Tcx, A, _\Bits),
    inference(Tcx, B, _\Bits).

inference(Tcx, A << B, TyA) :-
    inference(Tcx, A, TyA),
    TyB = u\IdxBits,
    ( inference(Tcx, B, TyB) -> true
    ; throw(error('shift amount must be an unsigned integer'(B\TyB)))
    ),
    _\TyABits = TyA,
    ( 2 ^ #IdxBits #>= #TyABits -> true
    ; throw(error('type of shift amount must have at least lg(N) bits to shift a value with N bits.'(A\TyA >> B\TyB)))
    ).
inference(Tcx, A >> B, TyA) :-
    inference(Tcx, A, TyA),
    TyB = u\IdxBits,
    ( inference(Tcx, B, TyB) -> true
    ; throw(error('shift amount must be an unsigned integer'(B\TyB)))
    ),
    _\TyABits = TyA,
    ( 2 ^ #IdxBits #>= #TyABits -> true
    ; throw(error('type of shift amount must have at least lg(N) bits to shift a value with N bits.'(A\TyA >> B\TyB)))
    ).

inference(Tcx, ~(A), i\Bits) :-
    inference(Tcx, A, _\Bits).
inference(Tcx, !(A), i\1) :-
    inference(Tcx, A, _\1).

%% meet(Lhs:ty, Meeting:ty, Rhs:ty).
meet(u\Bits, i\Bits, i\Bits).
meet(s\Bits, i\Bits, i\Bits).
meet(i\Bits, i\Bits, i\Bits).
meet(i\Bits, i\Bits, u\Bits).
meet(i\Bits, i\Bits, s\Bits).
meet(s\Bits, s\Bits, s\Bits).
meet(u\Bits, u\Bits, u\Bits).

inference(Tcx, compare(A, RelOp, B), i\1) :-
    relop(RelOp, Ty),
    ( inference(Tcx, A, TyA) -> true
    ; throw(error('failure to infer `compare` operand'(A)))
    ),
    ( TyA = Ty -> true
    ; throw(error('`compare` operand type doesn''t match operator type'(compare(A\TyA, RelOp, B\TyB))))
    ),
    ( inference(Tcx, B, TyB) -> true
    ; throw(error('failure to infer `compare` operand'(B)))
    ),
    ( TyB = Ty -> true
    ; throw(error('`compare` operand type doesn''t match operator type'(compare(A\TyA, RelOp, B\TyB))))
    ).

relop(<(Ty), Ty) :- int_ty(Ty).
relop(>(Ty), Ty) :- int_ty(Ty).
relop(<=(Ty), Ty) :- int_ty(Ty).
relop(>=(Ty), Ty) :- int_ty(Ty).


inference(Tcx, [Address], _\8) :-
    inference(Tcx, Address, AddressTy),
    ( AddressTy = u\16 -> true
    ; throw(error('memory must be accessed with a `u\16` address'(Address\AddressTy)))
    ).

inference(Tcx, ?Symbol, Ty) :-
    member(Symbol-Ty, Tcx).
inference(_, $Reg, i\Bits) :- isa:regname_uses(Reg, _), isa:register_size(Bits).
inference(_, $$SysReg, i\Bits) :- isa:sysregname_name_size_description(SysReg, _, Bits, _).
inference(Tcx, attr(Path), Ty) :-
    sem:def(attr(Path), Value),
    ( Value = signal -> Ty = i\1
    ; inference(Tcx, Value, Ty)
    ).

inference(Tcx, b_pop(LVal), i\1) :-
    inference(Tcx, LVal, Ty),
    int_ty(Ty).

inference(Tcx, zxt(Expr), i\ZxtBits) :-
    inference(Tcx, Expr, i\ExprBits),
    #ZxtBits #>= #ExprBits.
inference(Tcx, zxt(Expr), u\ZxtBits) :-
    inference(Tcx, Expr, u\ExprBits),
    #ZxtBits #>= #ExprBits.

inference(Tcx, sxt(Expr), i\SxtBits) :-
    inference(Tcx, Expr, i\ExprBits),
    #SxtBits #>= #ExprBits.
inference(Tcx, sxt(Expr), s\SxtBits) :-
    inference(Tcx, Expr, s\ExprBits),
    #SxtBits #>= #ExprBits.

inference(Tcx, sxt(Expr), _) :-
    inference(Tcx, Expr, u\_),
    throw(error('`sxt` cannot accept a `u\\_` argument.'(expression(Expr)))).
inference(Tcx, zxt(Expr), _) :-
    inference(Tcx, Expr, s\_),
    throw(error('`zxt` cannot accept a `s\\_` argument.'(expression(Expr)))).

inference(Tcx, hi(X), i\HalfBits) :-
    inference(Tcx, X, _\XBits),
    XBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XBits div 2.

inference(Tcx, lo(X), i\HalfBits) :-
    inference(Tcx, X, _\XBits),
    XBits in 16 \/ 32 \/ 64,
    #HalfBits #= #XBits div 2.

inference(Tcx, hi_lo(Hi, Lo), i\WholeBits) :-
    inference(Tcx, Hi, HiTy),
    inference(Tcx, Lo, LoTy),
    HiTy = LoTy,
    _\Bits = HiTy,
    #WholeBits #= 2 * #Bits.

inference(Tcx, { Elements }, Ty) :-
    comma_list(Elements, EleList),
    fold_concat_element_tys(Tcx, EleList, Ty).

fold_concat_element_tys(_, [], i\0).
fold_concat_element_tys(Tcx, [X | Xs], i\N) :-
    inference(Tcx, X, XTy),
    _\XBits = XTy,
    #N #= #N0 + #XBits,
    fold_concat_element_tys(Tcx, Xs, i\N0).
    

inference(Tcx, bit(LVal, Index), i\1) :-
    inference(Tcx, LVal, _\LValBits),
    inference(Tcx, Index, u\IndexBits),
    ( 2 ^ #IndexBits #>= #LValBits -> true
    ; throw(error('`bit` index value must have lg(N) bits to index a value with N bits'(bit(LVal\LValBits, Index\IndexBits))))
    ).

inference(Tcx, bitslice(LVal, HiExpr .. LoExpr), i\NewSize) :-
    inference(Tcx, LVal, _\LValBits),
    inference(Tcx, HiExpr, u\IndexBits),
    inference(Tcx, LoExpr, u\IndexBits),
    rval_consteval(HiExpr, #Hi),
    rval_consteval(LoExpr, #Lo),
    #Hi #> #Lo,
    2 ^ #IndexBits #> #LValBits,
    #NewSize #= #Hi - #Lo + 1.

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
    ( inference(Tcx, RVal, RValTy) -> true
    ; throw(error('`b_push` could not infer 2nd arg'(RVal)))
    ),
    ( RValTy = i\1 -> true
    ; throw(error('`b_push` accepts a boolean as 2nd arg'(RVal\RValTy)))
    ).

stmt_inference(Tcx, if(Cond, Consq), Tcx) :-
    (inference(Tcx, Cond, i\1) -> true ; throw(error('if condition must be type `i\\1`'(Cond)))),
    stmt_inference(Tcx, Consq, _).

stmt_inference(Tcx, if(Cond, Consq, Alt), Tcx) :-
    (inference(Tcx, Cond, i\1) -> true ; throw(error('if condition must be type `i\\1`'(Cond)))),
    stmt_inference(Tcx, Consq, _),
    stmt_inference(Tcx, Alt, _).

stmt_inference(Tcx0, First ; Rest, Tcx) :-
    ( First = (?Var = Expr) ->
        ( inference(Tcx0, Expr, ExprTy) -> true
        ; throw(error('bad binding expression'(Expr)))
        ),
        TcxExt = [Var-ExprTy | Tcx0],
        stmt_inference(TcxExt, Rest, Tcx)
    ;
        stmt_inference(Tcx0, First, Tcx1),
        stmt_inference(Tcx1, Rest, Tcx)
    ).

stmt_inference(Tcx, Dst <- Src, Tcx) :-
    inference(Tcx, Dst, DstTy),
    ( inference(Tcx, Src, SrcTy) -> true
    ; throw(error('bad binding expression'(Src)))
    ),
    ( supertype_subtype(DstTy, SrcTy) -> true
    ;
        format(atom(Msg), 'cannot store a `~q` in a `~q`', [SrcTy, DstTy]),
        throw(error('bad assignment'(Dst <- Src, Msg)))
    ).



