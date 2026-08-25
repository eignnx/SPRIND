/** <module> querilog_syntax
A Verilog-like specification language embedded in Prolog syntax.
*/
:- module(querilog_syntax, [
    op(5, fx, $),
    op(5, fx, $$),
    op(5, fx, ?),
    op(400, yfx, >>>),
    op(500, yfx, and),
    op(500, yfx, or),
    op(50, fx, #),
    op(25, xfx, \),
    op(950, xfx, <-),
    op(950, xfx, :=)
]).

