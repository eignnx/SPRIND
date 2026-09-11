:- module(querilog_tck_driver, [
    typecheck_instr/1
]).

:- use_module(querilog_syntax).
:- use_module(querilog_tck, [
    gprregister_name_size/2,        % ?Name, -Size
    sysregister_name_size/2,        % ?Name, -Size
    querilog_module_name_sig_def/3  % ?Name, -Sig, -Def
]).

:- use_module(isa).
:- use_module(sem, [instr_info/2]).


querilog_tck:gprregister_name_size(Reg, Size) :-
    isa:gprreg(Reg),
    isa:register_size(Size).

querilog_tck:sysregister_name_size(SysReg, Size) :-
    isa:sysreg_size(SysReg, Size).

querilog_tck:querilog_module_name_sig_def(ModName, Sig, Def) :-
    sem:mod_def(Sig, Def),
    is_dict(Sig, ModName).



typecheck_instr_sems :-
    findall(Status, typecheck_some_instr_sem(Status), Statuses),
    exclude(=(success), Statuses, NonSuccesses),
    partition(=(todo), NonSuccesses, Todos, Failures),
    length(Statuses, NTotal),
    length(Failures, NFail),
    length(Todos, NTodos),
    utils:list_enumerated1(Failures, FailuresEnum),
    maplist([N-F]>>(
        arg(1, F, Instr),
        format('~t~d.~3| ~p:~t~14|~p~n', [N, Instr, F])
    ), FailuresEnum),
    format('RESULTS:~n'),
    format('  * ~d failures out of ~d instructions~n', [NFail, NTotal]),
    format('  * ~d todos~n', [NTodos]),
true.
typecheck_some_instr_sem(Status) :-
    sem:instr_info(Instr, Info),
    Sem = Info.sem,
    ( Sem = todo -> Status = todo ;
        catch(
            ( typecheck_instr(Instr) ->
                Status = success
            ;
                Status = typechecking_failed(Instr, Sem)
            ),
            error(E, _),
            Status = exception(Instr, E)
        )
    ).

typecheck_instr(Instr) :-
    sem:instr_info(Instr, Info),
    isa:fmt_instr(Fmt, Instr),
    once(derive:fmt_opcodebits_immbits(Fmt, _, ImmBits)),
    syntax_operands(Info.syntax, Operands),
    maplist(tcx_binding_from_syn_operands(ImmBits), Operands, Tcx),
    querilog_tck:init_state(S0, Tcx),
    phrase(querilog_tck:stmt_typechecked(Info.sem, _TypeChecked), [S0], [_S]).

tcx_binding_from_syn_operands(ImmBits, Operand, ?VarName-Dir-Size) :-
    operand_immbits_name_size_dir(Operand, ImmBits, VarName, Size, Dir).

operand_immbits_name_size_dir(   imm(?Name), ImmBits, Name, ImmBits, net(param(in))).
operand_immbits_name_size_dir(  simm(?Name), ImmBits, Name, ImmBits, net(param(in))).
operand_immbits_name_size_dir(reg(_, ?Name),       _, Name,    Bits, reg) :-
    gprregister_name_size(_RegName, Bits).

syntax_operands({}, []).
syntax_operands({CommaList}, VarDecls) :-
    comma_list(CommaList, Operands),
    phrase(operand_vardecl(Operands), VarDecls).
syntax_operands(Lhs -> _Rhs, Operands) :- syntax_operands(Lhs, Operands).

operand_vardecl([]) --> [].
operand_vardecl([X|Xs]) -->
    ( { [Inner] = X } -> expand_bracket_content(Inner) ; [X]),
    operand_vardecl(Xs).
expand_bracket_content(A + B) --> !, [A], [B].
expand_bracket_content(A) --> [A].

