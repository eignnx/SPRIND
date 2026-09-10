:- module(consts, [
    def_const/2
]).

:- use_module(querilog_syntax).
:- use_module(utils, [
    template_goal_condition_index/4
]).

% Condition Codes Register ($CC): bit assignments
def_iota(cc, carry_flag_bit).
def_iota(cc, overflow_flag_bit).
def_iota(cc, jmp_tgt_validation_req_flag_bit).
def_iota(cc, jmp_tgt_validation_en_flag_bit).

% Interrupt Service Routines: vector indices
def_iota(isr, nonexe0).
def_iota(isr, break).
def_iota(isr, unimpl).

def_const(subr_align, Align) :- once(derive:subr_byte_alignment(Align)).
def_const(reg_size_bits, Size) :- isa:register_size(Size).
def_const(cc::Id, Value) :- iota_index(cc, Id, Value).
def_const(isr::Id, Value) :- iota_index(isr, Id, Value).


iota_index(CounterId, ConstId, Index) :-
    template_goal_condition_index(Id, def_iota(CounterId, Id), ConstId, Index),
    !.
iota_index(CounterId, ConstId, _) :-
    setof(X:Y, def_iota(X, Y), Ids),
    domain_error(oneof(Ids), CounterId:ConstId).

