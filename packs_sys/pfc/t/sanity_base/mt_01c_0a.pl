/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(header_sane,[]).

:- include(test_header).

% :- rtrace.
:- begin_pfc.

:- must(is_pfc_file).

:- set_fileAssertMt(cycKB1).

:- listing(baseKB:mtProlog/1).
:- listing(baseKB:mtHybrid/1).
:- listing(baseKB:genlMt/2).
:- listing(cycKB1:_).
:- listing(genlMt/2).

:- mpred_trace_exec.

cycKB1:loves(sally,joe).

:- mpred_must(clause_u(cycKB1:loves(_,_))).

:- mpred_must(\+clause_u(baseKB:loves(_,_))).

:- pfc_test_feature(mt,\+ clause_u(header_sane:loves(_,_))).

:- mpred_must(clause_u(loves(_,_))).


:- mpred_must(call_u(cycKB1:loves(_,_))).

:- pfc_test_feature(mt,\+ call_u(baseKB:loves(_,_))).

:- pfc_test_feature(mt,(call_u(loves(_,_)))).

:- pfc_test_feature(mt,\+ call_u(header_sane:loves(_,_))).


