/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(mt_01,[]).

:- include(test_header).



:- begin_pfc.

:- set_defaultAssertMt(myMt).

baseKB:mtHybrid(socialMt).

socialMt:loves(sally,joe).

:- set_defaultAssertMt(myMt).

:- mpred_test(clause_u(socialMt:loves(_,_))).
:- mpred_test(\+clause_u(myMt:loves(_,_))).
:- mpred_test(\+clause_u(header_sanity:loves(_,_))).



