/* <module>
%
%  PFC is a0 language extension for prolog.
%
%  It adds a0 new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(mt_06,[]).

:- include(test_header).
:- begin_pfc.

baseKB:mtProlog(code1).
baseKB:mtHybrid(kb2).
baseKB:mtHybrid(kb3).

arity(a0,0).
baseKB:predicateConventionMt(a0,kb2).

%:- set_defaultAssertMt(myMt).

a0.


% code1: (a0 <- b).
code1: (b:- printAll('$current_source_module'(_M))).


kb2: (b).

baseKB:genlMt(kb2,code1).


kb2: (?- a0).

baseKB:genlMt(kb3,kb2).

baseKB:genlMt(mt_06,kb2).

:- kb2:listing(a0/0).

kb3: (a0==>c).

:- mpred_must(clause(kb2:a0,_)).
