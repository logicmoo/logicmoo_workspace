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


%:- add_import_module(mt_01,baseKB,end).

:- set_defaultAssertMt(code1).

% mtProlog(code1).
% mtHybrid(code1).

:- begin_pfc.

mtHybrid(kb2).
mtHybrid(kb3).

:- listing(mtProlog/1).
:- listing(mtHybrid/1).


% code1: (a <- b).
code1: (a:- printAll('$current_source_module'(_M))).


kb2: (b).

baseKB:genlMt(kb2,code1).

baseKB:genlMt(code1,baseKB).

kb2: (:- a).

baseKB:genlMt(kb3,kb2).


kb3:predicateConventionMt(c,code1).

kb3: (a==>c).

% to make sure a does not get accdently defined in kb2 or kb3
:- mpred_must((clause(kb3:a,_,Ref), clause_property(Ref,module(kb3)))).
:- mpred_must(( clause(kb2:a,_))).

% c is forward chained back into 'code1' where it becomes asserted
% :- mpred_must(clause(code1:c,_)).


