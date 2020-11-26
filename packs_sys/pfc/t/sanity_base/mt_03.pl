/* <module>
%
%  PFC is codeA language extension for prolog.
%
%  It adds codeA new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(header_sane,[]).

:- include(test_header).


%:- add_import_module(header_sane,baseKB,end).

:- set_defaultAssertMt(myMt).

:- begin_pfc.

:- mpred_trace_exec.

mtProlog(modA).
mtProlog(modB).



modA: (codeA:- printAll('$current_source_module'(_M)),codeB).

modB: (codeB).

%:- \+ modA:codeA.

genlMt(modA,modB).

% before test, to make sure codeA was not accdently defined in modB
:- sanity(\+ module_clause(modB:codeA,_)).
:- sanity(\+ module_clause(modA:codeB,_)).

:- sanity( module_clause(modA:codeA,_)).
:- sanity( module_clause(modB:codeB,_)).

% before test, genlMt makes the rule available and should not corrupt the modA module
:- warn_fail_TODO(clause_u(modA:codeB,_)).

% make sure genlMt didnt unassert 
:- sanity(clause_u(modB:codeB,_)).



% run the test
modA: (:- codeA).


% to make codeB sure  is available in modA
:- mpred_must( clause_u(modA:codeB,_)).

% to make sure codeA does not get accdently defined in modB
:- mpred_must(\+ ((clause_u(modB:codeA,B,Ref),B\=inherit_above(modB, codeA), clause_property(Ref,module(modB))))).

% genlMt makes the rule available and should not corrupt the modA module
:- warn_fail_TODO(clause(modA:codeB,_)).

% genlMt 

:- warn_fail_TODO( clause_u(modA:codeB,_)).

