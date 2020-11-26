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

:- set_defaultAssertMt(modA).

:- begin_pfc.

:- mpred_trace_exec.

:- set_prolog_flag(retry_undefined, kb_shared).

mtHybrid(modA).
mtHybrid(modB).



modA: (codeA:- 
  (notrace(printAll((
   '$current_source_module'(_),
   '$current_typein_module'(_),
   context_module(_),
   ignore(prolog_load_context(reloading,_)),
   current_prolog_flag(retry_undefined,_),
   current_prolog_flag(debug,_),
   current_prolog_flag(unknown,_)))),
  %set_prolog_flag(unknown,error),
  trace,
  %set_prolog_flag(retry_undefined,kb_shared),
  notrace(call(call,codeB)),
  format('~n~n~n~n~nSuccess~n~n~n',[]))).
modB: (codeB).

genlMt(modA,modB).


:- set_prolog_flag(retry_undefined,kb_shared).

% run the test
%modA: (:- codeA).

:- set_prolog_flag(unknown,error).
:- catch(modA:codeA,E,wdmsg(E)).
:- break. 



% before test, to make sure codeA was not accdently defined in modB
:- sanity(\+ module_clause(modB:codeA,_)).
:- sanity(\+ module_clause(modA:codeB,_)).

:- sanity( module_clause(modA:codeA,_)).
:- sanity( module_clause(modB:codeB,_)).

% before test, genlMt makes the rule available and should not corrupt the modA module

% make sure genlMt didnt unassert 
:- sanity(clause_u(modB:codeB,_)).


:- warn_fail_TODO(clause_u(modA:codeB,_)).


% to make codeB sure  is available in modA
:- mpred_must( clause_u(modA:codeB,_)).

% to make sure codeA does not get accdently defined in modB
:- mpred_must(\+ ((clause_u(modB:codeA,B,Ref),B\=inherit_above(modB, codeA), clause_property(Ref,module(modB))))).

% genlMt makes the rule available and should not corrupt the modA module
:- warn_fail_TODO(clause(modA:codeB,_)).

% genlMt 

:- warn_fail_TODO( clause_u(modA:codeB,_)).

