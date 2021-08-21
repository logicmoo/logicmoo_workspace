% LogicMOO Skel

:- use_module(library(logicmoo_user)).	 
%=  setup pfc
:- expects_dialect(pfc).

%= Constraint Logic Programming
:- use_module(library(dif)).		% Sound inequality
:- use_module(library(clpfd)).		% Finite domain constraints
:- use_module(library(clpb)).		% Boolean constraints
:- use_module(library(chr)).		% Constraint Handling Rules
:- use_module(library(when)).		% Coroutining


%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).


%=  Trace execution
:- mpred_trace_exec.

% Your program goes here

a ==> b.

a.

/** <examples> Your example queries go here, e.g.
?- b.
?- X #> 1.
*/
