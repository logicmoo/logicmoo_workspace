% Prolog Forward Chaining

%=  setup pfc
:- expects_dialect(pfc).

% Your program goes here

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).


%=  Trace execution
:- mpred_trace_exec.

%= Your program goes here

a ==> b.

a.

/** <examples> Your example queries go here, e.g.
?- b.
?- X #> 1.
*/

