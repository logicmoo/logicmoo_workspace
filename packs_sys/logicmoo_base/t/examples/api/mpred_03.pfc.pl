% #!/usr/bin/env swipl

:- module(myMicrotheory,[]).

:- ensure_loaded(library(pfc)).

:- begin_pfc.

:- mpred_trace_exec.


:- must(( 
  (Code =
     (example_known_is_success(G):- G)),
   fully_expand(change(assert,assert_u),Code,Compiled),
  (Compiled = 
     (example_known_is_success(G):- call(G))))).

:- assert_u((example_known_is_success(G):- G)).
:- must(clause_asserted_u((example_known_is_success(G):- call(G)))).

:- must(clause_asserted_u((example_known_is_success(G):- G))).


%= define the example language
example_known_is_success(G):- G.
example_impossible_is_success(G):-  ~(G).
example_known_is_failure(G):-  \+ G.
example_impossible_is_failure(G):- \+  ~(G).

%= define the four truth values
example_proven_true(G):- example_known_is_success(G),example_impossible_is_failure(G).
example_proven_false(G):- example_impossible_is_success(G),example_known_is_failure(G).
example_inconsistent(G):- example_known_is_success(G),example_impossible_is_success(G).
example_unknown(G):- example_known_is_failure(G),example_impossible_is_failure(G).



