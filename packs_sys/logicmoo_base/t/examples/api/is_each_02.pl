/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/

% :- module(is_each_01,[]).

:- ensure_loaded(library(pfc)).

:- begin_pfc.

% :- fully_expand( (isEach([system(X),system(Y)]) :- related(X,Y)), _O).

% :- fully_expand( (isEach(aa(X),bb(Y)) :- related(X,Y)), _O).



:- must(( 
  (Code =
     (example_known_is_success(G):- G)),
   fully_expand(clause(assert,assert_u),Code,Compiled),
  (Compiled = 
     (example_known_is_success(G):- call(G))))).

% end_of_file.

:- assert_u((example_known_is_successF(G):- G)).
:- must(clause_asserted_u((example_known_is_successF(G):- call(G)))).

:- must(clause_asserted_u((example_known_is_successF(G):- G))).


%= define the example language
example_known_is_success(G):- G.
:- must(clause_asserted_u((example_known_is_success(G):- G))).
example_impossible_is_success(G):-  ~(G).
example_known_is_failure(G):-  \+ G.
example_impossible_is_failure(G):- \+  ~(G).

%= define the four truth values
example_proven_true(G):- example_known_is_success(G),example_impossible_is_failure(G).
example_proven_false(G):- example_impossible_is_success(G),example_known_is_failure(G).
example_inconsistent(G):- example_known_is_success(G),example_impossible_is_success(G).
example_unknown(G):- example_known_is_failure(G),example_impossible_is_failure(G).

==>(isEach(aa(X),bb(Y)) :- related(X,Y)).

:- listing([aa,bb]).

:-  time((ensure_loaded(baseKB:library(logicmoo/pfc/'system_markers.pfc')))).

/*
?- fully_expand(arity(isEach(X,TY),4),O). 
O =  (arity(X, 4), arity(TY, 4)).
*/  

% before:  436,431,078 inferences, 496.191 CPU in 496.916 seconds (100% CPU, 879562 Lips)
% before:  261.117

% :- mpred_trace_exec.

