/*
This program models the effect of flu and hay fever on the sneezing symptom.
From
F. Riguzzi and T. Swift. The PITA system: Tabling and answer subsumption for reasoning under uncertainty. Theory and Practice of Logic Programming, 27th International Conference on Logic Programming (ICLP'11) Special Issue, 11(4-5):433-449, 2011.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

sneezing(X) : 0.7  :- flu(X).
% if X has the flu, there is a probability of 0.3 
% that he sneezes 

sneezing(X) : 0.8 :- hay_fever(X).
% if X has hay fever, there is a probability of 0.2 
% that he sneezes
% and a probability of 0.6 that she has moderate sneezing

flu(bob).
% bob has certainly the flu

hay_fever(bob).
% bob has certainly hay fever

:- end_lpad.

/** <examples>

?- prob(sneezing(bob),Prob).
?- bdd_dot_string(sneezing(bob),BDD,Var).

*/

