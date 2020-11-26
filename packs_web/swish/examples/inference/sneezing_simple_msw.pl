
:- use_module(library(pita)).

:- pita.

:- begin_lpad.

sneezing(X):- flu(X), msw(flu_sneezing(X),1).
sneezing(X):- hay_fever(X),msw(hay_fever_sneezing(X),1).
flu(bob).
 hay_fever(bob).

values(flu_sneezing(_X),[1,0]).
values(hay_fever_sneezing(_X),[1,0]).
:- set_sw(flu_sneezing(_X),[0.7,0.3]).
:- set_sw(hay_fever_sneezing(_X),[0.8,0.2]).
:- end_lpad.

/** <examples> 

?- prob(sneezing(bob),Prob).

*/
