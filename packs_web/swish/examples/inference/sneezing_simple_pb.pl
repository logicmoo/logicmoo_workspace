:- use_module(library(pita)).

:- pita.

:- begin_lpad.

sneezing(X):- flu(X),flu_sneezing(X).
sneezing(X):- hay_fever(X),hay_fever_sneezing(X).
flu(bob).
hay_fever(bob).
0.7::flu_sneezing(_).
0.8::hay_fever_sneezing(_).
:- end_lpad.

/** <examples>
?- prob(sneezing(bob),Prob).

*/
