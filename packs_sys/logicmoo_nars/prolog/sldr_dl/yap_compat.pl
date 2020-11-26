%:- statistics( walltime, [X,Y]), XY is X+Y, srandom(X),!.
random(X):- X is random.
:- use_module(library(random)).
:- use_module(library(lists)).

/*flatten([], []).
flatten([H|T], M) :- flatten(H, Hf),
   flatten(T, Tf),
   append(Hf, Tf, M).*/

round(X,Y):- Y = round(X).

