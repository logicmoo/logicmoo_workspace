:- module(fib,[fib/2]).

:- use_module(library(andorra/andorra)).
:- determinate(fib(X,Y),(nonvar(X);(nonvar(Y),Y \== 1))).

fib(0,1).
fib(1,1).
fib(A,B):-
 	A > 1,
 	C is A - 1,
 	D is A - 2,
 	fib(C,E),
 	fib(D,F),
 	B is F + E.
