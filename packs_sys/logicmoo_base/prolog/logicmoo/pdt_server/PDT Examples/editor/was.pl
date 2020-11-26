:- module(was,[
	
]).

p(A-B, Xs) :-
	setof(X, Y^p(X,Y), Xs),
	c(A-B, A, B).

foo(Var, 1-Var).

foo(Foo=F
foo(Foo-F
foo(Foo:F

p(A) :-
