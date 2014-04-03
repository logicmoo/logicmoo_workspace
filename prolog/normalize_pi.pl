:- module(normalize_pi, [normalize_pi/2]).

normalize_pi(H,   H) :- var(H), !.
normalize_pi('<initialization>', _:'<initialization>'/_).
normalize_pi(M:P, M:P) :- var(P), !.
normalize_pi(M:F/A, M:F/A) :- !.
normalize_pi(M:H,   M:F/A) :- functor(H, F, A).
