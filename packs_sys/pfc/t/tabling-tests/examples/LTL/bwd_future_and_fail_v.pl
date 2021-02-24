%%% An example that demonstrates the inadequacy of verifier.pl,
%%% by Brian W. DeVries.  Works with v.pl.

:- ['v.pl'].

proposition(p).
proposition(q).

state(s0).
state(s1).


holds(s1, p).

trans(s0, s1).
trans(s1, s0).


q :- check(s0, ~(f p ^ q)).
