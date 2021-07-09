%%% An example that demonstrates the inadequacy of verifier.pl,
%%% by Brian W. DeVries.  Works with v.pl.

:- ['v.pl'].

proposition(p).

state(s0).

holds(s0, p).

trans(s0, s0).


q :- check(s0, ~ (x g p)).
q2 :- check(s0, ~ (g p)).
