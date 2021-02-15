
:- expects_dialect(lps).

% This ilustrates a bug: 
% the askMoney action is not firing because the timeless predicate parent's second solution is not being considered
maxTime(5).

events party/1.
actions askMoney/2.
fluents hasMoney/1.

initially hasMoney(mum).
observe party(bob) to 2.

parent(bob, dad).
parent(bob, mum).

if party(bob) then 
	parent(bob,X), hasMoney(X), askMoney(bob, X).