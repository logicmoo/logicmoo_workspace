
:- expects_dialect(lps).

maxTime(5).
events party/1.
actions askMoney/2.
fluents money/1.

observe party(bob) to 2.

initially money(mum).

father(bob, dad).
mother(bob, mum).

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

if party(bob) to T, not money(bob) at T then 
	parent(bob,X), money(X), askMoney(bob, X).


