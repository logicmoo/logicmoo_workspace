c(jones).
c(paul).
c(jacob).

head(p(X), person(X)) :- c(X).

head(jm, moustache).
body(jm, reliable(jones)).

head(pj, neg(reliable(jones))).
body(pj, reliable(paul)).

head(jj, reliable(jones)).
body(jj, reliable(jacob)).

def(pr(X)) :- c(X).
head(pr(X), reliable(X)) :- c(X).
body(pr(X), person(X)) :- c(X).

%def(pnr(X)) :- c(X).
%head(pnr(X), neg(reliable(X))) :- c(X).
%body(pnr(X), person(X)) :- c(X).
