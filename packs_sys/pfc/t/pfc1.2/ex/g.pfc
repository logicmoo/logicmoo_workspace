% -*-Prolog-*-

:- dynamic ('-->>')/2.
:- dynamic ('--*>>')/2.

% a simple pfc dcg grammar.  requires dcg_pfc.pl

% backward grammar rules.
s(s(Np,Vp)) -->> np(Np), vp(Vp).

vp(vp(V,Np)) -->> verb(V), np(Np).
vp(vp(V)) -->> verb(V).
vp(vp(VP,X)) -->> vp(VP), pp(X).

np(np(N,D)) -->> det(D), noun(N).
np(np(N)) -->> noun(N).
np(np(Np,pp(Pp))) -->> np(Np), pp(Pp).

pp(pp(P,Np)) -->> prep(P), np(Np).

% forward grammar rules.
P --*>>  [W],{cat(W,Cat),P =.. [Cat,W]}.

% simple facts.
cat(the,det).
cat(a,det).
cat(man,noun).
cat(fish,noun).
cat(eats,verb).
cat(catches,verb).
cat(in,prep).
cat(on,prep).
cat(house,noun).
cat(table,noun).