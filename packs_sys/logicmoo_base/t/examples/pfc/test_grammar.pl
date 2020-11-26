:- dynamic ('-->>'/2).

s(Np,Vp) -->> np(Np), vp(Vp).
vp(V,Np) -->> verb(V), np(Np).
vp(V,nil) -->> verb(V).
np(N,D) -->> det(D), noun(N).
np(N,nil) -->> noun(N).

det(W) -->> [W],{cat(W,det)}.
noun(W) -->> [W],{cat(W,noun)}.
verb(W) -->> [W],{cat(W,verb)}.


cat(the,det).
cat(a,det).
cat(man,noun).
cat(fish,noun).
cat(eats,verb).
cat(catches,verb).
