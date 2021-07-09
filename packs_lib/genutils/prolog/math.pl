:- module(math, [mul/3, sub/3, add/3, neg/2, divby/3, recip/2, log/2, exp/2, gammaln/2,
                 max/3, equal/3, stoch/3, to_float/2, prodlist/2, map_sum/3, map_sum/4]).

:- use_module(library(apply_macros)).

max(X,Y,Z)   :- Z is max(X,Y).
divby(X,Y,Z) :- Z is Y/X.
sub(X,Y,Z)   :- Z is Y-X.
add(X,Y,Z)   :- Z is Y+X.
neg(X,Y)     :- Y is -X.
mul(X,Y,Z)   :- Z is X*Y.
equal(X,Y,V) :- X=Y -> V=1; V=0.
recip(X,Y)   :- Y is 1/X.
gammaln(X,Y) :- Y is lgamma(X).
log(X,Y)     :- Y is log(X).
exp(X,Y)     :- Y is exp(X).

prodlist(L,X) :- prodlist(L,1,X).
prodlist([],X,X) :- !.
prodlist([A|AX],X,Z) :- Y is A*X, prodlist(AX,Y,Z).


%% stoch( +X:list(nonneg), -Y:list(nonneg), -Total:nonneg) is semidet.
%% stoch( +X:list(nonneg), -Y:list(nonneg)) is semidet.
%
%  Compute normalised probability distribution from histogram counts, with total.
%  Fails if total is less than or equal to zero.
stoch(H,P,N) :- sumlist(H,N), N>0, maplist(divby(N),H,P).

%% to_float( +Expr, -FloatVal:float) is det.
%  Return the floating point value of Expr as evaluated
%  by is/2.
to_float(X,Y) :- Y is float(X).

%% map_sum(+P:pred(+number,+number,-number), +L1:list(number), +L2:list(number), -S:number) is det.
%% map_sum(+P:pred(+number,-number), +L:list(number), -S:number) is det.
%
%  Map P (arity N) over (N-1) supplied lists sum the resulting (Nth) list.
%  Provided as predicates and goal expansions.
:- meta_predicate map_sum(2,+,-), map_sum(3,+,+,-).
map_sum(P,X,Y,Sum) :- maplist(P,X,Y,Z), sum_list(Z,Sum).
map_sum(P,X,Sum)   :- maplist(P,X,Y),   sum_list(Y,Sum).

user:goal_expansion(map_sum(P,X,Sum),   (maplist(P,X,Y),   sum_list(Y,Sum))).
user:goal_expansion(map_sum(P,X,Y,Sum), (maplist(P,X,Y,Z), sum_list(Z,Sum))).
