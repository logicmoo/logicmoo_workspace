:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
coupons(N,T):-
  length(CP,N),
  CPTerm=..[cp|CP],
  new_coupon(N,CPTerm,0,N,T).

new_coupon(0,_CP,T,_N,T).
new_coupon(N0,CP,T0,N,T):-
  N0>0,
  collect(CP,N,T0,T1),
  N1 is N0-1,
  new_coupon(N1,CP,T1,N,T).
collect(CP,N,T0,T):-
  pick_a_box(T0,N,I),
  T1 is T0+1,
  arg(I,CP,CPI),
  (var(CPI)->
    CPI=1,
    T=T1
  ;
    collect(CP,N,T1,T)
  ).

pick_a_box(_,N,I):uniform(I,L):-numlist(1, N, L).

:-end_lpad.
stickers(N,P,T):-
  coupons(N,T0),
  T is ceiling(T0/P).

dist(N,Samples,Chart):-
  mc_sample_arg_first(coupons(N,T),Samples,T,L),
  density(L,Chart,[nbins(10)]).

boxes_vs_N(MaxN,Step,Chart):-
  NSteps is ceil((MaxN-1)/Step),
  findall(E,(
    between(0,NSteps,N0),N is 1+N0*Step,mc_expectation(coupons(N,T),10,T,E)
  ),V),
  findall(N,(between(0,NSteps,N0),N is 1+N0*Step),X),
  findall(NlogN,(between(0,NSteps,N0),N is 1+N0*Step,NlogN is 1+1.2*N*log(N)),NLN),
  Chart=c3{data:_{x:x, columns:[[x|X],['Expected number of boxes'|V],['1+1.2NlogN'|NLN]]}}.

