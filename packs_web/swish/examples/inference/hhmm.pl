%%%%%%%%%% Hybrid Hidden Markov Model
:- use_module(library(mcintyre)).

:- mc.

:- begin_lpad.

init_s(S):gaussian(S,0,1).
trans_err_a(_,E):gaussian(E,0,2).
trans_err_b(_,E):gaussian(E,0,4).
obs_err_a(_,E):gaussian(E,0,1).
obs_err_b(_,E):gaussian(E,0,3).

type(0,a):0.4;type(0,b):0.6.
type(I,a):0.3;type(I,b):0.7:- I>0, I1 is I-1, type(I1,a).
type(I,a):0.7;type(I,b):0.3:- I>0, I1 is I-1, type(I1,b).

ok :- 
    kf(2,[_,A],_),
    A > 2.

kf(N,O,LS) :-
  init_s(S),
  kf_part(0, N, S,O,LS).

kf_part(I, N, S, [V|RO], [S|LS]) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS, RO, LS).

kf_part(N, N, _S, [], []).

trans(S,I,NextS) :-
  type(I,a),  
  trans_err_a(I,E),
  {NextS = E + S}.

trans(S,I,NextS) :-
  type(I,b),  
  trans_err_b(I,E),
  {NextS = E + S}.

emit(S,I,V) :-
  type(I,a),  
  obs_err_a(I,X),
  {V = S +X}.

emit(S,I,V) :-
  type(I,b),
  obs_err_b(I,X),
  {V = S +X}.

:- end_lpad.

/** <examples> Your example queries go here, e.g.

?- mc_sample(ok,10000,Prob).

*/

