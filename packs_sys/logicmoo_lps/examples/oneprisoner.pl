
:- expects_dialect(lps).

% oneprisoner.lps
% solving the prisoner dilema from the stand point of one agent
% coder: Jacinto Dávila
% 2007
% this code is free software with the Affero GPL 3.0

maxTime(10).

fluents arrested.

actions select(_).

events do_my_best, selects_with_prob(_,_), capturing, releasing.

if arrested at T then do_my_best from T to T2.

do_my_best if
   choice(A),
   select(A).
   %
   %selects_with_prob(B1, P),
   %selects_with_prob(B2, Q),
   %not(better_than(A, B1, B2, P, Q)).

% There is some action better than A1 in the context of B1, B2, P and Q.
better_than(A1, B1, B2, P, Q) :-
   diff(B1,B2),
   utility(A1,B1,U0),
   utility(A1,B2,U1),
   utility(A2,B1,U3),
   utility(A2,B2,U4),
   diff(A1,A2),
   less(U0*P+U1*Q, U3*P+U4*Q).

less(E1, E2) :- V0 is E1, V1 is E2, V0 < V1.

diff(A,B) :- not(A=B).

choice(cooperate).
choice(defect).

utility(defect, cooperates, 0).
utility(defect, defects, -3).
utility(cooperate, cooperates, -1).
utility(cooperate, defects, -6).

false
  select(A),
  selects_with_prob(B1, P),
  selects_with_prob(B2, Q),
  better_than(A, B1, B2, P, Q).

capturing initiates arrested if not arrested.
releasing terminates arrested if arrested.

observe capturing from 1 to 2.
observe selects_with_prob(cooperates, 0.5) from T to T2.
observe selects_with_prob(defects, 0.5) from T to T2.

/*
% oneprisoner.prolog
% a Prolog code to compare with the above LPS


% There is some action better than A1 
better_than(A1) :-
   utility(A1,B1,U0), selects_with_prob(B1, P),
   utility(A1,B2,U1), selects_with_prob(B2, Q),
   utility(A2,B1,U3), utility(A2,B2, U4),
   diff(A1,A2), diff(B1,B2),
   less(U0*P+U1*Q, U3*P+U4*Q).

less(E1, E2) :- V0 is E1, V1 is E2, V0 < V1.

diff(A,B) :- not(A=B).

utility(cooperate, cooperates, -1).
utility(cooperate, defects, -6).
utility(defect, cooperates, 0).
utility(defect, defects, -3).

selects_with_prob(cooperates, 0.5).
selects_with_prob(defects, 0.5).
*/