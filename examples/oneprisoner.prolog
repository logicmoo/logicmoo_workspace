% oneprisoner.prolog
% a Prolog code to compare with oneprisoner.lps


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
