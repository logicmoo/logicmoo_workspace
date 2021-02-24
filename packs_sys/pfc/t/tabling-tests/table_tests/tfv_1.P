:- export p_m/1.

:- table p_m/1, q_m/1, r_m/1.

p_m(X):- q_m(X).
q_m(X):- r_m(X).

r_m(1). 
