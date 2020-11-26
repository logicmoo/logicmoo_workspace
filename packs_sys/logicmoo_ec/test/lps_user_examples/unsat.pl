:- expects_dialect(lps).

% Code given by Jan Burse alias "Mostowski Collapse" here:
% https://stackoverflow.com/questions/63505466/prolog-implementation-of-quines-algorithm-for-classical-propositional-logic-in
% It was very easy to complete the code to get a full translation of Quine's algorithm.
% Operators first 
:- op(700, xfy, <=>).
:- op(600, xfy, =>).
:- op(500, xfy, v).
:- op(400, xfy, &).
:- op(300, fy, ~).
% Code to get the *recursive* evaluation of formulas. 
eval(A, A) :- var(A), !.
eval(A <=> B, R) :- !, eval(A, X), eval(B, Y), simp(X <=> Y, R).
eval(A => B, R) :- !, eval(A, X), eval(B, Y), simp(X => Y, R).
eval(A & B, R) :- !, eval(A, X), eval(B, Y), simp(X & Y, R).
eval(A v B, R) :- !, eval(A, X), eval(B, Y), simp(X v Y, R).
eval(~ A, R) :- !, eval(A, X), simp(~X, R).
eval(A, A).
% Quine reduction rules
simp(A, A) :- var(A), !.
simp(A <=> B, B) :- A == 1, !.
simp(A <=> B, A) :- B == 1, !.
simp(A <=> B, ~ B) :- A == 0, !.
simp(A <=> B, ~ A) :- B == 0, !.
simp(A => _, 1) :- A == 0, !.
simp(A => B, B) :- A == 1, !.
simp(_ => B, 1) :- B == 1, !.
simp(A => B, ~ A) :- B == 0, !.
simp(A & _, 0) :- A == 0, !.
simp(_ & B, 0) :- B == 0, !.
simp(A & B, B) :- A == 1, !.
simp(A & B, A) :- B == 1, !.
simp(A v B, B) :- A == 0, !.
simp(A v B, A) :- B == 0, !.
simp(A v _, 1) :- A == 1, !.
simp(_ v B, 1) :- B == 1, !.
simp(~ A, 1) :- A == 0, !.
simp(~ A, 0) :- A == 1, !.
simp(A, A).
% the prover engine: if A is a tautology, then unsat(A) is false (CLP(B) unsat(F) :- sat(~ F),
% otherwise the values that satisfy unsat(F) (i.e. ~ F) are given).
unsat(A) :- eval(A, B), term_variables(B, L), unsat(B, L).
unsat(0, _) :- !.
unsat(A, [0|_]) :- unsat(A), !.
unsat(A, [1|_]) :- unsat(A).



/*
Examples: 

?- unsat(~ ~ A <=> A).
false.

?- unsat(A => B).
A = 1,
B = 0.

*/
