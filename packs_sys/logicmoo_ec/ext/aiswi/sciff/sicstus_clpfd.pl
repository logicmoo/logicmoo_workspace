% Predicates that use syntax specific of the CLP(FD) library of the host Prolog 
% Version for SICStus Prolog
% The main difference is that SICStus uses #<=>, while SWI uses #<==>

reified_equality_solver(X,Y,B):- X #= Y #<=> B.

solver_and(B,B1,B2):- B #<=> (B1 #/\ B2).

reification(T,B):- call(T #<=> B).

opposite(eq(A,B),neq(A,B)).
opposite(neq(A,B),eq(A,B)).
opposite(lt(A,B),geq(A,B)).
opposite(leq(A,B),gt(A,B)).
opposite(gt(A,B),leq(A,B)).
opposite(geq(A,B),lt(A,B)).
opposite(C,C #<=> 0).
