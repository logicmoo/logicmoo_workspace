:- module(print_clp_constraints, [
		constraints_closure/2
	]).


%------------------------------------------------------------
% Retrieving a set of CLP(FD) constraints
% Marco Gavanelli
% 1 October 2003
%-------------------------------------------------------------


:- use_module(library(clpfd)).

% constraints_closure (+Vars,-C)
% Returns in C the closure of the list of constraints on 
% the variables in the list Vars.
% C is a goal (i.e., a conjunction of constraints)
% E.g.: 
% | ?- A in 1..10, B #> A, constraints_closure([A],C).
% C = A in_set[[1|10]],B in_set[[2|sup]],clpfd:'t>=u+c'(B,A,1),
% A in 1..10,
% B in 2..sup ? 
% yes

% Notice that returns only the constraints connected to the
% variables in Vars
% E.g.
% | ?- A in 1..10, X#>Y, constraints_closure([A],C).
% C = A in_set[[1|10]],
% A in 1..10,
% Y in inf..sup,
% X in inf..sup ? 



constraints_closure(Var,C):-
	fd_closure(Var,L),
	fd_copy_term(L,LL,C),
	L = LL.


