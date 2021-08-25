%Schur numbers.

% Domain predicates
domain(1..44). 
range(1..4).

% Assign each integer to exactly one part. 
%%% GASP Syntax
%	assignment(X,Y).

%%% LPARSE SYNTAX
 1{ assignment(X,P): range(P) }1 :- domain(X).

% X, Y, and X+Y cannot be in the same part
:- domain(X), domain(Y), domain(Z), range(P), 
   assignment(X,P), assignment(Y,P), assignment(Z,P),
   X < Y+1, Z = X+Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
