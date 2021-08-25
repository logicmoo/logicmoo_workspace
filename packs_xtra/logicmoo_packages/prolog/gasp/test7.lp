%%% schur numbers without ad.hoc and cardinality constraints

% Domain predicates
number(1..10).
part(1..3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AGO: unfolded here the above cardinality constraint
%%% for cases  "part=2", "part=3", e "part=4"
%%% Uncomment the proper case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2
%inpart(X,1) :- not inpart(X,2),  number(X).
%inpart(X,2) :- not inpart(X,1),  number(X).
%%% 3
inpart(X,1) :- not inpart(X,2), not inpart(X,3), number(X).
inpart(X,2) :- not inpart(X,1), not inpart(X,3), number(X).
inpart(X,3) :- not inpart(X,1), not inpart(X,2), number(X).
%%% 4
%inpart(X,1) :- not inpart(X,2), not inpart(X,3), not inpart(X,4), number(X).
%inpart(X,2) :- not inpart(X,1), not inpart(X,3), not inpart(X,4), number(X).
%inpart(X,3) :- not inpart(X,1), not inpart(X,2), not inpart(X,4), number(X).
%inpart(X,4) :- not inpart(X,1), not inpart(X,2), not inpart(X,3), number(X).

% X, Y, and X+Y cannot be in the same part

:- number(X), number(Y), part(P), inpart(X,P), inpart(Y,P), inpart(Z,P),
   X < Y+1, Z = X+Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
