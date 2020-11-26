resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.

%defeat(+Proposition,-Asumptions)

defeat(P,A) :- defeat1(P,A,[]).

%defeat((P,Q),A,L) :- 
%  defeat
%
defeat1(~P,P,_).

%defeat1(P,A,L) :-
%  \+memberchk(P,L),
%    bagof(A1,S^(directSupport(P,S),defeat