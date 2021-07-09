:- module(argutils, 
          [	sumargs/2
          ,	addargs/3
          ]).

sumargs(Term,Total) :- 
   functor(Term,_,N), sumargs(N,Term,0,Total).

sumargs(0,_,S,S) :- !.
sumargs(N,Term,S1,S3) :- 
	succ(M,N), arg(N,Term,X), S2 is S1+X, 
	sumargs(M,Term,S2,S3).


%% addargs( +Term, +Args, -NewTerm) is det.
%
% Add an extra argument to a term, eg 
% ==
% ?- addargs(spam(fish,cake),spoon,T).
%   T = spam(fish,cake,spoon).
%   true.
% ==
addargs(Term,Args,NewTerm) :-
	var(NewTerm) ->
		(Term=..L,
		append(L,Args,LL),
		NewTerm=..LL)
	;
		(NewTerm=..LL,
		append(L,Args,LL),
		Term=..L).

	%	functor(S1,F,A), arg(N,S1,X1), 
	%	functor(S2,F,A), arg(N,S2,X2),
	%	phrase(P,X1,X2),
	%	foreach((arg(I,S1,X),I\=N), arg(I,S2,X)).

% reinstatevars(Bindings,'$VAR'(Name),Var) :- member(Name=Var,Bindings), !. 
% reinstatevars(_,Atomic,Atomic) :- atomic(Atomic), !.
% reinstatevars(Bindings,Term1,Term2) :-
%    Term1 =.. [F | Args1],
%    maplist(reinstatevars(Bindings),Args1,Args2),
%    Term2 =.. [F | Args2].
