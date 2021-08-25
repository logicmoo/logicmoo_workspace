/*
 *	file:		arg.pl
 *	version:	1.0
 *	date:		November 28, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains some primitive term manipulating predicates.
 *
 *	history:
 *	891128	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

path_arg([],Term,Term).
path_arg([Index|Indices],Term,SubTerm) :-
	genarg(Index,Term,Arg),
	path_arg(Indices,Arg,SubTerm).

genarg(I1,T,Arg) :-
	nonvar(I1), !,
	arg(I1,T,Arg).
genarg(I1,T,Arg) :-
	var(X),
	\+ atomic(T), 
	functor(T,_,N),
	genargs(I1,T,Arg,N).

genargs(N,T,Arg,N) :-
	arg(N,T,Arg).
genargs(I1,T,Arg,N) :-
	M is N-1,
	M > 0,
	genargs(I1,T,Arg,M).

change_path_arg([],OldTerm,OldTerm,NewSub,NewSub) :- !.
change_path_arg([Index|Indices],OldTerm,OldSub,NewTerm,NewSub) :-
	OldTerm=..[Op|Args],
	n_th(Index,Args,Arg),
%	arg(OldTerm,Index,Arg),
	change_path_arg(Indices,Arg,OldSub,NewArg,NewSub),
	replace_n_th(Index,Args,NewArg,NewArgs),
	NewTerm=..[Op|NewArgs].
	
	
change_path_arg([],OldTerm,NewTerm,NewTerm) :- !.
change_path_arg([Index|Indices],OldTerm,NewTerm,NewSub) :-
	OldTerm=..[Op|Args],
	n_th(Index,Args,Arg),
%	arg(OldTerm,Index,Arg),
	change_path_arg(Indices,Arg,NewArg,NewSub),
	replace_n_th(Index,Args,NewArg,NewArgs),
	NewTerm=..[Op|NewArgs].


n_th(1,[H|T],H) :- !.
n_th(N,[H|T],E) :-
	M is N-1,
	n_th(M,T,E).

replace_n_th(1,[H|T],F,[F|T]) :- !.
replace_n_th(N,[H|T],F,[H|L]) :-
	M is N-1,
	n_th(M,T,F,L).
