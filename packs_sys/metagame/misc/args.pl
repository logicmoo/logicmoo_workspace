%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% args.pl
%%% Some more general routines for term-manipulation

% SAME_FUNCTOR(?T1,?T2)
% Succeeds when T1 and T2 share the same functor.
% Fails unless one of T1,T2 is non-var.
same_functor(T1,T2) :-
	( nonvar(T1) 
	-> functor(T1,F,A),
	   functor(T2,F,A)
	; ( nonvar(T2) 
	  -> functor(T2,F,A),
	     functor(T1,F,A)
	  ;  format("Error in same_functor: Both terms variables!~n",[]),
	     fail
	  )
	).

% SAME_FUNCTOR(?T1,?T2,?A)
% Succeeds when T1 and T2 share the same functor,
% and both have arity A.
% Fails unless one of T1,T2 is non-var.
% (So not as general as Quintus's library pred.
same_functor(T1,T2,A) :-
	( nonvar(T1) 
	-> functor(T1,F,A),
	   functor(T2,F,A)
	; ( nonvar(T2) 
	  -> functor(T2,F,A),
	     functor(T1,F,A)
	  ;  format("Error in same_functor: Both terms variables!~n",[]),
	     fail
	  )
	).


% SAME_ARG(+N,?T1,?T2)
% Succeeds when T1 and T2, both non-var, have the same arg N.
same_arg(N,T1,T2) :-
	arg(N,T1,Item),
	arg(N,T2,Item).

% SAME_ARG(+N,?T1,?T2,+Item)
% Succeeds when T1 and T2, both non-var, have the same Item as arg N.
same_arg(N,T1,T2,Item) :-
	arg(N,T1,Item),
	arg(N,T2,Item).


% CORRESPONDING_ARG(N,T1,Item1,T2,Item2)
% Item1 and Item2 are the Nth args in T1 and T2,
% respectively.  
% Not as general as in Quintus.  
corresponding_arg(N,T1,Item1,T2,Item2) :-
	arg(N,T1,Item1),
	arg(N,T2,Item2).


% (The following routines were borroowed from Quintus)

%   genarg(?N, +Term, ?Item)
%   like arg(N,Term,Item), but will generate N if necessary.

genarg(N, Term, Arg) :-
	integer(N),
	nonvar(Term),
	!,
	arg(N, Term, Arg).
genarg(N, Term, Arg) :-
	var(N),
	nonvar(Term),
	!,
	functor(Term, _, Arity),
	genarg(Arity, Term, Arg, N).

genarg(1, Term, Arg, 1) :- !,
	arg(1, Term, Arg).
genarg(N, Term, Arg, N) :-
	arg(N, Term, Arg).
genarg(K, Term, Arg, N) :-
	K > 1, J is K-1,
	genarg(J, Term, Arg, N).


%   path_arg(Path, Term, SubTerm)
%   This routine replaces two predicates in the old Dec-10 Prolog
%   library: patharg/3 and position/3.  It does everything they did,
%   and reports errors as well.  

path_arg(Path, Term, SubTerm) :-
	var(Term),
	!,
	(   Path == [] -> SubTerm = Term
	;   \+ (Path = [_|_]) -> fail
	;   format(user_error,
		'~N! Instantiation fault in argument ~w of ~q/~w~n! Goal: ~p~n',
		[2,path_arg,3,path_arg(Path,Term,SubTerm)]),
	    fail
	).
path_arg([], Term, Term).
path_arg([Head|Tail], Term, SubTerm) :-
	(   integer(Head) ->
	    arg(Head, Term, Arg),
	    path_arg(Tail, Arg, SubTerm)
	;   var(Head) ->
	    functor(Term, _, Arity),
	    genarg(Arity, Term, Arg, Head),
	    path_arg(Tail, Arg, SubTerm)
	;   /* otherwise */
	    format(user_error,
		'~N! Type failure in argument ~w of ~q/~w~n! Goal: ~p~n',
		[1,path_arg,3,path_arg([Head|Tail],Term,SubTerm)]),
	    fail
	).

