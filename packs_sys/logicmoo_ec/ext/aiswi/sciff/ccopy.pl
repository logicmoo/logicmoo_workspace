%-------------------------------------------------------------
% Implementation of ccopy
% Marco Gavanelli
% Ver 3
% Considers also the quantifier restrictions
%-------------------------------------------------------------

:-module(ccopy,[ccopy/2]).

:- use_module(quantif).

% ccopy(+Term,-CopiedTerm)

/*

Marco Alberti:

This version replaces all multiple occurrences of the same "old"
variable with the same "new" variable.

*/

get_substituted_variable([(X1,Y1)|_],X2,Y2):-
	X1==X2,
	!,
	Y1=Y2.
get_substituted_variable([_|T],X,Y):-
	get_substituted_variable(T,X,Y).


ccopy(X,Y):-
	ccopy(X,Y,[],Substitutions),
	copy_restrictions(Substitutions).

ccopy(X,Y,Substitutions,Substitutions) :-
	var(X), get_quant(X,existsf),
	!, X=Y.
ccopy(X,Y,_,_):- 
	var(X), nonvar(Y), !,
	writeln("Error: ccopy invoked with 2nd argument non var"), 
	halt.

ccopy(X,Y,Substitutions,Substitutions):-
	var(X),
	get_substituted_variable(Substitutions,X,Y),
	!.
ccopy(X,Y,OldSubstitutions,[(X,Y)|OldSubstitutions]) :-
	var(X),!,
	((get_quant(X,QX),nonvar(QX)) 
	  -> 	%set_term_quantification(Y,QX)
            set_quant(Y,QX) 
	  ; 	true).
% Uncomment the following clause if you want to save memory (at the cost of speed)
% ccopy(X,Y,Old,New):- ground(X),!,X=Y,Old=New.
ccopy(X,Y,OldSubstitutions,NewSubstitutions) :-
	functor(X,F,N),
	functor(Y,F,N),
	ccopy_arg(N,X,Y,OldSubstitutions,NewSubstitutions).

ccopy_arg(0,_,_,Substitutions,Substitutions):-!. % SICStus non ottimizza questo se tolgo il cut e metto N>0
ccopy_arg(N,X,Y,OldSubstitutions,NewSubstitutions):-
	%N>0,
	arg(N,X,Xn),
	arg(N,Y,Yn),
	ccopy(Xn,Yn,OldSubstitutions,IntSubstitutions),
	N1 is N-1,
	ccopy_arg(N1,X,Y,IntSubstitutions,NewSubstitutions).

copy_restrictions(R):- copy_restrictions(R,R).
copy_restrictions([],_).
copy_restrictions([(X,Y)|T],Subs):-
	get_restrictions(X,Rx),
	%(X,Y) is already a member of Subs; however, we are
	%renaming the restrictions on variable X, so it is
	%handy to have the couple (X,Y) as first element
	%of the list.
	substitute_vars(Rx,[(X,Y)|Subs],Ry),
	%set_restriction_list(Ry), MG 27 jul 2007: This gives a strong speedup in block world, where many restrictions are imposed
	set_restriction_list(Ry,Y),
	copy_restrictions(T,Subs).

substitute_vars(X,Subs,Z):-
	var(X),!,
	(get_substituted_variable(Subs,X,Y)
	  ->	Z=Y
	  ;	Z=X).
%substitute_vars(X,_Subs,X):- Without this clause is slightly faster
%	ground(X),!.
substitute_vars(X,Subs,Y):-
	functor(X,F,N),
	functor(Y,F,N),
	substitute_vars_arg(N,X,Y,Subs).

substitute_vars_arg(0,_,_,_):- !.
substitute_vars_arg(N,X,Y,Subs):-
	arg(N,X,Xn),
	arg(N,Y,Yn),
	substitute_vars(Xn,Subs,Yn),
	N1 is N-1,
	substitute_vars_arg(N1,X,Y,Subs).
