%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 14/07/96   File: base.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 25/03/96 Created                                                          %%
%% 14/07/96 removed configuration utilities (-> defaults.pl)
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog base.pl                                                   %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------------------
%%% 
%%%       additional predicates
%%%
%%%

apply_to_list([],_,[]).
apply_to_list([Elem|List],P,[Elem1|List1]) :-
	T =.. [P,Elem,Elem1],
	call(T),
	apply_to_list(List,P,List1).


apply_to_list_flat([],_,[]).
apply_to_list_flat([Elem|List],P,ResList) :-
	T =.. [P,Elem,Result1],
	call(T),
	apply_to_list(List,P,List1),
	append(Result1,List1,ResList).


%%% mymember(?Element, ?Set)
%%% is true when Set is a list, and Element occurs in it.  It may be used
%%% to test for an element or to enumerate all the elements by backtracking.
%%% Indeed, it may be used to generate the Set!

mymember(X, [X|_]    ).
mymember(X, [_,X|_]  ).
mymember(X, [_,_,X|_]).
mymember(X, [_,_,_|L]) :-
	mymember(X, L).

%%% myselect(?Element, ?Set, ?Residue)
%%% is true when Set is a list, Element occurs in Set, and Residue is
%%% everything in Set except Element (things stay in the same order).

myselect(X, [X|R],     R        ).
myselect(X, [A,X|R],   [A|R]    ).
myselect(X, [A,B,X|R], [A,B|R]  ).
myselect(X, [A,B,C|L], [A,B,C|R]) :-
	myselect(X, L, R).

%%% mysubset(+Set1, +Set2)
%%% is true when each member of Set1 occurs in Set2.
%%%

mysubset([],_).
mysubset([X|R],L) :-
	mymember(X,L),
	mysubset(R,L).


concatenate(String1,String2,String) :-
	name(String1,L1),
	name(String2,L2),
	append(L1,L2,L),
	name(String,L),
	!.

