/*
* Copyright (C) 2002, 2003 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, write to the Free Software Foundation, Inc., 59 Temple
* Place, Suite 330, Boston, MA 02111-1307 USA
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TERM_SUPPORT.PL  Version: 1.0 Patchlevel: %I% Date: %G%
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(term_support, 
          [ ordered_term_variables/2,
	    absmember/2,
	    subsumeschk/2,
            remove_subsumed_elements/2]).

%%%% 
%%%% ordered_term_variables(+Term,-Varlist)
%%%%
%%%% List of variables in Term, no duplicates. Ordering depends only on
%%%% term structure. Ordering corresponds to left to right when term would
%%%% be printed.
%%%%
ordered_term_variables(Term,Vars) :-
	otv1(Term,[],Vars).

otv1(Term,Vars,[Term|Vars]) :-
	var(Term), \+ absmember(Term,Vars), !.
otv1(Term,Vars1,Vars2) :-
	compound(Term),
	!,
	functor(Term,_,Arity),
	otv2(Arity,Term,Vars1,Vars2).
otv1(_,Vars,Vars).	

otv2(1,Term,Vars1,Vars2) :- !,
	arg(1,Term,Argument),
	otv1(Argument,Vars1,Vars2).
otv2(N,Term,Vars1,Vars2) :-
	arg(N,Term,Argument),
	otv1(Argument,Vars1,Vars3),
	M is N-1,
	otv2(M,Term,Vars3,Vars2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absmember(X, [Y|_]) :- 
	X == Y, 
	!.
absmember(X, [_|L]) :- 
	absmember(X, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% Seems it is needed to copy both X and Y to permit variables
%% shared between them in the input
%%
subsumeschk(X, Y) :-
  \+ \+  ( copy_term(X, X1),
	   copy_term(Y, Y1),
           unify_with_occurs_check(X1, Y1),
	   Y =@= Y1 ).

%% %%%% - the version in library(quintus) is not copy stable:
%% 
%% ?- subsumes_chk(p(Y,Y),p(a,Y)).
%% 
%% Yes
%% ?- subsumes_chk(p(Y,Y),p(a,W)).
%% No

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Ys contains those elements of Xs which are not subsumed by
%%%% another element of Xs, in the same ordering as in Xs.
%%%% Elements may share variables.
%%%%
remove_subsumed_elements(Xs, Ys) :-
	rse(Xs, [], Ys).

rse([X|Xs], Ys, Zs) :-
	rse1(Ys, X, Zs1),
	rse(Xs, Zs1, Zs).
rse([], Ys, Ys).

rse1([Y|Ys], X, [Y|Ys]) :-
	subsumeschk(Y, X),
	!.
rse1([Y|Ys], X, Ys1) :-
	subsumeschk(X, Y),
	!,
	rse1(Ys, X, Ys1).
rse1([Y|Ys], X, [Y|Ys1]) :-
	rse1(Ys, X, Ys1).
rse1([], X, [X]).

