/*
* Copyright (C) 2002, 2003, 2016 Christoph Wernhard
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
%%%% TERM_HANDLING.PL  Version: 1.0 Patchlevel: %I% Date: %G%
%%%%
%%%% Operations on terms.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(term_handling, 
          [ 
	   oc_member/2,
	    	    
	   subsumes_chk/2,
	   subsumes_chk_noshare/2,
	   variant_chk/2,
	   variant_chk_noshare/2,

	   std_term_variables/2,
	   ordered_term_variables/2,

	   % contains_var/2 - in library(occurs)
	   each_var_in_term/2,

	   is_ground/1,
	   contains_n_vars/2,
	   term_atom/2,
	   absmember/2 ]).

%% 
%% subsumes_chk/2 note: 
%% 
%% In eclipse instance/2 (and related predicates) may not have shared
%% vars in both arguments. So a "safe" subsumes_chk has to copy one
%% argument.
%%
%% compare_instances seems slightly faster then instance/2
%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% These have to be modified for use of attributed variables.
%%%% 
%% 
%% ignore values of attributed variables:
%% compare_instances,
%% copy_term,
%% occurs
%% 
%% instance and variant obey attributed variables.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oc_member_for_constraints(X, Y) :-
	oc_member(X, Y).

subsumes_chk(X, Y) :-
	subsumes_term(X, Y).
%   \+ \+ ( copy_term(Y, Y1),
%           unify_with_occurs_check(X, Y1),
% 	  Y =@= Y1 ).

subsumes_chk_noshare(X, Y) :-
	subsumes_term(X, Y).
%   \+ \+ ( copy_term(Y, Y1),
%           unify_with_occurs_check(X, Y1),
% 	  Y =@= Y1 ).

std_term_variables(Term, Vars) :-
	term_variables(Term, Vars).

%%%% 
%%%% ordered_term_variables(+Term,-Varlist)
%%%%
%%%% List of variables in Term, no duplicates. Ordering depends only on
%%%% term structure. Ordering corresponds to left to right when term would
%%%% be printed.
%%%%
ordered_term_variables(Term, Vars) :-
	term_variables(Term, Vars).
	
%	otv1(Term,[],Vars).
%
% otv1(Term,Vars,[Term|Vars]) :-
% 	var(Term), \+ absmember(Term,Vars), !.
% otv1(Term,Vars1,Vars2) :-
% 	compound(Term),
% 	!,
% 	functor(Term,_,Arity),
% 	otv2(Arity,Term,Vars1,Vars2).
% otv1(_,Vars,Vars).	
% 
% otv2(1,Term,Vars1,Vars2) :- !,
% 	arg(1,Term,Argument),
% 	otv1(Argument,Vars1,Vars2).
% otv2(N,Term,Vars1,Vars2) :-
% 	arg(N,Term,Argument),
% 	otv1(Argument,Vars1,Vars3),
% 	M is N-1,
% 	otv2(M,Term,Vars3,Vars2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variant_chk(Term1, Term2) :-
	copy_term(Term1, Term1C),
	Term1C =@= Term2.

variant_chk_noshare(Term1, Term2) :-
	Term1 =@= Term2.

each_var_in_term(Vars, Term) :-
	term_variables(Term, Vars1),
	abs_subset(Vars, Vars1).

abs_subset([], _).
abs_subset([X|Xs], Ys) :-
	absmember(X, Ys),
	abs_subset(Xs, Ys).

is_ground(Term) :- ground(Term).

contains_n_vars(N, Term) :-  %% see Eclipse's nonground/3
	term_variables(Term, Vars),
	length(Vars, Len),
	Len >= N.

term_atom(Term, Atom) :-
	term_to_atom(Term, Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%%  Unify
%%%% 

oc_member(X, [Y|_]) :-
	unify_with_occurs_check(X, Y).
oc_member(X, [_|Z]) :-
	oc_member(X, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

absmember(X, [Y|_]) :- 
	X == Y, 
	!.
absmember(X, [_|L]) :- 
	absmember(X, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
