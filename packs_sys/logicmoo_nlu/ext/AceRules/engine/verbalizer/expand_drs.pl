% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(expand_drs, [
		expand_drs/2  % +DRSIn, -DRSOut
	]).

:- use_module('../skolemizer/skolemizer').
:- use_module('../op_defs').
:- use_module('../list_utils').

/** <module> Predicate expander

This module expands the predicates that have been condensed during parsing.

@author Tobias Kuhn
@version 2008-11-24
*/


%% expand_drs(+DRSIn, -DRSOut)
%
% Expands the predicates of the DRS.

expand_drs(drs(_, CondIn), drs([], CondOut)) :-
	expand(CondIn, CondTemp),
	expand_neg_mod(CondTemp, CondOut).


expand_neg_mod([], []).

expand_neg_mod([Term-1|RestIn], [Term-1|RestOut]) :-
	expand_neg_mod(RestIn, RestOut).

expand_neg_mod([-drs(_, CondIn)|RestIn], [-drs([], CondOut)|RestOut]) :-
	expand(CondIn, CondTemp),
	expand_neg_mod(CondTemp, CondOut),  % there can be nested modality boxes
	expand_neg_mod(RestIn, RestOut).

expand_neg_mod([In|RestIn], [Out|RestOut]) :-
    In =.. [Mod, drs(_, CondIn)],
	expand(CondIn, CondOut),
	Out =.. [Mod, drs([], CondOut)],
	expand_neg_mod(RestIn, RestOut).


expand(CondIn, CondOut) :-
	expand_be_adj(CondIn, CondTemp1),
	expand_pred(CondTemp1, CondTemp2),
	introduce_be(CondTemp2, CondTemp3),
	expand_of(CondTemp3, CondTemp4),
	transform_objects(CondTemp4, CondOut).


expand_pred(CondIn, CondOut) :-
	member(pred_mod(Name,A,Mods)-1, CondIn),
	!,
	remove(CondIn, pred_mod(Name,A,Mods)-1, CondTemp1),
	skolemize(v, Var),
	transform_mods(Mods, Var, ModsT),
	append([predicate(Var,Name,A)-1|ModsT], CondTemp1, CondTemp2),
	expand_pred(CondTemp2, CondOut).

expand_pred(CondIn, CondOut) :-
	member(pred_mod(Name,A,B,Mods)-1, CondIn),
	!,
	remove(CondIn, pred_mod(Name,A,B,Mods)-1, CondTemp1),
	skolemize(v, Var),
	transform_mods(Mods, Var, ModsT),
	append([predicate(Var,Name,A,B)-1|ModsT], CondTemp1, CondTemp2),
	expand_pred(CondTemp2, CondOut).

expand_pred(CondIn, CondOut) :-
	member(pred_mod(Name,A,B,C,Mods)-1, CondIn),
	!,
	remove(CondIn, pred_mod(Name,A,B,C,Mods)-1, CondTemp1),
	skolemize(v, Var),
	transform_mods(Mods, Var, ModsT),
	append([predicate(Var,Name,A,B,C)-1|ModsT], CondTemp1, CondTemp2),
	expand_pred(CondTemp2, CondOut).

expand_pred(Cond, Cond).


expand_of(CondIn, CondOut) :-
	member(of_relation(object(A,Noun,Q,U,O,C),B)-1, CondIn),
	!,
	remove(CondIn, of_relation(object(A,Noun,Q,U,O,C),B)-1, CondTemp1),
	CondTemp2 = [object(A,Noun,Q,U,O,C)-1, relation(A,of,B)-1|CondTemp1],
	expand_of(CondTemp2, CondOut).

expand_of(Cond, Cond).


expand_be_adj(CondIn, CondOut) :-
	member(be_adj(A,Adj,D,Mods)-1, CondIn),
	!,
	remove(CondIn, be_adj(A,Adj,D,Mods)-1, CondTemp1),
	skolemize(v, B),
	CondTemp2 = [pred_mod(be,A,B,Mods)-1, property(B,Adj,D)-1|CondTemp1],
	expand_be_adj(CondTemp2, CondOut).

expand_be_adj(Cond, Cond).


introduce_be(CondIn, CondOut) :-
	member(of_relation(object(v(A),Noun,Q,U,O,C),Obj)-1, CondIn),
	atom(A),
	\+ Q = named,
	!,
	remove(CondIn, of_relation(object(v(A),Noun,Q,U,O,C),Obj)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [of_relation(object(Var1,Noun,Q,U,O,C),Obj)-1,
	             predicate(Var2,be,v(A),Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
	member(object(v(A),Noun,Q,U,O,C)-1, CondIn),
	atom(A),
	\+ Q = named,
	!,
	remove(CondIn, object(v(A),Noun,Q,U,O,C)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [object(Var1,Noun,Q,U,O,C)-1,
	             predicate(Var2,be,v(A),Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
	member(object(v(A),_,_,_,_,_)-1, CondIn),
	member(of_relation(object(v(A),Noun,Q,U,O,C),Obj)-1, CondIn),
	\+ atom(A),
	!,
	remove(CondIn, of_relation(object(v(A),Noun,Q,U,O,C),Obj)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [of_relation(object(Var1,Noun,Q,U,O,C),Obj)-1,
	             predicate(Var2,be,v(A),Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
	member(of_relation(object(F,Noun,Q,U,O,C),Obj)-1, CondIn),
	compound(F),
	F \= v(_),
	!,
	remove(CondIn, of_relation(object(F,Noun,Q,U,O,C),Obj)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [of_relation(object(Var1,Noun,Q,U,O,C),Obj)-1,
	             predicate(Var2,be,F,Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
	member(object(v(A),Noun1,Q1,U1,O1,C1)-1, CondIn),
	member(object(v(A),Noun2,Q2,U2,O2,C2)-1, CondIn),
	\+ atom(A),
	\+ object(v(A),Noun1,Q1,U1,O1,C1) == object(v(A),Noun2,Q2,U2,O2,C2),
	!,
	remove(CondIn, object(v(A),Noun2,Q2,U2,O2,C2)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [object(Var1,Noun2,Q2,U2,O2,C2)-1,
	             predicate(Var2,be,v(A),Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
	member(object(F,Noun,Q,U,O,C)-1, CondIn),
	compound(F),
	F \= v(_),
	!,
	remove(CondIn, object(F,Noun,Q,U,O,C)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [object(Var1,Noun,Q,U,O,C)-1,
	             predicate(Var2,be,F,Var1)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
    member(property(F,Adj,D)-1, CondIn),
    compound(F),
    \+ member(predicate(v(_),be,_,F)-1, CondIn),
	!,
	remove(CondIn, property(F,Adj,D)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [predicate(Var1,be,F,Var2)-1,
	             property(Var2,Adj,D)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
    member(property(F,Adj,D,B)-1, CondIn),
    compound(F),
    \+ member(predicate(v(_),be,_,F)-1, CondIn),
	!,
	remove(CondIn, property(F,Adj,D,B)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [predicate(Var1,be,F,Var2)-1,
	             property(Var2,Adj,D,B)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(CondIn, CondOut) :-
    member(property(F,Adj,B,D,T,C)-1, CondIn),
    compound(F),
    \+ member(predicate(v(_),be,_,F)-1, CondIn),
	!,
	remove(CondIn, property(F,Adj,B,D,T,C)-1, CondTemp1),
	skolemize(v, Var1),
	skolemize(v, Var2),
	CondTemp2 = [predicate(Var1,be,F,Var2)-1,
	             property(Var2,Adj,B,D,T,C)-1
	            |CondTemp1],
	introduce_be(CondTemp2, CondOut).

introduce_be(Cond, Cond).


transform_objects(CondIn, CondOut) :-
	member(object(A,v(V),Q,U,O,C)-1, CondIn),
	!,
	remove(CondIn, object(A,v(V),Q,U,O,C)-1, CondTemp1),
	CondTemp2 = [object(A,something,dom,na,na,na)-1|CondTemp1],
	transform_objects(CondTemp2, CondOut).

transform_objects(Cond, Cond).


transform_mods([], _, []).

transform_mods([modifier_adv(Adv,D)|RestIn], Var, [modifier_adv(Var,Adv,D)-1|RestOut]) :-
	transform_mods(RestIn, Var, RestOut).

transform_mods([modifier_pp(Prep,R2)|RestIn], Var, [modifier_pp(Var,Prep,R2)-1|RestOut]) :-
	transform_mods(RestIn, Var, RestOut).

