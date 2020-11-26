/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(meta_inference_examples, [
	simple0/1,
	simple1/1,
	simple2/2,
	unify0/1,
	unify1/1,
	unify2/1,
	unify3/1,
	unify4/2,
	unify5/1,
	unify6/2,
	construct_term0/2,
	construct_term1/1,
	construct_term2/2,
	construct_functor0/1,
	construct_functor1/1,
	construct_functor2/2,
	construct_functor3/1
]).

% direct meta call
% simple0(0)
:- meta_predicate simple0(0).

simple0(Z) :-
	call(Z).

% nested meta call
% simple1(0)
simple1(Z) :-
	call(call(Z)).

% meta call in disjunction
% simple2(0, *)
simple2(C1, C2) :-
	(	C1 = C2
	;	call(C1)
	).

% variable unification
% unify0(0)
unify0(Y) :-
	Y = Z,
	call(Z).

% term unification
% unify1(0)
unify1(X) :-
	term(X, _Y) = term(Z, funny),
	call(Z).

% unification chain
% unify2(0)
unify2(X) :-
	X = Y,
	Y = Z,
	call(Z).

tobeinferred(Goal) :- 
	findall(a,Goal,_).

callOfToBeInferred :- tobeinferred(unify2(non_meta1(a))).

% unification chain, other order
% unify3(0)
unify3(X) :-
	Y = Z,
	X = Y,
	call(Z).

% multiple unification in term
% unify4(0, 0)
unify4(X, Y) :-
	term(X, X) = term(Y, Z),
	call(Z).

% unification chain via term
% unify5(0)
unify5(X) :-
	Y = term(X),
	Y = term(Z),
	call(Z).

% unification after meta call not relevant
% unify6(0, *)
unify6(Z, Y) :-
	call(Z),
	Z = Y.

% term construction via functor/3
% construct_term0(functor(0), arity(0))
construct_term0(X, Y) :-
	functor(Z, X, Y),
	call(Z).

% term construction via univ/2
% construct_term1(univ_list(0))
construct_term1(Y) :-
	Z =.. Y,
	call(Z).

% term construction via univ/2, only functor
% construct_term2(functor(0), *)
construct_term2(X, Y) :-
	Z =.. [X, 1|Y],
	call(Z).

% construct functor with atom_concat/3, add a prefix
% construct_functor0(add_prefix(abc, 0))
construct_functor0(Y) :-
	atom_concat(abc, Y, Z),
	call(Z).

% construct functor with atom_concat/3, add a suffix
% construct_functor1(add_suffix(abc, 0))
construct_functor1(Y) :-
	atom_concat(Y, abc, Z),
	call(Z).

% construct functor with atom_concat/3, add an unknown prefix and an unknown suffix
% construct_functor2(is_prefix(0), is_suffix(0))
construct_functor2(X, Y) :-
	atom_concat(X, Y, Z),
	call(Z).

% construct functor with atom_concat/3, add a suffix
% the constructed functor is used to construct a term using functor/3
% construct_functor3(add_suffix(abc, has_arity(1, 0))
construct_functor3(X) :-
	atom_concat(X, abc, Y),
	functor(Z, Y, 1),
	call(Z).

% the following predicate are not meta predicates
% infer_meta/2 fails
non_meta0(Z) :-
	call(p(Z)).

non_meta1(_Z) :-
	call(_Y).

non_meta2(Z) :-
	Z = 4.

non_meta3(Z) :-
	Y =.. [p, Z],
	call(Y).

non_meta4(Z) :-
	Y = p(Z),
	call(Y).

non_meta5(Z) :-
	functor(Z, X, Y),
	assertz(p(Z, X, Y)).

