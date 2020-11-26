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

:- module(call_graph_examples, []).

:- use_module('../metainference/meta_inference_examples').

abc_de.
abc_de(_).
de_abc.
de_abc(_).

xyz.

% direct meta call
% simple0(0)
simple0 :-
	simple0(abc_de).

% nested meta call
% simple1(0)
simple1 :-
	simple1(abc_de).

% meta call in disjunction
% simple2(0, *)
simple2 :-
	simple2(abc_de, xyz).

% variable unification
% unify0(0)
unify0 :-
	unify0(abc_de).

% term unification
% unify1(0)
unify1 :-
	unify1(abc_de).

% unification chain
% unify2(0)
unify2 :-
	unify2(abc_de).

% unification chain, other order
% unify3(0)
unify3 :-
	unify3(abc_de).

% multiple unification in term
% unify4(0, 0)
unify4 :-
	unify4(abc_de, de_abc).

% unification chain via term
% unify5(0)
unify5 :-
	unify5(abc_de).

% unification after meta call not relevant
% unify6(0, *)
unify6 :-
	unify6(abc_de, xyz).

% term construction via univ/2
% construct_term1(univ_list(0))
construct_term1 :-
	construct_term1([abc_de]).

% construct functor with atom_concat/3, add a prefix
% construct_functor0(add_prefix(abc, 0))
construct_functor0 :-
	construct_functor0('_de').

% construct functor with atom_concat/3, add a suffix
% construct_functor1(add_suffix(abc, 0))
construct_functor1 :-
	construct_functor1(de_).


% construct functor with atom_concat/3, add a suffix
% the constructed functor is used to construct a term using functor/3
% construct_functor3(add_suffix(abc, has_arity(1, 0))
construct_functor3 :-
	construct_functor3(de_).
