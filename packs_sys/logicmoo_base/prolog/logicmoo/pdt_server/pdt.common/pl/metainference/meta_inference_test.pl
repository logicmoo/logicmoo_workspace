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

:- module(meta_inference_test, []).

:- use_module(pdt_prolog_metainference).
:- use_module(library(lists)).

:- begin_tests(simple_meta).

test(direct_meta_call) :-
	infer_meta(meta_inference_examples:simple0(_), MetaSpec),
	MetaSpec == simple0(0).

test(nested_meta_call) :-
	infer_meta(meta_inference_examples:simple1(_), MetaSpec),
	MetaSpec == simple1(0).

test(meta_call_in_disjunction) :-
	infer_meta(meta_inference_examples:simple2(_,_), MetaSpec),
	MetaSpec == simple2(0, *).

:- end_tests(simple_meta).


:- begin_tests(unification).

test(variable) :-
	infer_meta(meta_inference_examples:unify0(_), MetaSpec),
	MetaSpec == unify0(0).

test(term) :-
	infer_meta(meta_inference_examples:unify1(_), MetaSpec),
	MetaSpec == unify1(0).

test(chain) :-
	infer_meta(meta_inference_examples:unify2(_), MetaSpec),
	MetaSpec == unify2(0).

test(chain_other_order) :-
	infer_meta(meta_inference_examples:unify3(_), MetaSpec),
	MetaSpec == unify3(0).

test(multiple_in_term) :-
	infer_meta(meta_inference_examples:unify4(_,_), MetaSpec),
	MetaSpec == unify4(0,0).

test(chain_via_term) :-
	infer_meta(meta_inference_examples:unify5(_), MetaSpec),
	MetaSpec == unify5(0).

test(after_meta_call_not_relevant) :-
	infer_meta(meta_inference_examples:unify6(_,_), MetaSpec),
	MetaSpec == unify6(0,*).

:- end_tests(unification).


:- begin_tests(term_construction).

test(functor_3) :-
	infer_meta(meta_inference_examples:construct_term0(_,_), MetaSpec),
	MetaSpec == construct_term0(functor(0), arity(0)).

test(univ_2) :-
	infer_meta(meta_inference_examples:construct_term1(_), MetaSpec),
	MetaSpec == construct_term1(univ_list(0)).

test(univ_2_only_functor) :-
	infer_meta(meta_inference_examples:construct_term2(_,_), MetaSpec),
	MetaSpec == construct_term2(functor(0), *).

:- end_tests(term_construction).


:- begin_tests(functor_construction).

test(atom_concat_3_add_prefix) :-
	infer_meta(meta_inference_examples:construct_functor0(_), MetaSpec),
	MetaSpec == construct_functor0(add_prefix(abc, 0)).

test(atom_concat_3_add_suffix) :-
	infer_meta(meta_inference_examples:construct_functor1(_), MetaSpec),
	MetaSpec == construct_functor1(add_suffix(abc, 0)).

test(atom_concat_3_prefix_and_suffix) :-
	infer_meta(meta_inference_examples:construct_functor2(_,_), MetaSpec),
	MetaSpec == construct_functor2(is_prefix(0), is_suffix(0)).

test(atom_concat_3_add_suffix_construct_term_with_functor_3) :-
	infer_meta(meta_inference_examples:construct_functor3(_), MetaSpec),
	MetaSpec == construct_functor3(add_suffix(abc, has_arity(1, 0))).

:- end_tests(functor_construction).


:- begin_tests(non_meta).

test(non_meta0) :-
	\+ infer_meta(meta_inference_examples:non_meta0(_), _).

test(non_meta1) :-
	\+ infer_meta(meta_inference_examples:non_meta1(_), _).

test(non_meta2) :-
	\+ infer_meta(meta_inference_examples:non_meta2(_), _).

test(non_meta3) :-
	\+ infer_meta(meta_inference_examples:non_meta3(_), _).

test(non_meta4) :-
	\+ infer_meta(meta_inference_examples:non_meta4(_), _).

test(non_meta5) :-
	\+ infer_meta(meta_inference_examples:non_meta5(_), _).

:- end_tests(non_meta).
