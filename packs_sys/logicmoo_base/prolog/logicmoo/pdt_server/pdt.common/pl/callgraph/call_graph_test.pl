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

:- module(call_graph_test, []).

:- use_module(library(lists)).
:- use_module(pdt_call_graph).

refs_to(RName, RArity, Refs) :-
	ensure_call_graph_generated,
	findall([Module, Name, Arity], calls(call_graph_examples, RName, RArity, Module, Name, Arity, _NumberOfCalls), Refs).

:- begin_tests(call_simple_meta).

test(direct_meta_call) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, simple0, 0], Refs),
	!.

test(nested_meta_call) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, simple1, 0], Refs),
	!.

test(meta_call_in_disjunction) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, simple2, 0], Refs),
	!.

:- end_tests(call_simple_meta).


:- begin_tests(call_unification).

test(variable) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify0, 0], Refs),
	!.

test(term) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify1, 0], Refs),
	!.

test(chain) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify2, 0], Refs),
	!.

test(chain_other_order) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify3, 0], Refs),
	!.

test(multiple_in_term) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify4, 0], Refs),
	refs_to(de_abc, 0, Refs2),
	member([call_graph_examples, unify4, 0], Refs2),
	!.

test(chain_via_term) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify5, 0], Refs),
	!.

test(after_meta_call_not_relevant) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, unify6, 0], Refs),
	!.

:- end_tests(call_unification).


:- begin_tests(call_term_construction).

test(univ_2) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, construct_term1, 0], Refs),
	!.

:- end_tests(call_term_construction).


:- begin_tests(call_functor_construction).

test(atom_concat_3_add_prefix) :-
	refs_to(abc_de, 0, Refs),
	member([call_graph_examples, construct_functor0, 0], Refs),
	!.

test(atom_concat_3_add_suffix) :-
	refs_to(de_abc, 0, Refs),
	member([call_graph_examples, construct_functor1, 0], Refs),
	!.

test(atom_concat_3_add_suffix_construct_term_with_functor_3) :-
	refs_to(de_abc, 1, Refs),
	member([call_graph_examples, construct_functor3, 0], Refs),
	!.

:- end_tests(call_functor_construction).


:- begin_tests(call_uncalled).

test(uncalled) :-
	refs_to(xyz, 0, Refs),
	Refs == [].

:- end_tests(call_uncalled).
