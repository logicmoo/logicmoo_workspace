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

:- use_module(pdt_meta_specification, [extended_meta_predicate/2]).
:- use_module(pdt_prolog_metainference, []).
%:- [meta_inference_examples].
%:- [meta_inference_test].

/* 
declared_in_module(Module, Name, Arity, Module), functor(Head, Name, Arity), infer_meta(Module:Head, MetaSpec), infer_meta_predicate(Module:Head, MetaSpec2), MetaSpec \== MetaSpec2.

pmi:inferred_meta_pred(_, M, MetaSpec), predicate_property(M:MetaSpec, file(File)), atom_concat('l:/work/beckera/git-repos/jtransformer', _, File), \+ predicate_property(M:MetaSpec, meta_predicate(_)).
*/
