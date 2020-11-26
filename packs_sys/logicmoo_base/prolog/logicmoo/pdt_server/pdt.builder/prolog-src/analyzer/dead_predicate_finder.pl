/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(dead_predicate_finder,[	uncalled_predicate/1,
									uncalled_local_predicate/1,
									locally_dead_predicate/1]).

:- ensure_loaded('../pdt_factbase').
:- use_module('../modules_and_visibility').
:- use_module('edge_counter').


locally_dead_predicate(Dead) :-
    locally_dead_predicate(Dead, [Dead]).

locally_dead_predicate(Dead, _Visited):-
    uncalled_local_predicate(Dead).
locally_dead_predicate(Dead, Visited):-
    forall(	
    	(call_edges_for_predicates(Caller,Dead,_), Caller \== Dead, not(member(Caller, Visited))),
    	locally_dead_predicate(Caller, [Caller | Visited])
    ),
    \+(exporting(_,Dead,_)).
    		
    
    


uncalled_local_predicate(Uncalled):-
    uncalled_predicate(Uncalled),
    \+(exporting(_,Uncalled,_)).


uncalled_predicate(Uncalled):-
    predicateT(Uncalled,_,_,_,_),
    \+(call_edges_for_predicates(_,Uncalled,_)).


