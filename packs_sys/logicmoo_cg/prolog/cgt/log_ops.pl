/* COPYRIGHT ************************************************************

Conceptual Graph Tools (CGT) - a partial implementation of Sowa's CS Theory
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

/* GENERALITIES *********************************************************
 
File Name	: LOG_OPS.PL
Creation Date	: 90/09/04 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Implements logical operations with conceptual graphs
 
************************************************************************/

/* HISTORY **************************************************************

0.0	90/09/04  mw	Definition 4.2.2, erasure/1, insertion/2, double_neg/2
0.1	90/09/08  mw	Definition 4.2.4, (de)iteration, check predicates
0.2	90/11/08  mw	changed direct calls to c/3
0.3     90/12/11  mw    double_negation now handles the empty set of graphs
1.0     91/01/03  mw    debugged check_deiteration/3 and made it faster

************************************************************************/
 
/* CONTENTS *************************************************************

is_neg_context/1	succeeds iff a graph represents a negative context
is_double_neg/1		succeeds iff a graph represents a double negation

depth/2			returns the depth of a graph or concept
evenly_enclosed/1	succeeds iff a graph or concept is evenly enclosed
oddly_enclosed/1	succeeds iff a graph or concept is oddly enclosed
dominates/2		succeeds iff a context dominates another one

erasure/1		erases a given graph
insertion/2		inserts a given graph in a given context
check_iteration/2	succeeds if the conditions for an iteration are met
iteration/3		copies a given graph into a given context
check_deiteration/2	succeeds if the conditions for a deiteration are met
deiteration/1		deiterates a given graph
double_neg/3		draws or removes a double negation

	
************************************************************************/

/************************************************************************

			D E F I N I T I O N   4 . 2 . 2

************************************************************************/
 
/* is_neg_context/1 *****************************************************

Usage		: is_neg_context(+Graph)
Argument(s)	: 	      	   GID
Description	: succeeds iff Graph represents a negative context
Notes		: 

************************************************************************/

is_neg_context(GID) :-
	g(GID, [p/Id-_], [neg(_)]), p(p/Id, proposition, _, _).

/* is_double_neg/1 ******************************************************

Usage		: is_double_neg(+Graph)
Argument(s)	: 	      	  GID
Description	: succeeds iff Graph is a double negation
Notes		: 

************************************************************************/

is_double_neg(g/G1) :-
	g(g/G1, [p/P1-_], [neg(_)]), p(p/P1, proposition, [g/G2], _),
	g(g/G2, [p/P2-_], [neg(_)]), p(p/P2, proposition, _, p/P1).

/************************************************************************

			D E F I N I T I O N   4 . 2 . 4

************************************************************************/
 
/* depth/2 **************************************************************

Usage		: depth(+Object, -Depth)
Argument(s)	: 	   ID	 integer
Description	: returns the depth of Object (a graph or concept)
Notes		: this predicate assumes Object isn't a list (compound graph)

************************************************************************/

depth(outer, 0) :- !.
depth(g/Id, N) :-
	which_context(g/Id, PID), depth(PID, N), !.
depth(p/Id, N) :-
	which_graph(p/Id, _, GID), depth(GID, M),
	( is_neg_context(GID) -> succ(M, N) ; M = N ), !.
depth(c/Id, N) :-
	which_graph(c/Id, _, GID), depth(GID, N), !.
	
/* evenly_enclosed/1 ****************************************************

Usage		: evenly_enclosed(+Object)
Argument(s)	: 	   	     ID
Description	: succeeds if Object (a graph or concept) is evenly enclosed
Notes		: this predicate assumes Object isn't a list (compound graph)

************************************************************************/

evenly_enclosed(ID) :-
	depth(ID, N), 0 is N mod 2.

/* oddly_enclosed/1 *****************************************************

Usage		: oddly_enclosed(+Object)
Argument(s)	: 	   	    ID
Description	: succeeds if Object (a graph or concept) is oddly enclosed
Notes		: this predicate assumes Object isn't a list (compound graph)

************************************************************************/

oddly_enclosed(ID) :-
	depth(ID, N), 1 is N mod 2.

/* dominates/2 **********************************************************

Usage		: dominates(?Context1, ?Context2)
Argument(s)	: 	       PID	  PID 
Description	: succeeds iff Context1 dominates Context2
Notes		: at least one of the arguments must be instantiated
		  generates through backtracking all dominating (dominated)
		  	contexts of Context2 (by Context1)

************************************************************************/

dominates(Env1, Env2) :-
	p(Env2, _, _, Env1).
dominates(Env1, Env2) :-
	var(Env2), !, p(TmpEnv, _, _, Env1), dominates(TmpEnv, Env2).
dominates(Env1, Env2) :-
	p(Env2, _, _, TmpEnv), !, dominates(Env1, TmpEnv).
dominates(outer, _).
	
/************************************************************************

			A S S U M P T I O N   4 . 3 . 1

************************************************************************/

/* erasure/1 ************************************************************

Usage		: erasure(+Graph)
Argument(s)	: 	    GID
Description	: erases Graph
Notes		: this predicate assumes Graph is evenly enclosed

************************************************************************/

erasure(CG) :-
	which_context(CG, ID), delete_obj(CG),
	( ID = outer
	; retract( p(ID, Type, Ref, Env) ), take_graph([CG], Ref, NewRef),
	  assert( p(ID, Type, NewRef, Env) )
	).

/* insertion/2 **********************************************************

Usage		: insertion(+Graph, +Context)
Argument(s)	: 	      GID      PID
Description	: inserts Graph in Context
Notes		: this predicate assumes Context is oddly enclosed

************************************************************************/

insertion(CG, PID) :-
	retract( p(PID, Type, Ref, Env) ), put_graph([CG], Ref, NewRef),
	assert( p(PID, Type, NewRef, Env) ), update_env(CG, PID).

/* check_iteration/2 ****************************************************

Usage		: check_iteration(+Graph, +Context)
Argument(s)	: 	     	    GID     term
Description	: succeeds iff Graph may be iterated into Context 
Notes		: 

************************************************************************/

check_iteration(CG, Env) :-
	which_context(CG, ID), ( ID = Env ; dominates(ID, Env) ).
 
/* iteration/3 **********************************************************

Usage		: iteration(+OldGraph, +Context, -NewGraph)
Argument(s)	: 	       GID       term	    GID
Description	: copies OldGraph into Context returning the copy's ID
Notes		: this predicate assumes Context is OldGraph's context
			or a context dominated by OldGraph 

************************************************************************/

iteration(CG, outer, NewCG) :-
	copy_graph(CG, NewCG, outer).
iteration(CG, PID, NewCG) :-
	copy_graph(CG, NewCG, PID), retract( p(PID, Type, Ref, Env) ),
	put_graph([NewCG], Ref, NewRef), assert( p(PID, Type, NewRef, Env) ).

/* check_deiteration/3 **************************************************

Usage		: check_deiteration(+Graph, -Copy, -Context)
Argument(s)	: 	      	      GID    GID     term	
Description	: succeeds iff Graph may be deiterated because of the presence
			of Copy in Context
Notes		: 

************************************************************************/

check_deiteration(CG, Copy, p/Id) :-
        which_context(CG, p/P),
        ( p/Id = p/P ; dominates(p/Id, p/P) ),
        referent(p/Id, GIDs), member(Copy, GIDs), Copy \= CG, is_copy(Copy, CG).
check_deiteration(CG, Copy, outer) :-
        g(Copy, _, _), Copy \= CG, which_context(Copy, outer), 
        is_copy(CG, Copy).

/* is_copy/2 ************************************************************

Usage		: is_copy(+Object1, +Object2)
Argument(s)	: 	    term      term
Description	: succeeds iff Object2 is an exact copy of Object1
Notes		: Object is a graph, concept, abstraction, referent
			or a list of objects

************************************************************************/

is_copy([O1|List1], [O2|List2]) :-
	length(List1, N), length(List2, N), 
	map(is_copy(_, _), [O1|List1], [O2|List2]).
is_copy(g/G1, g/G2) :-
	g(g/G1, [C1-_], []), !, g(g/G2, [C2-_], []), is_copy(C1, C2).
is_copy(g/G1, g/G2) :-
	g(g/G1, CL1, RL1), dir_reference(CL1, RL1), map(_ =.. _, RL1, RL11),
	g(g/G2, CL2, RL2), dir_reference(CL2, RL2), map(_ =.. _, RL2, RL21),
	is_copy_rel(RL11, RL21, [], [], Cs1, Cs2), is_copy(Cs1, Cs2).
is_copy(ID1, ID2) :-
	type(ID1, Type1), referent(ID1, Ref1), basic_ref(Ref1, Basic1),
	type(ID2, Type2), referent(ID2, Ref2), basic_ref(Ref2, Basic2),
	is_copy(Type1, Type2), is_copy(Basic1, Basic2).
is_copy(l/L1, l/L2) :-
	l(l/L1, IDs1, GIDs1), l(l/L2, IDs2, GIDs2), is_copy(GIDs1, GIDs2), 
	map(copy_parameter(_, _, GIDs1, GIDs2), IDs1, IDs2).
is_copy(set(Kind, Set1, X), set(Kind, Set2, X)) :-
	subset(Set1, Set2), subset(Set2, Set1).
is_copy(X, X).

/* is_copy_rel/6 ********************************************************

Usage		: is_copy_rel(+Rels1, +Rels2, +CLI1, +CLI2, -CLO1, -CLO2)
Argument(s)	: lists
Description	: succeeds iff Rels1 is a copy of Rels2, i.e. the same 
			relations are connected to the same concepts
Notes		: CLI1 and CLI2 (CLO1 and CLO2) are the lists of concepts 
			that must be copies before (after) this predicate
		  Rels1 and Rels2 are lists of lists with 
		  	head = type of relation and tail = connected concepts

************************************************************************/

is_copy_rel([[Type|Args1]|T1], RL2, CLI1, CLI2, CLO1, CLO2) :-
	member([Type|Args2], RL2), 
	correspond_args(Args1, Args2, CLI1, CLI2, TmpCL1, TmpCL2),
	delete_one([Type|Args2], RL2, TmpRL),
	is_copy_rel(T1, TmpRL, TmpCL1, TmpCL2, CLO1, CLO2).
is_copy_rel([], [], CL1, CL2, CL1, CL2).

/* correspond_args/6 ****************************************************

Usage		: correspond_args(+IDs1, +IDs2, +CLI1, +CLI2, -CLO1, -CLO2)
Argument(s)	: lists
Description	: succeeds iff concepts IDs1 correspond to concepts IDs2
Notes		: CLI1 and CLI2 (CLO1 and CLO2) are the lists of concepts 
			that must be copies before (after) this predicate

************************************************************************/

correspond_args([ID1|T1], [ID2|T2], CLI1, CLI2, CLO1, CLO2) :-
	nth_member(ID1, CLI1, N), !, nth_member(ID2, CLI2, N),
	correspond_args(T1, T2, CLI1, CLI2, CLO1, CLO2).
correspond_args([_ID1|_T1], [ID2|_T2], _CLI1, CLI2, _CLO1, _CLO2) :-
	member(ID2, CLI2), !, fail.
correspond_args([ID1|T1], [ID2|T2], CLI1, CLI2, CLO1, CLO2) :-
	correspond_args(T1, T2, [ID1|CLI1], [ID2|CLI2], CLO1, CLO2).
correspond_args([], [], CL1, CL2, CL1, CL2).

/* deiteration/1 ********************************************************

Usage		: deiteration(+Graph)
Argument(s)	: 	        GID
Description	: erases the Graph
Notes		: this predicate assumes Graph has a copy in the same 
			or a dominating context

************************************************************************/

deiteration(CG) :-
	which_context(CG, ID), delete_obj(CG),
	( ID = outer
	; retract( p(ID, Type, Ref, Env) ), take_graph([CG], Ref, NewRef),
	  assert( p(ID, Type, NewRef, Env) )
	).

/* double_negation/3 ****************************************************

Usage		: double_negation(+OldGraph, +Context, -NewGraph)
Argument(s)	: 	      	   GID/list     PID,    list/GID
Description	: puts or removes a double negation
Notes		: if OldGraph is a GID, it is assumed to be a double negation
			which is removed returning the list of GIDs inside it
		  if OldGraph is a list of GIDs, this predicate puts a double
		  	negation around them and returns its GID (NewGraph)
		  Context is OldGraph's context

************************************************************************/

double_negation(g/G1, PID, GIDs) :-			% takes double negation
	retract( g(g/G1, [p/P1-_], _) ), free_id(g/G1), 
	retract( p(p/P1, _, [g/G2], Env) ), free_id(p/P1),
	retract( g(g/G2, [p/P2-_], _) ), free_id(g/G2),
	retract( p(p/P2, _, Ref, _) ), free_id(p/P2),
        ( Ref = '*', GIDs = []
	; GIDs = Ref, update_env(GIDs, Env)
	), %which_context(g/G1, PID),
	( PID = outer
	; retract( p(PID, Type, Ref, Env2) ),
	  take_graph([g/G1], Ref, TmpRef), put_graph(GIDs, TmpRef, NewRef),
	  assert( p(PID, Type, NewRef, Env2) )
	).
double_negation([], Env, g/G2) :-                       % puts double negation
	new_id(p/P1), new_id(p/P2), new_id(g/G1), new_id(g/G2),
        assert( p(p/P1, proposition, *, p/P2) ),
	assert( g(g/G1, [p/P1-X], [neg(X)]) ),
	assert( p(p/P2, proposition, [g/G1], Env) ),
	assert( g(g/G2, [p/P2-X], [neg(X)]) ),
        ( Env = outer
	; retract( p(Env, Type, Ref, Env2) ),
	  put_graph([g/G2], Ref, NewRef),
	  assert( p(Env, Type, NewRef, Env2) )
	).
double_negation(GIDs, Env, g/G2) :-			% puts double negation
	%GIDs = [GID|_], %which_context(GID, Env),
	new_id(p/P1), new_id(p/P2), new_id(g/G1), new_id(g/G2),
	assert( p(p/P1, proposition, GIDs, p/P2) ),
	assert( g(g/G1, [p/P1-X], [neg(X)]) ),
	assert( p(p/P2, proposition, [g/G1], Env) ),
	assert( g(g/G2, [p/P2-X], [neg(X)]) ),
	update_env(GIDs, p/P1),
	( Env = outer
	; retract( p(Env, Type, Ref, Env2) ),
	  take_graph(GIDs, Ref, TmpRef), put_graph([g/G2], TmpRef, NewRef),
	  assert( p(Env, Type, NewRef, Env2) )
	).

/* update_env/2 *********************************************************

Usage		: update_env(+Object, +Context)
Argument(s)	: 	        ID       PID
Description	: changes the context of Object to Context
Notes		: Object is a graph, concept or a list of objects

************************************************************************/

update_env([ID|List], Env) :-
	update_env(ID, Env), update_env(List, Env), !.
update_env(g/Id, Env) :-
	g(g/Id, CL, _), update_env(CL, Env), !.
update_env(p/Id-_, Env) :-
	retract( p(p/Id, Type, Ref, _) ), assert( p(p/Id, Type, Ref, Env) ), !.
update_env(_, _).			% update a concept or empty list
	
/* put_graph/3 **********************************************************

Usage		: put_graph(+Graph, +OldRef, -NewRef)
Argument(s)	: 	     list     term     term 
Description	: adds Graph to OldRef, obtaining NewRef
Notes		: this predicate assumes OldRef contains no coreference links

************************************************************************/

put_graph(GIDs, '*', GIDs).
put_graph(GIDs, Ref, NewRef) :-
	conc(GIDs, Ref, NewRef).

/* take_graph/3 *********************************************************

Usage		: take_graph(+Graph, +OldRef, -NewRef)
Argument(s)	: 	      list     term     term 
Description	: takes Graph out of OldRef, obtaining NewRef
Notes		: this predicate assumes OldRef contains no coreference links

************************************************************************/

take_graph(GIDs, Ref, NewRef) :-
	difference(Ref, GIDs, TmpRef),
	( TmpRef = [] -> NewRef = ('*') ; NewRef = TmpRef ).
