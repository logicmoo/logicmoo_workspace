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
 
File Name	: TYPE_OPS.PL
Creation Date	: 90/06/16 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Implements operations on concept and relation types
 
************************************************************************/

/* HISTORY **************************************************************

1.0	90/06/23  mw	doesn't work for single-use types (lambda 
			abstractions)
1.1	90/07/01  mw	now it does: changed subtype/2 and proper_subtype/2
1.2	90/09/05  mw	max_common_subtype/3 much more efficient
			added type expansion operations
			correted bugs for single-use types
1.3	90/10/29  mw	corrected bug in build_graphs/3
1.4	90/11/07  mw	type/1 is deterministic

************************************************************************/
 
/* CONTENTS *************************************************************

type/2			returns the type of a concept 
subtype/2		succeeds iff a type is subtype of another
supertype/2		succeeds iff a type is supertype of another
proper_subtype/2	implements the definition of proper subtype 
proper_supertype/2	implements the definition of proper supertype 
common_subtype/3	returns a common subtype of two given types
common_supertype/3	returns a common supertype of two given types
max_common_subtype/3	returns the maximal common subtype of two types 
min_common_supertype/3	returns the minimal common supertype of two types 

rel_expansion/3		implements relational expansion
min_type_expansion/3	implements minimal type expansion
max_type_expansion/3	implements maximal type expansion

************************************************************************/

/************************************************************************

			A S S U M P T I O N   3 . 2 . 1

************************************************************************/
 
/* type/2 ***************************************************************

Usage		: type(+ConceptId, ?Type)
Argument(s)	: 	  term	    atom
Description	: succeeds iff Type is the type of the given concept
Notes		: 

************************************************************************/

type(p/Id, Type) :- 
	p(p/Id, Type, _, _), !.
type(CID, Type) :- 
	c(CID, Type, _), !.

/************************************************************************

	   A S S U M P T I O N S   3 . 2 . 3   and   3 . 6 . 8

************************************************************************/
 
/* subtype/2 ************************************************************

Usage		: subtype(?Type1, ?Type2)
Argument(s)	: 	   atom	   atom
Description	: succeeds iff Type1 <= Type2 in the type hierarchy
Notes		: generates all (sub/super)types of a given type by 
		  backtracking 

************************************************************************/

subtype(X, X).
subtype(l/Id, Type) :-
	l(l/Id, [ID], [GID]), g(GID, [_], []), 
	type(ID, SomeType), subtype(SomeType, Type).
subtype(X, Y) :- 
	proper_subtype(X, Y).

/* supertype/2 **********************************************************

Usage		: supertype(?Type1, ?Type2)
Argument(s)	:            atom    atom
Description	: succeeds iff Type1 >= Type2 in the type hierarchy
Notes		: generates all (sub/super)types of a given type by 
		  backtracking 

************************************************************************/

supertype(X, Y) :- 
	subtype(Y, X).

/* proper_subtype/2 *****************************************************

Usage		: proper_subtype(?Type1, ?Type2)
Argument(s)	: 	   	  atom	  atom
Description	: succeeds iff Type1 < Type2 in the type hierarchy
Notes		: generates all proper (sub/super)types of a given type
		  by backtracking 

************************************************************************/

proper_subtype(X, Y) :- 
	call('<<'(X , Z)), subtype(Z, Y).
proper_subtype(absurd, X) :- 
	concept_type(X, _, _, _, _), X \= absurd.
proper_subtype(X, universal) :- 
	concept_type(X, _, _, _, _), X \= universal.
proper_subtype(l/Id, Type) :-
	l(l/Id, [ID], [GID]), g(GID, [_], []), !, 
	type(ID, SomeType), proper_subtype(SomeType, Type).
proper_subtype(l/Id, Type) :-
	l(l/Id, [ID], _), type(ID, SomeType), subtype(SomeType, Type).

/* proper_supertype/2 ***************************************************

Usage		: proper_supertype(?Type1, ?Type2)
Argument(s)	: 	   	    atom    atom
Description	: succeeds iff Type1 > Type2 in the type hierarchy
Notes		: generates all proper (sub/super)types of a given type
		  by backtracking 

************************************************************************/

proper_supertype(X, Y) :- 
	proper_subtype(Y, X).

/* common_subtype/3 *****************************************************

Usage		: common_subtype(?Common, +Type1, +Type2)
Argument(s)	: 	   	  atom	   atom    atom
Description	: succeeds iff Common <= Type1 and Common <= Type2
Notes		: generates all common subtypes by backtracking 

************************************************************************/

common_subtype(X, Y, Z) :- 
	subtype(X, Y), subtype(X, Z).

/* common_supertype/3 ***************************************************

Usage		: common_supertype(?Common, +Type1, +Type2)
Argument(s)	: 	   	    atom     atom    atom 
Description	: succeeds iff Common >= Type1 and Common >= Type2
Notes		: generates all common supertypes by backtracking 

************************************************************************/

common_supertype(X, Y, Z) :-
	supertype(X, Y), supertype(X, Z).

/************************************************************************

			A S S U M P T I O N   3 . 2 . 5

************************************************************************/
 
/* max_common_subtype/3 *************************************************

Usage		: max_common_subtype(+Type1, +Type2, ?MCommon)
Argument(s)	: 	   	      atom    atom     atom
Description	: MCommon is the maximal common subtype of Type1 and Type2
Notes		: 

************************************************************************/

max_common_subtype(X, Y, X) :-
	subtype(X, Y).
max_common_subtype(X, Y, Y) :-
	subtype(Y, X).
max_common_subtype(X, Y, Z) :-
	common_subtype(Z, X, Y), 
	\+ (( common_subtype(W, X, Y), proper_supertype(W, Z) )), !.

/* min_common_supertype/3 ***********************************************

Usage		: min_common_supertype(+Type1, +Type2, ?MCommon)
Argument(s)	: 	   		atom	atom	 atom
Description	: MCommon is the minimal common supertype of Type1 and Type2
Notes		: 

************************************************************************/

min_common_supertype(X, Y, Z) :-
	common_supertype(Z, X, Y), 
	\+ (( common_supertype(W, X, Y), proper_subtype(W, Z) )), !.

/************************************************************************

			D E F I N I T I O N   3 . 6 . 15

************************************************************************/
 
/* rel_expansion/3 ******************************************************

Usage		: rel_expansion(+Relation, +Graph, -Result)
Argument(s)	: 	   	   term	     GID     list
Description	: returns the Result of expanding the Graph's Relation
Notes		: the functor of Relation is its type
		  the arguments of Relation are the connected concepts' IDs
		  Result is a list of GIDs

************************************************************************/

rel_expansion(Rel, GID, GIDList) :-
	Rel =.. [Type|Args],
	relation_type(Type, _, l/Id, _, _), l(l/Id, Param, GIDs),
	remove_rel(Rel, GID, SomeGIDs),
	which_context(GID, Env), copy_graph(GIDs, NewGIDs, Env),
	map(copy_parameter(_, _, GIDs, NewGIDs), Param, NewParam),
	map(join_concept(_, _), Args, NewParam), 
	conc(SomeGIDs, NewGIDs, MoreGIDs),
	join_graphs_on(MoreGIDs, Args, NewParam, GIDList).
rel_expansion(_, GID, [GID]).

/* remove_rel/3 *********************************************************

Usage		: remove_rel(+Relation, +Graph, -Graphs)
Argument(s)	: 		term	  GID	  list
Description	: removes Relation from Graph creating disconnected Graphs
Notes		: the functor of Relation is its type
		  the arguments of Relation are the connected concepts' IDs

************************************************************************/

remove_rel(Rel, GID, GIDs) :-
	retract( g(GID, CL, RL) ), free_id(GID), dir_reference(CL, RL),
	delete_one(Rel, RL, RestRels), Rel =.. [_Type|Args],
	build_graphs(Args, RestRels, GIDs).

/* build_graphs/3 *******************************************************

Usage		: build_graphs(+Concepts, +Relations, -Graphs)
Argument(s)	: lists
Description	: builds Graphs using Relations
Notes		: if Concepts are still connected, then Graphs is just one

************************************************************************/

build_graphs([Arg|ArgList], RelList, [g/Id|GIDList]) :- 
	part_of_graph([Arg], RelList, IDs, TmpRL),
	ind_reference(TmpRL, RL, [Arg-_Var], CL),
	new_id(g/Id), assert( g(g/Id, CL, RL) ),
	difference(RelList, TmpRL, RestRels), 
	difference(ArgList, IDs, RestArgs),
	build_graphs(RestArgs, RestRels, GIDList).
build_graphs([], [], []).

/* part_of_graph/4 ******************************************************

Usage		: part_of_graph(+Concepts, +Graph, -ConList, -RelList)
Argument(s)	: lists
Description	: ConList/RelList is the list of concepts/relations that form
		  the part of the Graph attached to Concepts
Notes		: Graph is a list of relations with CIDs as arguments

************************************************************************/

part_of_graph(IDs, [Rel|List], CL, RL) :-
	Rel =.. [_|Args], intersection(Args, IDs, []), 
	part_of_graph(IDs, List, CL, RL).
part_of_graph(IDs, [Rel|T1], CL, [Rel|T2]) :-
	Rel =.. [_|Args], delete_dup(Args, MoreIDs),
	union(IDs, MoreIDs, NewIDs), part_of_graph(NewIDs, T1, CL, T2).
part_of_graph(IDs, [], IDs, []).

/************************************************************************

			D E F I N I T I O N   3 . 6 . 6

************************************************************************/
 
/* min_type_expansion/3 *************************************************

Usage		: min_type_expansion(+Concept, +Graph, -Result)
Argument(s)	: 	   	      CID/PID	 GID	 list
Description	: returns the Result of expanding minimally the Concept's type
Notes		: Concept belongs to Graph; Result is a list of GIDs

************************************************************************/

min_type_expansion(ID, GID, Result) :-
	type(ID, Type),	concept_type(Type, _, l/Id, _, _), l(l/Id, [CID], GIDs),
	which_context(GID, Env), copy_graph(GIDs, NewGIDs, Env), 
	copy_parameter(CID, NewCID, GIDs, NewGIDs),
	join_concept(ID, NewCID), 
	join_graphs_on([GID|NewGIDs], [ID], [NewCID], Result).
min_type_expansion(_, GID, [GID]).

/************************************************************************

			D E F I N I T I O N   3 . 6 . 7

************************************************************************/
 
/* max_type_expansion/3 *************************************************

Usage		: max_type_expansion(+Concept, +Graph, -Result)
Argument(s)	: 	   	      CID/PID	 GID	 list
Description	: returns the Result of expanding maximally the Concept's type
Notes		: Concept belongs to Graph; Result is a list of GIDs

************************************************************************/

max_type_expansion(ID, GID, [GID|RestGIDs]) :-
	type(ID, Type),	
	( concept_type(Type, _, l/Id, _, _) ; Type = l/Id ),
	l(l/Id, [CID], GIDs),
	which_context(GID, Env), copy_graph(GIDs, NewGIDs, Env), 
	copy_parameter(CID, NewCID, GIDs, NewGIDs),
	which_graph(NewCID, NewGIDs, NewGID), 
	join_concept(ID, NewCID), type(NewCID, SuperType),
	( retract( c(ID, Type, Ref) ), assert( c(ID, SuperType, Ref) )
	; retract( p(ID, Type, Ref, Env) ), assert( p(ID, SuperType, Ref, Env) )
	),
	g(GID, CL1, RL1), dir_reference(CL1, RL1), map(_ =.. _, RL1, Rel1),
	g(NewGID, CL2, RL2), dir_reference(CL2, RL2), map(_ =.. _, RL2, Rel2),
	matched_concepts([ID-NewCID], Rel1, Rel2, MC1, MC2),
	join_on(GID, NewGID, MC1, MC2), delete_one(NewGID, NewGIDs, RestGIDs),
	( Env = outer
	; retract( p(Env, TyEnv, RefEnv, EnvEnv) ), 
	  put_graph(RestGIDs, RefEnv, NewRefEnv), 
	  assert( p(Env, TyEnv, NewRefEnv, EnvEnv) )
	).
max_type_expansion(_, GID, [GID]).


