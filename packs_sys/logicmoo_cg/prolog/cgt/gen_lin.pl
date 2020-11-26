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
 
File Name	: GEN_LIN.PL
Creation Date	: 90/06/26 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Generates the linear notation of a semantic net component
Notes		: the arity of the DCG predicates doesn't include the lists
		  an edge is an arc with its associated nodes
 
************************************************************************/

/* HISTORY **************************************************************
 
1.0	90/07/08  mw	handles contexts and single-use types
1.1	90/08/23  mw	handles type definitions and schemas
1.2	90/08/25  mw	supports n-adic relations; lots of code simplified
1.3	90/08/29  mw	supports compound graphs; more code simplified
1.4	90/10/23  mw	added can_graph to gen_linear/2
1.5     90/11/26  mw    write_linear/3 now stops with the empty list
1.6     90/11/27  mw    added gen_header/6

************************************************************************/

/* CONTENTS *************************************************************
 
write_linear/2		pretty prints the linear form of a graph or abstraction
write_linear/3          does the actual pretty-printing
gen_header/6            DCG predicate to generate the descriptive first line

************************************************************************/

/* write_linear/2 ******************************************************

Usage		: write_linear(+Kind, +ObjectId)
Argument(s)	: 	        atom   	 term
Description	: writes the linear form of a graph, type definition or schema
Notes		: the possible values for the Kind-ObjectId pair are
		  graph-GID, type_def-TypeName, rel_def-RelName, schema-LID
		  and can_graph-TypeName

************************************************************************/

write_linear(Kind, Obj) :-
	gen_linear(Kind, Obj, Linear, ['.']), 
	( recorded(crl, _, Ref), erase(Ref), fail ; true ),
	write_linear(0, Linear, []), !.

/* gen_linear/2 *********************************************************

Usage		: gen_linear(+Kind, +ObjectId)
Argument(s)	: 	      atom     term
Description	: DCG predicate to generate the linear notation
Notes		: the possible values for the Kind-ObjectId pair are
		  graph-GID, type_def-TypeName, rel_def-RelName, schema-LID
		  and can_graph-TypeName
		  			  
************************************************************************/

gen_linear(type_def, Type) -->
	{ concept_type(Type, Label, l/Id, _, _), l(l/Id, CIDs, GID) },
        gen_header(type_def, Label, CIDs, Marked, 0, TmpVar), [nl],
	gen_graph(GID, Marked, TmpVar, _).
gen_linear(rel_def, Type) -->
	{ relation_type(Type, Label, l/Id, _, _), l(l/Id, CIDs, GID) },
        gen_header(rel_def, Label, CIDs, Marked, 0, TmpVar), [nl],
	gen_graph(GID, Marked, TmpVar, _).
gen_linear(schema, LID) -->
	{ l(LID, [CID], GID), type(CID, Type), 
	  concept_type(Type, Label, _, _, _)
	},
        gen_header(schema, Label, [CID], Marked, 0, TmpVar), [nl],
	gen_graph(GID, Marked, TmpVar, _).
gen_linear(can_graph, Type) -->
	{ concept_type(Type, Label, _, Can, _) 
	; relation_type(Type, Label, _, Can, _)
	},
        gen_header(can_graph, Label, [], Marked, 0, TmpVar), [nl],
        gen_graph(Can, Marked, TmpVar, _).
gen_linear(graph, GID) -->
	gen_graph(GID, [], 0, _).

gen_header(type_def, Label, CIDs, Marked, VarIn, VarOut) --> 
	['type ', Label, '('], 
	gen_param(CIDs, Marked, VarIn, VarOut), [') is'].
gen_header(rel_def, Label, CIDs, Marked, VarIn, VarOut) --> 
	['relation ', Label, '('], 
	gen_param(CIDs, Marked, VarIn, VarOut), [') is'].
gen_header(schema, Label, CIDs, Marked, VarIn, VarOut) --> 
	['schema for ', Label, '('], 
	gen_param(CIDs, Marked, VarIn, VarOut), [') is'].
gen_header(can_graph, Label, _, [], VarIn, VarIn) --> 
	['canonical graph for ', Label, ' is']. 

/* gen_param/4 **********************************************************

Usage		: gen_param(+Parameters, -Marked, +VarIn, -Varout)
Argument(s)	: 	     	list	   list   integer integer
Description	: DCG predicate to generate the linear form (variables)
		  of the Parameters of an abstraction
Notes		: Parameters is a list of GID-CID pairs
		  VarIn/VarOut is the number of variables used before/after
		  this predicate
		  Marked is the list of CID-variable pairs
		  			  
************************************************************************/

gen_param([CID], [CID+Var], VarIn, VarOut) -->
	{ number2var(VarIn, Var), succ(VarIn, VarOut) }, [Var].
gen_param([CID|T1], [CID+Var|T2], VarIn, VarOut) -->
	{ number2var(VarIn, Var), succ(VarIn, TmpVar) },
	[Var, ','], gen_param(T1, T2, TmpVar, VarOut).

/* gen_graph/4 *********************************************************

Usage		: gen_graph(+Graph, +Marked, +VarIn, -VarOut)
Argument(s)	: 	      GIDs    list   integer integer
Description	: DCG predicate to generate the linear notation of Graph
Notes		: VarIn/VarOut is the number of variables used before/after
		  linearizing Graph
		  Marked is a list of concepts of Graph which are already
		  attached to a variable 
		  Graph is a list of GIDs if it is compound

************************************************************************/

gen_graph([GID], Marked, VarIn, VarOut) -->
	gen_graph(GID, Marked, VarIn, VarOut).
gen_graph([H|T], Marked, VarIn, VarOut) -->
	gen_graph(H, Marked, VarIn, TmpVar), [';'], 
	gen_graph(T, Marked, TmpVar, VarOut).
gen_graph(GID, Marked, VarIn, VarOut) -->
	{ g(GID, [CID-_], []) }, 	% graph consists of a single concept
	process_vars([CID], Marked, VarIn, VarOut).
gen_graph(GID, Marked, VarIn, VarOut) -->
	{ g(GID, CL, RL), dir_reference(CL, RL), edges(RL, 0, EdgeList),
	  gen_graph(EdgeList, Linear, [])
	}, process_vars(Linear, Marked, VarIn, VarOut).

/* process_vars/4 *******************************************************

Usage		: process_vars(+Linear, +Marked, +VarIn, -VarOut)
Argument(s)	: 		 list	  list	 integer integer
Description	: DCG predicate to get the correct linear notation for the
		  concepts, using variables as referents when needed
Notes		: VarIn/VarOut is the number of variables used before/after
		  processing Linear
		  Marked is a list of concepts that already appeared as
		  parameters (CID+Var) or in the same graph (CID-Var) and
		  therefore have a variable as referent
		  Linear contains the IDs of the concepts, not their 
		  linear form

************************************************************************/

process_vars([X/Id|T], Marked, VarIn, VarOut) -->
	{ member(X/Id+Var, Marked), delete_one(X/Id+Var, Marked, TmpMarked) }, 
	gen_concept(X/Id-Var, VarIn, TmpVar), 
	process_vars(T, [X/Id-Var|TmpMarked], TmpVar, VarOut).
process_vars([X/Id|T], Marked, VarIn, VarOut) -->
	{ member(X/Id-Var, Marked) }, 
	gen_concept(X/Id*Var, VarIn, TmpVar), 
	process_vars(T, Marked, TmpVar, VarOut).
process_vars([X/Id|T], Marked, VarIn, VarOut) -->
	{ member(X/Id, T), succ(VarIn, TmpVar1), number2var(VarIn, Var) },
	gen_concept(X/Id-Var, TmpVar1, TmpVar2), 
	process_vars(T, [X/Id-Var|Marked], TmpVar2, VarOut).
process_vars([X/Id|T], Marked, VarIn, VarOut) -->
	gen_concept(X/Id, VarIn, TmpVar),
	process_vars(T, Marked, TmpVar, VarOut).
process_vars([H|T], Marked, VarIn, VarOut) -->
	[H], process_vars(T, Marked, VarIn, VarOut).		% not a concept
process_vars([], _, VarIn, VarIn) --> [].

/* edges/3 ***************************************************************

Usage		: edges(+Relations, +Number, -Edges)
Argument(s)	: 	   list	    integer   list	
Description	: computes the Edges of a graph given the Relations
Notes		: Number is used to uniquely identify the relation;
		  an edge is of the form e(N, CID, Rel-Number) where N > 0
		  if the arc points to Rel, otherwise N < 0

************************************************************************/

edges([H|T], N, L) :-
	H =.. [Rel|Args], length(Args, NumArgs), 
	( NumArgs > 2 -> ArcCount = 1 ; ArcCount = none ),
	edges_with_rel(Rel-N, ArcCount, Args, L1), 
	succ(N, N1), edges(T, N1, L2), conc(L1, L2, L).
edges([], _, []).

/* edges_with_rel/4 *****************************************************

Usage		: edges_with_rel(+Relation, +ArcCount, +Arguments, -Edges)
Argument(s)	:	     	   term	     integer	  list      list		
Description	: computes all Edges which include Relation
Notes		: ArcCount is 'none' if Relation is monadic or dyadic
		  Arguments is the list of CIDs attached to Relation

************************************************************************/

edges_with_rel(Rel, none, [CID], [e(-_, CID, Rel)]).	% last arc points away
edges_with_rel(Rel, N, [CID], [e(-N, CID, Rel)]).
edges_with_rel(Rel, none, [CID|T1], [e(+_, CID, Rel)|T2]) :-
	edges_with_rel(Rel, none, T1, T2).
edges_with_rel(Rel, N, [CID|T1], [e(+N, CID, Rel)|T2]) :-
	succ(N, N1), edges_with_rel(Rel, N1, T1, T2).

/* relation_linked/3 ****************************************************

Usage		: relation_linked(+Relation, ?List, +Edges)
Argument(s)	:		    term      list   list		
Description	: List contains all of the Edges which include Relation
Notes		: if there are several edges including Relation and the 
		  same concept, only one is considered

************************************************************************/

relation_linked(Rel, List, AL) :-
	findall(e(N, CID, Rel), member(e(N, CID, Rel), AL), TmpList),
	del_dup_edges(TmpList, List).

/* concept_linked/3 *****************************************************

Usage		: concept_linked(+Concept, ?List, +Edges)  
Argument(s)	:		    CID	    list   list
Description	: List contains all of the Edges which include Concept
Notes		: if there are several edges including Concept and the
		  same relation, only one appears in List

************************************************************************/

concept_linked(CID, List, AL) :-
	findall(e(N, CID, Rel), member(e(N, CID, Rel), AL), TmpList),
	del_dup_edges(TmpList, List).

/* del_dup_edges/2 *******************************************************

Usage		: del_dup_edges(+Edges, ?NewList)
Argument(s)	: 	   	 list	  list
Description	: NewList has all members of Edges but without duplicates
Notes		: two edges are considered duplicates when they include
		  the same nodes, no matter the direction of the arrow

************************************************************************/

del_dup_edges([e(_, CID, Rel)|T], L) :-
	member(e(_, CID, Rel), T), 
	del_dup_edges(T, L).
del_dup_edges([H|T1], [H|T2]) :-
	del_dup_edges(T1, T2).	
del_dup_edges([], []) :-
	!.

/* mark_edges/3 *********************************************************

Usage		: mark_edges(+Edges, +List, -Marked)
Argument(s)	: lists	
Description	: Marked has all edges from List which include the
		  relations appearing in Edges
Notes		: 

************************************************************************/

mark_edges([e(_, _, Rel)|T], AL, Marked) :-
	relation_linked(Rel, List, AL), mark_edges(T, AL, AL2),
	conc(List, AL2, Marked).
mark_edges([], _, []) :-
	!.

/* gen_graph/1 **********************************************************

Usage		: gen_graph(+Edges)
Argument(s)	: 	     list
Description	: DCG predicate to linearize a graph given its Edges
Notes		: 

************************************************************************/

gen_graph([e(N, CID, neg-X)|T]) -->
	gen_relation(neg-X), gen_conlink([e(N, CID, neg-X)|T], _, neg-X).
gen_graph([e(N, CID, Rel)|T]) -->
	[CID], gen_rlink([e(N, CID, Rel)|T], _, CID).

/* gen_conlink/3 ********************************************************

Usage		: gen_conlink(+EdgesIn, -EdgesOut, +Relation)
Argument(s)	: 	        list       list	      term
Description	: DCG predicate to linearize the part of the graph attached 
		  to Relation
Notes		: EdgesIn/EdgesOut are the edges still unused before/after
		  this predicate has acted

************************************************************************/

gen_conlink(AL, AL, Rel) -->
	{ relation_linked(Rel, [], AL) }.
gen_conlink(AL, AL3, Rel) -->
	{ relation_linked(Rel, [Edge], AL), delete_one(Edge, AL, AL2) },
	gen_arc(Rel, CID, Edge), [CID], gen_rlink(AL2, AL3, CID).
gen_conlink(AL, AL3, Rel) -->
	{ relation_linked(Rel, EdgeList, AL), difference(AL, EdgeList, AL2) },
	[start_list], gen_conlist(AL2, AL3, EdgeList), [end_list].

/* gen_conlist/3 ********************************************************

Usage		: gen_conlist(+EdgesIn, -EdgesOut, +List)
Argument(s)	: lists
Description	: DCG predicate to process the List of concepts attached to
		  the same relation
Notes		: EdgesIn/EdgesOut are the edges still unused before/after
		  this predicate has acted

************************************************************************/

gen_conlist(AL, AL3, [Edge|T]) -->
	[nl], gen_arc(_, CID, Edge),
	[CID], gen_rlink(AL, AL2, CID), gen_conlist(AL2, AL3, T).
gen_conlist(AL, AL, []) -->
	[].

/* gen_rlink/3 **********************************************************

Usage		: gen_rlink(+EdgesIn, -EdgesOut, +Concept)
Argument(s)	: 	      list       list	    CID
Description	: DCG predicate to linearize the part of the graph attached
		  to Concept
Notes		: EdgesIn/EdgesOut are the edges still unused before/after
		  this predicate has acted

************************************************************************/

gen_rlink(AL, AL, CID) -->
	{ concept_linked(CID, [], AL) }.
gen_rlink(AL, AL3, CID) -->
	{ concept_linked(CID, [Edge], AL), delete_one(Edge, AL, AL2) },
	gen_arc(CID, Rel, Edge), gen_relation(Rel), gen_conlink(AL2, AL3, Rel).
gen_rlink(AL, AL3, CID) -->
	{ concept_linked(CID, EdgeList, AL), difference(AL, EdgeList, AL2) },
	[start_list], gen_rlist(AL2, AL3, EdgeList), [end_list].

/* gen_rlist/3 **********************************************************

Usage		: gen_rlist(+EdgesIn, -EdgesOut, +List)
Argument(s)	: lists
Description	: DCG predicate to process the List of relations attached to
		  the same concept
Notes		: EdgesIn/EdgesOut are the edges still unused before/after
		  this predicate has acted

************************************************************************/

gen_rlist(AL, AL5, [Edge|T]) -->
	[nl], 
	{ Edge = e(_, _, Rel),
	  mark_edges(T, AL, MAL), difference(AL, MAL, AL2) 
	},
	gen_relation(Rel), gen_conlink(AL2, AL3, Rel), 
	{ conc(MAL, AL3, AL4) }, gen_rlist(AL4, AL5, T).
gen_rlist(AL, AL, []) -->
	[].
	
/* gen_arc/3 ************************************************************

Usage		: gen_arc(?Node1, ?Node2, +Edge)
Argument(s)	: terms
Description	: DCG predicate to draw the arrow 
Notes		: this predicate is called once in mode -/-/+ and the
		  caller assumes that Node2 is the concept

************************************************************************/

gen_arc(Rel, CID, e(-N, CID, Rel)) --> ( { nonvar(N) }, [N] ; [] ), ['->'].
gen_arc(Rel, CID, e(+N, CID, Rel)) --> ( { nonvar(N) }, [N] ; [] ), ['<-'].
gen_arc(CID, Rel, e(+N, CID, Rel)) --> ( { nonvar(N) }, [N] ; [] ), ['->'].
gen_arc(CID, Rel, e(-N, CID, Rel)) --> ( { nonvar(N) }, [N] ; [] ), ['<-'].

/* gen_relation/1 ***********************************************************

Usage		: gen_relation(+Relation)
Argument(s)	: 	      	  term
Description	: DCG predicate to linearize a relation
Notes		: 

************************************************************************/

gen_relation(Type-_) --> 
	{ relation_type(Type, TypeName, _, _, _) },
	['(', TypeName, ')'].

/* gen_concept/3 ********************************************************

Usage		: gen_concept(+Concept, +VarIn, -VarOut)
Argument(s)	: 	      PID/CID   integer integer
Description	: DCG predicate to get the linear notation of Concept
Notes		: VarIn/VarOut is the number of variables used before/after
		  linearizing Concept

************************************************************************/

gen_concept(ID*Var, VarIn, VarOut) -->
	{ type(ID, Type) },
	['['], gen_typefield(Type, VarIn, VarOut), [':', '*', Var, ']'].
gen_concept(ID-Var, VarIn, VarOut) --> 
	{ type(ID, Type), referent(ID, Ref) },
	['['], gen_typefield(Type, VarIn, TmpVar),
	[':'], gen_reffield(ID, Ref = '*'-Var, TmpVar, VarOut), [']'].
gen_concept(ID, VarIn, VarOut) --> 
	{ referent(ID, '*'), type(ID, Type) },
	['['], gen_typefield(Type, VarIn, VarOut), [']'].
gen_concept(ID, VarIn, VarOut) --> 
	{ type(ID, Type), referent(ID, Ref) },
	['['], gen_typefield(Type, VarIn, TmpVar), 
	[':'], gen_reffield(ID, Ref, TmpVar, VarOut), [']'].

/* gen_typefield/3 ******************************************************

Usage		: gen_typefield(+Type, +VarIn, -VarOut)
Argument(s)	: 	     	 term  integer integer
Description	: DCG predicate to get the linear notation of Type
Notes		: VarIn/VarOut is the number of variables used before/after
		  linearizing Type

************************************************************************/

gen_typefield(Type, VarIn, VarIn) --> 
	{ concept_type(Type, TypeName, _, _, _) }, [TypeName].
gen_typefield(l/Id, VarIn, VarOut) --> 
	{ l(l/Id, [CID], GIDs), succ(VarIn, TmpVar), number2var(VarIn, Var) },
	['\\', Var], gen_graph(GIDs, [CID+Var], TmpVar, VarOut).
 
/* gen_reffield/4 *******************************************************

Usage		: gen_reffield(+Concept, +Referent, +VarIn, -VarOut)
Argument(s)	: 	     	CID/PID	    term    integer integer
Description	: DCG predicate to get the linear notation of Referent
Notes		: VarIn/VarOut is the number of variables used before/after
		  linearizing Referent of Concept

************************************************************************/

gen_reffield(CID, ('*') = X, VarIn, VarOut) -->		% '*' is defined as fy
	gen_reffield(CID, X, VarIn, VarOut).
gen_reffield(CID, Ref = X, VarIn, VarOut) -->
	gen_reffield(CID, Ref, VarIn, TmpVar), 
	['='], gen_reffield(CID, X, TmpVar, VarOut).
gen_reffield(_, Kind/Id, VarIn, VarIn) -->
	{ recorded(crl, Kind/Id-Var, _) }, reffield(('*') = '*'-Var).
gen_reffield(CID, _Kind/_Id, VarIn, VarOut) -->
	{ number2var(VarIn, Var), succ(VarIn, VarOut), 
	  recorda(crl, CID-Var, _) },
	reffield(('*') = '*'-Var).
gen_reffield(p/_Id, [GID|List], VarIn, VarOut) -->
	[push, nl], gen_graph([GID|List], [], VarIn, VarOut), [pop, nl].
gen_reffield(_, '*'-Var, VarIn, VarIn) -->
	reffield(('*') = '*'-Var).
gen_reffield(_, Ref, VarIn, VarIn) -->
	reffield(Ref).
	
/* write_linear/3 *******************************************************

Usage		: write_linear(+Level, +Linear, +Buffer)
Argument(s)	: 	       integer   list	  list
Description	: pretty prints the Linear notation of a graph at the
		  current indentation Level 
Notes		: Linear may contain formatting information
		  Buffer is empty or contains looked-ahead commas 

************************************************************************/

write_linear(N, [push|T], []) :-
	succ(N, N1), write_linear(N1, T, []).
write_linear(N, [pop|T], _) :-
	succ(N0, N), write_linear(N0, T, []).
write_linear(N, [nl|T], _) :-
	nl, Indent is N * 4, tab(Indent), write_linear(N, T, []).
write_linear(N, [start_list|T], []) :-
	write(' -'), write_linear(N, [push|T], []).
write_linear(N, ['='|T], []) :-
	write(' = '), write_linear(N, T, []).
write_linear(N, [','|T], []) :-
	write(', '), write_linear(N, T, []).
write_linear(N, [':'|T], _) :-
	write(': '), write_linear(N, T, []).
write_linear(N, ['(', 'NEG', ')', '->'|T], []) :-
	write('~'), write_linear(N, T, []).
write_linear(N, [I, '->'|T], []) :-
	integer(I), write(' '), write(I), write(' -> '), 
	write_linear(N, T, []).
write_linear(N, [I, '<-'|T], []) :-
	integer(I), write(' '), write(I), write(' <- '), 
	write_linear(N, T, []).
write_linear(N, ['->'|T], []) :-
	write(' -> '), write_linear(N, T, []).
write_linear(N, ['<-'|T], []) :-
	write(' <- '), write_linear(N, T, []).
write_linear(N, [end_list|T], Buf) :-
	succ(N0, N), write_linear(N0, T, [','|Buf]).
write_linear(N, [';'|T], _) :-
	write(';'), write_linear(N, [nl|T], []).
write_linear(_, ['.'], _) :-
	write('.').
write_linear(N, [H|T], []) :-
	write(H), write_linear(N, T, []).
write_linear(N, [H|T], Buf) :-
	apply(write(_), Buf), write(H), write_linear(N, T, []).
write_linear(_, [], _).