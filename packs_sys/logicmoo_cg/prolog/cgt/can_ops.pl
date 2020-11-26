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
 
File Name	: CAN_OPS.PL
Creation Date	: 90/06/16 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Implements the canonical formation rules
 
************************************************************************/

/* HISTORY **************************************************************

0.0	90/06/24  mw	works only for graphs without coreferent links
0.1	90/07/01  mw	single-use types added
0.2	90/07/17  mw	restrict/3 works for contexts
			bugs in copy_type/2 corrected
0.3	90/08/19  mw	max_join/2 added; bug in simplify/1 corrected
			copy/3 simplified
0.4	90/08/29  mw	copy/3 supports compound graphs
0.5	90/09/03  mw	compound graphs supported; simplified more code
			join_graphs_on/4 and join_concept/2 added
			copy/3 renamed to copy_graph/3
0.6	90/10/17  mw	extend_join/4 added
0.61	90/10/19  mw	delete_obj/1 in join_on/4 changed to delete_concepts/1
			corrected bug in conform/2
0.62	90/10/22  mw	debugging
0.7	90/10/26  mw	added is_generalization/2 and is_specialization/2
0.8	90/11/05  mw	match_concept/4 and join_graphs_on/4 deterministic
0.9	90/11/08  mw	call to c/3 and p/4 in match_concept/4 removed
1.0	90/12/12  mw    debugged first clause of join_on/4  

************************************************************************/
 
/* CONTENTS *************************************************************

join_graphs_on/4	joins arbitrary graphs on arbitrary concepts
join_on/4		joins two given graphs on the given concepts
max_join/3		maximally joins two graphs
extend_join/4		extends maximally a join of two concepts
join_graph/2	  	joins two given graphs on one concept
join_concept/2		tries to join two given concepts 
simplify/1		eliminates duplicate relations
restrict/3		restricts a concept
copy_graph/3 		copies a graph in a given context 
copy_parameter/4	finds the abstraction's copy corresponding parameters

is_generalization/2	implements the definition of generalization
is_specialization/2	implements the definition of specialization

************************************************************************/

/* join_graphs_on/4 *****************************************************

Usage		: join_graphs_on(+Graphs, +Concepts1, +Concepts2, -NewGraphs)
Argument(s)	: lists
Description	: joins Concepts1 and Concepts2 obtaining NewGraphs
Notes		: assumes that the concepts are already restricted and
		  the join is thus possible 

************************************************************************/

join_graphs_on(OldGIDs, [ID1|T1], [ID2|T2], NewGIDs) :-
	which_graph(ID1, OldGIDs, G1), which_graph(ID2, OldGIDs, G2),
        join_on(G1, G2, [ID1-X], [ID2-X]),
	delete_one(G2, OldGIDs, TmpGIDs), 
	join_graphs_on(TmpGIDs, T1, T2, NewGIDs), !.
join_graphs_on(GIDs, [], [], GIDs).

/* join_on/4 ************************************************************

Usage		: join_on(+Graph1, +Graph2, +Concepts1, +Concepts2)
Argument(s)	: 	    GID	     GID       list 	   list
Description	: joins Graph1 and Graph2 on Concepts1 and Concepts2
Notes		: assumes that the concepts are already restricted and
		  the join is thus possible 

************************************************************************/

join_on(CG, CG, Cs1, Cs2) :-
	retract( g(CG, CL, RL) ), subset(Cs1, CL), difference(CL, Cs2, CL2),
	assert( g(CG, CL2, RL) ), shallow_delete(Cs2), !.
join_on(CG1, CG2, Cs1, Cs2) :-
	retract( g(CG1, CL1, RL1) ), retract( g(CG2, CL2, RL2) ), free_id(CG2),
	subset(Cs1, CL1), difference(CL2, Cs2, Tmp),
	conc(RL1, RL2, RL3), conc(CL1, Tmp, CL3),
	assert( g(CG1, CL3, RL3) ), shallow_delete(Cs2), !.	
	
/************************************************************************

			A S S U M P T I O N   3 . 5 . 9

************************************************************************/
 
/* max_join/3 ***********************************************************

Usage		: max_join(+Graph1, +Graph2, -NewGraph)
Argument(s)	:     	      GID      GID	GID
Description	: succeeds iff Graph1 and Graph2 can be maximally joined
Notes		: 

************************************************************************/

max_join(GIDs1, GIDs2, NewGIDs) :-
	member(G1, GIDs1), member(G2, GIDs2), max_join(G1, G2, G1),
	delete_one(G2, GIDs2, TmpGIDs), max_join(GIDs1, TmpGIDs, NewGIDs).
max_join(GIDs, [], GIDs).
max_join(CG1, CG2, CG1) :-
	g(CG1, CL1, _RL1), g(CG2, CL2, _RL2), 
	member(C1-_, CL1), member(C2-_, CL2), join_concept(C1, C2),
	extend_join(CG1, CG2, C1, C2).

/* extend_join/4 ********************************************************

Usage		: extend_join(+Graph1, +Graph2, +Concept1, +Concept2)
Argument(s)	:     	        GID      GID	 CID/PID    CID/PID
Description	: extends maximally the join of Concept1 of Graph1 with
		  Concept2 of Graph2 
Notes		: succeeds always 

************************************************************************/

extend_join(CG1, CG2, C1, C2) :-
	g(CG1, CL1, RL1), dir_reference(CL1, RL1), map(_ =.. _, RL1, Rel1),
	g(CG2, CL2, RL2), dir_reference(CL2, RL2), map(_ =.. _, RL2, Rel2),
	matched_concepts([C1-C2], Rel1, Rel2, MCL1, MCL2),
	join_on(CG1, CG2, MCL1, MCL2), simplify(CG1), !.

/* matched_concepts/5 ***************************************************

Usage		: matched_concepts(+Matched, +Rel1, +Rel2, -Conc1, -Conc2)
Argument(s)	: lists
Description	: returns the concept lists Conc1 and Conc2 on which to join
Notes		: Matched is a list of terms CID1-CID2 denoting the concepts
			which are known to match
		  Rel1 and Rel2 contain the relations of the two graphs in
			list form (head = relation type, tail = CIDs)

************************************************************************/

matched_concepts([C1-C2|T], RL1, RL2, L1, L2) :-	
	member([Rel|Args1], RL1), nth_member(C1, Args1, N),
	member([Rel|Args2], RL2), nth_member(C2, Args2, N),
	match_args(Args1, Args2, [C1-C2|T], Matches),
	delete_one([Rel|Args1], RL1, RL11),
	delete_one([Rel|Args2], RL2, RL21),
	matched_concepts(Matches, RL11, RL21, L1, L2).
matched_concepts([C1-C2|T], RL1, RL2, [C1-Var|T1], [C2-Var|T2]) :-	
	matched_concepts(T, RL1, RL2, T1, T2).
matched_concepts([], _, _, [], []).

/* match_args/4 *********************************************************

Usage		: match_args(+CIDList1, +CIDList2, +OldMatches, -NewMatches)
Argument(s)	: lists
Description	: succeeds iff all concepts in CIDList1 and CIDList2 match
		  respectively
Notes		: OldMatches/NewMatches is a list of terms CID1-CID2 denoting
		  the concepts known to match before/after this predicate

************************************************************************/

match_args([ID1|T1], [ID2|T2], OldMatches, NewMatches) :-
	member(ID1-ID2, OldMatches), !,
	match_args(T1, T2, OldMatches, NewMatches).
match_args([ID1|T1], [ID2|T2], OldMatches, [ID1-ID2|NewMatches]) :-
	match_concept(ID1, ID2, Type, Ref),
	match_args(T1, T2, OldMatches, NewMatches),
	restrict(ID1, Type, Ref), referent(ID2, Referent),
	update_crl(ID2, ID1, Referent).
match_args([], [], Matches, Matches).

/************************************************************************

			A S S U M P T I O N   3 . 4 . 3

************************************************************************/
 
/* join_graph/3 *********************************************************

Usage		: join_graph(+Graph1, +Graph2, -NewGraph)
Argument(s)	: GIDs
Description	: succeeds iff Graph1 and Graph2 can be joined on one concept
Notes		: 

************************************************************************/

join_graph(GIDs1, GIDs2, NewGIDs) :-
	member(G1, GIDs1), member(G2, GIDs2), join_graph(G1, G2, G1),
	delete_one(G2, GIDs2, TmpGIDs), conc(GIDs1, TmpGIDs, NewGIDs), !.
join_graph(CG1, CG2, CG1) :-
	g(CG1, CL1, _), g(CG2, CL2, _),
	member(C1-_, CL1), member(C2-_, CL2), join_concept(C1, C2),
	join_on(CG1, CG2, [C1-Var], [C2-Var]).

/* join_concept/2 *******************************************************

Usage		: join_concept(+Concept1, +Concept2)
Argument(s)	: 	 	CID/PID	   CID/PID
Description	: succeeds iff Concept1 and Concept2 were joined together
Notes		: 

************************************************************************/

join_concept(ID1, ID2) :-
	match_concept(ID1, ID2, Type, Ref), restrict(ID1, Type, Ref),
	referent(ID2, Referent), update_crl(ID2, ID1, Referent), !.

/* match_concept/4 ******************************************************

Usage		: match_concept(+Concept1, +Concept2, -Type, -Referent)
Argument(s)	: 	 	 CID/PID     CID/PID
Description	: succeeds iff Concept1 and Concept2 match on Type and Referent
Notes		: 

************************************************************************/

match_concept(c/C1, c/C2, ST, Ref) :-
	type(c/C1, T1), referent(c/C1, Ref1),
	type(c/C2, T2), referent(c/C2, Ref2),
	match_referent(Ref1, Ref2, Ref),
	max_common_subtype(T1, T2, ST),
	( ST = absurd, !, fail 
	; conform(ST, Ref), !
	).
match_concept(p/P1, p/P2, ST, Ref) :-
	type(p/P1, T1), referent(p/P1, Ref1),
	type(p/P2, T2), referent(p/P2, Ref2),
	match_referent(Ref1, Ref2, Ref),
	max_common_subtype(T1, T2, ST), !.

/* match_referent/3 *****************************************************

Usage		: match_referent(+Concept1, +Concept2, -Type, -Referent)
Argument(s)	: 	 	  CID/PID     CID/PID
Description	: succeeds iff Concept1 and Concept2 match on Type and Referent
Notes		: 

************************************************************************/

match_referent(X, X, X).
match_referent('*', X, X).
match_referent(X, '*', X).
match_referent(A, B = CRL, C = CRL) :-
	match_referent(A, B, C).
match_referent(A = CRL, B, C = CRL) :-
	match_referent(A, B, C).
match_referent(A, B, set(coll, [A, B], 2)) :-		% set coercion and join
	set_element(A, _, _), set_element(B, _, _).
match_referent(set(Kind, S1, C), set(Kind, S2, C), set(Kind, S3, C)) :-
	union(S1, S2, S3).
	
/************************************************************************

			A S S U M P T I O N   3 . 3 . 3

************************************************************************/
 
/* conform/2 ************************************************************

Usage		: conform(+Type, +Referent)
Argument(s)	: 	   atom	    term
Description	: succeeds iff Referent conforms to Type
Notes		: Referent may be a list of referents

************************************************************************/

conform(Type, [Ref]) :- 
	conform(Type, Ref), !.
conform(Type, [Ref|List]) :- 
	conform(Type, Ref), conform(Type, List), !.
conform(universal, _) :- !.
conform(absurd, _) :- 
	!, fail.
conform(_, '*') :- !.
conform(_, #) :- !.
conform(_, every) :- !.
conform(Type, Ref = _CRL) :- 
	conform(Type, Ref), !.
conform(Type, set(_, Set, _)) :- 
	conform(Type, Set), !.
conform(Type, Ref) :- 
	c(_, Type, Ref), !.
conform(Type, Ref) :- 
	proper_subtype(SubType, Type), conform(SubType, Ref).
conform(Type, Ref) :- 
	conform(Type1, Ref), conform(Type2, Ref), Type1 \= Type2,
	max_common_subtype(Type1, Type2, Type).

/* simplify/1 ***********************************************************

Usage		: simplify(+Graph)
Argument(s)	: 	     GID
Description	: deletes all duplicate relations of Graph
Notes		: succeeds always

************************************************************************/

simplify([Graph|List]) :- 
        apply(simplify(_), [Graph|List]).
simplify(CG) :-
	g(CG, CL, RL), delete_eq(RL, RL2),
	( RL == RL2
	; retract( g(CG, _, _) ), assert( g(CG, CL, RL2) )
	).

/* restrict/3 ***********************************************************

Usage		: restrict(+Concept, +Type, +Referent)
Argument(s)	: 	      CID     type   referent
Description	: restricts Concept to have the given Type and Referent
Notes		: doesn't check the conformity of Type and Referent

************************************************************************/

restrict(CID, Type, Ref) :-
	type(CID, Type), referent(CID, Ref), !.
restrict(CID, Type, Ref) :-
	retract( c(CID, _, _) ), assert( c(CID, Type, Ref) ), !.
restrict(PID, Type, Ref) :-
	retract( p(PID, _, _, Env) ), assert( p(PID, Type, Ref, Env) ), !.

/* update_crl/3 *********************************************************

Usage		: update_crl(+OldCRL, +NewCRL, +Referent)
Argument(s)	: 	     CID/PID  CID/PID     term
Description	: updates OldCRL to NewCRL in all concepts pointed by Referent
Notes		: 

************************************************************************/

update_crl(OldCRL, NewCRL, Ref = CID) :-
	retract( c(CID, Type, OldRef) ),
	change_ref(OldCRL, OldRef, NewCRL, NewRef),
	assert( c(CID, Type, NewRef) ),
	update_crl(OldCRL, NewCRL, Ref).
update_crl(OldCRL, NewCRL, Ref = PID) :-
	retract( p(PID, Type, OldRef, Env) ),
	change_ref(OldCRL, OldRef, NewCRL, NewRef),
	assert( p(PID, Type, NewRef, Env) ),
	update_crl(OldCRL, NewCRL, Ref).
update_crl(_, _, _).

/* copy_graph/3 *********************************************************

Usage		: copy_graph(+Graph, -Copy, +Environment)
Argument(s)	: 	       GID    GID	 PID
Description	: copies Graph in Environment and returns the Copy's GID
Notes		: if Graph is in the outer context, Environment is the
		  atom 'outer'
		  Graph and Copy are lists if they are compound graphs

************************************************************************/

copy_graph([GID], [NewGID], Env) :-
        copy_graph(GID, NewGID, Env), !.
copy_graph([GID|List], [NewGID|NewList], Env) :-
	copy_graph(GID, NewGID, Env), copy_graph(List, NewList, Env), !.
copy_graph(CG1, g/G2, Env) :-
	g(CG1, CL, RL), new_id(g/G2), 
	map(copy_concept(_, _, Env), CL, CL2),
	assert( g(g/G2, CL2, RL) ), !.

/* copy_concept/3 *******************************************************

Usage		: copy_concept(+Concept1, -Concept2, +Env)
Argument(s)	: 	  	CID/PID    CID/PID    PID
Description	: copies Concept1 to Concept2 
Notes		: Env is the context of Concept2

************************************************************************/

copy_concept(X/C1-Var, X/C2-Var, Env) :-
	type(X/C1, Type1), referent(X/C1, Ref1), new_id(X/C2), 
	( copy_abstraction(Type1, Type2) ; Type1 = Type2 ),
	copy_ref(X/C1, X/C2, Ref1, Ref2),
	( X = p -> assert( p(X/C2, Type2, Ref2, Env) )
		;  assert( c(X/C2, Type2, Ref2) )
	).

/* copy_abstraction/2 ***************************************************

Usage		: copy_abstraction(+Abstraction1, -Abstraction2)
Argument(s)	: LIDs
Description	: copies Abstraction1 to Abstraction2
Notes		:

************************************************************************/

copy_abstraction(l/L1, l/L2) :-
	l(l/L1, CIDs, GIDs), copy_graph(GIDs, NewGIDs, outer),
	map(copy_parameter(_, _, GIDs, NewGIDs), CIDs, NewCIDs),
	new_id(l/L2), assert( l(l/L2, NewCIDs, NewGIDs) ).

/* copy_parameter/4 *****************************************************

Usage		: copy_parameter(+Param1, -Param2, +OldGraph, +NewGraph)
Argument(s)	: 		 PID/CID  PID/CID     list	 list
Description	: returns the Param2 of NewGraph corresponding 
		  to Param1 of OldGraph
Notes		:

************************************************************************/

copy_parameter(ID1, ID2, G1s, G2s) :-
	nth_member(CG1, G1s, N), g(CG1, CL1, _), nth_member(ID1-_, CL1, M),
	nth_member(CG2, G2s, N), g(CG2, CL2, _), nth_member(ID2-_, CL2, M), !.

/* copy_ref/4 ***********************************************************

Usage		: copy_ref(+OldID, +NewID, +Ref1, -Ref2)
Argument(s)	: terms
Description	: copies Ref1 to Ref2
Notes		: OldID/NewID is the ID of the original/duplicate concept

************************************************************************/

copy_ref(Old, New, Ref = CRL, NewRef = NewCRL) :-
	copy_ref(Old, New, CRL, NewCRL), copy_ref(Old, New, Ref, NewRef).
copy_ref(Old, New, Ref = _, NewRef) :-
	copy_ref(Old, New, Ref, NewRef).
copy_ref(Old, _New, ID, _) :-
	recorded(crl, _ = ID, DbRef), erase(DbRef),
	( retract( c(Old, Type, OldRef) ),
	  change_ref(ID, OldRef, none, NewRef),
	  assert( c(Old, Type, NewRef) )
	; retract( p(Old, Type, OldRef, Env) ),
	  change_ref(ID, OldRef, none, NewRef),
	  assert( p(Old, Type, NewRef, Env) )
	), !, fail.
copy_ref(Old, New, ID, NewID) :-
	recorded(crl, ID = NewID, DbRef), erase(DbRef),
	( retract( c(NewID, Type, OldRef) ),
	  change_ref(Old, OldRef, New, NewRef),
	  assert( c(NewID, Type, NewRef) )
	; retract( p(NewID, Type, OldRef, Env) ), 
	  change_ref(Old, OldRef, New, NewRef),
	  assert( p(NewID, Type, NewRef, Env) )
	).
copy_ref(Old, New, c/ID, c/ID) :-
	recorda(crl, Old = New, _), recorda(crl, Old = New, _),
	retract( c(c/ID, Type, Ref) ), assert( c(c/ID, Type, Ref = New) ).
copy_ref(Old, New, p/ID, p/ID) :-
	recorda(crl, Old = New, _), recorda(crl, Old = New, _),
	retract( p(p/ID, Type, Ref, Env) ), 
	assert( p(p/ID, Type, Ref = New, Env) ).
/*copy_ref(Old, New, X/ID, _) :-
	recorda(crl, Old = New, _), !, fail.*/
copy_ref(_, ID2, [GID|List], NewGIDList) :-
	copy_graph([GID|List], NewGIDList, ID2).
copy_ref(_, _, X, X).

/************************************************************************

			D E F I N I T I O N   3 . 5 . 1

************************************************************************/

/* is_specialization/2 **************************************************

Usage		: is_specialization(+Graph1, +Graph2)
Argument(s)	: GIDs
Description	: succeeds iff Graph1 <= Graph2
Notes		: deterministic

************************************************************************/

is_specialization(G1, G2) :-
	is_generalization(G2, G1).

/* is_generalization/2 **************************************************

Usage		: is_generalization(+Graph1, +Graph2)
Argument(s)	: GIDs
Description	: succeeds iff Graph1 >= Graph2
Notes		: deterministic

************************************************************************/

is_generalization(G1, G2) :-
	mark,
	copy_graph(G1, Tmp1, outer), copy_graph(G2, Tmp2, outer),
	( max_join(Tmp1, Tmp2, Tmp1), is_copy(Tmp1, G2), sweep, !
	; sweep, !, fail
	).
