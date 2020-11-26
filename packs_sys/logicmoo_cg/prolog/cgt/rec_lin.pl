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
 
File Name	: REC_LIN.PL
Creation Date	: 90/07/08 	By: mw
Abbreviations	: mw - Michel Wermelinger 
Description	: Recognizes the linear notation of a semantic net component
Notes		: the arity of the DCG predicates doesn't include the lists
		  marked concepts have variables as referents
		  for a description of lists of marked concepts see pop_ct/2

************************************************************************/

/* HISTORY **************************************************************
 
1.0	90/07/11  mw	handles contexts and single-use types
1.1	90/08/23  mw	handles non-recursive type definitions and schemas
			uses new error handling and mark-&-sweep predicates
1.2	90/08/25  mw	supports n-adic relations
1.3	90/08/28  mw	supports compound graphs
1.4 	90/09/09  mw	generalized reffield/1; simplified some code
1.5	90/10/23  mw	added can_graph to rec_linear/2
			changed processing of referents which are names 
1.6	90/11/06  mw	changed put_arg/3 because of C-Prolog bug
1.7	90/12/28  mw    card/2 checks if integer is a word
	                can't change canonical graph anymore

************************************************************************/

/* CONTENTS *************************************************************
 
read_linear/2		builds a graph, schema or type from its linear form

************************************************************************/

/* read_linear/2 ********************************************************

Usage		: read_linear(-Kind, -Identifier)
Argument(s)	: 	       atom	term
Description	: recognizes the linear notation of a semantic net component
Notes		: the possible values for the Kind-Identifier pair are
		  graph-GID, type_def-TypeName, rel_def-RelName, schema-LID
		  and can_graph-TypeName
		  			  
************************************************************************/

read_linear(Kind, Obj) :-
	get_token(T), mark, rec_linear(Kind, Obj, T, ['.']), unmark.

/* rec_linear/2 *********************************************************

Usage		: rec_linear(-Kind, -Identifier)
Argument(s)	: 	       atom	term
Description	: DCG predicate to recognize the linear notation of something
Notes		: all data structures are created while parsing the linear
		  notation
		  the possible values for the Kind-Identifier pair are
		  graph-GID, type_def-TypeName, rel_def-RelName, schema-LID
		  			  
************************************************************************/

rec_linear(type_def, Name) --> 
	[(type)], rec_typelabel(Label), ['(', Var, ')', is], 
        { ( concept_type(_, Label, _, _, _)
		-> cg_error(dup_type_def, Label)
	         ; true
	  ) },
        rec_graph_list(GIDs, [], MC, outer),
	{ abstraction_args([Var], MC, [CID]), 
	  new_id(l/Id), assert( l(l/Id, [CID], GIDs) ),
	  label_to_name(Label, Name),
	  assert( concept_type(Name, Label, l/Id, none, []) ),
	  type(CID, Type), assert( Name << Type )
	}.
rec_linear(rel_def, Label) -->
	[relation, Label, '('], rec_param(Vars), [')', is], 
        { ( relation_type(_, Label, _, _, Args)
		-> cg_error(dup_rel_def, Label/Args)
	         ; true
	  ) },
	rec_graph_list(GIDs, [], MC, outer),
	{ abstraction_args(Vars, MC, Params), new_id(l/Id), 
	  assert( l(l/Id, Params, GIDs) ), length(Params, Arcs),
	  assert( relation_type(Label, Label, l/Id, none, Arcs) )
	}.
rec_linear(schema, l/Id) -->
	[schema, for], rec_typelabel(Label), ['(', Var, ')', is], 
	{ concept_type(_, Label, _, _, _) ; cg_error(unknown_type, Label) },
	rec_graph_list(GIDs, [], MC, outer),
	{ abstraction_args([Var], MC, [CID]), type(CID, Type), 
	  ( retract( concept_type(Type, Label, Def, Can, SL) )
	  ; cg_error(ambiguous_var, Var)
	  ),
	  new_id(l/Id), assert( l(l/Id, [CID], GIDs) ),
	  assert( concept_type(Type, Label, Def, Can, [l/Id|SL]) )
	}.
rec_linear(can_graph, Type) --> 
	[canonical, graph, for], rec_typelabel(Label), ['(', Var, ')', is], 
	{ ( concept_type(Type, Label, Def, Can, SL)
	  ; cg_error(unknown_type, Label)
	  ),
	  ( Can = none ; cg_error(dup_type_can, Label) )
	},
	rec_graph_list(GIDs, [], MC, outer),
	{ abstraction_args([Var], MC, [CID]), 
	  ( type(CID, Type) ; cg_error(ambiguous_var, Var) ),  
	  retract( concept_type(Type, Label, Def, Can, SL) ),
	  ( GIDs = [GID] ; GIDs = GID ),
	  assert( concept_type(Type, Label, Def, GID, SL) )
	}.
rec_linear(can_graph, Type) --> 
	[canonical, graph, for, Label, is], 
        { ( relation_type(_, Label, _, Can, Arcs)
	  ; cg_error(unknown_type, Label)
	  ),
	  ( Can = none ; cg_error(dup_rel_can, Label/Arcs) )
	},
	rec_graph_list(GIDs, [], _, outer),
	{ retract( relation_type(Type, Label, Def, Can, Arcs) ),
	  ( GIDs = [GID] ; GIDs = GID ),
	  assert( relation_type(Type, Label, Def, GID, Arcs) )
	}.
rec_linear(graph, GIDs) -->
	rec_graph_list(GIDs, [], _, outer). 

/* label_to_name/2 ******************************************************

Usage		: label_to_name(+Label, -Name)
Argument(s)	: atoms
Description	: builds a type Name for a given Label
Notes		: 
		  			  
************************************************************************/

label_to_name(Label, Name) :-
	name(Label, [C|T]), name('"', [C]), conc(N, [C], T), name(Name, N), !.
label_to_name(Label, Label).

/* rec_param/1 **********************************************************

Usage		: rec_param(-Variables)
Argument(s)	: 	       list
Description	: DCG predicate to recognize a list of variables (parameters)
Notes		: 
		  			  
************************************************************************/

rec_param([Var|T]) --> 
	[Var], 
	( [','], rec_param(T)
	; { T = [] }
	).

/* abstraction_args/3 ***************************************************

Usage		: abstraction_args(+Variables, +Marked, -Concepts)
Argument(s)	: lists
Description	: returns the Concepts in Marked denoted by the Variables
Notes		: 
		  			  
************************************************************************/

abstraction_args([Var|T], Marked, [CID|T1]) :-
	member(_GID-CID-Var, Marked), abstraction_args(T, Marked, T1).
abstraction_args([Var|_], _, _) :-
	cg_error(undef_param, Var).
abstraction_args([], _, []).

/* put_arg/3 ************************************************************

Usage		: put_arg(+Concept,  +N,  +Relation)
Argument(s)	: 	     CID   integer   term
Description	: Concept will be the N-th argument of Relation
Notes		: if N > 0 then the arc points to Relation, else points away
		  
************************************************************************/
:- style_check(-singleton).

put_arg(CID, -NArgs, Rel) :-
	functor(Rel, _, NArgs), arg(NArgs, Rel, Arg), var(Arg), Arg = CID.
put_arg(CID, +N, Rel) :-
	var(N), Rel =.. [_|Args], conc(Inwards, [Last], Args),
	setof(Arg, ( member(Arg, Inwards), var(Arg) ), [Arg]), 
	member(Arg, Args), var(Arg), Arg = CID.
put_arg(_, +N, Rel) :-
	var(N), cg_error(ambiguous_arc, Rel).
put_arg(_, -_, Rel) :-
	cg_error(point_into, Rel).
put_arg(_, +NArgs, Rel) :-
	nonvar(NArgs), functor(Rel, _, NArgs), cg_error(point_away, Rel).
put_arg(CID, +N, Rel) :-
	arg(N, Rel, Arg), var(Arg), Arg = CID.
/*put_arg(_, +N, Rel) :-
	nonvar(N), cg_error(duplicate_arc, N-Rel).*/
put_arg(CID, _, Rel) :-
	cg_error(too_many_arcs, Rel).

/* inv_arrow/2 **********************************************************

Usage		: inv_arrow(+Arrow, ?Inv)
Argument(s)	: terms
Description	: Inv has the opposite direction of Arrow
Notes		: 

************************************************************************/

inv_arrow(+N, -N).
inv_arrow(-N, +N).

/* rec_graph_list/4 *****************************************************

Usage		: rec_graph_list(-GIDs, +MarkedIn, -MarkedOut, +Env)
Argument(s)	: 	          list	   list	      list     term
Description	: DCG predicate to recognize the linear notation of graphs
Notes		: GIDs is the list of graphs built during parsing
		  Env is the current environment
		  MarkedIn is the list of marked concepts in other graphs
		  (may be coreference links)
		  MarkedOut is the list of marked concepts after this 
		  predicate

************************************************************************/

rec_graph_list([GID|T], MCI, MCO, Env) -->
	{ new_id(g/Id), GID = g/Id }, rec_graph(GID, MCI, TmpMC, Env), 
	{ check_graph(GID) },
	( [;], rec_graph_list(T, TmpMC, MCO, Env)
	; { T = [], MCO =  TmpMC }
	).

/* rec_graph/4 **********************************************************

Usage		: rec_graph(-Graph, +MarkedIn, -MarkedOut, +Env)
Argument(s)	: 	      GID      list   	   list	   term
Description	: DCG predicate to recognize the linear notation of Graph
Notes		: Env is the current context
		  MarkedIn is the list of marked concepts in other graphs
		  (may be coreference links)
		  MarkedOut is the list of marked concepts after this 
		  predicate

************************************************************************/

rec_graph(g/Id, MCI, MCO, Env) -->
	rec_concept(g/Id, CID, Env, MCI, TmpMC), 
	rec_rlink(g/Id, CID, TmpRL, Env, TmpMC, MCO),
	{ ind_reference(TmpRL, RL, [CID-Var], CL), assert( g(g/Id, CL, RL) ) }.
rec_graph(g/Id, MCI, MCO, Env) --> 
	rec_relation(Rel), rec_conlink(g/Id, Rel, T, Env, MCI, MCO),
	{ numbervars(Rel, 0, 0), 
	  ind_reference([Rel|T], RL, [], CL), assert( g(g/Id, CL, RL) )
	; cg_error(too_few_arcs, Rel)
	}.

/* rec_rlink/6 **********************************************************

Usage		: rec_rlink(+Graph, +Con, -Rel, +Env, +MCI, -MCO)
Argument(s)	: 	      GID    CID  list	term  list  list
Description	: DCG predicate to recognize the part of the Graph
		  attached to Con(cept), generating the Rel(ations) parsed
Notes		: Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

rec_rlink(GID, CID, [Rel|T], Env, MCI, MCO) -->
	rec_arc(A), rec_relation(Rel), 
	{ put_arg(CID, A, Rel) }, 
	rec_conlink(GID, Rel, T, Env, MCI, MCO), 
	{ numbervars(Rel, 0, 0) ; cg_error(too_few_arcs, Rel) }, !.
rec_rlink(GID, CID, RL, Env, MCI, MCO) -->
	['-'], rec_rlist(GID, CID, RL, Env, MCI, MCO),
	( [','] ; [] ), !.
rec_rlink(_, _, [], _, MCI, MCI) --> [].
				     
/* rec_rlist/6 **********************************************************

Usage		: rec_rlist(+Graph, +Con, -Rel, +Env, +MCI, -MCO)
Argument(s)	: 	      GID    CID  list	term  list  list
Description	: DCG predicate to recognize the list of Rel(ations)
		  attached to Con(cept)
Notes		: Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

rec_rlist(GID, CID, [Rel|T], Env, MCI, MCO) -->
	( ['|'] ; [] ), 
	rec_relation(Rel), rec_conlink(GID, Rel, T1, Env, MCI, MC1), 
	{ put_arg(CID, _, Rel),
	  ( numbervars(Rel, 0, 0) ; cg_error(too_few_arcs, Rel) ) 
	}, 
	rec_rlist(GID, CID, T2, Env, MC1, MCO), 
	{ conc(T1, T2, T) }.
rec_rlist(_, _, [], _, MCI, MCI) --> [].

/* rec_conlink/6 ********************************************************

Usage		: rec_conlink(+Graph, +Rel, -RL, +Env, +MCI, -MCO)
Argument(s)	: 	        GID   term  list term  list  list
Description	: DCG predicate to recognize the part of Graph attached
		  to the relation Rel
Notes		: RL is the relation list generated by this predicate
		  Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

rec_conlink(GID, Rel, RL, Env, MCI, MCO) -->
	rec_arc(A), rec_concept(GID, CID, Env, MCI, MC1), 
	{ inv_arrow(A, A1), put_arg(CID, A1, Rel) }, 
	rec_rlink(GID, CID, RL, Env, MC1, MCO), !.
rec_conlink(GID, Rel, RL, Env, MCI, MCO) -->
	['-'], rec_conlist(GID, Rel, RL, Env, MCI, MCO), 
	( [','] ; [] ), !.
rec_conlink(_, _, [], _, MCI, MCI) --> [].

/* rec_conlist/6 ********************************************************

Usage		: rec_conlist(+Graph, +Rel, -RL, +Env, +MCI, -MCO)
Argument(s)	: 	        GID   term  list term  list  list
Description	: DCG predicate to recognize the list of concepts attached
		  to relation Rel
Notes		: RL is the relation list generated by this predicate
		  Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

rec_conlist(GID, Rel, RL, Env, MCI, MCO) -->
	['|'], rec_arc(A), rec_concept(GID, CID, Env, MCI, MC1), 
	{ inv_arrow(A, A1), put_arg(CID, A1, Rel) }, 
	rec_rlink(GID, CID, L1, Env, MC1, MC2), 
	rec_conlist(GID, Rel, L2, Env, MC2, MCO),
	{ conc(L1, L2, RL) }.
rec_conlist(_, _, [], _, MCI, MCI) --> [].

/* rec_arc/1 ************************************************************

Usage		: rec_arc(-Arc)
Argument(s)	: 	  term 
Description	: DCG predicate to recognize an arrow
Notes		: 

************************************************************************/

rec_arc(+N) --> [N, -, >], { integer(N) }.
rec_arc(-N) --> [N, <, -], { integer(N) }.
rec_arc(+_) --> [-, >].
rec_arc(-_) --> [<, -].

/* rec_relation/1 *******************************************************

Usage		: rec_relation(-Relation)
Argument(s)	:		  term
Description	: DCG predicate to recognize the linear notation of Relation
Notes		: 

************************************************************************/

rec_relation(Rel) -->
	['(', Label, ')'], 
	{ relation_type(Type, Label, _, _, NArgs), functor(Rel, Type, NArgs) }.
rec_relation(Rel) -->
	['(', Label], { cg_error(unknown_rel, Label) }.

/* rec_concept/5 ********************************************************

Usage		: rec_concept(+Graph, -Concept, +Env, +MCI, -MCO)
Argument(s)	: 	        GID    PID/CID	term  list  list
Description	: DCG predicate to recognize the linear notation of Concept
Notes		: Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

rec_concept(GID, ID, Env, MCI, MCO) -->
	['['], reffield(Ref), [']'], 
	{ basic_ref(Ref, '*'),
	  process_concept(GID, ID, proposition, Ref, Env, MCI, MCO)
	}.
rec_concept(GID, ID, Env, MCI, MCO) -->
	['['], rec_typefield(Type),
	( [']'], 
	  { process_concept(GID, ID, Type, '*', Env, MCI, MCO) }
	; [:], reffield(Ref), [']'], 
	  { process_concept(GID, ID, Type, Ref, Env, MCI, MCO) }
	; [:], { new_id(p/Id), ID = p/Id },
	  rec_graph_list(GIDs, [ct|MCI], TmpMC, ID), [']'], 
	  { assert( p(ID, Type, GIDs, Env) ), pop_ct(TmpMC, MCO) }
	).
rec_concept(GID, ID, Env, MCI, MCO) -->
	['['], { new_id(p/Id), ID = p/Id },
	rec_graph_list(GIDs, [ct|MCI], TmpMC, ID), [']'],
	{ assert( p(ID, proposition, GIDs, Env) ), pop_ct(TmpMC, MCO) }.

/* pop_ct/2 *************************************************************

Usage		: pop_ct(+MCI, -MCO)
Argument(s)	: lists
Description	: predicate to pop out of a context
Notes		: MCI/MCO is the list of marked concepts before/after this
		  predicate
		  a list of marked concepts consists of GID-CID-Var terms
		  and atoms 'ct' to separate the contexts
		  the list is used as a stack (outer context at the end)

************************************************************************/

pop_ct([ct|T], T).
pop_ct([_|T], L) :- pop_ct(T, L).

/* process_concept/7 ****************************************************

Usage	    	: process_concept(+Graph, -Con, +Type, +Ref, +Env, +MCI, -MCO)
Argument(s)	: 	  	    GID	   ID    term  term  term  list  list
Description	: processes Type and Ref to obtain the concept's ID
Notes		: Env is the current context
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

process_concept(GID, ID, Type, ('*') = '*'-V, Env, MCI, MCI) :-
	member(GID-ID-V, MCI), ( type(ID, Type) ; cg_error(ambiguous_var, V) ).
process_concept(GID, ID, Type, Ref = CRL, Env, MCI, MCI) :-
	defined_concept(GID, Ref = CRL, MCI), 
	cg_error(double_def, Ref = CRL) .
process_concept(GID, PID, Type, Ref, Env, MCI, MCO) :-
	subtype(Type, proposition), new_id(p/Id), PID = p/Id, 
	process_referent(GID, PID, Type, Ref, NewRef, MCI, MCO),
	assert( p(PID, Type, NewRef, Env) ).
process_concept(GID, CID, Type, Ref, Env, MCI, MCO) :-
	new_id(c/Id), CID = c/Id, 
	process_referent(GID, CID, Type, Ref, NewRef, MCI, MCO),
	assert( c(CID, Type, NewRef) ).

/* defined_concept/3 ****************************************************

Usage	    	: defined_concept(+Graph, +Referent, +MCI)
Argument(s)	: 	  	    GID	     term    list
Description	: succeeds iff Referent denotes a concept defined in Graph
Notes		: MCI is the list of already marked concepts 

************************************************************************/

defined_concept(GID, Ref = '*'-Var, MC) :- 
	member(GID-ID-Var, MC).
defined_concept(GID, Ref = '*'-Var, MC) :-
	defined(GID, Ref, MC).
defined_concept(GID, '*'-Var, MC) :- 
	member(GID-ID-Var, MC).

/* process_referent/7 ***************************************************

Usage	    	: process_referent(
			+Graph, +Con, +Type, +Ref, -NewRef, +MCI, -MCO)
Argument(s)	: 	  GID	 ID    term  term    term   list  list
Description	: processes Type and Ref to obtain the Con(cept)'s NewRef
Notes		: Ref contains unresolved coreference links
		  MCI/MCO is the list of marked concepts before/after this
		  predicate

************************************************************************/

process_referent(GID, ID, Type, Ref = '*'-V, NewRef = ID2, MCI, MCO) :-
	member(GID2-ID2-V, MCI), type(ID2, Type2),
	( subtype(Type, Type2) ; subtype(Type2, Type) ; cg_error(wrong_crl, V) ),
	( retract( c(ID2, Type2, Ref2) ), 
	  assert( c(ID2, Type2, Ref2 = ID) )
	; retract( p(ID2, Type2, Ref2, Env2) ),
	  assert( p(ID2, Type2, Ref2 = ID, Env2) )		    
	), process_referent(GID, ID, Type, Ref, NewRef, MCI, MCO).
process_referent(GID, ID, Type, Ref = '*'-V, NewRef, MCI, [GID-ID-V|MCO]) :-
	process_referent(GID, ID, Type, Ref, NewRef, MCI, MCO).
/*process_referent(GID, ID, Type, '*'-V, ('*') = ID2, MCI, MCI) :- 
	member(GID2-ID2-V, MCI), type(ID2, Type2),
	( subtype(Type, Type2) ; subtype(Type2, Type) ; cg_error(wrong_crl, V) ),
	( retract( c(ID2, Type2, Ref2) ), 
	  assert( c(ID2, Type2, Ref2 = ID) )
	; retract( p(ID2, Type2, Ref2, Env2) ),
	  assert( p(ID2, Type2, Ref2 = ID, Env2) )
	).
process_referent(GID, ID, Type, '*'-V, '*', MCI, [GID-ID-V|MCI]). */
process_referent(_GID, _ID, _Type, Ref, Ref, MCI, MCI).

/* rec_typefield/1 ******************************************************

Usage		: rec_typefield(-Type)
Argument(s)	: 	   	 term
Description	: DCG predicate to recognize the Type of a concept
Notes		: 

************************************************************************/

rec_typefield(l/Id) -->
	['\\', Var], rec_graph_list(GIDs, [], MC, _), 
	{ member(GID-CID-Var, MC), new_id(l/Id), assert( l(l/Id, [CID], GIDs) )
	; cg_error(undef_param, Var)
	}.
rec_typefield(Type) -->
	rec_typelabel(Label), { concept_type(Type, Label, _, _, _) }.
/*
rec_typefield(Type) -->
        ['"', TypeName, '"'],
        { name('"', [C]), name(TypeName, L1), conc([C|L1], [C], L2),
	  name(Label, L2), concept_type(Type, Label, _, _, _)
	}.
rec_typefield(Type) -->
	[TypeName], 
	{ concept_type(Type, TypeName, _, _, _) }.
*/
rec_typefield(Type) -->
	rec_typelabel(TypeName), %[TypeName], 
	{ name(TypeName, [L|_]), letter(L), cg_error(unknown_type, TypeName) }.

/* rec_typelabel/1 ******************************************************

Usage		: rec_typelabel(-Label)
Argument(s)	: 	   	 atom
Description	: DCG predicate to recognize the Label of a type
Notes		: 

************************************************************************/

rec_typelabel(Label) -->
	['"', TypeName, '"'],
        { name('"', [C]), name(TypeName, L1), conc([C|L1], [C], L2),
	  name(Label, L2) }.
rec_typelabel(Label) -->
	[Label].

/* reffield/1 ***********************************************************

Usage		: reffield(?Ref)
Argument(s)	: 	   term
Description	: DCG predicate to process the (multiple) referent(s) 
		  of a concept
Notes		: this predicate is bidirectional

************************************************************************/

reffield(Ref) --> { var(Ref) }, ['*', Var], coref(('*') = '*'-Var, Ref).
reffield(Ref) --> { var(Ref) }, referent(B), coref(B, Ref).

reffield(('*')='*'-Var) --> ['*', Var].
reffield(Ref = '*'-Var) --> { nonvar(Ref) }, reffield(Ref), [=, '*', Var].
reffield(Ref)		--> referent(Ref).

coref(R, R) --> [].
coref(B, R) --> [=, '*', Var], coref(B = '*'-Var, R).

/* referent/1 ***********************************************************

Usage		: referent(?Ref)
Argument(s)	: 	   term
Description	: DCG predicate to process a single referent of a concept
Notes		: this predicate is bidirectional

************************************************************************/

referent(set(dist, L, C)) --> ['Dist', '{'], set(L), ['}'], card(C).
referent(set(resp, L, C)) --> ['Resp', '<'], set(L), ['>'], card(C).
referent(set(coll, L, C)) -->         ['{'], set(L), ['}'], card(C).
referent(set(disj, L, C)) -->     ['{'], disj_set(L), ['}'], card(C).
referent(meas(M))    	  --> ['@'], set_element(name(M)).
referent(every)	     	  --> ['V'].
referent(R)	    	  --> set_element(R).

/* set_element/1 ********************************************************

Usage		: set_element(?Ref)
Argument(s)	: 	      term
Description	: DCG predicate to process those referents which can appear
		  in a set
Notes		: this predicate is bidirectional

************************************************************************/

set_element('*')  	--> ['*'].
set_element(I)    	--> ['#', I], { integer(I) }.
set_element('#')  	--> ['#'].
set_element(name(Name))	--> 
	[Name], { subtype(Name, word) /*; cg_error(inv_name, Name) */}. 

/* set/1 ****************************************************************

Usage		: set(?Set)
Argument(s)	:     list
Description	: DCG predicate to process a referent which is a set
Notes		: this predicate is bidirectional

************************************************************************/

set([R])   --> set_element(R).
set([H|T]) --> set_element(H), [','], set(T).

/* disj_set/1 ***********************************************************

Usage		: disj_set(?Set)
Argument(s)	: 	   list
Description	: DCG predicate to process a disjunctive set
Notes		: this predicate is bidirectional

************************************************************************/

disj_set([R])   --> set_element(R).
disj_set([H|T]) --> set_element(H), ['|'], disj_set(T).

/* card/1 ***************************************************************

Usage		: card(?Cardinality)
Argument(s)	: 	 integer
Description	: DCG predicate to process the Cardinality of a set
Notes		: this predicate is bidirectional

************************************************************************/

card(C) --> ['@', C], { integer(C), concept_type(C, _, _, _, _) }.
card(X) --> [], { var(X) }.
