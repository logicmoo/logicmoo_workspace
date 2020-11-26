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
 
File Name	: MISC.PL
Creation Date	: 90/06/16 	By: mw
Abbreviations	: mw - Michel Wermelinger
Description	: Implements miscellaneous operations on conceptual graphs
 
************************************************************************/

/* HISTORY **************************************************************

0.0	90/06/23  mw	show_obj/1, _id predicates
0.1	90/08/23  mw	mark-&-sweep memory management
			error handling predicates
0.2	90/09/09  mw	added quantity, name and measure expansion/contraction
			debugged mark-&-sweep predicates
0.3	90/10/19  mw	added delete_concepts/1
0.4	90/10/23  mw	added cg_warning/1, new_type/3 and _db predicates
0.5	90/10/29  mw	improved mark-&-sweep predicates
0.6	90/11/04  mw	generalized delete_concepts/1 to shallow_delete/1
			added sweep_all/0; debugged sweep/0
0.7	90/11/05  mw	show_obj/1 now deterministic; added take_crl/2
0.71	90/11/07  mw	put_crl/2 deterministic
0.72	90/11/08  mw	referent/2 deterministic
			changed direct calls to c/3
0.8	90/12/05  mw    changed some predicates to work with CGE
			( heads of contraction preedicates )
0.81	90/12/06  mw    added same_context/2
0.9	90/12/10  mw    added description file to save_db and load_db
1.0	90/12/13  mw    added notion of current database
1.1	91/05/02  mw	free_id/1 takes into account that an object may have 
			been marked in a mark level superior to the current one;
			sweep/0 now also deletes the level mark
1.2	91/05/14  mw	free_id/1 now ignores at what level an object was marked
1.21	92/04/23  mw	added comments
1.22	92/05/05  mw	augmented copyright notice; 
			'lexicon' isn't reconsulted by start_cgp/1 anymore

************************************************************************/
 
/* CONTENTS *************************************************************

start_cgp/1		start processing conceptual graphs
end_cgp/1		stop processing conceptual graphs
clear_db/0		clears current database
current_db/1		returns current database name
load_db/1		loads a graph database
save_db/1		saves current database

mark/0			starts a new level to mark objects created henceforth
unmark/0		unmarks all objects created in current level
sweep/0			deletes all object marked in current level
new_id/1		returns a new unique identifier for the created object
free_id/1		makes an identifier available again
shallow_delete/1	deletes an object but not its sub-components
delete_obj/1		deletes an object (graph, abstraction or concept)

number2var/2		given N returns Nth variable (a, b, ..., z, aa, ab, ...)
dir_reference/2		makes relations refer directly to attached concepts
ind_reference/4		makes relations refer indirectly to attached concepts
new_type/3		creates a new concept or relation type

referent/2		returns referent of given concept (Assumption 3.3.1)
basic_ref/2		returns referent of given concept without coref. links
change_ref/2		changes a referent or coreference link
put_crl/2		creates a coreference link between two given concepts
take_crl/2		removes coreference link between two given concepts

meas_expansion/2	measure expansion
meas_contraction/2	measure contraction
qty_expansion/2		quantity expansion
qty_contraction/2	quantity contraction
name_expansion/2	name expansion
name_contraction/2	name contraction
del_univ_quant/5	universal quantifier expansion

which_graph/3		returns graph containing a given concept
which_context/2		returns deepest context containing given graph
same_context/2		succeeds if given graphs are all in the same context

check_graph/1		checks if given graph is well defined

cg_error/2		outputs an error message

************************************************************************/

/************************************************************************

		D A T A B A S E   O P E R A T I O N S 

************************************************************************/
 
/* start_cgp/1 **********************************************************

Usage		: start_cgp(+Canon)
Argument(s)	: 	     atom
Description	: starts a session loading the database from Canon
Notes		: 

************************************************************************/

start_cgp(Canon) :-
    clear_db, load_id, load_db(Canon), nl, nl,
    write('Conceptual Graph Tools (CGT) version 1.0'), nl,
    write('Copyright (C) 1990 Miguel Alexandre Wermelinger'), nl, nl,
    write('CGT comes with ABSOLUTELY NO WARRANTY.'), nl,
    write('This is free software, and you are welcome to redestribute it'),
    nl, 
    write('according to the GNU General Public License (version 2 or later);'),
    nl,
    write('see the acompanying file "COPYING".'), nl, nl,
    write('If you have not received it, contact the Free Software'), nl,
    write('Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA'), nl, 
    write('or'), nl,
    write('Michel Wermelinger, Dep. de Informatica, Univ. Nova de Lisboa,'),
    nl,
    write('Quinta da Torre, 2825 Monte da Caparica, PORTUGAL'), nl,
    write('E-mail: mw@fct.unl.pt'), nl, nl.


/* end_cgp/1 ************************************************************

Usage		: end_cgp(+Canon)
Argument(s)	: 	    atom
Description	: ends the session saving the current database to Canon
Notes		: 

************************************************************************/

end_cgp(Canon) :-
	save_id, save_db(Canon), clear_db.
	
/* clear_db/0 ***********************************************************

Usage		: clear_db
Argument(s)	: 
Description	: clears the current database
Notes		: succeeds always

************************************************************************/

clear_db :-
	abolish(g/3), abolish(c/3), abolish(p/4), abolish(l/3), sweep_all,
	member(Key, [c,g,p,l]), recorded(Key, _, Ref), erase(Ref), fail.
clear_db.

/* current_db/1 *********************************************************

Usage		: current_db(-Canon)
Argument(s)	: 	      atom
Description	: returns the name of the current database
Notes		: 

************************************************************************/

current_db(Canon) :-
        recorded(get_current_db, Canon, _).

/* load_db/1 ************************************************************

Usage		: load_db(+Canon)
Argument(s)	: 	    atom
Description	: loads the current database from the given Canon
Notes		: 

************************************************************************/


load_db_dir(Canon,CanonFull):- 
  absolute_file_name(library(cgt/Canon),File,[extensions([gr]),access(read)]),
  atom_concat(CanonFull,'.gr',File),!.
load_db(Canon0) :-
        load_db_dir(Canon0,Canon),
	name(Canon, L),
	name('.gr',  E1), conc(L, E1, L1), name(File1, L1), reconsult(File1),
	name('.hrc', E2), conc(L, E2, L2), name(File2, L2), reconsult(File2),
	name('.rty', E3), conc(L, E3, L3), name(File3, L3), reconsult(File3),
	name('.cty', E4), conc(L, E4, L4), name(File4, L4), reconsult(File4),
	name('.dsc', E5), conc(L, E5, L5), name(File5, L5), reconsult(File5),
        ( recorded(get_current_db, _, Ref), erase(Ref) ; true ),
        recorda(get_current_db, Canon, _).

% the following clause is to be used iff CGE is available
load_db(Canon) :-
        acknowledge([Canon, ' is not a valid GET database!']), !, fail.

/* the following clause is to be used iff CGE isn't available
load_db(Canon) :-
        nl, write(Canon), write(' is not a valid GET database!'), nl, !, fail.
*/

/* save_db/1 ************************************************************

Usage		: save_db(+Canon)
Argument(s)	: 	    atom
Description	: saves the current database to the given Canon
Notes		: 

************************************************************************/

save_db(Canon) :- 
	telling(Old), told, 
	save_gr(Canon), save_cty(Canon), save_rty(Canon), 
        save_hrc(Canon), save_dsc(Canon),
	tell(Old), save_id, recorded(get_current_db, OldDb, Ref),
        ( OldDb = Canon
	; erase(Ref), recorda(get_current_db, Canon, _)
	).

/* save_gr/1 ************************************************************

Usage		: save_gr(+Canon)
Argument(s)	: 	    atom
Description	: saves the currently stored graphs to the given Canon
Notes		: 

************************************************************************/

save_gr(Canon) :-
	name(Canon, L1), name('.gr', L2), conc(L1, L2, L3), name(File, L3),
	tell(File), write(':- dynamic g/3, c/3, p/4, l/3.'), nl,
	g(ID, C, R), portray_clause( g(ID, C, R) ), fail.
save_gr(_) :-
	c(ID, T, R), portray_clause( c(ID, T, R) ), fail.
save_gr(_) :-
	p(ID, T, R, E), portray_clause( p(ID, T, R, E) ), fail.
save_gr(_) :-
	l(ID, C, G), portray_clause( l(ID, C, G) ), fail.
save_gr(_) :- told.

/* save_cty/1 ***********************************************************

Usage		: save_cty(+Canon)
Argument(s)	: 	    atom
Description	: saves the current concept types to the given Canon
Notes		: 

************************************************************************/

save_cty(Canon) :-
	name(Canon, L1), name('.cty', L2), conc(L1, L2, L3), name(File, L3),
	tell(File), write(':- dynamic concept_type/5.'), nl,
	concept_type(T, L, D, C, S), 
        portray_clause( concept_type(T, L, D, C, S) ),
	fail.
save_cty(_) :- told.

/* save_rty/1 ***********************************************************

Usage		: save_rty(+Canon)
Argument(s)	: 	    atom
Description	: saves the current relation types to the given Canon
Notes		: 

************************************************************************/

save_rty(Canon) :-
	name(Canon, L1), name('.rty', L2), conc(L1, L2, L3), name(File, L3),
	tell(File), write(':- dynamic relation_type/5.'), nl,
	relation_type(T, L, D, C, S), 
        portray_clause( relation_type(T, L, D, C, S) ), fail.
save_rty(_) :- told.

/* save_hrc/1 ***********************************************************

Usage		: save_hrc(+Canon)
Argument(s)	: 	    atom
Description	: saves the current type hierarchy to the given Canon
Notes		: 

************************************************************************/

save_hrc(Canon) :-
	name(Canon, L1), name('.hrc', L2), conc(L1, L2, L3), name(File, L3),
	tell(File), write(':- dynamic ''<<''/2.'), nl,
	X << Y, portray_clause( X << Y), fail.
save_hrc(_) :- told.

/* save_dsc/1 ***********************************************************

Usage		: save_dsc(+Canon)
Argument(s)	: 	    atom
Description	: saves the current descriptions to the given Canon
Notes		: 

************************************************************************/

save_dsc(Canon) :-
	name(Canon, L1), name('.dsc', L2), conc(L1, L2, L3), name(File, L3),
	tell(File), write(':- dynamic description/3.'), nl,
	description(Kind, Obj, Desc), 
        portray_clause( description(Kind, Obj, Desc) ),
        fail.
save_dsc(_) :- told.

/************************************************************************

		   M E M O R Y   M A N A G E M E N T

************************************************************************/
 
/* mark/0 ***************************************************************

Usage		: mark
Argument(s)	: 
Description	: starts a new mark level
Notes		: all objects created after this will be automatically marked

************************************************************************/

mark :- recorded(mark_level, N, _), succ(N, M), recorda(mark_level, M, _), !.
mark :- recorda(mark_level, 0, _).

/* unmark/0 *************************************************************

Usage		: unmark
Argument(s)	: 
Description	: unmarks all objects marked at the current mark level
Notes		: 

************************************************************************/

unmark :- 
	recorded(mark_level, N, Ref), 
	( recorded(cg_mark, _ID-N, R), erase(R), fail
	; erase(Ref)
	), !.

/* sweep/0 **************************************************************

Usage		: sweep
Argument(s)	: 
Description	: deletes all objects marked at the current mark level
Notes		: 

************************************************************************/

sweep :-
	recorded(mark_level, N, Ref),
	( recorded(cg_mark, ID-N, DbRef), erase(DbRef), 
	  ( delete_obj(ID) ; delete_obj(ID-_) ), fail
	; erase(Ref)
	), !.
sweep.

/* sweep_all/0 **********************************************************

Usage		: sweep_all
Argument(s)	: 
Description	: deletes all objects currently marked
Notes		: 

************************************************************************/

sweep_all :-
	recorded(mark_level, _, _), sweep, fail.
sweep_all.

/* new_id/1 *************************************************************

Usage		: new_id(?Identifier)
Argument(s)	: 	      ID
Description	: returns a new unique Identifier 
Notes		: the kind of identifier (concept, context, graph, or 
			abstraction) must be indicated

************************************************************************/

new_id(Key/Id) :-
	recorded(Key, Id, Ref), erase(Ref),
	( recorded(mark_level, N, _), recorda(cg_mark, Key/Id-N, _)
	; true
	),
	( recorded(Key, _, _)
	; succ(Id, NewId), recorda(Key, NewId, _)
	), !.

/* free_id/1 ************************************************************

Usage		: free_id(+Identifier)
Argument(s)	: 	      ID
Description	: makes Identifier available for reuse by a new concept,
			graph, or lambda abstraction
Notes		: 

************************************************************************/

free_id(Key/Id) :-
	% recorded(mark_level, N, _), % !, 
	recorded(cg_mark, Key/Id- _N, Ref), erase(Ref),
	recorda(Key, Id, _), !.
free_id(Key/Id) :-
	recorda(Key, Id, _).

/* load_id/0 ************************************************************

Usage		: load_id
Argument(s)	: 
Description	: loads identifiers from a file
Notes		: 

************************************************************************/

cgp_id_dat(File):- absolute_file_name(library('cgt/cgp_id.dat'), File,[access(read)]).

load_id :-
        cgp_id_dat(File),
	seeing(Old), seen, see(File),
	repeat, read(T), 
	( T = end_of_file
	; T = id(Key, Id), recordz(Key, Id, _), fail
	),
	seen, seeing(Old).
load_id :-
        cgp_id_dat(File),
	format('File ~w is misssing!~n',[File]),  abort.

/* save_id/0 ************************************************************

Usage		: save_id
Argument(s)	: 
Description	: saves current state of used identifiers to a file
Notes		: succeeds always

************************************************************************/

save_id :-
        cgp_id_dat(File),
	telling(Old), told, tell(File),
	( member(Key, [c,g,p,l]), recorded(Key, Id, _Ref),% erase(Ref), 
	  write(id(Key, Id)), write('.'), nl, fail
	; told, tell(Old)
	).

/* shallow_delete/1 *****************************************************

Usage		: shallow_delete(+Object)
Argument(s)	: 	     ID/list
Description	: deletes non-recursively the Object
Notes		: succeeds always; Object may be a list of IDs

************************************************************************/

shallow_delete([ID|List]) :-
	shallow_delete(ID), shallow_delete(List).
shallow_delete(g/Id) :-
	retract( g(g/Id, _, _) ), free_id(g/Id).
shallow_delete(c/Id-_) :-
	retract( c(c/Id, _, _) ), free_id(c/Id).
shallow_delete(p/Id-_) :-
	retract( p(p/Id, _, _, _) ), free_id(p/Id).
shallow_delete(l/Id) :-
	retract( l(l/Id, _, _) ), free_id(l/Id).
shallow_delete(_).

/* delete_obj/1 *********************************************************

Usage		: delete_obj(+Object)
Argument(s)	: 	     ID/list
Description	: deletes the Object and recursively all its components
Notes		: succeeds always; Object may be a list of IDs

************************************************************************/

delete_obj([ID|List]) :-
	delete_obj(ID), delete_obj(List), !.
delete_obj(g/Id) :-
	retract( g(g/Id, CL, _) ), free_id(g/Id), delete_obj(CL), !.
delete_obj(p/Id-_) :-
	retract( p(p/Id, Type, Ref, _) ), 
	free_id(p/Id), delete_obj(Type),
	basic_ref(Ref, GIDs), delete_obj(GIDs), !.
delete_obj(c/Id-_) :-
	retract( c(c/Id, Type, _) ), free_id(c/Id), delete_obj(Type), !.
delete_obj(l/Id) :-
	retract( l(l/Id, _, GIDs) ), free_id(l/Id), delete_obj(GIDs), !.
delete_obj(_) :- !.

/************************************************************************

			M I S C E L A N N E O U S

************************************************************************/

/* show_obj/1 ***********************************************************

Usage		: show_obj(+Object)
Argument(s)	: 	   ID/list
Description	: displays the data structures representing the Object
Notes		: succeeds always; Object may be a list of IDs
		  this predicate is only for debugging purposes

************************************************************************/

:- style_check(-singleton).

getable_objects(g/Id) :- g(g/Id, CL, RL).
getable_objects(p/Id-_) :- p(p/Id, Type, Ref, Env).
getable_objects(c/Id-_) :- type(c/Id, Type) ; referent(c/Id, Ref).
getable_objects(l/Id) :- l(l/Id, CL, GIDs).
getable_objects(Rel) :- relation_type(Rel, Label, Def, Can, Arcs).
getable_objects(Type) :- concept_type(Type, Label, Def, Can, SL).



show_obj(Var):-var(Var),!,getable_objects(Var),nonvar(Var),show_obj(Var).
show_obj([ID|List]) :- 
	show_obj(ID), show_obj(List), !.
show_obj(g/Id) :-
	g(g/Id, CL, RL), write( g(g/Id, CL, RL) ), nl, show_obj(CL), !.
show_obj(p/Id-_) :-
	p(p/Id, Type, Ref, Env), write( p(p/Id, Type, Ref, Env) ), nl,
	( Type = l/Id -> show_obj(Type) ; true ),
	basic_ref(Ref, GIDs), show_obj(GIDs), !.
show_obj(c/Id-_) :-
	type(c/Id, Type), referent(c/Id, Ref),
	write( c(c/Id, Type, Ref) ), nl, 
	( Type = l/_ -> show_obj(Type) ; true ), !.
show_obj(l/Id) :-
	l(l/Id, CL, GIDs), write( l(l/Id, CL, GIDs) ), nl, show_obj(GIDs), !.
show_obj(Rel) :-
	relation_type(Rel, Label, Def, Can, Arcs),
	write( relation_type(Rel, Label, Def, Can, Arcs) ), nl,
	show_obj(Def), show_obj(Can), !.
show_obj(Type) :-
	concept_type(Type, Label, Def, Can, SL),
	write( concept_type(Type, Label, Def, Can, SL) ), nl,
	show_obj(Def), show_obj(Can), show_obj(SL), !.
show_obj(_).


/* number2var/2 *********************************************************

Usage		: number2var(+Number, ?Variable)
Argument(s)	: 	     integer	 atom
Description	: Variable is the atom corresponding to Number according
			the following sequence: 0 - a, 1 - b, ..., 26 - aa, ...
Notes		: 

************************************************************************/

number2var(X, V) :-
	n2v(X, L1), reverse(L1, L2), name(V, L2).

n2v(-1, []).
n2v(0, [97]).
n2v(X, [Y|L]) :-
	Y is X mod 26 + 97, Z is X // 26 - 1, n2v(Z, L).

/* dir_reference/2 ******************************************************

Usage		: dir_reference(+Concepts, +Relations)
Argument(s)	: lists
Description	: each variable in Relations is substitued with the CID it
		  stands for (direct reference)
Notes		: 

************************************************************************/

dir_reference([CID-CID|T], RL) :- 
	dir_reference(T, RL).
dir_reference([], _).

/* ind_reference/4 ******************************************************

Usage		: ind_reference(+OldRel, -NewRel, +OldConcepts, -NewConcepts)
Argument(s)	: lists
Description	: each argument in OldRel is substitued with the 
		  corresponding variable (indirect reference)
Notes		: OldConcepts is the list of CID-Var pairs already known

************************************************************************/

ind_reference([Rel|T1], [NewRel|T2], CL, NewCL) :-
	Rel =.. [Type|Args], args_reference(Args, NewArgs, CL, TmpCL),
	NewRel =.. [Type|NewArgs], ind_reference(T1, T2, TmpCL, NewCL).
ind_reference([], [], CL, CL).
	
/* args_reference/4 *****************************************************

Usage		: args_reference(+Args, -NewArgs, +OldConcepts, -NewConcepts)
Argument(s)	: lists
Description	: Args, a list of CIDs, is translated into a list of the
		  corresponding variables
Notes		: OldConcepts is the list of CID-Var pairs already known

************************************************************************/

args_reference([ID|T1], [Var|T2], CL, NewCL) :-
	member(ID-Var, CL), args_reference(T1, T2, CL, NewCL).
args_reference([ID|T1], [Var|T2], CL, NewCL) :-
	args_reference(T1, T2, [ID-Var|CL], NewCL).
args_reference([], [], CL, CL).

/* new_type/3 ***********************************************************

Usage		: new_type(+Type, +Label, +Supertypes_or_NumberOfArgs)
Argument(s)	: 	    atom   atom            list/number
Description	: adds Type and the corresponding Label to the database
Notes		: if Type is to be a concept type then the third argument
			must be a (possibly empty) list of its immediate
			supertypes (excluding T)
		  if Type is to be a relation type, then the third argument
			must be an integer (greater then 0) stating the
			relation's arity
		  it is not checked whether Type already exists

************************************************************************/

new_type(Type, Label, []) :-
	assert( concept_type(Type, Label, none, none, []) ).
new_type(Type, Label, [SuperLabel|T]) :-
	concept_type(SuperType, SuperLabel, _, _, _),
	assert( Type << SuperType ), new_type(Type, Label, T).

new_type(Type, Label, Args) :-
	assert( relation_type(Type, Label, none, none, Args) ).

/************************************************************************

		R E F E R E N T   O P E R A T I O N S

************************************************************************/

/* referent/2 ***********************************************************

Usage		: referent(+Concept, ?Referent)
Argument(s)	: 	    CID/PID	term
Description	: succeeds iff Referent is the referent of Concept
Notes		: 

************************************************************************/

referent(p/Id, Ref) :-
	p(p/Id, _, Ref, _), !.
referent(c/Id, Ref) :-
	c(c/Id, _, Ref), !.

/* basic_ref/2 **********************************************************

Usage		: basic_ref(+Referent, ?Basic)
Argument(s)	: terms 
Description	: succeeds iff Basic is the basic part of Referent
Notes		: Basic is just Referent with the coreference links 
			stripped off 

************************************************************************/

basic_ref(A = _CRL, C) :- basic_ref(A, C).
basic_ref(A, A).

/* change_ref/4 *********************************************************

Usage		: change_ref(+OldPart, +OldRef, +NewPart, -NewRef)
Argument(s)	: terms 
Description	: the part of OldRef which matches OldPart is changed to NewPart
Notes		: NewRef is the resulting referent

************************************************************************/

change_ref(OldCRL, Ref = OldCRL, none, Ref) :- !.
change_ref(OldCRL, Ref = OldCRL, NewCRL, Ref = NewCRL).
change_ref(Old, OldRef = CRL, New, NewRef = CRL) :-
	change_ref(Old, OldRef, New, NewRef).
change_ref(OldRef, OldRef, NewRef, NewRef).

/* put_crl/2 ************************************************************

Usage		: put_crl(+Concept1, +Concept2)
Argument(s)	: 	      ID	 ID
Description	: links the two concepts with a coreference link
Notes		: 

************************************************************************/

put_crl(ID1, ID2) :-
	retract( c(ID1, Type1, Ref1) ), assert( c(ID1, Type1, Ref1 = ID2) ),
	retract( c(ID2, Type2, Ref2) ), assert( c(ID2, Type2, Ref2 = ID1) ), !.
put_crl(ID1, ID2) :-
	retract( p(ID1, Type1, Ref1, Env1) ), 
	assert( p(ID1, Type1, Ref1 = ID2, Env1) ),
	retract( p(ID2, Type2, Ref2, Env2) ), 
	assert( p(ID2, Type2, Ref2 = ID1, Env2) ), !.

/* take_crl/2 ***********************************************************

Usage		: take_crl(+Concept1, +Concept2)
Argument(s)	: 	      ID	 ID
Description	: removes the coreference link between the two concepts
Notes		: 

************************************************************************/

take_crl(ID1, ID2) :-
	retract( c(ID1, Type1, OldRef1) ),
	change_ref(ID2, OldRef1, none, NewRef1),
	assert( c(ID1, Type1, NewRef1) ),
	retract( c(ID2, Type2, OldRef2) ),
	change_ref(ID1, OldRef2, none, NewRef2),
	assert( c(ID2, Type2, NewRef2) ), !.
take_crl(ID1, ID2) :-
	retract( p(ID1, Type1, OldRef1, Env1) ),
	change_ref(ID2, OldRef1, none, NewRef1),
	assert( p(ID1, Type1, NewRef1, Env1) ),
	retract( p(ID2, Type2, OldRef2, Env2) ),
	change_ref(ID1, OldRef2, none, NewRef2),
	assert( p(ID2, Type2, NewRef2, Env2) ), !.

/* meas_expansion/2 *****************************************************

Usage		: meas_expansion(+Concept, +Graph)
Argument(s)	: 	     	    ID	     GID
Description	: expands the Concept of Graph according to measure expansion
Notes		: this predicate assumes the referent of Concept is a measure

************************************************************************/

meas_expansion(CID, GID) :-
	retract( c(CID, Type, Ref) ), change_ref(meas(M), Ref, '*', NewRef),
	assert( c(CID, Type, NewRef) ),
	new_id(c/Id), assert( c(c/Id, measure, name(M)) ),
	retract( g(GID, CL, RL) ), member(CID-X, CL),
	assert( g(GID, [c/Id-Y|CL], [meas(X, Y)|RL]) ).

/* meas_contraction/2 ***************************************************

Usage		: meas_contraction(+Relation, +Graph)
Argument(s)	: 	     	      term      GID
Description	: tries to contract the measure Relation of Graph
Notes		: this predicate fails iff
			a) the concept with the dimension is not generic
		     or	b) the concept with the measure is coreferenced
		     or c) the concept with the measure is generic	

************************************************************************/

meas_contraction(meas(ID1, ID2), GID) :-
	g(GID, CL, RL), dir_reference(CL, RL), member(meas(ID1, ID2), RL), !,
	referent(ID2, name(M)), referent(ID1, Ref), %c(ID1, _, Ref), 
	change_ref('*', Ref, name(M), NewRef), 
	retract( c(ID1, Type, _) ), assert( c(ID1, Type, NewRef) ),
	join_on(GID, GID, [ID1-Var], [ID2-Var]),
	retract( g(GID, NewCL, NewRL) ), delete_one(meas(_, _), NewRL, RL2),
	assert( g(GID, NewCL, RL2) ).

/* qty_expansion/2 ******************************************************

Usage		: qty_expansion(+Concept, +Graph)
Argument(s)	: 	     	   ID	    GID
Description	: expands the Concept of Graph according to quantity expansion
Notes		: this predicate assumes the referent of Concept is a set
			with known cardinality

************************************************************************/

qty_expansion(CID, GID) :-
	retract( c(CID, Type, Ref) ), 
	change_ref(set(Kind, Set, Card), Ref, set(Kind, Set, _), NewRef),
	assert( c(CID, Type, NewRef) ),
	new_id(c/Id), assert( c(c/Id, number, name(Card)) ),
	retract( g(GID, CL, RL) ), member(CID-X, CL),
	assert( g(GID, [c/Id-Y|CL], [qty(X, Y)|RL]) ).

/* qty_contraction/2 ****************************************************

Usage		: qty_contraction(+Relation, +Graph)
Argument(s)	: 	     	     term      GID
Description	: tries to contract the quantity Relation of Graph
Notes		: this predicate fails iff there is a qty/2 relation but
			a) the concept with the set has known cardinality
		     or	b) the referent of the number concept isn't an integer
		     or c) the number concept is coreferenced
	
************************************************************************/

qty_contraction(qty(ID1, ID2), GID) :-
	g(GID, CL, RL), dir_reference(CL, RL), member(qty(ID1, ID2), RL), !,
	referent(ID2, name(Number)), integer(Number), referent(ID1, Ref),
	change_ref(set(Kind, Set, Card), Ref, set(Kind, Set, Number), NewRef),
	var(Card), retract( c(ID1, Type, _) ), assert( c(ID1, Type, NewRef) ),
	join_on(GID, GID, [ID1-Var], [ID2-Var]),
	retract( g(GID, NewCL, NewRL) ), delete_one(qty(_, _), NewRL, RL2),
	assert( g(GID, NewCL, RL2) ).

/* name_expansion/2 *****************************************************

Usage		: name_expansion(+Concept, +Graph)
Argument(s)	: 	     	    ID	     GID
Description	: expands the Concept of Graph according to name expansion
Notes		: this predicate assumes the referent of Concept is a name

************************************************************************/

name_expansion(CID, GID) :-
	retract( c(CID, Type, Ref) ), change_ref(name(N), Ref, '*', NewRef), 
	assert( c(CID, Type, NewRef) ),
	new_id(c/Id), assert( c(c/Id, N, '*') ),
	retract( g(GID, CL, RL) ), member(CID-X, CL),
	assert( g(GID, [c/Id-Y|CL], [name(X, Y)|RL]) ).

/* name_contraction/2 ***************************************************

Usage		: name_contraction(+Relation, +Graph)
Argument(s)	: 	     	      term      GID
Description	: tries to contract the name Relation of Graph
Notes		: this predicate fails iff
			a) the concept to be named is not generic
		     or	b) the concept with the name is coreferenced
		     or c) the concept with the name is not generic	

************************************************************************/

name_contraction(name(ID1, ID2), GID) :-
	g(GID, CL, RL), dir_reference(CL, RL), member(name(ID1, ID2), RL), !,
	type(ID2, Name), referent(ID2, '*'), referent(ID1, Ref),
	change_ref('*', Ref, name(Name), NewRef),
	retract( c(ID1, Type, _) ), assert( c(ID1, Type, NewRef) ),
	join_on(GID, GID, [ID1-Var], [ID2-Var]),
	retract( g(GID, NewCL, NewRL) ), delete_one(name(_, _), NewRL, RL2),
	assert( g(GID, NewCL, RL2) ).

/* del_univ_quant/5 *****************************************************

Usage		: del_univ_quant(+Con, +Graph, -NewCon, -NewGraph, -DoubleNeg)
Argument(s)	: 	   	  ID	 GID	  ID	   GID	       GID
Description	: deletes the universal quantifier of Con(cept) in Graph
Notes		: NewCon in NewGraph is the Con(cept)'s coreferenced copy
		  DoubleNeg is the double negation created during the process

************************************************************************/

del_univ_quant(ID, GID, NewID, NewGraph, NewGID) :- 
	( retract( c(ID, Type, Ref) ) ; retract( p(ID, Type, Ref, Env) ) ),
	change_ref(every, Ref, '*', NewRef),
	( ID = c/_ -> assert( c(ID, Type, NewRef) ) 
		    ; assert( p(ID, Type, NewRef, Env) )
	),
	which_context(GID, PID),
/*	( PID = outer -> GIDs = [GID]
		       ; p(PID, _, Ref1, _), basic_ref(Ref1, GIDs)
	),
*/	double_negation([GID], PID, NewGID), 
	g(NewGID, [NewEnv-_], _),
	copy_concept(ID-_, NewID-_, NewEnv),
	new_id(g/NewGraph), assert( g(g/NewGraph, [NewID-_], []) ),
	retract( p(NewEnv, Type2, GIDs2, Env2) ),
	assert( p(NewEnv, Type2, [g/NewGraph|GIDs2], Env2) ),
	put_crl(ID, NewID).
	
/************************************************************************

		    S E A R C H   O P E R A T I O N S

************************************************************************/

/* which_graph/3 ********************************************************

Usage		: which_graph(+Concept, ?Possible, -Graph)
Argument(s)	: 	       CID/PID	   list	     GID
Description	: returns the Graph containing Concept
Notes		: Possible is the list of graphs searched for Concept
		  if Possible is a variable, all graphs in the database are
		  	searched

************************************************************************/

which_graph(ID, GIDList, GID) :- 
	member(GID, GIDList), g(GID, CL, _), member(ID-_, CL).

/* which_context/2 ******************************************************

Usage		: which_context(+Graph, ?Context)
Argument(s)	: 	   	  GID	  term
Description	: succeeds iff Context is the deepest context containing Graph
Notes		: 

************************************************************************/

which_context(g/Id, Env) :- 
	p(ID, _, Ref, _), basic_ref(Ref, GIDs), member(g/Id, GIDs), !, Env = ID.
which_context(g/_, outer).

/* same_context/2 *******************************************************

Usage		: same_context(+Graphs, ?Context)
Argument(s)	: 	   	 GIDs	 term
Description	: succeeds iff Graphs are all in the same Context
Notes		: 

************************************************************************/

same_context([GID], Env) :-
        !, which_context(GID, Env).
same_context([GID|List], Env) :-
        which_context(GID, p/Id), !, Env= p/Id, 
        referent(p/Id, Ref), basic_ref(Ref, GIDs),
        subset(List, GIDs), Env = p/Id.
same_context([_|List], outer) :-
        !, same_context(List, outer).
same_context(GID, Env) :-
        which_context(GID, Env).

/************************************************************************

		     O U T P U T   O P E R A T I O N S

************************************************************************/

/* cg_warning/1 *********************************************************

Usage		: cg_warning(+Msg)
Argument(s)	: 	     atom
Description	: issues a warning message
Notes		: 

************************************************************************/

cg_warning(Msg) :-
	nl, write('Warning: '), write(Msg), nl.

/* cg_error/2 ***********************************************************

Usage		: cg_error(+Kind, +Culprit)
Argument(s)	: 	    atom    term
Description	: issues an error message and aborts execution
Notes		: Kind describes the nature of the error
		  Culprit describes the location of the error

************************************************************************/

% the following clause is to be used iff CGE is available
cg_error(Kind, Culprit) :-
        acknowledge(write_msg(Kind, Culprit)), sweep, get_back.

/* the following clause is to be used iff CGE isn't available
cg_error(Kind, Culprit) :-
	nl, write_msg(Kind, Culprit), nl, sweep, abort.
*/

/* write_msg/2 **********************************************************

Usage		: write_msg(+Kind, +Culprit)
Argument(s)	: 	     atom    term
Description	: displays an error message
Notes		: Kind describes the nature of the error
		  Culprit describes the location of the error

************************************************************************/

write_msg(dup_type_def, Label) :-
        write('Type '), write(Label), write(' is already defined!').
write_msg(dup_rel_def, Relation) :-
        write('Relation '), write(Relation), write(' is already defined!').
write_msg(dup_type_can, Label) :-
        write('Type '), write(Label), write(' already has a canonical graph!').
write_msg(dup_rel_can, Relation) :-
        write('Relation '), write(Relation), 
        write(' already has a canonical graph!').
write_msg(too_many_arcs, Rel) :-
	write('Relation '), write_rel(Rel),
	write(' has too many arcs attached to it!').
write_msg(too_few_arcs, Rel) :-
	write('Relation '), write_rel(Rel),
	write(' has not enough arcs attached to it!').
write_msg(point_away, Rel) :-
	write('The last arc attached to '), write_rel(Rel),
	write(' must point away from it!').
write_msg(point_into, Rel) :-
	write('All but one arc attached to '), write_rel(Rel),
	write(' must point to it!').
write_msg(duplicate_arc, N-Rel) :-
	write('Arc '), write(N), write(' of relation '), 
	write_rel(Rel), write(' is duplicate!').
write_msg(ambiguous_arc, Rel) :-
	write('Relation '), write_rel(Rel),
	write(' has ambiguous arcs attached to it!').
write_msg(undef_param, Var) :-
	write('Parameter '), write(Var), 
	write(' does not denote any concept in the abstraction body!').
write_msg(ambiguous_var, Var) :-
	write('Variable '), write(Var), write(' denotes different concepts!').
write_msg(wrong_crl, Var) :-
	write('Coreference link '), write(Var), 
	write(' denotes incompatible concepts!').
write_msg(unknown_type, Type) :-
	write('Concept type '), write(Type), write(' is not defined!').
write_msg(unknown_rel, Type) :-
	write('Relation type '), write(Type), write(' is not defined!').
write_msg(inv_name, Name) :-
	write(Name), write(' is not a valid name!').
write_msg(wrong_rel_arg, Rel-N-Type) :-
	write('The concept attached to arc '), write(N), 
	write(' of '), write_rel(Rel), write(' must be of a subtype of '),
	concept_type(Type, Label, _, _, _), write(Label), write('!').
write_msg(double_def, Ref) :-
	write('Referent '), reffield(Ref, L, []), apply(write(_), L),
	write(' denotes a concept defined in the same graph!').
write_msg(context_type, Type) :-
	concept_type(Type, Label, _, _, _), write(Label), 
	write(' should be a subtype of PROPOSITION!').
write_msg(context_ref, Ref) :-
	write('Referent '), reffield(Ref, L, []), apply(write(_), L),
	write(' should be generic or a set of graphs!').
write_msg(not_measure, Type) :-
	concept_type(Type, Label, _, _, _), write('A concept of type '),
	write(Label), write(' cannot have a measure as referent!').

/* write_rel/1 **********************************************************

Usage		: write_rel(+Relation)
Argument(s)	: 	       term
Description	: writes Relation in the form label/arity
Notes		: 

************************************************************************/

write_rel(Rel) :-
	functor(Rel, Type, NumArgs), 
	relation_type(Type, Label, _, _, _), write(Label/NumArgs).

/************************************************************************

		C H E C K I N G   O P E R A T I O N S

************************************************************************/

/* check_graph/1 ********************************************************

Usage		: check_graph(+Graph)
Argument(s)	: 	   	GID
Description	: succeeds iff Graph is well defined
Notes		: 

************************************************************************/

check_graph(GID) :-
	g(GID, CL, RL), dir_reference(CL, RL), 
	apply(check_relation(_, GID), RL), apply(check_concept(_, GID), CL).

/* check_relation/2 *****************************************************

Usage		: check_relation(+Relation, +Graph)
Argument(s)	: 	       	    term      GID
Description	: succeeds iff Relation of Graph is well used
Notes		: 

************************************************************************/

check_relation(Rel, GID) :-
	functor(Rel, Type, NumArgs), relation_type(Type, _, Def, Can, NumArgs),
	check_rel_def(Def, Rel, GID), check_rel_can(Can, Rel, GID).
	
/* check_rel_def/3 ******************************************************

Usage		: check_rel_def(+Definition, +Relation, +Graph)
Argument(s)	: 	       	    LID		term	  GID
Description	: succeeds iff Relation of Graph is well used according
		  to its Definition
Notes		: Definition may be the atom 'none'

************************************************************************/

check_rel_def(LID, Rel, GID) :-
	l(LID, CL, _), Rel =.. [_RelType|Args], 
	nth_member(CID1, Args, N), type(CID1, Type1),
	nth_member(CID2, CL,   N), type(CID2, Type2),
	( subtype(Type1, Type2) -> fail ; 
		delete_obj(GID), cg_error(wrong_rel_arg, Rel-N-Type2) 
	).
check_rel_def(_, _, _).

/* check_rel_can/3 ******************************************************

Usage		: check_rel_can(+Canonical, +Relation, +Graph)
Argument(s)	: 	       	    GID	       term	 GID
Description	: succeeds iff Relation of Graph is well used according
		  to its Canonical graph
Notes		: 

************************************************************************/

check_rel_can(Can, Rel1, GID) :-
	g(Can, CL, RL), dir_reference(CL, RL), 
	Rel1 =.. [RelType|Args1], member(Rel2, RL), Rel2 =.. [RelType|Args2],
	nth_member(CID1, Args1, N), type(CID1, Type1),
	nth_member(CID2, Args2, N), type(CID2, Type2),
	( subtype(Type1, Type2) -> fail ; 
		delete_obj(GID), cg_error(wrong_rel_arg, Rel1-N-Type2)
	).
check_rel_can(_, _, _).

/* check_concept/2 ******************************************************

Usage		: check_concept(+Concept, +Graph)
Argument(s)	: 	       	   ID	    GID
Description	: succeeds iff Concept (a node of Graph) is well-formed
Notes		: if Concept is a context, it must be subtype of PROPOSITION
			and it may not be coreferenced
		  if Concept's referent is a measure then its type must 
			be a dimension 

************************************************************************/

check_concept(p/Id-_, GID) :-
	p(p/Id, Type, Ref, _), basic_ref(Ref, Ident),
	( subtype(Type, proposition) 
	; delete_obj(GID), cg_error(context_type, Type)
	),
	( Ident = [_|_] ; Ident = ('*')
	; delete_obj(GID), cg_error(context_ref, Ident)
	).
check_concept(c/Id-_, _) :-
	type(c/Id, Type), referent(c/Id, Ref), basic_ref(Ref, meas(_)),
	( subtype(Type, dimension) ; cg_error(not_measure, Type) ).
check_concept(c/_-_, _).

%%% unused

%partially_specified(set(_, Set, _)) :- delete_one('*', Set, [_|_]).
 
