
/*

Rules to say which atoms can co-occur

*/

%----------------------------------------------------------------------------------

:- module(checklist_target_model,
	  [target_atom/2,
	   target_atom_excludes/2]
      ).

%----------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).

%----------------------------------------------------------------------------------

:- dynamic target_atom_excludes/2.

%----------------------------------------------------------------------------------

init_target_model :-
	retractall(target_atom_excludes(_, _)),
	all_target_model_atoms(AllAtoms),
	init_target_model1(AllAtoms),
	length(AllAtoms, NAtoms),
	format('~N~nTarget language model initialised (~d atoms)~n', [NAtoms]),
	!.
init_target_model :-
	format('~N*** Error: call to init_target_model failed.~n', []),
	fail.

init_target_model1([]).
init_target_model1([F | R]) :-
	findall(Excluded, atom_excludes_atom(F, Excluded), ExcludedList),
	list_to_ord_set(ExcludedList, ExcludedListOS),
	assert(target_atom_excludes(F, ExcludedListOS)),
	!,
	init_target_model1(R).

%----------------------------------------------------------------------------------

all_target_model_atoms(AllAtomsOS) :-
	findall(Atom, target_atom(Atom, _Type), AllAtoms),
	list_to_ord_set(AllAtoms, AllAtomsOS),
	!.
all_target_model_atoms(AllAtomsOS) :-
	format('~N*** Error: call failed: ~q~n', [all_target_model_atoms(AllAtomsOS)]),
	fail.

%----------------------------------------------------------------------------------

atom_excludes_atom(X, Y) :-
	target_atom(X, _XType),
	target_atom(Y, _YType),
	dif(X, Y),
	\+ atom_may_cooccur_with_atom_symmetric(X, Y).

%----------------------------------------------------------------------------------

atom_may_cooccur_with_atom_symmetric(X, Y) :-
	atom_may_cooccur_with_atom(X, Y).
atom_may_cooccur_with_atom_symmetric(X, Y) :-
	atom_may_cooccur_with_atom(Y, X).

%----------------------------------------------------------------------------------

% Numbers can cooccur with anything except another number
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	Atom = '*number*',
	target_atom(OtherAtom, _AnyClass),
	OtherAtom \== '*number*'.	

% Onoff_relation atoms can occur together with 'onoff', 'device', 'room' and numbers
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	target_atom(Atom, onoff_relation),
	(   target_atom(OtherAtom, onoff) ;
	    target_atom(OtherAtom, device) ;
	    target_atom(OtherAtom, room) ;
	    OtherAtom = '*number*' 
	).

% non_onoff_relation atoms can occur together with 'device', 'room' and numbers
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	target_atom(Atom, non_onoff_relation),
	(   target_atom(OtherAtom, device) ;
	    target_atom(OtherAtom, room) ;
	    OtherAtom = '*number*' 
	).

% onoff atoms can occur together with 'device', 'room' and numbers
% (Since 'cooccur with' is symmetric, we don't need to write both directions,
% and we've already said that 'onoff_relation' can cooccur with onoff).
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	target_atom(Atom, onoff),
	(   target_atom(OtherAtom, device) ;
	    target_atom(OtherAtom, room) ;
	    OtherAtom = '*number*' 
	).

% device atoms can occur together with 'room' and numbers
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	target_atom(Atom, device),
	(   target_atom(OtherAtom, room) ;
	    OtherAtom = '*number*' 
	).

% room atoms can occur together with numbers
atom_may_cooccur_with_atom(Atom, OtherAtom) :-
	target_atom(Atom, device),
	OtherAtom = '*number*'.

%----------------------------------------------------------------------------------

target_atom(switch, onoff_relation).
target_atom(is, onoff_relation).

target_atom(dim, non_onoff_relation).

target_atom(on, onoff).
target_atom(off, onoff).

target_atom(light, device).
target_atom(fan, device).

target_atom(kitchen, room).
target_atom(living_room, room).

target_atom('*number*', other).

%----------------------------------------------------------------------------------

:- init_target_model.
