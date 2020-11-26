
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(mmorph_to_lopar,
	  [mmorph_to_lopar/2,
	   test_mmorph_to_lopar/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_mmorph_to_lopar(small) :-
	mmorph_to_lopar('$ACCEPT/MT/Europarl/Mmorph/french_mmorph_small.pl',
			'$ACCEPT/MT/Europarl/Mmorph/french_mmorph_lopar_small.txt').

test_mmorph_to_lopar(full) :-
	mmorph_to_lopar('$ACCEPT/MT/Europarl/Mmorph/french_mmorph.pl',
			'$ACCEPT/MT/Europarl/Mmorph/french_mmorph_lopar.txt').

%---------------------------------------------------------------

mmorph_to_lopar(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	load_mmorph_file(AbsInFile),
	zero_counter,

	open(AbsOutFile, write, S, [encoding('UTF-8')]),

	loaded_mmorph_to_lopar(S),

	close(S),

	get_counter_value(N),
	format('~N--- Written MMorph data (~d entries) to ~w~n', [N, AbsOutFile]),
	!.

load_mmorph_file(AbsInFile) :-
	format('~N--- Loading MMorph data from: ~w~n', [AbsInFile]),
	compile(AbsInFile),
	format('~N--- Done~n', []).

loaded_mmorph_to_lopar(S) :-
	mmorph_entry(MmorphEntry),
	mmorph_entry_to_lopar(MmorphEntry, LoparEntry),
	format(S, '~N~w~n', [LoparEntry]),
	bump_counter,
	fail.
loaded_mmorph_to_lopar(_S).

%---------------------------------------------------------------

%mmorph_entry([surface=accueillions,lemma=accueillir,pos='Verb',(mode)=[subjunctive],tense=[present],number=[plural],person=['1'],form=[surface],type=['3']]).
%
% ->
%
% accueillions_Verb.subjunctive.present.plural.1_accueillir 

mmorph_entry_to_lopar(MmorphEntry, LoparEntry) :-
	member(surface=Surface, MmorphEntry),
	\+ multiword_surface_form(Surface),
	member(lemma=Lemma, MmorphEntry),
	mmorph_entry_to_lopar1(MmorphEntry, Surface, Lemma, LoparEntry).

mmorph_entry_to_lopar1(MmorphEntry, Surface, Lemma, LoparEntry) :-
	member(pos=POS, MmorphEntry),
	features_for_pos(POS, Feats),
	vals_for_feats(Feats, MmorphEntry, Vals),
	append_atoms([POS | Vals], 0'., POSAndVals),
	append_atoms([Surface, POSAndVals, Lemma], 0'_, LoparEntry),
	!.
mmorph_entry_to_lopar1(MmorphEntry, LoparEntry) :-
	format('~N*** Error: bad call: ~w~n', [mmorph_entry_to_lopar1(MmorphEntry, LoparEntry)]),
	fail.

vals_for_feats([], _MmorphEntry, []).
vals_for_feats([Feat | Feats], MmorphEntry, [Val | Vals]) :-
	val_for_feat(Feat, MmorphEntry, Val),
	!,
	vals_for_feats(Feats, MmorphEntry, Vals).
vals_for_feats([_Feat | Feats], MmorphEntry, Vals) :-
	!,
	vals_for_feats(Feats, MmorphEntry, Vals).

val_for_feat(Feat, MmorphEntry, Val) :-
	member(Feat=Vals, MmorphEntry),
	append_atoms(Vals, 0',, Val),
	!.

/*
mmorph_entry([surface=asservissements,lemma=asservissement,pos='Noun',gender=[masculine],number=[plural],form=[surface]]).

-> asservissements_Noun.masculine.plural_asservissement

mmorph_entry([surface=accueillions,lemma=accueillir,pos='Verb',(mode)=[subjunctive],tense=[present],number=[plural],person=['1'],form=[surface],type=['3']]).

-> accueillions_Verb.subjunctive.present.plural.1_accueillir

mmorph_entry([surface=décrassants,lemma=décrasser,pos='Adjective',gender=[masculine],number=[plural],degree=[positive],form=[surface]]).

-> décrassants_Adjective.masculine.plural.positive_décrasser

mmorph_entry([surface=le,lemma=le,pos='Det',gender=[masculine],number=[singular],type_s=[art],quant=[definite],type=['2'],tag=[det],form=[surface]]).

-> le_Det.masculine.singular_le
*/

%---------------------------------------------------------------

features_for_pos('Noun', [gender, number]) :- !.
features_for_pos('Verb', [mode, tense, number, person]) :- !.
features_for_pos('Aux', [mode, tense, number, person]) :- !.
features_for_pos('Adjective', [gender, number, degree]) :- !.
features_for_pos('Det', [gender, number]) :- !.
features_for_pos(_Other, []).

%---------------------------------------------------------------

multiword_surface_form(Atom) :-
	atom_codes(Atom, String),
	multiword_surface_string(String).

multiword_surface_string([F | R]) :-
	(   multiword_character(F) ->
	    true
	;
	    otherwise ->
	    multiword_surface_string(R)
	).

multiword_character(0'_).
multiword_character(0'-).
multiword_character(0'/).

%---------------------------------------------------------------

:- dynamic counter/0.

zero_counter :-
	retractall(counter(_)),
	assertz(counter(0)).

bump_counter :-
	retract(counter(I)),
	I1 is I + 1,
	assertz(counter(I1)),
	(   0 is I1 mod 1000 ->
	    format(' ~d', [I1])
	;
	    otherwise ->
	    true
	),
	!.

get_counter_value(N) :-
	counter(N),
	!.
get_counter_value(N) :-
	format('~N*** Error: bad call: ~w~n', [get_counter_value(N)]),
	fail.
	
%---------------------------------------------------------------

