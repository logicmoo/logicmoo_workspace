
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(make_verb_transforms,
	  [make_verb_transforms/2,
	   make_inverse_verb_transforms/2,
	   make_indicative_verb_list/2,
	   make_verb_taking_subjunctive_list/2,
	   test_make_verb_transforms/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_make_verb_transforms(transforms) :-
	make_verb_transforms('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
			     '$ACCEPT/MT/Europarl/Mmorph/verb_transforms.pl').

test_make_verb_transforms(transforms_faire) :-
	make_verb_transforms('$ACCEPT/MT/Europarl/Mmorph/verbs_faire.pl',
			     '$ACCEPT/MT/Europarl/Mmorph/verb_transforms_faire.pl').

test_make_verb_transforms(inverse_transforms) :-
	make_inverse_verb_transforms('$ACCEPT/MT/Europarl/Mmorph/verb_transforms.pl',
				     '$ACCEPT/MT/Europarl/Mmorph/verb_transforms_2sing_to_2pl.pl').

test_make_verb_transforms(verbs) :-
	make_verb_list('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
		       '$ACCEPT/MT/Europarl/Mmorph/verbs_with_mood.pl').

test_make_verb_transforms(verbs_taking_subjunctive) :-
	make_verb_taking_subjunctive_list('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
					  '$ACCEPT/MT/Europarl/Mmorph/verbs_taking_subjunctive.pl').

test_make_verb_transforms(indicative_verbs1) :-
	make_indicative_verb_list1('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
				   '$ACCEPT/MT/Europarl/Mmorph/verbs_with_mood.pl').
	
%---------------------------------------------------------------

make_verb_transforms(VerbFile, TransformFile) :-
	init_make_verb_transforms(VerbFile),
	make_verb_transforms1(TransformFile).

make_verb_transforms1(TransformFile) :-
	safe_absolute_file_name(TransformFile, AbsTransformFile),
	find_transforms(Transforms),
	write_out_transforms(Transforms, AbsTransformFile).

%---------------------------------------------------------------

make_inverse_verb_transforms(TransformFile, InverseTransformFile) :-
	safe_absolute_file_name(TransformFile, AbsTransformFile),
	safe_absolute_file_name(InverseTransformFile, AbsInverseTransformFile),

	safe_prolog_file_to_list_printing_statistics(AbsTransformFile, Transforms),

	invert_verb_transforms(Transforms, InverseTransforms0),
	sort(InverseTransforms0, InverseTransforms),

	length(InverseTransforms, NOut),
	list_to_prolog_file(InverseTransforms, AbsInverseTransformFile),

	format('~N--- Written file (~d items) ~w~n', [NOut, AbsInverseTransformFile]),
	!.

%---------------------------------------------------------------

make_verb_list(VerbFile, VerbListFile) :-
	init_make_verb_transforms(VerbFile),
	make_verb_list1(VerbListFile).

make_verb_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_verbs(Verbs),
	write_out_transforms(Verbs, AbsFile).

%---------------------------------------------------------------

make_verb_taking_subjunctive_list(VerbFile, VerbListFile) :-
	init_make_verb_transforms(VerbFile),
	make_verb_taking_subjunctive_list1(VerbListFile).

make_verb_taking_subjunctive_list1(File) :-
	safe_absolute_file_name(File, AbsFile),
	find_verbs_taking_subjunctive(Verbs),
	find_non_verbs_taking_subjunctive(NonVerbs),
	append(Verbs, NonVerbs, All),
	write_out_transforms(All, AbsFile).

%---------------------------------------------------------------

init_make_verb_transforms(VerbFile) :-
	safe_absolute_file_name(VerbFile, AbsVerbFile),
	format('~N--- Loading verb file: ~w~n', [AbsVerbFile]),
	compile(VerbFile).

%---------------------------------------------------------------

find_verbs(Transforms) :-
	findall(verb(Verb, Mode, Person, Number),
		(   mmorph_entry([surface=Verb, lemma=_Lemma, pos='Verb' | OtherFeats]),
		    member(mode=Modes, OtherFeats),
		    (   member(number=Numbers, OtherFeats) ->
			true
		    ;
			Numbers = [no_number]
		    ),
		    (   member(person=Persons, OtherFeats) ->
			true
		    ;
			Persons = [no_number]
		    ),
		    member(Mode, Modes),
		    member(Person, Persons),
		    member(Number, Numbers)
		),
		Transforms
	       ).

%---------------------------------------------------------------

find_verbs_taking_subjunctive(Transforms) :-
	findall(verb_taking_subjunctive(Verb),
		(   mmorph_entry([surface=Verb, lemma=Lemma, pos='Verb', (mode)=Modes,
				  tense=_Tenses, number=_Number, person=_Person | _OtherFeats]),
		    verb_taking_subjunctive(Lemma),
		    member(indicative, Modes)
		),
		Transforms
	       ).

find_non_verbs_taking_subjunctive(Transforms) :-
	findall(non_verb_taking_subjunctive(Word),
		non_verb_taking_subjunctive(Word),
		Transforms
	       ).

/*
aimer que
apprécier que
attendre que
consentir à ce que
défendre que
désirer que
douter que
être content que
être désolé que
être étonné que
être fâché que
être furieux que
être heureux que
être ravi que
être surpris que
être triste que
exiger que
il convient que
il est bon que
il est dommage que
il est douteux que
il est essentiel que
il est important que
il est impossible que
il est improbable que
il est juste que
il est nécessaire que
il est obligatoire que
il est peu probable que
il est rare que
il est possible que
il est préférable que
il est utile que
il est regrettable que
il est temps que
il semble que
il faut que
il ne faut pas que
interdire que
il suffit que
il vaut mieux que
recommander que
ordonner que
proposer que
s'opposer à ce que
refuser que
s'attendre à ce que
vouloir que
souhaiter que
tenir à ce que
*/

verb_taking_subjunctive(aimer).
verb_taking_subjunctive(apprécier).
verb_taking_subjunctive(attendre).
verb_taking_subjunctive(attendre).
verb_taking_subjunctive(consentir).
verb_taking_subjunctive(convient).
verb_taking_subjunctive(douter).
verb_taking_subjunctive(défendre).
verb_taking_subjunctive(désirer).
verb_taking_subjunctive(falloir).
verb_taking_subjunctive(faut).
verb_taking_subjunctive(interdire).
verb_taking_subjunctive(opposer).
verb_taking_subjunctive(ordonner).
verb_taking_subjunctive(proposer).
verb_taking_subjunctive(recommander).
verb_taking_subjunctive(refuser).
verb_taking_subjunctive(semble).
verb_taking_subjunctive(souhaiter).
verb_taking_subjunctive(suffire).
verb_taking_subjunctive(suffit).
verb_taking_subjunctive(tenir).
verb_taking_subjunctive(valoir).
verb_taking_subjunctive(vouloir).

non_verb_taking_subjunctive(bon).
non_verb_taking_subjunctive(dommage).
non_verb_taking_subjunctive(douteux).
non_verb_taking_subjunctive(essentiel).
non_verb_taking_subjunctive(important).
non_verb_taking_subjunctive(improbable).
non_verb_taking_subjunctive(juste).
non_verb_taking_subjunctive(nécessaire).
non_verb_taking_subjunctive(obligatoire).
non_verb_taking_subjunctive(probable).
non_verb_taking_subjunctive(rare).
non_verb_taking_subjunctive(possible).
non_verb_taking_subjunctive(préférable).
non_verb_taking_subjunctive(utile).
non_verb_taking_subjunctive(regrettable).
non_verb_taking_subjunctive(temps).

%---------------------------------------------------------------

find_transforms(Transforms) :-
	findall([surface=V2Plur, lemma=Lemma, (mode)=ModeP, tense=TenseP | OtherFeats],
		(   mmorph_entry([surface=V2Plur, lemma=Lemma, pos='Verb', (mode)=ModeP,
				  tense=TenseP, number=[plural], person=PersonP | OtherFeats]),
		    member('2', PersonP)
		),
		PluralTuples
	       ),
	length(PluralTuples, NPluralTuples),
	format('~N--- Found ~d 2nd person plural verbs~n', [NPluralTuples]),
	find_transforms1(PluralTuples, Transforms).

find_transforms1(PluralTuples, Transforms) :-
	find_transforms2(PluralTuples, TransformsForTuples, 0),
	append_list(TransformsForTuples, Transforms),
	length(Transforms, NTransforms),
	format('~N--- Found ~d 2nd person plural to singular transforms~n', [NTransforms]),
	!.

find_transforms2([], [], _I).
find_transforms2([F | R], [F1 | R1], I) :-
	all_transforms_for_tuple(F, F1),
	I1 is I + 1,
	(   0 is I1 mod 100 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	find_transforms2(R, R1, I1).

all_transforms_for_tuple(Tuple, Transforms) :-
	findall(Transform,
		verb_transforms_for_tuple(Tuple, Transform),
		Transforms),
	(   Transforms = [] ->
	    warn_about_missing_transforms_for_tuple(Tuple)
	;
	    otherwise ->
	    true
	).

warn_about_missing_transforms_for_tuple(Tuple) :-
	format('~N--- Warning: no 2nd plur -> 2nd sing transforms found for ~w~n', [Tuple]).

write_out_transforms(Transforms, AbsTransformFile) :-
	length(Transforms, N),
	list_to_prolog_file(Transforms, AbsTransformFile),
	format('~N--- Written verb file (~d elements): ~w~n', [N, AbsTransformFile]),
	!.

%mmorph_entry([surface=accueillez,lemma=accueillir,pos='Verb',(mode)=[indicative,imperative],tense=[present],number=[plural],person=['2'],form=[surface],type=['3']]).
%mmorph_entry([surface=accueilles,lemma=accueillir,pos='Verb',(mode)=[indicative],tense=[present],number=[singular],person=['2'],form=[surface],type=['3']]).

% mmorph_entry([surface=faites,lemma=faire,pos='Verb',(mode)=[indicative,imperative],tense=[present],number=[plural],person=['2'],form=[surface]]).
% mmorph_entry([surface=fais,lemma=faire,pos='Verb',(mode)=[indicative],tense=[present],number=[singular],person=['1','2'],form=[surface],type=['3']]).

verb_transforms_for_tuple(Tuple, verb_2_pl_to_verb_2_sing(V2Plur, V2Sing, Mode, Tense)) :-
	Tuple = [surface=V2Plur, lemma=Lemma, (mode)=ModeP, tense=TenseP | _OtherFeats],
	mmorph_entry([surface=V2Sing, lemma=Lemma, pos='Verb', (mode)=ModeS, tense=TenseS, number=[singular], person=PersonS | _OtherFeats1]),
	member('2', PersonS),
	compatible_feats(ModeP, ModeS, Mode),
	compatible_feats(TenseP, TenseS, Tense).

compatible_feats(Feats1, Feats2, Feat) :-
	member(Feat, Feats1),
	member(Feat, Feats2),
	!.

invert_verb_transforms([], []).
invert_verb_transforms([F | R], [F1 | R1]) :-
	invert_verb_transform(F, F1),
	!,
	invert_verb_transforms(R, R1).

invert_verb_transform(verb_2_pl_to_verb_2_sing(V2Plur, V2Sing, Mode, Tense),
		      verb_2_sing_to_verb_2_pl(V2Sing, V2Plur, Mode, Tense)) :-
	!.
invert_verb_transform(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [invert_verb_transform(F, F1)]),
	fail.
