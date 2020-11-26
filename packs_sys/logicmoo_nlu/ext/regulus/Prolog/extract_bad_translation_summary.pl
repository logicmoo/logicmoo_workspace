
:- module(extract_bad_translation_summary,
	  [extract_bad_translation_summary/2]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%----------------------------------------------------------------------

extract_bad_translation_summary(InFile, OutFile) :-
	init_extract_bad_translation_summary,
	
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	prolog_file_to_list(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsInFile]),

	extract_bad_translations_and_missing_generation_constants(InList),
	
	collect_bad_translations(BadTranslationList),
	length(BadTranslationList, NBadTranslations),

	collect_missing_generation_constants(MissingGenerationConstants),
	length(MissingGenerationConstants, NMissingGenerationConstants),

	NRecords is NBadTranslations + NMissingGenerationConstants,

	open(AbsOutFile, write, S),
	format(S, '~N*** CONSTANTS FAILING TO TRANSLATE ***~n', []),
	print_bad_translations(BadTranslationList, S),
	format(S, '~N*** CONSTANTS MISSING IN GENERATION GRAMMAR ***~n', []),
	print_bad_translations(MissingGenerationConstants, S),
	close(S),
	
	format('~N--- Written bad translation summary (~d records): ~w~n', [NRecords, AbsOutFile]).
extract_bad_translation_summary(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_bad_translation_summary(InFile, OutFile)]),
	fail.

:- dynamic unable_to_translate/1.
:- dynamic missing_generation_constant/1.

init_extract_bad_translation_summary :-
	retractall(unable_to_translate(_)),
	retractall(missing_generation_constant(_)).

extract_bad_translations_and_missing_generation_constants([]).
extract_bad_translations_and_missing_generation_constants([F | R]) :-
	extract_bad_translations_and_missing_generation_constants_from_record(F),
	!,
	extract_bad_translations_and_missing_generation_constants(R).

/*

translation(
    'WH-QUESTION headache become-better sc-when [ you experience what PRESENT ACTIVE ] PRESENT ACTIVE',
    transfer_from_interlingua_failed([if=[clause,[null=[tense,present],unable_to_translate:(arg2=[spec,what]),unable_to_translate:(null=[state,experience]),unable_to_translate:(null=[utterance_type,dcl])]],subject=[symptom,dutong],null=[tense,present],null=[utterance_type,question],unable_to_translate:(null=[event,become_better])]),
    [interlingua=[when=[clause,[arg1=[pronoun,you],arg2=[spec,what],null=[state,experience],null=[tense,present],null=[utterance_type,dcl],null=[voice,active]]],null=[event,become_better],arg1=[secondary_symptom,headache],null=[tense,present],null=[utterance_type,whq],null=[voice,active]]],
    error
  ).

*/

extract_bad_translations_and_missing_generation_constants_from_record(translation(_Source, Target, _Info, _Judgement)) :-
	compound(Target),
	functor(Target, F, 1),
	bad_translation_wrapper(F),
	arg(1, Target, Arg),
	extract_bad_translations_from_record1(Arg),
	!.
extract_bad_translations_and_missing_generation_constants_from_record(translation(_Source, Target, _Info, _Judgement)) :-
	Target = generation_failed(_TargetForm, missing_generation_constants:Constants),
	extract_missing_generation_constants_from_list(Constants),
	!.
extract_bad_translations_and_missing_generation_constants_from_record(_).

bad_translation_wrapper(transfer_from_interlingua_failed).
bad_translation_wrapper(transfer_to_interlingua_failed).
bad_translation_wrapper(transfer_to_source_discourse_failed).

extract_bad_translations_from_record1([]).
extract_bad_translations_from_record1([F | R]) :-
	extract_bad_translations_from_item(F),
	!,
	extract_bad_translations_from_record1(R).

extract_bad_translations_from_item(unable_to_translate:Bad) :-
	assertz(unable_to_translate(Bad)),
	!.
extract_bad_translations_from_item([clause, Clause]) :-
	extract_bad_translations_from_record1(Clause),
	!.
extract_bad_translations_from_item(_Role=[clause, Clause]) :-
	extract_bad_translations_from_record1(Clause),
	!.
extract_bad_translations_from_item(_Other).

extract_missing_generation_constants_from_list([]).
extract_missing_generation_constants_from_list([F | R]) :-
	assertz(missing_generation_constant(F)),
	!,
	extract_missing_generation_constants_from_list(R).

collect_bad_translations(OutList) :-
	findall(Bad,
		unable_to_translate(Bad),
		OutList0),
	list_to_ordered_multiset(OutList0, OutList).

collect_missing_generation_constants(OutList) :-
	findall(Bad,
		missing_generation_constant(Bad),
		OutList0),
	list_to_ordered_multiset(OutList0, OutList).

print_bad_translations([], _S).
print_bad_translations([F | R], S) :-
	print_bad_translation(F, S),
	!,
	print_bad_translations(R, S).

print_bad_translation(Freq-Item, S) :-
	format(S, '~N~d~4|~q~n', [Freq, Item]),
	!.
print_bad_translation(Other, S) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_bad_translation(Other, S)]),
	fail.

