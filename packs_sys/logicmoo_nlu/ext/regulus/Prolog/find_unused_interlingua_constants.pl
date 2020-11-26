
:- module(find_unused_interlingua_constants,
	  [find_unused_interlingua_constants/3]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%----------------------------------------------------------------------

find_unused_interlingua_constants(InFiles, InterlinguaFile, OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),

	compile(InterlinguaFile),

	find_interlingua_constants_in_files(InFiles, OutList-[]),
	sort(OutList, SortedOutList),
	find_unused_interlingua_constants1(SortedOutList, UnusedConstants),
	
	length(UnusedConstants, NUnusedConstants),

	list_to_prolog_file(UnusedConstants, AbsOutFile),
	format('~N--- Written file (~d records): ~w~n', [NUnusedConstants, AbsOutFile]).
find_unused_interlingua_constants(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [find_unused_interlingua_constants(InFile, OutFile)]),
	fail.

find_interlingua_constants_in_files([], Out-Out).
find_interlingua_constants_in_files([F | R], In-Out) :-
	find_interlingua_constants_in_file(F, In-Next),
	!,
	find_interlingua_constants_in_files(R, Next-Out).

find_interlingua_constants_in_file(InFile, In-Out) :-
	absolute_file_name(InFile, AbsInFile),
	prolog_file_to_list(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsInFile]),

	find_interlingua_constants_in_list(InList, In-Out).	

find_interlingua_constants_in_list([], Out-Out).
find_interlingua_constants_in_list([F | R], In-Out) :-
	find_interlingua_constants_in_record(F, In-Next),
	!,
	find_interlingua_constants_in_list(R, Next-Out).

/*
%---------------------------------------
%Wavfile: c:/cygwin/home/speech/speechtranslation/medslt2/fre/corpora/speech/pierrette/2005-11-21/14.15.47.156/utt0019.wav
%Source: avez vous à la tête+do you have the headaches when you read
%Recognised: avez vous mal à la tête
%Target: do you have the headaches
%*** BAD TRANSLATION ***
%    Source representation: [[body_part,tête],[locative_prep,à],[path_proc,avoir],[pronoun,vous],[symptom,mal],[tense,present],[utterance_type,sentence],[voice,active]]
%         Source discourse: [[utterance_type,sentence],[voice,active],[tense,present],[path_proc,avoir],[pronoun,vous],[symptom,mal],[locative_prep,à],[body_part,tête]]
%Resolved source discourse: [[utterance_type,sentence],[voice,active],[tense,present],[path_proc,avoir],[pronoun,vous],[symptom,mal],[locative_prep,à],[body_part,tête]]
%    Resolution processing: trivial
%              Interlingua: [[symptom,headache],[state,have_symptom],[pronoun,you],[modal,can],[utterance_type,ynq],[voice,active]]
%    Target representation: [[pronoun,you],[secondary_symptom,headache],[state,have_symptom],[tense,present],[utterance_type,ynq],[voice,active]]

translation(
    'avez vous à la tête'+'do you have the headaches when you read',
    'do you have the headaches',
    [wavfile='c:/cygwin/home/speech/speechtranslation/medslt2/fre/corpora/speech/pierrette/2005-11-21/14.15.47.156/utt0019.wav',recognised='avez vous mal à la tête',n_parses=1,source_representation=[[body_part,tête],[locative_prep,à],[path_proc,avoir],[pronoun,vous],[symptom,mal],[tense,present],[utterance_type,sentence],[voice,active]],source_discourse=[[utterance_type,sentence],[voice,active],[tense,present],[path_proc,avoir],[pronoun,vous],[symptom,mal],[locative_prep,à],[body_part,tête]],resolved_source_discourse=[[utterance_type,sentence],[voice,active],[tense,present],[path_proc,avoir],[pronoun,vous],[symptom,mal],[locative_prep,à],[body_part,tête]],resolution_processing=trivial,interlingua=[[symptom,headache],[state,have_symptom],[pronoun,you],[modal,can],[utterance_type,ynq],[voice,active]],target_representation=[[pronoun,you],[secondary_symptom,headache],[state,have_symptom],[tense,present],[utterance_type,ynq],[voice,active]],n_generations=1,other_translations=[]],
    bad
  ).
*/

find_interlingua_constants_in_record(Record, In-Out) :-
	Record = translation(_Source, _Target, Info, _Judgement),
	member(interlingua=Interlingua, Info),
	find_interlingua_constants_in_representation(Interlingua, In-Out),
	!.
find_interlingua_constants_in_record(_Record, In-In).

find_interlingua_constants_in_representation([], Out-Out) :-
	!.
find_interlingua_constants_in_representation([F | R], In-Out) :-
	find_interlingua_constants_in_element(F, In-Next),
	!,
	find_interlingua_constants_in_representation(R, Next-Out).
find_interlingua_constants_in_representation(Interlingua, _Lists) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [find_interlingua_constants_in_representation(Interlingua, '...')]),
	fail.					     

find_interlingua_constants_in_element([clause, Body], In-Out) :-
	find_interlingua_constants_in_representation(Body, In-Out),
	!.
find_interlingua_constants_in_element(Element, [Element | Out]-Out).

%---------------------------------------

find_unused_interlingua_constants1(UsedConstants, UnusedConstants) :-
	store_used_constants(UsedConstants),
	findall(deprecated_interlingua_constant(UnusedConstant),
		(   interlingua_constant(UnusedConstant),
		    \+ used_constant(UnusedConstant)
		),
		UnusedConstants0),
	sort(UnusedConstants0, UnusedConstants),
	!.

:- dynamic used_constant/1.

store_used_constants(UsedConsants) :-
	retractall(used_constant(_)),
	store_used_constants1(UsedConsants).

store_used_constants1([]).
store_used_constants1([F | R]) :-
	assertz(used_constant(F)),
	!,
	store_used_constants1(R).



