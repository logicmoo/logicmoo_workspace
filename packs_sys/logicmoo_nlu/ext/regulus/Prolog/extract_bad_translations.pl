
:- module(extract_bad_translations,
	  [extract_bad_translations/2]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%----------------------------------------------------------------------

extract_bad_translations(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	prolog_file_to_list(AbsInFile, InList),
	length(InList, NInList),
	format('~N--- Read file (~d records): ~w~n', [NInList, AbsInFile]),

	extract_bad_translations1(InList, OutList),
	length(OutList, NOutList),

	open(AbsOutFile, write, S),
	print_bad_translations(OutList, S),
	close(S),
	format('~N--- Written file (~d records): ~w~n', [NOutList, AbsOutFile]).
extract_bad_translations(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_bad_translations(InFile, OutFile)]),
	fail.

extract_bad_translations1([], []).
extract_bad_translations1([F | R], [F | R1]) :-
	bad_translation(F),
	!,
	extract_bad_translations1(R, R1).
extract_bad_translations1([_F | R], R1) :-
	!,
	extract_bad_translations1(R, R1).

bad_translation(Record) :-
	Record = translation(_Source, Target, _Info, Judgement),
	member(Judgement, [error, bad]),
	Target \== no_translation_due_to_bad_recognition,
	!.

print_bad_translations([], _S).
print_bad_translations([F | R], S) :-
	print_bad_translation(F, S),
	!,
	print_bad_translations(R, S).

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

print_bad_translation(Record, S) :-
	Record = translation(Source, Target, Stats, Judgement),
	format(S, '~N~n%---------------------------------------', []),
	(   member(wavfile=Wavfile, Stats) ->
	    format(S, '~N%Wavfile: ~w', Wavfile) ;
	    true
	),
	format(S, '~N%Source: ~w', Source),
	(   member(recognised=Recognised, Stats) ->
	    format(S, '~N%Recognised: ~w~n', Recognised) ;
	    true
	),
	format(S, '~N%Target: ~w~n', Target),
	present_interesting_stats(S, Stats),
	format(S, '~N~ntranslation(~n', []),
	format(S, '~N    ~q,~n', [Source]),
	format(S, '~N    ~q,~n', [Target]),
	format(S, '~N    ~q,~n', [Stats]),
	format(S, '~N    ~q~n', [Judgement]),
	format(S, '~N  ).~n', []),
	!.

present_interesting_stats(S, Stats) :-
	(   member(source_representation=FirstParse, Stats) ->
	    format(S, '~N%    Source representation: ~w~n', [FirstParse]) ;
	    true
	),
	(   member(n_parses=NParses, Stats), NParses > 1 ->
	    format(S, '~N%*** WARNING *** ~d parses~n', [NParses]) ;
	    true
	),
	(   member(source_discourse=SourceDiscourse, Stats) ->
	    format(S, '~N%         Source discourse: ~w~n', [SourceDiscourse]) ;
	    true
	),
	(   member(resolved_source_discourse=ResolvedSourceDiscourse, Stats) ->
	    format(S, '~N%Resolved source discourse: ~w~n', [ResolvedSourceDiscourse]) ;
	    true
	),
	(   member(resolution_processing=ResolutionProcessing, Stats) ->
	    format(S, '~N%    Resolution processing: ~w~n', [ResolutionProcessing]) ;
	    true
	),
	(   member(interlingua=Interlingua, Stats) ->
	    format(S, '~N%              Interlingua: ~w~n', [Interlingua]) ;
	    true
	),
	(   member(target_representation=TransferredParse, Stats) ->
	    format(S, '~N%    Target representation: ~w~n', [TransferredParse]) ;
	    true
	),
	(   member(n_generations=NGenerations, Stats), NGenerations > 1 ->
	    format(S, '~N%*** WARNING *** ~d generated strings~n', [NGenerations]) ;
	    true
	).


