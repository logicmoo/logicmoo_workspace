
:- module(translation_judging,
	  [combine_translation_results_files/2,
	   consolidate_translation_judgement_files/2,
	   evaluate_translation_file_against_judgements_file/3,
	   compare_results_files/4,
	   compare_results_file_differences/2]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%---------------------------------------------------------------------

:- dynamic stored_translation/3, stored_judgement/3.

%---------------------------------------------------------------------

combine_translation_results_files(InFiles, OutFile) :-
	init_combine_translation_results_files,
	read_translation_results_files(InFiles),
	write_combined_translation_results_file(OutFile).

%---------------------------------------------------------------------

consolidate_translation_judgement_files(TaggedFiles, OutFile) :-
	read_tagged_translation_judgement_files(TaggedFiles, TaggedLists),
	consolidate_translation_judgement_lists(TaggedLists, OutList),
	write_consolidated_judgement_file(OutList, OutFile).

%---------------------------------------------------------------------

evaluate_translation_file_against_judgements_file(TranslationFile, JudgementsFile, Results) :-
	init_judgements,
	read_judgements_file(JudgementsFile),
	read_translation_results_file_to_list(TranslationFile, List),
	evaluate_translations_against_stored_judgements(List, Results).

%---------------------------------------------------------------------

compare_results_files(TranslationFile1, TranslationFile2, Mode, Results) :-
	init_compare_results_files,
	store_translation_file_for_comparison(TranslationFile1, Mode),
	store_translation_file_for_comparison(TranslationFile2, Mode),
	compare_results_files1(TranslationFile1, TranslationFile2, Results).

compare_results_file_differences(TranslationFile1, TranslationFile2) :-
	init_compare_results_files,
	store_translation_file_for_comparison(TranslationFile1, source_target),
	store_translation_file_for_comparison(TranslationFile2, source_target),
	compare_results_file_differences1(TranslationFile1, TranslationFile2, N),
	format('~N~nTotal of ~d different translations~n', [N]).

%=====================================================================

init_combine_translation_results_files :-
	retractall(stored_translation(_, _, _)),
	!.

read_translation_results_files([]).
read_translation_results_files([F | R]) :-
	read_translation_results_file(F),
	!,
	read_translation_results_files(R).

read_translation_results_file(File) :-
	read_translation_results_file_to_list(File, List),
	store_translation_results_list(List),
	!.

read_translation_results_file_to_list(File, List) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	!.

store_translation_results_list([]).
store_translation_results_list([F | R]) :-
	store_translation_result(F),
	!,
	store_translation_results_list(R).

/*
translation(
    'is the pain stabbing',
    'la douleur est-elle comme un coup de poignard',
    [wavfile='c:/home/speech/speechtranslation/medslt2/corpora/coling2004/002-m-34/21-47-56-scylax-microsoft_sound_mapper/utt01.wav',recognised='is the pain stabbing',n_parses=1,source_representation=[[adj,stabbing],[secondary_symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],source_discourse=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],resolved_source_discourse=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],resolution_processing=trivial,interlingua=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],target_representation=[[degree,en_coup_de_poignard],[state,être],[symptom,douleur],[tense,present],[utterance_type,sentence],[voice,active]],n_generations=1,other_translations=[]],
    ?
  ).
*/

store_translation_result(Record) :-
	Record = translation(Source, Target, _Info, Judgement),
	(   Judgement \== '?' ->
	    true ;
	    store_translation_result1(Source, Target, Judgement)
	),
	!.
store_translation_result(Record) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_translation_result(Record)]),
	fail.

store_translation_result1(Source, Target, Judgement) :-
	stored_translation(Source, Target, Judgement),
	!.
store_translation_result1(Source, Target, Judgement) :-
	assertz(stored_translation(Source, Target, Judgement)),
	!.

%---------------------------------------------------------------------

write_combined_translation_results_file(OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),
	findall(translation(Source, Target, null, Judgement),
		stored_translation(Source, Target, Judgement),
		Records),
	sort(Records, SortedRecords),
	length(SortedRecords, N),
	list_to_prolog_file(SortedRecords, AbsOutFile),
	format('~N~n--- Written file (~d records) ~w~n', [N, AbsOutFile]),
	!.
write_combined_translation_results_file(OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_combined_translation_results_file(OutFile)]),
	fail.

%=====================================================================

read_tagged_translation_judgement_files([], []) :-
	!.
read_tagged_translation_judgement_files([F | R], [F1 | R1]) :-
	read_tagged_translation_judgement_file(F, F1),
	!,
	read_tagged_translation_judgement_files(R, R1).
read_tagged_translation_judgement_files(TaggedFiles, TaggedLists) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [read_tagged_translation_judgement_files(TaggedFiles, TaggedLists)]),
	fail.

read_tagged_translation_judgement_file(Tag-File, Tag-List) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	!.

%---------------------------------------------------------------------

consolidate_translation_judgement_lists(TaggedLists, OutList) :-
	(   check_tagged_lists_are_same_length(TaggedLists, _Length) ->
	    
	    consolidate_translation_judgement_lists1(TaggedLists, OutList, 1) ;
	    
	    format2error('~N*** Error in call to consolidate_translation_judgement_lists/2: lists are not of same length~n', []),
	    fail
	).

check_tagged_lists_are_same_length([], _Length) :-
	!.
check_tagged_lists_are_same_length([_Tag-List | R], Length) :-
	length(List, Length),
	!,
	check_tagged_lists_are_same_length(R, Length).

consolidate_translation_judgement_lists1(TaggedLists, [], _I) :-
	null_tagged_lists(TaggedLists),
	!.
consolidate_translation_judgement_lists1(TaggedLists, [F1 | R1], I) :-
	first_and_rest_on_tagged_lists(TaggedLists, F, R),
	consolidate_translation_judgement_on_single_item(F, F1, I),
	I1 is I + 1,
	!,
	consolidate_translation_judgement_lists1(R, R1, I1).

first_and_rest_on_tagged_lists([], [], []).
first_and_rest_on_tagged_lists([Tag-[F | R] | RestLists], [Tag-F | RestF], [Tag-R | RestR]) :-
	!,
	first_and_rest_on_tagged_lists(RestLists, RestF, RestR).
first_and_rest_on_tagged_lists(TaggedLists, F, R) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [first_and_rest_on_tagged_lists(TaggedLists, F, R)]),
	fail.

null_tagged_lists([]).
null_tagged_lists([_Tag-[] | R]) :-
	null_tagged_lists(R).

consolidate_translation_judgement_on_single_item(TaggedRecords, Result, _I) :-
	consolidate_translation_judgement_on_single_item1(TaggedRecords, Source, Target, TaggedJudgements),
	(   all_judgements_are_the_same(TaggedJudgements, SingleJudgement) ->
	    Result = translation(Source, Target, SingleJudgement, []) ;

	    majority_result_exists(TaggedJudgements, MajorityJudgement) ->
	    Result = translation(Source, Target, MajorityJudgement, TaggedJudgements) ;
				   
	    Result = translation(Source, Target, ?, TaggedJudgements)
	),
	!.
consolidate_translation_judgement_on_single_item(TaggedRecords, incompatible_records, I) :-
	\+ consolidate_translation_judgement_on_single_item1(TaggedRecords, _Source, _Target, _TaggedJudgements),
	!,
	format2error('~N*** Error: incompatible records in line ~d of files: ~w~n', [I, TaggedRecords]).
consolidate_translation_judgement_on_single_item(F, F1, I) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [consolidate_translation_judgement_on_single_item(F, F1, I)]),
	fail.

consolidate_translation_judgement_on_single_item1([], _Source, _Target, []).
consolidate_translation_judgement_on_single_item1([Tag-translation(Source, Target, Judgement) | R],
						  Source, Target, [Tag-Judgement | R1]) :-
	consolidate_translation_judgement_on_single_item1(R, Source, Target, R1).

all_judgements_are_the_same([], _SingleJudgement).
all_judgements_are_the_same([_Tag-SingleJudgement | R], SingleJudgement) :-
	all_judgements_are_the_same(R, SingleJudgement).

majority_result_exists(TaggedJudgements, MajorityJudgement) :-
	length(TaggedJudgements, N),
	HalfN is N / 2,
	
	findall(Judgement, member(_T-Judgement, TaggedJudgements), Judgements),
	sort(Judgements, UniqueJudgements),
	
	member(MajorityJudgement, UniqueJudgements),
	findall(x, member(_T1-MajorityJudgement, TaggedJudgements), Xs),
	length(Xs, N1),
	
	N1 > HalfN,
	!.

%---------------------------------------------------------------------

write_consolidated_judgement_file(List, OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),
	length(List, N),
	open(AbsOutFile, write, S),

	write_consolidated_judgement_list(List, S),

	close(S),
	format('~N~n--- Written file (~d records) ~w~n', [N, AbsOutFile]),
	!.
write_consolidated_judgement_file(List, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_consolidated_judgement_file(List, OutFile)]),
	fail.

write_consolidated_judgement_list([], _S).
write_consolidated_judgement_list([F | R], S) :-
	write_consolidated_judgement_list_item(F, S),
	!,
	write_consolidated_judgement_list(R, S).

write_consolidated_judgement_list_item(translation(Source, Target, Judgement, TaggedJudgements), S) :-
	format(S, '~N~n', []),
	write_tagged_judgements_as_comments(TaggedJudgements, S),
	format(S, '~N~q.~n', [translation(Source, Target, Judgement)]),
	!.
write_consolidated_judgement_list_item(F, S) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_consolidated_judgement_list_item(F, S)]),
	fail.

write_tagged_judgements_as_comments([], _S).
write_tagged_judgements_as_comments([F | R], S) :-
	write_tagged_judgement_as_comment(F, S),
	!,
	write_tagged_judgements_as_comments(R, S).

write_tagged_judgement_as_comment(Tag-Judgement, S) :-
	format(S, '~N% ~w: ~w~n', [Tag, Judgement]),
	!.
write_tagged_judgement_as_comment(F, S) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_tagged_judgement_as_comment(F, S)]),
	fail.

%=====================================================================

init_judgements :-
	retractall(stored_judgement(_, _, _)).

read_judgements_file(File) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	store_judgements_list(List),
	!.

store_judgements_list([]).
store_judgements_list([F | R]) :-
	store_judgement(F),
	!,
	store_judgements_list(R).

store_judgement(translation(Source, Target, Judgement)) :-
	store_judgement1(Source, Target, Judgement),
	!.
store_judgement(translation(Source, Target, _Info, Judgement)) :-
	store_judgement1(Source, Target, Judgement),
	!.
store_judgement(Record) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_judgement(Record)]),
	fail.

store_judgement1(Source, Target, Judgement) :-
	stored_judgement(Source, Target, Judgement),
	!.
store_judgement1(Source, Target, Judgement) :-
	assertz(stored_judgement(Source, Target, Judgement)),
	!.

%---------------------------------------------------------------------

evaluate_translations_against_stored_judgements(List, Results) :-
	evaluate_translations_against_stored_judgements1(List, Results0),
	list_to_ordered_multiset(Results0, Results1),
	normalise_ordered_multiset(Results1, Results2),
	add_extra_elts_to_multiset(Results2, Results),
	!.
evaluate_translations_against_stored_judgements(List, Results) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [evaluate_translations_against_stored_judgements(List, Results)]),
	fail.

evaluate_translations_against_stored_judgements1([], []).
evaluate_translations_against_stored_judgements1([F | R], [F1 | R1]) :-
	evaluate_translation_against_stored_judgements(F, F1),
	!,
	evaluate_translations_against_stored_judgements1(R, R1).

evaluate_translation_against_stored_judgements(translation(Source, Target, _Info, OldJudgement), Judgement) :-
	(   OldJudgement = error ->
	    Judgement = OldJudgement ;
	    stored_judgement(Source, Target, Judgement)
	),
	!.
evaluate_translation_against_stored_judgements(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [evaluate_translation_against_stored_judgements(F, F1)]),
	fail.

normalise_ordered_multiset(Multiset, NormalisedMultiset) :-
	findall(T, member(T-_, Multiset), Ts),
	safe_sum_list(Ts, Sum),
	(   Sum = 0 ->
	    Multiset = NormalisedMultiset ;
	    normalise_ordered_multiset1(Multiset, Sum, NormalisedMultiset)
	),
	!.
normalise_ordered_multiset(Multiset, NormalisedMultiset) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [normalise_ordered_multiset(Multiset, NormalisedMultiset)]),
	fail.

normalise_ordered_multiset1([], _Sum, []).
normalise_ordered_multiset1([F | R], Sum, [F1 | R1]) :-
	normalise_ordered_multiset_element(F, Sum, F1),
	!,
	normalise_ordered_multiset1(R, Sum, R1).

normalise_ordered_multiset_element(T-Elt, Sum, [T, T1]-Elt) :-
	T1 is T / Sum,
	!.
normalise_ordered_multiset_element(F, Sum, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [normalise_ordered_multiset_element(F, Sum, F1)]),
	fail.

add_extra_elts_to_multiset(ResultsIn, ResultsOut) :-
	(   member([Good, Good1]-good, ResultsIn) ->
	    true ;
	    [Good, Good1] = [0, 0]
	),
	(   member([OK, OK1]-ok, ResultsIn) ->
	    true ;
	    [OK, OK1] = [0, 0]
	),
	(   member([Bad, Bad1]-bad, ResultsIn) ->
	    true ;
	    [Bad, Bad1] = [0, 0]
	),
	(   member([Error, Error1]-error, ResultsIn) ->
	    true ;
	    [Error, Error1] = [0, 0]
	),
	
	GoodOrOK is Good + OK,
	GoodOrOK1 is Good1 + OK1,

	BadOrError is Bad + Error,
	BadOrError1 is Bad1 + Error1,

	ResultsOut = [[GoodOrOK, GoodOrOK1]-good_or_ok, [BadOrError, BadOrError1]-bad_or_error | ResultsIn],
	!.
add_extra_elts_to_multiset(ResultsIn, ResultsOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_extra_elts_to_multiset(ResultsIn, ResultsOut)]),
	fail.

%=====================================================================

:- dynamic stored_translation_result_for_comparison/4.

init_compare_results_files :-
	retractall(stored_translation_result_for_comparison(_, _, _, _)).

store_translation_file_for_comparison(File, Mode) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	store_translation_file_for_comparison1(List, File, Mode, 0-N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	!.
store_translation_file_for_comparison(File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_translation_file_for_comparison(File)]),
	fail.

store_translation_file_for_comparison1([], _Id, _Mode, N-N).
store_translation_file_for_comparison1([F | R], Id, Mode, In-Out) :-
	store_translation_file_item(F, Id, Mode, In-Next),
	!,
	store_translation_file_for_comparison1(R, Id, Mode, Next-Out).

/*
translation(
    'is the pain stabbing',
    'la douleur est-elle comme un coup de poignard',
    [wavfile='c:/home/speech/speechtranslation/medslt2/corpora/coling2004/002-m-34/21-47-56-scylax-microsoft_sound_mapper/utt01.wav',recognised='is the pain stabbing',n_parses=1,source_representation=[[adj,stabbing],[secondary_symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],source_discourse=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],resolved_source_discourse=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],resolution_processing=trivial,interlingua=[[adj,stabbing],[symptom,pain],[tense,present],[utterance_type,ynq],[verb,be],[voice,active]],target_representation=[[degree,en_coup_de_poignard],[state,être],[symptom,douleur],[tense,present],[utterance_type,sentence],[voice,active]],n_generations=1,other_translations=[]],
    ?
  ).
*/

store_translation_file_item(Record, Id, source_target, In-Out) :-
	Record = translation(Source, Target0, Info, _Judgement),
	(   atom(Target0) ->
	    Target = Target0
	;
	    Target = failed
	),
	(   member(recognised=Recognised, Info) ->
	    true
	;
	    Recognised = '*no_recognition_result*'
	),
	assertz(stored_translation_result_for_comparison(Source, Id, Recognised, Target)),
	Out is In + 1,
	!.
store_translation_file_item(Record, Id, strict, In-Out) :-
	Record = translation(Source0, _Target, Info, _Judgement0),
	(   atom(Source0) ->
	    Source = Source0 ;
	    Source0 = Source+_Context
	),
	member(wavfile=Wavfile, Info),
	member(recognised=Recognised, Info),
	(   Source = Recognised ->
	    Judgement = good ;
	    Judgement = bad
	),
	assertz(stored_translation_result_for_comparison(Wavfile, Id, Recognised, Judgement)),
	Out is In + 1,
	!.
store_translation_file_item(Record, Id, sem, In-Out) :-
	Record = translation(Source, Target, Info, Judgement0),
	(   get_judgement(Source, Target, Judgement) ->
	    true ;
	    Judgement0 = Judgement
	),
	member(wavfile=Wavfile, Info),
	(   member(recognised=Recognised, Info) ->
	    true
	;
	    Recognised = '*no_recognition_result*'
	),
	Judgement \== '?',
	assertz(stored_translation_result_for_comparison(Wavfile, Id, Recognised, Judgement)),
	Out is In + 1,
	!.
store_translation_file_item(_Record, _Id, _Mode, In-In).

compare_results_files1(Id1, Id2, [Id1Better, Id2Better]) :-
	find_all_stored_keys(AllWavfiles),
	count_better_records(AllWavfiles, Id1, Id2, 0-Id1Better),
	count_better_records(AllWavfiles, Id2, Id1, 0-Id2Better),
	!.
compare_results_files1(Id1, Id2, Results) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [compare_results_files1(Id1, Id2, Results)]),
	fail.

find_all_stored_keys(AllWavfiles) :-
	findall(Wavfile,
		stored_translation_result_for_comparison(Wavfile, _Id, _Rec, _Judgement),
		AllWavfiles0),
	sort(AllWavfiles0, AllWavfiles).

count_better_records([], _Id1, _Id2, In-In).
count_better_records([F | R], Id1, Id2, In-Out) :-
	first_id_is_better_on_wavfile(F, Id1, Id2),
	Next is In + 1,
	!,
	count_better_records(R, Id1, Id2, Next-Out).
count_better_records([_F | R], Id1, Id2, In-Out) :-
	!,
	count_better_records(R, Id1, Id2, In-Out).

first_id_is_better_on_wavfile(Wavfile, Id1, Id2) :-
	stored_translation_result_for_comparison(Wavfile, Id1, _Recognised1, Judgement1),
	stored_translation_result_for_comparison(Wavfile, Id2, _Recognised2, Judgement2),
	judgement_is_better(Judgement1, Judgement2),
	!.

get_judgement(Source, Target, Judgement) :-
	current_predicate(user:judged_translation/2),
	user:judged_translation(Source, Info),
	member(Target-Judgement, Info),
	!.

judgement_is_better(good, Other) :-
	Other \== good,
	Other \== '?'.
judgement_is_better(ok, Other) :-
	Other \== good,
	Other \== ok,
	Other \== '?'.

compare_results_file_differences1(Id1, Id2, N) :-
	find_all_stored_keys(Keys),
	compare_results_file_differences2(Keys, Id1, Id2, 0-N).

compare_results_file_differences2([], _Id1, _Id2, N-N).
compare_results_file_differences2([F | R], Id1, Id2, In-Out) :-
	show_result_file_difference(F, Id1, Id2, In-Next),
	!,
	compare_results_file_differences2(R, Id1, Id2, Next-Out).

show_result_file_difference(Source, Id1, Id2, In-Out) :-
	stored_translation_result_for_comparison(Source, Id1, Recognised1, Target1),
	stored_translation_result_for_comparison(Source, Id2, Recognised2, Target2),
	Target1 \== Target2,
	format('~N~n-------------------------------------------~n', []),
	format('~NSource: ~w~n', [Source]),
	format('~NTarget: ~w (~w)~n', [Target1, Id1]),
	format('~N   Rec: ~w~n', [Recognised1]),
	format('~NTarget: ~w (~w)~n', [Target2, Id2]),
	format('~N   Rec: ~w~n', [Recognised2]),
	Out is In + 1,
	!.
show_result_file_difference(_Source, _Id1, _Id2, In-In).
