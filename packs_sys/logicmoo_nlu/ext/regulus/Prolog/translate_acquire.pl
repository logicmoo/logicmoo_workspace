
:- module(translate_acquire,
	  [
	   create_translation_rule_learning_files/1,
	   create_translation_rule_learning_files/5,

	   process_to_interlingua_learning_file/3,
	   process_to_interlingua_learning_file/4,

	   process_from_interlingua_learning_file/3,
	   process_from_interlingua_learning_file/5,
	   
	   translation_rule_learning_file_pathnames/6]
	 ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/translate').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusLanguageServer/Prolog/remote_regulus').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(terms)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%----------------------------------------------------------------------

port_for_remote_regulus(4321).	

%----------------------------------------------------------------------

create_translation_rule_learning_files(BatchrecFile) :-
	translation_rule_learning_file_pathnames(BatchrecFile,
						 ToInterlinguaFile, FromInterlinguaFile,
						 GenerationFile, BadTranslationsFile,
						 SuggestionsFile),
	create_translation_rule_learning_files(BatchrecFile,
					       ToInterlinguaFile, FromInterlinguaFile,
					       GenerationFile, BadTranslationsFile,
					       SuggestionsFile).

create_translation_rule_learning_files(BatchrecFile,
				       ToInterlinguaFile, FromInterlinguaFile,
				       GenerationFile, BadTranslationsFile,
				       SuggestionsFile) :-
	read_batchrec_file(BatchrecFile, BatchrecList),
	store_batchrec_file_info(BatchrecList),
	read_and_store_suggestions_file(SuggestionsFile),
	make_to_interlingua_list(ToInterlinguaList),
	make_from_interlingua_list(FromInterlinguaList),
	make_generation_list(GenerationList),
	make_bad_translation_list(BadTranslationList),
	write_interlingua_learning_file(ToInterlinguaList, ToInterlinguaFile),
	write_interlingua_learning_file(FromInterlinguaList, FromInterlinguaFile),
	write_interlingua_learning_file(GenerationList, GenerationFile),
	write_interlingua_learning_file(BadTranslationList, BadTranslationsFile),
	!.
create_translation_rule_learning_files(BatchrecFile,
				       ToInterlinguaFile, FromInterlinguaFile,
				       GenerationFile, BadTranslationsFile,
				       SuggestionsFile) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [create_translation_rule_learning_files(BatchrecFile, ToInterlinguaFile, FromInterlinguaFile, GenerationFile, BadTranslationsFile, SuggestionsFile)]),
	fail.

%----------------------------------------------------------------------

process_to_interlingua_learning_file(ToInterlinguaFile, CFGFile, SuggestionsFile) :-
	read_and_store_suggestions_file(SuggestionsFile),
	translation_rule_learned_file_pathnames(ToInterlinguaFile, NewToRulesFile, _NewGenerationCorpusFile),
	process_to_interlingua_learning_file1(ToInterlinguaFile, CFGFile, NewToRulesFile),
	write_suggestions_file_from_stored_data(SuggestionsFile).

process_from_interlingua_learning_file(FromInterlinguaFile, CFGFile, SuggestionsFile) :-
	read_and_store_suggestions_file(SuggestionsFile),
	translation_rule_learned_file_pathnames(FromInterlinguaFile, NewFromRulesFile, NewGenerationCorpusFile),
	process_from_interlingua_learning_file1(FromInterlinguaFile,
					       CFGFile,
					       NewFromRulesFile,
					       NewGenerationCorpusFile),
	write_suggestions_file_from_stored_data(SuggestionsFile).

%----------------------------------------------------------------------

process_to_interlingua_learning_file1(ToInterlinguaFile,
				     CFGFile,
				     NewToRulesFile) :-
	init_process_to_interlingua_learning_file(CFGFile),
	process_to_interlingua_learning_file2(ToInterlinguaFile,
					      CFGFile,
					      NewToRulesFile).

process_to_interlingua_learning_file2(ToInterlinguaFile,
				      CFGFile,
				      NewToRulesFile) :-
	on_exception(
	Exception, 
	process_to_interlingua_learning_file3(ToInterlinguaFile,
					      CFGFile,
					      NewToRulesFile),
	(   format2error('~N~n*** Error in process_to_interlingua_learning_file1/3 ***~n~n', []), 
	    print_message(error, Exception),
	    conclude_process_to_interlingua_learning_file
	)
		    ),
	!.
process_to_interlingua_learning_file2(_ToInterlinguaFile,
				      _CFGFile,
				      _NewToRulesFile) :-
	conclude_process_to_interlingua_learning_file.

process_to_interlingua_learning_file3(ToInterlinguaFile,
				      CFGFile,
				      NewToRulesFile) :-
	store_translation_suggestions_from_interlingua_learning_file(ToInterlinguaFile),
	%parse_and_transfer_interlingua_learning_file(ToInterlinguaFile, CFGFile, specialised),
	parse_and_transfer_interlingua_learning_file(ToInterlinguaFile, CFGFile, unspecialised),	
	collect_rules_from_to_interlingua_learning_file(ToInterlinguaFile, ToRulesList),
	format('~N~n***********************************************************~n', []),
	format('~NWRITING OUT ACQUIRED RULES~n~n', []),
	write_new_transfer_rules_file(ToRulesList, NewToRulesFile),
	conclude_process_to_interlingua_learning_file.

process_from_interlingua_learning_file1(FromInterlinguaFile,
				       CFGFile,
				       NewFromRulesFile,
				       NewGenerationCorpusFile) :-
	init_process_from_interlingua_learning_file(CFGFile),
	process_from_interlingua_learning_file2(FromInterlinguaFile,
						CFGFile,
						NewFromRulesFile,
						NewGenerationCorpusFile).

process_from_interlingua_learning_file2(FromInterlinguaFile,
					CFGFile,
					NewFromRulesFile,
					NewGenerationCorpusFile) :-
	on_exception(
	Exception, 
	process_from_interlingua_learning_file3(FromInterlinguaFile,
						CFGFile,
						NewFromRulesFile,
						NewGenerationCorpusFile),
	(   format2error('~N~n*** Error in process_from_interlingua_learning_file1/4 ***~n~n', []), 
	    print_message(error, Exception),
	    conclude_process_to_interlingua_learning_file
	)
		    ),
	!.
process_from_interlingua_learning_file2(_FromInterlinguaFile,
					_CFGFile,
					_NewFromRulesFile,
					_NewGenerationCorpusFile) :-
	conclude_process_from_interlingua_learning_file.

process_from_interlingua_learning_file3(FromInterlinguaFile,
					CFGFile,
					NewFromRulesFile,
					NewGenerationCorpusFile) :-
	store_translation_suggestions_from_interlingua_learning_file(FromInterlinguaFile),
	parse_interlingua_learning_file(FromInterlinguaFile, CFGFile, specialised),
	parse_interlingua_learning_file(FromInterlinguaFile, CFGFile, unspecialised),
	collect_rules_from_from_interlingua_learning_file(FromInterlinguaFile, FromRulesList, GenerationCorpusList),
	format('~N~n***********************************************************~n', []),
	format('~NWRITING OUT ACQUIRED RULES~n~n', []),
	write_new_transfer_rules_file(FromRulesList, NewFromRulesFile),
	write_new_generation_corpus_file(GenerationCorpusList, NewGenerationCorpusFile),
	conclude_process_from_interlingua_learning_file.

%----------------------------------------------------------------------

read_batchrec_file(BatchrecFile, BatchrecList) :-
	absolute_file_name(BatchrecFile, AbsBatchrecFile),
	prolog_file_to_list(AbsBatchrecFile, BatchrecList),
	length(BatchrecList, N),
	format('~N--- Read batchrec file (~d records) ~w~n', [N, AbsBatchrecFile]),
	!.
	
%----------------------------------------------------------------------

% batchrec_item(Source, InterlinguaRepresentation, TargetRepresentation, Status)

:- dynamic batchrec_item/5.

store_batchrec_file_info(BatchrecList) :-
	retractall(batchrec_item(_, _, _, _, _)),
	store_batchrec_file_info1(BatchrecList),
	!.
store_batchrec_file_info(_BatchrecList) :-
	format2error('~N *** Error: call to store_batchrec_file_info/1 failed~n', []),
	fail.

store_batchrec_file_info1([]).
store_batchrec_file_info1([F | R]) :-
	store_batchrec_file_item(F) ->
	!,
	store_batchrec_file_info1(R).
store_batchrec_file_info1([_F | R]) :-
	!,
	store_batchrec_file_info1(R).

/*

GOOD:

translation(
    'aka wain de zutsu ga okoru koto wa ari masu ka'+'do you have the headaches when you drink red wine',
    'do you have the headaches when you drink red wine',
    [n_parses=1,source_representation=[[cause,aka_wain],[event,okoru],[postpos,causal],[symptom,zutsu],[tense,present],[utterance_type,sentence],[verb,koto_ga_aru]],source_discourse=[[utterance_type,sentence],[cause,aka_wain],[postpos,causal],[symptom,zutsu],[tense,present],[event,okoru],[tense,present],[verb,koto_ga_aru]],resolved_source_discourse=[[utterance_type,sentence],[cause,aka_wain],[postpos,causal],[symptom,zutsu],[tense,present],[event,okoru],[tense,present],[verb,koto_ga_aru]],resolution_processing=trivial,interlingua=[[sc,when],[clause,[[utterance_type,dcl],[pronoun,you],[tense,present],[voice,active],[action,drink],[cause,red_wine]]],[pronoun,you],[state,have_symptom],[symptom,headache],[tense,present],[voice,active],[utterance_type,ynq]],target_representation=[[clause,[[action,drink],[cause,red_wine],[pronoun,you],[tense,present],[utterance_type,dcl],[voice,active]]],[pronoun,you],[sc,when],[secondary_symptom,headache],[state,have_symptom],[tense,present],[utterance_type,ynq],[voice,active]],n_generations=1,other_translations=[]],
    good
  ).

*/

store_batchrec_file_item(Record) :-
	Record = translation(Source, Target, Info, Judgement),
	member(Judgement, [good, ok, '?']),
	member(resolved_source_discourse=SourceDiscourseRepresentation, Info),
	member(interlingua=InterlinguaRepresentation, Info),
	member(target_representation=TargetRepresentation, Info),
	Status = correct(Target),
	ToStore = batchrec_item(Source, SourceDiscourseRepresentation, InterlinguaRepresentation, TargetRepresentation, Status),
	assertz(ToStore),
	!.
	
/*

TO-INTERLINGUA FAILED:

translation(
    'syuu ni suukai zutsu wa okori masu ka'+'do the headaches last for weeks',
    transfer_to_interlingua_failed([unable_to_translate:[duration,issyuukan],[pronoun,you],[state,have_symptom],[symptom,headache],unable_to_translate:[temporal,suukai],[tense,present],[voice,active],[utterance_type,ynq]]),
    [n_parses=1,source_representation=[[duration,issyuukan],[event,okoru],[symptom,zutsu],[temporal,suukai],[tense,present],[utterance_type,sentence]],source_discourse=[[utterance_type,sentence],[duration,issyuukan],[temporal,suukai],[symptom,zutsu],[tense,present],[event,okoru]],resolved_source_discourse=[[utterance_type,sentence],[duration,issyuukan],[temporal,suukai],[symptom,zutsu],[tense,present],[event,okoru]],resolution_processing=trivial],
    error
  ).

*/

store_batchrec_file_item(Record) :-
	Record = translation(Source, Target, Info, error),
	Target = transfer_to_interlingua_failed(InterlinguaRepresentation),
	member(resolved_source_discourse=SourceDiscourseRepresentation, Info),
	TargetRepresentation = [],
	Status = to_interlingua_failed,
	ToStore = batchrec_item(Source, SourceDiscourseRepresentation, InterlinguaRepresentation, TargetRepresentation, Status),
	assertz(ToStore),
	!.

/*


FROM-INTERLINGUA FAILED:

translation(
    'issyuukan ni sankai kurai zutsu wa okori masu ka'+'do you have the headaches once a week',
    transfer_from_interlingua_failed([[frequency,time],unable_to_translate:[number,3],[prep,duration],[pronoun,you],[state,have_symptom],[secondary_symptom,headache],[tense,present],[freq_timeunit,week],[utterance_type,ynq],[voice,active]]),
    [n_parses=1,source_representation=[[duration,issyuukan],[event,okoru],[symptom,zutsu],[temporal,sankai],[tense,present],[utterance_type,sentence]],source_discourse=[[utterance_type,sentence],[duration,issyuukan],[temporal,sankai],[symptom,zutsu],[tense,present],[event,okoru]],resolved_source_discourse=[[utterance_type,sentence],[duration,issyuukan],[temporal,sankai],[symptom,zutsu],[tense,present],[event,okoru]],resolution_processing=trivial,interlingua=[[prep,frequency],[number,3],[frequency,time],[prep,duration],[timeunit,week],[pronoun,you],[state,have_symptom],[symptom,headache],[tense,present],[voice,active],[utterance_type,ynq]]],
    error
  ).

*/

store_batchrec_file_item(Record) :-
	Record = translation(Source, Target, Info, error),
	Target = transfer_from_interlingua_failed(TargetRepresentation),
	member(resolved_source_discourse=SourceDiscourseRepresentation, Info),
	member(interlingua=InterlinguaRepresentation, Info),
	Status = from_interlingua_failed,
	ToStore = batchrec_item(Source, SourceDiscourseRepresentation, InterlinguaRepresentation, TargetRepresentation, Status),
	assertz(ToStore),
	!.

/*

GENERATION FAILED:

translation(
    'suubyou tsuzuki masu ka'+'do you have the headaches when you are stressed',
    generation_failed([[prep,duration_time],[secondary_symptom,pain],[state,last],[tense,present],[timeunit,second],[utterance_type,ynq],[voice,active]]),
    [n_parses=1,source_representation=[[duration,suubyou],[state,tsuzuku],[tense,present],[utterance_type,sentence]],source_discourse=[[utterance_type,sentence],[duration,suubyou],[tense,present],[state,tsuzuku]],resolved_source_discourse=[[utterance_type,sentence],[duration,suubyou],[tense,present],[state,tsuzuku]],resolution_processing=trivial,interlingua=[[timeunit,second],[prep,duration_time],[symptom,pain],[state,last],[tense,present],[voice,active],[utterance_type,ynq]],target_representation=[[prep,duration_time],[secondary_symptom,pain],[state,last],[tense,present],[timeunit,second],[utterance_type,ynq],[voice,active]]],
    error
  ).

*/

store_batchrec_file_item(Record) :-
	Record = translation(Source, Target, Info, error),
	Target = generation_failed(TargetRepresentation),
	member(resolved_source_discourse=SourceDiscourseRepresentation, Info),
	member(interlingua=InterlinguaRepresentation, Info),
	Status = generation_failed,
	ToStore = batchrec_item(Source, SourceDiscourseRepresentation, InterlinguaRepresentation, TargetRepresentation, Status),
	assertz(ToStore),
	!.

/*

BAD TRANSLATION:

translation(
    'itami ga okori mashita ka',
    'did you experience the pain',
    [n_parses=1,source_representation=[[event,okoru],[symptom,itami],[tense,past],[utterance_type,sentence]],source_discourse=[[utterance_type,sentence],[symptom,itami],[tense,past],[event,okoru]],resolved_source_discourse=[[utterance_type,sentence],[symptom,itami],[tense,past],[event,okoru]],resolution_processing=trivial,interlingua=[[pronoun,you],[state,have_symptom],[symptom,pain],[tense,past],[voice,active],[utterance_type,ynq]],target_representation=[[pronoun,you],[secondary_symptom,pain],[state,experience],[tense,past],[utterance_type,ynq],[voice,active]],n_generations=1,other_translations=[]],
    bad
  ).

*/

store_batchrec_file_item(Record) :-
	Record = translation(Source, Target, Info, bad),
	member(resolved_source_discourse=SourceDiscourseRepresentation, Info),
	member(interlingua=InterlinguaRepresentation, Info),
	member(target_representation=TargetRepresentation, Info),
	Status = bad_translation(Target),
	ToStore = batchrec_item(Source, SourceDiscourseRepresentation, InterlinguaRepresentation, TargetRepresentation, Status),
	assertz(ToStore),
	!.

%----------------------------------------------------------------------

make_to_interlingua_list(ToInterlinguaList) :-
	findall(ToInterlinguaItem,
		find_to_interlingua_item(ToInterlinguaItem),
		ToInterlinguaList).

make_from_interlingua_list(FromInterlinguaList) :-
	findall(FromInterlinguaItem,
		find_from_interlingua_item(FromInterlinguaItem),
		FromInterlinguaList).

make_generation_list(GenerationList) :-
	findall(GenerationItem,
		find_bad_generation_item(GenerationItem),
		GenerationList).

make_bad_translation_list(BadTranslationList) :-
	findall(BadTranslationItem,
		find_bad_translation_item(BadTranslationItem),
		BadTranslationList).

find_to_interlingua_item(ToInterlinguaItem) :-
	batchrec_item(Source, SourceRepresentation, InterlinguaRepresentation, _TargetRepresentation, to_interlingua_failed),
	most_similar_targets(InterlinguaRepresentation, interlingua, SimilarTargets),
	ToInterlinguaItem = item(Source, no_translation, SourceRepresentation, InterlinguaRepresentation, SimilarTargets).

find_from_interlingua_item(FromInterlinguaItem) :-
	batchrec_item(Source, _SourceRepresentation, InterlinguaRepresentation, TargetRepresentation, from_interlingua_failed),
	most_similar_targets(TargetRepresentation, target, SimilarTargets),
	FromInterlinguaItem = item(Source, no_translation, InterlinguaRepresentation, TargetRepresentation, SimilarTargets).

find_bad_generation_item(GenerationItem) :-
	batchrec_item(Source, _SourceRepresentation, InterlinguaRepresentation, TargetRepresentation, generation_failed),
	most_similar_targets(TargetRepresentation, target, SimilarTargets),
	GenerationItem = item(Source, no_translation, InterlinguaRepresentation, TargetRepresentation, SimilarTargets).

find_bad_translation_item(BadTranslationItem) :-
	batchrec_item(Source, _SourceRepresentation, InterlinguaRepresentation, TargetRepresentation, bad_translation(Target)),
	most_similar_targets(TargetRepresentation, target, SimilarTargets),
	BadTranslationItem = item(Source, Target, InterlinguaRepresentation, TargetRepresentation, SimilarTargets).

%----------------------------------------------------------------------

write_interlingua_learning_file(List, File) :-
	absolute_file_name(File, AbsFile),
	open(File, write, S),
	write_interlingua_learning_list(List, S),
	close(S),
	length(List, N),
	format('~N--- Written (~d records) ~w~n', [N, AbsFile]).

write_interlingua_learning_list([], _S).
write_interlingua_learning_list([F | R], S) :-
	write_interlingua_learning_list_item(F, S),
	!,
	write_interlingua_learning_list(R, S).

write_interlingua_learning_list_item(item(Source, Target, FromRepresentation, ToRepresentation, SimilarTargets), S) :-
	format(S, '~N', []),
	format(S, '~nitem(~q,', [Source]),
	format(S, '~n     ~q,', [FromRepresentation]),
	format(S, '~n     ~q,', [ToRepresentation]),
	(   Target = no_translation ->
	    true ;
	    format(S, '~n', []),
	    format(S, '~n     % Current translation: "~w"', [Target])
	),
	format(S, '~n', []),
	format(S, '~n     % Similar examples:', []),
	write_similar_targets(SimilarTargets, S),
	format(S, '~n', []),
	(   get_suggested_translation(Source, SuggestedTranslation) ->
	    format(S, '~n     ~q).', [SuggestedTranslation]) ;
	    format(S, '~n     \'?\').', [])
	),
	!.
write_interlingua_learning_list_item(F, S) :-
	format2error('~N*** Error: bad call: ~q~n', [write_interlingua_learning_list_item(F, S)]),
	fail.

write_similar_targets([], _S).
write_similar_targets([[Source, Target] | R], S) :-
	format(S, '~n     % ~w -> ~w', [Source, Target]),
	!,
	write_similar_targets(R, S).

%----------------------------------------------------------------------

init_process_from_interlingua_learning_file(_CFGFile) :-
	retractall(parsing_record(_, _, _)),
	format('~N~n***********************************************************~n', []),
	format('~NLOADING TRANSLATION RULES IN LOCAL IMAGE~n~n', []),
	load_translate_local,
	init_remote_regulus.

conclude_process_from_interlingua_learning_file :-
	conclude_remote_regulus.

%----------------------------------------------------------------------

collect_rules_from_from_interlingua_learning_file(FromInterlinguaFile, FromRulesList, GenerationCorpusList) :-
	absolute_file_name(FromInterlinguaFile, AbsFromInterlinguaFile),
	prolog_file_to_list(AbsFromInterlinguaFile, InList),
	format('~N~n***********************************************************~n', []),
	format('~NTRYING TO INDUCE RULES~n', []),
	collect_rules_from_from_interlingua_learning_list(InList, FromRulesList-[], GenerationCorpusList-[]).

collect_rules_from_from_interlingua_learning_list([], Rules-Rules, Corpus-Corpus).
collect_rules_from_from_interlingua_learning_list([F | R], RulesIn-RulesOut, CorpusIn-CorpusOut) :-
	collect_rules_from_from_interlingua_learning_item0(F, RulesIn-RulesNext, CorpusIn-CorpusNext),
	(   ( R = [] ; R = [item(_SourceSent, _SourceLF, _FailedTargetLF, '?') | _] ) ->
	    true ;
	    format('~N~n------------------------------- END OF EXAMPLE -------------------------------~n', [])
	),
	!,
	collect_rules_from_from_interlingua_learning_list(R, RulesNext-RulesOut, CorpusNext-CorpusOut).

collect_rules_from_from_interlingua_learning_item0(item(SourceSent, Interlingua, FailedTargetLF, TargetSent),
						   RulesIn-RulesOut, CorpusIn-CorpusOut) :-
	TargetSent \== '?',
	collect_rules_from_from_interlingua_learning_item(item(SourceSent, Interlingua, FailedTargetLF, TargetSent),
							  RulesIn-RulesOut, CorpusIn-CorpusOut).
collect_rules_from_from_interlingua_learning_item0(item(_SourceSent, _Interlingua, _FailedTargetLF, '?'),
						   RulesIn-RulesIn, CorpusIn-CorpusIn).

collect_rules_from_from_interlingua_learning_item(item(SourceSent, Interlingua, FailedTargetLF, TargetSent),
						  RulesIn-RulesOut, CorpusIn-CorpusOut) :-
	lf_most_similar_to_failed_target_lf(TargetSent, FailedTargetLF, specialised, MostSimilarLF),
	get_transfer_trace(Interlingua, TransferTrace, from_interlingua),
	induce_transfer_rule(SourceSent, TargetSent, Interlingua, FailedTargetLF, MostSimilarLF, TransferTrace, Rule),
	RulesIn = [Rule | RulesOut],
	CorpusIn = CorpusOut,
	!.
collect_rules_from_from_interlingua_learning_item(item(SourceSent, Interlingua, FailedTargetLF, TargetSent),
						  RulesIn-RulesOut, CorpusIn-CorpusOut) :-
	lf_most_similar_to_failed_target_lf(TargetSent, FailedTargetLF, unspecialised, MostSimilarLF),
	get_transfer_trace(Interlingua, TransferTrace, from_interlingua),
	(   induce_transfer_rule(SourceSent, TargetSent, Interlingua, FailedTargetLF, MostSimilarLF, TransferTrace, Rule) ->
	    RulesIn = [Rule | RulesOut] ;
	    RulesIn = RulesOut
	),
	findall(OtherLF,
		( parsing_record(TargetSent, OtherLF, unspecialised), OtherLF \== MostSimilarLF ),
		OtherLFs),
	induce_disambiguation_constraints(MostSimilarLF, OtherLFs, Constraints),
	CorpusRecord = sent(TargetSent, [default], Constraints),
	format('~N~nInduced generation corpus example:~n', []),
	format('~N~q~n', [CorpusRecord]),
	CorpusIn = [CorpusRecord | CorpusOut],
	!.
collect_rules_from_from_interlingua_learning_item(item(_SourceSent, _Interlingua, _FailedTargetLF, TargetSent),
						  RulesIn-RulesIn, CorpusIn-CorpusIn) :-
	\+ parsing_record(TargetSent, _OtherLF, unspecialised),
	format('~N~n*** Could not parse "~w"~n', [TargetSent]),
	!.
collect_rules_from_from_interlingua_learning_item(_Item,
					     RulesIn-RulesIn, CorpusIn-CorpusIn) :-
	format('~N*** Warning: no rule extracted~n', []),
	!.

%----------------------------------------------------------------------

init_process_to_interlingua_learning_file(_CFGFile) :-
	retractall(parsing_record(_, _, _)),
	retractall(interlingua_record(_, _, _, _)),
	format('~N~n***********************************************************~n', []),
	format('~NLOADING TRANSLATION RULES IN LOCAL IMAGE~n~n', []),
	load_translate_local,
	init_remote_regulus.

conclude_process_to_interlingua_learning_file :-
	conclude_remote_regulus.

collect_rules_from_to_interlingua_learning_file(ToInterlinguaFile, ToRulesList) :-
	absolute_file_name(ToInterlinguaFile, AbsToInterlinguaFile),
	prolog_file_to_list(AbsToInterlinguaFile, InList),
	format('~N~n***********************************************************~n', []),
	format('~NTRYING TO INDUCE RULES~n', []),
	collect_rules_from_to_interlingua_learning_list(InList, ToRulesList-[]).

collect_rules_from_to_interlingua_learning_list([], Rules-Rules).
collect_rules_from_to_interlingua_learning_list([F | R], RulesIn-RulesOut) :-
	collect_rules_from_to_interlingua_learning_item0(F, RulesIn-RulesNext),
	(   ( R = [] ; R = [item(_SourceSent, _SourceLF, _FailedTargetLF, '?') | _] ) ->
	    true ;
	    format('~N~n------------------------------- END OF EXAMPLE -------------------------------~n', [])
	),
	!,
	collect_rules_from_to_interlingua_learning_list(R, RulesNext-RulesOut).

collect_rules_from_to_interlingua_learning_item0(item(SourceSent, SourceLF, FailedTargetLF, TargetSent),
						 RulesIn-RulesOut) :-
	TargetSent \== '?',
	collect_rules_from_to_interlingua_learning_item(item(SourceSent, SourceLF, FailedTargetLF, TargetSent),
							RulesIn-RulesOut).
collect_rules_from_to_interlingua_learning_item0(item(_SourceSent, _SourceLF, _FailedTargetLF, '?'),
						 RulesIn-RulesIn).

collect_rules_from_to_interlingua_learning_item(item(SourceSent, SourceLF, FailedInterlingua, TargetSent),
						RulesIn-RulesOut) :-
	interlingua_most_similar_to_failed_interlingua(TargetSent, FailedInterlingua, unspecialised, MostSimilarInterlingua),
	get_transfer_trace(SourceLF, TransferTrace, to_interlingua),
	induce_transfer_rule(SourceSent, TargetSent, SourceLF, FailedInterlingua, MostSimilarInterlingua, TransferTrace, Rule),
	RulesIn = [Rule | RulesOut],
	!.
collect_rules_from_to_interlingua_learning_item(item(_SourceSent, _SourceLF, _FailedInterlingua, TargetSent),
						RulesIn-RulesIn) :-
	\+ parsing_record(TargetSent, _OtherLF, unspecialised),
	format('~N~n*** Could not parse "~w"~n', [TargetSent]),
	!.
collect_rules_from_to_interlingua_learning_item(item(_SourceSent, _SourceLF, _FailedInterlingua, TargetSent),
						RulesIn-RulesIn) :-
	\+ interlingua_record(TargetSent, _LF, _Interlingua, _GrammarType),
	format('~N~n*** Could not transfer "~w" to interlingua~n', [TargetSent]),
	!.
collect_rules_from_to_interlingua_learning_item(_Item,
						RulesIn-RulesIn) :-
	%format('~N*** Warning: no rule extracted~n', []),
	!.

%----------------------------------------------------------------------

load_grammar(specialised, CFGFile) :-
	remote_regulus_call(user:regulus_batch(CFGFile, ["EBL_LOAD"])),
	!.
load_grammar(unspecialised, CFGFile) :-
	remote_regulus_call(user:regulus_batch(CFGFile, ["LOAD"])),
	!.
load_grammar(GrammarType, CFGFile) :-
	format2error('~N*** Error: bad call: ~w~n', [load_grammar(GrammarType, CFGFile)]),
	fail.

load_translate(CFGFile) :-
	remote_regulus_call(user:regulus_batch(CFGFile, ["LOAD_TRANSLATE"])),
	!.
load_translate(CFGFile) :-
	format2error('~N*** Error: bad call: ~w~n', [load_translate(CFGFile)]),
	fail.

load_translate_local :-
	user:regulus_batch(["LOAD_TRANSLATE"]),
	!.
load_translate_local :-
	format2error('~N*** Error: bad call: ~w~n', [load_translate_local]),
	fail.

%----------------------------------------------------------------------

:- dynamic parsing_record/3.

parse_interlingua_learning_file(InterlinguaFile, CFGFile, GrammarType) :-
	format('~N~n***********************************************************~n', []),
	format('~NLOADING ~w GRAMMAR ON REGULUS LANGUAGE SERVER~n~n', [GrammarType]),
	load_grammar(GrammarType, CFGFile), 
	absolute_file_name(InterlinguaFile, AbsInterlinguaFile),
	prolog_file_to_list(AbsInterlinguaFile, InList),
	length(InList, N),
	non_question_mark_sentences_in_learning_list(InList, Sentences),
	length(Sentences, N1),
	format('~N--- Read learning file (~d/~d annotated records): ~w~n', [N1, N, AbsInterlinguaFile]),

	format('~N~n***********************************************************~n', []),
	format('~NPARSING USING ~w GRAMMAR ON REGULUS LANGUAGE SERVER~n~n', [GrammarType]),
	parse_and_store_sentence_list(Sentences, GrammarType),
	!.
parse_interlingua_learning_file(InterlinguaFile, CFGFile, GrammarType) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [parse_interlingua_learning_file(InterlinguaFile, CFGFile, GrammarType)]),
	fail.

parse_and_store_sentence_list([], _GrammarType).
parse_and_store_sentence_list([F | R], GrammarType) :-
	parse_and_store_sentence(F, GrammarType),
	!,
	parse_and_store_sentence_list(R, GrammarType).

parse_and_store_sentence(Sent, GrammarType) :-
	remote_regulus_call(user:single_top_level_grammar(GrammarAtom)),
	split_atom_into_words(Sent, WordsList),
	remote_regulus_call(findall(LF,
				    user:words_to_lf_and_tree_with_current_parser(WordsList, GrammarAtom, LF, _Tree),
				    LFs)),
	store_lfs_for_sentence_and_grammar_type(LFs, Sent, GrammarType),
	length(LFs, N),
	format('~NParsed "~w" using ~w grammar: ~d parses stored', [Sent, GrammarType, N]),
	(   N = 0 ->
	    report_unknown_words_in_word_list(WordsList) ;
	    true
	),
	!.
parse_and_store_sentence(Sent, GrammarType) :-
	format2error('~N*** Error: bad call: ~w~n', [parse_and_store_sentence(Sent, GrammarType)]),
	fail.

report_unknown_words_in_word_list(WordList) :-
	remote_regulus_call(user:unknown_words_in_word_list(WordList, UnknownWords)),
	(   UnknownWords \== [] ->
	    format(' (words not in current vocabulary: ~w)~n', [UnknownWords]) ;
	    format('~n', [])
	),
	!.
report_unknown_words_in_word_list(WordsList) :-
	format2error('~N*** Error: bad call: ~w~n', [report_unknown_words_in_word_list(WordsList)]),
	fail.

store_lfs_for_sentence_and_grammar_type([], _Sent, _GrammarType).
store_lfs_for_sentence_and_grammar_type([F | R], Sent, GrammarType) :-
	assertz(parsing_record(Sent, F, GrammarType)),
	!,
	store_lfs_for_sentence_and_grammar_type(R, Sent, GrammarType).

%----------------------------------------------------------------------

:- dynamic interlingua_record/4.

parse_and_transfer_interlingua_learning_file(ToInterlinguaFile, CFGFile, GrammarType) :-
	parse_interlingua_learning_file(ToInterlinguaFile, CFGFile, GrammarType),
	transfer_parsed_interlingua_items(CFGFile, GrammarType).

transfer_parsed_interlingua_items(CFGFile, GrammarType) :-
	format('~N~n***********************************************************~n', []),
	format('~NLOADING TRANSLATION RULES ON REGULUS LANGUAGE SERVER~n~n', []),

	load_translate(CFGFile),
	format('~N~n***********************************************************~n', []),
	format('~NTRANSFERRING TO INTERLINGUA ON REGULUS LANGUAGE SERVER~n~n', []),
	findall([Sent, LF],
		parsing_record(Sent, LF, GrammarType),
		Pairs),
	transfer_and_store_parsed_interlingua_items(Pairs, GrammarType).

transfer_and_store_parsed_interlingua_items([], _GrammarType).
transfer_and_store_parsed_interlingua_items([F | R], GrammarType) :-
	transfer_and_store_parsed_interlingua_item(F, GrammarType),
	!,
	transfer_and_store_parsed_interlingua_items(R, GrammarType).

transfer_and_store_parsed_interlingua_item([Sent, LF], GrammarType) :-
	remote_regulus_call((    translate:transfer_representation_to_source_discourse(LF, SourceDiscourse),
				 translate:transfer_representation_to_interlingua(SourceDiscourse, Interlingua)
			    )),
	assertz(interlingua_record(Sent, LF, Interlingua, GrammarType)),
	format('~NTransferred "~w" to interlingua~n', [Sent]),
	!.
transfer_and_store_parsed_interlingua_item([Sent, _LF], _GrammarType) :-
	format('~NFailed to transfer "~w" to interlingua~n', [Sent]),
	!.

%----------------------------------------------------------------------

get_transfer_trace(SourceLF, TransferTrace, to_interlingua) :-
	transfer_representation_to_interlingua(SourceLF, _Interlingua, TransferTrace),
	!.
get_transfer_trace(Interlingua, TransferTrace, from_interlingua) :-
	transfer_representation_from_interlingua(Interlingua, _Target, TransferTrace),
	!.
get_transfer_trace(FromRepresentation, TransferTrace, Type) :-
	format2error('~N*** Error: bad call: ~w~n', [get_transfer_trace(FromRepresentation, TransferTrace, Type)]),
	fail.

%----------------------------------------------------------------------

most_similar_targets(InterlinguaRepresentation, Type, SimilarTargets) :-
	findall(Score-Target,
		good_target_and_similarity_score(InterlinguaRepresentation,
						 Type,
						 Target,
						 Score),
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedList),
	safe_remove_duplicates_preserving_order(SortedList, SortedList1),
	pick_best_from_sorted_list(SortedList1, SimilarTargets).

good_target_and_similarity_score(Representation, Type, [Source, Target], Score) :-
	batchrec_item(Source, _SourceRepresentation, InterlinguaRepresentation, TargetRepresentation, correct(Target)),
	(   Type = interlingua ->
	    OtherRepresentation = InterlinguaRepresentation ;
	    Type = target ->
	    OtherRepresentation = TargetRepresentation
	),
	representation_similarity_score(Representation, OtherRepresentation, Score).

pick_best_from_sorted_list([First, Second, Third | _Rest], [First, Second, Third]) :-
	!.
pick_best_from_sorted_list(List, List) :-
	!.

%----------------------------------------------------------------------

lf_most_similar_to_failed_target_lf(TargetSent, FailedTargetLF, GrammarType, MostSimilarLF) :-
	findall(Score-LF,
		(   parsing_record(TargetSent, LF, GrammarType),
		    representation_similarity_score(FailedTargetLF, LF, Score)
		),						   
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedList),
	safe_remove_duplicates_preserving_order(SortedList, SortedList1),
	SortedList1 = [MostSimilarLF | _],
	!.

%----------------------------------------------------------------------

interlingua_most_similar_to_failed_interlingua(TargetSent, FailedInterlingua, GrammarType, MostSimilarInterlingua) :-
	findall(Score-Interlingua,
		(   interlingua_record(TargetSent, _LF, Interlingua, GrammarType),
		    representation_similarity_score(FailedInterlingua, Interlingua, Score)
		),						   
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedList),
	safe_remove_duplicates_preserving_order(SortedList, SortedList1),
	SortedList1 = [MostSimilarInterlingua | _],
	!.

%----------------------------------------------------------------------

representation_similarity_score(Representation, OtherRepresentation, Score) :-
	canonicalise_representation(Representation, Representation1-[]),
	canonicalise_representation(OtherRepresentation, OtherRepresentation1-[]),
	canonical_representation_similarity_score(Representation1, OtherRepresentation1, Score),
	!.
representation_similarity_score(Representation, OtherRepresentation, Score) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [representation_similarity_score(Representation, OtherRepresentation, Score)]),
	fail.

canonicalise_representation([], R-R).
canonicalise_representation([[clause, Content] | R], [[clause, content] | R1]-ROut) :-
	!,
	canonicalise_representation(Content, R1-RNext),
	canonicalise_representation(R, RNext-ROut).
canonicalise_representation([F | R], [F | R1]-ROut) :-
	canonicalise_representation(R, R1-ROut).

canonical_representation_similarity_score(L1, L2, Score) :-
	list_to_ord_set(L1, OrdL1),
	list_to_ord_set(L2, OrdL2),
	ord_symdiff(OrdL1, OrdL2, Diff),
	length(Diff, Score),
	!.

%----------------------------------------------------------------------

induce_transfer_rule(SourceSent, TargetSent, SourceLF, FailedTargetLF, MostSimilarLF, TransferTrace, Rule) :-
	make_transfer_representation_canonical(FailedTargetLF, FailedTargetLF1),
	make_transfer_representation_canonical(MostSimilarLF, MostSimilarLF1),
	
	format('~N~n--- Trying to induce rule from "~w" -> "~w":~n~n', [SourceSent, TargetSent]),
	format('~N                Source representation: ~q~n', [SourceLF]),
	format('~NIncomplete transferred representation: ~q~n', [FailedTargetLF1]),
	format('~N     Best match from suggested target: ~q~n~n', [MostSimilarLF1]),
	format('~N                       Transfer trace:~n', []),
	prettyprint_transfer_trace(TransferTrace),

	retranslate_source_using_trace(SourceLF, TransferTrace, MostSimilarLF1, FailedTargetLF2),
	format('~N~n', []),
	format('~N     Revised incomplete representation: ~q~n', [FailedTargetLF2]),
	
	(   induce_transfer_rule1(FailedTargetLF2, MostSimilarLF1, Rule) ->
	    format('~N~nInduced rule: ~q~n', [Rule]) ;

	    FailedTargetLF2 = MostSimilarLF1 ->
	    format('~N~nRepresentations are the same: no rule needed~n', []),
	    fail ;
	    
	    format('~N~nNo induced rule found~n', []),
	    fail
	).

induce_transfer_rule1(FailedTargetLF, MostSimilarLF, Rule) :-
	\+ member([clause, _], FailedTargetLF),
	\+ member([clause, _], MostSimilarLF),	    
	list_to_ord_set(FailedTargetLF, FailedTargetLFOS),
	list_to_ord_set(MostSimilarLF, MostSimilarLFOS),
	ord_subtract(FailedTargetLFOS, MostSimilarLFOS, FailedTargetElements),
	ord_subtract(MostSimilarLFOS, FailedTargetLFOS, NewElementsInMostSimilar),
	strip_failed_target_elements(FailedTargetElements, FailedTargetElements1),
	FailedTargetElements1 \== [],
	create_transfer_rule(FailedTargetElements1, NewElementsInMostSimilar, Rule),
	!.
induce_transfer_rule1(FailedTargetLF, MostSimilarLF, Rule) :-
	member([clause, Clause], FailedTargetLF),
	member([clause, Clause], MostSimilarLF),
	delete(FailedTargetLF, [clause, Clause], FailedTargetLF1),
	delete(MostSimilarLF, [clause, Clause], MostSimilarLF1),
	induce_transfer_rule1(FailedTargetLF1, MostSimilarLF1, Rule),
	!.
induce_transfer_rule1(FailedTargetLF, MostSimilarLF, Rule) :-
	member([clause, Clause1], FailedTargetLF),
	member([clause, Clause2], MostSimilarLF),
	delete(FailedTargetLF, [clause, Clause1], FailedTargetLF1),
	delete(MostSimilarLF, [clause, Clause2], MostSimilarLF1),
	permutation(FailedTargetLF1, MostSimilarLF1),
	induce_transfer_rule1(Clause1, Clause2, Rule),
	!.

%----------------------------------------------------------------------

%retranslate_source_using_trace(SourceLF, TransferTrace, MostSimilarLF, FailedTargetLF)

retranslate_source_using_trace(SourceLF, TransferTrace, MostSimilarLF, FailedTargetLF2) :-
	make_trace_canonical(TransferTrace, TransferTrace1),
	retranslate_source_using_trace1(SourceLF, TransferTrace1, MostSimilarLF, FailedTargetLF, non_clause),
	flatten_transfer_representation(FailedTargetLF, FailedTargetLF1),
	make_transfer_representation_canonical(FailedTargetLF1, FailedTargetLF2),
	!.
retranslate_source_using_trace(SourceLF, TransferTrace, MostSimilarLF, FailedTargetLF1) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [retranslate_source_using_trace(SourceLF, TransferTrace, MostSimilarLF, FailedTargetLF1)]),
	fail.

retranslate_source_using_trace1([], _TransferTrace, _MostSimilarLF, [], _ClauseOrNonClause).
retranslate_source_using_trace1([F | R], TransferTrace, MostSimilarLF, [RHS | R1], ClauseOrNonClause) :-
	member([F | RestLHS]-RHS, TransferTrace),
	ord_subtract(R, RestLHS, Rest),
	check_elements_are_in_lf(RHS, MostSimilarLF, ClauseOrNonClause),
	!,
	retranslate_source_using_trace1(Rest, TransferTrace, MostSimilarLF, R1, ClauseOrNonClause).
retranslate_source_using_trace1([[clause, Clause] | R], TransferTrace, MostSimilarLF, [[clause, Clause1] | R1], non_clause) :-
	retranslate_source_using_trace1(Clause, TransferTrace, MostSimilarLF, Clause1, clause),
	!,
	retranslate_source_using_trace1(R, TransferTrace, MostSimilarLF, R1, non_clause).
retranslate_source_using_trace1([F | R], TransferTrace, MostSimilarLF, [unable_to_translate:F | R1], ClauseOrNonClause) :-
	!,
	retranslate_source_using_trace1(R, TransferTrace, MostSimilarLF, R1, ClauseOrNonClause).

%----------------------------------------------------------------------

make_trace_canonical([], []).
make_trace_canonical([F | R], [F1 | R1]) :-
	make_trace_element_canonical(F, F1),
	!,
	make_trace_canonical(R, R1).

make_trace_element_canonical((LHS-RHS):_Tag, (LHS1-RHS1)) :-
	make_transfer_representation_canonical(LHS, LHS1),
	make_transfer_representation_canonical(RHS, RHS1),
	!.
make_trace_element_canonical(X, X).

%----------------------------------------------------------------------

check_elements_are_in_lf(Elements, LF, clause) :-
	member([clause, Clause], LF),
	check_elements_are_in_lf(Elements, Clause, non_clause).
check_elements_are_in_lf(Elements, LF, non_clause) :-
	ord_subset(Elements, LF).

%----------------------------------------------------------------------

strip_failed_target_elements([], []).
strip_failed_target_elements([unable_to_translate:F | R], [F | R1]) :-
	strip_failed_target_elements(R, R1).

create_transfer_rule([L], [R], transfer_lexicon(L, R)) :-
	!.
create_transfer_rule(L, R, transfer_rule(L, R)).

%----------------------------------------------------------------------

induce_disambiguation_constraints(MostSimilarLF, OtherLFs, Constraints) :-
	delete(OtherLFs, MostSimilarLF, OtherLFs1),
	induce_disambiguation_constraints1(MostSimilarLF, OtherLFs1, Constraints),
	!.
induce_disambiguation_constraints(MostSimilarLF, OtherLFs, Constraints) :-
	format2error('~N*** Error: bad call: ~q~n', [induce_disambiguation_constraints(MostSimilarLF, OtherLFs, Constraints)]),
	fail.

induce_disambiguation_constraints1(_MostSimilarLF, [], []).
induce_disambiguation_constraints1(MostSimilarLF, OtherLFs, [lf_includes_structure=Structure | R]) :-
	characteristic_element_in_lf(MostSimilarLF, OtherLFs, Structure, MatchingOtherLFs),
	!,
	induce_disambiguation_constraints1(MostSimilarLF, MatchingOtherLFs, R).
induce_disambiguation_constraints1(MostSimilarLF, OtherLFs, [lf_doesnt_includes_structure=Structure | R]) :-
	characteristic_element_in_other_lf(MostSimilarLF, OtherLFs, Structure, NonMatchingOtherLFs),
	!,
	induce_disambiguation_constraints1(MostSimilarLF, NonMatchingOtherLFs, R).

characteristic_element_in_lf(LF, OtherLFs, Element, MatchingOtherLFs) :-
	length(OtherLFs, N),
	potential_characteristic_element(LF, Element),
	findall(MatchingOtherLF,
		(   member(MatchingOtherLF, OtherLFs),
		    term_contains_subterm(MatchingOtherLF, Element)
		),
		MatchingOtherLFs),
	length(MatchingOtherLFs, N1),
	N1 < N,
	!.

characteristic_element_in_other_lf(LF, OtherLFs, Element, NonMatchingOtherLFs) :-
	member(OtherLF, OtherLFs),
	potential_characteristic_element(OtherLF, Element),
	\+ term_contains_subterm(LF, Element),
	findall(NonMatchingOtherLF,
		(   member(NonMatchingOtherLF, OtherLFs),
		    \+ term_contains_subterm(NonMatchingOtherLF, Element)
		),
		NonMatchingOtherLFs),
	!.

potential_characteristic_element([F | _R], F) :-
	\+ safe_subsumes_chk([clause, _], F).
potential_characteristic_element([[clause, Clause] | _R], Element) :-
	potential_characteristic_element(Clause, Element).
potential_characteristic_element([_F | R], Element) :-
	potential_characteristic_element(R, Element).

%----------------------------------------------------------------------

non_question_mark_sentences_in_learning_list(List, Sentences) :-
	findall(Sentence,
	       (   member(item(_Source, _FromRepresentation, _ToRepresentation, Sentence), List),
		   Sentence \== '?'
	       ),
	       Sentences),
	!.

%----------------------------------------------------------------------

init_remote_regulus :-
	format('~N~n***********************************************************~n', []),
	format('~NSTARTING REGULUS LANGUAGE SERVER~n~n', []),
	port_for_remote_regulus(Port),
	remote_regulus_init(Port).

conclude_remote_regulus :-
	format('~N~n***********************************************************~n', []),
	format('~NCLOSING DOWN REGULUS LANGUAGE SERVER~n~n', []),
	remote_regulus_exit_server.

%----------------------------------------------------------------------

translation_rule_learning_file_pathnames(BatchrecFile, ToInterlinguaFile, FromInterlinguaFile, GenerationFile, BadTranslationsFile, SuggestionsFile) :-
	split_off_extension_from_pathname(BatchrecFile, WithoutExtension, Extension),
	format_to_atom('~w_bad_to_interlingua.~w', [WithoutExtension, Extension], ToInterlinguaFile),
	format_to_atom('~w_bad_from_interlingua.~w', [WithoutExtension, Extension], FromInterlinguaFile),
	format_to_atom('~w_bad_generation.~w', [WithoutExtension, Extension], GenerationFile),
	format_to_atom('~w_bad_translations.~w', [WithoutExtension, Extension], BadTranslationsFile),
	format_to_atom('~w_suggestions.~w', [WithoutExtension, Extension], SuggestionsFile),
	!.
translation_rule_learning_file_pathnames(BatchrecFile, ToInterlinguaFile, FromInterlinguaFile, GenerationFile, BadTranslationsFile, SuggestionsFile) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [translation_rule_learning_file_pathnames(BatchrecFile, ToInterlinguaFile, FromInterlinguaFile, GenerationFile, BadTranslationsFile, SuggestionsFile)]),
	fail.

translation_rule_learned_file_pathnames(LearningFile, RulesFile, GenerationCorpusFile) :-
	split_off_extension_from_pathname(LearningFile, WithoutExtension, Extension),
	format_to_atom('~w_rules.~w', [WithoutExtension, Extension], RulesFile),
	format_to_atom('~w_generation_corpus.~w', [WithoutExtension, Extension], GenerationCorpusFile),
	!.
translation_rule_learned_file_pathnames(LearningFile, RulesFile, GenerationCorpusFile) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [translation_rule_learned_file_pathnames(LearningFile, RulesFile, GenerationCorpusFile)]),
	fail.

%----------------------------------------------------------------------

write_new_transfer_rules_file(FromRulesList, NewFromRulesFile) :-
	absolute_file_name(NewFromRulesFile, AbsNewFromRulesFile),
	safe_remove_duplicates_preserving_order(FromRulesList, FromRulesList1),
	length(FromRulesList1, N),
	list_to_prolog_file(FromRulesList1, AbsNewFromRulesFile),
	format('~N--- Written (~d records) ~w~n', [N, AbsNewFromRulesFile]),
	!.

write_new_generation_corpus_file(GenerationCorpusList, NewGenerationCorpusFile) :-
	absolute_file_name(NewGenerationCorpusFile, AbsNewGenerationCorpusFile),
	length(GenerationCorpusList, N),
	list_to_prolog_file(GenerationCorpusList, AbsNewGenerationCorpusFile),
	format('~N--- Written (~d records) ~w~n', [N, AbsNewGenerationCorpusFile]),
	!.

%----------------------------------------------------------------------

:- dynamic translation_suggestion/2.

read_and_store_suggestions_file(SuggestionsFile) :-
	retractall(translation_suggestion(_, _)),
	absolute_file_name(SuggestionsFile, AbsSuggestionsFile),
	(   file_exists(AbsSuggestionsFile) ->
	    
	    prolog_file_to_list(AbsSuggestionsFile, List),
	    length(List, N),
	    format('~N--- Read stored translation suggestions file (~d records) ~w~n', [N, AbsSuggestionsFile]),
	    store_suggestions_list(List) ;

	    format('~N--- No stored translation suggestions found~n', [])
	),
	!.
read_and_store_suggestions_file(SuggestionsFile) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [read_and_store_suggestions_file(SuggestionsFile)]),
	fail.

store_suggestions_list([]).
store_suggestions_list([F | R]) :-
	store_suggestions_item(F),
	!,
	store_suggestions_list(R).

store_suggestions_item(F) :-
	assertz(F),
	!.

store_translation_suggestions_from_interlingua_learning_file(File) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	store_translation_suggestions_from_interlingua_learning_list(List),
	!.
store_translation_suggestions_from_interlingua_learning_file(File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_translation_suggestions_from_interlingua_learning_file(File)]),
	fail.

store_translation_suggestions_from_interlingua_learning_list([]).
store_translation_suggestions_from_interlingua_learning_list([F | R]) :-
	store_translation_suggestions_from_interlingua_learning_item(F),
	!,
	store_translation_suggestions_from_interlingua_learning_list(R).

store_translation_suggestions_from_interlingua_learning_item(Item) :-
	Item = item(Source, _FromRepresentation, _ToRepresentation, Target),
	Target \== '?',
	retractall(translation_suggestion(Source, _AnyTarget)),
	assertz(translation_suggestion(Source, Target)),
	!.
store_translation_suggestions_from_interlingua_learning_item(_Item).

get_suggested_translation(Source, SuggestedTranslation) :-
	translation_suggestion(Source, SuggestedTranslation),
	!.
get_suggested_translation(Source, SuggestedTranslation) :-
	atom(Source),
	translation_suggestion(Source+_Context, SuggestedTranslation),
	!.
get_suggested_translation(Source+_Context, SuggestedTranslation) :-
	translation_suggestion(Source, SuggestedTranslation),
	!.

write_suggestions_file_from_stored_data(SuggestionsFile) :-
	absolute_file_name(SuggestionsFile, AbsSuggestionsFile),
	findall(translation_suggestion(From, To),
		translation_suggestion(From, To),
		Suggestions),
	length(Suggestions, N),
	(   N > 0 ->
	    
	    list_to_prolog_file(Suggestions, AbsSuggestionsFile),
	    format('~N--- Written stored translation suggestions file (~d records) ~w~n', [N, AbsSuggestionsFile]) ;

	    true
	).
write_suggestions_file_from_stored_data(SuggestionsFile) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [write_suggestions_file_from_stored_data(SuggestionsFile)]),
	fail.
	






	