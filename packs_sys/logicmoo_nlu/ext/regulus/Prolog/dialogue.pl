
:- module(dialogue,
	[process_lf_for_dialogue/2,
	 initialise_dialogue_state/0,
	 initialise_dialogue_state/1,

	 %set_last_init_dialogue_state_parameter/1,
	 %get_last_init_dialogue_state_parameter/1,
	 
	 get_current_dialogue_state/1,
	 set_current_dialogue_state/1,
	 
	 get_current_namespace_from_dialogue_state/1,

	 dialogue_process_lf/4,
	 dialogue_process_sent_and_dialogue_move/5,
	 dialogue_process_utterance/4,
	 dialogue_process_nbest_list/5,

	 dialogue_process_utterance_for_java_gui/2,

	 dialogue_process_file/2,

	 batch_dialogue_file_to_paraphrase_csv_file/2,

	 warn_if_dialogue_resources_not_loaded/0,

	 recognise_and_dialogue_process_file/9,

	 set_batch_dialogue_printing_format/1,

	 print_dialogue_processing_record/2,
	 
	 load_dialogue_corpus_judgements_file/1,
	 update_dialogue_judgements/4
	]).

:- use_module('$REGULUS/Prolog/generate').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/nbest').
:- use_module('$REGULUS/Prolog/recognition').
:- use_module('$REGULUS/Prolog/logging').
:- use_module('$REGULUS/Prolog/java_gui_utils').
:- use_module('$REGULUS/Prolog/speech_output').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
%:- use_module(library(assoc)).
:- use_module(library(terms)).
:- use_module(library(timeout)).

%---------------------------------------------------------------

% Max paraphrase generation time in seconds.
paraphrase_generation_time_limit(2).

% Max dialogue processing time in seconds.
dialogue_processing_time_limit(TimeLimit) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(dialogue_processing_time_limit, TimeLimit0),
	(   number(TimeLimit0) ->
	    TimeLimit = TimeLimit0 ;
	    format2error('~N*** Error: generation time limit = ~w, should be a number. Using default value.~n', [TimeLimit0]),
	    fail
	),
	!.
dialogue_processing_time_limit(10).

%---------------------------------------------------------------

/*
Called by the top loop. Invoke each stage of dialogue processing in
turn, storing the context in the Prolog database.
*/
process_lf_for_dialogue(SemValue0, WordList) :-
	get_current_dialogue_state(InState),
	(   current_predicate(PrintPackage:print_dialogue_state/1) ->
		
	    format('~N~n      Old state: ~n', []),
	    PrintPackage:print_dialogue_state(InState)
	;
	    assoc_generics_to_lists_in_term(InState, InState1),
	    nl, prettyprint_term_with_intro_grounded('      Old state', InState1)
	),
	Record = [in_state=InState, parse=SemValue0 | Record1],
	    
	prettyprint_term_with_intro_grounded('             LF', SemValue0), 
	statistics(runtime, [T0, _]),

	(   current_predicate(LFResPackage:resolve_lf/4) ->

	    (   LFResPackage:resolve_lf(SemValue0, InState, SemValue, Substitutions) ->
		prettyprint_term_with_intro_grounded('    Resolved LF', SemValue),
		prettyprint_term_with_intro_grounded('     Resolution', Substitutions),
		Record1 = [resolved_lf=SemValue, resolution=Substitutions | Record2]
	    ;

		format('~N~nCall to resolve_lf/4 failed~n', []),
		fail
	    )
	;
	    SemValue = SemValue0,
	    Record1 = Record2
	),	    
	    
	(   current_predicate(InputPackage:lf_to_dialogue_move/5) ->
		
	    (   InputPackage:lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove, IntermediateResults) ->
		true
	    ;
		format('~N~nCall to lf_to_dialogue_move/5 failed~n', []),
		fail
	    )
	;

	    (   current_predicate(InputPackage:lf_to_dialogue_move/4) ->

		IntermediateResults = [],
		(   InputPackage:lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove) ->
		    true
		;
		    format('~N~nCall to lf_to_dialogue_move/4 failed~n', []),
		    fail
		)
	    )
	;
	    (   current_predicate(InputPackage:lf_to_dialogue_move/3) ->

		IntermediateResults = [],
		(   InputPackage:lf_to_dialogue_move(SemValue, InState, DialogueMove) ->
		    true
		;
		    format('~N~nCall to lf_to_dialogue_move/3 failed~n', []),
		    fail
		)
	    )
	;
	    (   current_predicate(InputPackage:lf_to_dialogue_move/2) ->

		IntermediateResults = [],
		(   InputPackage:lf_to_dialogue_move(SemValue, DialogueMove) ->
		    true
		;
		    format('~N~nCall to lf_to_dialogue_move/2 failed~n', []),
		    fail
		)
	    )
	;
	    format('~N~nlf_to_dialogue_move/{2,3,4,5} not defined~n', []),
	    fail
	),

	add_dialogue_move_and_intermediate_results_to_record(DialogueMove, IntermediateResults, Record3, Record2),

	prettyprint_intermediate_result_terms_with_intro_grounded(IntermediateResults, 1),
	    
	prettyprint_term_with_intro_grounded('  Dialogue move', DialogueMove),
	
	(   current_predicate(DMResPackage:resolve_dialogue_move/3) ->

	    (   DMResPackage:resolve_dialogue_move(DialogueMove, InState, ResolvedDialogueMove) ->
		prettyprint_term_with_intro_grounded('  Resolved move', ResolvedDialogueMove),
		Record3 = [resolved_dialogue_move=ResolvedDialogueMove | Record4]
	    ;

		format('~N~nCall to resolve_dialogue_move/3 failed~n', []),
		fail
	    )
	;
	    DialogueMove = ResolvedDialogueMove,
	    Record3 = Record4
	),

	(   current_predicate(paraphrase:generate_paraphrase/3) ->

	    (   paraphrase_from_dialogue_move(ResolvedDialogueMove, Paraphrase) ->
		prettyprint_term_with_intro_grounded('     Paraphrase', Paraphrase),
		Record4 = [paraphrase=Paraphrase | Record5]
	    ;

		format('~N~nCall to paraphrasing failed~n', []),
		fail
	    )
	;
	    Record4 = Record5
	),

	(   current_predicate(DMPackage:update_dialogue_state/4) ->
		
	    (   DMPackage:update_dialogue_state(ResolvedDialogueMove, InState, AbstractAction, OutState) ->
		Record5 = [abstract_action_and_out_state=[AbstractAction, OutState] | Record6]
	    ;
		format('~N~nCall to update_dialogue_state/4 failed~n', []),
		fail
	    )
	;

	    current_predicate(DMPackage:update_dialogue_state/5) ->
		
	    (   DMPackage:update_dialogue_state(ResolvedDialogueMove, SemValue, InState, AbstractAction0, OutState0) ->
		(   current_predicate(DMPackage:update_dialogue_state_postprocess/4) ->
		    DMPackage:update_dialogue_state_postprocess(AbstractAction0, OutState0, AbstractAction, OutState)
		;
		    otherwise ->
		    AbstractAction0 = AbstractAction,
		    OutState0 = OutState
		),
		Record5 = [abstract_action_and_out_state=[AbstractAction, OutState] | Record6]
	    ;
		format('~N~nCall to update_dialogue_state/5 failed~n', []),
		fail
	    )
	;
	    format('~N~n update_dialogue_state/{4,5} not defined~n', []),
	    fail
	),
	    
	prettyprint_term_with_intro_grounded('Abstract action', AbstractAction),
	    
	(   current_predicate(OutputPackage:abstract_action_to_action/2) ->
		
	    (   OutputPackage:abstract_action_to_action(AbstractAction, Action) ->
		Record6 = [action=Action]
	    ;
		format('~N~nCall to abstract_action_to_action/2 failed~n', []),
		fail
	    )
	;
	    format('~N~n abstract_action_to_action/2 is not defined~n', []),
	    fail
	),
	    
	convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	format('~NConcrete action: ~w', [PrintFormForAction]), 

	(   current_predicate(PrintPackage:print_dialogue_state/1) ->
		
	    format('~N~n      New state: ~n', []),
	    PrintPackage:print_dialogue_state(OutState) ;

	    assoc_generics_to_lists_in_term(OutState, OutState1),
	    nl, prettyprint_term_with_intro_grounded('      New state', OutState1)
	),

	set_current_dialogue_state(OutState),

	(   current_predicate(nbest_features:feature_weight/2) ->
	    nbest_preferences_score_for_dialogue_record(Record, TotalScore, PrefTrace),
	    format('~N~nN-BEST FEATURES AND SCORES:~n~n', []),
	    print_nbest_preference_info(PrefTrace, user),
	    format(user, '~N~nTotal score: ~2f~n', [TotalScore])
	;
	    true
	),

	speak_dialogue_processing_result_if_appropriate(Action),
	print_dialogue_processing_result_if_appropriate(Action),
	
	statistics(runtime, [T1, _]),
	Time is ( T1 - T0 ) / 1000,
	format('~N~nDialogue processing time: ~2f seconds~n', [Time]),
	!.

prettyprint_intermediate_result_terms_with_intro_grounded([], _I) :-
	!.
prettyprint_intermediate_result_terms_with_intro_grounded([F | R], I) :-
	format_to_atom(' Intermediate ~d', [I], Tag),
	prettyprint_term_with_intro_grounded(Tag, F),
	I1 is I + 1,
	!,
	prettyprint_intermediate_result_terms_with_intro_grounded(R, I1).
prettyprint_intermediate_result_terms_with_intro_grounded(IntermediateResults, I) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [prettyprint_intermediate_result_terms_with_intro_grounded(IntermediateResults, I)]),
	fail.

%---------------------------------------------------------------

print_dialogue_processing_result_if_appropriate(Call) :-
	printable_dialogue_processing_result(Call),
	format('~N~n****************** TEXT RESPONSE ******************************~n', []),
	print_dialogue_processing_result(Call),
	format('~N~n**************** END TEXT RESPONSE ****************************~n~n', []),
	!.
print_dialogue_processing_result_if_appropriate(_Other).

printable_dialogue_processing_result([F | R]) :-
	(   printable_dialogue_processing_result(F)
	;
	    printable_dialogue_processing_result(R)
	).
printable_dialogue_processing_result(print(Arg)) :-
	(   is_prolog_string(Arg)
	;
	    atom(Arg)
	).
printable_dialogue_processing_result(format(Format, Args)) :-
	(   is_prolog_string(Format)
	;
	    atom(Format)
	),
	is_list(Args).
printable_dialogue_processing_result(display_file(Arg)) :-
	atom(Arg).

print_dialogue_processing_result([F | R]) :-
	print_dialogue_processing_result(F),
	print_dialogue_processing_result(R).
print_dialogue_processing_result(print(Arg)) :-
	(   is_prolog_string(Arg) ->
	    String = Arg
	;
	    atom(Arg) ->
	    atom_codes(Arg, String)
	;
	    String = "Sorry, something went wrong"
	),
	format('~N~n~s', [String]),
	!.
print_dialogue_processing_result(format(Format, Args)) :-
	format(Format, Args),
	!.
print_dialogue_processing_result(display_file(File)) :-
	(   ( safe_absolute_file_name(File, AbsFile), safe_file_exists(AbsFile) ) ->
	    copy_file_to_stream(AbsFile, user)
	;
	    otherwise ->
	    format('~N~n*** Error: unsuccessful attempt to print file ~w~n', [File])
	),
	!.
print_dialogue_processing_result(_Other) :-
	!.

%---------------------------------------------------------------

dialogue_process_utterance_for_java_gui(SourceChars, AnswerTerm) :-
	sleep_if_sicstus4_and_using_gui,
	is_prolog_string(SourceChars),
	atom_codes(SourceAtom, SourceChars),
	log_event('DIALOGUE_PROCESS_UTTERANCE', [SourceAtom]),
	format('~N--- Dialogue process utterance: "~w"~n', [SourceAtom]),

	get_current_dialogue_state(InContext),
	safe_dialogue_process_utterance(SourceAtom, InContext, OutContext, Record),
	set_current_dialogue_state(OutContext),

	(   get_assoc_generic(action, Record, Action) ->
	    speak_dialogue_processing_result_if_appropriate(Action)
	;
	    true
	),

	package_dialogue_processing_result_for_java_gui(Record, AnswerTerm),
	
	convert_strings_to_quoted_atoms(AnswerTerm, AnswerTerm1),
	format('~N--- Dialogue process utterance returning:~n~n', []),
	prettyprint(AnswerTerm1),
	!.

safe_dialogue_process_utterance(Sent, InContext, OutContext, Record) :-
	on_exception(Exception,
		     dialogue_process_utterance(Sent, InContext, OutContext, Record),
		     handle_exception_in_process_dialogue_utterance(Exception, Record)
		    ).

handle_exception_in_process_dialogue_utterance(Exception, Record) :-
	inform_about_top_level_regulus_error(Exception),
	Record = error.

%---------------------------------------------------------------

package_dialogue_processing_result_for_java_gui(Record, AnswerTerm) :-
	remove_judgements_from_record(Record, Record1),
	tidy_up_dialogue_processing_result(Record1, Record2),
	package_processing_alist_for_java_gui(Record2, Record3),
	AnswerTerm =.. [dialogue_info | Record3],
	!.
package_dialogue_processing_result_for_java_gui(Record, AnswerTerm) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [package_dialogue_processing_result_for_java_gui(Record, AnswerTerm)]),
	fail.

tidy_up_dialogue_processing_result(ResultIn, ResultOut) :-
	delete_word_list(ResultIn, ResultNext1),
	expand_abstract_action_and_out_state(ResultNext1, ResultNext2),
	assoc_generics_to_lists_in_term(ResultNext2, ResultNext3),
	consolidate_action_items(ResultNext3, ResultOut),
	!.

delete_word_list(ResultIn, ResultOut) :-
	member(word_list=Words, ResultIn),
	delete(ResultIn, word_list=Words, ResultOut),
	!.
delete_word_list(ResultIn, ResultIn).

expand_abstract_action_and_out_state(ResultIn, ResultOut) :-
	member(abstract_action_and_out_state=[AbstractAction, OutState], ResultIn),
	delete(ResultIn, abstract_action_and_out_state=[AbstractAction, OutState], ResultNext),
	append(ResultNext, [abstract_action=AbstractAction, out_state=OutState], ResultOut),
	!.
expand_abstract_action_and_out_state(ResultIn, ResultIn).

consolidate_action_items(ResultIn, ResultOut) :-
	member(action=Action, ResultIn),
	member(readable(action)=ReadableAction, ResultIn),
	delete(ResultIn, action=Action, ResultNext1),
	delete(ResultNext1, readable(action)=ReadableAction, ResultNext2),
	append(ResultNext2, [action=ReadableAction], ResultOut),
	!.
consolidate_action_items(ResultIn, ResultIn).

%---------------------------------------------------------------
/* 

dialogue_process_utterance(+Sent, +InContext, -OutContext, -Record)

Sent is an atom
InContext and OutContext are contexts
Record is an alist

*/

dialogue_process_utterance(Sent, InContext, OutContext, Record) :-
	empty_assoc_generic(InResults),
	dialogue_process_item_normal(sent(Sent), _S, not_in_nbest,
				     InContext-OutContext0, InResults-_OutResults, '*no_paraphrase*'-_OutParaphrase,
				     Record0),
	maybe_dialogue_postprocess(Record0, OutContext0, Record, OutContext).
	
%---------------------------------------------------------------
/* 

dialogue_process_lf(+LF, +InContext, -OutContext, -Record)

LF is a term
InContext and OutContext are contexts
Record is an alist

*/

dialogue_process_lf(LF, InContext, OutContext, Record) :-
	empty_assoc_generic(InResults),
	dialogue_process_item_normal(lf(LF), _S, not_in_nbest,
				     InContext-OutContext0, InResults-_OutResults, '*no_paraphrase*'-_OutParaphrase,
				     Record0),
	maybe_dialogue_postprocess(Record0, OutContext0, Record, OutContext).
	
%---------------------------------------------------------------
/* 

dialogue_process_sent_and_dialogue_move(+Sent, +DialogueMove, +InContext, -OutContext, -Record)

Sent is an atom
DialogueMove is a term
InContext and OutContext are contexts
Record is an alist

*/

dialogue_process_sent_and_dialogue_move(Sent, DialogueMove, InContext, OutContext, Record) :-
	empty_assoc_generic(ResultsIn),
	split_atom_into_words(Sent, WordList),
	SemValue = no_lf,
	Record0 = [sent=Sent, word_list=WordList | RestRecord],
	dialogue_process_item_main_from_dialogue_move(DialogueMove, WordList, SemValue,
						      InContext-OutContext0, ResultsIn-_ResultsOut, '*no_paraphrase*'-_ParaphraseOut,
						      RestRecord),
	maybe_dialogue_postprocess(Record0, OutContext0, Record, OutContext).
	
%---------------------------------------------------------------
 
/*

dialogue_process_nbest_list(+NBestList, +InContext, -OutContext, -Record, -RankOfChosenElement)

NBestList is a list of items of the form nbest_hyp(Confidence, Sent), where Confidence is a number and Sent an atom
InContext and OutContext are contexts
Record is an alist
RankOfChosenElement is a number

*/

dialogue_process_nbest_list(NBestList, InContext, ChosenOutContext, ChosenRecord, RankOfChosenElement) :-
	findall([record=FullRecord, context=OutContext],
		(   safe_nth(Rank, NBestList, NBestListElement),
		    NBestListElement = nbest_hyp(Confidence, SentOrStringAndLF),
		    (   atom(SentOrStringAndLF) ->
			HypToProcess = sent(SentOrStringAndLF)
		    ;
			SentOrStringAndLF = string_and_lf(_, _) ->
			HypToProcess = SentOrStringAndLF
		    ;
			format('~N*** Error: bad nbest list element: ~w~n', [NBestListElement]),
			fail
		    ),
		    %dialogue_process_utterance(Sent, InContext, OutContext, Record),
		    empty_assoc_generic(InResults),
		    dialogue_process_item_normal(HypToProcess, _S, in_nbest,
						 InContext-OutContext, InResults-_OutResults, '*no_paraphrase*'-_OutParaphrase,
						 Record),
		    FullRecord = [rank=Rank, confidence=Confidence | Record]
		),
		Tuples),
	nbest_preferences(Tuples,
			  [record=ChosenRecord0, context=ChosenOutContext0],
			  Trace,
			  RankOfChosenElement),
	maybe_dialogue_postprocess(ChosenRecord0, ChosenOutContext0, ChosenRecord, ChosenOutContext),
	maybe_extract_and_print_training_data(Trace, RankOfChosenElement),
	!.

%---------------------------------------------------------------

% We may want to delay expensive process until after we've done n-best preferebces,
% hence this hook.

maybe_dialogue_postprocess(Record0, InContext, Record, OutContext) :-
	current_predicate(DMPackage:update_dialogue_state_postprocess/4),
	get_abstract_action_from_record(Record0, AbsAct0),
	remove_items_from_abstract_action_on_from_record(Record0, Record0Init),
	DMPackage:update_dialogue_state_postprocess(AbsAct0, InContext, AbsAct, OutContext),
	empty_assoc_generic(InResults),
	dialogue_process_item_main_from_abstract_action(AbsAct, InResults-_OutResults, RestRecord),
	append(Record0Init,
	       [abstract_action_and_out_state=[AbsAct, OutContext, _Judgement] | RestRecord],
	       Record).
maybe_dialogue_postprocess(Record, InContext, Record, InContext).

get_abstract_action_from_record(Record, AbsAct) :-
	member(abstract_action_and_out_state=[AbsAct, _OutContext, _Judgement], Record).

remove_items_from_abstract_action_on_from_record([], []).
remove_items_from_abstract_action_on_from_record([abstract_action_and_out_state=_ | R], R).
remove_items_from_abstract_action_on_from_record([F | R], [F | R1]) :-
	remove_items_from_abstract_action_on_from_record(R, R1).
	
%---------------------------------------------------------------

recognise_and_dialogue_process_file(
	NewOrStoredRecognitionResults, _TranscriptionsFile, _RecParams, _OutFile, _WavfilesFile, _BatchrecTraceFile, 
	_PrologBatchrecFile, _PrologBatchrecFileWithTranscriptions, _TmpDirectory) :-
	\+ member(NewOrStoredRecognitionResults, [new_recognition_results, stored_recognition_results]),
	!,
	format2error('~N*** Error: bad call to recognise_and_dialogue_process_file/9. First arg was ~w, must be "new_recognition_results" or "stored_recognition_results"~n', [NewOrStoredRecognitionResults]),
	fail.				    
recognise_and_dialogue_process_file(
	NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, OutFile, WavfilesFile, BatchrecTraceFile, 
	PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, TmpDirectory) :-

	timed_call(recognise_and_dialogue_process_file1(NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, OutFile,
							WavfilesFile, BatchrecTraceFile, 
							PrologBatchrecFile, PrologBatchrecFileWithTranscriptions,
							TmpDirectory),
		   TimeTaken),
	format('~NProcessing finished, ~2f secs~n~n', [TimeTaken]).

recognise_and_dialogue_process_file1(
	new_recognition_results, TranscriptionsFile, RecParams, OutFile, WavfilesFile, BatchrecTraceFile, 
	PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, TmpDirectory) :-

	transcriptions_file_to_wavfiles_file(TranscriptionsFile, WavfilesFile),

	do_batchrec(WavfilesFile, TranscriptionsFile, RecParams, BatchrecTraceFile, PrologBatchrecFile, TmpDirectory),
	add_transcriptions_to_prolog_batchrec_file(PrologBatchrecFile, TranscriptionsFile, PrologBatchrecFileWithTranscriptions),

	dialogue_process_prolog_batchrec_file_with_transcriptions(PrologBatchrecFileWithTranscriptions, OutFile).
recognise_and_dialogue_process_file1(
	stored_recognition_results, _TranscriptionsFile, _RecParams, OutFile, _WavfilesFile, _BatchrecTraceFile, 
	_PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, _TmpDirectory) :-

	safe_absolute_file_name(PrologBatchrecFileWithTranscriptions, AbsPrologBatchrecFileWithTranscriptions),
	format('~N--- Reading stored recognition results from ~w~n', [AbsPrologBatchrecFileWithTranscriptions]),
	
	dialogue_process_prolog_batchrec_file_with_transcriptions(PrologBatchrecFileWithTranscriptions, OutFile).

dialogue_process_prolog_batchrec_file_with_transcriptions(PrologBatchrecFileWithTranscriptions, OutFile) :-
	prolog_file_to_list(PrologBatchrecFileWithTranscriptions, PrologBatchrecListWithTranscriptions),
	
	zero_nbest_training_data_file_if_necessary(File),
	(   File \== no_file ->
	    format('~N~n--- Writing out N-best training data to: ~w~n~n', [File])
	;
	    true
	),
	
	open(OutFile, write, OutS),
	dialogue_process_items(PrologBatchrecListWithTranscriptions, OutS),
	close(OutS).	

%---------------------------------------------------------------

% Do batch dialogue processing on one or more files, writing out the results to OutFile
dialogue_process_file(FileOrFiles, OutFile) :-
	timed_call(dialogue_process_file1(FileOrFiles, OutFile),
		   TimeTaken),
	format('~NProcessing finished, ~2f secs~n~n', [TimeTaken]).

%dialogue_process_file1(FileOrFiles, OutFile) :-
%	read_file_or_files(FileOrFiles, Items),
%	open(OutFile, write, OutS),
%	
%	dialogue_process_items(Items, OutS),
%	
%	close(OutS),
%
%	make_cvs_summary_of_dialogue_output_file(OutFile).

dialogue_process_file1(FileOrFiles, OutFile) :-
	add_file_wrapper(FileOrFiles, Items),
	open(OutFile, write, OutS),
	
	dialogue_process_items(Items, OutS),
	
	close(OutS),

	make_cvs_summary_of_dialogue_output_file(OutFile).

add_file_wrapper([], []) :-
	!.
add_file_wrapper([F | R], [file(F) | R1]) :-
	!,
	add_file_wrapper(R, R1).
add_file_wrapper(File, [file(File)]).

%read_file_or_files(FileOrFiles, Items) :-
%	atomic(FileOrFiles),
%	read_file_or_files([FileOrFiles], Items),
%	!.
%read_file_or_files(FileOrFiles, Items) :-
%	is_list(FileOrFiles),
%	prolog_files_to_lists(FileOrFiles, ItemsList),
%	append_list(ItemsList, Items),
%	!.
%
%prolog_files_to_lists([], []).
%prolog_files_to_lists([F | R], [F1 | R1]) :-
%	safe_absolute_file_name(F, AbsF),
%	prolog_file_to_list(AbsF, F1),
%	format('~NProcessing file ~w~n', [AbsF]),
%	!,
%	prolog_files_to_lists(R, R1).

%---------------------------------------------------------------

dialogue_process_items(Items, OutS) :-
	empty_assoc_generic(ResultsIn),
	
	(   current_predicate(Package:initial_dialogue_state/1) ->
	    
	    Package:initial_dialogue_state(ContextIn) ;
	    
	    format2error('~N*** Error: initial_dialogue_state/1 not defined~n', []),
	    fail
	),

	dialogue_process_items1(Items, OutS, ContextIn-_ContextOut, ResultsIn-ResultsOut, '*no_paraphrase*'-_OutParaphrase, 0-_NOut),
	dialogue_process_items_print_summary(user, ResultsOut),
	format(OutS, '~N~n/*~n', []), %*/ to keep Emacs happy
	dialogue_process_items_print_summary(OutS, ResultsOut),
	format(OutS, '~N*/~n', []),
	!.

dialogue_process_items_print_summary(S, ResultsOut) :-
	get_assoc_generic_or_zero(processed, ResultsOut, NProcessed),
	get_assoc_generic_or_zero(no_parse, ResultsOut, NNoParse),
	get_assoc_generic_or_zero(im(good), ResultsOut, NIMGood),
	get_assoc_generic_or_zero(dm(good), ResultsOut, NDMGood),
	get_assoc_generic_or_zero(om(good), ResultsOut, NOMGood),
	get_assoc_generic_or_zero(paraphrase(good), ResultsOut, NParaphraseGood),
	get_assoc_generic_or_zero(im(ok), ResultsOut, NIMOK),
	get_assoc_generic_or_zero(dm(ok), ResultsOut, NDMOK),
	get_assoc_generic_or_zero(om(ok), ResultsOut, NOMOK),
	get_assoc_generic_or_zero(paraphrase(ok), ResultsOut, NParaphraseOK),	
	get_assoc_generic_or_zero(im(?), ResultsOut, NIMUnknown),
	get_assoc_generic_or_zero(dm(?), ResultsOut, NDMUnknown),
	get_assoc_generic_or_zero(om(?), ResultsOut, NOMUnknown),
	get_assoc_generic_or_zero(paraphrase(?), ResultsOut, NParaphraseUnknown),	
	get_assoc_generic_or_zero(im(bad), ResultsOut, NIMBad),
	get_assoc_generic_or_zero(dm(bad), ResultsOut, NDMBad),
	get_assoc_generic_or_zero(om(bad), ResultsOut, NOMBad),
	get_assoc_generic_or_zero(paraphrase(bad), ResultsOut, NParaphraseBad),	
	get_assoc_generic_or_zero(im(no_result), ResultsOut, NIMNoResult),
	get_assoc_generic_or_zero(dm(no_result), ResultsOut, NDMNoResult),
	get_assoc_generic_or_zero(om(no_result), ResultsOut, NOMNoResult),
	get_assoc_generic_or_zero(paraphrase(no_result), ResultsOut, NParaphraseNoResult),	
	get_assoc_generic_or_zero(sem_recognition_measured, ResultsOut, NSemRecognitionMeasured0),
	get_assoc_generic_or_zero(sem_recognition_ok, ResultsOut, NSemRecognitionOK),
	get_assoc_generic_or_zero(source_words, ResultsOut, NSourceWords0),
	get_assoc_generic_or_zero(word_errors, ResultsOut, NWordErrors),
	get_assoc_generic_or_zero(oov_words, ResultsOut, NOOVWords),

	NAll0 is NProcessed,
	(   NAll0 = 0 ->
	    NAll = 1
	;
	    NAll = NAll0
	),
	(   NSemRecognitionMeasured0 = 0 ->
	    NSemRecognitionMeasured = 1
	;
	    NSemRecognitionMeasured = NSemRecognitionMeasured0
	),
	(   NSourceWords0 = 0 ->
	    NSourceWords = 1
	;
	    NSourceWords = NSourceWords0
	),

	PCNoParse is 100 * NNoParse / NAll,
	
	PCIMGood is 100 * NIMGood / NAll,
	PCDMGood is 100 * NDMGood / NAll,
	PCOMGood is 100 * NOMGood / NAll,
	PCParaphraseGood is 100 * NParaphraseGood / NAll,

	PCIMOK is 100 * NIMOK / NAll,
	PCDMOK is 100 * NDMOK / NAll,
	PCOMOK is 100 * NOMOK / NAll,
	PCParaphraseOK is 100 * NParaphraseOK / NAll,

	PCIMUnknown is 100 * NIMUnknown / NAll,
	PCDMUnknown is 100 * NDMUnknown / NAll,
	PCOMUnknown is 100 * NOMUnknown / NAll,
	PCParaphraseUnknown is 100 * NParaphraseUnknown / NAll,

	PCIMBad is 100 * NIMBad / NAll,
	PCDMBad is 100 * NDMBad / NAll,
	PCOMBad is 100 * NOMBad / NAll,
	PCParaphraseBad is 100 * NParaphraseBad / NAll,

	PCIMNoResult is 100 * NIMNoResult / NAll,
	PCDMNoResult is 100 * NDMNoResult / NAll,
	PCOMNoResult is 100 * NOMNoResult / NAll,
	PCParaphraseNoResult is 100 * NParaphraseNoResult / NAll,

	PCSemRecognitionOK is 100 * NSemRecognitionOK / NSemRecognitionMeasured,

	WER is 100 * NWordErrors / NSourceWords,
	OOVRate is 100 * NOOVWords / NSourceWords,

	format(S, '~N#~n', []),
	format(S, '~N#     No Parse: ~d (~1f%)~n', [NNoParse, PCNoParse]),
	format(S, '~N##############################~n', []),
	format(S, '~N#      IM Good: ~d (~1f%)~n', [NIMGood, PCIMGood]),
	format(S, '~N#        IM OK: ~d (~1f%)~n', [NIMOK, PCIMOK]),
	format(S, '~N#   IM Unknown: ~d (~1f%)~n', [NIMUnknown, PCIMUnknown]),
	format(S, '~N#       IM Bad: ~d (~1f%)~n', [NIMBad, PCIMBad]),
	format(S, '~N# IM No Result: ~d (~1f%)~n', [NIMNoResult, PCIMNoResult]),			
	format(S, '~N##############################~n', []),
	format(S, '~N#      DM Good: ~d (~1f%)~n', [NDMGood, PCDMGood]),
	format(S, '~N#      DM   OK: ~d (~1f%)~n', [NDMOK, PCDMOK]),
	format(S, '~N#   DM Unknown: ~d (~1f%)~n', [NDMUnknown, PCDMUnknown]),
	format(S, '~N#       DM Bad: ~d (~1f%)~n', [NDMBad, PCDMBad]),
	format(S, '~N# DM No Result: ~d (~1f%)~n', [NDMNoResult, PCDMNoResult]),				
	format(S, '~N##############################~n', []),
	format(S, '~N#      OM Good: ~d (~1f%)~n', [NOMGood, PCOMGood]),
	format(S, '~N#      OM   OK: ~d (~1f%)~n', [NOMOK, PCOMOK]),
	format(S, '~N#   OM Unknown: ~d (~1f%)~n', [NOMUnknown, PCOMUnknown]),
	format(S, '~N#       OM Bad: ~d (~1f%)~n', [NOMBad, PCOMBad]),
	format(S, '~N# OM No Result: ~d (~1f%)~n', [NOMNoResult, PCOMNoResult]),				
	format(S, '~N##############################~n', []),
	format(S, '~N#     Paraphrase Good: ~d (~1f%)~n', [NParaphraseGood, PCParaphraseGood]),
	format(S, '~N#     Paraphrase   OK: ~d (~1f%)~n', [NParaphraseOK, PCParaphraseOK]),
	format(S, '~N#  Paraphrase Unknown: ~d (~1f%)~n', [NParaphraseUnknown, PCParaphraseUnknown]),
	format(S, '~N#      Paraphrase Bad: ~d (~1f%)~n', [NParaphraseBad, PCParaphraseBad]),
	format(S, '~N#Paraphrase No Result: ~d (~1f%)~n', [NParaphraseNoResult, PCParaphraseNoResult]),			
	format(S, '~N##############################~n', []),

	(   NSourceWords > 0 ->
	    format(S, '~N#   Transcribed words in corpus: ~d~n', [NSourceWords]),
	    format(S, '~N#   Word errors: ~d (WER = ~2f%)~n', [NWordErrors, WER]),
	    format(S, '~N#     OOV words: ~d (OOV rate = ~2f%)~n', [NOOVWords, OOVRate]),
	    format(S, '~N##############################~n', [])
	;
	    true
	),
	
	(   NSemRecognitionOK > 0 ->
	    format(S, '~N#   Sem Rec OK: ~d (~1f%)~n', [NSemRecognitionOK, PCSemRecognitionOK]),
	    format(S, '~N##############################~n', [])
	;
	    true
	),
	format(S, '~N#    Processed: ~d~n', [NProcessed]),	    
	format(S, '~N#~n', []),
	flush_output(user),
	!.
dialogue_process_items_print_summary(ResultsOut) :-
	assoc_generic_to_list(ResultsOut, ResultsOutList),
	format2error('~N*** Error: bad call: dialogue_process_items_print_summary~n', []),
	prettyprint2error(ResultsOutList),
	!,
	fail.

%---------------------------------------------------------------

dialogue_process_items1([], _S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn, N-N).
dialogue_process_items1([file(File) | R], S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, NIn-NOut) :-
	safe_absolute_file_name(File, AbsFile),
	format(S, '~N~n%=======================================', []),
	format(S, '~N~n%Processing items from ~w~n~n', [AbsFile]),
	format('~N~n --- Processing items from ~w~n~n', [AbsFile]),
	open(AbsFile, read, InS),
	dialogue_process_items_from_stream(InS, S, ContextIn-ContextNext, ResultsIn-ResultsNext, ParaphraseIn-ParaphraseNext, NIn-NNext),
	close(InS),
	!,
	dialogue_process_items1(R, S, ContextNext-ContextOut, ResultsNext-ResultsOut, ParaphraseNext-ParaphraseOut, NNext-NOut).
dialogue_process_items1([F | R], S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, NIn-NOut) :-
	format(S, '~N~n%---------------------------------------', []),
	dialogue_process_item(F, S, ContextIn-ContextNext, ResultsIn-ResultsNext, ParaphraseIn-ParaphraseNext),
	NNext is NIn + 1,
	format(user, '.', []),
	(   ( 0 is NNext mod 100, NNext > 0 ) ->
	    
	    format(' (~d) ~n', [NNext])
	;
	    true
	),
	flush_output(user),
	!,
	dialogue_process_items1(R, S, ContextNext-ContextOut, ResultsNext-ResultsOut, ParaphraseNext-ParaphraseOut, NNext-NOut).

dialogue_process_items_from_stream(InS, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, NIn-NOut) :-
	read(InS, Term),
	!,
	dialogue_process_items_from_stream1(Term, InS, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, NIn-NOut).

dialogue_process_items_from_stream1(end_of_file, _InS, _S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn, N-N).
dialogue_process_items_from_stream1(Term, InS, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, NIn-NOut) :-
	format(S, '~N~n%---------------------------------------', []),
	dialogue_process_item(Term, S, ContextIn-ContextNext, ResultsIn-ResultsNext, ParaphraseIn-ParaphraseNext),
	NNext is NIn + 1,
	format(user, '.', []),
	(   ( 0 is NNext mod 100, NNext > 0 ) ->
	    
	    format(' (~d) ~n', [NNext])
	;
	    true
	),
	flush_output(user),
	!,
	dialogue_process_items_from_stream(InS, S, ContextNext-ContextOut, ResultsNext-ResultsOut, ParaphraseNext-ParaphraseOut, NNext-NOut).
	
dialogue_process_item(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut) :-
	safe_dialogue_process_item_main(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut).

safe_dialogue_process_item_main(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut) :-
	on_exception(
	Exception, 
	dialogue_process_item_main_with_timeout(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut),
	handle_dialogue_process_exception(Exception, Record, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut)
    ),
	!.
safe_dialogue_process_item_main(Record, S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn) :-
	format('~N*** Warning: dialogue processing failed for "~w"', [Record]),
	format(S, '~N*** Warning: dialogue processing failed for "~w"', [Record]),
	!.

handle_dialogue_process_exception(Exception, Record, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn) :-
	format2error('~N~n*** Error: dialogue_process_item_main/4 threw exception while processing "~w":~n~n', [Record]),
	format2error('~N~q~n', [Exception]),
	!.

dialogue_process_item_main_with_timeout(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut) :-
	dialogue_processing_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	
	time_out(dialogue_process_item_main(Record, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut),
		 TimeLimitInMilliseconds,
		 Result),

	(   Result = time_out ->
	    format2error('~N~n*** Error: time out (~d seconds) when trying to process ~w~n',
		   [TimeLimit, Record]),
	    format(S, '~N~n% *** Error: time out (~d seconds) when trying to process ~q~n',
		   [TimeLimit, Record]),
	    
	    ContextIn = ContextOut,
	    ResultsIn = ResultsOut
	;
	    otherwise ->
	    true
	).

%---------------------------------------------------------------

% We write the out-record a line at a time onto S. We tally the judgement scores in ResultsIn-ResultsOut.

dialogue_process_item_main(comment(Comment), S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn) :-
	(   is_prolog_string(Comment) ->
	    
	    format(S, '~N% ~s~n', [Comment]) ;
	    
	    format(S, '~N% ~w~n', [Comment])
	),
	!.
dialogue_process_item_main(init_dialogue, S, _ContextIn-ContextOut, ResultsIn-ResultsIn, _ParaphraseIn-ParaphraseOut) :-
	(   dialogue_manager:initial_dialogue_state(ContextOut) ->

	    ParaphraseOut = '*no_paraphrase*',
	    format(S, '~N% Dialogue state re-initialised to "~w"~n', [ContextOut])
	;
	    
	    format(S, '~NError: Unable to re-initialise dialogue state', []),
	    fail
	),
	!.
dialogue_process_item_main(set_notional_time(Time), S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn) :-
	atom_codes(Time, TimeChars),
	(   parse_datime(TimeChars, Datime) ->
	    
	    set_notional_time(Datime),
	    format(S, '~N% Notional dialogue time set to "~w"~n', [Time]) ;
	    
	    format(S, '~NUnable to parse time "~w". Format = YYYY-MM-DD_HH-MM-SS, e.g. 2006-12-31_23-59-59~n', [Time]),
	    fail
	),
	!.
dialogue_process_item_main(set_notional_speaker(Name), S, ContextIn-ContextIn, ResultsIn-ResultsIn, ParaphraseIn-ParaphraseIn) :-
	(   atom(Name) ->
	    
	    set_notional_speaker(Name),
	    format(S, '~N% Notional dialogue speaker set to "~w"~n', [Name]) ;
	    
	    format('~NUnable to set speaker "~w" - needs to be an atom~n', [Name]),
	    fail
	),
	!. 
dialogue_process_item_main(InRecord, S, ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut) :-
	(   InRecord = batchrec_item(_BatchrecList)
	;
	    InRecord = sent(_Sent)
	;
	    InRecord = wavfile(_Wavfile)
	),
	set_notional_time_and_init_dialogue_from_wavfile_if_possible(InRecord, S, ContextIn-ContextNext),
	dialogue_process_item_normal(InRecord, S, not_in_nbest, ContextNext-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, OutRecord),
	print_dialogue_processing_record(S, OutRecord),
	!.
dialogue_process_item_main(rec_stats(RecStatsList), S, Context-Context, Results-Results, Paraphrase-Paraphrase) :-
	format(S, '~N%~q.~n', [rec_stats(RecStatsList)]),
	!.
 
dialogue_process_item_normal(wavfile(Wavfile), S, InNBest,
			     ContextIn-ChosenContextOut, ResultsIn-ChosenResultsOut, ParaphraseIn-ParaphraseOut, 
			     ChosenRecord) :-
	safe_absolute_file_name(Wavfile, AbsWavfile),
	recognise_from_wavfile_as_defined_by_config_file(AbsWavfile, RecResult),
	(   get_transcription_from_wavfile(AbsWavfile, Transcription) ->
	    true
	;
	    Transcription = '*no_transcription*'
	),
	rec_result_and_transcription_to_batchrec_item(RecResult, AbsWavfile, Transcription, BatchrecItem),
	dialogue_process_item_normal(BatchrecItem, S, InNBest,
				     ContextIn-ChosenContextOut, ResultsIn-ChosenResultsOut, ParaphraseIn-ParaphraseOut, 
				     ChosenRecord),
	!.
dialogue_process_item_normal(batchrec_item(BatchrecList), S, not_in_nbest,
			     ContextIn-ChosenContextOut, ResultsIn-ChosenResultsFinal, ParaphraseIn-ChosenParaphraseOut,
			     ChosenRecordFinal) :-
	unpack_nbest_batchrec_list_for_dialogue(BatchrecList, NBestBatchrecLists),
	!,
	findall([record=Record, context=ContextOut, results=ResultsOut, paraphrase=ParaphraseOut],
		(   member(SingleBatchrecList, NBestBatchrecLists),
		    dialogue_process_item_normal(batchrec_item(SingleBatchrecList), S, in_nbest,
						 ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
						 Record)
		),
		Tuples),
	nbest_preferences(Tuples,
			  [record=ChosenRecord, context=ChosenContextOut, results=ChosenResultsOut, paraphrase=ChosenParaphraseOut],
			  Trace,
			  RankOfChosenElement),
	(   notional_batchrec_item_for_transcription_in_nbest_list(BatchrecList, TranscriptionBatchrecList) ->
	    dialogue_process_item_normal(batchrec_item(TranscriptionBatchrecList), S, in_nbest,
					 ContextIn-TransContextOut, ResultsIn-TransResultsOut, ParaphraseIn-TransParaphraseOut, TransRecord),
	    nbest_preferences([[record=TransRecord, context=TransContextOut, results=TransResultsOut, paraphrase=TransParaphraseOut]],
			      _ChosenTransRecord,
			      TransTrace,
			      _TransRank),
	    append(TransTrace, Trace, FullTrace),
	    add_semantic_recognition_accuracy_info(ChosenRecord, TransRecord, ChosenRecordFinal,
						   ChosenResultsOut-ChosenResultsFinal)
	;
	    otherwise ->
	    ChosenRecordFinal = ChosenRecord,
	    ChosenResultsFinal = ChosenResultsOut,
	    FullTrace = Trace
	),
	add_semantic_recognition_accuracy_info_to_nbest_trace(FullTrace, TransRecord, FullTraceWithSemRecInfo),
	print_nbest_trace(FullTraceWithSemRecInfo, RankOfChosenElement, S).
dialogue_process_item_normal(batchrec_item(BatchrecList), S, InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsFinal, ParaphraseIn-ParaphraseOut,
			     RecordFinal) :-
	member(wavfile=Wavfile, BatchrecList),
	member(words=RecognisedWords, BatchrecList),
	member(transcription=SourceWords, BatchrecList),
	join_with_spaces(RecognisedWords, Recognised),
	join_with_spaces(SourceWords, Source),
	(   ( member(rank=Rank, BatchrecList), member(confidence=Confidence, BatchrecList) ) ->
	    Record = [wavfile=Wavfile, rank=Rank, confidence=Confidence | NextRecord0]
	;
	    member(rank=Rank, BatchrecList) ->
	    Record = [wavfile=Wavfile, rank=Rank | NextRecord0]
	;
	    otherwise ->
	    Record = [wavfile=Wavfile | NextRecord0]
	),
	(   member(paraphrase=ParaphraseWords, BatchrecList) ->
	    join_with_spaces(ParaphraseWords, Paraphrase),
	    NextRecord0 = [transcription_paraphrase=Paraphrase | NextRecord1]
	;
	    otherwise ->
	    NextRecord0 = NextRecord1
	),
	(   is_assoc_generic(ContextIn) ->
	    assoc_generic_to_list(ContextIn, ContextIn1),
	    NextRecord1 = [in_state=ContextIn, readable(in_state)=ContextIn1 | NextRecord2]
	;
	    otherwise ->
	    NextRecord1 = [in_state=ContextIn | NextRecord2]
	),
	inc_assoc_generic(ResultsIn, processed, Results1),
	implicit_context(ImplicitContext),
	NextRecord2 = [sent=Source | NextRecord3],
	NextRecord3 = [recognised=RecognisedWords | NextRecord4],
	NextRecord4 = [word_list=SourceWords, implicit_context=ImplicitContext, last_paraphrase=ParaphraseIn | NextRecord5],
	(   atom_to_parse_unless_nothing_recognised(Recognised, Parse) ->
	    %user:atom_to_parse_using_current_parser(Recognised, '.MAIN', Parse) ->

	    NextRecord5 = [parse=Parse | NextRecord6],
	    dialogue_process_item_main_from_parse(Parse, SourceWords, RecognisedWords, 
						  ContextIn-ContextOut, Results1-ResultsOut, ParaphraseIn-ParaphraseOut, NextRecord6) ;

	    % If parsing failed, keep the same context and close off the record.
	    user:report_unknown_words_in_sent_atom(Recognised),
	    ContextIn = ContextOut,
	    ParaphraseIn = ParaphraseOut,
	    NextRecord5 = [parse=no_parse],
	    inc_assoc_generic(Results1, no_parse, ResultsOut)
	),
	(   (   InNBest = not_in_nbest,
		notional_batchrec_item_for_transcription_in_nbest_list(BatchrecList, TranscriptionBatchrecList)
	    ) ->
	    dialogue_process_item_normal(batchrec_item(TranscriptionBatchrecList), S, in_nbest,
					 ContextIn-_TransContextOut, ResultsIn-_TransResultsOut, ParaphraseIn-_TransParaphraseOut,
					 TransRecord),
	    add_semantic_recognition_accuracy_info(Record, TransRecord, RecordFinal0, ResultsOut-ResultsFinal0)
	;
	    otherwise ->
	    Record = RecordFinal0,
	    ResultsOut = ResultsFinal0
	),
	add_word_error_info(RecordFinal0, SourceWords, RecognisedWords, RecordFinal, ResultsFinal0-ResultsFinal),
	!.
dialogue_process_item_normal(sent(Sent), S, InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, Record) :-
	atom_codes(Sent, Chars),
	interpret_string_as_raw_lf_input(Chars, Parse),
	%format('~N--- Processing "~w"~n', [Sent]),
	!,
	dialogue_process_item_normal(lf(Parse), S, InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, Record).
dialogue_process_item_normal(string_and_lf(Sent, Parse), _S, _InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, Record) :-
	!,
	(   is_assoc_generic(ContextIn) ->
	    assoc_generic_to_list(ContextIn, ContextIn1),
	    Record = [in_state=ContextIn, readable(in_state)=ContextIn1 | NextRecord1]
	;
	    otherwise ->
	    Record = [in_state=ContextIn | NextRecord1]
	),
	inc_assoc_generic(ResultsIn, processed, Results1),
	split_atom_into_words(Sent, WordList),
	implicit_context(ImplicitContext),
	NextRecord1 = [sent=Sent | NextRecord2],
	NextRecord2 = [word_list=WordList, implicit_context=ImplicitContext, last_paraphrase=ParaphraseIn | NextRecord3],
	NextRecord3 = [parse=Parse | NextRecord4],
	dialogue_process_item_main_from_parse(Parse, WordList, WordList,
					      ContextIn-ContextOut, Results1-ResultsOut, ParaphraseIn-ParaphraseOut,
					      NextRecord4),
	!.
dialogue_process_item_normal(lf(Parse), _S, _InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut, Record) :-
	!,
	(   is_assoc_generic(ContextIn) ->
	    assoc_generic_to_list(ContextIn, ContextIn1),
	    Record = [in_state=ContextIn, readable(in_state)=ContextIn1 | NextRecord1]
	;
	    otherwise ->
	    Record = [in_state=ContextIn | NextRecord1]
	),
	inc_assoc_generic(ResultsIn, processed, Results1),
	WordList = ['(no words)'],
	implicit_context(ImplicitContext),
	format_to_atom('LF: ~q', [Parse], Sent),
	NextRecord1 = [sent=Sent | NextRecord2],
	NextRecord2 = [word_list=WordList, implicit_context=ImplicitContext, last_paraphrase=ParaphraseIn | NextRecord3],
	NextRecord3 = [parse=Parse | NextRecord4],
	dialogue_process_item_main_from_parse(Parse, WordList, WordList,
					      ContextIn-ContextOut, Results1-ResultsOut, ParaphraseIn-ParaphraseOut,
					      NextRecord4),
	!.
dialogue_process_item_normal(sent(Sent), _S, _InNBest,
			     ContextIn-ContextOut, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
			     Record) :-
	%format('~N--- Processing "~w"~n', [Sent]),
	(   is_assoc_generic(ContextIn) ->
	    assoc_generic_to_list(ContextIn, ContextIn1),
	    Record = [in_state=ContextIn, readable(in_state)=ContextIn1 | NextRecord1]
	;
	    otherwise ->
	    Record = [in_state=ContextIn | NextRecord1]
	),
	inc_assoc_generic(ResultsIn, processed, Results1),
	split_atom_into_words(Sent, WordList),
	implicit_context(ImplicitContext),
	NextRecord1 = [sent=Sent | NextRecord2],
	NextRecord2 = [word_list=WordList, implicit_context=ImplicitContext, last_paraphrase=ParaphraseIn | NextRecord3],
	(   atom_to_parse_unless_nothing_recognised(Sent, Parse) ->
	    %user:atom_to_parse_using_current_parser(Sent, '.MAIN', Parse) ->

	    NextRecord3 = [parse=Parse | NextRecord4],
	    dialogue_process_item_main_from_parse(Parse, WordList, WordList, 
						  ContextIn-ContextOut, Results1-ResultsOut, ParaphraseIn-ParaphraseOut,
						  NextRecord4)
	;

	    % If parsing failed, keep the same context and close off the record.
	    user:report_unknown_words_in_sent_atom(Sent),
	    ContextIn = ContextOut,
	    ParaphraseIn = ParaphraseOut,
	    inc_assoc_generic(Results1, no_parse, ResultsOut),
	    (   (   get_good_judgement(paraphrase, [WordList, ParaphraseIn, ImplicitContext], [GoodParaphrase]),
		    GoodParaphrase \== '?'
		) ->
		NextRecord3 = [parse=no_parse, good_paraphrase=GoodParaphrase]
	    ;
		otherwise ->
		NextRecord3 = [parse=no_parse]
	    )
	),
	!.

% Similar to dialogue_process_item_main/4, but we start from the parse rather than the string.
dialogue_process_item_main_from_parse(SemValue, WordList, RecognisedList,
				      InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
				      Record) :-
	(   current_predicate(LFResPackage:resolve_lf/4) ->

	    (   LFResPackage:resolve_lf(SemValue, InState, ResolvedSemValue, Substitutions) ->
		Record = [resolved_lf=ResolvedSemValue, resolution=Substitutions | NextRecord],
		dialogue_process_item_main_from_resolved_parse(SemValue, ResolvedSemValue, WordList, RecognisedList,
							       InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
							       NextRecord) ;

		Record = [resolved_lf=no_resolved_lf],
		InState = OutState,
		inc_assoc_generic(ResultsIn, im(no_result), ResultsOut)
	    ) ;

	    dialogue_process_item_main_from_resolved_parse(SemValue, SemValue, WordList, RecognisedList,
							   InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
							   Record)
	).
	
dialogue_process_item_main_from_resolved_parse(SemValue, ResolvedSemValue, WordList, RecognisedList,
					       InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
					       Record) :-	
	(   call_lf_to_dialogue_move(ResolvedSemValue, RecognisedList, InState, DialogueMove, IntermediateResults) ->

	    % We are judging performance on the whole IM transformation, i.e. SemValue -> DialogueMove
	    get_judgement(im, [WordList, SemValue, InState], [DialogueMove], Judgement),
	    %Record = [dialogue_move=[DialogueMove, Judgement] | NextRecord],
	    add_dialogue_move_and_intermediate_results_to_record([DialogueMove, Judgement],
								 IntermediateResults,
								 NextRecord,
								 Record),
	    make_judgement_canonical(Judgement, CanonicalJudgement),
	    inc_assoc_generic(ResultsIn, im(CanonicalJudgement), Results1),
	    dialogue_process_item_main_from_dialogue_move(DialogueMove, WordList, ResolvedSemValue,
							  InState-OutState, Results1-ResultsOut, ParaphraseIn-ParaphraseOut,
							  NextRecord) ;

	    Record = [dialogue_move=[no_dialogue_move, no_result]],
	    InState = OutState,
	    inc_assoc_generic(ResultsIn, im(no_result), ResultsOut)
	).

% We can have between two and five arguments to lf_to_dialogue_move.
call_lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove, IntermediateResults) :-
	current_predicate(InputPackage:lf_to_dialogue_move/5),
	!,
	InputPackage:lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove, IntermediateResults).
call_lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove, []) :-
	current_predicate(InputPackage:lf_to_dialogue_move/4),
	!,
	InputPackage:lf_to_dialogue_move(SemValue, WordList, InState, DialogueMove).
call_lf_to_dialogue_move(SemValue, _WordList, InState, DialogueMove, []) :-
	current_predicate(InputPackage:lf_to_dialogue_move/3),
	!,
	InputPackage:lf_to_dialogue_move(SemValue, InState, DialogueMove).
call_lf_to_dialogue_move(SemValue, _WordList, _InState, DialogueMove, []) :-
	current_predicate(InputPackage:lf_to_dialogue_move/2),
	!,
	InputPackage:lf_to_dialogue_move(SemValue, DialogueMove).

add_dialogue_move_and_intermediate_results_to_record(DialogueMove, IntermediateResults, RestRecord, Record) :-
	package_intermediate_results_for_record(IntermediateResults, IntermediateResultsAlist, 1),
	Record2 = [dialogue_move=DialogueMove | RestRecord],
	append(IntermediateResultsAlist, Record2, Record),
	!.
add_dialogue_move_and_intermediate_results_to_record(DialogueMove, IntermediateResults, RestRecord, Record) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_dialogue_move_and_intermediate_results_to_record(DialogueMove, IntermediateResults, RestRecord, Record)]),
	fail.

package_intermediate_results_for_record([], [], _I) :-
	!.
package_intermediate_results_for_record([F | R], [intermediate_lf(I)=F | R1], I) :-
	I1 is I + 1,
	!,
	package_intermediate_results_for_record(R, R1, I1).

% Similar to dialogue_process_item_main/4, but we start from the dialogue move
dialogue_process_item_main_from_dialogue_move(DialogueMove, WordList, SemValue,
					      InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
					      Record) :-
	(   current_predicate(DMResPackage:resolve_dialogue_move/3) ->

	    (   DMResPackage:resolve_dialogue_move(DialogueMove, InState, ResolvedDialogueMove) ->
		Record = [resolved_dialogue_move=ResolvedDialogueMove | NextRecord],
		dialogue_process_item_main_from_resolved_dialogue_move(DialogueMove, WordList, ResolvedDialogueMove, SemValue,
								       InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
								       NextRecord)
	    ;

		Record = [resolved_dialogue_move=no_dialogue_move],
		InState = OutState,
		inc_assoc_generic(ResultsIn, dm(no_result), ResultsOut)
	    )
	;

	    dialogue_process_item_main_from_resolved_dialogue_move(DialogueMove, WordList, DialogueMove, SemValue,
								   InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
								   Record)
	).

dialogue_process_item_main_from_resolved_dialogue_move(DialogueMove, WordList, ResolvedDialogueMove, SemValue,
						       InState-OutState, ResultsIn-ResultsOut, ParaphraseIn-ParaphraseOut,
						       Record) :-
	implicit_context(ImplicitContext),
	(   call_paraphrase_from_dialogue_move(ResolvedDialogueMove, ParaphraseOut) ->
	    get_judgement(paraphrase, [WordList, ParaphraseIn, ImplicitContext], [ParaphraseOut], ParaphraseJudgement),
	    make_judgement_canonical(ParaphraseJudgement, CanonicalParaphraseJudgement),
	    inc_assoc_generic(ResultsIn, paraphrase(CanonicalParaphraseJudgement), ResultsNext1),
	    (   (   ParaphraseJudgement \== good,
		    get_good_judgement(paraphrase, [WordList, ParaphraseIn, ImplicitContext], [GoodParaphrase]),
		    GoodParaphrase \== '?'
		) ->
		Record = [paraphrase=[ParaphraseOut, ParaphraseJudgement], good_paraphrase=GoodParaphrase  | NextRecord1]
	    ;
		otherwise ->
		Record = [paraphrase=[ParaphraseOut, ParaphraseJudgement]  | NextRecord1]
	    )
	;
	    ParaphraseOut = ParaphraseIn,
	    Record = NextRecord1,
	    ResultsNext1 = ResultsIn
	),
	
	(   call_update_dialogue_state(ResolvedDialogueMove, SemValue, InState, AbstractAction, OutState) ->

	    % We are judging performance on the whole DM transformation, i.e. DialogueMove -> AbstractAction
	    get_judgement(dm, [DialogueMove, InState], [AbstractAction, OutState], Judgement),
	    make_judgement_canonical(Judgement, CanonicalJudgement),
	    (   is_assoc_generic(OutState) ->
		assoc_generic_to_list(OutState, OutState1),
		NextRecord1 = [abstract_action_and_out_state=[AbstractAction, OutState, Judgement],
			       readable(abstract_action_and_out_state)=[AbstractAction, OutState1, '*dont_edit_this_line*']
			      | NextRecord2]
	    ;
		otherwise ->
		NextRecord1 = [abstract_action_and_out_state=[AbstractAction, OutState, Judgement]
			      | NextRecord2]
	    ),
	    inc_assoc_generic(ResultsNext1, dm(CanonicalJudgement), ResultsNext2),
	    dialogue_process_item_main_from_abstract_action(AbstractAction, ResultsNext2-ResultsOut, NextRecord2)
	;
	    InState = OutState,
	    (   is_assoc_generic(OutState) ->
		assoc_generic_to_list(OutState, OutState1),
		NextRecord1 = [abstract_action_and_out_state=[no_abstract_action, OutState, no_result],
			       readable(abstract_action_and_out_state)=[no_abstract_action, OutState1, no_result]]
	    ;
		otherwise ->
		NextRecord1 = [abstract_action_and_out_state=[no_abstract_action, OutState, no_result]]
	    ),
	    inc_assoc_generic(ResultsNext1, dm(no_result), ResultsOut)
	).

call_paraphrase_from_dialogue_move(DialogueMove, Paraphrase) :-
	current_predicate(paraphrase:generate_paraphrase/3),
	paraphrase_from_dialogue_move(DialogueMove, Paraphrase),
	!.

call_update_dialogue_state(DialogueMove, SemValue, InState, AbstractAction, OutState) :-
	current_predicate(DMPackage:update_dialogue_state/5),
	DMPackage:update_dialogue_state(DialogueMove, SemValue, InState, AbstractAction, OutState),
	!.
call_update_dialogue_state(DialogueMove, _SemValue, InState, AbstractAction, OutState) :-
	current_predicate(DMPackage:update_dialogue_state/4),
	DMPackage:update_dialogue_state(DialogueMove, InState, AbstractAction, OutState),
	!.

% Similar to dialogue_process_item_main/4, but we start from the abstract action.
% We don't have any more processing to do, so we close off the form whether we succeed or not.
dialogue_process_item_main_from_abstract_action(AbstractAction, ResultsIn-ResultsOut, Record) :-	    
	(   call_abstract_action_to_action(AbstractAction, Action) ->

	    get_judgement(om, [AbstractAction], [Action], Judgement),
	    make_judgement_canonical(Judgement, CanonicalJudgement),
	    convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	    (   Action = PrintFormForAction ->
		Record = [action=[Action, Judgement]]
	    ;
		otherwise ->
		Record = [action=[Action, Judgement], readable(action)=[PrintFormForAction, '*dont_edit_this_line*']]
	    ),
	    inc_assoc_generic(ResultsIn, om(CanonicalJudgement), ResultsOut) ;

	    Record = [action=[no_action, no_result]],
	    inc_assoc_generic(ResultsIn, om(no_result), ResultsOut)
	),
	!.

call_abstract_action_to_action(AbstractAction, Action) :-
	current_predicate(OutputPackage:abstract_action_to_action/2),
	OutputPackage:abstract_action_to_action(AbstractAction, Action),
	!.

%---------------------------------------------------------------

add_word_error_info(RecordIn, SourceWords, RecognisedWords, RecordOut, ResultsIn-ResultsOut) :-
	length(SourceWords, NSourceWords),
	user:unknown_words_in_word_list(SourceWords, UnknownWords),
	length(UnknownWords, NUnknownWords),
	insertions_deletions_substitutions(RecognisedWords, SourceWords, Total, Insertions, Deletions, Substitutions),
	RecordOut = [word_errors=[total=Total, ins=Insertions, del=Deletions, sub=Substitutions] | RecordIn],
	inc_assoc_generic(ResultsIn, source_words, NSourceWords, ResultsNext1),
	inc_assoc_generic(ResultsNext1, oov_words, NUnknownWords, ResultsNext2),
	inc_assoc_generic(ResultsNext2, word_errors, Total, ResultsOut),
	!.
add_word_error_info(RecordIn, SourceWords, RecognisedWords, RecordOut, ResultsIn-ResultsOut) :-
	format2error('~N*** Error: bad call: ~q',
		     [add_word_error_info(RecordIn, SourceWords, RecognisedWords, RecordOut, ResultsIn-ResultsOut)]),
	fail.

%---------------------------------------------------------------

add_semantic_recognition_accuracy_info_to_nbest_trace([], _TransRecord, []).
add_semantic_recognition_accuracy_info_to_nbest_trace([F | R], TransRecord, [F1 | R1]) :-
	add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransRecord, F1),
	!,
	add_semantic_recognition_accuracy_info_to_nbest_trace(R, TransRecord, R1).

add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransRecord, F1) :-
	F = Score-[[record=Record | Rest], PrefTrace],
	F1 = Score-[[record=Record1 | Rest], PrefTrace],
	(   dialogue_processing_records_semantically_equivalent(Record, TransRecord) ->
	    SemRecognition = good
	;	    
	    SemRecognition = bad
	),
	add_sem_recognition_item_to_record(Record, SemRecognition, Record1),
	!.
add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransRecord, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransRecord, F1)]),
	fail.

add_semantic_recognition_accuracy_info(ChosenRecord, TransRecord, ChosenRecordFinal,
				       ChosenResults-ChosenResultsFinal) :-
	inc_assoc_generic(ChosenResults, sem_recognition_measured, ChosenResultsNext),
	(   dialogue_processing_records_semantically_equivalent(ChosenRecord, TransRecord) ->
	    SemRecognition = good,
	    inc_assoc_generic(ChosenResultsNext, sem_recognition_ok, ChosenResultsFinal)
	;	    
	    SemRecognition = bad,
	    ChosenResultsNext = ChosenResultsFinal
	),
	add_sem_recognition_item_to_record(ChosenRecord, SemRecognition, ChosenRecordFinal),
	!.
add_semantic_recognition_accuracy_info(ChosenRecord, TransRecord, ChosenRecordFinal, ChosenResultsPair) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_semantic_recognition_accuracy_info(ChosenRecord, TransRecord, ChosenRecordFinal, ChosenResultsPair)]),
	fail.

/*
Count sem_recognition as good if one of the following:
 a) the recognised words are all correct
 b) the dialogue move is non-trivial and is the one that the correct words would have produced.
 c) the resolved dialogue move is non-trivial and is the one that the correct words would have produced.
 d) the paraphrase is non-trivial and is the one that the correct words would have produced.
*/

dialogue_processing_records_semantically_equivalent(Record1, Record2) :-
	member(recognised=Words, Record1),
	member(recognised=Words, Record2),
	!.
dialogue_processing_records_semantically_equivalent(Record1, Record2) :-
	member(dialogue_move=[DialogueMove, _], Record1),
	member(dialogue_move=[DialogueMove, _], Record2),
	!.
dialogue_processing_records_semantically_equivalent(Record1, Record2) :-
	member(resolved_dialogue_move=[DialogueMove, _], Record1),
	member(resolved_dialogue_move=[DialogueMove, _], Record2),
	!.
dialogue_processing_records_semantically_equivalent(Record1, Record2) :-
	member(paraphrase=[Paraphrase, _], Record1),
	member(paraphrase=[Paraphrase, _], Record2),
	Paraphrase \== 'WARNING: PARAPHRASE GENERATION FAILED',
	!.

add_sem_recognition_item_to_record([],
				   SemRecognition,
				   [sem_recognition=SemRecognition]) :-
	!.
add_sem_recognition_item_to_record([dialogue_move=D | R],
				   SemRecognition,
				   [dialogue_move=D, sem_recognition=SemRecognition | R]) :-
	!.
add_sem_recognition_item_to_record([F | R], SemRecognition, [F | R1]) :-
	!,
	add_sem_recognition_item_to_record(R, SemRecognition, R1).
	
%---------------------------------------------------------------
 
warn_if_dialogue_resources_not_loaded :-
	(   lf_to_dialogue_move_defined ->
	    true
	;
	    format('~N*** Warning: lf_to_dialogue_move/{2,3,4 or 5} not defined~n', [])
	),
	(   update_dialogue_state_defined ->
	    true
	;
	    format('~N*** Warning: update_dialogue_state/{4 or 5} not defined~n', [])
	),
	(   abstract_action_to_action_defined ->
	    true
	;
	    format('~N*** Warning: abstract_action_to_action/2 not defined~n', [])
	),
	!.

lf_to_dialogue_move_defined :-
	current_predicate(_Package:lf_to_dialogue_move/2).
lf_to_dialogue_move_defined :-
	current_predicate(_Package:lf_to_dialogue_move/3).
lf_to_dialogue_move_defined :-
	current_predicate(_Package:lf_to_dialogue_move/4).
lf_to_dialogue_move_defined :-
	current_predicate(_Package:lf_to_dialogue_move/5).

update_dialogue_state_defined :-
	current_predicate(_Package:update_dialogue_state/4).
update_dialogue_state_defined :-
	current_predicate(_Package:update_dialogue_state/5).

abstract_action_to_action_defined :-
	current_predicate(_Package:abstract_action_to_action/2).

%---------------------------------------------------------------	

set_notional_time_and_init_dialogue_from_wavfile_if_possible(InRecord, S, ContextIn-ContextOut) :-
	(   InRecord = batchrec_item(BatchrecList) ->
	    member(wavfile=Wavfile, BatchrecList)
	;
	    InRecord = wavfile(Wavfile)
	),
	directory_for_wavfile(Wavfile, Dir),
	atom_codes(Dir, DirChars),
	parse_datime(DirChars, Datime),
	set_notional_time_and_init_dialogue_from_wavfile_if_possible1(Dir, Datime, S, ContextIn-ContextOut),
	!.
set_notional_time_and_init_dialogue_from_wavfile_if_possible(_InRecord, _S, ContextIn-ContextIn).

set_notional_time_and_init_dialogue_from_wavfile_if_possible1(_Dir, Datime, _S, ContextIn-ContextIn) :-
	get_notional_time(CurrentDatime),
	Datime = CurrentDatime,
	!.
set_notional_time_and_init_dialogue_from_wavfile_if_possible1(Dir, Datime, S, _ContextIn-ContextOut) :-
	(   dialogue_manager:initial_dialogue_state(ContextOut) ->
	    
	    format(S, '~N% Dialogue state re-initialised to "~w"~n', [ContextOut]) ;
	    
	    format(S, '~NError: Unable to re-initialise dialogue state', []),
	    fail
	),
	format(S, '~N% Notional dialogue time set to "~w"~n', [Dir]),
	set_notional_time(Datime),
	!.
set_notional_time_and_init_dialogue_from_wavfile_if_possible1(Dir, Datime, S, Context) :-
	format('~N*** Warning: bad call: ~w~n',
	       [set_notional_time_and_init_dialogue_from_wavfile_if_possible1(Dir, Datime, S, Context)]),
	fail.

%---------------------------------------------------------------

paraphrase_from_dialogue_move(DialogueMove, Paraphrase) :-
	current_predicate(paraphrase:generate_paraphrase/3),
	paraphrase_generation_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	time_out((   call_pre_process_for_paraphrasing(DialogueMove, DialogueMove1),
		     paraphrase:generate_paraphrase(DialogueMove1, _Tree, ParaphraseWords)
		 ),
		 TimeLimitInMilliseconds,
		 Result),
	(   Result = time_out ->
	    format_to_atom('WARNING: PARAPHRASE GENERATION TIMED OUT AT ~d SECONDS',
			   [TimeLimit],
			   Paraphrase)
	;
	    otherwise ->
	    fix_orthography(ParaphraseWords, ParaphraseWords1),
	    join_with_spaces(ParaphraseWords1, Paraphrase)
	),
	!.
paraphrase_from_dialogue_move(_DialogueMove, Paraphrase) :-
	current_predicate(paraphrase:generate_paraphrase/3),
	Paraphrase = 'WARNING: PARAPHRASE GENERATION FAILED',
	!.

call_pre_process_for_paraphrasing(DialogueMove, DialogueMove1) :-
	current_predicate(Package:pre_process_for_paraphrasing/2),
	!,
	(   Package:pre_process_for_paraphrasing(DialogueMove, DialogueMove1) ->
	    true
	;
	    format('~N*** Warning: bad call: ~w~n',
		   [Package:pre_process_for_paraphrasing(DialogueMove, DialogueMove1)]),
	    fail
	).
call_pre_process_for_paraphrasing(DialogueMove, DialogueMove).

%---------------------------------------------------------------

:- dynamic current_batch_dialogue_printing_format/1.

set_batch_dialogue_printing_format(Id) :-
	(   batch_dialogue_printing_list(Id, _List) ->
	    retractall(current_batch_dialogue_printing_format(_)),
	    assertz(current_batch_dialogue_printing_format(Id))
	;
	    otherwise ->
	    findall(SomeId, batch_dialogue_printing_list(SomeId, _), AllIds),
	    format('~N*** Error: unknown batch dialogue printing format "~w". Known formats: ~w~n',
		   [Id, AllIds]),
	    fail
	).

get_batch_dialogue_printing_list(List) :-
	(   current_batch_dialogue_printing_format(Id) ->
	    true
	;
	    otherwise ->
	    Id = normal
	),
	batch_dialogue_printing_list(Id, List).

batch_dialogue_printing_list(normal, List) :-
	List = [wavfile,
		rank,
		confidence,
		sent,
		recognised,
		implicit_context,
		word_list,
		last_paraphrase,
		paraphrase,
		good_paraphrase,
		in_state,
		readable(in_state),
		parse,
		resolved_lf,
		resolution,
		intermediate_lf(1),
		intermediate_lf(2),
		intermediate_lf(3),
		intermediate_lf(4),		
		dialogue_move,
		sem_recognition,
		resolved_dialogue_move,
		abstract_action_and_out_state,
		readable(abstract_action_and_out_state),
		action,
		readable(action)
		].
batch_dialogue_printing_list(no_paraphrases, List) :-
	batch_dialogue_printing_list(normal, List0),
	remove_items_from_ordered_list(List0,
				       [last_paraphrase,
					paraphrase,
					good_paraphrase],
				       List).
batch_dialogue_printing_list(no_datastructures, List) :-
	batch_dialogue_printing_list(normal, List0),
	remove_items_from_ordered_list(List0,
				       [in_state,
					readable(in_state),
					parse,
					resolved_lf,
					resolution,
					intermediate_lf(1),
					intermediate_lf(2),
					intermediate_lf(3),
					intermediate_lf(4),
					intermediate_lf(5),
					intermediate_lf(6),
					dialogue_move,
					sem_recognition,
					resolved_dialogue_move,
					abstract_action_and_out_state,
					readable(abstract_action_and_out_state),
					action,
					readable(action)],
				       List).

remove_items_from_ordered_list([], _ToRemove, []).
remove_items_from_ordered_list([F | R], ToRemove, R1) :-
	member(F, ToRemove),
	!,
	remove_items_from_ordered_list(R, ToRemove, R1).
remove_items_from_ordered_list([F | R], ToRemove, [F | R1]) :-
	!,
	remove_items_from_ordered_list(R, ToRemove, R1).

print_dialogue_processing_record(S, Record) :-
	get_batch_dialogue_printing_list(List),
	(   Record = [] ->
	    format(S, '~Ndialogue_record([]).~n', [])
	;
	    otherwise ->
	    format(S, '~N~ndialogue_record([~n', []),
	    print_dialogue_processing_element_list(List, Record, S),
	    format(S, ']).~n', [])
	).

print_dialogue_processing_element_list([], _Record, _S).
print_dialogue_processing_element_list([F | R], Record, S) :-
	(   real_elements_left_in_dialogue_record(R, Record) ->
	    AtEnd = not_at_end
	;
	    otherwise ->
	    AtEnd = at_end
	),
	print_dialogue_processing_element(F, Record, S, AtEnd),
	!,
	print_dialogue_processing_element_list(R, Record, S).

print_dialogue_processing_element(Key, Record, S, AtEnd) :-
	member(Key=Value0, Record),
	copy_term(Value0, Value),
	make_ground(Value),
	(   Key = readable(RealKey) ->
	    format(S, '~N%~w = ~w', [RealKey, Value])
	;
	    otherwise ->
	    prettyprintq_to_stream_key_val_pair(S, Key, Value)
	),
	(   AtEnd = not_at_end ->
	    format(S, ',~n', [])
	;
	    otherwise ->
	    format(S, '~n', [])
	),
	!.
print_dialogue_processing_element(_Key, _Record, _S, _AtEnd).

real_elements_left_in_dialogue_record(List, Record) :-
	member(Key, List),
	member(Key=_Value, Record),
	atomic(Key),
	!.

%---------------------------------------------------------------

batch_dialogue_file_to_paraphrase_csv_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	open(AbsInFile, read, SIn),
	open(AbsOutFile, write, SOut),

	format('~N~n--- Reading input from ~w~n', [AbsInFile]),

	batch_dialogue_to_paraphrase_csv(SIn, SOut, 0-N),

	format('~N--- Processed ~d records. Output written to ~w~n', [N, AbsOutFile]),

	close(SIn),
	close(SOut).

batch_dialogue_to_paraphrase_csv(SIn, SOut, NIn-NOut) :-
	read(SIn, T),
	!,
	batch_dialogue_to_paraphrase_csv1(T, SIn, SOut, NIn-NOut).

batch_dialogue_to_paraphrase_csv1(end_of_file, _SIn, _SOut, NIn-NIn) :-
	!.
batch_dialogue_to_paraphrase_csv1(T, SIn, SOut, NIn-NOut) :-
	(   get_paraphrases_and_source_from_dialogue_record(T, LastPara, Source, Para) ->
	    format(SOut, '~N~w,~w,~w~n', [LastPara, Source, Para]),
	    NNext is NIn + 1
	;
	    format2error('~N~n*** Warning: unable to process record ~w~n', [T]),
	    NNext is NIn
	),
	!,
	batch_dialogue_to_paraphrase_csv(SIn, SOut, NNext-NOut).

get_paraphrases_and_source_from_dialogue_record(Record, LastPara, Source, Para) :-
	Record = dialogue_record(List),
	(   member(last_paraphrase=LastPara, List) ->
	    true
	;
	    LastPara = '*no_paraphrase*'
	),
	(   member(sent=Source, List) ->
	    true
	;
	    Source = '*no_source*'
	),
	(   member(paraphrase=[Para, _Judgement], List) ->
	    true
	;
	    Para = '*no_paraphrase*'
	),
	!.
	
%---------------------------------------------------------------

get_current_namespace_from_dialogue_state(Namespace) :-
	(   get_current_dialogue_state(DialogueState) ->
	    true
	;
	    format('~N*** Error: unable to find dialogue state.~n', []),
	    fail
	),
	(   current_predicate(user:get_namespace_from_dialogue_state/2) ->
	    true
	;
	    format('~N*** Error: user:get_namespace_from_dialogue_state/2 is not defined. Unable to find current namespace.~n', []),
	    fail
	),
	(   user:get_namespace_from_dialogue_state(DialogueState, Namespace) ->
	    true
	;
	    format('~N*** Error: Unable to find current namespace.~n', []),
	    fail
	),
	!.

%---------------------------------------------------------------

% Predicates for manipulating the stored dialogue state used by the top-loop.

%:- dynamic last_init_dialogue_state_parameter/1.
%
%set_last_init_dialogue_state_parameter(Param) :-
%	retractall(last_init_dialogue_state_parameter(_)),
%	assertz(last_init_dialogue_state_parameter(Param)).
%
%get_last_init_dialogue_state_parameter(Param) :-
%	last_init_dialogue_state_parameter(Param).

:- dynamic current_dialogue_state/1.

get_current_dialogue_state(DialogueState) :-
	current_dialogue_state(DialogueState),
	!.
get_current_dialogue_state(DialogueState) :-
	current_predicate(Package:initial_dialogue_state/1),
	Package:initial_dialogue_state(DialogueState),
	!.
get_current_dialogue_state(X) :-
	format2error('~N*** Error: bad call: ~w~n', [get_current_dialogue_state(X)]),
	fail.

set_current_dialogue_state(DialogueState) :-
	retractall(current_dialogue_state(_)),
	asserta(current_dialogue_state(DialogueState)),
	!.
set_current_dialogue_state(X) :-
	format2error('~N*** Error: bad call: ~w~n', [set_current_dialogue_state(X)]),
	fail.

initialise_dialogue_state :-
	current_predicate(Package:initial_dialogue_state/1),
	Package:initial_dialogue_state(DialogueState),
	set_current_dialogue_state(DialogueState),
	!.
initialise_dialogue_state :-
	format2error('~N*** Error: bad call: ~w~n', [initialise_dialogue_state]),
	fail.

initialise_dialogue_state(Arg) :-
	current_predicate(Package:initial_dialogue_state/2),
	Package:initial_dialogue_state(Arg, DialogueState),
	set_current_dialogue_state(DialogueState),
	!.
initialise_dialogue_state(Arg) :-
	format2error('~N*** Error: bad call: ~w~n', [initialise_dialogue_state(Arg)]),
	fail.

%---------------------------------------------------------------

load_dialogue_corpus_judgements_file(JudgementsFile) :-
	abolish_if_defined(user:judged_dialogue_processing/4),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	file_exists(AbsJudgementsFile, read),
	safe_compile(user, AbsJudgementsFile),
	format('~NLoaded dialogue judgements from ~w~n', [AbsJudgementsFile]),
	!.
load_dialogue_corpus_judgements_file(JudgementsFile) :-
	format('~N~n*** Warning: unable to load dialogue judgements file ~w~n', [JudgementsFile]).
	
%---------------------------------------------------------------

/*
Accessing the stored judgements used by batch processing.

We need to be very careful about variables. It's possible for
either the query term or the database record to contain variables, and
we don't want either one to instantiate the other. Essentially, we
are trying to do an "id_call", by analogy with "id_member".

We consequently ground the database clauses; then after matching, we
unground and check that the [InArgs, OutArgs] pair is the same up to variable
renaming.
*/

get_judgement(_Type, _InArgs, OutArgs, Judgement) :-
	no_result_out_args(OutArgs),
	!,
	Judgement = no_result.
get_judgement(Type, InArgs, OutArgs, Judgement) :-
	current_predicate(user:judged_dialogue_processing/4),
	copy_term([InArgs, OutArgs], [InArgs1, OutArgs1]),
	user:judged_dialogue_processing(InArgs1, OutArgs1, Type, Judgement),
	unground([InArgs1, OutArgs1], [InArgs2, OutArgs2]),
	identical_up_to_variable_renaming([InArgs, OutArgs], [InArgs2, OutArgs2]),
	!.
% If we can't find a stored judgement, default is a version of question-mark.
get_judgement(im, _InArgs, _OutArgs, '?im').
get_judgement(dm, _InArgs, _OutArgs, '?dm').
get_judgement(om, _InArgs, _OutArgs, '?om').
get_judgement(paraphrase, _InArgs, _OutArgs, '?paraphrase').
get_judgement(_Type, _InArgs, _OutArgs, ?).

get_good_judgement(Type, InArgs, GoodOutArgs) :-
	current_predicate(user:judged_dialogue_processing/4),
	copy_term([InArgs], [InArgs1]),
	user:judged_dialogue_processing(InArgs1, GoodOutArgs, Type, good),
	unground([InArgs1], [InArgs2]),
	identical_up_to_variable_renaming([InArgs], [InArgs2]),
	!.

% We want to distinguish the different flavours of '?' so that we can efficiently query-replace
% when judging.

make_judgement_canonical('?im', '?') :- !.
make_judgement_canonical('?dm', '?') :- !.
make_judgement_canonical('?om', '?') :- !.
make_judgement_canonical('?paraphrase', '?') :- !.
make_judgement_canonical(Judgement, Judgement).

no_result_out_args(['WARNING: PARAPHRASE GENERATION FAILED']).

%---------------------------------------------------------------
 
/*

Our strategy for updating the stored judgements is to start by
copying all the loaded ones into a temporary database predicate. We
then manipulate this database predicate by adding new judgements and
retracting and old ones that are inconsistent with the new
material. Finally, we dump out the temporary predicate to the
judgements file.

*/

:- dynamic tmp_judged_dialogue_processing/4.

get_tmp_judgement(Type, InArgs, OutArgs, Judgement) :-
	copy_term(InArgs, InArgs1),
	tmp_judged_dialogue_processing(InArgs1, OutArgs1, Type, Judgement),
	unground([InArgs1, OutArgs1], [InArgs2, OutArgs2]),
	identical_up_to_variable_renaming(InArgs, InArgs2),
	OutArgs2 = OutArgs,
	!.

update_dialogue_judgements(ResultsFile, JudgementsFile, NNew, NChanged) :-
	retractall(tmp_judged_dialogue_processing(_, _, _, _)),
	prolog_file_to_list(ResultsFile, ResultsList),
	update_prolog_database_from_results_list(ResultsList, 0-NNew, 0-NChanged),
	create_new_judgements_file_from_prolog_database(JudgementsFile).

update_prolog_database_from_results_list([], NewIn-NewIn, ChangedIn-ChangedIn).
update_prolog_database_from_results_list([F | R], NewIn-NewOut, ChangedIn-ChangedOut) :-
	update_prolog_database_from_results_list_item(F, NewIn-NewNext, ChangedIn-ChangedNext),
	!,
	update_prolog_database_from_results_list(R, NewNext-NewOut, ChangedNext-ChangedOut).

update_prolog_database_from_results_list_item(dialogue_record(List), NewIn-NewOut, ChangedIn-ChangedOut) :-
	update_for_im(List, NewIn-New1, ChangedIn-Changed1),
	update_for_dm(List, New1-New2, Changed1-Changed2),
	update_for_om(List, New2-New3, Changed2-Changed3),
	update_for_paraphrase(List, New3-NewOut, Changed3-ChangedOut),
	!.
update_from_results_list_item(Other, New, Changed) :-
	format('~N*** ERROR: bad call: ~w~n',
	       [update_from_results_list_item(Other, New, Changed)]),
	fail.

update_for_im(List, NewIn-NewOut, ChangedIn-ChangedOut) :-
	member(in_state=InState, List),
	member(word_list=WordList, List),
	member(parse=Parse, List),
	member(dialogue_move=[DialogueMove, Judgement], List),

	InArgs = [WordList, Parse, InState],
	OutArgs = [DialogueMove],

	(   member(Judgement, ['?', '?im', '?dm', '?om', '?paraphrase', no_result]) ->
	    NewIn = NewOut,
	    ChangedIn = ChangedOut ;
	    
	    member(Judgement, [good, ok, bad]) ->
	    update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, im, Judgement),
					   NewIn-NewOut, ChangedIn-ChangedOut) ;

	    format('~N*** WARNING: unknown judgement discarded: ~w~n', [Judgement]),
	    NewIn = NewOut,
	    ChangedIn = ChangedOut
	),
	!.
update_for_im(_List, NewIn-NewIn, ChangedIn-ChangedIn).

update_for_dm(List, NewIn-NewOut, ChangedIn-ChangedOut) :-
	member(in_state=InState, List),
	member(dialogue_move=[DialogueMove, _IMJudgement], List),
	member(abstract_action_and_out_state=[AbstractAction, OutState, Judgement], List),

	InArgs = [DialogueMove, InState],
	OutArgs = [AbstractAction, OutState],

	(   member(Judgement, ['?', '?im', '?dm', '?om', '?paraphrase', no_result]) ->
	    NewIn = NewOut,
	    ChangedIn = ChangedOut ;
	    
	    member(Judgement, [good, ok, bad]) ->
	    update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, dm, Judgement),
					   NewIn-NewOut, ChangedIn-ChangedOut) ;

	    format('~N*** WARNING: unknown judgement discarded: ~w~n', [Judgement]),
	    NewIn = NewOut,
	    ChangedIn = ChangedOut
	),
	!.
update_for_dm(_List, NewIn-NewIn, ChangedIn-ChangedIn).

update_for_om(List, NewIn-NewOut, ChangedIn-ChangedOut) :-
	member(abstract_action_and_out_state=[AbstractAction, _OutState, _DMJudgement], List),
	member(action=[Action, Judgement], List),

	InArgs = [AbstractAction],
	OutArgs = [Action],

	(   member(Judgement, ['?', '?im', '?dm', '?om', '?paraphrase', no_result]) ->
	    NewIn = NewOut,
	    ChangedIn = ChangedOut ;
	    
	    member(Judgement, [good, ok, bad]) ->
	    update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, om, Judgement),
					   NewIn-NewOut, ChangedIn-ChangedOut) ;

	    format('~N*** WARNING: unknown judgement discarded: ~w~n', [Judgement]),
	    NewIn = NewOut,
	    ChangedIn = ChangedOut
	),
	!.
update_for_om(_List, NewIn-NewIn, ChangedIn-ChangedIn).

update_for_paraphrase(List, NewIn-NewOut, ChangedIn-ChangedOut) :-
	member(word_list=WordList, List),
	member(implicit_context=ImplicitContext, List),
	member(last_paraphrase=LastParaphrase, List),
	member(paraphrase=[Paraphrase, Judgement], List),

	InArgs = [WordList, LastParaphrase, ImplicitContext],
	OutArgs = [Paraphrase],

	(   member(Judgement, ['?', '?im', '?dm', '?om', '?paraphrase', no_result]) ->
	    NewIn = NewOut,
	    ChangedIn = ChangedOut ;
	    
	    member(Judgement, [good, ok, bad]) ->
	    update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, paraphrase, Judgement),
					   NewIn-NewOut, ChangedIn-ChangedOut) ;

	    format('~N*** WARNING: unknown judgement discarded: ~w~n', [Judgement]),
	    NewIn = NewOut,
	    ChangedIn = ChangedOut
	),
	!.
update_for_paraphrase(_List, NewIn-NewIn, ChangedIn-ChangedIn).

% Next predicate stores the judgement, manipulating tmp_judged_dialogue_processing/4.
% Several cases.

% We already have this judgement stored. Don't need to do anything
update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
			       NewIn-NewOut, ChangedIn-ChangedOut) :-
	get_judgement(Type, InArgs, OutArgs, Judgement1),
	Judgement = Judgement1,
	NewIn = NewOut,
	ChangedIn = ChangedOut,
	!.
% We have a new judgement. Store in tmp_judgements. Check to see if it's a new judgement or a change.
update_from_results_list_item1(judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
			       NewIn-NewOut, ChangedIn-ChangedOut) :-
	(   (   get_judgement(Type, InArgs, OutArgs, OldJudgement),
		Judgement \== OldJudgement,
		OldJudgement \== ? ) ->

	    ChangedOut is ChangedIn + 1,
	    NewOut = NewIn ;

	    ChangedOut is ChangedIn,
	    NewOut is NewIn + 1
	),
	Record = tmp_judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
	copy_term(Record, Record1),
	make_ground(Record1),
	assertz(Record1),
	!.

create_new_judgements_file_from_prolog_database(JudgementsFile) :-
	open(JudgementsFile, write, S),
	write_old_judgements_to_stream(S),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format(S, '~N~n%Judgements added ~w~n', [Timestamp]),
	write_new_judgements_to_stream(S),
	close(S).

% Write out all the old judgements, except the ones invalidated by new ones
write_old_judgements_to_stream(S) :-
	current_predicate(user:judged_dialogue_processing/4),
	user:judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
	unground([InArgs, OutArgs], [InArgs1, OutArgs1]),
	(   ( get_tmp_judgement(Type, InArgs1, OutArgs1, NewJudgement), NewJudgement \== Judgement ) ->
	    CommentedOutP = commented_out
	;
	    otherwise ->
	    CommentedOutP = not_commented_out
	),
	Record = judged_dialogue_processing(InArgs1, OutArgs1, Type, Judgement),
	write_judgement_record_to_stream(S, Record, CommentedOutP),
	fail.
write_old_judgements_to_stream(_S).

% Write out new judgements
write_new_judgements_to_stream(S) :-
	tmp_judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
	Record = judged_dialogue_processing(InArgs, OutArgs, Type, Judgement),
	write_judgement_record_to_stream(S, Record, not_commented_out),
	fail.
write_new_judgements_to_stream(_S).

% See comment on get_judgement/4 above for stuff about grounding of records.
write_judgement_record_to_stream(S, Record, CommentedOutP) :-
	make_ground(Record),
	(   CommentedOutP = not_commented_out ->
	    format(S, '~N~n', [])
	;
	    otherwise ->
	    format(S, '~N~n%', [])
	),
	write_term(S, Record, [quoted(true), numbervars(false)]),
	format(S, '.~n', []).

directory_for_wavfile(Wavfile, Dir) :-
	split_atom_into_words(Wavfile, 0'/, List),
	last_but_one_element_of_list(List, Dir),
	!.
directory_for_wavfile(Wavfile, Dir) :-
	format2error('~N*** Error: bad call: ~w~n', [directory_for_wavfile(Wavfile, Dir)]),
	fail.

%---------------------------------------------------------------
 
make_cvs_summary_of_dialogue_output_file(File) :-
	change_extension_in_file(File, csv, CSVFile),
	prolog_file_to_list(File, List),
	dialogue_file_output_to_sent_paraphrase_list(List, List1),
	list_of_lists_to_csv_file(List1, CSVFile),
	length(List, N),
	format('~N--- CSV summary of dialogue file (~d records) written to ~w~n', [N, CSVFile]),
	!.
make_cvs_summary_of_dialogue_output_file(File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_cvs_summary_of_dialogue_output_file(File)]),
	fail.

dialogue_file_output_to_sent_paraphrase_list([], []).
dialogue_file_output_to_sent_paraphrase_list([F | R], [F1 | R1]) :-
	dialogue_file_record_to_sent_paraphrase_item(F, F1),
	!,
	dialogue_file_output_to_sent_paraphrase_list(R, R1).
dialogue_file_output_to_sent_paraphrase_list([_F | R], R1) :-
	!,
	dialogue_file_output_to_sent_paraphrase_list(R, R1).

dialogue_file_record_to_sent_paraphrase_item(dialogue_record(List), [Sent, Paraphrase]) :-
	member(sent=Sent, List),
	(   ( member(paraphrase = [Paraphrase, _Judgement], List), \+ no_result_out_args([Paraphrase]) ) ->
	    true
	;
	    otherwise ->
	    Paraphrase = 'NO OUTPUT PRODUCED'
	),
	!.

%---------------------------------------------------------------

atom_to_parse_unless_nothing_recognised(nothing_recognised, nothing_recognised) :-
	!.
atom_to_parse_unless_nothing_recognised(Sent, Parse) :-
	user:atom_to_parse_using_current_parser(Sent, '.MAIN', Parse).

%---------------------------------------------------------------

last_but_one_element_of_list([X, _], X) :-
	!.
last_but_one_element_of_list([_F | R], X) :-
	last_but_one_element_of_list(R, X).

implicit_context(ImplicitContext) :-
	(   get_notional_time(Datime) ->
	    true
	;
	    datime(Datime)
	),
	(   get_notional_speaker(Name) ->
	    true
	;
	    Name = '*no_notional_speaker*'
	),
	ImplicitContext = [time=Datime, speaker=Name],
	!.
