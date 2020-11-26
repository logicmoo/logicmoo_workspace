  
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/strcat_semantics').
:- use_module('$REGULUS/Prolog/stanford_parser').
:- use_module('$REGULUS/Prolog/stanford_parser_compile').
:- use_module('$REGULUS/Prolog/regulus2dcg').
:- use_module('$REGULUS/Prolog/regulus2gemini').
:- use_module('$REGULUS/Prolog/regulus_eval').
:- use_module('$REGULUS/Prolog/generator_compiler').
:- use_module('$REGULUS/Prolog/surface_parse').
:- use_module('$REGULUS/Prolog/ebl_grammar_probs').
:- use_module('$REGULUS/Prolog/ebl_compile_operationality').
:- use_module('$REGULUS/Prolog/analysis_constraints').
:- use_module('$REGULUS/Prolog/random_generate').
:- use_module('$REGULUS/Prolog/logging').
:- use_module('$REGULUS/Prolog/help').
:- use_module('$REGULUS/Prolog/progress').

:- use_module('$REGULUS/Prolog/translate').
:- use_module('$REGULUS/Prolog/generate').
:- use_module('$REGULUS/Prolog/translate_acquire').
:- use_module('$REGULUS/Prolog/translate_bidirectional').
:- use_module('$REGULUS/Prolog/resolve').
:- use_module('$REGULUS/Prolog/speech_output').
:- use_module('$REGULUS/Prolog/split_corpus').
:- use_module('$REGULUS/Prolog/paraphrases').
:- use_module('$REGULUS/Prolog/extract_corpora').
:- use_module('$REGULUS/Prolog/parse_tree2categories').

:- use_module('$REGULUS/Prolog/lite_flat_file_to_grammar').

:- use_module('$REGULUS/Prolog/dialogue').
:- use_module('$REGULUS/Prolog/nbest').
:- use_module('$REGULUS/Prolog/compile_lf_patterns').
:- use_module('$REGULUS/Prolog/compile_lf_rewrite').

:- use_module('$REGULUS/Prolog/java_gui_utils').
 
:- use_module('$REGULUS/Prolog/stepper').
:- use_module('$REGULUS/Prolog/stepper_read').
:- use_module('$REGULUS/Prolog/stepper_item_db').

:- use_module('$REGULUS/Prolog/recognition').
:- use_module('$REGULUS/Prolog/nl_tool_parse').

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/Alterf/Prolog/alterf_patterns').

:- use_module('$REGULUS/doc/doc_utils').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

:- use_module('$REGULUS/RegulusLanguageServer/Prolog/remote_regulus').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(ordsets)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%---------------------------------------------------------------

:- dynamic regulus_config/2.
:- dynamic regulus_runtime_config/2.
 
%---------------------------------------------------------------

regulus(ConfigFile) :-
	%log_start_new_session(ConfigFile),
	log_event('LOAD_CONFIG_FILE', [ConfigFile]),
	load_regulus_config_file(ConfigFile),
	safe_absolute_file_name(ConfigFile, AbsConfigFile),
	set_current_config_file(AbsConfigFile),
	regulus_loop.

regulus :-
	(   get_current_config_file(ConfigFile) ->
	    format('~N~n--- Starting Regulus with current config file ~w~n', [ConfigFile]),
	    regulus(ConfigFile)
	;
	    format2error('~N~n*** Error: no current config file~n', []),
	    fail
	).
 
regulus_batch(Commands) :-
	log_event('START_BATCH_RUN', []),
	regulus_batch_loop(Commands),
	log_event('END_BATCH_RUN', []).

regulus_batch(ConfigFile, Commands) :-
	log_event('START_BATCH_RUN', []),
	load_regulus_config_file(ConfigFile),
	log_event('LOAD_CONFIG_FILE', [ConfigFile]),
	regulus_batch_loop(Commands),
	log_event('END_BATCH_RUN', []).

regulus_batch(ConfigFile, ExtraConfigEntries, Commands) :-
	log_event('START_BATCH_RUN', []),
	load_regulus_config_file(ConfigFile),
	log_event('LOAD_CONFIG_FILE', [ConfigFile]),
	load_extra_config_entries(ExtraConfigEntries),
	regulus_batch_loop(Commands),
	log_event('END_BATCH_RUN', []).

regulus_batch_storing_errors(ConfigFile, Commands, ErrorList) :-
	init_stored_errors,
	regulus_batch(ConfigFile, Commands),
	get_stored_errors_list(ErrorList).

regulus_batch_storing_errors(ConfigFile, ExtraConfigEntries, Commands, ErrorList) :-
	init_stored_errors,
	regulus_batch(ConfigFile, ExtraConfigEntries, Commands),
	get_stored_errors_list(ErrorList).

%---------------------------------------------------------------

config_file_alist_file('$REGULUS/Prolog/config_files.pl').
regulus_gui_communication_file('$REGULUS/tmp_java_gui_communication_file.txt').

get_config_file_alist_for_java_gui(AList) :-
	on_exception(Exception,
		     get_config_file_alist_for_java_gui1(AList),
		     handle_exception_for_get_config_file_alist_for_java_gui(Exception, AList)
		    ),
	!.
get_config_file_alist_for_java_gui(AList) :-
	AList = error.

get_config_file_alist_for_java_gui1(AlistTerm) :-
	format('~N--- Getting config file info for GUI~n', []),
	config_file_alist_file(File),
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Trying to read info from ~w~n', [AbsFile]),
	(   file_exists(AbsFile) ->
	    
	    prolog_file_to_list(File, Alist0),
	    make_file_names_absolute_in_alist(Alist0, Alist),
	    AlistTerm =.. [alist | Alist],
	    format('~N--- Found config file info for GUI~n', [])
	;

	    format2error('~N*** Error: can\'t find file: ~w~n', [AbsFile]),
	    AlistTerm = cant_find_file
	),
	flush_output(user),
	!.

handle_exception_for_get_config_file_alist_for_java_gui(_Exception, Alist) :-
	format2error('*** Error: internal error when reading config file info~n',[]),
	Alist = error.

make_file_names_absolute_in_alist([], []).
make_file_names_absolute_in_alist([F | R], [F1 | R1]) :-
	make_file_name_absolute_in_alist_entry(F, F1),
	!,
	make_file_names_absolute_in_alist(R, R1).

/*

An alist entry should be of the form

file(IDString, File, ...Commands...)

e.g.

file("Toy1 EngFre", '$REGULUS/Examples/Toy1/scripts/toy1_slt_batch.cfg', "LOAD", "LOAD_TRANSLATE").

*/

make_file_name_absolute_in_alist_entry(AlistEntry, AlistEntry1) :-
	AlistEntry =.. [F, ID, File | Commands],
	AlistEntry1 =.. [F, ID, AbsFileString | Commands],
	safe_absolute_file_name(File, AbsFile),
	atom_codes(AbsFile, AbsFileString),
	!.
make_file_name_absolute_in_alist_entry(X, Y) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [make_file_name_absolute_in_alist_entry(X, Y)]),
	fail.

%---------------------------------------------------------------

load_regulus_config_file_string(FileString, OutChars) :-
	with_output_to_chars(load_regulus_config_file_string0(FileString),
			     OutChars).

load_regulus_config_file_string0(FileString) :-
	is_prolog_string(FileString),
	atom_codes(File, FileString),
	%log_start_new_session(File),
	log_event('LOAD_CONFIG_FILE', [File]),
	load_regulus_config_file(File).

%---------------------------------------------------------------

load_regulus_config_file(File) :-
	load_regulus_config_file_init(File),
	load_regulus_config_file1(File).

load_regulus_config_file1(File) :-
	safe_absolute_file_name(File, AbsoluteFile),
	(   file_exists(AbsoluteFile, read) ->
	    
	    format('~NLoading settings from Regulus config file ~w~n', [AbsoluteFile]),
	    load_regulus_config_file2(AbsoluteFile)
	;
	    
	    format2error('~NError: unable to find config file ~w~n', [AbsoluteFile]),
	    fail
	).

load_regulus_config_file_init(File) :-
	retractall(regulus_config(_, _)),
	retractall(regulus_runtime_config(_)),
	set_regulus_runtime_config(current_cfg_file, File).

load_regulus_config_file2(File) :-
	open(File, read, S),
	load_regulus_config_stream(S),
	close(S).

load_regulus_config_stream(S) :-
	read(S, T),
	load_regulus_config_stream1(T, S).

load_regulus_config_stream1(end_of_file, _S) :-
	!.
load_regulus_config_stream1(T, S) :-
	load_regulus_config_item(T),
	!,
	load_regulus_config_stream(S).

%---------------------------------------------------------------

load_extra_config_entries(Entries) :-
	load_extra_config_entries1(Entries),
	!.
load_extra_config_entries(Other) :-
	format2error('~N*** Error: bad call: ~w~n', [load_extra_config_entries(Other)]),
	fail.

load_extra_config_entries1([]) :-
	!.
load_extra_config_entries1([F | R]) :-
	load_extra_config_entry(F),
	!,
	load_extra_config_entries1(R).

load_extra_config_entry(delete(Entry)) :-
	!,
	retractall(Entry).
load_extra_config_entry(Entry) :-
	load_regulus_config_item(Entry).

%---------------------------------------------------------------

load_regulus_config_item(include(File)) :-
	!,
	load_regulus_config_file1(File).
load_regulus_config_item(default_regulus_encoding(Encoding)) :-
	atomic(Encoding),
	!,
	retractall(user:default_regulus_encoding(_)),
	assertz(user:default_regulus_encoding(Encoding)),
	format('~N--- Set default Regulus encoding to "~w"~n', [Encoding]).
load_regulus_config_item(regulus_config(Key, Value)) :-
	ground(Key),
	\+ number(Key),
	ground(Value),
	!,
	assertz(regulus_config(Key, Value)).
%load_regulus_config_item(file_search_path(Key, Value)) :-
%	atomic(Key),
%	atomic(Value),
%	retractall(user:file_search_path(Key, _)),
%	assertz(file_search_path(Key, Value)),
%	!.
% Keep the earlier version if there are two conflicting file_search_path declarations
% since this is consistent with what we do about regulus_config entries, and it means
% that material at the beginning of a config file consistently overrides material later.
load_regulus_config_item(file_search_path(Key, Value)) :-
	atomic(Key),
	atomic(Value),
	(   ( current_predicate(user:file_search_path/2), user:file_search_path(Key, _) ) ->
	    true
	;
	    assertz(file_search_path(Key, Value))
	),
	!.
load_regulus_config_item(Other) :-
	format2error('~NError: bad item in config file: ~w~n', [Other]),
	fail.

%---------------------------------------------------------------

regulus_loop :-
	on_exception(
	_Exception, 
	regulus_loop1,
	handle_regulus_exit
    ).

regulus_loop1 :-
	format('~N~n>> ', []),
	read_line(user, Chars),
	(   split_string_into_words(Chars, ['EXIT']) ->
	    handle_regulus_exit
	;
	    process_regulus_loop_item_string0(Chars),
	    !,
	    regulus_loop1
	).

handle_regulus_exit :-
	format('~N--- Doing Regulus clean-up... ', []),
	exit_bidirectional_mode_if_necessary,
	finish_logfile,
	format('done~n', []).

handle_regulus_exit_batch :-
	format('~N--- Doing Regulus clean-up for batch... ', []),
	finish_logfile,
	format('done~n', []).

%---------------------------------------------------------------

regulus_batch_loop(List) :-
	on_exception(
	_Exception, 
	regulus_batch_loop1(List),
	handle_regulus_exit_batch
    ).

regulus_batch_loop1([]) :-
	handle_regulus_exit_batch,
	!.
regulus_batch_loop1([F | R]) :-
	process_regulus_loop_item_string0(F),
	!,
	regulus_batch_loop1(R).

%---------------------------------------------------------------

remote_process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorText) :-
	format('~N--- Execute remote command for GUI: "~s"~n', [InChars]),
	sleep_if_sicstus4_and_using_gui,
	is_in_bidirectional_mode,
	remote_regulus_call(process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorText)),
	format('~N--- Executed remote command~n', []),
	!.
remote_process_regulus_loop_item_for_java_gui(InChars, _CommentChars, _AnswerChars, OutChars, OutPromptChars, Status, ErrorText) :-
	(   is_in_bidirectional_mode ->
	    OutChars = "\nNot in bidirectional mode" ;
	    format_to_chars('~NError processing command: ~s~n', [InChars], OutChars)
	),
	OutPromptChars = no_menu,
	Status = error,
	ErrorText = "AO ERRORS".
	    
%---------------------------------------------------------------

process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorString) :-
	format('~N--- Execute command for GUI: "~s"~n', [InChars]),
	%format('~N~n-- Executing: ~q~n~n',
	%       [process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status)]),
	sleep_if_sicstus4_and_using_gui,
	set_answer_string(AnswerChars),
	init_stored_errors,
	with_output_to_chars(process_regulus_loop_item_for_java_gui1(InChars, CommentChars, OutPromptChars, Status),
			     OutChars0),
	get_stored_errors(ErrorString0),
	wrap_lines_in_string_for_java_gui(ErrorString0, ErrorString),
	format('~NError string:~n"~s"~n', [ErrorString]),
	%write_regulus_gui_command_output_to_communication_file(OutChars0),
	%OutChars = "Command processed",
	OutChars0 = OutChars,
	%format('~N~n-- Executed: ~q~n~n',
	%       [process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status)]),
	format('~N--- Executed command~n', []),
	!.

process_regulus_loop_item_for_java_gui1(InChars, CommentChars, MenuInfo, Status) :-
	on_exception(
	Exception, 
	( process_regulus_loop_item_string(InChars, CommentChars), MenuInfo = no_menu, Status = ok ),
	( handle_exception_for_java_gui(Exception, Status, MenuInfo) )
    ),
	!.
process_regulus_loop_item_for_java_gui1(InChars, _CommentChars, MenuInfo, Status) :-
	format('~NError processing command: ~s~n', [InChars]),
	MenuInfo = no_menu,
	Status = error.

%---------------------------------------------------------------

write_regulus_gui_command_output_to_communication_file(String) :-
	regulus_gui_communication_file(File),
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),
	format(S, '~s', [String]),
	close(S),
	!.

%---------------------------------------------------------------

process_regulus_loop_item_speech_input(Result) :-
	Result = recognition_failed(_),
	!.
process_regulus_loop_item_speech_input(Result) :-
	Result = recognition_succeeded(_Conf, Atom, _LF),
	!,
	atom(Atom),
	atom_codes(Atom, String),
	process_regulus_loop_item_string0(String).
process_regulus_loop_item_speech_input(Result) :-
	Result = recognition_succeeded([First | Rest]),
	!,
	length(Rest, N),
	(   N = 0 ->
	    true
	;
	    N = 1 ->
	    format2error('~N~n*** Warning: N-best input. Discarding 1 non-top hypothesis~n', [])
	;
	    otherwise ->
	    format2error('~N~n*** Warning: N-best input. Discarding ~d non-top hypotheses~n', [N])
	),
	First = rec_result(_Conf, Atom, _LF),
	atom(Atom),
	atom_codes(Atom, String),
	process_regulus_loop_item_string0(String).
process_regulus_loop_item_speech_input(Result) :-
	format2error('~N*** Error: unable to process speech result "~w"~n', [Result]),
	fail.

%---------------------------------------------------------------

process_regulus_loop_item_string0(Chars) :-
	pre_process_string_wrt_bidirectional_processing(Chars, Chars1, LocalOrRemote),
	(   LocalOrRemote = local ->
	    safe_process_regulus_loop_item_string(Chars1) ;

	    LocalOrRemote = remote ->
	    format('~N~n*** PERFORMING REMOTE PROCESSING FOR "~s" ***~n', [Chars1]),
	    process_regulus_loop_item_string_remote(Chars1, OutChars),
	    format('~N*** OUTPUT FROM REMOTE PROCESSING ***~n~s~n*** END OF OUTPUT FROM REMOTE PROCESSING ***~n~n', [OutChars])
	),
	!.
process_regulus_loop_item_string0(_Chars) :-
	format2error('~N*** Error in process_regulus_loop_item_string0~n', []).

process_regulus_loop_item_with_output_to_string(InChars, OutChars) :-
	with_output_to_chars(safe_process_regulus_loop_item_string(InChars), OutChars),
	!.

%---------------------------------------------------------------

safe_process_regulus_loop_item_string(Chars) :-
	on_exception(
	Exception, 
	(   timed_call(process_regulus_loop_item_string(Chars, "*null_comment*"),
		   Time),
	    format('~N~n--- Performed command ~s, time = ~2f seconds~n', [Chars, Time])
	),
	(   format2error('~N--- Error processing command: ~s~n', [Chars]),
	    inform_about_top_level_regulus_error(Exception)
	)
    ),
	!.
safe_process_regulus_loop_item_string(Chars) :-
	format2error('~N--- Error processing command: ~s~n', [Chars]).
 
%---------------------------------------------------------------
 
process_regulus_loop_item_string(Chars, CommentChars) :-
	interpret_string_as_raw_lf_input(Chars, LF),
	!,
	format_to_atom('~q', [LF], LFAtom),
	log_event('PROCESS_LF', CommentChars, [LFAtom]),
	regulus_dcg_parse_loop_item(lf(LF)).
process_regulus_loop_item_string(Chars, CommentChars) :-
	interpret_string_as_wavfile_input(Chars, WavfileAtom),
	!,
	(   WavfileAtom = error ->
	    true
	; 
	    otherwise ->
	    log_event('PROCESS_WAVFILE', CommentChars, [WavfileAtom]),
	    recognise_from_wavfile_as_defined_by_config_file(WavfileAtom, RecResult),
	    process_regulus_loop_item_speech_input(RecResult)
	).
process_regulus_loop_item_string(Chars, CommentChars) :-
	split_string_into_words(Chars, Words),
	loop_command(Words, HelpMessage),
	format('~N(~w)~n', [HelpMessage]),
	!,
	Words = [Command | Args],
	log_event(Command, CommentChars, Args),
	process_regulus_loop_command(Words).
process_regulus_loop_item_string(Chars, CommentChars) :-
	split_string_into_words(Chars, Words),
	Words = [NumberAtom],
	atom_to_int(NumberAtom, N),
	interpret_number_as_test_sent(N, SentChars, Comment),
	log_event('TEST_CORPUS_EXAMPLE', CommentChars, [N]),
	atom_codes(Sent, SentChars),
	format('~NInput from test corpus: "~w"', [Sent]),
	(   Comment = '' ->
	    format('~n', []) ;
	    format(' (~w)~n', [Comment])
	),
	Chars \== SentChars,
	process_regulus_loop_item_string(SentChars, CommentChars),
	!.
process_regulus_loop_item_string(Chars, CommentChars) :-
	maybe_echo_text(Chars),
	atom_codes(Atom, Chars),
	log_event('PARSE', CommentChars, [Atom]),
	(   (   help_response_is_on,
		get_regulus_runtime_config(processing_mode, translate),
		current_predicate(check_interlingua:check_interlingua/3)
	    ) ->
	    get_current_discourse_context(Context),
	    show_help_response_for_translation_context(Atom, Context)
	;
	    help_response_is_on ->
	    show_help_response(Atom)
	;
	    true
	),
	regulus_dcg_parse_loop_item(Chars),
	!.

interpret_number_as_test_sent(N, SentChars, Comment) :-
	(   regulus_config(test_corpus, File) ->
	    true
	;
	    format2error('~N*** Error: numerical input is only meaningful with a test_corpus declaration~n', []),
	    fail
	),
	absolute_file_name(File, AbsFile),
	(   file_exists(AbsFile, read) ->
	    regulus_file_to_list(AbsFile, List)
	;
	    format2error('~N*** Error: can\'t find test_corpus file ~w~n', [AbsFile]),
	    fail
	),
	(   member(sent(N, Sent), List) ->
	    atom_codes_if_necessary(Sent, SentChars),
	    Comment = '' ;

	    member(sent(N, Sent, Comment), List) ->
	    atom_codes_if_necessary(Sent, SentChars)
	;
	    
	    format2error('~N*** Error: can\'t find record ~d in test_corpus file ~w~n',
			 [N, AbsFile]),
	    fail
	),
	!.

atom_codes_if_necessary(Sent, SentChars) :-
	is_prolog_string(Sent),
	!,
	Sent = SentChars.
atom_codes_if_necessary(Sent, SentChars) :-
	atom(Sent),
	!,
	atom_codes(Sent, SentChars).

maybe_echo_text(Chars) :-
	get_regulus_runtime_config(echo_text, yes),
	atom_codes(Atom, Chars),
	format('~N~n=====================================================================~n~n', []),
	format('~NProcessing text: "~w"~n', [Atom]),
	!.
maybe_echo_text(_Chars).

process_regulus_loop_command0(Words) :-
	loop_command(Words, HelpMessage),
	format('~N(~w)~n', [HelpMessage]),
	(   process_regulus_loop_command(Words) ->
	    true ;
	    format('~NUnable to process loop command "~w"~n', [Words])
	).

loop_command(Command, ShortHelp) :-
	loop_command(Command, ShortHelp, _Section).

loop_command(['HELP'], 'Print help for all commands', 'CommandLineHelp').
loop_command(['HELP', _Id], 'Print help for commands whose name or description match the string', 'CommandLineHelp').
loop_command(['HELP_CONFIG', _Id], 'Print help for config file entries whose name or description match the string', 'CommandLineHelp').
loop_command(['DOC', _Id], 'Print documentation for command or config file entry', 'CommandLineDoc').
loop_command(['RELOAD_CFG'], 'Reload current config file', 'CommandLineIntro').
loop_command(['LOAD'], 'Load current Regulus grammar in DCG and left-corner form', 'LoadGrammar').
loop_command(['LOAD_DEBUG'], 'Load current Regulus grammar in DCG and left-corner form, including extra debugging rules in left-corner grammar', 'LoadGrammar').
loop_command(['LOAD_PREFERENCES'], 'Load parse and N-best preference files', 'ParsePreferences').
loop_command(['LOAD_SURFACE_PATTERNS'], 'Load current surface patterns and associated files', 'AlterfOverview').
loop_command(['TRIVIAL_PARSER'], 'Parse everything as "null"', 'TrivialParser').
loop_command(['DCG'], 'Use DCG parser', 'DCGParser').
loop_command(['LC'], 'Use left-corner parser', 'LCParser').
loop_command(['NUANCE_PARSER'], 'Start new Nuance nl-tool process and use it as parser', 'NuanceParser').
loop_command(['KILL_NUANCE_PARSERS'], 'Kill any outstanding nl-tool processes (may be necessary after doing NUANCE_PARSER)', 'NuanceParser').
loop_command(['SURFACE'], 'Use surface pattern-matching parser', 'AlterfOverview').
loop_command(['NUANCE'], 'Compile current Regulus grammar into Nuance GSL form', 'UGToGSL').
loop_command(['GEMINI'], 'Compile current Regulus grammar into Gemini form', 'Gemini').
%loop_command(['TRACE'], 'Switch on tracing for DCG grammar', no_section).
%loop_command(['NOTRACE'], 'Switch off tracing for DCG grammar', no_section).
loop_command(['COMPACTION'], 'Switch on compaction processing for Regulus to Nuance conversion (default)', 'UGToGSL').
loop_command(['NO_COMPACTION'], 'Switch off compaction processing for Regulus to Nuance conversion', 'UGToGSL').

loop_command(['LOAD_GENERATION'], 'Compile and load current generator grammar', 'Generation').
loop_command(['LOAD_GENERATION', _Id], 'Compile and load current generator grammar, and store as designated subdomain grammar', 'Generation').
loop_command(['LOAD_RECOGNITION_GENERATION'], 'Compile and load current generator grammar(s) for converting recognition results to other scripts', 'Generation').

loop_command(['NORMAL_PROCESSING'], 'Do normal processing on input sentences', 'CommandLineOverview').
loop_command(['TRANSLATE'], 'Do translation-style processing on input sentences', 'CommandLineOverview').
loop_command(['DIALOGUE'], 'Do dialogue-style processing on input sentences', 'CommandLineOverview').
loop_command(['GENERATION'], 'Generate from parsed input sentences', 'CommandLineOverview').
loop_command(['EBL_MODE'], 'Do EBL processing on input sentences', 'CommandLineOverview').
loop_command(['EBL_MODE_VERBOSE'], 'Do EBL processing on input sentences (verbose printing of rules)', 'CommandLineOverview').
loop_command(['ECHO_ON'], 'Echo input sentences (normally useful only in batch mode)', 'RegulusBatch').
loop_command(['ECHO_OFF'], 'Don\'t echo input sentences (default)', 'RegulusBatch').
loop_command(['LINE_INFO_ON'], 'Print line and file info for rules and lex entries in parse trees (default)', 'ParseTrees').
loop_command(['LINE_INFO_OFF'], 'Don\'t print line and file info for rules and lex entries in parse trees', 'ParseTrees').
loop_command(['PRINT_TREE_SUMMARY_ON'], 'Print summary versions of parse trees', 'ParseTrees').
loop_command(['PRINT_TREE_SUMMARY_OFF'], 'Don\'t print summary versions of parse trees (default)', 'ParseTrees').
loop_command(['PRINT_TREE_CATEGORIES_ON'], 'Print categories in parse trees', 'ParseTrees').
loop_command(['PRINT_TREE_CATEGORIES_OFF'], 'Don\'t print categories in parse trees (default)', 'ParseTrees').
loop_command(['FEAT', _Id], 'Display information for specified feature', 'GrammarDebuggingCommands').
loop_command(['CAT', _Id], 'Display information for specified category', 'GrammarDebuggingCommands').
 
loop_command(['LOAD_TRANSLATE'], 'Load translation-related files', 'TranslationMode').
loop_command(['TRANSLATE_TRACE_ON'], 'Switch on translation tracing', 'TranslationTrace').
loop_command(['TRANSLATE_TRACE_OFF'], 'Switch off translation tracing (default)', 'TranslationTrace').
loop_command(['GENERATE_TRACE_ON'], 'Switch on generation tracing', 'GenerationTrace').
loop_command(['GENERATE_TRACE_OFF'], 'Switch off generation tracing (default)', 'GenerationTrace').
loop_command(['INTERLINGUA_TRACE_ON'], 'Switch on interlingua tracing', no_section).
loop_command(['INTERLINGUA_TRACE_OFF'], 'Switch off interlingua tracing (default)', no_section).
loop_command(['INTERLINGUA_DEBUGGING_ON'], 'Switch on interlingua debugging', 'InterlinguaDebugging').
loop_command(['INTERLINGUA_DEBUGGING_OFF'], 'Switch off interlingua debugging (default)', 'InterlinguaDebugging').
loop_command(['ANSWER_ELLIPSIS_ON'], 'Switch on answer ellipsis', 'AnswerEllipsis').
loop_command(['ANSWER_ELLIPSIS_OFF'], 'Switch off answer ellipsis (default)', 'AnswerEllipsis'). 
loop_command(['BIDIRECTIONAL_ON'], 'Switch on bidirectional_mode', 'Bidirectional').
loop_command(['BIDIRECTIONAL_OFF'], 'Switch off bidirectional mode (default)', 'Bidirectional').
loop_command(['COMPILE_ELLIPSIS_PATTERNS'], 'Compile patterns used for ellipsis processing', 'TranslationEllipsis').
loop_command(['NO_ELLIPSIS_PROCESSING'], 'Unload any ellipsis processing rules that may be loaded', 'TranslationEllipsis').
loop_command(['TRANSLATE_CORPUS'], 'Process text translation corpus', 'TranslationRegressionText').
loop_command(['TRANSLATE_CORPUS', _Id], 'Process text translation corpus with specified ID', 'TranslationRegressionText').
loop_command(['CHECK_BACKTRANSLATION', _Id], 'Process Lang -> Lang output in Lang -> Int environment to check that back-translations parse', 'TranslationRegressionText').
loop_command(['STORE_TRANSLATION_TARGET_VOCAB', _Id], 'Process Source -> Target output and store target vocabulary items in the predicate regulus_preds:target_vocabulary_item', 'TranslationRegressionText').
loop_command(['TRANSLATE_PARSE_TIMES'], 'Print parse times for latest run on text translation corpus', 'TranslationRegressionTextTimes').
loop_command(['TRANSLATE_PARSE_TIMES', _Id], 'Print parse times for latest run on text translation corpus with specified ID', 'TranslationRegressionTextTimes').
loop_command(['TRANSLATE_SPEECH_CORPUS'], 'Process speech translation corpus', 'TranslationRegressionSpeech').
loop_command(['TRANSLATE_SPEECH_CORPUS', _Id], 'Process speech translation corpus with specified ID', 'TranslationRegressionSpeech').
loop_command(['TRANSLATE_SPEECH_CORPUS_AGAIN'], 'Process speech translation corpus, using recognition results from previous run', 'TranslationRegressionSpeechRerun').
loop_command(['TRANSLATE_SPEECH_CORPUS_AGAIN', _Id], 'Process speech translation corpus with specified ID, using recognition results from previous run', 'TranslationRegressionSpeechRerun').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS'], 'Update translation judgements file from annotated translation corpus output', 'TranslationJudgingProlog').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS', _Id], 'Update translation judgements file from annotated translation corpus output with specified ID', 'TranslationJudgingProlog').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH'], 'Update translation judgements file from annotated speech translation corpus output', 'TranslationJudgingProlog').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH', _Id], 'Update translation judgements file from annotated speech translation corpus output with specified ID', 'TranslationJudgingProlog').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_CSV'], 'Update translation judgements file from CSV version of annotated translation corpus output', 'TranslationJudgingCSV').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_CSV', _Id], 'Update translation judgements file from CSV version of annotated translation corpus output with specified ID', 'TranslationJudgingCSV').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH_CSV'], 'Update translation judgements file from CSV version of annotated speech translation corpus output', 'TranslationJudgingCSV').
loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH_CSV', _Id], 'Update translation judgements file from CSV version of annotated speech translation corpus output with specified ID', 'TranslationJudgingCSV').
loop_command(['UPDATE_RECOGNITION_JUDGEMENTS'], 'Update recognition judgements file from temporary translation corpus recognition judgements', 'TranslationJudgingProlog').
loop_command(['UPDATE_RECOGNITION_JUDGEMENTS', _Id], 'Update recognition judgements file from temporary translation corpus recognition judgements with specified ID', 'TranslationJudgingProlog').
loop_command(['SPLIT_SPEECH_CORPUS', training_corpus, _Id0, _Id1, _Id2], 'Split speech corpus into in-training and out-of-training pieces with respect to EBL training corpus. Arguments: <FromCorpusId>, <InTrainingCorpusId> <OutOfTrainingCorpusId>', 'SplittingCorpora').
loop_command(['SPLIT_SPEECH_CORPUS', _GrammarAtom, _Id1, _Id2], 'Split default speech corpus into in-coverage and out-of-coverage pieces with respect to the specified grammar. Arguments: <GrammarAtom>, <InCoverageCorpusId> <OutOfCoverageCorpusId>', 'SplittingCorpora').
loop_command(['SPLIT_SPEECH_CORPUS', _GrammarAtom, _Id0, _Id1, _Id2], 'Split speech corpus into in-coverage and out-of-coverage pieces with respect to the specified grammar. Arguments: <GrammarAtom>, <CorpusId>, <InCoverageCorpusId> <OutOfCoverageCorpusId>', 'SplittingCorpora').
%loop_command(['LEARN_TRANSFER_RULES', _Id], 'Learn transfer rules and generation corpus entries from annotated data of specified type in translation corpus output', 'LearningTranslation').
%loop_command(['LEARN_TRANSFER_RULES', _Id1, _Id2], 'Learn transfer rules and generation corpus entries from annotated data of specified type in translation corpus output with specified ID', 'LearningTranslation').

loop_command(['INTERLINGUA'], 'Perform translation through interlingua', 'Interlingua').
loop_command(['NO_INTERLINGUA'], 'Perform translation directly, i.e. not through interlingua', 'Interlingua').

loop_command(['SET_NOTIONAL_TIME', _Time], 'Set notional time for dialogue processing. Format = YYYY-MM-DD_HH-MM-SS, e.g. 2006-12-31_23-59-59', 'SettingDialogueContext').
loop_command(['UNSET_NOTIONAL_TIME'], 'Use real as opposed to notional time for dialogue processing', 'SettingDialogueContext').
loop_command(['DIALOGUE_TIME'], 'Show time (notional or real) currently being used for dialogue processing', 'SettingDialogueContext').

loop_command(['SET_NOTIONAL_SPEAKER', _Name], 'Set notional name of speaker for dialogue processing.', 'SettingDialogueContext').
loop_command(['UNSET_NOTIONAL_SPEAKER'], 'Remove setting of notional name for dialogue processing', 'SettingDialogueContext').
loop_command(['DIALOGUE_SPEAKER'], 'Show notional speaker (if any) currently being used for dialogue processing', 'SettingDialogueContext').

loop_command(['SET_BATCH_DIALOGUE_FORMAT', _Format], 'Set format for printing batch dialogue results. Default is "normal"', 'SettingBatchDialogueFormat').

loop_command(['LOAD_DIALOGUE'], 'Load dialogue-related files', 'DialogueMode').
loop_command(['INIT_DIALOGUE'], 'Initialise the dialogue state', 'DialogueMode').
loop_command(['REINIT_DIALOGUE'], 'Initialise the dialogue state, using the same arguments as last time this session', 'DialogueMode').
loop_command(['REINIT_DIALOGUE_KEEPING_STATE'], 'Initialise the dialogue state, using the same arguments as last time this session, but don\'t change the state', 'DialogueMode').
loop_command(['INIT_DIALOGUE', _Id], 'Initialise the dialogue state, passing it the given argument', 'DialogueMode').
loop_command(['DUMP_NBEST_TRAINING_DATA_ON'], 'Write out training data when doing batch processing of N-best results', 'DialogueNBest').
loop_command(['DUMP_NBEST_TRAINING_DATA_OFF'], 'Don\'t write out training data when doing batch processing of N-best results (default)', 'DialogueNBest').
loop_command(['BATCH_DIALOGUE'], 'Process dialogue corpus', 'RegressionTestingDialogue').
loop_command(['BATCH_DIALOGUE', _Id], 'Process dialogue corpus with specified ID', 'RegressionTestingDialogue').
loop_command(['BATCH_DIALOGUE_SPEECH'], 'Process dialogue speech corpus', 'RegressionTestingDialogue').
loop_command(['BATCH_DIALOGUE_SPEECH_AGAIN'], 'Process dialogue speech corpus, using recognition results from previous run', 'RegressionTestingDialogue').
loop_command(['BATCH_DIALOGUE_SPEECH', _Id], 'Process dialogue speech corpus with specified ID', 'RegressionTestingDialogue').
loop_command(['BATCH_DIALOGUE_SPEECH_AGAIN', _Id], 'Process dialogue speech corpus with specified ID, using recognition results from previous run', 'RegressionTestingDialogue').
loop_command(['UPDATE_DIALOGUE_JUDGEMENTS'], 'Update dialogue judgements file from annotated dialogue corpus output', 'RegressionTestingDialogue').
loop_command(['UPDATE_DIALOGUE_JUDGEMENTS', _Id], 'Update dialogue judgements file with specified ID from annotated dialogue corpus output', 'RegressionTestingDialogue').
loop_command(['UPDATE_DIALOGUE_JUDGEMENTS_SPEECH'], 'Update dialogue judgements file from annotated speech dialogue corpus output', 'RegressionTestingDialogue').
loop_command(['UPDATE_DIALOGUE_JUDGEMENTS_SPEECH', _Id], 'Update dialogue judgements file with specified ID from annotated speech dialogue corpus output', 'RegressionTestingDialogue').

%loop_command(['SLM'], 'Perform recognition using SLM recogniser', no_section).
%loop_command(['NO_SLM'], 'Perform translation using GLM recogniser (default)', no_section).

loop_command(['EBL_TREEBANK'], 'Parse all sentences in current EBL training set into treebank form', 'InvokingSpecialisation').
loop_command(['EBL_TRAIN'], 'Do EBL training on current treebank', 'InvokingSpecialisation').
loop_command(['EBL_POSTPROCESS'], 'Postprocess results of EBL training into specialised Regulus grammar', 'InvokingSpecialisation').
loop_command(['EBL_LOAD'], 'Load current specialised Regulus grammar in DCG and left-corner form', 'SpecialisedParsing').
loop_command(['EBL_LOAD_GENERATION'], 'Compile and load current specialised Regulus grammar for generation', 'SpecialisedGeneration').
loop_command(['EBL_LOAD_GENERATION', _Id], 'Compile and load designated version of current specialised Regulus grammar for generation', 'SpecialisedGeneration').
loop_command(['EBL_NUANCE'], 'Compile current specialised Regulus grammar into Nuance GSL form', 'SpecialisedRecognition').
loop_command(['EBL_GEMINI'], 'Compile current specialised Regulus grammar into Gemini form', 'Gemini').
loop_command(['EBL_GRAMMAR_PROBS'], 'Create Nuance grammar probs training set from current EBL training set or grammar_probs_data file', 'PCFG').
loop_command(['MAKE_TARGET_GRAMMAR_PROBS_CORPUS', _Grammar], 'Create Nuance grammar probs training set for given grammar from translation output', 'PCFG').
loop_command(['MAKE_TARGET_SENT_CORPUS'], 'Create sent-formatted corpus from translation output', 'PCFG').
loop_command(['EBL'], 'Do main EBL processing: equivalent to LOAD, EBL_TREEBANK, EBL_TRAIN, EBL_POSTPROCESS, EBL_NUANCE', no_section).
loop_command(['EBL_ANALYSIS'], 'Do main EBL processing, except for creation of Nuance grammar: equivalent to LOAD, EBL_TREEBANK, EBL_TRAIN, EBL_POSTPROCESS', 'InvokingSpecialisation').
loop_command(['EBL_GENERATION'], 'Do main generation EBL processing: equivalent to LOAD, EBL_TREEBANK, EBL_TRAIN, EBL_POSTPROCESS, EBL_LOAD_GENERATION', 'InvokingSpecialisation').
loop_command(['PARSE_HISTORY' | _Args], 'Show parse history for examples matching specified string', 'ParseHistory').

loop_command(['CHECK_ALTERF_PATTERNS'], 'Check the consistency of the current Alterf patterns file', 'LFPatterns').
loop_command(['RANDOM_GENERATE', _N], 'Randomly generate and print the specified number of sentences', 'RandomGenerationRegulus'). 
loop_command(['RANDOM_GENERATE', _N, _Depth], 'Randomly generate and print the specified number of sentences, with specified maximum depth', 'RandomGenerationRegulus').
loop_command(['STEPPER'], 'Start grammar stepper', 'GrammarDebuggingStepper').
loop_command(['COMPILE_HELP'], 'Compile material for targeted help', 'HelpSystemConstruction').
loop_command(['LOAD_HELP'], 'Load compiled material for targeted help', 'HelpSystemCommandLine').
loop_command(['LIST_MISSING_HELP_DECLARATIONS'], 'Write out a list of lexical items that are not listed in targeted help declarations', 'HelpSystemClasses').
loop_command(['HELP_RESPONSE_ON'], 'Switch on help response in main loop (default off)', 'HelpSystemCommandLine').
loop_command(['HELP_RESPONSE_OFF'], 'Switch off help response in main loop (default off)', 'HelpSystemCommandLine').
loop_command(['BATCH_HELP'], 'Process help corpus', 'HelpSystemCommandLine').
loop_command(['BATCH_HELP', _Id], 'Process help corpus with specified ID', 'HelpSystemCommandLine').
loop_command(['INCREMENTAL_TREEBANKING_ON'], 'Try to reuse old treebank material when possible (default on)', 'IncrementalTreebanking').
loop_command(['INCREMENTAL_TREEBANKING_OFF'], 'Don\'t try to reuse old treebank material (default on)', 'IncrementalTreebanking').
loop_command(['LF_POST_PROCESSING_ON'], 'Switch on semantic post-processing of LFs (default)', 'GrammarDebuggingCommandLine').
loop_command(['LF_POST_PROCESSING_OFF'], 'Switch off semantic post-processing of LFs', 'GrammarDebuggingCommandLine').
loop_command(['LOAD_RECOGNITION'], 'Load recognition resources: license manager, recserver, TTS and regserver', 'SpeechInput').
loop_command(['LOAD_RECOGNITION', _N], 'Load recognition resources: license manager, recserver, TTS and regserver, using specified port for Regserver', 'SpeechInput').
loop_command(['LOAD_REGSERVER'], 'Load Regserver, using specified port and assuming other resources are already loaded', 'SpeechInput').
loop_command(['LOAD_REGSERVER', _N], 'Load Regserver, using specified port and assuming other resources are already loaded', 'SpeechInput').
loop_command(['CLOSE_DOWN_RECOGNITION'], 'Close down recognition resources: license manager, recserver, TTS and regserver', 'SpeechInput').
loop_command(['RECOGNISE'], 'Take next loop input from live speech', 'SpeechInput').
loop_command(['SET_NBEST_N', _N], 'Set the maximum number of hypotheses used for N-best processing', 'DialogueNBest').
loop_command(['SET_REGSERVER_TIMEOUT', _N], 'Set the time the system waits before starting up the Regserver', 'SpeechInput').
loop_command(['NUANCE_COMPILE'], 'Compile Nuance grammar into recogniser package', 'GSLToNuance').
loop_command(['NUANCE_COMPILE_WITH_PCFG'], 'Compile Nuance grammar into recogniser package, first doing PCFG training', 'GSLToNuance').
loop_command(['WAVFILES'], 'Show wavfiles recorded from speech input at top-level', 'RecordedWavfiles').
loop_command(['WAVFILES', _N], 'Show most recent N wavfiles recorded from speech input at top-level', 'RecordedWavfiles').
loop_command(['CHECK_PARAPHRASES'], 'Check that transcription paraphrases are in coverage', 'TranslationRegressionParaphrases').
loop_command(['ALLOW_UNDEFINED_INTERLINGUA'], 'Allow interlingua elements in translation rules to be undefined in the interlingua', 'Translation').
loop_command(['DONT_ALLOW_UNDEFINED_INTERLINGUA'], 'Do not allow interlingua elements in translation rules to be undefined in the interlingua (default)', 'Translation').
loop_command(['COMPILE_LITE'], 'Compile Lite files', 'Lite').
loop_command(['COMPILE_CURRENT_LITE_NAMESPACE'], 'Compile Lite files (current namespace only)', 'Lite').

process_regulus_loop_command(['HELP']) :-
	print_all_loop_commands_with_help_text,
	!.
process_regulus_loop_command(['HELP', Atom]) :-
	print_all_loop_commands_with_matching_help_text(Atom),
	!.
process_regulus_loop_command(['HELP_CONFIG', Atom]) :-
	print_all_config_file_entries_with_matching_help_text(Atom),
	!.
process_regulus_loop_command(['DOC', Item]) :-
	print_doc_for_command_or_config_file_item(Item),
	!.
process_regulus_loop_command(['RELOAD_CFG']) :-
	(   get_regulus_runtime_config(current_cfg_file, File) ->
	    load_regulus_config_file(File)
	;
	    format2error('~N*** Error: no current config file defined, cannot reload~n', [])
	),
	!.
process_regulus_loop_command(['LOAD']) :-
	get_regulus_config_item(macro_expanded_grammar, MacroExpandedFile),
	get_regulus_config_item(regulus_grammar, GrammarFile),
	get_regulus_config_item(dcg_grammar, DCGFile),
	get_regulus_config_item(reflective_dcg_grammar, ReflectiveDCGFile),
	get_regulus_config_item(stanford_dcg_grammar, StanfordDCGFile),
	get_regulus_config_item(lc_tables_file, LCTablesFile),
	%trace,
	set_new_current_progress_file_for_command(['LOAD']),
	make_macro_expanded_file(GrammarFile, MacroExpandedFile),
	add_progress_line(macro_expanded_file),
	zero_cached_dynamic_lex_entries,
	make_and_load_dcg_and_lc(GrammarFile, [], normal,
				 DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile),
	process_regulus_loop_command(['LOAD_PREFERENCES']),
	!.
process_regulus_loop_command(['LOAD_DEBUG']) :-
	get_regulus_config_item(macro_expanded_grammar, MacroExpandedFile),
	get_regulus_config_item(regulus_grammar, GrammarFile),
	get_regulus_config_item(dcg_grammar, DCGFile),
	get_regulus_config_item(reflective_dcg_debug_grammar, ReflectiveDCGFile),
	get_regulus_config_item(stanford_dcg_debug_grammar, StanfordDCGFile),
	get_regulus_config_item(lc_debug_tables_file, LCTablesFile),
	set_new_current_progress_file_for_command(['LOAD']),
	make_macro_expanded_file(GrammarFile, MacroExpandedFile),
	add_progress_line(macro_expanded_file),
	zero_cached_dynamic_lex_entries,
	make_and_load_dcg_and_lc(GrammarFile, [], debug,
				 DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile),
	process_regulus_loop_command(['LOAD_PREFERENCES']),
	!.
process_regulus_loop_command(['LOAD_PREFERENCES']) :-
	load_parse_preferences_file_if_there_is_one,
	load_nbest_preferences_file_if_there_is_one,
	!.
process_regulus_loop_command(['LOAD_SURFACE_PATTERNS']) :-
	(   regulus_config(alterf_patterns_file, LFPatternsFile) ->
	    safe_compile(user, LFPatternsFile) ;
	    true
	),
	get_regulus_config_item(surface_patterns, PatternsFile),
	get_regulus_config_item(discriminants, DiscriminantsFile),
	get_regulus_config_item(tagging_grammar, TaggingGrammarFile),
	get_regulus_config_item(target_model, TargetModelFile),
	safe_compile(user, PatternsFile),
	safe_compile(user, DiscriminantsFile),
	safe_compile(user, TaggingGrammarFile),
	safe_compile(user, TargetModelFile),
	(   regulus_config(surface_postprocessing, PostprocessingFile) ->
	    safe_compile(user, PostprocessingFile) ;
	    true
	),
	(   regulus_config(surface_constituent_rules, SurfaceConstituentsFile) ->
	    get_regulus_config_item(compiled_surface_constituent_rules, CompiledSurfaceConstituentsFile),
	    compile_surface_constituents_file(SurfaceConstituentsFile, CompiledSurfaceConstituentsFile),
	    safe_compile(user, CompiledSurfaceConstituentsFile) ;
	    true
	),
	!.
process_regulus_loop_command(['TRIVIAL_PARSER']) :-
	set_regulus_runtime_config(parser, trivial),
	!.
process_regulus_loop_command(['DCG']) :-
	set_regulus_runtime_config(parser, dcg),
	!.
process_regulus_loop_command(['LC']) :-
	set_regulus_runtime_config(parser, lc),
	!.
process_regulus_loop_command(['NUANCE_PARSER']) :-
	start_nl_tool_process_as_described_by_config_file,
	set_regulus_runtime_config(parser, nuance),
	process_regulus_loop_command(['LOAD_PREFERENCES']),
	!.
process_regulus_loop_command(['KILL_NUANCE_PARSERS']) :-
	kill_nl_tool_process,
	!.
process_regulus_loop_command(['SURFACE']) :-
	set_regulus_runtime_config(parser, surface),
	!.
/*
process_regulus_loop_command(['NO_LC']) :-
	set_regulus_runtime_config(load_lc, no),
	!.
process_regulus_loop_command(['USE_LC']) :-
	set_regulus_runtime_config(load_lc, yes),
	!.
*/
process_regulus_loop_command(['NUANCE']) :-
	get_regulus_config_item(regulus_grammar, InFile),
	get_regulus_config_item(nuance_grammar, OutFile),
	(   regulus_config(nuance_grammar_tag, GrammarTag) ->
	    true
	;
	    GrammarTag = default
	),
	regulus2nuance_specifying_grammar_tag(InFile, GrammarTag, OutFile),
	!.
process_regulus_loop_command(['GEMINI']) :-
	get_regulus_config_item(regulus_grammar, InFile),
	get_regulus_config_item(gemini_grammar, OutFile),
	regulus2gemini(InFile, OutFile),
	!.
process_regulus_loop_command(['TRACE']) :-
	regulus_trace,
	!.
process_regulus_loop_command(['NOTRACE']) :-
	regulus_notrace,
	!.
process_regulus_loop_command(['COMPACTION']) :-
	set_regulus_switch(compaction, on),
	!.
process_regulus_loop_command(['NO_COMPACTION']) :-
	set_regulus_switch(compaction, off),
	!.
process_regulus_loop_command(['LOAD_GENERATION']) :-
	compile_and_load_generation_rules_file(unspecialised, default),
	!.
process_regulus_loop_command(['LOAD_RECOGNITION_GENERATION']) :-
	load_original_script_recognition_generation_rules_file_if_there_is_one,
	load_gloss_recognition_generation_rules_file_if_there_is_one,
	!.
process_regulus_loop_command(['LOAD_GENERATION', Arg]) :-
	compile_and_load_generation_rules_file(unspecialised, Arg),
	!.
process_regulus_loop_command(['EBL_LOAD_GENERATION']) :-
	compile_and_load_generation_rules_file(specialised, default),
	!.
process_regulus_loop_command(['EBL_LOAD_GENERATION', Arg]) :-
	compile_and_load_generation_rules_file(specialised, Arg),
	!.
process_regulus_loop_command(['NORMAL_PROCESSING']) :-
	set_regulus_runtime_config(processing_mode, normal),
	!.
process_regulus_loop_command(['TRANSLATE']) :-
	set_regulus_runtime_config(processing_mode, translate),
	!.
process_regulus_loop_command(['EBL_MODE']) :-
	set_regulus_runtime_config(processing_mode, ebl),
	!.
process_regulus_loop_command(['EBL_MODE_VERBOSE']) :-
	set_regulus_runtime_config(processing_mode, ebl_verbose),
	!.
process_regulus_loop_command(['DIALOGUE']) :-
	set_regulus_runtime_config(processing_mode, dialogue),
	warn_if_dialogue_resources_not_loaded,
	!.
process_regulus_loop_command(['GENERATION']) :-
	set_regulus_runtime_config(processing_mode, generation),
	!.
process_regulus_loop_command(['ECHO_ON']) :-
	set_regulus_runtime_config(echo_text, yes),
	!.
process_regulus_loop_command(['ECHO_OFF']) :-
	set_regulus_runtime_config(echo_text, no),
	!.
process_regulus_loop_command(['LF_POST_PROCESSING_ON']) :-
	switch_on_lf_post_processing,
	!.
process_regulus_loop_command(['LF_POST_PROCESSING_OFF']) :-
	switch_off_lf_post_processing,
	!.
process_regulus_loop_command(['LINE_INFO_ON']) :-
	set_regulus_runtime_config(print_line_info, print_line_info),
	!.
process_regulus_loop_command(['LINE_INFO_OFF']) :-
	set_regulus_runtime_config(print_line_info, dont_print_line_info),
	!.
process_regulus_loop_command(['PRINT_TREE_SUMMARY_ON']) :-
	set_regulus_runtime_config(print_tree_summary, print_tree_summary),
	!.
process_regulus_loop_command(['PRINT_TREE_SUMMARY_OFF']) :-
	set_regulus_runtime_config(print_tree_summary, dont_print_tree_summary),
	!.
process_regulus_loop_command(['PRINT_TREE_CATEGORIES_ON']) :-
	set_regulus_runtime_config(print_tree_categories, print_tree_categories),
	!.
process_regulus_loop_command(['PRINT_TREE_CATEGORIES_OFF']) :-
	set_regulus_runtime_config(print_tree_categories, dont_print_tree_categories),
	!.
process_regulus_loop_command(['FEAT', Feat]) :-
	(   grammar_is_loaded ->
	    print_info_for_feature(Feat)
	;
	    otherwise ->
	    format('~NNo grammar is currently loaded~n', [])
	),
	!.
process_regulus_loop_command(['CAT', Cat]) :-
	(   grammar_is_loaded ->
	    print_info_for_category(Cat)
	;
	    otherwise ->
	    format('~NNo grammar is currently loaded~n', [])
	),
	!.
process_regulus_loop_command(['INTERLINGUA']) :-
	set_regulus_runtime_config(translation_mode, interlingua),
	!.
process_regulus_loop_command(['NO_INTERLINGUA']) :-
	set_regulus_runtime_config(translation_mode, direct),
	!.
process_regulus_loop_command(['NO_ELLIPSIS_PROCESSING']) :-
	(   current_predicate(user:ellipsis_class_example/2) ->
	    
	    safe_abolish(user:ellipsis_class_example/2),
	    format('~NEllipsis class declarations unloaded. Use the LOAD_TRANSLATE command to reload them.~n', []) ;

	    format('~NNo ellipsis class declarations were loaded, so nothing to do.~n', [])
	),
	!.
%process_regulus_loop_command(['SLM']) :-
%	set_regulus_runtime_config(recogniser, slm),
%	!.
%process_regulus_loop_command(['NO_SLM']) :-
%	set_regulus_runtime_config(recogniser, glm),
%	!.
process_regulus_loop_command(['LOAD_TRANSLATE']) :-
	%(   regulus_config(generation_regulus_grammar, _RegulusFile) ->
	%    compile_and_load_generation_rules_file ;
	%    load_generation_rules_file
	%),
	set_new_current_progress_file_for_command(['LOAD_TRANSLATE']),
	 
	load_generation_rules_file,
	load_alternate_generation_rules_files_if_there_are_any,
	add_progress_line(generation),
	
	load_original_script_generation_rules_file_if_there_is_one,
	add_progress_line(original_script_generation),
	
	load_gloss_generation_rules_file_if_there_is_one,
	add_progress_line(gloss_generation),
	
	load_generation_preferences_file_if_there_is_one,
	load_alternate_generation_preferences_file_if_there_is_one,
	load_resolution_preferences_file_if_there_is_one,
	add_progress_line(generation_preferences),

	zero_interlingua_constant_used_table,
	add_progress_line(zero_interlingua_constant_used_table),
	
	load_transfer_rules_file_if_there_is_one,
	add_progress_line(transfer_rules),
	
	load_interlingua_declarations_file_if_there_is_one,
	add_progress_line(interlingua_declarations),
	
	load_interlingua_structure_grammar_file_if_there_is_one,
	load_alternate_interlingua_structure_grammar_file_if_there_are_any,
	add_progress_line(interlingua_structure_grammar),

	zero_transfer_resources_defined_for_module,
	
	load_to_source_discourse_rules_file_if_there_is_one,
	add_progress_line(to_source_discourse_rules),
	
	load_to_interlingua_rules_file_if_there_is_one,
	add_progress_line(to_interlingua_rules),
	
	load_from_interlingua_rules_file_if_there_is_one,
	add_progress_line(from_interlingua_rules),
	
	%create_filtered_interlingua_declarations_file_if_relevant,
	load_ellipsis_classes_file_if_there_is_one,
	add_progress_line(ellipsis_classes),
	
	load_collocation_rules_file_if_there_is_one,
	add_progress_line(collocation_rules),
	
	load_original_script_collocation_rules_file_if_there_is_one,
	add_progress_line(original_script_collocation_rules),

	load_interlingua_collocation_rules_files_if_there_are_any,
	
	load_orthography_rules_files_if_there_are_any,
	add_progress_line(orthography_rules),
	
	load_original_script_orthography_rules_file_if_there_is_one,
	add_progress_line(original_script_orthography_rules),

	load_recognition_orthography_rules_file_if_there_is_one,
	add_progress_line(recognition_orthography_rules),

	initialise_wavfiles_directory_if_there_is_one,
	add_progress_line(wavfiles_directory),
	!.
process_regulus_loop_command(['TRANSLATE_TRACE_ON']) :-
	switch_on_transfer_tracing,
	!.
process_regulus_loop_command(['TRANSLATE_TRACE_OFF']) :-
	switch_off_transfer_tracing,
	!.
process_regulus_loop_command(['INTERLINGUA_TRACE_ON']) :-
	switch_on_interlingua_tracing,
	!.
process_regulus_loop_command(['INTERLINGUA_TRACE_OFF']) :-
	switch_off_interlingua_tracing,
	!.
process_regulus_loop_command(['INTERLINGUA_DEBUGGING_ON']) :-
	switch_on_interlingua_structure_debugging,
	!.
process_regulus_loop_command(['INTERLINGUA_DEBUGGING_OFF']) :-
	switch_off_interlingua_structure_debugging,
	!.
process_regulus_loop_command(['GENERATE_TRACE_ON']) :-
	switch_on_generation_tracing,
	!.
process_regulus_loop_command(['GENERATE_TRACE_OFF']) :-
	switch_off_generation_tracing,
	!.
process_regulus_loop_command(['ALLOW_UNDEFINED_INTERLINGUA']) :-
	allow_undefined_interlingua_elements,
	!.
process_regulus_loop_command(['DONT_ALLOW_UNDEFINED_INTERLINGUA']) :-
	dont_allow_undefined_interlingua_elements,
	!.
process_regulus_loop_command(['ANSWER_ELLIPSIS_ON']) :-
	switch_on_answer_resolution,
	!.
process_regulus_loop_command(['ANSWER_ELLIPSIS_OFF']) :-
	switch_off_answer_resolution,
	!.
process_regulus_loop_command(['BIDIRECTIONAL_ON']) :-
	start_bidirectional_mode,
	!.
process_regulus_loop_command(['BIDIRECTIONAL_OFF']) :-
	exit_bidirectional_mode,
	!.
process_regulus_loop_command(['LOAD_DIALOGUE']) :-
	load_dialogue_files,
	!.
process_regulus_loop_command(['INIT_DIALOGUE']) :-
	initialise_dialogue_state,
	get_current_dialogue_state(OutState),
	(   current_predicate(PrintPackage:print_dialogue_state/1) ->
		
	    format('~N~n      New state: ~n', []),
	    PrintPackage:print_dialogue_state(OutState) ;
		
	    format('~N~n      New state: ~w', [OutState])
	    ),
	!.
process_regulus_loop_command(['INIT_DIALOGUE', Arg]) :-
	initialise_dialogue_state(Arg),
	get_current_dialogue_state(OutState),
	(   current_predicate(PrintPackage:print_dialogue_state/1) ->
		
	    format('~N~n      New state: ~n', []),
	    PrintPackage:print_dialogue_state(OutState) ;
		
	    format('~N~n      New state: ~w', [OutState])
	    ),
	!.
process_regulus_loop_command(['REINIT_DIALOGUE']) :-
	(   regulus_config(init_dialogue_state_parameter, Arg) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to reinitialize dialogue state, init_dialogue_state_parameter not set.~n', []),
	    fail
	),
	(   Arg = '*null*' ->
	    process_regulus_loop_command(['INIT_DIALOGUE'])
	;
	    otherwise ->
	    process_regulus_loop_command(['INIT_DIALOGUE', Arg])
	),
	!.
process_regulus_loop_command(['REINIT_DIALOGUE_KEEPING_STATE']) :-
	get_current_dialogue_state(DialogueState),
	process_regulus_loop_command(['REINIT_DIALOGUE']),
	set_current_dialogue_state(DialogueState),
	!.
process_regulus_loop_command(['SET_NOTIONAL_TIME', Time]) :-
	atom_codes(Time, TimeChars),
	(   parse_datime(TimeChars, Datime) ->
	    
	    set_notional_time(Datime) ;
	    
	    format('~NUnable to parse time "~w". Format = YYYY-MM-DD_HH-MM-SS, e.g. 2006-12-31_23-59-59~n', [Time]),
	    fail
	),
	!.
process_regulus_loop_command(['UNSET_NOTIONAL_TIME']) :-
	unset_notional_time,
	!.
process_regulus_loop_command(['DIALOGUE_TIME']) :-
	(   get_notional_time(Datime) ->
	    true ;
	    datime(Datime)
	),
	datime_to_timestamp(Datime, FormattedDatime),
	format('~N~nCurrent dialogue time: ~w~n', [FormattedDatime]),
	!.
process_regulus_loop_command(['SET_NOTIONAL_SPEAKER', Name]) :-
	(   atom(Name) ->
	    
	    set_notional_speaker(Name) ;
	    
	    format('~NUnable to set speaker "~w" - needs to be an atom~n', [Name]),
	    fail
	),
	!.
process_regulus_loop_command(['SET_BATCH_DIALOGUE_FORMAT', Format]) :-
	(   atom(Format) ->
	    
	    set_batch_dialogue_printing_format(Format)
	;
	    otherwise ->
	    format('~NUnable to set batch dialogue format "~w" - needs to be an atom~n', [Format]),
	    fail
	),
	!.
process_regulus_loop_command(['UNSET_NOTIONAL_SPEAKER']) :-
	unset_notional_speaker,
	!.
process_regulus_loop_command(['DIALOGUE_SPEAKER']) :-
	(   get_notional_speaker(Name) ->
	    format('~N~nCurrent dialogue speaker: ~w~n', [Name]) ;
	    format('~N~nNo current dialogue speaker set.~w~n', [])
	),
	!.
process_regulus_loop_command(['DUMP_NBEST_TRAINING_DATA_ON']) :-
	print_nbest_training_data,
	!.
process_regulus_loop_command(['DUMP_NBEST_TRAINING_DATA_OFF']) :-
	dont_print_nbest_training_data,
	!.
process_regulus_loop_command(['SET_NBEST_N', NAtom]) :-
	(   atom_to_int(NAtom, N) ->
	    set_nbest_n(N),
	    (   N = 1 ->
		format('~N--- Using only top hypothesis in N-best processing~n', [])
	    ;
		otherwise ->
		format('~N--- Using at most ~d hypotheses in N-best processing~n', [N])
	    )
	;
	    otherwise ->
	    format2error('~N*** Error: bad argument "~w" to SET_NBEST_N. Must be an integer.~n', [NAtom]),
	    fail
	).
process_regulus_loop_command(['SET_REGSERVER_TIMEOUT', NAtom]) :-
	(   atom_to_int(NAtom, N) ->
	    set_wait_time_for_regserver(N),
	    format('~N--- Will wait ~d seconds before starting Regserver from top level~n', [N])
	;
	    otherwise ->
	    format2error('~N*** Error: bad argument "~w" to SET_REGSERVER_TIMEOUT. Must be an integer.~n', [NAtom]),
	    fail
	).
process_regulus_loop_command(['NUANCE_COMPILE']) :-
	do_nuance_compile_as_defined_by_config_file(no_pcfg),
	!.
process_regulus_loop_command(['NUANCE_COMPILE_WITH_PCFG']) :-
	do_pcfg_training_as_defined_by_config_file,
	do_nuance_compile_as_defined_by_config_file(pcfg),
	!.
process_regulus_loop_command(['WAVFILES']) :- 
	show_recorded_wavfiles(1000000),
	!.
process_regulus_loop_command(['WAVFILES', N0]) :-
	coerce_atom_to_int(N0, N),
	(   integer(N) ->
	    show_recorded_wavfiles(N)
	;
	    format2error('~N*** Error: argument to WAVFILES needs to be a positive integer~n', []),
	    fail
	),
	!.
process_regulus_loop_command(['BATCH_DIALOGUE']) :-
	get_regulus_config_item(dialogue_corpus, CorpusFileOrFiles),
	get_regulus_config_item(dialogue_corpus_results, ResultsFile),
	
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
		
	(   regulus_config(dialogue_corpus_judgements, JudgementsFile) ->
	    load_dialogue_corpus_judgements_file(JudgementsFile) ;
	    true
	),
	dialogue_process_file(CorpusFileOrFiles, AbsResultsFile),
	format('~NResults written to ~w~n', [AbsResultsFile]),
	!.
process_regulus_loop_command(['BATCH_DIALOGUE', Id]) :-
	get_regulus_config_item(dialogue_corpus(Id), CorpusFileOrFiles),
	get_regulus_config_item(dialogue_corpus_results(Id), ResultsFile),
	
	%safe_absolute_file_name(CorpusFile, AbsCorpusFile),
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
		
	(   regulus_config(dialogue_corpus_judgements, JudgementsFile) ->
	    load_dialogue_corpus_judgements_file(JudgementsFile) ;
	    true
	),
	dialogue_process_file(CorpusFileOrFiles, AbsResultsFile),
	format('~NResults written to ~w~n', [AbsResultsFile]),
	!.
process_regulus_loop_command(['BATCH_DIALOGUE_SPEECH']) :-
	execute_dialogue_speech_corpus_command(new_recognition_results, default),
	!. 
process_regulus_loop_command(['BATCH_DIALOGUE_SPEECH', Id]) :-
	execute_dialogue_speech_corpus_command(new_recognition_results, Id),
	!.
process_regulus_loop_command(['BATCH_DIALOGUE_SPEECH_AGAIN']) :-
	execute_dialogue_speech_corpus_command(stored_recognition_results, default),
	!. 
process_regulus_loop_command(['BATCH_DIALOGUE_SPEECH_AGAIN', Id]) :-
	execute_dialogue_speech_corpus_command(stored_recognition_results, Id),
	!.
process_regulus_loop_command(['TRANSLATE_CORPUS']) :-
	get_regulus_config_item(translation_corpus, CorpusFile),
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	load_all_translation_corpus_judgement_files,
	get_regulus_runtime_config(translation_mode, TranslationMode),
	set_new_current_progress_file_for_command(['TRANSLATE_CORPUS']),
	parse_transfer_and_generate_file(CorpusFile, prolog, ResultsFile, TranslationMode),
	safe_absolute_file_name(ResultsFile, ResultsFile1),
	print_timing_summary_for_translation_result_file(ResultsFile1),
	format('~NResults written to ~w~n', [ResultsFile1]),
	%create_translation_rule_learning_files(ResultsFile1),
	!.
process_regulus_loop_command(['TRANSLATE_CORPUS', Id]) :-
	get_regulus_config_item(translation_corpus(Id), CorpusFile),
	get_regulus_config_item(translation_corpus_results(Id), ResultsFile),
	load_all_translation_corpus_judgement_files,
	get_regulus_runtime_config(translation_mode, TranslationMode),
	parse_transfer_and_generate_file(CorpusFile, prolog, ResultsFile, TranslationMode),
	safe_absolute_file_name(ResultsFile, ResultsFile1),
	print_timing_summary_for_translation_result_file(ResultsFile1),
	format('~NResults written to ~w~n', [ResultsFile1]),
	%create_translation_rule_learning_files(ResultsFile1),
	!.
process_regulus_loop_command(['CHECK_BACKTRANSLATION', File]) :-
	check_backtranslation_on_corpus_output(File), 
	!.
process_regulus_loop_command(['STORE_TRANSLATION_TARGET_VOCAB', File]) :-
	store_target_vocabulary_for_corpus_output(File),
	!.
process_regulus_loop_command(['TRANSLATE_PARSE_TIMES']) :-
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	print_parse_time_report_for_translation_result_file(ResultsFile),
	!.
process_regulus_loop_command(['TRANSLATE_PARSE_TIMES', Id]) :-
	get_regulus_config_item(translation_corpus_results(Id), ResultsFile),
	print_parse_time_report_for_translation_result_file(ResultsFile),
	!.
process_regulus_loop_command(['COMPILE_ELLIPSIS_PATTERNS']) :-
	compile_ellipsis_classes_file,
	!.
process_regulus_loop_command(['TRANSLATE_SPEECH_CORPUS']) :-
	execute_translate_speech_corpus_command(new_recognition_results, default, ['TRANSLATE_SPEECH_CORPUS']),
	!.
process_regulus_loop_command(['TRANSLATE_SPEECH_CORPUS', Id]) :-
	execute_translate_speech_corpus_command(new_recognition_results, Id, ['TRANSLATE_SPEECH_CORPUS', Id]),
	!.
process_regulus_loop_command(['TRANSLATE_SPEECH_CORPUS_AGAIN']) :-
	execute_translate_speech_corpus_command(stored_recognition_results, default, ['TRANSLATE_SPEECH_CORPUS_AGAIN']),
	!.
process_regulus_loop_command(['TRANSLATE_SPEECH_CORPUS_AGAIN', Id]) :-
	execute_translate_speech_corpus_command(stored_recognition_results, Id, ['TRANSLATE_SPEECH_CORPUS_AGAIN', Id]),
	!.
process_regulus_loop_command(['UPDATE_RECOGNITION_JUDGEMENTS']) :-
	get_regulus_config_item(translation_corpus_tmp_recognition_judgements, TmpRecognitionJudgementsFile),
	get_regulus_config_item(translation_corpus_recognition_judgements, RecognitionJudgementsFile),

	safe_absolute_file_name(RecognitionJudgementsFile, AbsRecognitionJudgementsFile),
	safe_absolute_file_name(TmpRecognitionJudgementsFile, AbsTmpRecognitionJudgementsFile),
	update_recognition_judgements(AbsTmpRecognitionJudgementsFile, AbsRecognitionJudgementsFile),

	format('~NRecognition judgements file ~w updated from results file ~w~n', [AbsRecognitionJudgementsFile, AbsTmpRecognitionJudgementsFile]),
	!.
process_regulus_loop_command(['UPDATE_RECOGNITION_JUDGEMENTS', Id]) :-
	get_regulus_config_item(translation_corpus_tmp_recognition_judgements(Id), TmpRecognitionJudgementsFile),
	get_regulus_config_item(translation_corpus_recognition_judgements, RecognitionJudgementsFile),

	safe_absolute_file_name(RecognitionJudgementsFile, AbsRecognitionJudgementsFile),
	safe_absolute_file_name(TmpRecognitionJudgementsFile, AbsTmpRecognitionJudgementsFile),
	update_recognition_judgements(AbsTmpRecognitionJudgementsFile, AbsRecognitionJudgementsFile),

	format('~NRecognition judgements file ~w updated from results file ~w~n', [AbsRecognitionJudgementsFile, AbsTmpRecognitionJudgementsFile]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_CSV']) :-
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	change_extension_in_file(ResultsFile, csv, ResultsFileCSV),
	make_translation_file_from_csv_version(ResultsFileCSV),
	process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS']),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS']) :-
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	get_regulus_config_item(translation_corpus_judgements, JudgementsFile),
	load_translation_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_translation_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_translation_corpus_judgements_file(JudgementsFile),

	format('~NTranslation judgements file ~w updated from results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_DIALOGUE_JUDGEMENTS']) :-
	get_regulus_config_item(dialogue_corpus_results, ResultsFile),
	get_regulus_config_item(dialogue_corpus_judgements, JudgementsFile),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_dialogue_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	format('~NDialogue judgements file ~w updated from results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_DIALOGUE_JUDGEMENTS', Id]) :-
	get_regulus_config_item(dialogue_corpus_results(Id), ResultsFile),
	get_regulus_config_item(dialogue_corpus_judgements, JudgementsFile),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_dialogue_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	format('~NDialogue judgements file ~w updated from results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_DIALOGUE_JUDGEMENTS_SPEECH']) :-
	get_regulus_config_item(dialogue_speech_corpus_results, ResultsFile),
	get_regulus_config_item(dialogue_corpus_judgements, JudgementsFile),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_dialogue_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	format('~NDialogue judgements file ~w updated from speech results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_DIALOGUE_JUDGEMENTS_SPEECH', Id]) :-
	get_regulus_config_item(dialogue_speech_corpus_results(Id), ResultsFile),
	get_regulus_config_item(dialogue_corpus_judgements, JudgementsFile),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_dialogue_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_dialogue_corpus_judgements_file(JudgementsFile),

	format('~NDialogue judgements file ~w updated from speech results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_CSV', Id]) :-
	get_regulus_config_item(translation_corpus_results(Id), ResultsFile),
	change_extension_in_file(ResultsFile, csv, ResultsFileCSV),
	make_translation_file_from_csv_version(ResultsFileCSV),
	process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS', Id]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS', Id]) :-
	get_regulus_config_item(translation_corpus_results(Id), ResultsFile),
	get_regulus_config_item(translation_corpus_judgements, JudgementsFile),
	load_translation_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_translation_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_translation_corpus_judgements_file(AbsJudgementsFile),

	format('~NTranslation judgements file ~w updated from results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH_CSV']) :-
	get_regulus_config_item(translation_speech_corpus_results, ResultsFile),
	change_extension_in_file(ResultsFile, csv, ResultsFileCSV),
	make_translation_file_from_csv_version(ResultsFileCSV),
	process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH']),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH']) :-
	get_regulus_config_item(translation_speech_corpus_results, ResultsFile),
	get_regulus_config_item(translation_corpus_judgements, JudgementsFile),
	load_translation_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_translation_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_translation_corpus_judgements_file(JudgementsFile),

	format('~NTranslation judgements file ~w updated from speech results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH_CSV', Id]) :-
	get_regulus_config_item(translation_speech_corpus_results(Id), ResultsFile),
	change_extension_in_file(ResultsFile, csv, ResultsFileCSV),
	make_translation_file_from_csv_version(ResultsFileCSV),
	process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH', Id]),
	!.
process_regulus_loop_command(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH', Id]) :-
	get_regulus_config_item(translation_speech_corpus_results(Id), ResultsFile),
	get_regulus_config_item(translation_corpus_judgements, JudgementsFile),
	load_translation_corpus_judgements_file(JudgementsFile),

	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	update_translation_judgements(AbsResultsFile, AbsJudgementsFile, NNew, NChanged),
	load_translation_corpus_judgements_file(JudgementsFile),

	format('~NTranslation judgements file ~w updated from speech results file ~w~n', [AbsJudgementsFile, AbsResultsFile]),
	format('~N~d new judgements, ~d changed judgements~n', [NNew, NChanged]),
	!.
process_regulus_loop_command(['SPLIT_SPEECH_CORPUS', training_corpus, Id0, Id1, Id2]) :-
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id0), MainTranscriptionsFile) ->
	    true
	;
	    format2error('~N*** Error: need to define config item for "~w" or "~w"~n',
			 [translation_speech_corpus(Id0), dialogue_speech_corpus(Id0)]),
	    fail
	),
	(   get_regulus_config_item(ebl_corpus, CorpusFile) ->
	    true
	;
	    format2error('~N*** Error: need to define config item for "~w"~n', [ebl_corpus]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id1), TrainingTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id1)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id1)]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id2), NonTrainingTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id2)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id2)]),
	    fail
	),
	split_speech_corpus_with_respect_to_training_corpus(MainTranscriptionsFile,
							    CorpusFile,
							    TrainingTranscriptionsFile,
							    NonTrainingTranscriptionsFile),
	!.
process_regulus_loop_command(['SPLIT_SPEECH_CORPUS', GrammarAtom, Id1, Id2]) :-
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type), MainTranscriptionsFile) ->
	    true
	;
	    format2error('~N*** Error: need to define config item for "~w" or "~w"~n',
			 [translation_speech_corpus, dialogue_speech_corpus]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id1), InCoverageTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id1)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id1)]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id2), OutOfCoverageTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id2)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id2)]),
	    fail
	),
	split_speech_corpus_with_respect_to_current_grammar(MainTranscriptionsFile,
							    GrammarAtom,
							    InCoverageTranscriptionsFile,
							    OutOfCoverageTranscriptionsFile),
	(   regulus_config(paraphrase_corpus, ParaphraseFile) ->
	    update_paraphrase_file_from_out_of_coverage_file(OutOfCoverageTranscriptionsFile, ParaphraseFile)
	;
	    true
	),
	!.
process_regulus_loop_command(['SPLIT_SPEECH_CORPUS', GrammarAtom, Id0, Id1, Id2]) :-
	Id0 \== training_corpus,
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id0), MainTranscriptionsFile) ->
	    true
	;
	    format2error('~N*** Error: need to define config item for "~w" or "~w"~n',
			 [translation_speech_corpus(Id0), dialogue_speech_corpus(Id0)]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id1), InCoverageTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id1)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id1)]),
	    fail
	),
	(   get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id2), OutOfCoverageTranscriptionsFile) ->
	    true
	;
	    Type = translation ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [translation_speech_corpus(Id2)]),
	    fail
	;
	    Type = dialogue ->
	    format2error('~N*** Error: need to define config item for "~w"~n', [dialogue_speech_corpus(Id2)]),
	    fail
	),
	split_speech_corpus_with_respect_to_current_grammar(MainTranscriptionsFile,
							    GrammarAtom,
							    InCoverageTranscriptionsFile,
							    OutOfCoverageTranscriptionsFile),
	(   regulus_config(paraphrase_corpus, ParaphraseFile) ->
	    update_paraphrase_file_from_out_of_coverage_file(OutOfCoverageTranscriptionsFile, ParaphraseFile)
	;
	    true
	),
	!.
process_regulus_loop_command(['CHECK_PARAPHRASES']) :-
	get_regulus_config_item(paraphrase_corpus, ParaphraseFile),
	check_paraphrase_file(ParaphraseFile),
	!.
%process_regulus_loop_command(['LEARN_TRANSFER_RULES', Type]) :-
%	get_regulus_config_item(translation_corpus_results, ResultsFile),
%	execute_learn_transfer_rules_command(ResultsFile, Type),
%	!.		
%process_regulus_loop_command(['LEARN_TRANSFER_RULES', Type, Id]) :-
%	get_regulus_config_item(translation_corpus_results(Id), ResultsFile),
%	execute_learn_transfer_rules_command(ResultsFile, Type),
%	!.	
process_regulus_loop_command(['EBL_GRAMMAR_PROBS']) :-
	(   regulus_config(grammar_probs_data, CorpusFileOrFiles) ->
	    format('~N-- Taking data from grammar_probs_data config item~n', [])
	;
	    otherwise ->
	    get_regulus_config_item(ebl_corpus, CorpusFileOrFiles),
	    format('~N-- Taking data from ebl_corpus config item~n', [])
	),
	get_regulus_config_item(top_level_cat, TopLevelCat),
	get_regulus_config_item(ebl_grammar_probs, GrammarProbsFile),
	sents_file_or_files_to_grammar_probs_data_file(CorpusFileOrFiles, TopLevelCat, GrammarProbsFile),
	!.
process_regulus_loop_command(['MAKE_TARGET_GRAMMAR_PROBS_CORPUS', NuanceGrammar]) :-
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	get_regulus_config_item(target_grammar_probs, GrammarProbsFile),
	translation_corpus_results_to_grammar_probs_data_file(ResultsFile, NuanceGrammar, GrammarProbsFile),
	!.
process_regulus_loop_command(['MAKE_TARGET_SENT_CORPUS']) :-
	get_regulus_config_item(translation_corpus_results, ResultsFile),
	get_regulus_config_item(target_sent_corpus, SentCorpusFile),
	translation_corpus_results_to_sent_corpus_file(ResultsFile, SentCorpusFile),
	!.
process_regulus_loop_command(['PARSE_HISTORY' | Args]) :-
	get_regulus_config_item(parsing_history_file, ParsingHistoryFile),
	show_parse_history_for_string(Args, ParsingHistoryFile),
	!.
process_regulus_loop_command(['EBL_TREEBANK']) :-
	get_regulus_config_item(ebl_corpus, CorpusFile),
	get_regulus_config_item(top_level_cat, TopLevelCat),
	get_regulus_config_item(ebl_treebank, TreeFile),
	get_regulus_config_item(ebl_old_treebank, OldTreeFile),
	get_regulus_config_item(parsing_history_file, ParsingHistoryFile),
	get_ignored_subdomains(IgnoredSubdomains),
	make_ebl_training_data(CorpusFile, TopLevelCat, IgnoredSubdomains, ParsingHistoryFile, TreeFile, OldTreeFile),
	save_reflective_dcg_grammar_for_treebank,
	save_parse_preferences_for_treebank,
	!.
process_regulus_loop_command(['EBL_TRAIN']) :-
	get_regulus_config_item(ebl_treebank, TreeFile),
	get_regulus_config_item(ebl_operationality, Operationality0),
	get_regulus_config_item(ebl_created_sent_data, EBLCreatedSentDataFile),
	(   ( Operationality0 = file(File0), ground(File0) ) ->

	    safe_absolute_file_name(File0, AbsFile0),
	    format('~N~nTaking operationality criteria from ~w~n~n', [AbsFile0]),
	    create_ebl_operationality_file(AbsFile0, File),
	    Operationality = file(File) ;

	    format('~N~nUsing operationality criteria "~w" from $REGULUS/Prolog/ebl_operational.pl~n', [Operationality0]),
	    Operationality = Operationality0
	),
	get_regulus_config_item(ebl_raw_regulus_grammar, RawRegulusFile),
	(   regulus_config(ebl_include_lex, EBLIncludeLexFiles) ->
	    true ;
	    EBLIncludeLexFiles = []
	),
	get_ignored_subdomains(IgnoredSubdomains),
	ebl_train(TreeFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, RawRegulusFile),
	write_stored_ebl_created_sent_data_to_file(EBLCreatedSentDataFile),
	!. 
process_regulus_loop_command(['EBL_POSTPROCESS']) :-
	regulus_config_defined(working_file_prefix),
	get_regulus_config_item(ebl_raw_regulus_grammar, RawRegulusFile),
	get_regulus_config_item(ebl_regulus_grammar, EBLRegulusFile),
	get_regulus_config_item(ebl_regulus_no_binarise_grammar, EBLRegulusNoBinariseFile),
	get_regulus_config_item(ebl_rationalised_corpus, EBLRationalisedCorpusFile),
	get_regulus_config_item(ebl_multiple_grammar_decls, EBLMultipleGrammarDeclsFile),
	(   regulus_config(ebl_context_use_threshold, ContextUseThreshold) ->
	    true ;
	    ContextUseThreshold = 100000
	),
	(   regulus_config(ebl_filter_pred, FilterPred) ->
	    true ;
	    FilterPred = null
	),
	ebl_postprocess(RawRegulusFile, EBLMultipleGrammarDeclsFile,
			EBLRegulusFile, EBLRegulusNoBinariseFile, EBLRationalisedCorpusFile,
			FilterPred, ContextUseThreshold),
	!. 
process_regulus_loop_command(['EBL_LOAD']) :-
	(   regulus_config(ebl_regulus_component_grammar, GrammarFileOrFiles) ->
	    (   GrammarFileOrFiles = [GrammarFile] ->
		true
	    ;
		GrammarFile = GrammarFileOrFiles
	    )
	;
	    get_regulus_config_item(ebl_regulus_grammar, GrammarFile0),
	    add_tag_to_files_in_list([GrammarFile0], default, [GrammarFile])
	),
	
	% Need unique names for each subdomain, since we are caching compiled files
	%get_regulus_config_item(ebl_dcg_grammar, DCGFile),
	%get_regulus_config_item(ebl_reflective_dcg_grammar, ReflectiveDCGFile),
	%get_regulus_config_item(ebl_stanford_dcg_grammar, StanfordDCGFile),
	%get_regulus_config_item(ebl_lc_tables_file, LCTablesFile),

	safe_absolute_file_name(GrammarFile, AbsGrammarFile),
	directory_and_file_for_pathname(AbsGrammarFile, _Dir, BaseWithExtension),
	split_off_extension_from_pathname(BaseWithExtension, Base, _Extension),
	% Also need to make sure that the names are parameterised by working_file_prefix
	% in order to minimize risk of file collisions when we have multiple Regulus processes
	(   regulus_config(working_file_prefix, Prefix) ->
	    safe_absolute_file_name(Prefix, AbsPrefix)
	;
	    format2error('~NError: regulus_config declaration for working_file_prefix missing~n', []),
	    fail
	),
	format_to_atom('~w_~w_dcg.pl', [AbsPrefix, Base], DCGFile),
	format_to_atom('~w_~w_reflective_dcg.pl', [AbsPrefix, Base], ReflectiveDCGFile),
	format_to_atom('~w_~w_stanford_dcg.pl', [AbsPrefix, Base], StanfordDCGFile),
	format_to_atom('~w_~w_lc_tables.pl', [AbsPrefix, Base], LCTablesFile),

	make_ebl_ignore_feats_file(EBLIgnoreFeatsFile),
	set_new_current_progress_file_for_command(['EBL_LOAD']),
	make_and_load_dcg_and_lc(GrammarFile, [EBLIgnoreFeatsFile], normal,
				 DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile),
	process_regulus_loop_command(['LOAD_PREFERENCES']),
	!.
process_regulus_loop_command(['EBL_NUANCE']) :-
	get_regulus_config_item(ebl_regulus_grammar, InFile),
	get_regulus_config_item(ebl_nuance_grammar, OutFile),
	get_regulus_config_item(ebl_multiple_grammar_decls, MultipleGrammarDeclsFile),
	get_multiple_grammar_info(MultipleGrammarDeclsFile, Tags),
	make_ebl_ignore_feats_file(EBLIgnoreFeatsFile),
	regulus2nuance_for_multiple_grammars(InFile, EBLIgnoreFeatsFile, Tags, OutFile),
	!.
process_regulus_loop_command(['EBL_GEMINI']) :-
	get_regulus_config_item(ebl_regulus_grammar, InFile),
	get_regulus_config_item(ebl_gemini_grammar, OutFile),
	get_regulus_config_item(ebl_multiple_grammar_decls, MultipleGrammarDeclsFile),
	get_multiple_grammar_info(MultipleGrammarDeclsFile, Tags),
	make_ebl_ignore_feats_file(EBLIgnoreFeatsFile),
	regulus2gemini_for_multiple_grammars(InFile, EBLIgnoreFeatsFile, Tags, OutFile),
	!.
process_regulus_loop_command(['EBL']) :-
	process_regulus_loop_command(['LOAD']),
	process_regulus_loop_command(['EBL_TREEBANK']),
	process_regulus_loop_command(['EBL_TRAIN']),
	process_regulus_loop_command(['EBL_POSTPROCESS']),
	process_regulus_loop_command(['EBL_NUANCE']),
	!.
process_regulus_loop_command(['EBL_ANALYSIS']) :-
	process_regulus_loop_command(['LOAD']),
	process_regulus_loop_command(['EBL_TREEBANK']),
	process_regulus_loop_command(['EBL_TRAIN']),
	process_regulus_loop_command(['EBL_POSTPROCESS']),
	!.
process_regulus_loop_command(['EBL_GENERATION']) :-
	process_regulus_loop_command(['LOAD']),
	process_regulus_loop_command(['EBL_TREEBANK']),
	process_regulus_loop_command(['EBL_TRAIN']),
	process_regulus_loop_command(['EBL_POSTPROCESS']),
	process_regulus_loop_command(['EBL_LOAD_GENERATION']),
	!.
process_regulus_loop_command(['CHECK_ALTERF_PATTERNS']) :-
	get_regulus_config_item(alterf_patterns_file, PatternsFile),
	get_regulus_config_item(top_level_cat, TopLevelCat),
	get_regulus_config_item(alterf_sents_file, SentsFile),
	get_regulus_config_item(alterf_treebank_file, TreebankFile),
	check_alterf_patterns(PatternsFile, TopLevelCat, SentsFile, TreebankFile),
	!.
process_regulus_loop_command(['RANDOM_GENERATE', NAtom]) :-
	atom_to_int(NAtom, N),
	random_generate_and_print_atom_list(N, user, 100),
	!.
process_regulus_loop_command(['RANDOM_GENERATE', NAtom, DepthAtom]) :-
	atom_to_int(NAtom, N),
	atom_to_int(DepthAtom, Depth),
	random_generate_and_print_atom_list(N, user, Depth),
	!.
process_regulus_loop_command(['STEPPER']) :-
	stepper_loop.
process_regulus_loop_command(['COMPILE_HELP']) :-
	get_regulus_config_item(targeted_help_source_files, SourceFiles),
	get_regulus_config_item(targeted_help_classes_file, HelpClassesFile),

	get_regulus_config_item(targeted_help_corpus_file, CorpusFile),
	get_regulus_config_item(targeted_help_backed_off_corpus_file, BackedOffCorpusFile),
	get_regulus_config_item(compiled_targeted_help_classes_file, CompiledHelpClassesFile),

	compile_help_resources(SourceFiles, HelpClassesFile, CorpusFile, BackedOffCorpusFile, CompiledHelpClassesFile),
	!.
process_regulus_loop_command(['COMPILE_HELP']) :-
	remove_help_resources,
	format('~N*** Removed all compiled help resources.~n', []).
process_regulus_loop_command(['LOAD_HELP']) :-
	get_regulus_config_item(targeted_help_backed_off_corpus_file, BackedOffCorpusFile),
	get_regulus_config_item(compiled_targeted_help_classes_file, CompiledHelpClassesFile),

	load_help_resources(BackedOffCorpusFile, CompiledHelpClassesFile),
	!.
process_regulus_loop_command(['LOAD_HELP']) :-
	remove_help_resources,
	format('~N*** Removed all compiled help resources.~n', []),
	!.
process_regulus_loop_command(['LIST_MISSING_HELP_DECLARATIONS']) :-
	get_regulus_config_item(missing_help_class_decls, File),
	list_missing_help_declarations(File),
	!.
process_regulus_loop_command(['HELP_RESPONSE_ON']) :-
	switch_on_help_response,
	!.
process_regulus_loop_command(['HELP_RESPONSE_OFF']) :-
	switch_off_help_response,
	!.
process_regulus_loop_command(['BATCH_HELP']) :-
	get_regulus_config_item(help_corpus, CorpusFile),
	get_regulus_config_item(help_corpus_results, ResultsFile),
	
	safe_absolute_file_name(CorpusFile, AbsCorpusFile),
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
		
	(   regulus_config(help_corpus_judgements, JudgementsFile) ->
	    load_help_corpus_judgements_file(JudgementsFile)
	;
	    true
	),
	help_process_file(AbsCorpusFile, AbsResultsFile),
	format('~NResults written to ~w~n', [AbsResultsFile]),
	!.
process_regulus_loop_command(['BATCH_HELP', Id]) :-
	get_regulus_config_item(help_corpus(Id), CorpusFile),
	get_regulus_config_item(help_corpus_results(Id), ResultsFile),
	
	safe_absolute_file_name(CorpusFile, AbsCorpusFile),
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
		
	(   regulus_config(help_corpus_judgements, JudgementsFile) ->
	    load_help_corpus_judgements_file(JudgementsFile)
	;
	    true
	),
	help_process_file(AbsCorpusFile, AbsResultsFile),
	format('~NResults written to ~w~n', [AbsResultsFile]),
	!.
process_regulus_loop_command(['INCREMENTAL_TREEBANKING_ON']) :-
	switch_on_incremental_treebanking,
	!.
process_regulus_loop_command(['INCREMENTAL_TREEBANKING_OFF']) :-
	switch_off_incremental_treebanking,
	!.
process_regulus_loop_command(['LOAD_RECOGNITION']) :-
	start_recognition_resources,
	!.
process_regulus_loop_command(['LOAD_RECOGNITION', RegserverPort0]) :-
	(   coerce_atom_to_int(RegserverPort0, RegserverPort) ->
	    true
	;
	    format2error('~N*** Error: Regserver port "~w" must be an integer~n', [RegserverPort0]),
	    fail
	),
	start_recognition_resources(RegserverPort),
	!.
process_regulus_loop_command(['LOAD_REGSERVER']) :- 
	start_regserver_checking_other_rec_resources_are_loaded,
	!.
process_regulus_loop_command(['LOAD_REGSERVER', RegserverPort0]) :-
	(   coerce_atom_to_int(RegserverPort0, RegserverPort) ->
	    true
	;
	    format2error('~N*** Error: Regserver port "~w" must be an integer~n', [RegserverPort0]),
	    fail
	),
	start_regserver_checking_other_rec_resources_are_loaded(RegserverPort),
	!.
process_regulus_loop_command(['CLOSE_DOWN_RECOGNITION']) :-
	close_down_recognition_resources,
	!.
process_regulus_loop_command(['RECOGNISE']) :-
	recognise_as_defined_by_config_file(Recognised),
	process_regulus_loop_item_speech_input(Recognised).
process_regulus_loop_command(['COMPILE_LITE']) :-
	compile_lite_from_cfg_file_parameters.
process_regulus_loop_command(['COMPILE_CURRENT_LITE_NAMESPACE']) :-
	compile_current_lite_namespace_from_cfg_file_parameters.

%---------------------------------------------------------------	

get_regulus_commands_available_for_java_gui(PosPackage, NegPackage) :-
	format('~N--- Getting command status information for GUI~n', []),
	findall(CommandString-Status,
		regulus_command_string_and_status(CommandString, Status),
		Pairs),
	sort(Pairs, Pairs1),
	package_command_strings_status_for_java_gui(Pairs1, PosPackage, NegPackage),
	format('~N--- Found command status information for GUI~n', []),
	!.
get_regulus_commands_available_for_java_gui(PosPackage, NegPackage) :-
	format2error('*** Error: bad call: ~w~n',
		     [get_regulus_commands_available_for_java_gui(PosPackage, NegPackage)]),
	fail.

regulus_command_string_and_status(CommandString, Status) :-
	regulus_loop_command_available(CommandList, Status),
	join_with_spaces(CommandList, Command),
	atom_codes(Command, CommandString).

package_command_strings_status_for_java_gui(Pairs, PosPackage, NegPackage) :-
	findall(PosString,
		member(PosString-yes, Pairs),
		PosStrings),
	PosPackage =.. [positive | PosStrings],

	findall(NegString,
		member(NegString-no, Pairs),
		NegStrings),
	NegPackage =.. [negative | NegStrings],
	!.

%---------------------------------------------------------------

check_regulus_status_for_java_gui("bidirectional_on", YesNo) :-
	(   is_in_bidirectional_mode ->
	    YesNo = "yes" ;
	    YesNo = "no"
	).
check_regulus_status_for_java_gui(Other, YesNo) :-
	format('~N*** Bad call: ~w~n',
	       [check_regulus_status_for_java_gui(Other, YesNo)]),
	fail.

%---------------------------------------------------------------	

regulus_loop_command_available(['HELP'], yes).

regulus_loop_command_available(['LOAD'], YesNo) :-
	(   regulus_config_file_or_files_ok(regulus_grammar, regulus) ->
	    
	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['LOAD_DEBUG'], YesNo) :-
	(   regulus_config_file_or_files_ok(regulus_grammar, regulus) ->
	    
	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['LOAD_PREFERENCES'], YesNo) :-
	(   regulus_config_file_or_files_ok(parse_preferences, pl) ->
	    
	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['LOAD_SURFACE_PATTERNS'], YesNo) :-
	(   (   regulus_config_file_or_files_ok(surface_patterns, pl),
		regulus_config_file_or_files_ok(discriminants, pl),
		regulus_config_file_or_files_ok(tagging_grammar, pl),
		regulus_config_file_or_files_ok(target_model, pl)
	    ) ->

	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['DCG'], yes).

regulus_loop_command_available(['LC'], yes).

regulus_loop_command_available(['NUANCE_PARSE'], YesNo) :-
	(   recognition_resources_are_defined ->
	    
	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['SURFACE'], YesNo) :-
	(   regulus_loop_command_available(['LOAD_SURFACE_PATTERNS'], yes) ->
	    
	    YesNo = yes ;
	    
	    YesNo = no
	).

regulus_loop_command_available(['NUANCE'], YesNo) :-
	(   (   regulus_config_file_or_files_ok(regulus_grammar, regulus),
		regulus_config_defined(nuance_grammar)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['GEMINI'], YesNo) :-
	(   (   regulus_config_file_or_files_ok(regulus_grammar, regulus),
		regulus_config_defined(gemini_grammar)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRACE'], YesNo) :-
	(   grammar_is_loaded ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['NOTRACE'], YesNo) :-
	(   grammar_is_loaded ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['COMPACTION'], yes).

regulus_loop_command_available(['NO_COMPACTION'], yes).

regulus_loop_command_available(['LOAD_GENERATION'], YesNo) :-
	(   (    (   regulus_config_file_or_files_ok(generation_regulus_grammar, regulus)
		 ;
		     regulus_config_file_or_files_ok(regulus_grammar, regulus)
		 ),
		 (   regulus_config_defined(generation_grammar)
		 ;
		     regulus_config_defined(generation_rules)
		 ),
		grammar_is_loaded
	    )  ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['LOAD_GENERATION', SubDomain], yes) :-
	(    (   regulus_config_file_or_files_ok(generation_regulus_grammar, regulus)
	     ;
		 regulus_config_file_or_files_ok(regulus_grammar, regulus)
	     ),
	    
	    (   regulus_config_defined(generation_grammar(SubDomain))
	    ;
		regulus_config_defined(generation_rules(SubDomain))
	    ),
	    grammar_is_loaded
	).

regulus_loop_command_available(['EBL_LOAD_GENERATION'], YesNo) :-
	(   (   regulus_config_defined(working_file_prefix),
		get_regulus_config_item(ebl_regulus_no_binarise_grammar, GrammarFileBase),
		add_tag_to_files_in_list([GrammarFileBase], default, [GrammarFile]),
		file_ok(GrammarFile, regulus),
	 
		(   regulus_config_defined(generation_grammar) ;
		    regulus_config_defined(generation_rules) ;
		    regulus_config_defined(generation_grammar(default)) ;
		    regulus_config_defined(generation_rules(default))
		    
		)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

%regulus_loop_command_available(['EBL_LOAD_GENERATION', SubDomain], yes)	:-
%	get_regulus_config_item(ebl_regulus_no_binarise_grammar, GrammarFileBase),
%	add_tag_to_files_in_list([GrammarFileBase], SubDomain, [GrammarFile]),
%	file_ok(GrammarFile, regulus),
%		 
%	(   regulus_config_defined(generation_grammar) ;
%	    regulus_config_defined(generation_rules)
%	).

regulus_loop_command_available(['NORMAL_PROCESSING'], yes).

regulus_loop_command_available(['TRANSLATE'], YesNo) :-
	regulus_loop_command_available(['LOAD_TRANSLATE'], YesNo).

regulus_loop_command_available(['DIALOGUE'], YesNo) :-
	regulus_loop_command_available(['LOAD_DIALOGUE'], YesNo).

regulus_loop_command_available(['GENERATION'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_GENERATION'], yes) ;
		regulus_loop_command_available(['EBL_LOAD_GENERATION'], yes)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['ECHO_ON'], yes).

regulus_loop_command_available(['ECHO_OFF'], yes).

regulus_loop_command_available(['LINE_INFO_ON'], yes).

regulus_loop_command_available(['LINE_INFO_OFF'], yes).

regulus_loop_command_available(['INTERLINGUA'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_TRANSLATE'], yes),
		regulus_config_file_or_files_ok(interlingua_declarations, pl),
		regulus_config_file_or_files_ok(to_interlingua_rules, pl),
		regulus_config_file_or_files_ok(from_interlingua_rules, pl)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['NO_INTERLINGUA'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_TRANSLATE'], yes),
		regulus_config_file_or_files_ok(transfer_rules, pl)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['NO_ELLIPSIS_PROCESSING'], YesNo) :-
	(   current_predicate(user:ellipsis_class_example/2) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['LOAD_TRANSLATE'], YesNo) :-
	(   (   (   regulus_config_file_or_files_ok(generation_rules, regulus) ;
		    regulus_config_file_or_files_ok(generation_grammar, regulus)
		),
		(   regulus_config_file_or_files_ok(transfer_rules, pl) ;
		    (   regulus_config_file_or_files_ok(interlingua_declarations, pl),
			regulus_config_file_or_files_ok(to_interlingua_rules, pl),
			regulus_config_file_or_files_ok(from_interlingua_rules, pl)
		    )
		)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRANSLATE_TRACE_ON'], YesNo) :-
	regulus_loop_command_available(['LOAD_TRANSLATE'], YesNo).

regulus_loop_command_available(['TRANSLATE_TRACE_OFF'], YesNo) :-
	regulus_loop_command_available(['LOAD_TRANSLATE'], YesNo).

regulus_loop_command_available(['ANSWER_ELLIPSIS_ON'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_TRANSLATE'], yes),
		ellipsis_rules_are_loaded
	    )  ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['ANSWER_ELLIPSIS_OFF'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_TRANSLATE'], yes),
		ellipsis_rules_are_loaded
	    )  ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['LOAD_DIALOGUE'], YesNo) :-
	(   regulus_config_file_or_files_ok(dialogue_files, pl) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['INIT_DIALOGUE'], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_DIALOGUE'], yes),
		current_predicate(user:initial_dialogue_state/1)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['INIT_DIALOGUE', _Arg], YesNo) :-
	(   (   regulus_loop_command_available(['LOAD_DIALOGUE'], yes),
		current_predicate(user:initial_dialogue_state/2)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRANSLATE_CORPUS'], YesNo) :-
	(   (   translation_rules_are_loaded,
		regulus_config_file_or_files_ok(translation_corpus, pl),
		regulus_config_file_or_files_ok(translation_corpus_results, pl)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).
regulus_loop_command_available(['CHECK_BACKTRANSLATION', _File], YesNo) :-
	(   (   translation_rules_are_loaded
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).
regulus_loop_command_available(['STORE_TRANSLATION_TARGET_VOCAB', _File], YesNo) :-
	!,
	YesNo = yes.
regulus_loop_command_available(['TRANSLATE_CORPUS', Id], yes) :-
	translation_rules_are_loaded,
	regulus_config_file_or_files_ok(translation_corpus(Id), pl),
	regulus_config_file_or_files_ok(translation_corpus_results(Id), pl).

regulus_loop_command_available(['COMPILE_ELLIPSIS_PATTERNS'], YesNo) :-
	(   (   grammar_is_loaded,
		translation_rules_are_loaded,
		regulus_config_defined(ellipsis_classes),
		regulus_config_defined(top_level_cat)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS'], YesNo) :-
	(   (   grammar_is_loaded,
		translation_rules_are_loaded,
		regulus_config_file_or_files_ok(translation_speech_corpus, txt),
		regulus_config_defined(translation_speech_corpus_results),
		regulus_config_defined(translation_rec_params)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS', Id], yes) :-
	grammar_is_loaded,
	translation_rules_are_loaded,
	regulus_config_file_or_files_ok(translation_speech_corpus(Id), txt),
	regulus_config_defined(translation_speech_corpus_results(Id)),
	regulus_config_defined(translation_rec_params).

regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS_AGAIN'], YesNo) :-
	(   (   regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS'], yes),
		regulus_config_file_or_files_ok(batchrec_trace_prolog_with_transcriptions, pl)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS_AGAIN', Id], yes) :-
	regulus_loop_command_available(['TRANSLATE_SPEECH_CORPUS', Id], yes),
	regulus_config_file_or_files_ok(batchrec_trace_prolog_with_transcriptions(Id), pl).

regulus_loop_command_available(['UPDATE_RECOGNITION_JUDGEMENTS'], YesNo) :-
	(   (   regulus_config_defined(translation_corpus_recognition_judgements),
		regulus_config_file_or_files_ok(translation_corpus_tmp_recognition_judgements, pl)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['UPDATE_RECOGNITION_JUDGEMENTS', Id], yes) :-
	regulus_config_defined(translation_corpus_recognition_judgements),
	regulus_config_defined(translation_speech_corpus(Id)),
	regulus_config_file_or_files_ok(translation_corpus_tmp_recognition_judgements(Id), pl).

regulus_loop_command_available(['UPDATE_TRANSLATION_JUDGEMENTS'], YesNo) :-
	(   (   regulus_config_defined(translation_corpus_judgements),
		regulus_config_file_or_files_ok(translation_corpus_results)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['UPDATE_TRANSLATION_JUDGEMENTS', Id], yes) :-
	regulus_config_defined(translation_corpus_judgements),
	regulus_config_file_or_files_ok(translation_corpus_results(Id)).

regulus_loop_command_available(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH'], YesNo) :-
	(   (   regulus_config_defined(translation_corpus_judgements),
		regulus_config_file_or_files_ok(translation_speech_corpus_results)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['UPDATE_TRANSLATION_JUDGEMENTS_SPEECH', Id], yes) :-
	regulus_config_defined(translation_corpus_judgements),
	regulus_config_file_or_files_ok(translation_speech_corpus_results(Id)).

regulus_loop_command_available(['EBL_TREEBANK'], YesNo) :-
	(   (   regulus_config_file_or_files_ok(ebl_corpus, pl),
		regulus_config_defined(top_level_cat),
		grammar_is_loaded
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_TRAIN'], YesNo) :-
	(   (   regulus_config_file_or_files_ok(ebl_treebank),
		regulus_config_defined(ebl_operationality),
		get_regulus_config_item(ebl_operationality, file(OperationalityFile)),
		file_ok(OperationalityFile, pl),
		grammar_is_loaded
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_POSTPROCESS'], YesNo) :-
	(   regulus_config_file_or_files_ok(ebl_raw_regulus_grammar) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_LOAD'], YesNo) :-
	(   (   (   regulus_config(ebl_regulus_component_grammar, GrammarFile) ->
		    true ;
		    regulus_config_defined(working_file_prefix),
		    get_regulus_config_item(ebl_regulus_grammar, GrammarFile0),
		    add_tag_to_files_in_list([GrammarFile0], default, [GrammarFile])
		),
		file_or_files_ok(GrammarFile, regulus)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_NUANCE'], YesNo) :-
	(   (   regulus_loop_command_available(['EBL_LOAD'], yes),
		regulus_config_defined(ebl_nuance_grammar)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_GEMINI'], YesNo) :-
	(   (   regulus_loop_command_available(['EBL_LOAD'], yes),
		regulus_config_defined(ebl_gemini_grammar)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL'], YesNo) :-
	(   (   regulus_loop_command_available(['EBL_TREEBANK'], yes),
		regulus_config_defined(ebl_nuance_grammar)
	    )  ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_ANALYSIS'], YesNo) :-
	(   regulus_loop_command_available(['EBL_TREEBANK'], yes) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['EBL_GENERATION'], YesNo) :-
	(   (   regulus_loop_command_available(['EBL_TREEBANK'], yes),
		(   regulus_config_defined(generation_grammar) ;
		    regulus_config_defined(generation_rules)
		)
	    ) ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['STEPPER'], YesNo) :-
	(   grammar_is_loaded ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['LOAD_RECOGNITION'], YesNo) :-
	(   recognition_resources_are_defined ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['LOAD_REGSERVER'], YesNo) :-
	(   recognition_resources_are_defined ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['RECOGNISE'], YesNo) :-
	(   recognition_is_loaded ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['CLOSE_DOWN_RECOGNITION'], YesNo) :-
	(   recognition_is_loaded ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['NUANCE_COMPILE'], YesNo) :-
	(   do_nuance_compile_is_meaningful ->

	    YesNo = yes ;

	    YesNo = no
	).

regulus_loop_command_available(['NUANCE_COMPILE_WITH_PCFG'], YesNo) :-
	(   do_pcfg_training_is_meaningful ->

	    YesNo = yes ;

	    YesNo = no
	).

%---------------------------------------------------------------	

execute_translate_speech_corpus_command(NewOrStoredRecognitionResults, Id, Command) :-
	(   Id = default ->
	    
	    get_regulus_config_item(translation_speech_corpus, TranscriptionsFile),
	    get_regulus_config_item(translation_speech_corpus_results, ResultsFile),
	    (   regulus_config(translation_corpus_tmp_recognition_judgements, TmpRecognitionJudgementsFile) ->
		true ;
		TmpRecognitionJudgementsFile = null 
	    ),
	    get_regulus_config_item(batchrec_trace_prolog_with_transcriptions, PrologBatchrecFileWithTranscriptions) ;

	    get_regulus_config_item(translation_speech_corpus(Id), TranscriptionsFile),
	    get_regulus_config_item(translation_speech_corpus_results(Id), ResultsFile),
	    (   regulus_config(translation_corpus_tmp_recognition_judgements(Id), TmpRecognitionJudgementsFile) ->
		true ;
		TmpRecognitionJudgementsFile = null
	    ),
	    get_regulus_config_item(batchrec_trace_prolog_with_transcriptions(Id), PrologBatchrecFileWithTranscriptions)
	),
	get_regulus_config_item(translation_rec_params, RecParams),
	get_regulus_config_item(working_directory, WorkingDirectory),
	get_regulus_config_item(wavfiles, WavfilesFile),
	get_regulus_config_item(batchrec_trace, BatchrecTraceFile),
	get_regulus_config_item(batchrec_trace_prolog, PrologBatchrecFile),
	load_all_translation_corpus_judgement_files,
	(   regulus_config(translation_corpus_recognition_judgements, RecognitionJudgementsFile) ->
	    load_translation_corpus_recognition_judgements_file(RecognitionJudgementsFile) ;
	    true
	),
	load_paraphrase_file_if_defined,
	(   ( Id = default, regulus_config(wavfile_preceding_context, WavfileContextFile) ) ->
	    
	    load_wavfile_context_file(WavfileContextFile) ;

	    ( Id \== default, regulus_config(wavfile_preceding_context(Id), WavfileContextFile) ) ->
	    
	    load_wavfile_context_file(WavfileContextFile) ;
	    
	    true
	),
	get_regulus_runtime_config(translation_mode, TranslationMode),
	recognise_parse_transfer_and_generate_file(
	    NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, ResultsFile, WavfilesFile, BatchrecTraceFile, 
	    PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, WorkingDirectory, TranslationMode, Command),
	safe_absolute_file_name(ResultsFile, ResultsFile1),
	print_timing_summary_for_translation_result_file(ResultsFile1),
	format('~NResults written to ~w~n', [ResultsFile1]),
	%create_translation_rule_learning_files(ResultsFile1),
	(   TmpRecognitionJudgementsFile \== null ->
	    make_tmp_recognition_judgements_file(PrologBatchrecFileWithTranscriptions, TmpRecognitionJudgementsFile) ;
	    true
	),
	!.
 
%---------------------------------------------------------------	
 
execute_dialogue_speech_corpus_command(NewOrStoredRecognitionResults, Id) :-
	(   Id = default ->
	    
	    get_regulus_config_item(dialogue_speech_corpus, TranscriptionsFile),
	    get_regulus_config_item(dialogue_speech_corpus_results, ResultsFile),
	    get_regulus_config_item(dialogue_batchrec_trace_prolog_with_transcriptions, PrologBatchrecFileWithTranscriptions) ;

	    get_regulus_config_item(dialogue_speech_corpus(Id), TranscriptionsFile),
	    get_regulus_config_item(dialogue_speech_corpus_results(Id), ResultsFile),
	    get_regulus_config_item(dialogue_batchrec_trace_prolog_with_transcriptions(Id), PrologBatchrecFileWithTranscriptions)
	),
	get_regulus_config_item(dialogue_rec_params, RecParams),
	get_regulus_config_item(working_directory, WorkingDirectory),
	get_regulus_config_item(wavfiles, WavfilesFile),
	get_regulus_config_item(batchrec_trace, BatchrecTraceFile),
	get_regulus_config_item(batchrec_trace_prolog, PrologBatchrecFile),
	(   regulus_config(dialogue_corpus_judgements, JudgementsFile) ->
	    load_dialogue_corpus_judgements_file(JudgementsFile) ;
	    true
	),
	load_paraphrase_file_if_defined,
	recognise_and_dialogue_process_file(
	    NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, ResultsFile, WavfilesFile, BatchrecTraceFile, 
	    PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, WorkingDirectory),
	safe_absolute_file_name(ResultsFile, ResultsFile1),
	format('~NResults written to ~w~n', [ResultsFile1]),
	!.

%---------------------------------------------------------------

execute_learn_transfer_rules_command(ResultsFile, Type) :-
	absolute_file_name(ResultsFile, AbsResultsFile),
	translation_rule_learning_file_pathnames(AbsResultsFile,
						 ToInterlinguaFile, FromInterlinguaFile,
						 GenerationFile, BadTranslationsFile, 
						 SuggestionsFile),
	(   Type = bad_to_interlingua ->
	    get_regulus_config_item(to_interlingua_rule_learning_config_file, CFGFile),
	    process_to_interlingua_learning_file(ToInterlinguaFile, CFGFile, SuggestionsFile) ;

	    Type = bad_from_interlingua ->
	    get_regulus_config_item(from_interlingua_rule_learning_config_file, CFGFile),
	    process_from_interlingua_learning_file(FromInterlinguaFile, CFGFile, SuggestionsFile) ;

	    Type = bad_generation ->
	    get_regulus_config_item(from_interlingua_rule_learning_config_file, CFGFile),
	    process_from_interlingua_learning_file(GenerationFile, CFGFile, SuggestionsFile) ;

	    Type = bad_translation ->
	    get_regulus_config_item(from_interlingua_rule_learning_config_file, CFGFile),
	    process_from_interlingua_learning_file(BadTranslationsFile, CFGFile, SuggestionsFile) ;

	    true ->
	    format('~NUnknown argument ~w in LEARN_TRANSFER_RULES: should be one of \'bad_to_interlingua\', \'bad_from_interlingua\', \'bad_generation\' or \'bad_translation\'', [Type]),
	    fail
	),
	!.

%---------------------------------------------------------------


make_ebl_ignore_feats_file(EBLIgnoreFeatsFile) :-
	regulus_config(ebl_ignore_feats, EBLIgnoreFeats),
	get_regulus_config_item(ebl_ignore_feats_file, EBLIgnoreFeatsFile),
	is_list_of_atoms(EBLIgnoreFeats),
	findall(ignore_feature(Feat),
		member(Feat, EBLIgnoreFeats),
		EBLIgnoreFeatsDecls),
	list_to_prolog_file(EBLIgnoreFeatsDecls, EBLIgnoreFeatsFile),
	!.
make_ebl_ignore_feats_file(EBLIgnoreFeatsFile) :-
	get_regulus_config_item(ebl_ignore_feats_file, EBLIgnoreFeatsFile),
	list_to_prolog_file([], EBLIgnoreFeatsFile),
	!.
make_ebl_ignore_feats_file(_EBLIgnoreFeatsFile) :-
	format2error('~NError: bad call to make_ebl_ignore_feats_file/1~n', []),
	fail.

%---------------------------------------------------------------

create_ebl_operationality_file(FileIn, FileOut) :-
	get_regulus_config_item(tmp_ebl_operational_file, FileOut),
	compile_operationality_file(FileIn, FileOut),
	!.
create_ebl_operationality_file(FileIn, FileOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [create_ebl_operationality_file(FileIn, FileOut)]),
	fail.
	
%---------------------------------------------------------------

print_all_loop_commands_with_help_text :-
	findall([Command, HelpText],
		loop_command(Command, HelpText),
		Pairs),
	sort(Pairs, SortedPairs),
	print_loop_commands_with_help_text(SortedPairs).

print_all_loop_commands_with_matching_help_text(Atom) :-
	findall([Command, HelpText],
		(   loop_command(Command, HelpText),
		    loop_command_matches_meta_help_query(Command, HelpText, Atom)
		),
		Pairs),
	sort(Pairs, SortedPairs),
	length(SortedPairs, N),
	(   N = 0 ->
	    format('~N~nNo commands matching "~w"~n', [Atom]) ;

	    N = 1 ->
	    format('~N~nOne command matching "~w":~n~n', [Atom]) ;

	    otherwise ->
	    format('~N~n~d commands matching "~w":~n~n', [N, Atom])
	),	    
	print_loop_commands_with_help_text(SortedPairs).

print_all_config_file_entries_with_matching_help_text(Atom) :-
	findall([Entry, HelpText],
		(   config_file_entry(Entry, HelpText, _Section),
		    loop_command_matches_meta_help_query(Entry, HelpText, Atom)
		),
		Pairs),
	sort(Pairs, SortedPairs),
	length(SortedPairs, N),
	(   N = 0 ->
	    format('~N~nNo config file entries matching "~w"~n', [Atom]) ;

	    N = 1 ->
	    format('~N~nOne config file entry matching "~w":~n~n', [Atom]) ;

	    otherwise ->
	    format('~N~n~d config file entries matching "~w":~n~n', [N, Atom])
	),
	print_loop_commands_with_help_text(SortedPairs),
	!.

loop_command_matches_meta_help_query(CommandOrConfigEntry, HelpText, Atom) :-
	(   list_matches_meta_help_query(CommandOrConfigEntry, Atom)
	;
	    term_matches_meta_help_query(CommandOrConfigEntry, Atom)
	;
	    atom_matches_meta_help_query(CommandOrConfigEntry, Atom)
	;
	    atom_matches_meta_help_query(HelpText, Atom)
	),
	!.

list_matches_meta_help_query(List, QueryAtom) :-
	nonvar(List),
	List = [F | R],
	(   atom_matches_meta_help_query(F, QueryAtom) ;
	    list_matches_meta_help_query(R, QueryAtom)
	),
	!.

term_matches_meta_help_query(Term, Atom) :-
	compound(Term),
	functor(Term, F, _N),
	atom_matches_meta_help_query(F, Atom).

atom_matches_meta_help_query(TextAtom, QueryAtom) :-
	atom(TextAtom),
	atom(QueryAtom),
	lowercase_atom(TextAtom, TextAtom1),
	lowercase_atom(QueryAtom, QueryAtom1),
	atom_codes(TextAtom1, Text),
	atom_codes(QueryAtom1, Query),
	consecutive_sublist(Query, Text),
	!.

consecutive_sublist(SubList, List) :-
	append(_Prefix, Rest, List),
	append(SubList, _Suffix, Rest),
	!.

print_loop_commands_with_help_text([]).
print_loop_commands_with_help_text([F | R]) :-
	print_loop_command_with_help_text(F),
	!,
	print_loop_commands_with_help_text(R).

print_loop_command_with_help_text([Command, HelpText]) :-
	format('~N', []),
	print_loop_command(Command),
	(   HelpText = '' ->
	    %format('~N--- No documentation available~n', [])
	    format('~n', [])
	;
	    otherwise ->
	    format('(~w)~n', [HelpText])
	),
	!.
print_loop_command_with_help_text(Other) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_loop_command_with_help_text(Other)]),
	fail.

print_loop_command([]) :-
	!.
print_loop_command([F | R]) :-
	!,
	(   var(F) ->
	    F1 = '<Arg>' ;
	    F1 = F
	),
	format('~w ', [F1]),
	print_loop_command(R).
print_loop_command(Atom) :-
	atom(Atom),
	format('~w', [Atom]),
	!.
print_loop_command(Term) :-
	compound(Term),
	functor(Term, F, _N),
	format('~w(<Arg>)', [F]),
	!.

%---------------------------------------------------------------

% Call save_all_regulus_preds/1 before compilation and restore_all_regulus_preds/1 after,
% to make it possible to do generator grammar compilation inside an image that's
% running a different analysis grammar. Convenient for developing translation applications.

regulus2generator(GrammarFile, DCGFile, ModuleName, GenerationPred, TopLevelCat, TopLevelFeatName,
		  IncrementalDeepeningParams, GeneratorFile) :-
	get_regulus_config_item(tmp_preds, TmpPredsFile),
	save_all_regulus_preds(TmpPredsFile),
	on_exception(
	_Exception,
	regulus2generator1(GrammarFile, DCGFile, ModuleName, GenerationPred, TopLevelCat, TopLevelFeatName,
			   IncrementalDeepeningParams, GeneratorFile),
	restore_all_regulus_preds(TmpPredsFile)
    ),
	restore_all_regulus_preds(TmpPredsFile).

regulus2generator1(GrammarFile, DCGFile, ModuleName, GenerationPred, TopLevelCat, TopLevelFeatName,
		   IncrementalDeepeningParams, GeneratorFile) :-
	regulus2dcg(GrammarFile, DCGFile, keep_line_info(static_for_generation), SucceededOrFailed),
	(   SucceededOrFailed = succeeded ->
	    true
	;
	    format2error('~NError: unable to compile Regulus file ~w~n', [GrammarFile]),
	    fail
	),
	(   regulus_config(role_marked_semantics, yes) ->
	    dcg2generator_role_marked(DCGFile, GeneratorFile, ModuleName, GenerationPred,
				      TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams) ;
	    dcg2generator(DCGFile, GeneratorFile, ModuleName, GenerationPred,
			  TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams)
	).

%---------------------------------------------------------------	
 
make_macro_expanded_file(GrammarFileOrFiles, MacroExpandedFile) :-
	grammar_files_are_up_to_date(GrammarFileOrFiles, [MacroExpandedFile]),
	!.
make_macro_expanded_file(GrammarFileOrFiles, MacroExpandedFile) :-
	on_exception(
	Exception, 
	make_macro_expanded_file1(GrammarFileOrFiles, MacroExpandedFile),
	inform_about_top_level_regulus_error(Exception)
    ),
	!.
make_macro_expanded_file(GrammarFileOrFiles, _MacroExpandedFile) :-
	format('~NWarning: unable to create macro-expanded version of Regulus grammar ~w~n', [GrammarFileOrFiles]).

make_macro_expanded_file_init :-
	retract_all_regulus_preds.

make_macro_expanded_file1(GrammarFileOrFiles, MacroExpandedFile) :-
	make_macro_expanded_file_init,
	read_regulus_file_or_files(GrammarFileOrFiles, ReadRules0, Decls),
	add_lexical_feature_defaults(ReadRules0, ReadRules1),
	convert_rules_to_strcat_semantics_if_necessary(ReadRules1, ReadRules2),
	expand_abbreviations_in_rules(ReadRules2, ReadRules),
	append(Decls, ReadRules, AllItems),
	write_macro_expanded_file(AllItems, MacroExpandedFile).

write_macro_expanded_file(ReadRules, MacroExpandedFile) :-
	absolute_file_name(MacroExpandedFile, AbsMacroExpandedFile),

	open_regulus_file(AbsMacroExpandedFile, write, S),
	write_macro_expanded_file_to_stream(ReadRules, '*no_file*', '*no_line_numbers*', S),
	close(S),

	length(ReadRules, NReadRules),	
	format('~N -- Written macro-expanded version of grammar (~d entries): ~w~n', [NReadRules, AbsMacroExpandedFile]),
	!.	

write_macro_expanded_file_to_stream([], _LastFile, _LastLineNnmber, _S).
write_macro_expanded_file_to_stream([F | R], LastFile, LastLineNumber, S) :-
	write_macro_expanded_item_to_stream(F, LastFile-NextFile, LastLineNumber-NextLineNumber, S),
	!,
	write_macro_expanded_file_to_stream(R, NextFile, NextLineNumber, S).

write_macro_expanded_item_to_stream(Item, LastFile-NextFile, LastLineNumber-NextLineNumber, S) :-
	split_macro_expanded_item_into_main_item_and_line_info(Item, Main, NextFile, NextLineNumber),
	write_macro_expanded_item_line_info_to_stream(NextFile, NextLineNumber, LastFile, LastLineNumber, S),
	write_macro_expanded_item_to_stream1(Main, S).

split_macro_expanded_item_into_main_item_and_line_info(Item, Main, File, LineNumber) :-
	Item = declaration(_Label, Main, line_info(_, LineNumber, File)),
	!.
split_macro_expanded_item_into_main_item_and_line_info(Item, Main, File, LineNumber) :-
	Item = rule(Main, line_info(_, LineNumber, File)),
	!.
split_macro_expanded_item_into_main_item_and_line_info(Item, Main, File, LineNumber) :-
	Item = Main,
	File = '*no_file*',
	LineNumber = '*no_line_number*',
	!.

write_macro_expanded_item_line_info_to_stream(File, LineNumber, LastFile, LastLineNumber, S) :-
	(   File = LastFile ->
	    true ;

	    format(S, '~N~n% File: ~w~n', [File])
	),
	(   LineNumber = LastLineNumber ->
	    true ;

	    LineNumber = Line-Line ->
	    format(S, '~N~n% Line ~w~n', [Line]) ;

	    format(S, '~N~n% Lines ~w~n', [LineNumber])
	),
	!.
write_macro_expanded_item_line_info_to_stream(_File, _LineNumber, _LastFile, _LastLineNumber, _S).

write_macro_expanded_item_to_stream1(Item, S) :-
	copy_term(Item, Item1),
	make_ground(Item1),
	write_macro_expanded_item_to_stream2(Item1, S),
	!.

write_macro_expanded_item_to_stream2((Head --> Body), S) :-
	format(S, '~N~q -->~n', [Head]),
	write_macro_expanded_rule_body_to_stream(Body, S),
	format(S, '.~n', []),
	!.
write_macro_expanded_item_to_stream2(Item, S) :-
	format(S, '~N~q.~n', [Item]),
	!.

write_macro_expanded_rule_body_to_stream((F, R), S) :-
	write_macro_expanded_rule_body_to_stream(F, S),
	format(S, ',~n', []),
	write_macro_expanded_rule_body_to_stream(R, S).
write_macro_expanded_rule_body_to_stream(Item, S) :-
	format(S, '~8|~q', [Item]),
	!.

%---------------------------------------------------------------

make_and_load_dcg_and_lc(GrammarFile, IgnoreFeatsFiles, NormalOrDebug,
			 DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile) :-
	timed_call(make_and_load_dcg_and_lc1(GrammarFile, IgnoreFeatsFiles, NormalOrDebug,
					     DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile),
		   Time),
	format('~N~nGrammar loaded, time = ~2f seconds~n', [Time]).

make_and_load_dcg_and_lc1(GrammarFile0, IgnoreFeatsFiles, _NormalOrDebug,
			  DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile) :-	
	grammar_files_are_up_to_date(GrammarFile0,
				     [DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile]),
	!,
	% Need to do this to get certain declarations - could try caching the information instead
	(   is_list(GrammarFile0) ->
	    append(GrammarFile0, IgnoreFeatsFiles, GrammarFile) ;
	    append([GrammarFile0], IgnoreFeatsFiles, GrammarFile)
	),	
	read_regulus_file_or_files(GrammarFile, _, _),
	
	load_compiled_grammar_files(DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile),

	init_stepper_loop.
make_and_load_dcg_and_lc1(GrammarFile0, IgnoreFeatsFiles, NormalOrDebug,
			  DCGFile, ReflectiveDCGFile, StanfordDCGFile, LCTablesFile) :-
	(   is_list(GrammarFile0) ->
	    append(GrammarFile0, IgnoreFeatsFiles, GrammarFile)
	;
	    append([GrammarFile0], IgnoreFeatsFiles, GrammarFile)
	),
	format('~N~n--- LOADING GRAMMAR IN DCG FORM ---~n~n', []),
	regulus2dcg_and_load(GrammarFile, static, DCGFile),
	add_progress_line(static_dcg),

	(   NormalOrDebug = debug ->
	    make_dummy_top_level_rule_file(DummyTopLevelRuleFile),
	    append(GrammarFile, [DummyTopLevelRuleFile], GrammarFileWithDummies)
	;
	    GrammarFileWithDummies = GrammarFile
	),

	format('~N~n--- LOADING GRAMMAR IN REFLECTIVE DCG FORM ---~n~n', []),
	regulus2dcg_and_load(GrammarFileWithDummies, dynamic(normal), ReflectiveDCGFile),
	add_progress_line(dynamic_dcg),

	(   get_regulus_runtime_config(load_lc, no) ->

	    format('~N~n--- NOT LOADING GRAMMAR IN LEFT-CORNER PARSER FORM ---~n', [])
	;
	    format('~N~n--- LOADING GRAMMAR IN LEFT-CORNER PARSER FORM ---~n~n', []),
	    regulus2dcg_and_load(GrammarFileWithDummies, stanford, StanfordDCGFile),
	    add_progress_line(stanford_dcg),
	    format('~N~nBuilding left-corner parser tables... ', []), flush_output(user),
	    timed_call(build_reach_tables(LCTablesFile), TimeForReachTables),
	    add_progress_line(stanford_tables),
	    load_regulus_dcg_file(LCTablesFile),
	    add_progress_line(load_stanford_tables),
	    format('done, ~2f secs~n', [TimeForReachTables])
	),

	init_stepper_loop.

%---------------------------------------------------------------	

grammar_files_are_up_to_date(GrammarFileOrFiles, OutputFiles) :-
	files_in_list_all_exist(OutputFiles),

	get_regulus_runtime_config(current_cfg_file, CFGFile),
	include_closure_for_file_or_list(CFGFile, AllCFGFiles, cfg),

	include_closure_for_file_or_list(GrammarFileOrFiles, AllGrammarFiles, regulus),

	datime_for_file_list(AllCFGFiles, latest, LastTimeCFGFilesTouched),
	datime_for_file_list(AllGrammarFiles, latest, LastTimeGrammarFilesTouched),
	datime_for_directory('$REGULUS/Prolog', latest, pl, LastTimeRegulusPrologFilesTouched),
	datime_for_directory('$REGULUS/PrologLib', latest, pl, LastTimeRegulusPrologLibFilesTouched),

	datime_for_file_list(OutputFiles, earliest, LastTimeOutputFilesTouched),

	earlier_datime(LastTimeCFGFilesTouched, LastTimeOutputFilesTouched),
	earlier_datime(LastTimeGrammarFilesTouched, LastTimeOutputFilesTouched),
	earlier_datime(LastTimeRegulusPrologFilesTouched, LastTimeOutputFilesTouched),
	earlier_datime(LastTimeRegulusPrologLibFilesTouched, LastTimeOutputFilesTouched).

files_in_list_all_exist([]).
files_in_list_all_exist([F | R]) :-
	safe_file_exists(F),
	!,
	files_in_list_all_exist(R).
	
%---------------------------------------------------------------	

load_compiled_grammar_files(DCGFile, ReflectiveDCGFile, _StanfordDCGFile, LCTablesFile) :-
	format('~N~n--- COMPILED GRAMMAR FILES ARE UP TO DATE, LOADING ---~n~n', []),
	load_regulus_dcg_file(DCGFile),
	add_progress_line(static_dcg),
	
	load_regulus_dcg_file(ReflectiveDCGFile),
	add_progress_line(dynamic_dcg),
	
	%load_regulus_dcg_file(StanfordDCGFile),
	add_progress_line(stanford_dcg),
	add_progress_line(reachable_cat),
	add_progress_line(reachable_word),
	add_progress_line(reachable_gap),
	add_progress_line(stanford_tables),
	load_regulus_dcg_file(LCTablesFile),
	add_progress_line(load_stanford_tables).

%---------------------------------------------------------------	

regulus2dcg_and_load(InFile) :-
	regulus2dcg_and_load(InFile, static).

regulus2dcg_and_load(InFile, StaticOrDynamic) :-
	mktemp('tmp_dcg_file_XXXXXX', TmpDCGFile),
	regulus2dcg(InFile, TmpDCGFile, StaticOrDynamic, SucceededOrFailed),
	(   SucceededOrFailed = succeeded ->
	    format('~NProlog-compiling generated file~n~n', []),
	    load_regulus_dcg_file(TmpDCGFile),
	    delete_file(TmpDCGFile)
	;
	    format2error('~NError: unable to compile Regulus file ~w~n', [InFile]),
	    fail
	).

regulus2dcg_and_load(InFile, StaticOrDynamic, DCGFile) :-
	regulus2dcg(InFile, DCGFile, StaticOrDynamic, SucceededOrFailed),
	(   SucceededOrFailed = succeeded ->
	    format('~NProlog-compiling generated file~n~n', []),
	    load_regulus_dcg_file(DCGFile)
	;
	    format2error('~NError: unable to compile Regulus file ~w~n', [InFile]),
	    fail
	).

%---------------------------------------------------------------

make_dummy_top_level_rule_file(DummyTopLevelRuleFile) :-
	get_regulus_config_item(dummy_top_level_rules, DummyTopLevelRuleFile),
	dummy_top_level_rules(Rules),
	list_to_prolog_file(Rules, DummyTopLevelRuleFile),
	!.
make_dummy_top_level_rule_file(DummyTopLevelRuleFile) :-
	format2error('~N*** Error: bad call: ~w',
		     [make_dummy_top_level_rule_file(DummyTopLevelRuleFile)]),
	fail.

dummy_top_level_rules(Rules) :-
	findall(Rule,
		dummy_top_level_rule(Rule),
		Rules).

dummy_top_level_rule(Rule) :-
	Rule = category(dummy_top, [sem]).
dummy_top_level_rule(Rule) :-
	top_level_category(TopCat),
	Rule = ( TopCat:[gsem=[value=Sem]] --> dummy_top:[sem=Sem] ).
dummy_top_level_rule(Rule) :-
	category_internal(Cat, Feats),
	\+ top_level_category(Cat),
	dummy_top_level_rule_for_cat(Cat, Feats, Rule).

dummy_top_level_rule_for_cat(Cat, Feats, Rule) :-
	(   member(sem, Feats) ->
	    Rule = ( dummy_top:[sem=Sem] --> Cat, Cat:[sem=Sem] )
	;
	    Rule = ( dummy_top:[sem=[]] --> Cat, Cat:[] )
	).
	
%---------------------------------------------------------------

regulus_trace :-
	regulus_trace_init,
	spy_on_defined_regulus_dcg_categories,
	safe_abolish(user:(portray/1)),
	asserta((user:portray(X) :- regulus_print_goal(X))),
	debug.

regulus_notrace :-
	nodebug.

regulus_trace_init :-
	nospyall.

spy_on_defined_regulus_dcg_categories :-
	all_defined_regulus_categories(Cats),	
	(   Cats = [] ->
	    format('~N*** Warning: there is no Regulus grammar currently loaded.~n', []) ;
	    spy_on_defined_regulus_dcg_categories1(Cats)
	).

all_defined_regulus_categories(Cats) :-
	findall(Cat, category_internal(Cat, _Feats), Cats).

spy_on_defined_regulus_dcg_categories1([]).
spy_on_defined_regulus_dcg_categories1([F | R]) :-
	spy_on_defined_regulus_dcg_category(F),
	!,
	spy_on_defined_regulus_dcg_categories1(R).

spy_on_defined_regulus_dcg_category(Cat) :-
	functor(Goal, Cat, 6),
	(   current_predicate(Cat, Goal) ->
	    spy(Cat/6) ;
	    true
	).

regulus_print_goal(Goal) :-
	functor(Goal, F, N),
	category_internal(F, _),
	N = 6,
	!,
	copy_term(Goal, Goal1),
	Goal1 =.. [CatName, Tree, SynFeats, Local, Global, WordsIn, WordsOut],
	format('~N    Category: ~w', [CatName]),
	(   var(WordsOut) ->
	    format('~N       Words: ~w', [WordsIn]) ;
	    append(WordsDone, WordsOut, WordsIn),
	    format('~N       Words: ~w', [WordsDone])
	),
	(   var(Local) ->
	    true ;
	    format('~NReturn value: ~w', [Local])
	),
	(   var(Global) ->
	    true ;
	    format('~NGlobal value: ~w', [Global])
	),
	print_form_for_syn_feats(SynFeats, PrintSynFeats),
	numbervars(PrintSynFeats, 0, _),
	format('~NSyn features: ~w', [PrintSynFeats]),
	(   var(Tree) ->
	    true ;
	    format('~NParse tree:~n~n', []),
	    get_regulus_runtime_config(print_line_info, PrintLineInfo),
	    prettyprint_parse_tree(Tree, PrintLineInfo)
	),
	format('~n', []).
regulus_print_goal(Goal) :-
	print(Goal).

%---------------------------------------------------------------	

:- dynamic generation_tracing/0.

switch_on_generation_tracing :-
	generation_tracing,
	!.
switch_on_generation_tracing :-
	assertz(generation_tracing).

switch_off_generation_tracing :-
	retractall(generation_tracing).

%----------------------------------------------------------------------

regulus_dcg_parse_loop_item(lf(LF)) :-
	!,
	regulus_dcg_parse_loop_item1(grammar_is_irrelevant, lf(LF)).	
regulus_dcg_parse_loop_item(Chars) :-
	get_regulus_runtime_config(parser, trivial),
	!,
	split_string_into_words(Chars, WordsList),
	regulus_dcg_parse_loop_item1(grammar_is_irrelevant, WordsList).	
regulus_dcg_parse_loop_item(Chars) :-
	get_grammar_and_words_from_dcg_parse_loop_item(Chars, GrammarAtom, WordsList),
	!,
	regulus_dcg_parse_loop_item1(GrammarAtom, WordsList).
regulus_dcg_parse_loop_item(_Chars) :-
	\+ grammar_is_loaded,
	\+ get_regulus_runtime_config(parser, nuance),
	!,
	format('~N~nNo grammar is currently loaded. Use the LOAD or EBL_LOAD commands to load one, or use NUANCE_PARSE.', []).
regulus_dcg_parse_loop_item(_Chars) :-
	format('~N~nUsage should be <NT> : <Words> where <NT> is a nonterminal in the currently loaded DCG grammar', []),
	(   single_top_level_grammar(GrammarAtom) ->
	    format('~Nor simply <Words> in which case <NT> will be implicitly assumed to be "~w"', [GrammarAtom])
	).

get_grammar_and_words_from_dcg_parse_loop_item(Chars, GrammarAtom, WordsList) :-
	split_string_into_words(Chars, 0':, [GrammarComponent, WordsComponent]),
	split_atom_into_words(GrammarComponent, [GrammarAtom]),
	split_atom_into_words(WordsComponent, WordsList),
	functor(Goal, GrammarAtom, 6),
	current_predicate(GrammarAtom, Goal),
	!.
get_grammar_and_words_from_dcg_parse_loop_item(Chars, GrammarAtom, WordsList) :-
	(   get_regulus_runtime_config(parser, surface) ->
	    GrammarAtom = irrelevant
	;
	    get_regulus_runtime_config(parser, nuance) ->
	    GrammarAtom = irrelevant
	;
	    single_top_level_grammar(GrammarAtom)
	),
	split_string_into_words(Chars, WordsList),
	!.

single_top_level_grammar(GrammarAtom) :-
	findall(TopLevelGrammar, regulus_preds:top_level_category(TopLevelGrammar), TopLevelGrammars),
	TopLevelGrammars = [GrammarAtom],
	!.
	
regulus_dcg_parse_loop_item1(_GrammarAtom, WordList) :-
	is_list(WordList),
	\+ get_regulus_runtime_config(parser, trivial),
	\+ get_regulus_runtime_config(parser, surface),
	\+ get_regulus_runtime_config(parser, nuance),
	unknown_words_in_word_list(WordList, UnknownWords),
	UnknownWords \== [],
	!,
	format('~N~nWords not in current vocabulary: ~w~n', [UnknownWords]).
regulus_dcg_parse_loop_item1(_GrammarAtom, WordList) :-
	get_regulus_runtime_config(processing_mode, translate),
	get_regulus_runtime_config(translation_mode, TranslationMode),
	%TranslationMode = transfer
	!,
	(   WordList = lf(LF) ->
	    Source = lf(LF) ;
	    join_with_spaces(WordList, Source)
	),
	get_current_discourse_context(ContextIn),
	safe_parse_transfer_and_generate(Source, Result, ContextIn-ContextOut, Stats, TranslationMode),
	set_current_discourse_context(ContextOut),

	(   ellipsis_processing_is_activated ->
	    
	    get_preceding_target_utterance(PrecedingTargetUtt, ContextIn),
	    SourceInContext = Source+PrecedingTargetUtt ;

	    SourceInContext = Source
	),

	format('~N~nSource: ~w', [SourceInContext]),
	format('~NTarget: ~w~n', [Result]),
	format('~NOther info:~n', []),
	print_other_translation_info(Stats),

	(   current_predicate(nbest_features:feature_weight/2) ->
	    nbest_preferences_score_for_dialogue_record(Stats, TotalScore, PrefTrace),
	    format('~N~nN-BEST FEATURES AND SCORES:~n~n', []),
	    print_nbest_preference_info(PrefTrace, user),
	    format(user, '~N~nTotal score: ~2f~n', [TotalScore])
	;
	    true
	),
	
	speak_translation_result_if_appropriate(Result),
	(   ( generation_tracing, member(all_generation_tuples=AllTuples, Stats), AllTuples \== [] ) ->
	    show_generation_tuples(AllTuples) ;
	    true
	),
	(   (   is_in_bidirectional_mode,
		\+ error_result(Result),
		member(instantiated_target=TargetRepresentation, Stats)
	    ) ->

	    set_remote_discourse_context_from_target_representation(Result, TargetRepresentation, TraceChars),
	    format('~N~n*** OUTPUT FROM REMOTE PROCESSING ***~s~n*** END OF OUTPUT FROM REMOTE PROCESSING ***~n', [TraceChars]) ;
	    
	    true
	).	
regulus_dcg_parse_loop_item1(_GrammarAtom, WordList) :-
	get_regulus_runtime_config(processing_mode, translate),
	get_regulus_runtime_config(translation_mode, interlingua),
	!,
	(   WordList = lf(LF) ->
	    Source = lf(LF)
	;
	    join_with_spaces(WordList, Source)
	),
	get_current_discourse_context(ContextIn),
	parse_and_transfer_to_all_interlingua(Source, ContextIn, Tuples),
	nbest_preferences_on_interlingua(Tuples, SortedTuples),
	display_interlingua_tuples(SortedTuples),
	SortedTuples = [SelectedTuple | _Rest],
	member(context_out=ContextNext, SelectedTuple),
	member(interlingua=Interlingua, SelectedTuple),
	member(stats=StatsUpToInterlingua, SelectedTuple),
	safe_transfer_from_interlingua_and_generate(Interlingua, Result, ContextNext-ContextOut, StatsFromInterlingua),
	set_current_discourse_context(ContextOut),
	append(StatsUpToInterlingua, StatsFromInterlingua, Stats),

	(   ellipsis_processing_is_activated ->
	    get_preceding_target_utterance(PrecedingTargetUtt, ContextIn),
	    SourceInContext = Source+PrecedingTargetUtt
	;
	    SourceInContext = Source
	),

	format('~N~nSource: ~w', [SourceInContext]),
	format('~NTarget: ~w~n', [Result]),
	format('~NOther info:~n', []),
	print_other_translation_info(Stats),
	
	speak_translation_result_if_appropriate(Result),
	(   ( generation_tracing, member(all_generation_tuples=AllGenTuples, Stats), AllGenTuples \== [] ) ->
	    show_generation_tuples(AllGenTuples)
	;
	    true
	),
	(   (   is_in_bidirectional_mode,
		\+ error_result(Result),
		member(instantiated_target=TargetRepresentation, Stats)
	    ) ->
	    set_remote_discourse_context_from_target_representation(Result, TargetRepresentation, TraceChars),
	    format('~N~n*** OUTPUT FROM REMOTE PROCESSING ***~s~n*** END OF OUTPUT FROM REMOTE PROCESSING ***~n', [TraceChars])
	;
	    true
	).	
regulus_dcg_parse_loop_item1(GrammarAtom, WordList) :-
	get_regulus_runtime_config(processing_mode, dialogue),
	!,
	(   parse_with_current_parser(GrammarAtom, WordList, _Tree, _SynFeats, SemValue0, Global) ->

	    (   Global = [_Feat=SemValue] ->
		true ;
		SemValue = SemValue0
	    ),
	    %make_transfer_representation_canonical(SemValue, SemValue1),
	    SemValue = SemValue1,

	    process_lf_for_dialogue(SemValue1, WordList)
	;
	    format('~N~nUnable to parse utterance.~n', [])
	).
regulus_dcg_parse_loop_item1(GrammarAtom, WordList) :-
	get_regulus_runtime_config(processing_mode, generation),
	!,
	(   parse_with_current_parser(GrammarAtom, WordList, _Tree, SynFeats, SemValue0, Global) ->

	    (   Global = [_Feat=SemValue] ->
		true ;
		SemValue = SemValue0
	    ),
	    make_transfer_representation_canonical_and_unpack_if_necessary(SemValue, SemValue1),
	    print_form_for_syn_feats(SynFeats, PrintSynFeats),
	    numbervars(PrintSynFeats, 0, _),
	    %format('~N~nSem value: ~w~n', [SemValue]),
	    nl, prettyprint_term_with_intro('Sem value', SemValue1),
	    format('~N~nSyn features: ~w~n', [PrintSynFeats]),
	    format('~N~nGenerating:~n', []),
	    
	    (   regulus_config(top_level_generation_pred, GenerationPred) ->
		true ;
		GenerationPred = generate
	    ),
	    Call =.. [GenerationPred, SemValue1, GeneratedTree, GeneratedWords],
	    statistics(runtime, [T0, _]),
	    findall([GeneratedWords, GeneratedTree, SemValue1], call(Call), Tuples),
	    statistics(runtime, [T1, _]),
	    Time is ( T1 - T0 ) / 1000,
	    format('~N~nGeneration time: ~2f seconds~n', [Time]),
	    present_generated_paraphrases(Tuples, SemValue1) ;
	    
	    format('~N~nUnable to parse utterance.~n', [])
	).
regulus_dcg_parse_loop_item1(GrammarAtom, WordList) :-
	get_regulus_runtime_config(processing_mode, Mode),
	member(Mode, [ebl, ebl_verbose]),
	!,
	get_regulus_config_item(ebl_operationality, Operationality0),
	(   ( Operationality0 = file(File0), ground(File0) ) ->

	    safe_absolute_file_name(File0, AbsFile0),
	    format('~N~nTaking operationality criteria from ~w~n~n', [AbsFile0]),
	    create_ebl_operationality_file(AbsFile0, File),
	    Operationality = file(File) ;

	    format('~N~nUsing operationality criteria "~w" from $REGULUS/Prolog/ebl_operational.pl~n', [Operationality0]),
	    Operationality = Operationality0
	),
	(   parse_with_current_parser(GrammarAtom, WordList, Tree, _SynFeats, SemValue0, Global) ->
	    (   Global = [_Feat=SemValue] ->
		true ;
		SemValue = SemValue0
	    ),
	    ebl_train_single_example(GrammarAtom, Operationality, Tree, WordList, SemValue, RegulusRules),
	    ebl_postprocess_and_display_rules(RegulusRules, Mode)
	;
	    format('~N~nUnable to parse utterance.~n', [])
	).
regulus_dcg_parse_loop_item1(GrammarAtom, WordList) :-
	statistics(runtime, [T0, _]),
	(   WordList = lf(_) ->
	    format('~N*** Error: unable to accept LF input in normal parsing mode. You probably want to do DIALOGUE first, and maybe also INIT_DIALOGUE.~n', []),
	    fail
	;
	    print_current_parser_message
	),
	findall([Tree, SynFeats, Local, Global],
		(   parse_with_current_parser1(GrammarAtom, WordList, Tree, SynFeats, Local0, Global),
		    unpack_strcat_semantics_if_necessary(Local0, Local)
		),
		Tuples0),
	safe_remove_duplicates_preserving_order(Tuples0, Tuples),
	reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples),
	statistics(runtime, [T1, _]),
	Time is ( T1 - T0 ) / 1000,
	format('~N~nAnalysis time: ~2f seconds~n', [Time]),

	present_regulus_dcg_parses(ReorderedTuples).

%---------------------------------------------------------------

remote_parse_transfer_and_generate_for_java_gui(SourceChars, AnswerTerm) :-
	is_in_bidirectional_mode,
	sleep_if_sicstus4_and_using_gui,
	remote_regulus_call(parse_transfer_and_generate_for_java_gui(SourceChars, AnswerTerm)),
	!.
remote_parse_transfer_and_generate_for_java_gui(SourceChars, AnswerTerm) :-
	(   is_in_bidirectional_mode ->
	    Target = internal_error ;
	    Target = not_in_bidirectional_mode
	),
	InfoAlist = [],
	package_translation_results_for_java_gui(SourceChars, Target, InfoAlist, AnswerTerm),
	!.

%---------------------------------------------------------------

parse_transfer_and_generate_for_java_gui(SourceChars, AnswerTerm) :-
	sleep_if_sicstus4_and_using_gui,
	is_prolog_string(SourceChars),
	atom_codes(SourceAtom, SourceChars),
	log_event('TRANSLATE_UTTERANCE', [SourceAtom]),
	get_regulus_runtime_config(translation_mode, TranslationMode),
	zero_stored_transfer_trace,

	get_current_discourse_context(ContextIn),
	safe_parse_transfer_and_generate(SourceAtom, Target, ContextIn-ContextOut, InfoAlist, TranslationMode),
	set_current_discourse_context(ContextOut),

	collect_stored_transfer_trace_info_into_alist(TraceAlist),
	append(InfoAlist, TraceAlist, FullInfoAlist),
	package_translation_results_for_java_gui(SourceChars, Target, FullInfoAlist, AnswerTerm),

	set_remote_discourse_context_from_translate_alist_if_necessary(Target, FullInfoAlist),
	speak_translation_result_if_appropriate(Target),
	!.
parse_transfer_and_generate_for_java_gui(SourceChars, AnswerTerm) :-
	Target = internal_error,
	InfoAlist = [],
	package_translation_results_for_java_gui(SourceChars, Target, InfoAlist, AnswerTerm),
	!.

set_remote_discourse_context_from_translate_alist_if_necessary(Target, InfoAlist) :-
	is_in_bidirectional_mode,
	\+ error_result(Target),
	member(instantiated_target=TargetRepresentation, InfoAlist),
	set_remote_discourse_context_from_target_representation(Target, TargetRepresentation, _TraceChars),
	!.
set_remote_discourse_context_from_translate_alist_if_necessary(_Target, _InfoAlist).

%---------------------------------------------------------------

read_translation_output_file_for_java_gui(IdChars, AnswerTerm) :-
	format('~N--- Get translation output file for GUI: "~s"~n', [IdChars]),
	get_translation_corpus_results_file(IdChars, ResultsFile),
	package_translation_corpus_results_file_for_java_gui(ResultsFile, AnswerTerm),
	format('~N--- Found translation output file~n', []),
	!.
read_translation_output_file_for_java_gui(_IdChars, AnswerTerm) :-
	format('~N--- Unable to find translation output file~n', []),
	AnswerTerm = error.

read_speech_translation_output_file_for_java_gui(IdChars, AnswerTerm) :-
	format('~N--- Get speech translation output file for GUI: "~s"~n', [IdChars]),
	get_speech_translation_corpus_results_file(IdChars, ResultsFile),
	package_translation_corpus_results_file_for_java_gui(ResultsFile, AnswerTerm),
	!.
read_speech_translation_output_file_for_java_gui(_IdChars, AnswerTerm) :-
	format('~N--- Found speech translation output file~n', []),
	AnswerTerm = error.

store_judged_translation_output_file_for_java_gui(IdChars, JudgedFileString, Result) :-
	format('~N--- Store translation output file for GUI: "~s"~n', [IdChars]),
	get_translation_corpus_results_file(IdChars, ResultsFile),
	unpack_and_store_judged_translation_output_file_from_string(ResultsFile, JudgedFileString),
	Result = "ok",
	format('~N--- Stored translation output file~n', []),
	!.
store_judged_translation_output_file_for_java_gui(_IdChars, _JudgedFileString, Result) :-
	format('~N--- Unable to store translation output file~n', []),
	Result = "error".

store_judged_speech_translation_output_file_for_java_gui(IdChars, JudgedFileString, Result) :-
	format('~N--- Store speech translation output file for GUI: "~s"~n', [IdChars]),
	get_speech_translation_corpus_results_file(IdChars, ResultsFile),
	unpack_and_store_judged_translation_output_file_from_string(ResultsFile, JudgedFileString),
	Result = "ok",
	format('~N--- Stored speech translation output file~n', []),
	!.
store_judged_speech_translation_output_file_for_java_gui(_IdChars, _JudgedFileString, Result) :-
	format('~N--- Unable to store speech translation output file~n', []),
	Result = "error".

get_translation_corpus_results_file(IdChars, ResultsFile) :-
	is_prolog_string(IdChars),
	atom_codes(Id, IdChars),
	(   Id = default ->
	    get_regulus_config_item(translation_corpus_results, ResultsFile) ;
	    get_regulus_config_item(translation_corpus_results(Id), ResultsFile)
	),
	!.
get_translation_corpus_results_file(IdChars, _ResultsFile) :-
	(   is_prolog_string(IdChars) ->
	    atom_codes(Id, IdChars) ;
	    Id = IdChars
	),
	format2error('~N*** Error: unable to find translation corpus file "~w"~n', [Id]),
	fail.

get_speech_translation_corpus_results_file(IdChars, ResultsFile) :-
	is_prolog_string(IdChars),
	atom_codes(Id, IdChars),
	(   Id = default ->
	    get_regulus_config_item(translation_speech_corpus_results, ResultsFile) ;
	    get_regulus_config_item(translation_speech_corpus_results(Id), ResultsFile)
	),
	!.
get_translation_speech_corpus_results_file(IdChars, _ResultsFile) :-
	(   is_prolog_string(IdChars) ->
	    atom_codes(Id, IdChars) ;
	    Id = IdChars
	),
	format2error('~N*** Error: unable to find translation speech corpus file "~w"~n', [Id]),
	fail.
	
%---------------------------------------------------------------

print_current_parser_message :-
	get_regulus_runtime_config(parser, trivial),
	format('~N(Parsing with trivial parser)~n', []),
	!.
print_current_parser_message :-
	get_regulus_runtime_config(parser, dcg),
	format('~N(Parsing with DCG parser)~n', []),
	!.
print_current_parser_message :-
	get_regulus_runtime_config(parser, lc),
	format('~N(Parsing with left-corner parser)~n', []),
	!.
print_current_parser_message :-
	get_regulus_runtime_config(parser, nuance),
	format('~N(Parsing with Nuance parser)~n', []),
	!.
print_current_parser_message :-
	get_regulus_runtime_config(parser, surface),
	format('~N(Parsing with surface patterns)~n', []),
	!.
print_current_parser_message :-
	format2error('~NError: no current parser defined~n', []),
	fail.

%---------------------------------------------------------------

:- dynamic current_discourse_context/1.

get_current_discourse_context(DiscourseContext) :-
	current_discourse_context(DiscourseContext),
	!.
get_current_discourse_context(DiscourseContext) :-
	null_discourse_context(DiscourseContext),
	!.
get_current_discourse_context(X) :-
	format2error('~N*** Error: bad call: ~w~n', [get_current_discourse_context(X)]),
	fail.

set_current_discourse_context(DiscourseContext) :-
	retractall(current_discourse_context(_)),
	asserta(current_discourse_context(DiscourseContext)),
	!.
set_current_discourse_context(X) :-
	format2error('~N*** Error: bad call: ~w~n', [set_current_discourse_context(X)]),
	fail.

test_set_current_discourse_context_from_target_representation :-
	TargetAtom = 'dónde le duele',
	TargetRepresentation = [[locative,dónde], [pronoun,implicit_3_sg], [pronoun,usted], [state,doler],
				[tense,present], [utterance_type,wh], [voice,active]],
	set_current_discourse_context_from_target_representation(TargetAtom, TargetRepresentation, TraceChars),
	format('~s', [TraceChars]).

set_current_discourse_context_from_target_representation(TargetAtom, TargetRepresentation, TraceChars) :-
	with_output_to_chars(set_current_discourse_context_from_target_representation1(TargetAtom, TargetRepresentation),
			     TraceChars).

set_current_discourse_context_from_target_representation1(TargetAtom, TargetRepresentation) :-
	format('~N~nTrying to set discourse context from target: "~w"~n', [TargetAtom]),
	format('~N~nand target representation:~n~n', []),
	prettyprint(TargetRepresentation),
	(   transfer_representation_to_source_discourse(TargetRepresentation,
							SourceDiscourseRepresentation) ->
	    
	    format('~N~nNew discourse context:~n~n', []),
	    prettyprint(SourceDiscourseRepresentation) ;

	    format2error('~N*** Error: unable to create source discourse representation~n', [])
	),
	    
	get_current_discourse_context(ContextIn),
	set_preceding_target_utterance(TargetAtom, ContextIn, ContextNext),
	set_preceding_source_discourse(SourceDiscourseRepresentation, ContextNext, ContextOut),
	set_current_discourse_context(ContextOut) ;
	!.
set_current_discourse_context_from_target_representation1(TargetRepresentation) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [set_current_discourse_context_from_target_representation(TargetRepresentation)]),
	fail.

%---------------------------------------------------------------

print_other_translation_info(List) :-
	print_other_translation_info1(List, List).

print_other_translation_info1([], _All).
print_other_translation_info1([F | R], All) :-
	print_other_translation_info_item(F, All),
	print_other_translation_info1(R, All).

% Don't print all_generation_tuples or generated_tree
print_other_translation_info_item(all_generation_tuples = _Val, _All) :-
	!.
print_other_translation_info_item(generated_tree = _Val, _All) :-
	!.
% Only print instantiated_target if it's different from target_representation
print_other_translation_info_item(instantiated_target = Instantiated, All) :-
	member(target_representation = Uninstantiated, All),
	(   safe_subsumes_chk(Instantiated, Uninstantiated) ->
	    true
	;
	    prettyprint_key_val_pair(instantiated_target, Instantiated)
	),
	!.
print_other_translation_info_item(Key = Val, _All) :-
	atomic(Key),
	prettyprint_key_val_pair(Key, Val),
	!.
print_other_translation_info_item(Item, _All) :-
	format('~N   ~w~n', [Item]).

%----------------------------------------------------------------------

show_generation_tuples([]) :-
	!,
	format('~N~nNothing generated...~n', []).
show_generation_tuples([SingleTuple]) :-
	!,
	show_generation_tuple(SingleTuple).
show_generation_tuples(Tuples) :-
	show_generation_tuples1(Tuples, 1).

show_generation_tuples1([], _N).
show_generation_tuples1([F | R], I) :-
	format('~N~n================================================================', []),
	format('~NPossibility ~d', [I]),
	show_generation_tuple(F),
	I1 is I + 1,
	!,
	show_generation_tuples1(R, I1).

show_generation_tuple(Tuple) :-
	(   member(words=Words, Tuple) ->
	    
	    join_with_spaces(Words, WordsAtom),
	    format('~N~nWords: ~w', [WordsAtom]) ;

	    true
	),
	(   member(tagged_words=TaggedWords, Tuple) ->
	    
	    format('~N~nTagged words: ~w', [TaggedWords]) ;

	    true
	),
	(   member(preference_score=Score, Tuple) ->

	    format('~N~nPreference score: ~2f', [Score]) ;

	    true
	),
	(   ( member(preference_info=PreferenceInfo, Tuple), PreferenceInfo \== [] ) ->

	    format('~N~nPreference info:~n', []),
	    show_generation_preference_info(PreferenceInfo) ;

	    true
	),
	(   member(tree=Tree, Tuple) ->

	    format('~N~nGenerated tree:~n~n', []),
	    get_regulus_runtime_config(print_line_info, PrintLineInfo),
	    prettyprint_parse_tree(Tree, PrintLineInfo) ;

	    true
	),
	!.
show_generation_tuple(Tuple) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [show_generation_tuple(Tuple)]),
	fail.

show_generation_preference_info([]).
show_generation_preference_info([F | R]) :-
	show_generation_preference_info_item(F),
	!,
	show_generation_preference_info(R).

show_generation_preference_info_item(Pattern-Score) :-
	copy_term(Pattern, Pattern1),
	make_ground(Pattern1),
	(   integer(Score) ->
	    Format = '~N~d~8| ~w~n' ;
	    
	    float(Score) ->
	    Format = '~N~2f~8| ~w~n' ;
	
	    otherwise ->
	    fail
	),
	format(Format, [Score, Pattern]),
	!.
show_generation_preference_info_item(Other) :-
	format2error('~N~n*** Error: bad call: ~w~n',
		     [show_generation_preference_info_item(Other)]),
	fail.

%---------------------------------------------------------------

atom_to_interlingua_and_tree(Atom, GrammarAtom, ContextIn, ContextOut, Interlingua, InterlinguaTree) :-
	split_atom_into_words(Atom, WordList),
	words_to_lf_and_tree_with_current_parser(WordList, GrammarAtom, LF, _SourceTree),
	transfer_representation_to_surface_interlingua_using_context(LF, ContextIn, ContextOut, Interlingua, InterlinguaTree),
	!.

%---------------------------------------------------------------

words_to_lf_and_tree_with_current_parser(WordList, GrammarAtom, LF, Tree) :-
	parse_with_current_parser(GrammarAtom, WordList, Tree, _SynFeats, Local, Global),
	real_lf_out_of_local_and_global(Local, Global, LF),
	check_for_bad_lf(LF).

%---------------------------------------------------------------

sent_atom_is_in_coverage_according_to_current_parser(SentAtom) :-
	split_atom_into_words(SentAtom, WordList),
	get_regulus_config_item(top_level_cat, TopLevelCat),
	parse_with_current_parser(TopLevelCat, WordList, _Tree, _SynFeats, _SemValue, _Global),
	!.
	
%---------------------------------------------------------------

check_for_bad_lf(LF) :-
	current_predicate(bad_semantic_triples, bad_semantic_triples(_)),
	!,
	\+ bad_triple_in_lf(LF, _Triple).
check_for_bad_lf(_LF) :-
	!.

bad_triple_in_lf(LF, Triple) :-
	findall(Triple, sem_triple_in_lf(LF, Triple), Triples),
	bad_semantic_triples(Triples),
	!.

%---------------------------------------------------------------

parse_with_current_parser(_GrammarAtom, lf(LF), Tree, SynFeatVals, Local, Global) :-
	!,
	Tree = no_tree,
	SynFeatVals = [],
	Local = LF,
	Global = [].
parse_with_current_parser(GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	(   get_regulus_runtime_config(parser, ParserType) ->
	    parse_with_specified_parser(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global)
	;
	    format2error('~NError: no current parser defined~n', []),
	    fail
	).

parse_with_current_parser1(GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	(   get_regulus_runtime_config(parser, ParserType) ->
	    parse_with_specified_parser1(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global)
	;
	    format2error('~NError: no current parser defined~n', []),
	    fail
	).

parse_with_specified_parser(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	findall([Tree0, SynFeatVals0, Local1, Global0],
		(   parse_with_specified_parser1(ParserType, GrammarAtom, WordList, Tree0, SynFeatVals0, Local0, Global0),
		    unpack_strcat_semantics_if_necessary(Local0, Local1)
		),
		Tuples),
	reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples),
	!,
	member([Tree, SynFeatVals, Local, Global], ReorderedTuples).

parse_with_specified_parser1(ParserType, _GrammarAtom, _WordList, Tree, SynFeatVals, Local, Global) :-
	ParserType = trivial,
	!,
	Tree = no_tree,
	SynFeatVals = [],
	Local = [],
	Global = [].
parse_with_specified_parser1(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	ParserType = dcg,
	!,
	regulus_dcg_parse(GrammarAtom, WordList, Tree, SynFeatVals, Local, Global).
parse_with_specified_parser1(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	ParserType = lc,
	!,
	regulus_lc_parse(GrammarAtom, WordList, Tree, SynFeatVals, Local, Global).
parse_with_specified_parser1(ParserType, GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	ParserType = nuance,
	!,
	nuance_parse(GrammarAtom, WordList, LF),
	Tree = no_tree,
	SynFeatVals = [],
	Local = LF,
	Global = [].
parse_with_specified_parser1(ParserType, _GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	ParserType = surface,
	!,
	surface_patterns_parse(WordList, SemanticAtoms),
	Tree = no_tree,
	SynFeatVals = [],
	Local = SemanticAtoms,
	Global = [].

%---------------------------------------------------------------

reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples) :-
	tag_tuples_with_preference_scores(Tuples, ScoredTuples),
	keysort(ScoredTuples, ReorderedScoredTuples),
	unkey_list(ReorderedScoredTuples, ReorderedTuples0),
	reverse(ReorderedTuples0, ReorderedTuples),
	!.
reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples)]),
	fail.

tag_tuples_with_preference_scores([], []).
tag_tuples_with_preference_scores([F | R], [Score-F | R1]) :-
	preference_score_for_tuple(F, Score),
	!,
	tag_tuples_with_preference_scores(R, R1).

preference_score_for_tuple([Tree, _SynFeatVals, Local, Global], Score) :-
	real_lf_out_of_local_and_global(Local, Global, LF0),
	make_transfer_representation_canonical_and_unpack_if_necessary(LF0, LF),
	parse_tree_to_summary(Tree, TreeSummary),
	preference_score_for_lf_and_tree([lf=LF, full_tree=Tree, tree=TreeSummary], Score, _Trace).

%---------------------------------------------------------------

present_regulus_dcg_parses([]) :-
	!,
	format('~N~nNo parse...~n', []).	
present_regulus_dcg_parses([SingleTuple]) :-
	!,
	present_single_regulus_dcg_parse(SingleTuple).
present_regulus_dcg_parses(List) :-
	length(List, N),
	format('~N~n~d possibilities:', [N]),
	present_regulus_dcg_parses1(List, 1).

present_regulus_dcg_parses1([], _N).
present_regulus_dcg_parses1([F | R], I) :-
	format('~N~n----------------------------------------------------------------', []),
	format('~NPossibility ~d', [I]),
	present_single_regulus_dcg_parse(F),
	I1 is I + 1,
	!,
	present_regulus_dcg_parses1(R, I1).

present_single_regulus_dcg_parse([Tree, _SynFeats, Local, Global]) :-
	term_contains_functor(Tree, dummy_top/0),
	% tree_to_item/2 (in stepper_item_db) strips off the dummy top-level rules.
	% Then we extract the real tree and syn feats from the item.
	tree_to_item(Tree, Item),
	tree_for_item(Item, RealTree),
	syn_feats_for_item(Item, RealSynFeats),
	!,
	present_single_regulus_dcg_parse([RealTree, RealSynFeats, Local, Global]).
present_single_regulus_dcg_parse([Tree, SynFeats, Local, Global]) :-
	print_form_for_syn_feats(SynFeats, PrintSynFeats),
	numbervars(PrintSynFeats, 0, _),
	%format('~N~nReturn value: ~w', [Local]),
	make_transfer_representation_canonical_and_unpack_if_necessary(Local, Local1),
	nl, prettyprint_term_with_intro('Return value', Local1),
	%format('~N~nGlobal value: ~w~n', [Global]),
	make_transfer_representation_canonical_and_unpack_if_necessary(Global, Global1),
	nl, prettyprint_term_with_intro('Global value', Global1),
	nl, prettyprint_term_with_intro('Syn features', PrintSynFeats),
	format('~N~nParse tree:~n~n', []),
	get_regulus_runtime_config(print_line_info, PrintLineInfo),
	prettyprint_parse_tree(Tree, PrintLineInfo),
	maybe_print_tree_summary(Tree),
	maybe_present_semantic_triples(Local, Global),
	maybe_present_preference_scores(Local, Global, Tree),
	maybe_present_categories_for_tree(Tree).

%---------------------------------------------------------------

maybe_print_tree_summary(Tree) :-
	get_regulus_runtime_config(print_tree_summary, print_tree_summary),
	parse_tree_to_summary(Tree, Summary),
	format('~N~nSummary:~n~n', []),
	prettyprintq(Summary),
	format('~N', []),
	!.
maybe_print_tree_summary(_Tree).

%---------------------------------------------------------------

maybe_present_categories_for_tree(Tree) :-
	get_regulus_runtime_config(print_tree_categories, print_tree_categories),
	parse_tree2categories(Tree, Categories),
	format('~N~nCategories:~n~n', []),
	prettyprintq(Categories),
	format('~N', []),
	!.
maybe_present_categories_for_tree(_Tree).

%---------------------------------------------------------------

maybe_present_semantic_triples(Local, Global) :-
	regulus_config(lf_postproc_pred, riacs_postproc_lf),
	real_lf_out_of_local_and_global(Local, Global, LF),
	findall(Triple, sem_triple_in_lf(LF, Triple), Triples),
	format('~N~nSemantic triples: ~w~n', [Triples]),
	(   ( current_predicate(bad_semantic_triples, bad_semantic_triples(_)), bad_semantic_triples(Triples) ) ->
	    format('~N*** Bad semantic triples according to parse preferences ***~n', []) ;
	    true
	),
	!.
maybe_present_semantic_triples(_Local, _Global).
	
%---------------------------------------------------------------

maybe_present_preference_scores(_Local, _Global, Tree) :-
	\+ current_predicate(user:parse_preference_score/2),
	\+ frequency_preference_information_available_in_term(Tree),
	!.
maybe_present_preference_scores(Local, Global, Tree) :-
	real_lf_out_of_local_and_global(Local, Global, LF),
	parse_tree_to_summary(Tree, TreeSummary),
	make_transfer_representation_canonical_and_unpack_if_necessary(LF, LF1),
	preference_score_for_lf_and_tree([lf=LF1, full_tree=Tree, tree=TreeSummary], Score, Trace),
	present_preference_scores(Trace, Score).

present_preference_scores(Trace, _Score) :-
	Trace = [],
	!,
	format('~N~nNo preferences apply~n', []).
present_preference_scores(Trace, Score) :-
	format('~N~nPreference information:~n~n', []),
	present_preference_trace(Trace),
	format('~NTotal preference score: ~2f~n', [Score]).

present_preference_trace([]).
present_preference_trace([F | R]) :-
	present_preference_trace_item(F),
	!,
	present_preference_trace(R).

present_preference_trace_item(rule_frequency-Score) :-
	format('~N~2f~6|~w~n', [Score, 'Rule frequency score']),
	!.
present_preference_trace_item(Pattern-Score) :-
	copy_term(Pattern, Pattern1),
	make_ground(Pattern1),
	format('~N~d~6|~w~n', [Score, Pattern1]),
	!.
present_preference_trace_item(Other) :-
	format2error('~N*** Error: bad call: ~w~n', [present_preference_trace_item(Other)]),
	fail.

%---------------------------------------------------------------

present_generated_paraphrases([], _OriginalSemValue) :-
	!,
	format('~N~nNothing generated...~n', []).	
present_generated_paraphrases([SingleTuple], OriginalSemValue) :-
	!,
	present_single_generated_paraphrase(SingleTuple, OriginalSemValue).
present_generated_paraphrases(List0, OriginalSemValue) :-
	order_generated_paraphrase_list(List0, List),
	length(List, N),
	format('~N~n~d possibilities:', [N]),
	present_generated_paraphrases1(List, 1, OriginalSemValue).

present_generated_paraphrases1([], _N, _OriginalSemValue).
present_generated_paraphrases1([F | R], I, OriginalSemValue) :-
	format('~N~n----------------------------------------------------------------', []),
	format('~NPossibility ~d', [I]),
	present_single_generated_paraphrase(F, OriginalSemValue),
	I1 is I + 1,
	!,
	present_generated_paraphrases1(R, I1, OriginalSemValue).

present_single_generated_paraphrase([Words, Tree, SemValue], OriginalSemValue) :-
	join_with_spaces(Words, WordsAtom),
	format('~N~nWords: ~w', [WordsAtom]),
	(   safe_subsumes_chk(SemValue, OriginalSemValue) ->
	    true ;
	    format('~N~nSem value: ~w', [SemValue])
	),
	parse_tree_to_tagged_word_list(Tree, TaggedWords),
	format('~N~nTagged words: ~w', [TaggedWords]),
	(   frequency_preference_information_available_in_term(Tree) ->
	    (   rule_frequency_preference_score([full_tree=Tree], Score),
		format('~N~nRule frequency score: ~2f', [Score])
	    )
	;
	    otherwise ->
	    true
	),		
	format('~N~nGenerated tree:~n~n', []),
	get_regulus_runtime_config(print_line_info, PrintLineInfo),
	prettyprint_parse_tree(Tree, PrintLineInfo).

%---------------------------------------------------------------

order_generated_paraphrase_list(List, List) :-
	\+ frequency_preference_information_available_in_term(List),
	!.
order_generated_paraphrase_list(List, SortedList) :-
	tag_generated_paraphrase_list_with_frequency_information(List, TaggedList),
	keysort(TaggedList, TaggedList1),
	reverse(TaggedList1, TaggedList2),
	unkey_list(TaggedList2, SortedList),
	!.
order_generated_paraphrase_list(List, SortedList) :-
	format2error('~N*** Error: bad call: ~w~n', [order_generated_paraphrase_list(List, SortedList)]),
	fail.

tag_generated_paraphrase_list_with_frequency_information([], []).
tag_generated_paraphrase_list_with_frequency_information([F | R], [F1 | R1]) :-
	tag_generated_paraphrase_item_with_frequency_information(F, F1),
	!,
	tag_generated_paraphrase_list_with_frequency_information(R, R1).

tag_generated_paraphrase_item_with_frequency_information(F, Score-F) :-
	F = [_Words, Tree, _SemValue],
	rule_frequency_preference_score(Tree, Score),
	!.

%---------------------------------------------------------------

atom_to_regulus_dcg_parse(lf(LF), _GrammarAtom, LF) :-
	!.
atom_to_regulus_dcg_parse(Atom, GrammarAtom, Parse) :-
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, WordList),
	regulus_dcg_parse(GrammarAtom, WordList, _SynFeats, _Local, Global),
	Global = [value=Parse].

%---------------------------------------------------------------

atom_to_parse_using_current_parser(lf(LF), _GrammarAtom, LF) :-
	!.
atom_to_parse_using_current_parser(Atom, GrammarAtom, Parse) :-
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, WordList),
	parse_with_current_parser(GrammarAtom, WordList, _Tree, _SynFeats, Local, Global),
	(   Global = [_Feat=Parse] ->
	    true ;
	    Parse = Local
	).

%---------------------------------------------------------------

atom_to_original_script_and_gloss(Atom, OriginalScriptAtom, GlossAtom) :-
	grammar_is_loaded,
	current_predicate(recogniser_generator_original_script:generate/3),
	current_predicate(recogniser_generator_gloss:generate/3),
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, WordList),
	parse_with_specified_parser(lc, '.MAIN', WordList, Tree, _SynFeatVals, Local, _Global),
	recogniser_generate_original_script_result(Local, Tree, OriginalScriptAtom),
	recogniser_generate_gloss_result(Local, Tree, GlossAtom),
	!.

atom_to_original_script(Atom, OriginalScriptAtom) :-
	grammar_is_loaded,
	current_predicate(recogniser_generator_original_script:generate/3),
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, WordList),
	parse_with_specified_parser(lc, '.MAIN', WordList, Tree, _SynFeatVals, Local, _Global),
	recogniser_generate_original_script_result(Local, Tree, OriginalScriptAtom),
	!.

atom_to_gloss(Atom, GlossAtom) :-
	grammar_is_loaded,
	current_predicate(recogniser_generator_gloss:generate/3),
	atom_codes(Atom, Chars),
	split_string_into_words(Chars, WordList),
	parse_with_specified_parser(lc, '.MAIN', WordList, Tree, _SynFeatVals, Local, _Global),
	recogniser_generate_gloss_result(Local, Tree, GlossAtom),
	!.

%---------------------------------------------------------------

regulus_dcg_parse(StartSymbol, WordList, SynFeats, Local, Global) :-
	regulus_dcg_parse(StartSymbol, WordList, _Tree, SynFeats, Local, Global).

regulus_dcg_parse(StartSymbol, WordList, Tree, SynFeats, Local, Global) :-
	Goal =.. [StartSymbol, Tree, SynFeats, Local0, Global0, WordList, []],
	call(Goal),

	close_list(Global0),
	regulus_eval_text(Global0, Global),

	regulus_eval_text(Local0, Local1),
	(   var(Local1) ->
	    Local = '(none)' ;
	    Local = Local1
	).

%---------------------------------------------------------------

regulus_lc_parse(GrammarAtom, WordList, Tree, SynFeatVals, Local, Global) :-
	non_ignored_synfeats_for_cat(GrammarAtom, SynFeats),
	length(SynFeats, NSynFeats),
	length(SynVals, NSynFeats),
	(   NSynFeats = 0 ->
	    SynCat =.. [GrammarAtom, no_args] ;
	    SynCat =.. [GrammarAtom | SynVals]
	),
	lc_parse(SynCat/_, WordList, [_/[LF0, Tree], _]),
	feats_and_vals_to_feat_vals(SynFeats, SynVals, SynFeatVals),
	regulus_eval_text(LF0, Local0),
	(   var(Local0) ->
	    Local = '(none)' ;
	    Local = Local0
	),
	Global = [].

%---------------------------------------------------------------

%nuance_parse(GrammarAtom, WordList, LF) :-
%	join_with_spaces(WordList, SentAtom),
%	nuance_parse_as_described_by_config_file(SentAtom, GrammarAtom, Result),
%	Result = recognition_succeeded(_Conf, _Str, NLValue),
%	NLValue = [_Key=LF0],
%	regulus_eval_speech(LF0, LF),
%	!.

nuance_parse(_GrammarAtom, WordList, LF) :-
	join_with_spaces(WordList, SentAtom),
	nl_tool_parse_as_described_by_config_file(SentAtom, Result),
	Result = [_Key=LF0],
	regulus_eval_speech(LF0, LF).
	
%---------------------------------------------------------------

real_lf_out_of_local_and_global(Local, Global, LF) :-
	real_lf_out_of_local_and_global1(Local, Global, LF0),
	(   var(LF0) ->
	    LF = null
	;
	    otherwise ->
	    LF = LF0
	).

real_lf_out_of_local_and_global1(_Local, Global, LF) :-
	nonvar(Global),
	dif(Global, []),
	LF = Global,
	!.
real_lf_out_of_local_and_global1(Local, _Global, LF) :-
	LF = Local.

%---------------------------------------------------------------

report_unknown_words_in_sent_atom(SentAtom) :-
	split_atom_into_words(SentAtom, WordList),
	unknown_words_in_word_list(WordList, UnknownWords),
	UnknownWords \== [],
	format('~NWords not in current vocabulary: ~w~n', [UnknownWords]),
	!.
report_unknown_words_in_sent_atom(_SentAtom).	

sent_atom_is_in_vocabulary_according_to_current_parser(SentAtom) :-
	split_atom_into_words(SentAtom, WordList),
	unknown_words_in_word_list1(WordList, OOVWords),
	!,
	OOVWords = [].

% First word is allowed to be a category name
unknown_words_in_word_list([F | R], Unknown) :-
	(   category_internal(F, _Feats) ->
	    unknown_words_in_word_list1(R, Unknown)
	;
	    otherwise ->
	    unknown_words_in_word_list1([F | R], Unknown)
	).	

unknown_words_in_word_list1([], []).
unknown_words_in_word_list1([F | R], [F | R1]) :-
	\+ regulus_preds:vocabulary_item(F),
	!,
	unknown_words_in_word_list1(R, R1).
unknown_words_in_word_list1([_F | R], R1) :-
	!,
	unknown_words_in_word_list1(R, R1).

%---------------------------------------------------------------

load_regulus_dcg_file(File) :-
	load_regulus_dcg_file(File, user).

load_regulus_dcg_file(File, Module) :-
	%safe_compile_with_redefine_warnings_off(user, File).
	load_file_without_warning_about_singleton_vars_discontiguous_rules_or_redefines(File, Module).

/*
load_regulus_dcg_file(File) :-
	set_prolog_flags_for_dcg_compilation(OldFlags),
	on_exception(
	_Exception,
	safe_compile(user, File),
	restore_prolog_flags_after_dcg_compilation(OldFlags)
    ),
	restore_prolog_flags_after_dcg_compilation(OldFlags).

set_prolog_flags_for_dcg_compilation(OldFlags) :-
	prolog_flag(redefine_warnings, OldRedefine, off),
	OldFlags = [OldRedefine].

restore_prolog_flags_after_dcg_compilation(OldFlags) :-
	OldFlags = [OldRedefine],
	prolog_flag(redefine_warnings, _Current, OldRedefine).
*/

%---------------------------------------------------------------

load_all_translation_corpus_judgement_files :-
	(   regulus_config(translation_corpus_judgements, JudgementsFile1) ->
	    load_translation_corpus_judgements_file(user, JudgementsFile1) ;
	    true
	),
	(   regulus_config(to_interlingua_translation_corpus_judgements, JudgementsFile2) ->
	    load_translation_corpus_judgements_file(to_interlingua_judgements, JudgementsFile2) ;
	    true
	),
	(   regulus_config(from_interlingua_translation_corpus_judgements, JudgementsFile3) ->
	    load_translation_corpus_judgements_file(from_interlingua_judgements, JudgementsFile3) ;
	    true
	),
	!.

load_translation_corpus_judgements_file(JudgementsFile) :-
	load_translation_corpus_judgements_file(user, JudgementsFile).

load_translation_corpus_judgements_file(Module, JudgementsFile) :-
	abolish_if_defined(Module:judged_translation/2),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	file_exists(AbsJudgementsFile, read),
	safe_compile(Module, AbsJudgementsFile),
	format('~NLoaded translation judgements into ~w from ~w~n', [Module, AbsJudgementsFile]),
	!.
load_translation_corpus_judgements_file(Module, JudgementsFile) :-
	format('~N~n*** Warning: unable to load translation judgements file into ~w from ~w~n',
	       [Module, JudgementsFile]).

%---------------------------------------------------------------

:- dynamic preceding_context/2.

load_wavfile_context_file(WavfileContextFile) :-
	safe_absolute_file_name(WavfileContextFile, AbsWavfileContextFile),
	file_exists(AbsWavfileContextFile, read),
	prolog_file_to_list(AbsWavfileContextFile, List),
	load_wavfile_context_list(List),
	length(List, N),
	format('~NLoaded wavfile context file (~d items) from ~w~n', [N, AbsWavfileContextFile]),
	!.
load_wavfile_context_file(WavfileContextFile) :-
	format2error('~N~n*** Error: unable to load wavfile context file ~w~n', [WavfileContextFile]).

load_wavfile_context_list([]).
load_wavfile_context_list([F | R]) :-
	load_wavfile_context_record(F),
	!,
	load_wavfile_context_list(R).

load_wavfile_context_record(preceding_context(X, Y)) :-
	assertz(preceding_context(X, Y)),
	!.
load_wavfile_context_record(Other) :-
	format2error('~N*** Error: bad element in wavfile context file: ~q~n', [Other]),
	fail.

%---------------------------------------------------------------

load_translation_corpus_recognition_judgements_file(JudgementsFile) :-
	abolish_if_defined(user:judged_recognition/3),
	safe_absolute_file_name(JudgementsFile, AbsJudgementsFile),
	file_exists(AbsJudgementsFile, read),
	safe_compile(user, AbsJudgementsFile),
	format('~NLoaded recognition judgements from ~w~n', [AbsJudgementsFile]),
	!.
load_translation_corpus_recognition_judgements_file(JudgementsFile) :-
	format('~N~n*** Warning: unable to load recognition judgements file ~w~n', [JudgementsFile]).

%---------------------------------------------------------------

load_parse_preferences_file_if_there_is_one :-
	%abolish_if_defined(user:bad_semantic_triples/1),
	%abolish_if_defined(user:parse_preference_score/2),
	regulus_config(parse_preferences, File),
	!,
	load_parse_preferences_file(File).
load_parse_preferences_file_if_there_is_one :-
	format('~N~nNo parse preferences file defined.~n', []).

load_parse_preferences_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	safe_file_exists(AbsFile),
	%safe_compile(user, AbsFile),
	read_and_compile_generic_regulus_related_file(File, user),
	format('~N~nLoaded parse preferences from ~w~n', [AbsFile]),
	!.
load_parse_preferences_file(File) :-
	format('~N*** Warning: unable to load parse preferences file ~w~n', [File]).

%---------------------------------------------------------------

load_nbest_preferences_file_if_there_is_one :-
	abolish_if_defined(nbest_features:feature_weight/2),
	abolish_if_defined(nbest_features:feature_value_for_record/3),
	regulus_config(nbest_preferences, File),
	!,
	load_nbest_preferences_file(File).
load_nbest_preferences_file_if_there_is_one :-
	format('~N~nNo N-best preferences file defined.~n', []).

load_nbest_preferences_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	file_exists(AbsFile, read),
	safe_compile_with_redefine_warnings_off(nbest_features, AbsFile),
	format('~N~nLoaded N-best preferences from ~w~n', [AbsFile]),
	!.
load_nbest_preferences_file(File) :-
	format('~N*** Warning: unable to load N-best preferences file ~w~n', [File]).
 
%---------------------------------------------------------------

load_alternate_generation_preferences_file_if_there_is_one :-
	findall([Module, File],
		regulus_config(alternate_generation_preferences(Module), File),
		Pairs),
	load_alternate_generation_preferences_files(Pairs),
	!.
load_alternate_generation_preferences_file_if_there_is_one.

load_alternate_generation_preferences_files([]).
load_alternate_generation_preferences_files([F | R]) :-
	load_alternate_generation_preferences_file(F),
	!,
	load_alternate_generation_preferences_files(R).

load_alternate_generation_preferences_file([Module, File]) :-
	load_generation_preferences_file(File, Module).
	
%---------------------------------------------------------------

load_generation_preferences_file_if_there_is_one :-
	regulus_config(generation_preferences, File),
	!,
	load_generation_preferences_file(File, user).
load_generation_preferences_file_if_there_is_one :-
	format('~N~nNo generation preferences file defined.~n', []).

load_generation_preferences_file(File, Module) :-
	safe_absolute_file_name(File, AbsFile),
	safe_file_exists(AbsFile),
	read_and_compile_generic_regulus_related_file(AbsFile, Module),
	format('~N~nLoaded generation preferences from ~w into module ~w~n', [AbsFile, Module]),
	!.
load_generation_preferences_file(File, Module) :-
	format('~N*** Warning: unable to load generation preferences file ~w into module ~w~n',
	       [File, Module]).

%---------------------------------------------------------------

load_resolution_preferences_file_if_there_is_one :-
	regulus_config(resolution_preferences, File),
	!,
	load_resolution_preferences_file(File).
load_resolution_preferences_file_if_there_is_one :-
	format('~N~nNo resolution preferences file defined.~n', []).

load_resolution_preferences_file(File) :-
	abolish_if_defined(user:resolution_preference/2),
	get_regulus_config_item(resolution_preferences, File),
	safe_compile(user, File),
	!.
load_resolution_preferences_file(File) :-
	format('~N*** Warning: unable to load resolution preferences file ~w~n', [File]).

%---------------------------------------------------------------

load_transfer_rules_file_if_there_is_one :-
	abolish_if_defined(user:transfer_rule/6),
	abolish_if_defined(user:role_transfer_rule/4),
	abolish_if_defined(user:transfer_lexicon/4),
	regulus_config(transfer_rules, File),
	!,
	load_transfer_rules_file(File).
load_transfer_rules_file_if_there_is_one :-
	format('~N~nNo transfer rules file defined.~n', []).

load_transfer_rules_file(File) :-
	get_regulus_config_item(compiled_transfer_rules, CompiledFile),
	compile_transfer_file_or_files(File, CompiledFile, direct),
	safe_compile(user, CompiledFile),
	!.
load_transfer_rules_file(File) :-
	format('~N*** Warning: unable to load transfer rules file ~w~n', [File]).

%---------------------------------------------------------------

%load_interlingua_declarations_file_if_there_is_one :-
%	abolish_if_defined(user:interlingua_constant/1),
%	abolish_if_defined(user:deprecated_interlingua_constant/1),	
%	regulus_config(interlingua_declarations, File),
%	!,
%	load_interlingua_declarations_file(File).
%load_interlingua_declarations_file_if_there_is_one :-
%	format('~N~nNo interlingua declaration file defined.~n', []).
%
%load_interlingua_declarations_file(File) :-
%	safe_compile(user, File),	
%	!.
%load_interlingua_declarations_file(File) :-
%	format('~N*** Warning: unable to load interlingua declarations file ~w~n', [File]).

load_interlingua_declarations_file_if_there_is_one :-
	abolish_if_defined(user:interlingua_constant/1),
	abolish_if_defined(user:deprecated_interlingua_constant/1),
	findall(File,
		regulus_config(interlingua_declarations, File),
		Files),
	Files \== [],
	!,
	load_interlingua_declarations_files(Files).
load_interlingua_declarations_file_if_there_is_one :-
	format('~N~nNo interlingua declaration file defined.~n', []).

load_interlingua_declarations_files(Files) :-
	get_regulus_config_item(tmp_interlingua_declarations_file, CombinedFile),
	combine_interlingua_declaration_files(Files, CombinedFile),
	safe_compile(user, CombinedFile),
	!.
load_interlingua_declarations_files(Files) :-
	format('~N*** Warning: unable to load interlingua declarations files ~w~n', [Files]).

combine_interlingua_declaration_files(Files, CombinedFile) :-
	prolog_file_or_files_to_list(Files, List),
	length(List, NIn),
	format('~N--- Read interlingua files (~d elements): ~w~n', [NIn, Files]),
	
	findall((:- (Body)),
		member((:- (Body)), List),
		Decls0),
	sort(Decls0, Decls),
	findall(NormalClause,
		(   member(NormalClause, List),
		    functor(NormalClause, F, N),
		    F/N \== ((:-)/1),
		    make_ground(NormalClause)
		),
		NormalClauses0),
	sort(NormalClauses0, NormalClauses),
	
	append(Decls, NormalClauses, AllContent),
	length(Decls, NDecls),
	length(NormalClauses, NNormalClauses),
	safe_absolute_file_name(CombinedFile, AbsCombinedFile),
	list_to_prolog_file(AllContent, AbsCombinedFile),
	format('~N--- Written combined interlingua file (~d declarations, ~d clauses): ~w~n',
	       [NDecls, NNormalClauses, AbsCombinedFile]),
	!.

%---------------------------------------------------------------

create_filtered_interlingua_declarations_file_if_relevant :-
	\+ regulus_config(interlingua_declarations, _File),
	!.
create_filtered_interlingua_declarations_file_if_relevant :-
	regulus_config(interlingua_declarations, CurrentFile),
	get_regulus_config_item(filtered_interlingua_declarations_file, FilteredFile),
	create_filtered_interlingua_declarations_file(CurrentFile, FilteredFile),
	!.
create_filtered_interlingua_declarations_file_if_relevant :-
	format('~N*** Warning: unable to create filtered interlingua declarations file~n', []).

%---------------------------------------------------------------

load_to_source_discourse_rules_file_if_there_is_one :-
	abolish_if_defined(user:to_source_discourse_rule/6),
	abolish_if_defined(user:to_source_discourse_role_rule/4),
	abolish_if_defined(user:to_source_discourse_lexicon/4),
	fail.
load_to_source_discourse_rules_file_if_there_is_one :-
	regulus_config(to_source_discourse_rules(Module), File),
	load_to_source_discourse_rules_file(File, Module),
	fail.
load_to_source_discourse_rules_file_if_there_is_one :-
	regulus_config(to_source_discourse_rules, File),
	!,
	load_to_source_discourse_rules_file(File, user).
load_to_source_discourse_rules_file_if_there_is_one :-
	format('~N~nNo to_source_discourse rules file defined.~n', []).

load_to_source_discourse_rules_file(File, Module) :-
	abolish_if_defined(Module:to_source_discourse_rule/6),
	abolish_if_defined(Module:to_source_discourse_role_rule/4),
	abolish_if_defined(Module:to_source_discourse_lexicon/4),
	get_regulus_config_item(compiled_to_source_discourse_rules(Module), CompiledFile),
	compile_transfer_file_or_files(File, CompiledFile, to_source_discourse),
	safe_compile_with_discontiguous_warnings_off(Module, CompiledFile),
	set_transfer_resources_defined_for_module(Module),
	format('~N--- Compiled to-source-discourse rules file ~w in package ~w~n', [CompiledFile, Module]),
	!.
load_to_source_discourse_rules_file(File, _Module) :-
	format('~N*** Warning: unable to load to_source_discourse rules file ~w~n', [File]).

%---------------------------------------------------------------	

load_to_interlingua_rules_file_if_there_is_one :-
	abolish_if_defined(user:to_interlingua_rule/6),
	abolish_if_defined(user:to_interlingua_role_rule/4),
	abolish_if_defined(user:to_interlingua_lexicon/4),
	fail.
load_to_interlingua_rules_file_if_there_is_one :-
	regulus_config(to_interlingua_rules(Module), File),
	load_to_interlingua_rules_file(File, Module),
	fail.
load_to_interlingua_rules_file_if_there_is_one :-
	regulus_config(to_interlingua_rules, File),
	!,
	load_to_interlingua_rules_file(File, user).
load_to_interlingua_rules_file_if_there_is_one :-
	format('~N~nNo to_interlingua rules file defined.~n', []).

load_to_interlingua_rules_file(File, Module) :-
	abolish_if_defined(Module:to_interlingua_rule/6),
	abolish_if_defined(Module:to_interlingua_role_rule/4),
	abolish_if_defined(Module:to_interlingua_lexicon/4),
	get_regulus_config_item(compiled_to_interlingua_rules(Module), CompiledFile),
	compile_transfer_file_or_files(File, CompiledFile, to_interlingua),
	safe_compile_with_discontiguous_warnings_off(Module, CompiledFile),
	set_transfer_resources_defined_for_module(Module),
	format('~N--- Compiled to-interlingua rules file ~w in package ~w~n', [CompiledFile, Module]),
	!.
load_to_interlingua_rules_file(File, _Module) :-
	format('~N*** Warning: unable to load to_interlingua rules file ~w~n', [File]).

%---------------------------------------------------------------

load_from_interlingua_rules_file_if_there_is_one :-
	abolish_if_defined(user:from_interlingua_rule/6),
	abolish_if_defined(user:from_interlingua_role_rule/4),
	abolish_if_defined(user:from_interlingua_lexicon/4),
	fail.
load_from_interlingua_rules_file_if_there_is_one :-
	regulus_config(from_interlingua_rules(Module), File),
	load_from_interlingua_rules_file(File, Module),
	fail.
load_from_interlingua_rules_file_if_there_is_one :-
	regulus_config(from_interlingua_rules, File),
	!,
	load_from_interlingua_rules_file(File, user).
load_from_interlingua_rules_file_if_there_is_one :-
	format('~N~nNo from_interlingua rules file defined.~n', []).

load_from_interlingua_rules_file(File, Module) :-
	abolish_if_defined(Module:from_interlingua_rule/6),
	abolish_if_defined(Module:from_interlingua_role_rule/4),
	abolish_if_defined(Module:from_interlingua_lexicon/4),
	get_regulus_config_item(compiled_from_interlingua_rules(Module), CompiledFile),
	compile_transfer_file_or_files(File, CompiledFile, from_interlingua),
	safe_compile_with_discontiguous_warnings_off(Module, CompiledFile),
	set_transfer_resources_defined_for_module(Module),
	format('~N--- Compiled from-interlingua rules file ~w in package ~w~n', [CompiledFile, Module]),
	!.
load_from_interlingua_rules_file(File) :-
	format('~N*** Warning: unable to load from_interlingua rules file ~w~n', [File]).

%---------------------------------------------------------------

compile_transfer_file_or_files(FileOrFiles, CompiledFile, Type) :-
	get_transfer_direction_from_file_or_files(FileOrFiles, FileOrFiles1, Direction),
	get_transfer_representation(Representation),
	compile_transfer_file_or_files(FileOrFiles1, CompiledFile, Type, Direction, Representation).

get_transfer_direction_from_file_or_files(Files, Files1, Direction) :-
	is_list(Files),
	member(transfer_direction(Direction), Files),
	delete(Files, transfer_direction(Direction), Files1),
	!.
get_transfer_direction_from_file_or_files(FileOrFiles, FileOrFiles, forward) :-
	!.

get_transfer_representation(role_marked_linear) :-
	regulus_config(role_marked_semantics, yes),
	!.
get_transfer_representation(nested) :-
	regulus_config(nested_semantics, yes),
	!.
get_transfer_representation(linear).

%---------------------------------------------------------------

load_graphical_collocation_rules_file_if_there_is_one :-
	abolish_if_defined(user:better_collocation_internal/2),
	regulus_config(graphical_interlingua_collocations, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	load_collocation_rules_file(AbsFile),
	format('~N--- Loaded graphical collocation rules file ~w~n', [AbsFile]).
load_graphical_collocation_rules_file_if_there_is_one.

load_interlingua_collocation_rules_files_if_there_are_any :-
	findall([File, Tag],
		regulus_config(interlingua_collocation_rules(Tag), File),
		Pairs0),
	(   regulus_config(interlingua_collocation_rules, DefaultFile) ->
	    Pairs = [[DefaultFile, check_interlingua] | Pairs0]
	;
	    otherwise ->
	    Pairs = Pairs0
	),
	load_interlingua_collocation_rules_files_if_there_are_any1(Pairs).

load_interlingua_collocation_rules_files_if_there_are_any1([]).
load_interlingua_collocation_rules_files_if_there_are_any1([[File, Tag] | R]) :-
	load_interlingua_collocation_rules_file(File, Tag),
	!,
	load_interlingua_collocation_rules_files_if_there_are_any1(R).

load_interlingua_collocation_rules_file(File, Tag) :-
	interlingua_collocation_rules_module(Tag, Module),
	load_collocation_rules_file(File, Module).

load_collocation_rules_file_if_there_is_one :-
	regulus_config(collocation_rules, File),
	!,
	load_collocation_rules_file(File, user).
load_collocation_rules_file_if_there_is_one :-
	format('~N~nNo collocation rules file defined.~n', []).

load_collocation_rules_file(File, Module) :-
	abolish_if_defined(Module:better_collocation_internal/2),
	safe_absolute_file_name(File, AbsFile),
	get_regulus_config_item(compiled_collocation_rules(Module), CompiledFile),
	compile_collocation_file(AbsFile, CompiledFile),
	safe_compile(Module, CompiledFile),
	!.
load_collocation_rules_file(File, Module) :-
	format('~N*** Warning: unable to load collocation rules file ~w into "~w"~n', [File, Module]).

%---------------------------------------------------------------

load_original_script_collocation_rules_file_if_there_is_one :-
	abolish_if_defined(user:better_original_script_collocation_internal/2),
	regulus_config(original_script_collocation_rules, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	load_original_script_collocation_rules_file(AbsFile).
load_original_script_collocation_rules_file_if_there_is_one.

load_original_script_collocation_rules_file(File) :-
	get_regulus_config_item(compiled_original_script_collocation_rules, CompiledFile),
	compile_original_script_collocation_file(File, CompiledFile),
	safe_compile(user, CompiledFile),
	!.
load_original_script_collocation_rules_file(File) :-
	format('~N*** Warning: unable to load original script collocation rules file ~w~n', [File]).

%---------------------------------------------------------------

compile_ellipsis_classes_file :-
	regulus_config(ellipsis_classes, File),
	get_regulus_config_item(top_level_cat, TopLevelCat),
	get_regulus_config_item(ellipsis_classes_sents_file, SentsFile),
	get_regulus_config_item(ellipsis_classes_treebank_file, TreebankFile),
	safe_absolute_file_name(File, AbsFile),
	%get_regulus_config_item(compiled_ellipsis_classes, CompiledFile),
	get_compiled_ellipsis_classes_file(CompiledFile),
	compile_ellipsis_class_file(AbsFile, TopLevelCat, SentsFile, TreebankFile, CompiledFile),
	!.
compile_ellipsis_classes_file :-
	format2error('~N~n*** Error: unable to compile ellipsis class file.~n', []).
	
%---------------------------------------------------------------

load_ellipsis_classes_file_if_there_is_one :-
	abolish_if_defined(user:ellipsis_class_example/2),
	regulus_config(ellipsis_classes, File),
	%get_regulus_config_item(compiled_ellipsis_classes, CompiledFile),
	get_compiled_ellipsis_classes_file(CompiledFile),
	safe_absolute_file_name(CompiledFile, AbsCompiledFile),
	(   file_exists(AbsCompiledFile) ->
	    load_file_without_warning_about_singleton_vars_or_redefines(AbsCompiledFile) ;
	    format('~N*** Warning: ellipsis classes file defined, but has not been compiled. Unable to load ~w~n', [File])
	),
	!.
load_ellipsis_classes_file_if_there_is_one :-
	format('~N~nNo ellipsis class file defined.~n', []).

%---------------------------------------------------------------

get_compiled_ellipsis_classes_file(CompiledFile) :-
	regulus_config(compiled_ellipsis_classes, CompiledFile),
	!.
get_compiled_ellipsis_classes_file(CompiledFile) :-
	get_regulus_config_item(default_compiled_ellipsis_classes, CompiledFile),
	!.

%---------------------------------------------------------------

compile_and_load_generation_rules_file(SpecialisedOrUnspecialised, SubDomain) :-
	(   atom(SubDomain) ->
	    true
	;
	    format2error('~N*** Error: bad second argument "~w" to compile_and_load_generation_rules_file/2. Should be atom',
			 [SubDomain]),
	    fail
	),
	(   SpecialisedOrUnspecialised = unspecialised ->
	    (   regulus_config(generation_regulus_grammar, GrammarFile) ->
		format('~N~n--- Taking grammar from value of "generation_regulus_grammar"~n', [])
	    ;
		
		regulus_config(regulus_grammar, GrammarFile) ->
		format('~N~n--- Taking grammar from value of "regulus_grammar"~n', []) ;
		
		format2error('~N~n*** Error: no grammar defined~n', []),
		fail
	    )
	;
	    
	    SpecialisedOrUnspecialised = specialised ->
	    get_regulus_config_item(ebl_regulus_no_binarise_grammar, GrammarFileBase),
	    make_ebl_ignore_feats_file(EBLIgnoreFeatsFile),
	    add_tag_to_files_in_list([GrammarFileBase], SubDomain, [GrammarFileForSubDomain]),
	    GrammarFile = [GrammarFileForSubDomain, EBLIgnoreFeatsFile]
	;
	    
	    format2error('~N*** Error: bad argument "~w" to compile_and_load_generation_rules_file/1. Should be "unspecialised" or "specialised(<SubDomainTag>)"',
			 [SpecialisedOrUnspecialised]),
	    fail
	),
	get_regulus_config_item(generation_dcg_grammar, DCGFile),
	get_regulus_config_item(reflective_dcg_grammar_for_generation, DynamicDCGFile),
	(   ( SubDomain = default, regulus_config(generation_grammar, GeneratorFile) ) ->
	    true
	;
	    ( SubDomain = default, regulus_config(generation_rules, GeneratorFile) ) ->
	    true
	;
	    regulus_config(generation_grammar(SubDomain), GeneratorFile) ->
	    true
	;
	    regulus_config(generation_rules(SubDomain), GeneratorFile) ->
	    true
	;
	    format2error('~N*** Error: compiled generation grammar file needs to be defined using generation_grammar or generation_rules declaration', []),
	    fail
	),
	(   regulus_config(generation_module_name, GenerationModule) ->
	    true
	;
	    GenerationModule = generator
	),
	(   regulus_config(top_level_generation_pred, GenerationPred) ->
	    true
	;
	    GenerationPred = generate
	),
	(   regulus_config(top_level_generation_cat, GenerationCat) ->
	    true ;
	    GenerationCat = '.MAIN'
	),
	(   regulus_config(top_level_generation_feat, GenerationFeat) ->
	    true
	;
	    GenerationFeat = value
	),
	(   regulus_config(generation_incremental_deepening_parameters, IncrementalDeepeningParams) ->
	    true
	;
	    IncrementalDeepeningParams = [5, 5, 50]
	),
	regulus2generator(GrammarFile, DCGFile, GenerationModule, GenerationPred, GenerationCat, GenerationFeat,
			  IncrementalDeepeningParams, GeneratorFile),
	load_compiled_generation_rules_file(GeneratorFile),
	regulus2dcg(GrammarFile, DynamicDCGFile, dynamic(generation), SucceededOrFailed2),
	(   SucceededOrFailed2 = succeeded ->
	    load_regulus_dcg_file(DynamicDCGFile) ;
	    format2error('~NError: unable to compile dynamic generation file~n', []),
	    fail
	),
	!.
compile_and_load_generation_rules_file(SpecialisedOrUnspecialised, SubDomain) :-
	format2error('~N*** Error: bad call ~w',
		     [compile_and_load_generation_rules_file(SpecialisedOrUnspecialised, SubDomain)]),
	fail.

%---------------------------------------------------------------

load_generation_rules_file :-
	(   regulus_config(generation_rules, File) ;
	    regulus_config(generation_grammar, File)
	),
	!,
	(   load_compiled_generation_rules_file(File) ->
	    true ;
	    format('~N~n*** Warning: Unable to load compiled generation rules file.~n', [])
	).
load_generation_rules_file :-
	format('~N~n*** Warning: No generation rules file defined.~n', []).

%---------------------------------------------------------------

load_alternate_generation_rules_files_if_there_are_any :-
	findall([Tag, File],
		regulus_config(generation_rules(Tag), File),
		Triples),
	load_alternate_generation_rules_files(Triples).

load_alternate_generation_rules_files([]).
load_alternate_generation_rules_files([F | R]) :-
	load_alternate_generation_rules_file(F),
	!,
	load_alternate_generation_rules_files(R).

load_alternate_generation_rules_file([Tag, File]) :-
	(   load_compiled_generation_rules_file(File) ->
	    check_alternate_generation_pred_defined(File, Tag)
	;
	    format('~N~n*** Warning: Unable to load alternate generation rules file ~w.~n', [File]),
	    fail
	).

check_alternate_generation_pred_defined(_File, Module) :-
	current_predicate(Module:generate/3),
	!.
check_alternate_generation_pred_defined(File, Module) :-
	format('~N~n*** Warning: Loaded alternate generation rules file ~w but ~w:generate/3 not found.~n', [File, Module]),
	fail.

%---------------------------------------------------------------

load_original_script_generation_rules_file_if_there_is_one :-
	regulus_config(original_script_generation_rules, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	format('~N~n--- Loading compiled original script generation rules file: ~w~n', [AbsFile]),
	(   load_compiled_generation_rules_file(AbsFile) ->
	    true
	;
	    format2error('~N~n*** Error: Unable to load compiled original script generation rules file.~n', [])
	).
load_original_script_generation_rules_file_if_there_is_one :-
	abolish_if_defined(generator_original_script:generate/3),
	!.

%---------------------------------------------------------------

load_original_script_recognition_generation_rules_file_if_there_is_one :-
	regulus_config(original_script_recognition_generation_rules, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	format('~N~n--- Loading compiled original script recognition generation rules file: ~w~n', [AbsFile]),
	(   load_compiled_generation_rules_file(AbsFile) ->
	    true
	;
	    format2error('~N~n*** Error: Unable to load compiled original script generation rules file.~n', [])
	).
load_original_script_recognition_generation_rules_file_if_there_is_one :-
	abolish_if_defined(recogniser_generator_original_script:generate/3),
	!.

%---------------------------------------------------------------

load_gloss_generation_rules_file_if_there_is_one :-
	regulus_config(gloss_generation_rules, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	format('~N~n--- Loading compiled gloss generation rules file: ~w~n', [AbsFile]),
	(   load_compiled_generation_rules_file(AbsFile) ->
	    true
	;
	    format2error('~N~n*** Error: Unable to load compiled gloss generation rules file.~n', [])
	).
load_gloss_generation_rules_file_if_there_is_one :-
	abolish_if_defined(generator_gloss:generate/3),
	!.

%---------------------------------------------------------------

load_gloss_recognition_generation_rules_file_if_there_is_one :-
	regulus_config(gloss_recognition_generation_rules, File),
	safe_absolute_file_name(File, AbsFile),
	!,
	format('~N~n--- Loading compiled gloss recognition_generation rules file: ~w~n', [AbsFile]),
	(   load_compiled_generation_rules_file(AbsFile) ->
	    true
	;
	    format2error('~N~n*** Error: Unable to load compiled gloss generation rules file.~n', [])
	).
load_gloss_recognition_generation_rules_file_if_there_is_one :-
	abolish_if_defined(recogniser_generator_gloss:generate/3),
	!.

%---------------------------------------------------------------

load_interlingua_structure_grammar_file_if_there_is_one :-
	regulus_config(interlingua_structure, File),
	!,
	(   load_compiled_generation_rules_file(File) ->
	    true ;
	    format('~N~n*** Warning: Unable to load compiled interlingua structure file.~n', [])
	).
load_interlingua_structure_grammar_file_if_there_is_one :-
	format('~N~nNo compiled interlingua structure file defined.~n', []).

%---------------------------------------------------------------

load_alternate_interlingua_structure_grammar_file_if_there_are_any :-
	findall([Tag, File, Module],
		regulus_config(alternate_interlingua_structure(Tag), [File, Module]),
		Triples),
	(   Triples = [] ->
	    format('~N~nNo alternate compiled interlingua structure files defined.~n', [])
	;
	    otherwise ->
	    load_alternate_interlingua_structure_grammar_files(Triples),
	    check_default_interlingua_structure_tag_is_defined
	),
	!.
load_alternate_interlingua_structure_grammar_file_if_there_are_any :-
	format('~N~n*** Warning: Unable to load alternate compiled interlingua structure files.~n', []).

load_alternate_interlingua_structure_grammar_files([]).
load_alternate_interlingua_structure_grammar_files([F | R]) :-
	load_alternate_interlingua_structure_grammar_file(F),
	!,
	load_alternate_interlingua_structure_grammar_files(R).

load_alternate_interlingua_structure_grammar_file([_Tag, File, Module]) :-
	(   load_compiled_generation_rules_file(File) ->
	    check_alternate_interlingua_structure_pred_defined(File, Module)
	;
	    format('~N~n*** Warning: Unable to load alternate compiled interlingua structure file ~w.~n', [File]),
	    fail
	).

check_alternate_interlingua_structure_pred_defined(_File, Module) :-
	current_predicate(Module:check_interlingua/3),
	!.
check_alternate_interlingua_structure_pred_defined(File, Module) :-
	format('~N~n*** Warning: Loaded alternate compiled interlingua structure file ~w but ~w:check_interlingua/3 not found.~n', [File, Module]),
	fail.

check_default_interlingua_structure_tag_is_defined :-
	regulus_config(default_interlingua_structure_tag, _DefaultTag),
	!.
check_default_interlingua_structure_tag_is_defined :-
	format('~N~n*** Error: default_interlingua_structure_tag must be defined if there are alternate interlingua structure files~n', []),
	fail.
	
%---------------------------------------------------------------

/*
load_orthography_rules_file_if_there_is_one :-
	abolish_if_defined(user:orthography_rewrite/2),
	regulus_config(orthography_rules, File),
	!,
	safe_compile(user, File).
load_orthography_rules_file_if_there_is_one :-
	format('~N~nNo orthography rules file defined.~n', []).
*/

load_orthography_rules_files_if_there_are_any :-
	load_orthography_rules_file_if_there_is_one,
	findall([Id, File],
		regulus_config(special_orthography_rules(Id), File),
		Pairs),
	load_special_orthography_rules_files(Pairs).

load_special_orthography_rules_files([]).
load_special_orthography_rules_files([[Id, File] | R]) :-
	load_special_orthography_rules_file(File, Id),
	!,
	load_special_orthography_rules_files(R).

load_special_orthography_rules_file(File, Id) :-
	special_orthography_module(Id, Module),
	abolish_if_defined(Module:orthography_rewrite/2),
	abolish_if_defined(Module:letter_class/2),
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading "~w" special orthography rules file: ~w~n', [Id, AbsFile]),
	load_orthography_rules_file(AbsFile, Module).

load_orthography_rules_file_if_there_is_one :-
	abolish_if_defined(user:orthography_rewrite/2),
	abolish_if_defined(user:letter_class/2),
	regulus_config(orthography_rules, File),
	!,
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading orthography rules file: ~w~n', [AbsFile]),
	load_orthography_rules_file(AbsFile, user).
load_orthography_rules_file_if_there_is_one :-
	format('~N~nNo orthography rules file defined.~n', []).

load_orthography_rules_file(File, Module) :-
	get_regulus_config_item(compiled_orthography_rules(Module), CompiledFile),
	compile_orthography_file_or_files(File, CompiledFile),
	safe_compile(Module, CompiledFile),
	!.
load_orthography_rules_file(File, _Module) :-
	format('~N*** Warning: unable to load orthography rules file ~w~n', [File]).

%---------------------------------------------------------------

load_original_script_orthography_rules_file_if_there_is_one :-
	abolish_if_defined(user:original_script_orthography_rewrite/2),
	abolish_if_defined(user:original_script_letter_class/2),
	regulus_config(original_script_orthography_rules, File),
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading original script orthography rules file: ~w~n', [AbsFile]),
	!,
	load_original_script_orthography_rules_file(AbsFile).
load_original_script_orthography_rules_file_if_there_is_one.

load_original_script_orthography_rules_file(File) :-
	get_regulus_config_item(compiled_original_script_orthography_rules, CompiledFile),
	compile_original_script_orthography_file_or_files(File, CompiledFile),
	safe_compile(user, CompiledFile),
	!.
load_original_script_orthography_rules_file(File) :-
	format('~N*** Warning: unable to load original_script_orthography rules file ~w~n', [File]).

%---------------------------------------------------------------

load_recognition_orthography_rules_file_if_there_is_one :-
	abolish_if_defined(user:recognition_orthography_rewrite/2),
	abolish_if_defined(user:recognition_letter_class/2),
	regulus_config(recognition_orthography_rules, File),
	safe_absolute_file_name(File, AbsFile),
	format('~N--- Loading recognition orthography rules file: ~w~n', [AbsFile]),
	!,
	load_recognition_orthography_rules_file(AbsFile).
load_recognition_orthography_rules_file_if_there_is_one.

load_recognition_orthography_rules_file(File) :-
	get_regulus_config_item(compiled_recognition_orthography_rules, CompiledFile),
	compile_recognition_orthography_file_or_files(File, CompiledFile),
	safe_compile(user, CompiledFile),
	!.
load_recognition_orthography_rules_file(File) :-
	format('~N*** Warning: unable to load recognition_orthography rules file ~w~n', [File]).

%---------------------------------------------------------------

load_dialogue_files :-
	regulus_config(dialogue_files, Files),
	!,
	safe_compile_list(user, Files),
	load_lf_patterns_file_if_there_is_one,
	load_lf_rewrite_file_if_there_is_one,
	load_paraphrase_generation_grammar_if_there_is_one,
	load_generation_preferences_file_if_there_is_one,
	load_orthography_rules_file_if_there_is_one,
	initialise_wavfiles_directory_if_there_is_one,
	load_graphical_interlingua_grammar_if_there_is_one,
	% Using collocations in the graphical interlingua seems very messy - avoid if possible
	%load_graphical_collocation_rules_file_if_there_is_one,
	initialise_jpeg_directory_if_there_is_one.
load_dialogue_files :-
	format('~N~nNo dialogue files defined.~n', []).

%---------------------------------------------------------------	

load_lf_patterns_file_if_there_is_one :-
	\+ regulus_config(lf_patterns, _InFile),
	!.
load_lf_patterns_file_if_there_is_one :-
	regulus_config(lf_patterns, InFile),
	(   regulus_config(lf_patterns_modules, Modules) ->
	    true
	;
	    otherwise ->
	    Modules = []
	),
	get_regulus_config_item(compiled_lf_patterns, TmpFile),
	compile_lf_patterns(InFile, Modules, TmpFile),
	!.

%---------------------------------------------------------------	

load_lf_rewrite_file_if_there_is_one :-
	\+ regulus_config(lf_rewrite_rules, _InFile),
	!.
load_lf_rewrite_file_if_there_is_one :-
	regulus_config(lf_rewrite_rules, InFile),
	get_regulus_config_item(compiled_lf_rewrite_rules, TmpFile),
	compile_lf_rewrite_rules(InFile, TmpFile),
	!.

%---------------------------------------------------------------	

load_paraphrase_generation_grammar_if_there_is_one :-
	regulus_config(paraphrase_generation_grammar, File),
	!,
	(   load_compiled_generation_rules_file(File) ->
	    true ;
	    format('~N~n*** Warning: Unable to load paraphrase generation file.~n', [])
	).
load_paraphrase_generation_grammar_if_there_is_one :-
	format('~N~nNo compiled paraphrase generation grammar file defined.~n', []).

%---------------------------------------------------------------	

initialise_wavfiles_directory_if_there_is_one :-
	regulus_config(wavfile_directory, Dir),
	!,
	initialise_wavfiles_directory(Dir),
	write_script_recording_file.
initialise_wavfiles_directory_if_there_is_one :-
	remove_wavfile_table,
	format('~N~nNo wavfile directory defined, assuming TTS output.~n', []).

initialise_wavfiles_directory(Dir) :-
	safe_absolute_file_name(Dir, AbsDir),
	make_wavfile_table(Dir, NFiles),
	format('~N~nCached wavfile information (~d files) for directory ~w.~n', [NFiles, AbsDir]).	

write_script_recording_file :-
	\+ regulus_config(wavfile_recording_script, _WavfileRecordingScript),
	!.
write_script_recording_file :-
	get_regulus_config_item(wavfile_recording_script, WavfileRecordingScript),
	regulus_config(wavfile_directory, Dir),
	regulus_config(generation_rules, GenerationGrammarFile),
	(   regulus_config(collocation_rules, CollocationsFile) ->
	    true
	;
	    CollocationsFile = null
	),
	list_missing_wavfiles(GenerationGrammarFile, CollocationsFile, Dir, WavfileRecordingScript, NMissingFiles),
	safe_absolute_file_name(WavfileRecordingScript, AbsWavfileRecordingScript),
	(   NMissingFiles = 0 ->
	    format('~N~nAll necessary wavfiles appear to have been recorded.~n', [])
	;
	    otherwise ->
	    format('~N~nRecording script for missing wavfiles (~d files) written to file ~w.~n',
		   [NMissingFiles, AbsWavfileRecordingScript])
	),
	!.
write_script_recording_file :-
	format('~N~n*** Warning: call to write_script_recording_file/0 failed.~n', []).
	
%---------------------------------------------------------------	

load_graphical_interlingua_grammar_if_there_is_one :-
	regulus_config(graphical_interlingua_structure, InterlinguaGrammar),
	!,
	safe_absolute_file_name(InterlinguaGrammar, AbsInterlinguaGrammar),
	load_compiled_generation_rules_file(AbsInterlinguaGrammar).
load_graphical_interlingua_grammar_if_there_is_one.

%---------------------------------------------------------------	

initialise_jpeg_directory_if_there_is_one :-
	regulus_config(jpeg_directory, Dir),
	!,
	initialise_jpeg_directory(Dir),
	write_jpeg_script_recording_file.
initialise_jpeg_directory_if_there_is_one :-
	remove_jpeg_table.

initialise_jpeg_directory(Dir) :-
	safe_absolute_file_name(Dir, AbsDir),
	make_jpeg_table(Dir, NFiles),
	format('~N~nCached JPG information (~d files) for directory ~w.~n', [NFiles, AbsDir]).	

write_jpeg_script_recording_file :-
	\+ regulus_config(jpeg_recording_script, _RecordingScript),
	!.
write_jpeg_script_recording_file :-
	get_regulus_config_item(jpeg_recording_script, RecordingScript),
	regulus_config(jpeg_directory, Dir),
	regulus_config(graphical_interlingua_structure, GenerationGrammarFile),
	(   regulus_config(graphical_interlingua_collocations, CollocationsFile) ->
	    true
	;
	    otherwise ->
	    CollocationsFile = null
	),
	list_missing_jpegs(GenerationGrammarFile, CollocationsFile, Dir, RecordingScript, NMissingFiles),
	safe_absolute_file_name(RecordingScript, AbsRecordingScript),
	(   NMissingFiles = 0 ->
	    format('~N~nAll necessary JPEGs appear to exist.~n', [])
	;
	    otherwise ->
	    format('~N~nRecording script for missing JPEGs (~d files) written to file ~w.~n',
		   [NMissingFiles, AbsRecordingScript])
	),
	!.
write_jpeg_script_recording_file :-
	format('~N~n*** Warning: call to write_jpeg_script_recording_file/0 failed.~n', []).
	
%---------------------------------------------------------------

load_file_without_warning_about_singleton_vars(File) :-
	set_prolog_flags_for_no_singleton_var_warnings(OldFlags),
	on_exception(
	_Exception,
	safe_compile(user, File),
	restore_prolog_flags_after_removing_singleton_var_warnings(OldFlags)
    ),
	restore_prolog_flags_after_removing_singleton_var_warnings(OldFlags).

set_prolog_flags_for_no_singleton_var_warnings(OldFlags) :-
	prolog_flag(single_var_warnings, OldRedefine, off),
	OldFlags = [OldRedefine].

restore_prolog_flags_after_removing_singleton_var_warnings(OldFlags) :-
	OldFlags = [OldRedefine],
	prolog_flag(single_var_warnings, _Current, OldRedefine).

%---------------------------------------------------------------

load_file_without_warning_about_singleton_vars_or_redefines(File) :-
	set_prolog_flags_for_no_singleton_var_or_redefine_warnings(OldFlags),
	on_exception(
	_Exception,
	safe_compile(user, File),
	restore_prolog_flags_after_removing_singleton_var_warnings(OldFlags)
    ),
	restore_prolog_flags_after_removing_singleton_var_or_redefine_warnings(OldFlags).

set_prolog_flags_for_no_singleton_var_or_redefine_warnings(OldFlags) :-
	prolog_flag(single_var_warnings, OldSingleVar, off),
	prolog_flag(redefine_warnings, OldRedefine, off),
	OldFlags = [OldSingleVar, OldRedefine].

restore_prolog_flags_after_removing_singleton_var_or_redefine_warnings(OldFlags) :-
	OldFlags = [OldSingleVar, OldRedefine],
	prolog_flag(single_var_warnings, _CurrentSingleVar, OldSingleVar),
	prolog_flag(redefine_warnings, _CurrentRedefine, OldRedefine).

%---------------------------------------------------------------

load_compiled_generation_rules_file(File) :-
	load_file_without_warning_about_singleton_vars_discontiguous_rules_or_redefines(File).

load_file_without_warning_about_singleton_vars_discontiguous_rules_or_redefines(File) :-
	load_file_without_warning_about_singleton_vars_discontiguous_rules_or_redefines(File, user).

load_file_without_warning_about_singleton_vars_discontiguous_rules_or_redefines(File, Module) :-
	set_prolog_flags_for_no_singleton_var_discontiguous_rules_or_redefine_warnings(OldFlags),
	on_exception(
	_Exception,
	safe_compile(Module, File),
	restore_prolog_flags_after_removing_singleton_var_discontiguous_rules_or_redefine_warnings(OldFlags)
    ),
	restore_prolog_flags_after_removing_singleton_var_discontiguous_rules_or_redefine_warnings(OldFlags).

set_prolog_flags_for_no_singleton_var_discontiguous_rules_or_redefine_warnings(OldFlags) :-
	prolog_flag(single_var_warnings, OldSingleVar, off),
	prolog_flag(redefine_warnings, OldRedefine, off),
	prolog_flag(discontiguous_warnings, OldDiscontiguous, off),
	OldFlags = [OldSingleVar, OldRedefine, OldDiscontiguous].

restore_prolog_flags_after_removing_singleton_var_discontiguous_rules_or_redefine_warnings(OldFlags) :-
	OldFlags = [OldSingleVar, OldRedefine, OldDiscontiguous],
	prolog_flag(single_var_warnings, _CurrentSingleVar, OldSingleVar),
	prolog_flag(redefine_warnings, _CurrentRedefine, OldRedefine),
	prolog_flag(discontiguous_warnings, _CurrentDiscontiguous, OldDiscontiguous).

%---------------------------------------------------------------

get_ignored_subdomains(IgnoredSubdomains) :-
	findall(Subdomain,
		regulus_config(ignore_subdomain, Subdomain),
		IgnoredSubdomains0),
	list_to_ord_set(IgnoredSubdomains0, IgnoredSubdomains),
	!.
get_ignored_subdomains(IgnoredSubdomains) :-
	format2error('~N*** Error: bad call: ~w~n', [get_ignored_subdomains(IgnoredSubdomains)]),
	fail.

%---------------------------------------------------------------

regulus_config_file_or_files_ok(Key) :-
	regulus_config_file_or_files_ok(Key, null).

regulus_config_file_or_files_ok(Key, OptionalExtension) :-
	regulus_config(Key, Value),
	file_or_files_ok(Value, OptionalExtension).
regulus_config_file_or_files_ok(Key, OptionalExtension) :-
	regulus_working_file(Key, Suffix),
	regulus_config(working_file_prefix, Prefix),
	safe_absolute_file_name(Prefix, AbsPrefix),
	join_with_underscore([AbsPrefix, Suffix], Value),
	file_or_files_ok(Value, OptionalExtension).

regulus_config_defined(Key) :-
	regulus_config(Key, _Value).

grammar_is_loaded :-
	current_predicate(user:dcg_clause/2),
	%format('~NGrammar is loaded~n', []),
	!.
grammar_is_loaded :-
	%format('~NGrammar is not loaded~n', []),
	fail.

%---------------------------------------------------------------

file_or_files_ok([], _OptionalExtension) :-
	!.
file_or_files_ok([transfer_direction(_) | R], OptionalExtension) :-
	!,
	file_or_files_ok(R, OptionalExtension).
file_or_files_ok([F | R], OptionalExtension) :-
	file_ok(F, OptionalExtension),
	!,
	file_or_files_ok(R, OptionalExtension).
file_or_files_ok(File, OptionalExtension) :-
	\+ is_list(File),
	file_ok(File, OptionalExtension),
	!.

file_ok(File, _OptionalExtension) :-
	safe_absolute_file_name(File, AbsFile),
	file_exists(AbsFile),
	!.
file_ok(File, OptionalExtension) :-
	OptionalExtension \== null,
	safe_absolute_file_name(File, AbsFile),
	format_to_atom('~w.~w', [AbsFile, OptionalExtension], AbsFileWithExtension),
	file_exists(AbsFileWithExtension),
	!.
	    
%---------------------------------------------------------------

get_regulus_file_for_java_gui(KeyString, File, InputOrOutput) :-
	on_exception(Exception,
		     get_regulus_file_for_java_gui1(KeyString, File, InputOrOutput),
		     handle_exception_in_get_regulus_file_for_java_gui(KeyString, Exception)
		    ).

get_regulus_file_for_java_gui1(KeyString, AbsFile, InputOrOutput) :-
	read_key_for_get_regulus_file_for_java_gui(KeyString, Key),
	get_regulus_config_item(Key, File),
	absolute_file_name(File, AbsFile),
	(   InputOrOutput \== "input" ->
	    true
	;
	    file_exists(AbsFile) ->
	    true
	;
	    ( format2error('~N*** Error: input file "~w" does not exist~n', [AbsFile]), fail )
	),
	!.

read_key_for_get_regulus_file_for_java_gui(KeyString, Key) :-
	on_exception(_Exception,
		     read_key_for_get_regulus_file_for_java_gui1(KeyString, Key),
		     ( format2error('~N*** Error: key "~s" is ill-formed~n', [KeyString]), fail )
		    ).

read_key_for_get_regulus_file_for_java_gui1(KeyString, Key) :-
	append(KeyString, ".", KeyString1),
	read_from_chars(KeyString1, Key),
	!.

%---------------------------------------------------------------

delete_file_with_status_for_java_gui(FileString, Status) :-
	(   is_prolog_string(FileString) ->
	    format('--- Delete file: ~s~n', [FileString])
	;
	    otherwise ->
	    format('--- Delete file (nonstring, incorrect): ~w~n', [FileString])
	),
	(   delete_file_with_status(FileString, Status) ->
	    (   is_prolog_string(Status) ->
		format('--- ~s~n', [Status])
	    ;
		otherwise ->
		format('--- Delete file response (nonstring, incorrect): ~w~n', [Status])
	    )
	;
	    otherwise ->
	    format('--- Delete file failed~n', [])
	),	
	!.

%---------------------------------------------------------------

print_info_for_feature(Feat) :-
	\+ regulus_preds:feature(Feat, _FeatValSpaceId),
	!,
	format('~N~nCannot find a definition for feature "~w"~n', [Feat]).
print_info_for_feature(Feat) :-
	regulus_preds:feature(Feat, FeatValSpaceId),
	(   regulus_preds:feature_value_space(FeatValSpaceId, FeatValSpace) ->
	    format('~N~nFeature values for feature "~w": ~w~n', [Feat, FeatValSpace])
	;
	    otherwise ->
	    format('~N~nThe feature "~w" is associated with feature value space ~w, but I can\'t find a definition for that space~n', [Feat, FeatValSpaceId])
	).

print_info_for_category(Cat) :-
	\+ regulus_preds:category_internal(Cat, _Feats),
	!,
	format('~N~nCannot find a definition for category "~w"~n', [Cat]).
print_info_for_category(Cat) :-
	regulus_preds:category_internal(Cat, Feats),
	format('~N~nFeatures for category "~w": ~w~n', [Cat, Feats]),
	!.

%---------------------------------------------------------------

get_regulus_config_item(regulus_no_sem_decls, AbsNoSemFile) :-
	NoSemFile = '$REGULUS/Grammar/no_sem.regulus',
	safe_absolute_file_name(NoSemFile, AbsNoSemFile),
	!.
get_regulus_config_item(_Key, _Value) :-
	\+ regulus_config(_AnyKey, _AnyValue),
	!,
	format2error('~NError: no config file loaded', []),
	fail.
get_regulus_config_item(Key, Value) :-
	regulus_working_file(Key, Suffix),
	(   regulus_config(working_file_prefix, Prefix) ->
	    safe_absolute_file_name(Prefix, AbsPrefix),
	    join_with_underscore([AbsPrefix, Suffix], Value) ;
	    format2error('~NError: regulus_config declaration for working_file_prefix missing~n', []),
	    fail
	),
	!.
get_regulus_config_item(translation_or_dialogue_speech_corpus(Type), Value) :-
	!,
	(   regulus_config(translation_speech_corpus, Value),
	    Type = translation
	;
	    regulus_config(dialogue_speech_corpus, Value),
	    Type = dialogue
	).
get_regulus_config_item(translation_or_dialogue_speech_corpus(Type, Id), Value) :-
	!,
	(   regulus_config(translation_speech_corpus(Id), Value),
	    Type = translation
	;
	    regulus_config(dialogue_speech_corpus(Id), Value),
	    Type = dialogue
	).
get_regulus_config_item(Key, Value) :-
	regulus_config(Key, Value),
	!.
get_regulus_config_item(Key, _Value) :-
	format2error('~NError: no config value defined for key "~w"~n', [Key]),
	fail.

%---------------------------------------------------------------

config_file_entry(alterf_patterns_file,'',no_section).
config_file_entry(alterf_sents_file,'',no_section).
config_file_entry(alterf_treebank_file,'',no_section).
config_file_entry(analysis_time_limit,'',no_section).
config_file_entry(answer_config_file,'',no_section).
config_file_entry(batchrec_trace,'',no_section).
config_file_entry(batchrec_trace_prolog,'',no_section).
config_file_entry(batchrec_trace_prolog_with_transcriptions(_Arg),'',no_section).
config_file_entry(batchrec_trace_prolog_with_transcriptions,'',no_section).
config_file_entry(collocation_rules,'',no_section).
config_file_entry(compiled_collocation_rules,'',no_section).
config_file_entry(compiled_ellipsis_classes,'',no_section).
config_file_entry(compiled_from_interlingua_rules,'',no_section).
config_file_entry(compiled_lf_patterns,'',no_section).
config_file_entry(compiled_lf_rewrite_rules,'',no_section).
config_file_entry(compiled_original_script_collocation_rules,'',no_section).
config_file_entry(compiled_original_script_orthography_rules,'',no_section).
config_file_entry(compiled_recognition_orthography_rules,'',no_section).
config_file_entry(compiled_graphical_orthography_rules,'',no_section).
config_file_entry(compiled_orthography_rules,'',no_section).
config_file_entry(compiled_surface_constituent_rules,'',no_section).
%config_file_entry(compiled_targeted_help_classes_file,'',no_section).
config_file_entry(compiled_to_interlingua_rules,'',no_section).
config_file_entry(compiled_to_source_discourse_rules,'',no_section).
config_file_entry(compiled_transfer_rules,'',no_section).
config_file_entry(dcg_grammar,'',no_section).
config_file_entry(default_compiled_ellipsis_classes,'',no_section).
config_file_entry(dialogue_batchrec_trace_prolog_with_transcriptions(_Arg),'',no_section).
config_file_entry(dialogue_batchrec_trace_prolog_with_transcriptions,'',no_section).
config_file_entry(dialogue_corpus(_Arg),'',no_section).
config_file_entry(dialogue_corpus,'',no_section).
config_file_entry(dialogue_corpus_judgements,'',no_section).
config_file_entry(dialogue_corpus_results(_Arg),'',no_section).
config_file_entry(dialogue_corpus_results,'',no_section).
config_file_entry(dialogue_files,'',no_section).
config_file_entry(dialogue_processing_time_limit,'',no_section).
config_file_entry(dialogue_rec_params,'',no_section).
config_file_entry(dialogue_speech_corpus(_Arg),'',no_section).
config_file_entry(dialogue_speech_corpus,'',no_section).
config_file_entry(dialogue_speech_corpus_results(_Arg),'',no_section).
config_file_entry(dialogue_speech_corpus_results,'',no_section).
config_file_entry(discard_lexical_info_in_ebl_training,'',no_section).
config_file_entry(discriminants,'',no_section).
config_file_entry(ebl_context_use_threshold,'',no_section).
config_file_entry(ebl_corpus,'',no_section).
config_file_entry(ebl_filter_pred,'',no_section).
config_file_entry(ebl_gemini_grammar,'',no_section).
config_file_entry(ebl_grammar_probs,'',no_section).
config_file_entry(ebl_ignore_feats,'',no_section).
config_file_entry(ebl_ignore_feats_file,'',no_section).
config_file_entry(ebl_include_lex,'',no_section).
config_file_entry(ebl_multiple_grammar_decls,'',no_section).
config_file_entry(ebl_nuance_grammar,'',no_section).
config_file_entry(ebl_operationality,'',no_section).
config_file_entry(ebl_rationalised_corpus,'',no_section).
config_file_entry(ebl_raw_regulus_grammar,'',no_section).
config_file_entry(ebl_regulus_component_grammar,'',no_section).
config_file_entry(ebl_regulus_grammar,'',no_section).
config_file_entry(ebl_regulus_no_binarise_grammar,'',no_section).
config_file_entry(ebl_treebank,'',no_section).
config_file_entry(ellipsis_classes,'',no_section).
config_file_entry(ellipsis_classes_sents_file,'',no_section).
config_file_entry(ellipsis_classes_treebank_file,'',no_section).
config_file_entry(filtered_interlingua_declarations_file,'',no_section).
config_file_entry(from_interlingua_rule_learning_config_file,'',no_section).
config_file_entry(from_interlingua_rules,'',no_section).
config_file_entry(from_interlingua_translation_corpus_judgements,'',no_section).
config_file_entry(gemini_grammar,'',no_section).
config_file_entry(generation_dcg_grammar,'',no_section).
config_file_entry(generation_grammar(_Arg),'',no_section).
config_file_entry(generation_grammar,'',no_section).
config_file_entry(generation_incremental_deepening_parameters,'',no_section).
config_file_entry(generation_module_name,'',no_section).
config_file_entry(generation_preferences,'',no_section).
config_file_entry(generation_regulus_grammar,'',no_section).
config_file_entry(generation_rules(_Arg),'',no_section).
config_file_entry(generation_rules,'',no_section).
config_file_entry(generation_time_limit,'',no_section).
config_file_entry(global_context,'',no_section).
config_file_entry(gloss_generation_rules,'',no_section).
config_file_entry(grammar_probs_data,'',no_section).
config_file_entry(ignore_subdomain,'',no_section).
config_file_entry(interlingua_declarations,'',no_section).
config_file_entry(interlingua_structure,'',no_section).
config_file_entry(lc_tables_file,'',no_section).
config_file_entry(lf_patterns,'',no_section).
config_file_entry(lf_patterns_modules,'',no_section).
config_file_entry(lf_postproc_pred,'',no_section).
config_file_entry(macro_expanded_grammar,'',no_section).
config_file_entry(missing_help_class_decls,'',no_section).
config_file_entry(nbest_preferences,'',no_section).
config_file_entry(nbest_training_data_file,'',no_section).
config_file_entry(no_spaces_in_original_script,'',no_section).
config_file_entry(nuance_compile_params,'',no_section).
config_file_entry(nuance_grammar,'',no_section).
config_file_entry(nuance_grammar_for_compilation,'',no_section).
config_file_entry(nuance_grammar_for_pcfg_training,'',no_section).
config_file_entry(nuance_language_pack,'',no_section).
config_file_entry(nuance_recognition_package,'',no_section).
config_file_entry(only_translate_up_to_interlingua,'',no_section).
config_file_entry(original_script_collocation_rules,'',no_section).
config_file_entry(original_script_encoding,'',no_section).
config_file_entry(original_script_generation_rules,'',no_section).
config_file_entry(original_script_orthography_rules,'',no_section).
config_file_entry(orthography_rules,'',no_section).
config_file_entry(paraphrase_corpus,'',no_section).
config_file_entry(paraphrase_generation_grammar,'',no_section).
config_file_entry(parse_preferences,'',no_section).
config_file_entry(parsing_history_file,'',no_section).
config_file_entry(pcfg_training_output_directory,'',no_section).
config_file_entry(prolog_semantics,'',no_section).
config_file_entry(reflective_dcg_grammar,'',no_section).
config_file_entry(reflective_dcg_grammar_for_generation,'',no_section).
config_file_entry(regulus_grammar,'',no_section).
config_file_entry(regulus_no_sem_decls,'',no_section).
config_file_entry(resolution_preferences,'',no_section).
config_file_entry(role_marked_semantics,'',no_section).
config_file_entry(stanford_dcg_grammar,'',no_section).
config_file_entry(stanford_dcg_debug_grammar,'',no_section).
config_file_entry(strcat_semantics,'',no_section).
config_file_entry(surface_constituent_rules,'',no_section).
config_file_entry(surface_patterns,'',no_section).
config_file_entry(surface_postprocessing,'',no_section).
config_file_entry(tagging_grammar,'',no_section).
config_file_entry(target_model,'',no_section).
config_file_entry(targeted_help_backed_off_corpus_file,'',no_section).
config_file_entry(targeted_help_classes_file,'',no_section).
config_file_entry(targeted_help_corpus_file,'',no_section).
config_file_entry(targeted_help_source_files,'',no_section).
config_file_entry(test_corpus,'',no_section).
config_file_entry(tmp_ebl_operational_file,'',no_section).
config_file_entry(tmp_preds,'',no_section).
config_file_entry(to_interlingua_rule_learning_config_file,'',no_section).
config_file_entry(to_interlingua_rules,'',no_section).
config_file_entry(to_interlingua_translation_corpus_judgements,'',no_section).
config_file_entry(to_source_discourse_rules,'',no_section).
config_file_entry(top_level_cat,'',no_section).
config_file_entry(top_level_generation_cat,'',no_section).
config_file_entry(top_level_generation_feat,'',no_section).
config_file_entry(top_level_generation_pred,'',no_section).
config_file_entry(transfer_rules,'',no_section).
config_file_entry(translate_from_interlingua,'',no_section).
config_file_entry(translation_corpus(_Arg),'',no_section).
config_file_entry(translation_corpus,'',no_section).
config_file_entry(translation_corpus_judgements,'',no_section).
config_file_entry(translation_corpus_recognition_judgements,'',no_section).
config_file_entry(translation_corpus_results(_Arg),'',no_section).
config_file_entry(translation_corpus_results,'',no_section).
config_file_entry(translation_corpus_tmp_recognition_judgements(_Arg),'',no_section).
config_file_entry(translation_corpus_tmp_recognition_judgements,'',no_section).
config_file_entry(translation_rec_params,'',no_section).
config_file_entry(translation_speech_corpus(_Arg),'',no_section).
config_file_entry(translation_speech_corpus,'',no_section).
config_file_entry(translation_speech_corpus_results(_Arg),'',no_section).
config_file_entry(translation_speech_corpus_results,'',no_section).
config_file_entry(tts_command,'',no_section).
config_file_entry(wavfile_directory,'',no_section).
config_file_entry(wavfile_preceding_context(_Arg),'',no_section).
config_file_entry(wavfile_preceding_context,'',no_section).
config_file_entry(wavfile_recording_script,'',no_section).
config_file_entry(wavfiles,'',no_section).
config_file_entry(working_directory,'',no_section).
config_file_entry(working_file_prefix,'',no_section).

%---------------------------------------------------------------

regulus_working_file(macro_expanded_grammar, 'macro_expanded.regulus').
regulus_working_file(dcg_grammar, 'dcg.pl').
regulus_working_file(reflective_dcg_grammar, 'reflective_dcg.pl').
regulus_working_file(reflective_dcg_debug_grammar, 'reflective_debug_dcg.pl').
regulus_working_file(reflective_dcg_grammar_for_treebank, 'reflective_dcg_for_treebank.pl').
regulus_working_file(parse_preferences_for_treebank, 'parse_preferences_for_treebank.pl').
regulus_working_file(reflective_dcg_grammar_for_generation, 'reflective_dcg_for_generation.pl').
regulus_working_file(stanford_dcg_grammar, 'stanford_dcg.pl').
regulus_working_file(stanford_dcg_debug_grammar, 'stanford_debug_dcg.pl').
regulus_working_file(lc_tables_file, 'lc_tables.pl').
regulus_working_file(lc_debug_tables_file, 'lc_debug_tables.pl').
regulus_working_file(ebl_treebank, 'trees.pl').
regulus_working_file(ebl_old_treebank, 'old_trees.pl').
regulus_working_file(ebl_raw_regulus_grammar, 'specialised_raw.regulus').
regulus_working_file(ebl_created_sent_data, 'ebl_created_sent_data.pl').
regulus_working_file(ebl_regulus_grammar, 'specialised.regulus').
regulus_working_file(ebl_regulus_no_binarise_grammar, 'specialised_no_binarise.regulus').
regulus_working_file(ebl_multiple_grammar_decls, 'multiple_grammar_decls.pl').
regulus_working_file(ebl_rationalised_corpus, 'rationalised_corpus.pl').
regulus_working_file(ebl_dcg_grammar, 'ebl_dcg.pl').
regulus_working_file(ebl_reflective_dcg_grammar, 'ebl_reflective_dcg.pl').
regulus_working_file(ebl_stanford_dcg_grammar, 'ebl_stanford_dcg.pl').
regulus_working_file(ebl_lc_tables_file, 'ebl_lc_tables.pl').
regulus_working_file(alterf_sents_file, 'alterf_sents.pl').
regulus_working_file(alterf_treebank_file, 'alterf_trees.pl').
regulus_working_file(generation_dcg_grammar, 'generation_dcg_grammar.pl').
regulus_working_file(compiled_transfer_rules, 'compiled_transfer_rules.pl').
regulus_working_file(compiled_to_source_discourse_rules(Module), File) :-
	format_to_atom('compiled_to_source_discourse_rules_~w.pl', [Module], File).
regulus_working_file(compiled_to_interlingua_rules(Module), File) :-
	format_to_atom('compiled_to_interlingua_rules_~w.pl', [Module], File).
regulus_working_file(compiled_from_interlingua_rules(Module), File) :-
	format_to_atom('compiled_from_interlingua_rules_~w.pl', [Module], File).
regulus_working_file(compiled_collocation_rules, 'compiled_collocation_rules.pl').
regulus_working_file(compiled_collocation_rules(Module), File) :-
	format_to_atom('compiled_collocation_rules_~w.pl', [Module], File).
regulus_working_file(compiled_original_script_collocation_rules, 'compiled_original_script_collocation_rules.pl').
regulus_working_file(compiled_surface_constituent_rules, 'compiled_surface_constituents_rules.pl').
regulus_working_file(compiled_orthography_rules, 'compiled_orthography_rules.pl').
regulus_working_file(compiled_orthography_rules(Module), File) :-
	format_to_atom('compiled_orthography_rules_~w.pl', [Module], File).
regulus_working_file(compiled_original_script_orthography_rules, 'compiled_original_script_orthography_rules.pl').
regulus_working_file(compiled_recognition_orthography_rules, 'compiled_recognition_orthography_rules.pl').
regulus_working_file(compiled_lf_patterns, 'compiled_lf_patterns.pl').
regulus_working_file(compiled_lf_rewrite_rules, 'compiled_lf_rewrite_rules.pl').
%regulus_working_file(compiled_targeted_help_classes_file, 'compiled_targeted_help_classes.pl').
regulus_working_file(default_compiled_ellipsis_classes, 'compiled_ellipsis_classes.pl').
regulus_working_file(ellipsis_classes_sents_file, 'ellipsis_classes_sents.pl').
regulus_working_file(ellipsis_classes_treebank_file, 'ellipsis_classes_trees.pl').
regulus_working_file(filtered_interlingua_declarations_file, 'filtered_interlingua_declarations.pl').
regulus_working_file(ebl_ignore_feats_file, 'ebl_ignore_feats.regulus').
regulus_working_file(tmp_ebl_operational_file, 'tmp_ebl_operational.pl').
regulus_working_file(parsing_history_file, 'parsing_history.pl').
regulus_working_file(targeted_help_corpus_file, 'targeted_help_corpus.pl').
%regulus_working_file(targeted_help_backed_off_corpus_file, 'targeted_help_backed_off_corpus.pl').

regulus_working_file(wavfiles, 'wavfiles.txt').
regulus_working_file(batchrec_trace, 'batchrec_trace.txt').
regulus_working_file(batchrec_trace_prolog, 'batchrec_trace.pl').
regulus_working_file(batchrec_trace_prolog_with_transcriptions, 'batchrec_trace_with_transcriptions.pl').
regulus_working_file(dialogue_batchrec_trace_prolog_with_transcriptions, 'dialogue_batchrec_trace_with_transcriptions.pl').
regulus_working_file(batchrec_trace_prolog_with_transcriptions(Id), File) :-
	format_to_atom('batchrec_trace_with_transcriptions_~w.pl', [Id], File).
regulus_working_file(dialogue_batchrec_trace_prolog_with_transcriptions(Id), File) :-
	format_to_atom('dialogue_batchrec_trace_with_transcriptions_~w.pl', [Id], File).
regulus_working_file(missing_help_class_decls, 'missing_help_class_decls.pl').
regulus_working_file(cached_dynamic_lex_entries, 'cached_dynamic_lex_entries.pl').
regulus_working_file(intermediate_dynamic_lex_associations, 'intermediate_dynamic_lex_associations.pl').
regulus_working_file(dynamic_lex_associations, 'dynamic_lex_associations.pl').
regulus_working_file(dummy_top_level_rules, 'dummy_top_level_rules.regulus').
regulus_working_file(tmp_interlingua_declarations_file, 'tmp_interlingua_declarations_file.pl').
regulus_working_file(expanded_flat_file, 'expanded_flat.pl').

regulus_working_file(tmp_preds, 'tmp_preds.pl').

%---------------------------------------------------------------

set_regulus_runtime_config(Key, Value) :-
	retractall(regulus_runtime_config(Key, _)),
	assert(regulus_runtime_config(Key, Value)).

get_regulus_runtime_config(Key, Value) :-
	regulus_runtime_config(Key, Value0),
	!,
	Value = Value0.
get_regulus_runtime_config(Key, Value) :-
	regulus_runtime_config_default(Key, Value0),
	!,
	Value = Value0.

regulus_runtime_config_default(processing_mode, normal).
regulus_runtime_config_default(parser, lc).
regulus_runtime_config_default(translation_mode, interlingua).
%regulus_runtime_config_default(print_line_info, dont_print_line_info).
regulus_runtime_config_default(print_line_info, print_line_info).
regulus_runtime_config_default(print_tree_summary, dont_print_tree_summary).
regulus_runtime_config_default(print_tree_categories, dont_print_tree_categories).


