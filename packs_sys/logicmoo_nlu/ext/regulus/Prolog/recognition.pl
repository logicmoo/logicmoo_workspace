
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(recognition,
	  [do_nuance_compile_as_defined_by_config_file/1,
	   do_pcfg_training_as_defined_by_config_file/0,

	   do_nuance_compile_is_meaningful/0,
	   do_pcfg_training_is_meaningful/0,

	   recognition_resources_are_defined/0,
	   speech_output_resources_are_defined/0,

	   recognition_is_loaded/0,
	   tts_is_loaded/0,
	   
	   get_rec_params/1,
	   get_rec_params/2,
	   get_package_from_rec_params/2,
	   
	   start_recognition_resources/0,
	   start_recognition_resources/1,

	   restart_recognition_resources/0,
	   restart_recognition_resources/1,

	   start_regserver_checking_other_rec_resources_are_loaded/0,
	   start_regserver_checking_other_rec_resources_are_loaded/1,

	   set_wait_time_for_regserver/1,
	   
	   close_down_recognition_resources/0,
	   close_and_restart_recognition_including_recserver/0,

	   speak_translation_result_if_appropriate/1,
	   speak_dialogue_processing_result_if_appropriate/1,
	   postprocess_wavfile_output_atom/2,
	   
	   recognise_as_defined_by_config_file/1,
	   recognise_as_defined_by_config_file/2,
	   recognise_slm_as_defined_by_config_file/1,
	   
	   recognise_from_wavfile_as_defined_by_config_file/2,
	   recognise_from_wavfile_as_defined_by_config_file/3,
	   recognise_slm_from_wavfile_as_defined_by_config_file/2,
	   
	   nuance_parse_as_described_by_config_file/3,
	   recognise_for_gui_as_defined_by_config_file/1,
	   recognise_for_gui_from_wavfile_as_defined_by_config_file/2,
	   rec_result_and_transcription_to_batchrec_item/4,

	   interpret_string_as_wavfile_input/2,
	   get_recorded_wavfile_list/1,
	   show_recorded_wavfiles/1,
	   recorded_wavfile_list_for_gui/2
	  ]).

:- use_module('$REGULUS/Prolog/speech_output').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/java_gui_utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [shell/1, shell/2, working_directory/2] ) ) ).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%------------------------------------------------------------------------------------

:- dynamic recognition_is_loaded/0.

:- dynamic tts_is_loaded/0.

:- dynamic current_regserver_port/1.

store_current_regserver_port(Port) :-
	retractall(current_regserver_port(_)),
	assertz(current_regserver_port(Port)),
	format('~N--- Stored current regserver port: ~d~n', [Port]),
	!.

get_current_regserver_port(Port) :-
	(   current_regserver_port(Port) ->
	    format('~N--- Found current regserver port: ~d~n', [Port])
	;
	    format('~N--- No current regserver port defined~n', []),
	    fail
	).

%------------------------------------------------------------------------------------

do_nuance_compile_is_meaningful :-
	with_output_to_chars(get_nuance_grammar_for_compilation(_Grammar, no_pcfg), _Chars),
	!.
			     
do_pcfg_training_is_meaningful :-
	with_output_to_chars(( get_nuance_grammar_for_pcfg_training(_Grammar),
			       get_pcfg_training_data_file(_TrainingDataFile)
			     ),
			     _Chars),
	!.
	
%------------------------------------------------------------------------------------

do_nuance_compile_as_defined_by_config_file(PCFGOrNot) :-
	(  var(PCFGOrNot)
	;  \+ member(PCFGOrNot, [pcfg, no_pcfg])
	),
	!,
	format('~N*** Bad call: ~w. First argument must be "pcfg" or "no_pcfg"~n',
	       [do_nuance_compile_as_defined_by_config_file(PCFGOrNot)]),
	fail.
/*
cd <GrammarDir>
1) uconv -f ISO-8859-1 -t UTF-8 foo.grammar > foo_n10.grammar
2) convert_gsl -file foo_n10.grammar -lang en-US 
3) sgc -optimize 12 foo_n10.grxml

Result is foo_n10.gram
*/
do_nuance_compile_as_defined_by_config_file(PCFGOrNot) :-
	nuance10_loaded,
	!,
	check_config_file_is_loaded,
	get_nuance_grammar_for_compilation(N85GrammarFull, PCFGOrNot),
	get_n10_language_pack(LanguagePack),
	directory_and_file_for_pathname(N85GrammarFull, GrammarDir, N85Grammar),
	nuance_uconv_command(N85Grammar, _N85Converted, UConvCommand),
	nuance_convert_gsl_command(N85Grammar, LanguagePack, N10Grammar, ConvertGSLCommand),
	nuance_sgc_command(N10Grammar, N10CompileCommand),
	tmp_regulus_file('nuance_compile_output.txt', TraceFile1),
	tmp_regulus_file('nuance_compile_error_output.txt', TraceFile2),
	working_directory(OldWorkingDir, GrammarDir),
	% Using shell_writing_output_to_file_and_printing here doesn't work because of the redirection...
	% but there's normally nothing to see anyway, it's a silent operation.
	safe_shell(UConvCommand),
	shell_writing_output_to_file_and_printing(ConvertGSLCommand, TraceFile1, TraceFile2, 'Convert to GrXML'),
	shell_writing_output_to_file_and_printing(N10CompileCommand, TraceFile1, TraceFile2, 'Compile under Nuance 10'),
	working_directory(_, OldWorkingDir),
	!.
do_nuance_compile_as_defined_by_config_file(PCFGOrNot) :-
	check_config_file_is_loaded,
	get_nuance_grammar_for_compilation(Grammar, PCFGOrNot),
	get_language_pack(LanguagePack),
	get_nuance_compile_parameters(ParameterAtom),
	get_nuance_recognition_package_name(Grammar, PackageName),
	nuance_compile_command(Grammar, LanguagePack, ParameterAtom, PackageName, Command),
	directory_and_file_for_pathname(Grammar, GrammarDir, _GrammarBase),
	tmp_regulus_file('nuance_compile_output.txt', TraceFile1),
	tmp_regulus_file('nuance_compile_error_output.txt', TraceFile2),
	working_directory(OldWorkingDir, GrammarDir),
	shell_writing_output_to_file_and_printing(Command, TraceFile1, TraceFile2, 'Nuance compilation'),
	working_directory(_, OldWorkingDir),
	!.
do_nuance_compile_as_defined_by_config_file :-
	format2error('~N*** Error: unable to call nuance-compile~n', []),
	fail.

nuance_compile_command(Grammar, LanguagePack, ParameterAtom, PackageName, Command) :-
	format_to_atom('nuance-compile ~w ~w ~w -o ~w',
		       [Grammar, LanguagePack, ParameterAtom, PackageName],
		       Command).

/*
cd <GrammarDir>
1) uconv -f ISO-8859-1 -t UTF-8 foo.grammar > foo_n10.grammar
2) convert_gsl -file foo_n10.grammar -lang en-US 
3) sgc -optimize 12 foo_n10.grxml

Result is foo_n10.gram
*/

nuance_uconv_command(N85Grammar, N85ConvertedGrammar, Command) :-
	nuance_grammar_to_base_nuance_converted_grammar(N85Grammar, N85GrammarBase, N85ConvertedGrammar),
	format_to_atom('uconv -f ISO-8859-1 -t UTF-8 ~w.grammar > ~w',
		       [N85GrammarBase, N85ConvertedGrammar],
		       Command).

nuance_convert_gsl_command(N85Grammar, LanguagePack, N10Grammar, Command) :-
	nuance_grammar_to_base_nuance_converted_grammar(N85Grammar, _N85GrammarBase, N85ConvertedGrammar),
	nuance_grammar_to_base_nuance_grxml_grammar(N85Grammar, N10Grammar),
	format_to_atom('convert_gsl -file ~w -lang ~w',
		       [N85ConvertedGrammar, LanguagePack],
		       Command).

nuance_sgc_command(N10Grammar, Command) :-
	format_to_atom('sgc -optimize 12 ~w',
		       [N10Grammar],
		       Command).

nuance_grammar_to_base_nuance_converted_grammar(N85Grammar, N85GrammarBase, N85ConvertedGrammar) :-
	split_off_extension_from_pathname(N85Grammar, N85GrammarBase, _Extension),
	format_to_atom('~w_n10.grammar', [N85GrammarBase], N85ConvertedGrammar),
	!.

nuance_grammar_to_base_nuance_grxml_grammar(N85Grammar, N10Grammar) :-
	split_off_extension_from_pathname(N85Grammar, N85GrammarBase, _Extension),
	format_to_atom('~w_n10.grxml', [N85GrammarBase], N10Grammar),
	!.
 
%------------------------------------------------------------------------------------

do_pcfg_training_as_defined_by_config_file :-
	check_config_file_is_loaded,
	get_nuance_grammar_for_pcfg_training(Grammar),
	get_pcfg_training_data_file(TrainingDataFile),
	get_pcfg_training_output_directory(OutputDirectory),
	compute_grammar_probs_command(Grammar, TrainingDataFile, OutputDirectory, Command),
	tmp_regulus_file('compute_grammar_probs_output.txt', TraceFile1),
	tmp_regulus_file('compute_grammar_probs_error_output.txt', TraceFile2),	
	shell_writing_output_to_file_and_printing(Command, TraceFile1, TraceFile2, 'Nuance PCFG training'),
	!.
do_pcfg_training_as_defined_by_config_file :-
	format2error('~N*** Error: unable to do PCFG training~n', []),
	fail.	

% compute-grammar-probs -base_grammar_file ../Generated/recogniser -data grammar_probs_data_file.txt -output_dir ../Generated/pcfg_trained

compute_grammar_probs_command(Grammar, TrainingDataFile, OutputDirectory, Command) :-
	format_to_atom('compute-grammar-probs -base_grammar_file ~w -data ~w -output_dir ~w',
		       [Grammar, TrainingDataFile, OutputDirectory],
		       Command).

%------------------------------------------------------------------------------------

interpret_string_as_wavfile_input(Chars, Result) :-
	is_prolog_string(Chars),
	split_string_into_words(Chars, Chunks),
	(   Chunks = ['WAVFILE:' | RestChunks] 
	;
	    Chunks = ['WAVFILE', ':' | RestChunks]
	),
	!,
	append_atoms(RestChunks, 0' , WavfileAtom),
	(   safe_absolute_file_name(WavfileAtom, AbsWavfileAtom) ->
	    (   safe_file_exists(AbsWavfileAtom) ->
		Result = AbsWavfileAtom
	    ;
		otherwise ->
		format2error('~N*** Error: unable to find file "~w"~n', [AbsWavfileAtom]),
		Result = error
	    )
	;
	    otherwise ->
	    format2error('~N*** Error: unable to interpret "~w" as file name~n', [WavfileAtom]),
	    Result = error
	).	

%------------------------------------------------------------------------------------

recognition_resources_are_defined :-
	license_manager_file_exists,
	rec_params_are_defined,
	!.

%----------------------------------------------------------------------

speech_output_resources_are_defined :-
	(   tts_params_are_defined
	;   wavfile_table_is_defined
	).

%------------------------------------------------------------------------------------

start_recognition_resources :-
	default_regserver_port(RegserverPort),
	start_recognition_resources(RegserverPort).

start_recognition_resources(RegserverPort) :-
	user:regulus_config(only_use_regserver, yes),
	store_current_regserver_port(RegserverPort),
	kill_own_regserver_if_necessary,
	existing_recognition_processes(OldRecProcessList),
	get_rec_params(RecParams),
	start_recognition_resources_just_regserver(RecParams, RegserverPort),
	existing_recognition_processes(NewRecProcessList),
	store_own_regserver_pid(OldRecProcessList, NewRecProcessList),
	!.
start_recognition_resources(RegserverPort) :-
	\+ user:regulus_config(only_use_regserver, yes),
	store_current_regserver_port(RegserverPort),
	warn_if_dubious_path,
	kill_any_existing_recognition_processes,
	default_license_manager_file(LMFile),
	get_rec_params(RecParams),
	get_tts_command(TTSCommand),
	start_recognition_resources(LMFile, RecParams, RegserverPort, TTSCommand),
	!.

restart_recognition_resources :-
	restart_recognition_resources(_Action).

%restart_recognition_resources(Action) :-
%	get_current_regserver_port(RegserverPort),
%	format('~N--- Restarting recognition processes on port ~d~n',[RegserverPort]),
%	(   check_license_manager_and_recserver_processes_are_running ->
%	    format('~N--- Restarting just regserver, since recserver and license manager seem to be running~n',[]),
%	    start_regserver_checking_other_rec_resources_are_loaded(RegserverPort),
%	    Action = restarted_regserver_on_port(RegserverPort)
%	;
%	    otherwise ->
%	    format('~N--- Restarting all processes, since recserver and license manager do not seem to be running~n',[]),
%	    start_recognition_resources(RegserverPort),
%	    Action = restarted_all_recognition_resources_on_port(RegserverPort)
%	),
%	!.
restart_recognition_resources(Action) :-
	check_status_for_lm_recserver_and_regserver(Status),
	get_current_regserver_port(RegserverPort),
	format('~N--- Restarting recognition processes on port ~d~n',[RegserverPort]),
	start_recognition_resources(RegserverPort),
	Action = restarted_all_recognition_resources_on_port(RegserverPort, Status),
	!.
restart_recognition_resources(_Action) :-
	get_current_regserver_port(RegserverPort),
	format('~N*** Error: unsuccessfully tried to restart recognition processes on port ~d~n', [RegserverPort]),
	!.
restart_recognition_resources(_Action) :-
	format('~N*** Error: cannot restart recognition processes, no current port defined~n', []),
	!.


%------------------------------------------------------------------------------------

start_regserver_checking_other_rec_resources_are_loaded :-
	default_regserver_port(RegserverPort),
	start_regserver_checking_other_rec_resources_are_loaded(RegserverPort).

start_regserver_checking_other_rec_resources_are_loaded(RegserverPort) :-
	store_current_regserver_port(RegserverPort),
	get_rec_params(RecParams),
	start_recognition_resources_just_regserver(RecParams, RegserverPort),
	!.

%------------------------------------------------------------------------------------

default_wait_time_for_regserver(60).

:- dynamic wait_time_for_regserver/1.

get_wait_time_for_regserver(N) :-
	wait_time_for_regserver(N),
	!.
get_wait_time_for_regserver(N) :-
	default_wait_time_for_regserver(N).

set_wait_time_for_regserver(N) :-
	retractall(wait_time_for_regserver(_)),
	assertz(wait_time_for_regserver(N)).

%------------------------------------------------------------------------------------

default_regserver_port(1979).

default_license_manager_file(AbsFile) :-
	File = '$REGULUS/scripts/run_license.bat',
	safe_absolute_file_name(File, AbsFile),
	(   file_exists(AbsFile) ->
	    true
	;
	    format2error('~N*** Error: need to have a license manager file in "~w"~n', [File]),
	    fail
	).

license_manager_file_exists :-
	File = '$REGULUS/scripts/run_license.bat',
	safe_absolute_file_name(File, AbsFile),
	file_exists(AbsFile),
	!.

%----------------------------------------------------------------------

speak_dialogue_processing_result_if_appropriate([F | R]) :-
	speak_dialogue_processing_result_if_appropriate(F),
	!,
	speak_dialogue_processing_result_if_appropriate(R).
speak_dialogue_processing_result_if_appropriate(play_wavfile(Wavfile)) :-
	recognition_is_loaded,
	safe_absolute_file_name(Wavfile, AbsWavfile),
	regulus_sockettalk_say_list_atom(AbsWavfile).
speak_dialogue_processing_result_if_appropriate(Action) :-
	(   ( Action = tts(String), is_prolog_string(String) ) ->
	    true
	;
	    ( Action = tts(Atom), atom(Atom) ) ->
	    atom_codes(Atom, String)
	),
	recognition_is_loaded,
	tts_is_loaded,
	regulus_sockettalk_say_tts(String),
	!.
speak_dialogue_processing_result_if_appropriate(_Action).

%----------------------------------------------------------------------

speak_translation_result_if_appropriate(Atom) :-
	atom(Atom),
	recognition_is_loaded,
	(   wavfile_table_is_defined ->
	    atom_to_speech_output_form(Atom, WavfileAtom0),
	    postprocess_wavfile_output_atom(WavfileAtom0, WavfileAtom),
	    regulus_sockettalk_say_list_atom(WavfileAtom)
	;
	    tts_is_loaded ->
	    atom_codes(Atom, String),
	    regulus_sockettalk_say_tts(String)
	),
	!.
speak_translation_result_if_appropriate(_Atom).

postprocess_wavfile_output_atom(SpeechOutputAtom0, SpeechOutputAtom) :-
	atom_codes(SpeechOutputAtom0, SpeechOutputChars0),
	\+ is_contiguous_sublist("+text", SpeechOutputChars0),
	get_wavfile_directory(WavfileDir),
	add_wavfile_directory_to_speech_output_form(SpeechOutputAtom0, WavfileDir, SpeechOutputAtom),
	!.
postprocess_wavfile_output_atom(SpeechOutputAtom0, SpeechOutputAtom) :-
	format('~N*** Error: bad call: ~w~n',
	       [postprocess_wavfile_output_atom(SpeechOutputAtom0, SpeechOutputAtom)]),
	fail.

add_wavfile_directory_to_speech_output_form(SpeechOutputAtom0, WavfileDir, SpeechOutputAtom) :-
	split_atom_into_words(SpeechOutputAtom0, BaseWavfiles),
	expand_wavfile_list(BaseWavfiles, WavfileDir, FullWavfiles),
	append_atoms(FullWavfiles, 0',, SpeechOutputAtom).

expand_wavfile_list([], _WavfileDir, []).
expand_wavfile_list([F | R], WavfileDir, [F1 | R1]) :-
	expand_wavfile(F, WavfileDir, F1),
	!,
	expand_wavfile_list(R, WavfileDir, R1).

expand_wavfile(Base, WavfileDir, Expanded) :-
	format_to_atom('~w/~w.wav', [WavfileDir, Base], Expanded).

%------------------------------------------------------------------------------------

nuance_parse_as_described_by_config_file(SentAtom, GrammarAtom, Result) :-
	(   ( atom(SentAtom), atom(GrammarAtom) ) ->
	    true
	;
	    format2error('~N*** Bad call ~w~n',
			 [nuance_parse_as_described_by_config_file(SentAtom, GrammarAtom, Result)]),
	    fail
	),
	check_recognition_loaded_and_get_rec_params(_RecParams),
	regulus_sockettalk_interpret(SentAtom, GrammarAtom, Result).
 
%------------------------------------------------------------------------------------

recognise_from_wavfile_as_defined_by_config_file(Grammar, Wavfile, Recognised) :-
	on_exception(_Exception,
		     recognise_from_wavfile_as_defined_by_config_file1(Grammar, Wavfile, Recognised),
		     (   close_and_restart_recognition,
			 recognise_from_wavfile_as_defined_by_config_file1(Grammar, Wavfile, Recognised)
		     )
		    ).

recognise_from_wavfile_as_defined_by_config_file1(Grammar, Wavfile, Recognised) :-
	safe_absolute_file_name(Wavfile, AbsWavfile),
	check_recognition_loaded_and_get_rec_params(_RecParams),
	regulus_sockettalk_recognise_file(AbsWavfile, Grammar, Recognised).

recognise_from_wavfile_as_defined_by_config_file(Wavfile, Recognised) :-
	on_exception(_Exception,
		     recognise_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised),
		     (   close_and_restart_recognition,
			 recognise_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised)
		     )
		    ).

recognise_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised) :-
	safe_absolute_file_name(Wavfile, AbsWavfile),
	check_recognition_loaded_and_get_rec_params(RecParams),
	(   member(grammar=Grammar, RecParams) ->
	    regulus_sockettalk_recognise_file(AbsWavfile, Grammar, Recognised)
	;
	    format2error('~N*** Error: recognition parameters must define a grammar~n', []),
	    fail
	).

recognise_slm_from_wavfile_as_defined_by_config_file(Wavfile, Recognised) :-
	on_exception(_Exception,
		     recognise_slm_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised),
		     (   close_and_restart_recognition,
			 recognise_slm_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised)
		     )
		    ).

recognise_slm_from_wavfile_as_defined_by_config_file1(Wavfile, Recognised) :-
	safe_absolute_file_name(Wavfile, AbsWavfile),
	check_recognition_loaded_and_get_rec_params(slm, RecParams),
	(   RecParams = dummy ->
	    Recognised = dummy
	;
	    member(grammar=Grammar, RecParams) ->
	    regulus_sockettalk_recognise_file(AbsWavfile, Grammar, Recognised)
	;
	    format2error('~N*** Error: recognition parameters must define a grammar~n', []),
	    fail
	).

%------------------------------------------------------------------------------------

recognise_as_defined_by_config_file(Grammar, Recognised) :-
	on_exception(_Exception,
		     recognise_as_defined_by_config_file1(Grammar, Recognised),
		     (   close_and_restart_recognition,
			 recognise_as_defined_by_config_file1(Grammar, Recognised)
		     )
		    ).

recognise_as_defined_by_config_file1(Grammar, Recognised) :-
	check_recognition_loaded_and_get_rec_params(_RecParams),
	regulus_sockettalk_recognise(Grammar, Recognised),
	%format('~N~nRecognised:~n', []),
	%prettyprint(Recognised),
	format('~N~n', []),
	log_recognition_result(Recognised).

recognise_slm_as_defined_by_config_file(Recognised) :-
	on_exception(_Exception,
		     recognise_slm_as_defined_by_config_file1(Recognised),
		     (   close_and_restart_recognition,
			 recognise_slm_as_defined_by_config_file1(Recognised)
		     )
		    ).

recognise_slm_as_defined_by_config_file1(Recognised) :-
	check_recognition_loaded_and_get_rec_params(slm, RecParams),
	(   RecParams = dummy ->
	    Recognised = dummy
	;
	    member(grammar=Grammar, RecParams) ->
	    regulus_sockettalk_recognise(Grammar, Recognised),
	    %format('~N~nRecognised:~n', []),
	    %prettyprint(Recognised),
	    format('~N~n', []),
	    log_recognition_result(Recognised)
	;
	    format2error('~N*** Error: slm recognition parameters must define a grammar~n', []),
	    fail
	).

recognise_as_defined_by_config_file(Recognised) :-
	on_exception(_Exception,
		     recognise_as_defined_by_config_file1(Recognised),
		     (   close_and_restart_recognition,
			 recognise_as_defined_by_config_file1(Recognised)
		     )
		    ).

recognise_as_defined_by_config_file1(Recognised) :-
	check_recognition_loaded_and_get_rec_params(RecParams),
	(   member(grammar=Grammar, RecParams) ->
	    regulus_sockettalk_recognise(Grammar, Recognised),
	    %format('~N~nRecognised:~n', []),
	    %prettyprint(Recognised),
	    format('~N~n', []),
	    log_recognition_result(Recognised)
	;
	    format2error('~N*** Error: recognition parameters must define a grammar~n', []),
	    fail
	).

%------------------------------------------------------------------------------------

recognise_for_gui_as_defined_by_config_file(String) :-
	(   recognise_as_defined_by_config_file(Recognised) ->
	    postprocess_recognition_result_for_gui(Recognised, String)
	;
	    String = "ERROR: could not call recognition from Prolog"
	).

recognise_for_gui_from_wavfile_as_defined_by_config_file(WavfileString, String) :-
	format('~N--- Do recognition from wavfile: ', []),
	(   is_prolog_string(WavfileString) ->
	    format('"~s"~n', [WavfileString])
	;
	    format('(Error: wavfile spec ~w is not a string)~n', [WavfileString]),
	    fail
	),
	atom_codes(Wavfile, WavfileString),
	(   recognise_from_wavfile_as_defined_by_config_file(Wavfile, Recognised) ->
	    postprocess_recognition_result_for_gui(Recognised, String)
	;
	    String = "ERROR: could not call recognition from Prolog"
	).

%------------------------------------------------------------------------------------

:- dynamic own_regserver_pid/1.

kill_own_regserver_if_necessary :-
	own_regserver_pid(PID),
	Type = 'Regulus Speech Server (Regserver)',
	existing_recognition_processes(RecProcessList),
	member(rec_process(Type, PID), RecProcessList),
	format('~N--- Killing existing ~w process (PID = ~d)~n', [Type, PID]),
	kill_process(PID),
	!.
kill_own_regserver_if_necessary.
	       
store_own_regserver_pid(OldRecProcessList, NewRecProcessList) :-
	Type = 'Regulus Speech Server (Regserver)',
	member(rec_process(Type, PID), NewRecProcessList),
	\+ member(rec_process(Type, PID), OldRecProcessList),
	retractall(own_regserver_pid(_)),
	assertz(own_regserver_pid(PID)),
	format('~N--- Stored own Regserver PID: ~d~n', [PID]),
	!.
store_own_regserver_pid(_OldRecProcessList, _NewRecProcessList).

%------------------------------------------------------------------------------------

kill_any_existing_regserver_processes :-
	kill_any_existing_recognition_processes(['Regulus Speech Server (Regserver)']).

kill_any_existing_recognition_processes :-
	kill_any_existing_recognition_processes(['Nuance License Manager',
						 'Nuance Recserver',
						 'Regulus Speech Server (Regserver)',
						 'Vocalizer']).
	
%------------------------------------------------------------------------------------

kill_any_existing_recognition_processes(Types) :-
	existing_recognition_processes(List),
	kill_any_existing_recognition_processes1(List, Types),
	(   List \== [] ->
	    sleep(2)
	;
	    true
	),
	!.
kill_any_existing_recognition_processes(Types) :-
	format2error('~N*** Error: bad call: ~w~n', [kill_any_existing_recognition_processes(Types)]),
	fail.

kill_any_existing_recognition_processes1([], _Types).
kill_any_existing_recognition_processes1([F | R], Types) :-
	kill_recognition_process(F, Types),
	!,
	kill_any_existing_recognition_processes1(R, Types).

kill_recognition_process(rec_process(Type, PID), Types) :-
	(   member(Type, Types) ->
	    format('~N--- Killing existing ~w process (PID = ~d)~n', [Type, PID]),
	    kill_process(PID)
	;
	    otherwise ->
	    format('~N--- Existing ~w process (PID = ~d) found and ignored~n', [Type, PID])
	),
	!.
kill_recognition_process(Other, Types) :-
	format2error('~N*** Error: bad call: ~w~n', [kill_recognition_process(Other, Types)]),
	fail.

existing_recognition_processes(List) :-
	get_windows_ps_info(PSInfo),
	existing_recognition_processes1(PSInfo, List),
	!.
existing_recognition_processes(List) :-
	format2error('~N*** Error: bad call: ~w~n', [existing_recognition_processes(List)]),
	fail.

existing_recognition_processes1([], []).
existing_recognition_processes1([F | R], [rec_process(Type, PID) | R1]) :-
	rec_process_line(F, Type, PID),
	!,
	existing_recognition_processes1(R, R1).
existing_recognition_processes1([_F | R], R1) :-
	!,
	existing_recognition_processes1(R, R1).

rec_process_line(PSLine, Type, PID) :-
	member(command=CommandAtom, PSLine),
	member(pid=PID, PSLine),
	atom_codes(CommandAtom, CommandChars),
	rec_process_command_string(Type, String),
	is_contiguous_sublist(String, CommandChars),
	!.

rec_process_command_string('Nuance License Manager', "nlm.exe").
rec_process_command_string('Nuance Recserver', "recserver.exe").
rec_process_command_string('Regulus Speech Server (Regserver)', "regserver.exe").
rec_process_command_string('Vocalizer', "vocalizer.exe").

%------------------------------------------------------------------------------------

start_recognition_resources_just_regserver(RecParams, RegserverPort) :-
	retractall(recognition_is_loaded),
	check_license_manager_and_recserver_processes_are_running,
	format('~N--- License manager and recserver processes are running...~n', []),
	(   start_regserver(RecParams, RegserverPort) ->
	    assertz(recognition_is_loaded)
	;
	    otherwise ->
	    format2error('~N*** Error: unable to start Regserver~n', [])
	),
	!.
	
%------------------------------------------------------------------------------------

start_recognition_resources(LMFile, RecParams, RegserverPort, TTSCommand) :-
	retractall(recognition_is_loaded),
	start_license_manager(LMFile),
	start_tts(TTSCommand),
	start_recserver(RecParams),
	get_wait_time_for_regserver(WaitTime),
	format('~N--- Waiting ~d seconds before starting regserver...~n', [WaitTime]),
	sleep(WaitTime),
	check_license_manager_and_recserver_processes_are_running,
	format('~N--- License manager and recserver processes are running...~n', []),
	(   start_regserver(RecParams, RegserverPort) ->
	    assertz(recognition_is_loaded)
	;
	    otherwise ->
	    format2error('~N*** Error: unable to start Regserver~n', []),
	    close_down_recognition_resources
	),
	!.
start_recognition_resources(LMFile, RecParams, RegserverPort, TTSCommand) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [start_recognition_resources(LMFile, RecParams, RegserverPort, TTSCommand)]),
	fail.

%------------------------------------------------------------------------------------

check_status_for_lm_recserver_and_regserver(Status) :-
	existing_recognition_processes(List),
	(   member(rec_process('Nuance License Manager', _), List) ->
	    LMStatus = yes
	;
	    otherwise ->
	    LMStatus = no
	),
	(   member(rec_process('Nuance Recserver', _), List) ->
	    RecServerStatus = yes
	;
	    otherwise ->
	    RecServerStatus = no
	),
	(   member(rec_process('Regulus Speech Server (Regserver)', _), List) ->
	    RegServerStatus = yes
	;
	    otherwise ->
	    RegServerStatus = no
	),
	Status = [(lm = LMStatus), (recserver = RecServerStatus), (regserver = RegServerStatus) ],
	!.

%------------------------------------------------------------------------------------
	

check_license_manager_and_recserver_processes_are_running :-
	existing_recognition_processes(List),
	(   member(rec_process('Nuance License Manager', _PID1), List) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: no license manager process is running~n', []),
	    fail
	),
	(   member(rec_process('Nuance Recserver', _PID2), List) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: no recserver process is running~n', []),
	    fail
	),
	!.

%------------------------------------------------------------------------------------

close_and_restart_recognition_including_recserver :-
	format('~N--- Closing and restarting recognition processes including recserver~n',[]),
	close_down_recognition_resources,
	restart_recognition_resources.

close_and_restart_recognition :-
	format('~N--- Closing and restarting regserver but NOT recserver~n',[]),
	%close_down_recognition_resources,
	close_down_regserver,
	kill_any_existing_regserver_processes,
	restart_recognition_resources.
	
%------------------------------------------------------------------------------------

close_down_recognition_resources :-
	close_down_regserver,
	kill_any_existing_recognition_processes,
	retractall(recognition_is_loaded),
	retractall(tts_is_loaded),
	unset_current_record_directory_logfile.

%------------------------------------------------------------------------------------

close_down_regserver :-
	regulus_sockettalk_exit_client,
	format('~N--- Closed down regserver~n', []),
	!.
close_down_regserver.

%------------------------------------------------------------------------------------

start_license_manager(LMFile) :-
	get_license_manager_command_from_file(LMFile, Command),
	format('~N--- Invoking command "~w"~n', [Command]),
	exec_and_check_process_started(Command, 5, _PID),
	format('~N--- Started license manager~n', []),
	!.
start_license_manager(_LMFile) :-
	format2error('~N*** Error: unable to start license manager~n', []),
	fail.

get_license_manager_command_from_file(LMFile, Command) :-
	read_file_to_atom_list(LMFile, List),
	get_license_manager_command_from_list(List, Command),
	!.
get_license_manager_command_from_file(LMFile, _Command) :-
	format2error('~N*** Error: unable to find license manager command in ~w. Command should be of form "nlm <LicenseCode>"~n', [LMFile]),
	fail.

get_license_manager_command_from_list(List, Command) :-
	member(Command, List),
	split_atom_into_words(Command, Words),
	Words = [nlm | _],
	!.

%------------------------------------------------------------------------------------

start_recserver(RecParams) :-
	get_package_from_rec_params(RecParams, Package),
	get_external_packages_if_any(ExternalPackages),
	make_package_arguments([Package | ExternalPackages], PackageArguments),
	format_to_atom('recserver ~w', [PackageArguments], Command),
	format('~N--- Invoking command "~w"~n', [Command]),
	exec_and_check_process_started(Command, 10, _PID),
	format('~N--- Started recserver~n', []),
	!.
start_recserver(_RecParams) :-
	format2error('~N*** Error: unable to start recserver~n', []),
	fail.

get_external_packages_if_any(ExternalPackages) :-
	check_config_file_is_loaded,
	user:regulus_config(external_packages, ExternalPackages0),
	(   is_list(ExternalPackages0) ->
	    get_external_packages_from_list(ExternalPackages0, ExternalPackages)
	;
	    format2error('~N*** Error: external_packages value ~w is not a list~n', [ExternalPackages]),
	    fail
	),
	!.
get_external_packages_if_any([]).

get_external_packages_from_list([], []).
get_external_packages_from_list([F | R], [F1 | R1]) :-
	get_package(F, F1),
	!,
	get_external_packages_from_list(R, R1).

make_package_arguments([], '') :-
	!.
make_package_arguments([F | R], Args) :-
	make_package_arguments(R, RestArgs),
	format_to_atom('-package ~w ~w', [F, RestArgs], Args),
	!.

%------------------------------------------------------------------------------------

start_tts(Command) :-
	(   tts_params_are_defined ->
	    format('~N--- Invoking command "~w"~n', [Command]),
	    (   exec_and_check_process_started(Command, 10, _PID) ->
		format('~N--- Started TTS~n', []),
		assertz(tts_is_loaded)
	    ;
		otherwise ->
		format('~N--- Unable to start TTS, but continuing anyway~n', [])
	    )
	;
	    true
	).

%------------------------------------------------------------------------------------

start_regserver(RecParams, Port) :-
	get_package_from_rec_params(RecParams, Package),
	(   consume_parameter_value_pair(RecParams, package=_, RecParamsRest) ->
	    true
	;
	    format2error('~N*** Error: bad parameter list ~w~n', [RecParams]),
	    fail
	),
	% Nuance requires record dir to be a relative pathname,
	% so specify it relative to the Regulus root dir
	% and make that the current working dir.
	%set_working_directory_to_regulus_root_dir,
	set_working_directory_to_recorded_wavfiles_parent_dir,
	add_record_directory_to_params_if_necessary(RecParamsRest, RecordDir, RecParamsRest1),
	start_new_record_directory_logfile(RecordDir),
	(   parameter_list_to_atom(RecParamsRest1, ParametersAtom) ->
	    regulus_sockettalk_init(Port, Package, ParametersAtom)
	;
	    format2error('~N*** Error: bad parameter list ~w~n', [RecParams]),
	    fail
	),
	!.
start_regserver(_RecParams, _Port) :-
	format2error('~N*** Error: unable to start regserver~n', []),
	fail.

%------------------------------------------------------------------------------------

set_working_directory_to_recorded_wavfiles_parent_dir :-
	%absolute_file_name('$REGULUS', Dir),
	get_recorded_wavfiles_parent_directory(Dir0),
	absolute_file_name(Dir0, Dir),
	working_directory(_, Dir),
	format('~N--- Setting current working directory to ~w~n', [Dir]).

%------------------------------------------------------------------------------------

get_package_from_rec_params(RecParams, Package) :-
	(   member(package=Package0, RecParams) ->
	    true
	;
	    format2error('~N*** Error: no package defined in ~w~n', [RecParams]),
	    fail
	),
	get_package(Package0, Package).

get_package(Package0, Package) :-
	(   safe_absolute_file_name(Package0, Package) ->
	    true
	;
	    format2error('~N*** Error: cannot interpret ~w as pathname~n', [Package0]),
	    fail
	),
	(   safe_is_directory(Package) ->
	    true
	;
	    format2error('~N*** Error: unable to find package ~w~n', [Package]),
	    fail
	).

%------------------------------------------------------------------------------------

add_record_directory_to_params_if_necessary(Params, AbsDir, Params) :-
	member('client.RecordDirectory' = Dir, Params),
	safe_absolute_file_name(Dir, AbsDir),
	!.
% Very messy: if we're specifying the record directory parent explicitly, it's
% because we're starting from a script file, and we have problems with interactions
% between Nuance and SICStus with relative pathnames. In this case, we need
% to prepend the record directory parent (which must be a relative pathname)
% to the record dir we pass to the Regserver.
add_record_directory_to_params_if_necessary(ParamsIn, AbsRecordDirectory, ParamsOut) :-
	get_timestamped_record_dir(RecordDirectory),
	%format_to_atom('$REGULUS/~w', [RecordDirectory], RecordDirectory1),
	get_recorded_wavfiles_parent_directory(ParentDir),
	format_to_atom('~w/~w', [ParentDir, RecordDirectory], RecordDirectory1),
	safe_absolute_file_name(RecordDirectory1, AbsRecordDirectory),
	(   record_directory_parent_explicitly_specified ->
	    RecordDirectoryToUse = RecordDirectory1
	;
	    otherwise ->
	    RecordDirectoryToUse = RecordDirectory
	),
	append(ParamsIn, ['client.RecordDirectory' = RecordDirectoryToUse], ParamsOut),
	!.
add_record_directory_to_params_if_necessary(ParamsIn, _Dir, ParamsOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_record_directory_to_params_if_necessary(ParamsIn, ParamsOut)]),
	fail.

%------------------------------------------------------------------------------------

postprocess_recognition_result_for_gui(Result, String) :-
	Result = recognition_failed(_),
	String = "ERROR: recognition failed",
	!.
postprocess_recognition_result_for_gui(Result, String) :-
	Result = recognition_succeeded(_Conf, Atom, _LF),
	atom(Atom),
	atom_codes(Atom, String0),
	replace_characters_in_string_for_gui(String0, String),
	!.
postprocess_recognition_result_for_gui(Result, String) :-
	Result = recognition_succeeded(NBestList),
	is_list(NBestList),
	%String = "ERROR: unable to handle N-best recognition",
	NBestList = [First | _Rest],
	First = rec_result(Conf, Atom, LF),
	postprocess_recognition_result_for_gui(recognition_succeeded(Conf, Atom, LF), String),
	!.
postprocess_recognition_result_for_gui(Result, String) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [postprocess_recognition_result_for_gui(Result, String)]),
	fail.

%------------------------------------------------------------------------------------

check_recognition_loaded_and_get_rec_params(RecParams) :-
	check_recognition_loaded_and_get_rec_params(default, RecParams).

check_recognition_loaded_and_get_rec_params(Type, RecParams) :-
	(   recognition_is_loaded ->
	    true
	;
	    get_rec_params(Type, _SomeRecParams) ->
	    format2error('~N*** Error: recognition resources not loaded. Use LOAD_RECOGNITION to load them.~n', []),
	    fail
	;
	    format2error('~N*** Error: no recognition parameters defined in config file.~n', []),
	    fail
	),
	get_rec_params(Type, RecParams),
	!.

%---------------------------------------------------------------

rec_result_and_transcription_to_batchrec_item(RecResult, AbsWavfile, Transcription, BatchrecItem) :-
	RecResult = recognition_failed(_),
	(   Transcription = '*no_transcription*' ->
	    TranscriptionWords = ['*no_transcription*']
	;
	    Transcription = TranscriptionWords
	),
	List = [wavfile=AbsWavfile, transcription=TranscriptionWords, confidence=0, words=['<rejected>']],
	BatchrecItem = batchrec_item(List).
rec_result_and_transcription_to_batchrec_item(RecResult, AbsWavfile, Transcription, BatchrecItem) :-
	RecResult = recognition_succeeded(Conf, RecognisedAtom, _LF),
	split_atom_into_words(RecognisedAtom, RecognisedWords),
	(   Transcription = '*no_transcription*' ->
	    append(RecognisedWords, ['(guessed)'], TranscriptionWords)
	;
	    Transcription = TranscriptionWords
	),
	List = [wavfile=AbsWavfile, transcription=TranscriptionWords, confidence=Conf, words=RecognisedWords],
	BatchrecItem = batchrec_item(List).
rec_result_and_transcription_to_batchrec_item(RecResult, AbsWavfile, Transcription, BatchrecItem) :-
	RecResult = recognition_succeeded(NBestList),
	is_list(NBestList),
	nbest_list_to_batchrec_item_list(NBestList, AbsWavfile, Transcription, BatchrecList),
	BatchrecItem = batchrec_item(BatchrecList).

nbest_list_to_batchrec_item_list([], _AbsWavfile, _Transcription, []).
nbest_list_to_batchrec_item_list([F | R], AbsWavfile, Transcription, List) :-
	F = rec_result(Conf, RecognisedAtom, _LF),
	rec_result_and_transcription_to_batchrec_item(recognition_succeeded(Conf, RecognisedAtom, null_lf),
						      AbsWavfile, Transcription, batchrec_item(FirstList)),
	nbest_list_to_batchrec_item_list(R, AbsWavfile, Transcription, RestList),
	append(FirstList, RestList, List).

%------------------------------------------------------------------------------------

rec_params_are_defined :-
	rec_params_are_defined(default).

rec_params_are_defined(Other) :-
	nonvar(Other),
	\+ member(Other, [default, slm]),
	!,
	format2error('~N*** Error: unknown argument "~w" to rec_params_are_defined/1. Must be "default" or "slm"~n', [Other]),
	fail.
rec_params_are_defined(default) :-
	check_config_file_is_loaded,
	(   user:regulus_config(translation_rec_params, _RecParams)
	;   user:regulus_config(dialogue_rec_params, _RecParams)
	),
	!.
rec_params_are_defined(slm) :-
	check_config_file_is_loaded,
	user:regulus_config(slm_rec_params, _RecParams),
	!.

get_rec_params(RecParams) :-
	get_rec_params(default, RecParams).

get_rec_params(Other, _RecParams) :-
	nonvar(Other),
	\+ member(Other, [default, slm]),
	!,
	format2error('~N*** Error: unknown first argument "~w" to get_rec_params/2. Must be "default" or "slm"~n', [Other]),
	fail.
get_rec_params(default, RecParams) :-
	check_config_file_is_loaded,
	user:regulus_config(translation_rec_params, RecParams),
	!.
get_rec_params(default, RecParams) :-
	check_config_file_is_loaded,
	user:regulus_config(dialogue_rec_params, RecParams),
	!.
get_rec_params(default, _RecParams) :-
	format2error('~N*** Error: need to define either "translation_rec_params" or "dialogue_rec_params"~n', []),
	fail.
get_rec_params(slm, RecParams) :-
	check_config_file_is_loaded,
	user:regulus_config(slm_rec_params, RecParams),
	!.
get_rec_params(slm, _RecParams) :-
	format2error('~N*** Error: need to define "slm_rec_params"~n', []),
	fail.


%------------------------------------------------------------------------------------

tts_params_are_defined :-
	check_config_file_is_loaded,
	user:regulus_config(tts_command, _Command).

get_tts_command(TTSCommand) :-
	check_config_file_is_loaded,
	user:regulus_config(tts_command, TTSCommand),
	!.
get_tts_command('*no_tts_command*').

%------------------------------------------------------------------------------------

get_nuance_grammar_for_compilation(AbsGrammar, PCFGOrNot) :-
	check_config_file_is_loaded,
	get_nuance_grammar_for_compilation1(Grammar, PCFGOrNot),
	safe_absolute_file_name(Grammar, AbsGrammar),
	format_to_atom('~w.grammar', [AbsGrammar], ActualGrammarFile),
	!,
	(   safe_file_exists(ActualGrammarFile) ->
	    true
	;
	    otherwise ->
	    format2error('~N*** Error: Nuance grammar file "~w" does not exist~n', [AbsGrammar])
	).
get_nuance_grammar_for_compilation(_AbsGrammar, _PCFGOrNot) :-
	format2error('~N*** Error: unable to find Nuance grammar for compilation~n', []),
	fail.

get_nuance_grammar_for_compilation1(Grammar, _PCFGOrNot) :-
	user:regulus_config(nuance_grammar_for_compilation, Grammar),
	!.
get_nuance_grammar_for_compilation1(_Grammar, no_pcfg) :-
	user:regulus_config(ebl_nuance_grammar, _Grammar1),
	user:regulus_config(nuance_grammar, _Grammar2),
	!,
	format2error('~N*** Error: both "ebl_nuance_grammar" and "nuance_grammar" defined. Need to define "nuance_grammar_for_compilation".~n', []),
	fail.
get_nuance_grammar_for_compilation1(Grammar, no_pcfg) :-
	user:regulus_config(nuance_grammar, Grammar),
	!.
get_nuance_grammar_for_compilation1(Grammar, no_pcfg) :-
	user:regulus_config(ebl_nuance_grammar, Grammar),
	!.
get_nuance_grammar_for_compilation1(_Grammar, no_pcfg) :-
	!,
	format2error('~N*** Error: need to define one of "ebl_nuance_grammar", "nuance_grammar" or "nuance_grammar_for_compilation".~n', []),
	fail.
get_nuance_grammar_for_compilation1(_Grammar, pcfg) :-
	!,
	format2error('~N*** Error: need to define "nuance_grammar_for_compilation".~n', []),
	fail.

%------------------------------------------------------------------------------------

get_nuance_grammar_for_pcfg_training(AbsGrammar) :-
	check_config_file_is_loaded,
	get_nuance_grammar_for_pcfg_training1(Grammar),
	safe_absolute_file_name(Grammar, AbsGrammar),
	format_to_atom('~w.grammar', [AbsGrammar], ActualGrammarFile),
	!,
	(   safe_file_exists(ActualGrammarFile) ->
	    true
	;
	    otherwise ->
	    format2error('~N*** Error: Nuance grammar file "~w" does not exist~n', [AbsGrammar])
	).
get_nuance_grammar_for_pcfg_training(_AbsGrammar) :-
	format2error('~N*** Error: unable to find Nuance grammar for PCFG training~n', []),
	fail.

get_nuance_grammar_for_pcfg_training1(Grammar) :-
	user:regulus_config(nuance_grammar_for_pcfg_training, Grammar),
	!.
get_nuance_grammar_for_pcfg_training1(_Grammar) :-
	user:regulus_config(ebl_nuance_grammar, _Grammar1),
	user:regulus_config(nuance_grammar, _Grammar2),
	!,
	format2error('~N*** Error: both "ebl_nuance_grammar" and "nuance_grammar" defined. Need to define "nuance_grammar_for_pcfg_training".~n', []),
	fail.
get_nuance_grammar_for_pcfg_training1(Grammar) :-
	user:regulus_config(nuance_grammar, Grammar),
	!.
get_nuance_grammar_for_pcfg_training1(Grammar) :-
	user:regulus_config(ebl_nuance_grammar, Grammar),
	!.
get_nuance_grammar_for_pcfg_training1(_Grammar) :-
	!,
	format2error('~N*** Error: need to define one of "ebl_nuance_grammar", "nuance_grammar" or "nuance_grammar_for_pcfg_training".~n', []),
	fail.

%------------------------------------------------------------------------------------

get_pcfg_training_output_directory(AbsOutputDirectory) :-
	check_config_file_is_loaded,
	user:regulus_config(pcfg_training_output_directory, OutputDirectory),
	safe_absolute_file_name(OutputDirectory, AbsOutputDirectory),
	!.
get_pcfg_training_output_directory(OutputDirectory) :-
	get_nuance_grammar_for_compilation(FinalGrammar, pcfg),
	directory_and_file_for_pathname(FinalGrammar, OutputDirectory, _FinalGrammarFile),
	!.

%------------------------------------------------------------------------------------

get_pcfg_training_data_file(AbsTrainingDataFile) :-
	check_config_file_is_loaded,
	(   user:regulus_config(ebl_grammar_probs, TrainingDataFile) ->
	    true
	;
	    format2error('~N*** Error: need to define "ebl_grammar_probs"~n', []),
	    fail
	),
	safe_absolute_file_name(TrainingDataFile, AbsTrainingDataFile),
	(   safe_file_exists(AbsTrainingDataFile) ->
	    true
	;
	    format2error('~N*** Error: PCFG training data file "~w" does not exist~n', [AbsTrainingDataFile]),
	    fail
	).

%------------------------------------------------------------------------------------

get_language_pack(LanguagePack) :-
	user:regulus_config(nuance_language_pack, LanguagePack),
	!,
	(   atom(LanguagePack) ->
	    true
	;
	    format2error('~N*** Error: value of "nuance_language_pack", "~w" is not an atom~n', [LanguagePack]),
	    fail
	).
get_language_pack(_LanguagePack) :-
	format2error('~N*** Error: need to define "nuance_language_pack".~n', []),
	fail.

%------------------------------------------------------------------------------------

get_n10_language_pack(LanguagePack) :-
	user:regulus_config(nuance_10_language_pack, LanguagePack),
	!,
	(   atom(LanguagePack) ->
	    true
	;
	    format2error('~N*** Error: value of "nuance_10_language_pack", "~w" is not an atom~n', [LanguagePack]),
	    fail
	).
get_n10_language_pack(_LanguagePack) :-
	format2error('~N*** Error: need to define "nuance_10_language_pack".~n', []),
	fail.

%------------------------------------------------------------------------------------

get_nuance_compile_parameters(ParameterAtom) :-
	user:regulus_config(nuance_compile_params, ParameterList),
	!,
	(   parameter_list_to_atom(ParameterList, ParameterAtom) ->
	    true
	;
	    format2error('~N*** Error: bad value for "nuance_compile_params", "~w"~n', [ParameterList]),
	    fail
	),
	warn_if_no_dont_flatten(ParameterAtom).
get_nuance_compile_parameters('').

warn_if_no_dont_flatten(ParameterAtom) :-
	atom_codes(ParameterAtom, ParameterChars),
	(   is_substring("-dont_flatten", ParameterChars) ->
	    true
	;
	    format('~N*** Warning: nuance-compile parameters "~w" have no "-dont_flatten" parameter.', [ParameterAtom]),
	    format('~N*** This is often needed for Regulus grammars.~n', [])
	).

get_nuance_recognition_package_name(_Grammar, AbsPackageName) :-
	check_config_file_is_loaded,
	user:regulus_config(nuance_recognition_package, PackageName),
	safe_absolute_file_name(PackageName, AbsPackageName),
	!.
get_nuance_recognition_package_name(Grammar, Grammar).

%------------------------------------------------------------------------------------

get_wavfile_directory(AbsWavfileDir) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(wavfile_directory, WavfileDir),
	safe_absolute_file_name(WavfileDir, AbsWavfileDir),
	!.

%---------------------------------------------------------------

get_recorded_wavfiles_parent_directory(Dir) :-
	current_predicate(user:recorded_wavfiles_parent_directory/1),
	user:recorded_wavfiles_parent_directory(Dir),
	!.
get_recorded_wavfiles_parent_directory('$REGULUS') :-
	!.
get_recorded_wavfiles_parent_directory(Other) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_recorded_wavfiles_parent_directory(Other)]),
	fail.

record_directory_parent_explicitly_specified :-
	current_predicate(user:recorded_wavfiles_parent_directory/1),
	user:recorded_wavfiles_parent_directory(_Dir),
	!.

%---------------------------------------------------------------

get_timestamped_record_dir(RecordDirectory) :-
	create_recorded_wavfiles_directory_if_necessary,
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	get_recorded_wavfiles_parent_directory(ParentDir),
	format_to_atom('recorded_wavfiles/~w', [Timestamp], RecordDirectory),
	%format_to_atom('$REGULUS/recorded_wavfiles/~w', [Timestamp], RecordDirectory1),
	format_to_atom('~w/recorded_wavfiles/~w', [ParentDir, Timestamp], RecordDirectory1),
	absolute_file_name(RecordDirectory1, AbsRecordDirectory1),
	format('~N--- Making directory ~w~n', [AbsRecordDirectory1]),
	make_directory(AbsRecordDirectory1).

create_recorded_wavfiles_directory_if_necessary :-
	get_recorded_wavfiles_parent_directory(ParentDir),
	%absolute_file_name('$REGULUS/recorded_wavfiles', Dir),
	format_to_atom('~w/recorded_wavfiles/', [ParentDir], Dir0),
	absolute_file_name(Dir0, Dir),
	(   safe_directory_exists(Dir) ->
	    true
	;
	    otherwise ->
	    make_directory(Dir)
	),
	!.
create_recorded_wavfiles_directory_if_necessary :-
	format2error('~N*** Error: bad call: ~w~n', [create_recorded_wavfiles_directory_if_necessary]),
	fail.

%------------------------------------------------------------------------------------
 
start_new_record_directory_logfile(RecordDir) :-
	format_to_atom('~w/logfile.pl', [RecordDir], Logfile),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	safe_absolute_file_name(Logfile, AbsLogfile),
	format('~N--- Starting logfile ~w~n', [AbsLogfile]),
	open(AbsLogfile, write, S),
	format(S, '% Starting logfile at ~w~n', [Timestamp]),
	close(S),
	set_current_record_directory_logfile(Logfile).

log_recognition_result(RecResult) :-
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	get_most_recent_recorded_wavfile(Wavfile),
	clean_up_recorded_wavfile_name(Wavfile, Wavfile1),
	Entry = recognition_event(Timestamp, Wavfile1, RecResult),
	add_entry_to_record_directory_logfile(Entry),
	!.
log_recognition_result(RecResult) :-
	format2error('~N*** Error: bad call: ~w~n', [log_recognition_result(RecResult)]),
	fail.

% File is originally in Windows format and relative to the wavfiles parent dir
clean_up_recorded_wavfile_name(WavfileIn, WavfileOut) :-
	%format_to_atom('$REGULUS\\~w', [WavfileIn], WavfileNext),
	get_recorded_wavfiles_parent_directory(ParentDir),
	format_to_atom('~w\\~w', [ParentDir, WavfileIn], WavfileNext),
	safe_absolute_file_name(WavfileNext, WavfileOut),
	!.
clean_up_recorded_wavfile_name(WavfileIn, WavfileOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [clean_up_recorded_wavfile_name(WavfileIn, WavfileOut)]),
	fail.

%------------------------------------------------------------------------------------

add_entry_to_record_directory_logfile(Entry) :-
	get_current_record_directory_logfile(Logfile),
	open(Logfile, append, S),
	format(S, '~q.~n', [Entry]),
	close(S),
	!.

:- dynamic current_record_directory_logfile/1.
	
set_current_record_directory_logfile(Logfile) :-
	retractall(current_record_directory_logfile(_)),
	assertz(current_record_directory_logfile(Logfile)),
	!.

unset_current_record_directory_logfile :-
	retractall(current_record_directory_logfile(_)),
	!.

get_current_record_directory_logfile(Logfile) :-
	current_record_directory_logfile(Logfile),
	!.
get_current_record_directory_logfile(Logfile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_current_record_directory_logfile(Logfile)]),
	fail.

%---------------------------------------------------------------

get_most_recent_recorded_wavfile(Wavfile) :-
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile),
	!.
get_most_recent_recorded_wavfile(Wavfile) :-
	format2error('~N*** Error: bad call: ~w~n', [get_most_recent_recorded_wavfile(Wavfile)]),
	fail.

%---------------------------------------------------------------

recorded_wavfile_list_for_gui(N, Result) :-
	get_recorded_wavfile_list(List),
	firstn_or_all(List, N, List1),
	recorded_wavfile_list_for_gui1(List1, List2),
	Result =.. [list | List2],
	!.
recorded_wavfile_list_for_gui(N, Result) :-
	format2error('~N*** Error: bad call: ~w~n', [recorded_wavfile_list_for_gui(N, Result)]),
	fail.

recorded_wavfile_list_for_gui1([], []).
recorded_wavfile_list_for_gui1([F | R], [F1 | R1]) :-
	package_recorded_wavfile_item_for_gui(F, F1),
	!,
	recorded_wavfile_list_for_gui1(R, R1).


%---------------------------------------------------------------

show_recorded_wavfiles(N) :-
	get_recorded_wavfile_list(List),
	(   List = [] ->
	    format('~N~n--- There are no recorded wavfiles to show~n', [])
	;
	    otherwise ->
	    firstn_or_all(List, N, List1),
	    length(List1, NShown),
	    format('~N~n--- Showing ~d wavfiles:~n~n', [NShown]),
	    show_recorded_wavfiles1(List1)
	),
	!.
show_recorded_wavfiles(N) :-
	format2error('~N*** Error: bad call: ~w~n', [show_recorded_wavfiles(N)]),
	fail.

show_recorded_wavfiles1([]).
show_recorded_wavfiles1([F | R]) :-
	show_recorded_wavfile(F),
	!,
	show_recorded_wavfiles1(R).

/*
recognition_event('2008-04-24_22-36-40',
		  'c:/cygwin/home/speech/regulus/recorded_wavfiles/2008-04-24_22-36-03/utt01.wav',
		  recognition_succeeded(43,
					'switch on the light in the kitchen',
					[value=[[utterance_type,command],[action,switch],
						[onoff,on],[device,light],[location,kitchen]]])
		 ).
*/

package_recorded_wavfile_item_for_gui(Record, GUIRecord) :-
	words_for_recognition_event(Record, Words),
	Record = recognition_event(Timestamp, File, _RecResult),
	atom_codes(Timestamp, TimestampString),
	atom_codes(File, FileString),
	atom_codes(Words, WordsString),
	GUIRecord = recognition_event(TimestampString, FileString, WordsString),
	!.
package_recorded_wavfile_item_for_gui(Record, GUIRecord) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [package_recorded_wavfile_item_for_gui(Record, GUIRecord)]),
	fail.

show_recorded_wavfile(Record) :-
	words_for_recognition_event(Record, Words),
	Record = recognition_event(Timestamp, File, _RecResult),
	format('~N      File: ~w~n', [File]),
	format('~N Timestamp: ~w~n', [Timestamp]),
	format('~N     Words: ~w~n~n', [Words]),
	!.
show_recorded_wavfile(Record) :-
	format2error('~N*** Error: bad call: ~w~n', [show_recorded_wavfile(Record)]),
	fail.

words_for_recognition_event(recognition_event(_Timestamp, File, _RecResult), Words) :-
	get_transcription_from_wavfile(File, TranscriptionWords),
	join_with_spaces(TranscriptionWords, Words0),
	format_to_atom('"~w"', [Words0], Words),
	!.
words_for_recognition_event(recognition_event(_Timestamp, _File, RecResult), Words) :-
	RecResult = recognition_succeeded(_Conf, Words0, _LF),
	format_to_atom('"~w (recognised)"', [Words0], Words),
	!.
words_for_recognition_event(recognition_event(_Timestamp, _File, RecResult), Words) :-
	RecResult = recognition_succeeded(NBestList),
	is_list(NBestList),
	NBestList = [rec_result(_Conf, Words0, _LF) | _],
	format_to_atom('"~w (recognised)"', [Words0], Words),
	!.
words_for_recognition_event(_, '(unable to determine)') :-
	!.

%---------------------------------------------------------------

get_recorded_wavfile_list(List) :-
	get_all_recorded_wavfiles_logfiles(Logfiles),
	findall(LogfileContents,
		( member(Logfile, Logfiles), prolog_file_to_list(Logfile, LogfileContents) ),
		LogfileContentsList),
	append_list(LogfileContentsList, List0),
	sort(List0, List1),
	reverse(List1, List),
	!.
get_recorded_wavfile_list(List) :-
	format2error('~N*** Error: bad call: ~w~n', [get_recorded_wavfile_list(List)]),
	fail.

get_all_recorded_wavfiles_logfiles(Logfiles) :-
	%directory_files_recursive('$REGULUS/recorded_wavfiles', AllFiles),
	get_recorded_wavfiles_parent_directory(ParentDir),
	format_to_atom('~w/recorded_wavfiles', [ParentDir], Dir),
	directory_files_recursive(Dir, AllFiles),
	findall(File,
		( member(File, AllFiles), is_logfile(File) ),
		Logfiles).

is_logfile(File) :-
	atom_codes(File, FileChars),
	is_substring("logfile", FileChars),
	!.

%---------------------------------------------------------------

parameter_list_to_atom(ParameterList, ParameterAtom) :-
	parameter_list_to_atom1(ParameterList, '', ParameterAtom),
	!.
parameter_list_to_atom(ParameterList, ParameterAtom) :-
	format('~N*** Error: bad call: ~q~n', [parameter_list_to_atom(ParameterList, ParameterAtom)]),
	fail.

parameter_list_to_atom1([], ReturnParameterAtom, ReturnParameterAtom) :-
	!.
parameter_list_to_atom1([Item | ParameterList], ParameterAtom, _ReturnParameterAtom) :-
	(   Item = (Parameter=Value) ->
	    append_atoms([Parameter, Value], 0'=, OneParameterAtom) ;
	    atomic(Item) ->
	    Item = OneParameterAtom
	),
	join_with_spaces([OneParameterAtom, ParameterAtom], NewParameterAtom),
	parameter_list_to_atom1(ParameterList, NewParameterAtom, _ReturnParameterAtom).

%---------------------------------------------------------------

warn_if_dubious_path :-
	environ('OS', OSAtom),
	environ('PATH', PathAtom),
	required_dir_in_path_missing(PathAtom, OSAtom, Patterns, Example),
	warn_about_missing_dir_in_path(PathAtom, Patterns, Example),
	!.
warn_if_dubious_path.

warn_about_missing_dir_in_path(PathAtom, Patterns, Example) :-
	format('~N*** Warning: you may need to change the value of PATH~n', []),
	format('~NCurrent value is ~w~n', [PathAtom]),
	format('~NNo directory matches the pattern "~w"~n', [Patterns]),
	format('~NYou probably want to add something like "~w"~n', [Example]),
	!.

required_dir_in_path_missing(PathAtom, OSAtom, DirPatterns, Example) :-
	split_atom_into_words(PathAtom, 0';, Dirs),
	required_dir_patterns_for_os(OSAtom, DirPatterns, Example), 
	\+ dir_pattern_found_in_list(Dirs, DirPatterns),
	!.

dir_pattern_found_in_list(Dirs, DirPatterns) :-
	member(DirPattern, DirPatterns),
	atom_codes(DirPattern, DirPatternString),
	member(Dir, Dirs),
	safe_absolute_file_name(Dir, AbsDir),
	atom_codes(AbsDir, DirString),
	is_substring(DirPatternString, DirString),
	!.

required_dir_patterns_for_os('Windows_NT', ['cygwin/bin', 'cygwin64/bin'], 'C:\\cygwin\\bin or C:\\cygwin64\\bin').

