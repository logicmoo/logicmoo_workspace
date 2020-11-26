:- module(progress,
	  [get_progress_file_info_for_java_gui/2,

	   set_new_current_progress_file_for_command/1,
	   set_new_current_progress_file_for_command/2,

	   set_current_progress_file/1,
	   get_current_progress_file/1,

	   add_progress_line/1]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/PrologLib/batchrec_tools').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%----------------------------------------------------------------------

get_progress_file_info_for_java_gui(Command0, Info) :-
	split_string_into_words(Command0, Command),
	get_progress_file_info_for_command(Command, Info0),
	package_progress_file_info(Info0, Info1),
	Info =.. [progress_files | Info1],
	!.
get_progress_file_info_for_java_gui(_Command, no_progress_info).

package_progress_file_info([], []).
package_progress_file_info([F | R], [F1 | R1]) :-
	package_progress_file_item(F, F1),
	!,
	package_progress_file_info(R, R1).

package_progress_file_item(progress_file(File, N, Pattern),
			   progress_file(FileString, N, PatternString)) :-
	atom(File),
	atom(Pattern),
	atom_codes(File, FileString),
	atom_codes(Pattern, PatternString),
	!.
package_progress_file_item(Item, Item1) :-
	format2error('~N*** Error: bad call: ~w', [package_progress_file_item(Item, Item1)]),
	fail.

%----------------------------------------------------------------------

get_progress_file_info_for_command(['LOAD'],
				   [progress_file(File, 9, '*no_pattern*')]) :-
	get_progress_file('load_progress.txt', File),
	!.
get_progress_file_info_for_command(['EBL_LOAD'],
				   [progress_file(File, 8, '*no_pattern*')]) :-
	get_progress_file('ebl_load_progress.txt', File),
	!.
get_progress_file_info_for_command(['LOAD_TRANSLATE'],
				   [progress_file(File, 17, '*no_pattern*')]) :-
	get_progress_file('load_translate_progress.txt', File),
	!.
get_progress_file_info_for_command(['TRANSLATE_CORPUS'],
				   [progress_file(File, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_corpus, CorpusFile),
	count_records_in_file(CorpusFile, N),
	get_progress_file('translate_corpus_progress.txt', File),
	!.
get_progress_file_info_for_command(['TRANSLATE_CORPUS', Id],
				   [progress_file(File, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_corpus(Id), CorpusFile),
	count_records_in_file(CorpusFile, N),
	format_to_atom('translate_corpus_progress_~w.txt', [Id], BaseFile),
	get_progress_file(BaseFile, File),
	!.
get_progress_file_info_for_command(['TRANSLATE_SPEECH_CORPUS'],
				   [progress_file(BatchrecTraceFile, N, 'Rec Total:'),
				    progress_file(TranslationProgressFile, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_speech_corpus, TranscriptionsFile),
	parse_transcriptions_file_to_list(TranscriptionsFile, List),
	length(List, N),
	user:get_regulus_config_item(batchrec_trace, BatchrecTraceFile),
			    
	get_progress_file('translate_speech_corpus_progress.txt', TranslationProgressFile),
	!.
get_progress_file_info_for_command(['TRANSLATE_SPEECH_CORPUS_AGAIN'],
				   [progress_file(TranslationProgressFile, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_speech_corpus, TranscriptionsFile),
	parse_transcriptions_file_to_list(TranscriptionsFile, List),
	length(List, N),
			    
	get_progress_file('translate_speech_corpus_progress.txt', TranslationProgressFile),
	!.
get_progress_file_info_for_command(['TRANSLATE_SPEECH_CORPUS', Id],
				   [progress_file(BatchrecTraceFile, N, 'Rec Total:'),
				    progress_file(TranslationProgressFile, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_speech_corpus(Id), TranscriptionsFile),
	parse_transcriptions_file_to_list(TranscriptionsFile, List),
	length(List, N),
	user:get_regulus_config_item(batchrec_trace, BatchrecTraceFile),

	format_to_atom('translate_speech_corpus_progress_~w.txt', [Id], BaseFile),
	get_progress_file(BaseFile, TranslationProgressFile),
	!.
get_progress_file_info_for_command(['TRANSLATE_SPEECH_CORPUS_AGAIN', Id],
				   [progress_file(TranslationProgressFile, N, '*no_pattern*')]) :-
	user:get_regulus_config_item(translation_speech_corpus(Id), TranscriptionsFile),
	parse_transcriptions_file_to_list(TranscriptionsFile, List),
	length(List, N),

	format_to_atom('translate_speech_corpus_progress_~w.txt', [Id], BaseFile),
	get_progress_file(BaseFile, TranslationProgressFile),
	!.
get_progress_file_info_for_command(Command, List) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_progress_file_info_for_command(Command, List)]),
	fail.

%----------------------------------------------------------------------

set_new_current_progress_file_for_command(Command) :-
	set_new_current_progress_file_for_command(Command, 1).

set_new_current_progress_file_for_command(Command, N) :-
	get_progress_file_info_for_command(Command, Alist),
	safe_nth(N, Alist, progress_file(File, _NLines, _Pattern)),
	set_current_progress_file(File),
	open(File, write, S),
	close(S),
	!.
set_new_current_progress_file_for_command(Command, N) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [set_new_current_progress_file_for_command(Command, N)]),
	fail.
	
%----------------------------------------------------------------------

get_progress_file(Name, AbsFile) :-
	get_tmp_file_directory(Dir),
	format_to_atom('~w/~w', [Dir, Name], File),
	absolute_file_name(File, AbsFile).

%----------------------------------------------------------------------

:- dynamic current_progress_file/1.

set_current_progress_file(File) :-
	absolute_file_name(File, AbsFile),
	retractall(current_progress_file(_)),
	assertz(current_progress_file(AbsFile)).

get_current_progress_file(File) :-
	current_progress_file(File).

%----------------------------------------------------------------------

add_progress_line(Message) :-
	get_current_progress_file(File),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	open(File, append, S),
	format(S, '~N~q.~n', [progress(Timestamp, Message)]),
	close(S).

%----------------------------------------------------------------------

count_records_in_file(File, N) :-
	prolog_file_or_files_to_list(File, List),
	length(List, N). 

%----------------------------------------------------------------------

get_tmp_file_directory(Dir) :-
	absolute_file_name('$REGULUS/tmp', Dir),
	(   safe_directory_exists(Dir) ->
	    true
	;
	    otherwise ->
	    make_directory(Dir)
	),
	!.
get_tmp_file_directory(Dir) :-
	format2error('~N*** Error: bad call: ~w~n', [get_tmp_file_directory(Dir)]),
	fail.
