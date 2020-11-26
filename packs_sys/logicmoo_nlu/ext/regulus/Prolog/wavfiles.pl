
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(wavfiles,
	[rename_subdirectories_with_spaces_in_names/1,
	 convert_wavfile_format_in_directory/3,
	 wavfiles_in_directory/2,
	 untranscribed_wavfiles_in_directory/2,
	 wavfile_transcribing_script_for_directory/2,
	 full_wavfile_transcribing_script_for_directory/2
	]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
%:- use_module(library(system)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [shell/1, shell/2] ) ) ).

:- use_module(library(lists)).

%---------------------------------------------------------------

rename_subdirectories_with_spaces_in_names(Dir) :-
	(   user:sicstus_version(3) ->
	    format2error('~N*** Error: rename_subdirectories_with_spaces_in_names/1 only works under Sicstus 4~n', []),
	    fail
	;
	    true
	),
	safe_absolute_file_name(Dir, AbsDir),
	(   safe_is_directory(AbsDir) ->
	    true
	;
	    format2error('~N*** Error in rename_subdirectories_with_spaces_in_names/1: ~w is not a directory~n', [AbsDir]),
	    fail
	),
	(   ( atom_codes(AbsDir, AbsDirCodes), member(0' , AbsDirCodes) ) ->
	    format2error('~N*** Error in rename_subdirectories_with_spaces_in_names/1: top-level directory ~w contains spaces in pathname~n', [AbsDir]),
	    fail
	;
	    rename_subdirectories_with_spaces_in_names1(AbsDir)
	).

rename_subdirectories_with_spaces_in_names1(Dir) :-
	safe_is_directory(Dir),
	safe_absolute_file_name(Dir, AbsDir),
	atom_codes(AbsDir, AbsDirCodes),
	(   member(0' , AbsDirCodes) ->
	    remove_spaces_from_string(AbsDirCodes, AbsDirCodes1),
	    atom_codes(AbsDir1, AbsDirCodes1),
	    rename_directory(AbsDir, AbsDir1),
	    format('~N--- Renamed "~w" to "~w"~n', [AbsDir, AbsDir1])
	;
	    otherwise ->
	    AbsDir1 = AbsDir
	),
	directory_members_of_directory(AbsDir1, SubDirPairsList),
	!,
	rename_subdirectories_with_spaces_in_names_list(SubDirPairsList).

rename_subdirectories_with_spaces_in_names_list([]).
rename_subdirectories_with_spaces_in_names_list([_BaseName-FullName | R]) :-
	rename_subdirectories_with_spaces_in_names1(FullName),
	!,
	rename_subdirectories_with_spaces_in_names_list(R).

remove_spaces_from_string([], []).
remove_spaces_from_string([0' | R], R1) :-
	!,
	remove_spaces_from_string(R, R1).
remove_spaces_from_string([F | R], [F | R1]) :-
	!,
	remove_spaces_from_string(R, R1).
	
%---------------------------------------------------------------

% Typical command:
% wavconvert utt01.wav utt01_sphere.wav linear sphere

convert_wavfile_format_in_directory(Dir, NewEncoding, NewFormat) :-
	wavfiles_in_directory(Dir, Wavfiles),
	length(Wavfiles, N),
	format('~N--- Converting format for ~d wavfiles~n', [N]),
	convert_wavfile_format_in_list(Wavfiles, NewEncoding, NewFormat).

convert_wavfile_format_in_list([], _NewEncoding, _NewFormat).
convert_wavfile_format_in_list([F | R], NewEncoding, NewFormat) :-
	convert_wavfile_format(F, NewEncoding, NewFormat),
	format('.', []),
	flush_output(user),
	!,
	convert_wavfile_format_in_list(R, NewEncoding, NewFormat).

convert_wavfile_format(Wavfile, NewEncoding, NewFormat) :-
	tmp_regulus_file('tmp_wavfile.wav', TmpWavfile),
	safe_absolute_file_name(Wavfile, AbsWavfile),
	safe_absolute_file_name(TmpWavfile, AbsTmpWavfile),
	format_to_atom('wavconvert ~w ~w ~w ~w',
		       [AbsWavfile, AbsTmpWavfile, NewEncoding, NewFormat],
		       WavconvertCommand),
	%safe_exec(WavconvertCommand, [std, null, null], _PID1),
	shell(WavconvertCommand),
	format_to_atom('mv ~w ~w',
		       [AbsTmpWavfile, AbsWavfile],
		       MoveCommand),
	%safe_exec(MoveCommand, [std, null, null], _PID2),
	shell(MoveCommand),
	!.
convert_wavfile_format(Wavfile, NewEncoding, NewFormat) :-
	format2error('~N*** Error: bad call: ~w~n', [convert_wavfile_format(Wavfile, NewEncoding, NewFormat)]),
	fail.

%---------------------------------------------------------------

wavfiles_in_directory(Dir, Wavfiles) :-
	(   safe_is_directory(Dir) ->
	    true
	;
	    format2error('~N*** Error: ~w is not a directory~n', [Dir]),
	    fail
	),
	directory_files_recursive(Dir, Files),
	(   user:sicstus_version(4) ->
	    reverse(Files, Files1)
	;
	    otherwise ->
	    Files = Files1
	),
	wavfiles_in_list(Files1, Wavfiles).

wavfiles_in_list([], []).
wavfiles_in_list([F | R], [F | R1]) :-
	is_wavfile(F),
	!,
	wavfiles_in_list(R, R1).
wavfiles_in_list([_F | R], R1) :-
	!,
	wavfiles_in_list(R, R1).

is_wavfile(File) :-
	split_off_extension_from_pathname(File, _Base, Extension),
	Extension = wav,
	!.

%---------------------------------------------------------------

untranscribed_wavfiles_in_directory(Dir, UntranscribedWavfiles) :-
	wavfiles_in_directory(Dir, Wavfiles),
	untranscribed_wavfiles_in_list(Wavfiles, UntranscribedWavfiles),
	!.

untranscribed_wavfiles_in_list([], []).
untranscribed_wavfiles_in_list([F | R], [F | R1]) :-
	untranscribed_wavfile(F),
	!,
	untranscribed_wavfiles_in_list(R, R1).
untranscribed_wavfiles_in_list([_F | R], R1) :-
	!,
	untranscribed_wavfiles_in_list(R, R1).

untranscribed_wavfile(File) :-
	\+ get_transcription_from_wavfile(File, _Transcription).

%---------------------------------------------------------------

wavfile_transcribing_script_for_directory(Dir, ScriptFile) :-
	untranscribed_wavfiles_in_directory(Dir, UntranscribedWavfiles) ->
	length(UntranscribedWavfiles, N),
	absolute_file_name(ScriptFile, AbsScriptFile),
	open(AbsScriptFile, write, S),
	write_wavfile_transcribing_script(UntranscribedWavfiles, S),
	close(S),
	format('~N--- Written wavfile transcribing script (~d items) to ~w~n', [N, AbsScriptFile]).

full_wavfile_transcribing_script_for_directory(Dir, ScriptFile) :-
	wavfiles_in_directory(Dir, Wavfiles) ->
	length(Wavfiles, N),
	absolute_file_name(ScriptFile, AbsScriptFile),
	open(AbsScriptFile, write, S),
	write_wavfile_transcribing_script(Wavfiles, S),
	close(S),
	format('~N--- Written wavfile transcribing script (~d items) to ~w~n', [N, AbsScriptFile]).

write_wavfile_transcribing_script([], _S).
write_wavfile_transcribing_script([F | R], S) :-
	write_wavfile_transcribing_script_line(F, S),
	!,
	write_wavfile_transcribing_script(R, S).

write_wavfile_transcribing_script_line(AbsFile, S) :-
	directory_and_file_for_pathname(AbsFile, Dir, File),
	absolute_windows_file_name(Dir, WindowsDir),
	%format(S, '~Ncd "~w"~n', [WindowsDir]),
	%format(S, '~Nxwavedit ~w~n', [File]).
	format(S, '~Ncd "~w" & xwavedit ~w~n', [WindowsDir, File]).
