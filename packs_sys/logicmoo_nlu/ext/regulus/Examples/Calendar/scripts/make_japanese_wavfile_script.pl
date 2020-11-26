
:- use_module('$REGULUS/Prolog/speech_output').

go :-
	ScriptFile = '$REGULUS/Examples/Calendar/japanese_wavfiles/record_wavfiles.bat',
	EditFile = '$REGULUS/Examples/Calendar/japanese_wavfiles/edit_wavfiles.bat',
	Directory = '$REGULUS/Examples/Calendar/japanese_wavfiles',
	list_missing_wavfiles('$REGULUS/Examples/Calendar/Prolog/japanese_output_manager.pl',
			      none,
			      Directory,
			      ScriptFile,
			      NMissingFiles),
	format('~N~n--- ~w wavfiles to record. Script is in ~w~n~n',
	      [NMissingFiles, ScriptFile]),

	make_wavfile_edit_script(Directory, EditFile).

:- go.

:- halt.
