
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').
:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

toy0_app_with_get_param(Port) :-
	RecognitionPackage = '$REGULUS/Examples/Toy0/Generated/recogniser',
	top_loop(Port,
		 RecognitionPackage,
		 'audio.OutputVolume=200',
		 "Ready to start",
		 no_state,
		 process_recogniser_output).

process_recogniser_output(Result, _InState, _OutState) :-
	Result = recognition_succeeded(_Conf, _Words, [value=LF]),
	LF = [[spec, Number], [noun, Noun]],
	wavfile_for_noun(Noun, Wavfile),
	(   Number = 1 ->
	    regulus_sockettalk_say_file(Wavfile) ;
	    
	    Number = 2 ->
	    regulus_sockettalk_say_file(Wavfile),
	    regulus_sockettalk_say_file(Wavfile)
	),
	regulus_sockettalk_get_parameter('client.FilenameRecorded', ParameterInfo),
	format('~N~n--- Nuance Param info:~w', [ParameterInfo]),		
	!.
process_recogniser_output(_Result, _InState, _OutState) :-
	regulus_sockettalk_say_tts("Something went wrong"),
	!.

wavfile_for_noun(felis_domesticus,
		 '$REGULUS/Examples/Toy0/wavfiles/miaow.wav').
wavfile_for_noun(canis_domesticus,
		 '$REGULUS/Examples/Toy0/wavfiles/woof.wav').






	