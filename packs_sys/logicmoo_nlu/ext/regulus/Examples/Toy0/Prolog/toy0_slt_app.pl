
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').
:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

toy0_slt_app(Port) :-
	RecognitionPackage = '$REGULUS/Examples/Toy0/Generated/recogniser',
	top_loop(Port,
		 RecognitionPackage,
		 'audio.OutputVolume=200',
		 "Commencez!",
		 no_state,
		 process_recogniser_output).

process_recogniser_output(Result, _InState, _OutState) :-
	Result = recognition_succeeded(_Conf, _Words, [value=LF]),
	french_utterance(LF, TargetWords, []),
	join_with_spaces(TargetWords, TargetAtom),
	atom_chars(TargetAtom, TargetString),
	regulus_sockettalk_say_tts(TargetString),
	!.
process_recogniser_output(_RecogniserOutput, _InState, _OutState) :-
	regulus_sockettalk_say_tts("Excusez-moi."),
	!.

french_utterance([Number, Noun]) -->
	french_number(Number, SingPlur),
	french_noun(Noun, SingPlur).

french_number([spec, 1], sing) --> [un].
french_number([spec, 2], plur) --> [deux].

french_noun([noun, felis_domesticus], sing) --> [chat].
french_noun([noun, felis_domesticus], plur) --> [chats].
french_noun([noun, canis_domesticus], sing) --> [chien].
french_noun([noun, canis_domesticus], plur) --> [chiens].
