
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

% This assumes that the translation files and the target grammar have been loaded.

:- use_module('$REGULUS/Prolog/translate').

:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

%======================================================================

toy1_slt_app(Port) :-
	RecognitionPackage = '$REGULUS/Examples/Toy1Specialised/Generated/toy1_specialised_recogniser',
	InitialState = no_state,
	top_loop(Port,
		 RecognitionPackage,
		 'audio.OutputVolume=200',
		 "Commencez!",
		 InitialState,
		 process_recogniser_output).

%======================================================================

process_recogniser_output(RecogniserOutput, _InState, _OutState) :-
	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=Representation]),
	format('~N~nWords from recogniser: "~w" (confidence: ~d)~n', [Words, Confidence]),
	format('~N~nRecogniser representation: ~w~n', [Representation]),
	translate_to_interlingua(Representation, InterlinguaRepresentation),
	translate_to_target_representation(InterlinguaRepresentation, TargetRepresentation),
	output_generation(TargetRepresentation, Response),
	regulus_sockettalk_say_tts(Response),
	!.
process_recogniser_output(RecogniserOutput, _InState, _OutState) :-
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	regulus_sockettalk_say_tts("Excusez-moi, il y a quelque problem").

%======================================================================

% TRANSLATION TO INTERLINGUA

translate_to_interlingua(Representation, InterlinguaRepresentation) :-
	transfer_representation_to_interlingua(Representation, InterlinguaRepresentation),
	format('~N~nInterlingua representation: ', []), portray_clause(InterlinguaRepresentation),
	!.
translate_to_interlingua(_Representation, _InterlinguaRepresentation) :-
	format('~N~nUnable to convert to interlingua representation~n', []),
	fail.

%======================================================================

% TRANSLATION FROM INTERLINGUA TO TARGET LANGUAGE 

translate_to_target_representation(InterlinguaRepresentation, TargetRepresentation) :-
	transfer_representation_from_interlingua(InterlinguaRepresentation, TargetRepresentation),
	format('~N~nTarget representation: ', []), portray_clause(TargetRepresentation),
	!.
translate_to_target_representation(_InterlinguaRepresentation, _TargetRepresentation) :-
	format('~N~nUnable to convert to target representation~n', []),
	fail.

%======================================================================

% OUTPUT GENERATION

output_generation(TargetRepresentation, TargetString) :-
	generate_surface_words(TargetRepresentation, TargetWords, _),
	join_with_spaces(TargetWords, TargetAtom),
	atom_chars(TargetAtom, TargetString),
	format('~N~nTarget words: "~w"~n', [TargetAtom]),
	!.
output_generation(_TargetRepresentation, _TargetString) :-
	format('~N~nUnable to produce target words.~n', []),
	fail.
