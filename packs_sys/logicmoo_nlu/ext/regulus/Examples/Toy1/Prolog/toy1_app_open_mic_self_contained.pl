
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/input_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/output_manager').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

toy1_app(Port) :-
	
	Package = '$REGULUS/Examples/Toy1/Generated/recogniser',
	NuanceParameters = 'audio.Provider=continuous client.TTSAddresses=localhost:32323 audio.OutputVolume=200',
	StartMessage = "Ready to start",
	initial_dialogue_state(InitialState),
	
	toy1_app(Port, Package, NuanceParameters, StartMessage, InitialState).

toy1_app(Port, Package, NuanceParameters, StartMessage, InitialState) :-
	absolute_file_name(Package, AbsPackage),
	regulus_sockettalk_debug,
	regulus_sockettalk_init(Port, AbsPackage, NuanceParameters),
	regulus_sockettalk_say_tts(StartMessage),
	top_loop_asynchronous(InitialState, no_outstanding_rec_request).

%======================================================================

top_loop_asynchronous(InState, RecRequestPIn) :-
	% Issue a recognition request if we don't already have one outstanding
	(   RecRequestPIn = no_outstanding_rec_request ->
	    regulus_sockettalk_recognise_asynchronous('.MAIN'),
	    RecRequestPNext = outstanding_rec_request
	;
	    otherwise ->
	    RecRequestPNext = RecRequestPIn
	),
	
	% Check if any recognition result has come in
	regulus_sockettalk_poll_for_recognition_result(Result),
	
	% If we have no rec result, go round again
	(   Result = '*no_recognition_result*' ->
	    RecRequestPOut = RecRequestPNext,
	    OutState = InState
	;
	    % If we do have a rec result, 
	    otherwise ->
	    % process it 
	    process_recognition_result(Result, Action, InState, OutState),
	    perform_action(Action),
	    % and mark that we no longer have a rec request outstanding
	    RecRequestPOut = no_outstanding_rec_request
	),
	!,
	top_loop_asynchronous(OutState, RecRequestPOut).

%======================================================================		

process_recognition_result(RecogniserOutput, Action, InState, OutState) :-
	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=LF]),
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile),
	format('~N~n--- Current wavfile: ~w~n~n', [Wavfile]),
	
	format('~N~n              Old state: ~w', [InState]),

	format('~N~n  Words from recogniser: "~w" (confidence: ~d)~n', [Words, Confidence]),
	format('~N                     LF: ~w~n', [LF]),
	
	lf_to_dialogue_move(LF, InState, DialogueMove),
	format('~N          Dialogue move: ', []), portray_clause(DialogueMove),
	
	update_dialogue_state(DialogueMove, InState, AbstractAction, OutState),
	format('~N        Abstract action: ~w', [AbstractAction]),
	
	abstract_action_to_action(AbstractAction, Action),
	convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	format('~N        Concrete action: ~w', [PrintFormForAction]),
	
	format('~N~n              New state: ~w~n', [OutState]),
	!.
process_recognition_result(RecogniserOutput, Action, InState, OutState) :-
	RecogniserOutput = recognition_succeeded(_Confidence, _Words, [value=_LF]),
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	Action = tts("Sorry, something went wrong"),
	InState = OutState,
	!.
process_recognition_result(RecogniserOutput, no_action, InState, OutState) :-
	RecogniserOutput = recognition_failed(_Reason),
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	InState = OutState,
	!.

%======================================================================

perform_action(no_action) :-
	!.
perform_action(Action) :-
	(   ( Action = tts(String), is_prolog_string(String) ) ->
	    String1 = String ;
	    String1 = "Sorry, something went wrong"
	),
	regulus_sockettalk_say_tts(String1),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.
