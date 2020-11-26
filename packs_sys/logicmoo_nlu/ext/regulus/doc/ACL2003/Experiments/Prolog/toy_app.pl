% toy_app.pl

%----------------------------------------------------------------------

:- module(toy_app,
	[toy_app/1]
    ).

%----------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(lists)).

:- use_module(library(utilities)).

:- use_module(library(regulus_filetalk)).
:- use_module(library(regulus_eval)).

%----------------------------------------------------------------------

toy_app(Mode) :-
	check_mode_value(Mode),
	toy_app_init,
	toy_app_loop(Mode).

check_mode_value(continuous) :- !.
check_mode_value(push_to_talk) :- !.
check_mode_value(Other) :-
	format('~N*** ERROR: bad value "~w" of toy_app parameter. Should be "continuous" or "push_to_talk"', [Other]),
	fail.

%----------------------------------------------------------------------

toy_app_init :-
	(   absolute_file_name('$REGULUS/Examples/PSA/scripts/regulus_server.cfg', CFGFile) ->
	    regulus_filetalk_init(CFGFile) ;
	    format('~NError: couldn\'t interpret "~w" as a pathname~n', [toy_app('scripts/regulus_server.cfg')]),
	    fail
	),
	say_tts_or_print("Ready to receive input").

toy_app_loop(Mode):-
	(   
	    Mode = push_to_talk ->
	    wait_for_push_to_talk ;
	    true
	),
	regulus_filetalk_post_recognition_request('.MAIN'),
	wait_for_regulus_server_response(Response),
	process_toy_app_result(Response, Mode),
	!,
	toy_app_loop(Mode).

wait_for_push_to_talk :-
	format('~N~nPRESS RETURN KEY TO RECOGNISE >> ', []),
	getline(user, _Line).

%----------------------------------------------------------------------

process_toy_app_result(Response, Mode) :-
	format('~N~n---------------------------------------------------------------------------------------------~n~n', []),
	%format('Recognition result: ~w~n', [Response]),
	process_toy_app_result1(Response),
	(   
	    Mode = push_to_talk ->
	    regulus_filetalk_clean_up ;
	    true
	).

process_toy_app_result1(recognition_failed(_)) :-
	say_tts_or_print("Sorry, I couldn't understand that"),
	!.
process_toy_app_result1(recognition_succeeded(RecConfidence, RecognisedWordsAtom, [value=NLVal])) :-
	format('~N~nRecognised word string: "~w" (Confidence: ~d)~n', [RecognisedWordsAtom, RecConfidence]),
	regulus_eval_speech(NLVal, NLVal1, riacs_postproc_lf),
	format('~N~nNLVal after post-processing:', []),
	format('~N~n~w~n~n', [NLVal1]),
	!.
process_toy_app_result1(recognition_succeeded(_SourceRepresentation)) :-
	say_tts_or_print("Sorry, I couldn't process the speech output"),
	!.
process_toy_app_result1(_Other) :-
	say_tts_or_print("Sorry, I couldn't understand that"),
	!.

%----------------------------------------------------------------------

say_tts_or_print(String) :-
	format('~N~n(Would say: "~s")~n', [String]).
%say_tts_or_print(String) :-
%	regulus_filetalk_say_tts(String).
