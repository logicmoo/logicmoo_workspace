% toy_app.pl

/*

Prolog side of toy RegulusServer app

First start Nuance processes, then load into Prolog and start by calling

?- toy_app.

at Prolog top-level.

*/

% Library directories:

% $REGULUS1/PrologLib
:- absolute_file_name('../../../PrologLib', PrologLibDir), asserta(library_directory(PrologLibDir)).

% $REGULUS1/RegulusSpeechServer/Prolog
:- absolute_file_name('../../Prolog', PrologDir), asserta(library_directory(PrologDir)).

% $REGULUS1/RegulusSpeechServer/SampleApp
:- absolute_file_name('..', SampleAppDir), asserta(file_search_path(sample_app, SampleAppDir)).

%----------------------------------------------------------------------

:- module(toy_app,
	[toy_app/0]
    ).

%----------------------------------------------------------------------

:- use_module(library(regulus_filetalk)).

%----------------------------------------------------------------------

toy_app :-
	toy_app_init,
	toy_app_loop.

%----------------------------------------------------------------------

toy_app_init :-
	regulus_filetalk_init(sample_app('scripts/regulus_server.cfg')),
	regulus_filetalk_say_tts("Ready to receive input").

toy_app_loop:-
	regulus_filetalk_say_tts("Next"),
	regulus_filetalk_post_recognition_request('.MAIN'),
	wait_for_regulus_server_response(Response),
	process_toy_app_result(Response),
	!,
	toy_app_loop.

%----------------------------------------------------------------------

process_toy_app_result(Response) :-
	format('~N~n---------------------------------------------------------------------------------------------~n~n', []),
	process_toy_app_result1(Response).

process_toy_app_result1(recognition_succeeded([value=Representation])) :-
	!,
	format('Recognition result: ~w~n', [Representation]),
	regulus_filetalk_say_tts("Printed result").
process_toy_app_result1(recognition_failed(_)) :-
	regulus_filetalk_say_tts("Sorry, I couldn't get that").

