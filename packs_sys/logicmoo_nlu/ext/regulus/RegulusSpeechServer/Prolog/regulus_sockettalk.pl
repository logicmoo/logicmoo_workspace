:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_sockettalk,
	[regulus_sockettalk_debug/0,
	 regulus_sockettalk_nodebug/0,
	 regulus_sockettalk_init_with_timeout/4,
	 regulus_sockettalk_init/3,
	 regulus_sockettalk_init/4,
	 regulus_sockettalk_init_multi_package/3,
	 regulus_sockettalk_init_with_timeout_multi_package/4,
	 regulus_sockettalk_exit_client/0,
	 regulus_sockettalk_exit_server/0,
	 regulus_sockettalk_say_file/1,
	 regulus_sockettalk_say_tts/1,
	 regulus_sockettalk_say_list_atom/1,
	 regulus_sockettalk_say_list/1,
	 regulus_sockettalk_set_output_volume/1,
	 regulus_sockettalk_set_parameter/2,
	 regulus_sockettalk_get_parameter/2,
	 regulus_sockettalk_recognise/2,
	 regulus_sockettalk_recognise_asynchronous/1,
	 regulus_sockettalk_poll_for_recognition_result/1,
	 regulus_sockettalk_recognise_file/3,
	 regulus_sockettalk_interpret/3,
	 regulus_sockettalk_abort_recognition/0,
	 regulus_sockettalk_abort_playback/0]
    ).

%----------------------------------------------------------------------

:- use_module(library(sockets)).
% We only want environ/2 and sleep/1 from system
:- use_module(library(system), [environ/2, sleep/1]).

:- use_module(library(lists)).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------
%
%   regulus_sockettalk_debug
%
% Switches on sockettalk trace.
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_nodebug
%
% Switches off sockettalk trace.
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_init(Package)
%
% Initialise Regulus sockettalk system; call before invoking any of the 
% other calls. Equivalent to
%
% regulus_sockettalk_init(1975, '-package <Package> -p 1975')
%
% Sample call:
%
% regulus_sockettalk_init('C:/home/speech/Regulus/Examples/Toy0/Generated/recogniser')
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_init(PortName, ServerArguments, ServerCommand, Timeout)
%
% Initialise Regulus sockettalk system; call before invoking any of the 
% other calls. Version which starts up a Regserver process before
% attempting to connect to it.
%
% ServerArguments: atom representing arguments to pass to server
%
% ServerCommand: atom representing name of server command
%
% Timeout: length of time to wait, in milliseconds, before trying to connect.
%          4 attempts are made before giving up.
%
% Sample call:
%
% regulus_sockettalk_init(4321,
%                         'C:/home/speech/Regulus/RegulusSpeechServer/Runtime/regserver',
%                         '-package C:/home/speech/Regulus/Examples/Toy0/Generated/recogniser -p 1414',
%                         8000).                        
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_exit_client
%
% Closes connection to regserver
%
% Sample call:
%
% regulus_sockettalk_exit_client
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_exit_server
%
% Exits regserver
%
% Sample call:
%
% regulus_sockettalk_exit_server
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_say_file(+File)
%
% where 
%
%   File is an atom whose print name is the name of a .wav file in the current
%   Regulus server prompt directory
%
% The wavfile is played
%
% Sample call:
%
% regulus_sockettalk_say_file('hello.wav')
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_say_tts(+String)
%
% where 
%
%   String is a Prolog string
%
% A request to say String using TTS is appended to the prompt queue
%
% Sample call:
%
% regulus_sockettalk_say_tts("hello world")
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_say_list(+ItemList)
%
% where 
%
%   ItemList is a list of items of form either
%       a) file(Atom) - play wavfile called Atom
%       b) tts(Atom) - perform TTS on the print-name of Atom
%

% Sample call:
%
% regulus_sockettalk_say_list([file('hello.wav'), file('world.wav'), tts('OK, did that')])
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_say_list_atom(+ListAtom)
%
% where 
%
%   ListAtom is an atom whose printname is a concatenation of items of form either
%       a) +text "String" - perform TTS on String
%       b) String - play wavfile called String
%

% Sample call:
%
% regulus_sockettalk_say_list_atom('hello_world +text "OK, did that"'])
%
% plays the wavfile hello_world.wav followed by doing TTS on "OK, did that"
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_set_output_volume(+Number)
%
% where 
%
%   Number is an integer between 0 and 255 inclusive
%
% A request to set the output volume to Number is sent to the server
%
% Sample call:
%
% regulus_sockettalk_set_output_volume(255)
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_set_parameter(+ParamName, +Value)
%
% where 
%
%   ParamName is an atom
%   Value is an atom or number
%
% A request to set Param to Value is sent to the server
%
% Sample call:
%
% regulus_sockettalk_set_param('audio.OutputVolume', 255)
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_get_parameter(+ParamName, -Value)
%
% where 
%
%   ParamName is an atom
%
% A request to get Param is sent to the server. Value is unified with the result, which can be either
% an integer, a float or an atom. 
%
% Sample call:
%
% regulus_sockettalk_get_parameter('audio.OutputVolume', Volume)
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_recognise(+GrammarName, -Result)
%   regulus_sockettalk_recognise_file(+Wavfile, +GrammarName, -Result)
%   regulus_sockettalk_interpret(+StringAtom, +GrammarName, -Result)
%
% where 
%
%   GrammarName is a Prolog atom representing a grammar name
%   Wavfile is an atom representing a wavfile
%   StringAtom is an atom representing a text string
%
% A request to recogise speech using the grammar GrammarName is sent to the server.
%   For regulus_sockettalk_recognise_file/3, recognition is performed on the designated wavfile.
%   For regulus_sockettalk_interpret, the designated string is parsed using the specified grammar.
%
% The response can be one of the following:
%
%   recognition_succeeded(Confidence, RecString, Result) where 
%        - RecString is the recognition string, expressed as a Prolog atom
%        - Result is a Regulus semantic expression, expressed as a Prolog term.
%
%   recognition_failed('no or empty result') 
%
% Sample call:

% regulus_sockettalk_recognise('.MAIN', Result)
%
% Sample result:
%
% recognition_succeeded(59, 'yes', [value=[[utterance_type, phrase], [interjection, yes]])
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_recognise_asynchronous(+GrammarName)
%   regulus_sockettalk_poll_for_recognition_result(-Result)
%
% Asynchronous version of regulus_sockettalk_recognise(+GrammarName, -Result)
%
% regulus_sockettalk_poll_for_recognition_result(-Result) returns quickly with either a recognition result
% or the atom '*no_result*'
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_abort_recognition
%
% A request to abort speech recognition is sent to the server.
%
%----------------------------------------------------------------------
%
%   regulus_sockettalk_abort_recognition_playback
%
% A request to abort production of spoken output is sent to the server.
%
%----------------------------------------------------------------------

:- dynamic current_socket/1.
:- dynamic current_socket_stream/1.
:- dynamic sockettalk_debug_on/0.

%----------------------------------------------------------------------

regulus_sockettalk_debug :-
	assertz(sockettalk_debug_on),
	!.

regulus_sockettalk_nodebug :-
	retractall(sockettalk_debug_on).

%----------------------------------------------------------------------

%regulus_sockettalk_init(Port) :-
%	current_host(Host),
%	socket('AF_INET', Socket),
%	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
%	set_current_socket(Socket),
%	set_current_socket_stream(Stream),
%	format('~N--- Connected to Regserver on port ~d~n', [Port]),
%	!.
%regulus_sockettalk_init(Port) :-
%	format('~N*** Error: unable to connect to Regserver on port ~d~n', [Port]),
%	fail,
%	!.

%----------------------------------------------------------------------

%regulus_sockettalk_init(RecPackage) :-
%	DefaultPort = 1975,
%	format_to_atom('-package ~w -p ~d', [RecPackage, DefaultPort], ServerArguments),
%	regulus_sockettalk_init(DefaultPort, ServerArguments).

%----------------------------------------------------------------------

regulus_sockettalk_init(Port, RecPackage, NuanceConfigParameters) :-
	DefaultTimeout = 8000,
	regulus_sockettalk_init_with_timeout(Port, RecPackage, NuanceConfigParameters, DefaultTimeout).

regulus_sockettalk_init_with_timeout(Port, RecPackage, NuanceConfigParameters, Timeout) :-
	get_server_command(ServerCommand),
	format_to_atom('-package ~w -p ~d ~w', [RecPackage, Port, NuanceConfigParameters], ServerArguments),
	regulus_sockettalk_init(Port, ServerArguments, ServerCommand, Timeout).

regulus_sockettalk_init_multi_package(Port, RecPackages, NuanceConfigParameters) :-
	DefaultTimeout = 8000,
	regulus_sockettalk_init_with_timeout_multi_package(Port, RecPackages, NuanceConfigParameters, DefaultTimeout).

regulus_sockettalk_init_with_timeout_multi_package(Port, RecPackages, NuanceConfigParameters, Timeout) :-
	get_server_command(ServerCommand),
	packages_to_nuance_argument(RecPackages, RecPackagesAtom),
	format_to_atom('~w -p ~d ~w', [RecPackagesAtom, Port, NuanceConfigParameters], ServerArguments),
	regulus_sockettalk_init(Port, ServerArguments, ServerCommand, Timeout).

packages_to_nuance_argument([Package|Packages], CompletePackageAtom):-
	add_package_flag([Package|Packages], PackagesWithFlag),
	join_with_spaces(PackagesWithFlag, CompletePackageAtom).

add_package_flag([],[]).

add_package_flag([Package|Packages], [PackageWithFlag|PackagesWithFlag]):-
	join_with_spaces(['-package', Package], PackageWithFlag),
	add_package_flag(Packages, PackagesWithFlag).
    

	

get_server_command(AbsServerCommand) :-
	(   environ('MACHINE_TYPE', MachineType) ->
	    get_server_command_for_machine_type(MachineType, ServerCommand) ;
	    
	    format2error('~N*** Error: couldn\'t find machine type~n', []),
	    fail
	),
	absolute_file_name(ServerCommand, AbsServerCommand),
	!.
get_server_command(AbsServerCommand) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_server_command(AbsServerCommand)]),
	fail.

get_server_command_for_machine_type(win32,
				    '$REGULUS/RegulusSpeechServer/runtime/win32/regserver.exe') :-
	!.
get_server_command_for_machine_type('sparc-solaris',
				    '$REGULUS/RegulusSpeechServer/runtime/sparc-solaris/regserver') :-
	!.
get_server_command_for_machine_type(MachineType, ServerCommand) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_server_command_for_machine_type(MachineType, ServerCommand)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_init(Port, ServerArguments, ServerCommand, Timeout) :-
	format_to_atom('~w ~w', [ServerCommand, ServerArguments], Command),
	format('~N--- Launching Regserver: "~w"~n', [Command]),
	(   safe_exec(Command, [null, null, null], _PID) ->
	    
	    true ;

	    format2error('~N*** Error: unable to start Regserver~n', []),
	    fail
	),	    
	current_host(Host),
	safe_socket('AF_INET', Socket),
	TimeoutInSeconds is Timeout / 1000,
	!,
	regulus_sockettalk_init1(Port, Host, Socket, TimeoutInSeconds, 1, 4).

regulus_sockettalk_init1(Port, _Host, _Socket, _TimeoutInSeconds, N, MaxTries) :-
	N > MaxTries,
	format2error('~N*** Error: giving up on attempting to connect to Regserver on port ~d~n', [Port]),
	fail,
	!.
regulus_sockettalk_init1(Port, Host, Socket, TimeoutInSeconds, N, MaxTries) :-
	N =< MaxTries,
	sleep(TimeoutInSeconds),
	format('~N--- Trying to connect to Regserver (attempt ~d)~n', [N]),
	(   no_regserver_process_is_running(CommandStringSearchedOn) ->
	    format('~N--- Unable to find running process matching "~s", waiting some more~n',
		   [CommandStringSearchedOn]),
	    N1 is N + 1,
	    regulus_sockettalk_init1(Port, Host, Socket, TimeoutInSeconds, N1, MaxTries)
	;
	    safe_socket_connect(Socket, Host, Port, Stream) ->
	    
	    set_current_socket(Socket),
	    set_current_socket_stream(Stream),
	    format('~N--- Connected to Regserver on port ~d~n', [Port])
	;
	    otherwise ->
	    N1 is N + 1,
	    regulus_sockettalk_init1(Port, Host, Socket, TimeoutInSeconds, N1, MaxTries)
	).
regulus_sockettalk_init1(Port, _Host, _Socket, _TimeoutInSeconds, _N, _MaxTries) :-
	format2error('~N*** Error: unable to connect to Regserver on port ~d~n', [Port]),
	fail,
	!.

%----------------------------------------------------------------------

regserver_command_string("regserver.exe").

no_regserver_process_is_running(CommandStringSearchedOn) :-
	regserver_command_string(CommandStringSearchedOn),
	\+ regserver_process_is_running(CommandStringSearchedOn).

regserver_process_is_running(CommandStringSearchedOn) :-
	get_windows_ps_info(PSInfo),
	member(PSLine, PSInfo),
	member(command=CommandAtom, PSLine),
	atom_codes(CommandAtom, CommandChars),
	is_contiguous_sublist(CommandStringSearchedOn, CommandChars),
	!.

%----------------------------------------------------------------------

regulus_sockettalk_exit_client :-
	get_current_socket(Socket),
	get_current_socket_stream(Stream),
	safe_socket_close(Socket),
	safe_close(Stream),
	!.
regulus_sockettalk_exit_client :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_exit_client]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_exit_server :-
	get_current_socket(Socket),
	get_current_socket_stream(Stream),
	format_to_socket_stream('CLEAN_UP~n', []),
	safe_socket_close(Socket),
	safe_close(Stream),
	format('~N--- Regserver exit complete~n', []),
	!.
regulus_sockettalk_exit_server :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_exit_server]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_say_file(PromptFile) :-
	safe_absolute_file_name(PromptFile, AbsPromptFile),
	format_to_socket_stream('SAY_LIST ~w~n', [AbsPromptFile]),
	!.
regulus_sockettalk_say_file(PromptFile) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_say_file(PromptFile)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_say_tts(String) :-
	add_escape_chars_to_string_for_sockettalk(String, String1),
	format_to_socket_stream('SAY_LIST -tts_text: ~s~n', [String1]),
	!.
regulus_sockettalk_say_tts(String) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_say_tts(String)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_say_list_atom(ListAtom) :-
	atom(ListAtom),
	format_to_socket_stream('SAY_LIST ~w~n', [ListAtom]),
	!.
regulus_sockettalk_say_list_atom(ListAtom) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_say_list_atom(ListAtom)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_say_list(List) :-
	format_say_list_to_atom(List, ListAtom),
	format_to_socket_stream('SAY_LIST ~w~n', [ListAtom]),
	!.
regulus_sockettalk_say_list(List) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_say_list(List)]),
	fail.

format_say_list_to_atom([], '') :-
	!.
format_say_list_to_atom([F | R], Atom) :-
	format_say_list_item_to_atom(F, FAtom),
	format_say_list_to_atom(R, RAtom),
	format_to_atom('~w ~w', [FAtom, RAtom], Atom),
	!.

format_say_list_item_to_atom(file(File), OutAtom) :-
	safe_absolute_file_name(File, AbsFile),
	format_to_atom(' ~w', [AbsFile], OutAtom),
	!.
format_say_list_item_to_atom(tts(Atom), OutAtom) :-
	add_escape_chars_to_atom_for_sockettalk(Atom, Atom1),
	format_to_atom(' -tts_text: ~w', [Atom1], OutAtom),
	!.
format_say_list_item_to_atom(Other, S) :-
	format2error('~N*** Error: bad call: ~w~n', [format_say_list_item_to_atom(Other, S)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_set_output_volume(N) :-
	integer(N),
	0 =< N,
	N =< 255,
	format_to_socket_stream('SET_OUTPUT_VOLUME ~d~n', [N]),
	read_from_socket_stream(Reply),
	Reply = set_parameter_ok,
	!.
regulus_sockettalk_set_output_volume(N) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_set_output_volume(N)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_set_parameter(ParamName, Value) :-
	format_to_socket_stream('SET_PARAMETER ~w ~w~n', [ParamName, Value]),
	read_from_socket_stream(Reply),
	Reply = set_parameter_ok,
	!.
regulus_sockettalk_set_parameter(ParamName, Value) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_set_parameter(ParamName, Value)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_get_parameter(ParamName, Value) :-
	format_to_socket_stream('GET_PARAMETER ~w~n', [ParamName]),
	(   read_from_socket_stream(Reply) ->
	    true
	;
	    format2error('~N*** Error: didn\'t read any reply to socket call~n', []),
	    fail
	),
	(   Reply = parameter(_ParamType, _ParamName, Value) ->
	    true
	;
	    format2error('~N*** Error: unexpected reply to socket call: ~w~n', [Reply]),
	    fail
	),
	!.
regulus_sockettalk_get_parameter(ParamName, Value) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_get_parameter(ParamName, Value)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_recognise(Grammar, Result) :-
	format_to_socket_stream('RECOGNISE ~w~n', [Grammar]),
	read_from_socket_stream(Result),
	!.
regulus_sockettalk_recognise(Grammar, Result) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_recognise(Grammar, Result)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_recognise_asynchronous(Grammar) :-
	format_to_socket_stream('RECOGNISE ~w~n', [Grammar]),
	!.
regulus_sockettalk_recognise_asynchronous(Grammar) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_recognise_asynchronous(Grammar)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_poll_for_recognition_result(RecognitionResult) :-
	get_current_socket_stream(Stream),
	poll_for_waiting_reply(Stream),
	read(Stream, RecognitionResult),
	RecognitionResult \== end_of_file,
	(   sockettalk_debug_on ->
	    format('~N--- Received message from Regserver: "~q"~n', [RecognitionResult])
	;
	    otherwise ->
	    true
	),
	!.
regulus_sockettalk_poll_for_recognition_result(Result) :-
	Result = '*no_recognition_result*',
	!.
regulus_sockettalk_poll_for_recognition_result(Result) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_poll_for_recognition_result(Result)]),
	fail.

% Wait 100 microseconds for result
poll_for_waiting_reply(Stream) :-	
	socket_select([], [], 0:100, [Stream], StreamsWithInput),
	StreamsWithInput \== [],
	!.

socket_select([], [], 0:100, [Stream], StreamsWithInput) :-
	socket_select([], _SReady, [Stream], StreamsWithInput, [], _WReady, 0:100).

%----------------------------------------------------------------------

regulus_sockettalk_recognise_file(Wavfile, Grammar, Result) :-
	format_to_socket_stream('RECOGNISE_FILE ~w ~w~n', [Wavfile, Grammar]),
	read_from_socket_stream(Result),
	!.
regulus_sockettalk_recognise_file(Wavfile, Grammar, Result) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_recognise_file(Wavfile, Grammar, Result)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_interpret(StringAtom, Grammar, Result) :-
	format_to_socket_stream('INTERPRET ~w ~w~n', [StringAtom, Grammar]),
	read_from_socket_stream(Result),
	!.
regulus_sockettalk_interpret(StringAtom, Grammar, Result) :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_interpret(StringAtom, Grammar, Result)]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_abort_recognition :-
	format_to_socket_stream('ABORT_RECOGNITION~n', []),
	!.
regulus_sockettalk_abort_recognition :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_abort_recognition]),
	fail.

%----------------------------------------------------------------------

regulus_sockettalk_abort_playback :-
	format_to_socket_stream('ABORT_PLAYBACK~n', []),
	!.
regulus_sockettalk_abort_playback :-
	format2error('~N*** Error: bad call: ~w~n', [regulus_sockettalk_abort_playback]),
	fail.

%----------------------------------------------------------------------


format_to_socket_stream(FormatAtom, Args) :-
	get_current_socket_stream(S),
	format_to_atom(FormatAtom, Args, Message),
	(   sockettalk_debug_on ->
	    format('~N--- Sending message to Regserver: "~w"~n', [Message]) ;
	    true
	),
	format(S, '~w', [Message]),
	flush_output(S),
	!.

read_from_socket_stream(Term) :-
	get_current_socket_stream(S),
	read(S, Term),
	(   sockettalk_debug_on ->
	    format('~N--- Received message from Regserver: "~q"~n', [Term]) ;
	    true
	),
	!.

%----------------------------------------------------------------------

set_current_socket(Socket) :-
	retractall(current_socket(_)),
	assertz(current_socket(Socket)),
	!.

get_current_socket(Socket) :-
	current_socket(Socket),
	!.
get_current_socket(_Socket) :-
	format2error('~N*** Error: no current Regserver socket defined.~n', []),
	fail,
	!.

%----------------------------------------------------------------------

set_current_socket_stream(Stream) :-
	retractall(current_socket_stream(_)),
	assertz(current_socket_stream(Stream)),
	!.

get_current_socket_stream(Stream) :-
	current_socket_stream(Stream),
	!.
get_current_socket_stream(_Stream) :-
	format2error('~N*** Error: no current Regserver socket stream defined.~n', []),
	fail,
	!.

%----------------------------------------------------------------------

add_escape_chars_to_atom_for_sockettalk(Atom, Atom1) :-
	atom_codes(Atom, String),
	add_escape_chars_to_string_for_sockettalk(String, String1),
	atom_codes(Atom1, String1),
	!.
add_escape_chars_to_atom_for_sockettalk(Atom, Atom1) :-
	format2error('~N*** Error: bad call: ~w~n', [add_escape_chars_to_atom_for_sockettalk(Atom, Atom1)]),
	fail,
	!.

add_escape_chars_to_string_for_sockettalk([], []) :-
	!.
add_escape_chars_to_string_for_sockettalk([F | R], [0'\\, F | R1]) :-
	char_requiring_escape(F),
	!,
	add_escape_chars_to_string_for_sockettalk(R, R1).
add_escape_chars_to_string_for_sockettalk([F | R], [F | R1]) :-
	!,
	add_escape_chars_to_string_for_sockettalk(R, R1).

char_requiring_escape(0',).


