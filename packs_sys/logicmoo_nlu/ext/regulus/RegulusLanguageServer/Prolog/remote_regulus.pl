
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(remote_regulus,
	[remote_regulus_debug/0,
	 remote_regulus_nodebug/0,
	 remote_regulus_init/1,
	 remote_regulus_init/2,
	 remote_regulus_exit_client/0,
	 remote_regulus_exit_server/0,
	 remote_regulus_call/1]
    ).

%----------------------------------------------------------------------

:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------
%
%   remote_regulus_debug
%
% Switches on remote_regulus trace.
%
%----------------------------------------------------------------------
%
%   remote_regulus_nodebug
%
% Switches off remote_regulus trace.
%
%----------------------------------------------------------------------
%
%   remote_regulus_init(PortName, Timeout)
%
% Initialise Regulus language server; call before invoking any of the 
% other calls. 
%
% PortName: number of port to use
%
% Timeout: length of time to wait, in milliseconds, before trying to connect.
%          4 attempts are made before giving up.
%
% Sample call:
%
% remote_regulus_init(4321, 8000).                        
%
%----------------------------------------------------------------------
%
%   remote_regulus_exit_client
%
% Closes connection to regserver
%
% Sample call:
%
% remote_regulus_exit_client
%
%----------------------------------------------------------------------
%
%   remote_regulus_exit_server
%
% Exits regserver
%
% Sample call:
%
% remote_regulus_exit_server
%
%----------------------------------------------------------------------
%
%   remote_regulus_call(+Goal)
%
% where 
%
%   Goal is any Prolog goal
%
% A request to evaluate Goal is sent to the server. The result is unified with Goal.
%
% Sample calls:
%
% remote_regulus_call(user:regulus_batch('$MED_SLT2/EngEng/Prolog/med_new_interlingua.cfg', ["EBL_LOAD"]))
%
% remote_regulus_call(user:words_to_lf_and_tree_with_current_parser([where, 'is', the, pain], '.MAIN', LF, Tree)
%
%----------------------------------------------------------------------

:- dynamic current_socket/1.
:- dynamic current_socket_stream/1.
:- dynamic remote_regulus_debug_on/0.

%----------------------------------------------------------------------

remote_regulus_debug :-
	assertz(remote_regulus_debug_on),
	!.

remote_regulus_nodebug :-
	retractall(remote_regulus_debug_on).

%----------------------------------------------------------------------

remote_regulus_init(Port) :-
	DefaultTimeout = 8000,
	remote_regulus_init(Port, DefaultTimeout).

remote_regulus_init(Port, Timeout) :-
	get_server_start_file(File),
	remote_regulus_init(File, Port, Timeout).

get_server_start_file(AbsStartServerFile) :-
	StartServerFile = '$REGULUS/RegulusLanguageServer/Prolog/start_server.pl',
	absolute_file_name(StartServerFile, AbsStartServerFile),
	!.
get_server_start_file(AbsStartServerFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_server_start_file(AbsStartServerFile)]),
	fail.

%----------------------------------------------------------------------

remote_regulus_init(StartServerFile, Port, Timeout) :-
	current_host(Host),
	safe_socket('AF_INET', Socket),
	(   current_predicate(user:remote_server_started_by_hand/0) ->
	    StartServerInvocation = null ;
	    format_to_atom('sicstus -l ~w -a ~d', [StartServerFile, Port], StartServerInvocation)
	),
	TimeoutInSeconds is Timeout / 1000,
	!,
	remote_regulus_init1(StartServerInvocation, Port, Host, Socket, TimeoutInSeconds, 4).

remote_regulus_init1(_StartServerInvocation, Port, _Host, _Socket, _TimeoutInSeconds, NTriesLeft) :-
	NTriesLeft =< 0,
	format('~N*** Error: unable to connect to Regulus language server on port ~d~n', [Port]),
	fail,
	!.
remote_regulus_init1(StartServerInvocation, Port, Host, Socket, TimeoutInSeconds, NTriesLeft) :-
	NTriesLeft > 0,
	(   StartServerInvocation = null ->

	    format('~N*** Assuming Regulus language server is already started~n', []) ;

	    safe_exec(StartServerInvocation, [null, null, null], _PID) ->
	    
	    true ;

	    format('~N*** Error: unable to start Regulus language server~n', []),
	    fail
	),	    
	sleep(TimeoutInSeconds),
	(   safe_socket_connect(Socket, Host, Port, Stream) ->
	    
	    set_current_socket(Socket),
	    set_current_socket_stream(Stream),
	    format('~N--- Connected to Regulus language server on port ~d~n', [Port]) ;

	    NTriesLeft1 is NTriesLeft - 1,
	    remote_regulus_init1(StartServerInvocation, Port, Host, Socket, TimeoutInSeconds, NTriesLeft1)
	).
remote_regulus_init1(_StartServerInvocation, Port, _Host, _Socket, _TimeoutInSeconds, _NTriesLeft) :-
	format('~N*** Error: unable to connect to Regulus language server on port ~d~n', [Port]),
	fail,
	!.

%----------------------------------------------------------------------

remote_regulus_exit_client :-
	get_current_socket(Socket),
	safe_socket_close(Socket),
	!.
remote_regulus_exit_client :-
	format('~N*** Error: bad call: ~w~n', [remote_regulus_exit_client]),
	fail.

%----------------------------------------------------------------------

remote_regulus_exit_server :-
	get_current_socket(Socket),
	write_to_socket_stream_and_read_reply(client_shutdown, _Reply),
	safe_socket_close(Socket),
	format('~N--- Regulus language server exit complete~n', []),
	!.
remote_regulus_exit_server :-
	format('~N*** Error: bad call: ~w~n', [remote_regulus_exit_server]),
	fail.

%----------------------------------------------------------------------

remote_regulus_call(Goal) :-
	write_to_socket_stream_and_read_reply(call(Goal), Goal1),
	!,
	Goal = Goal1.
remote_regulus_call(Goal) :-
	format('~N*** Error: bad call: ~w~n', [remote_regulus_call(Goal)]),
	fail.

%----------------------------------------------------------------------

write_to_socket_stream_and_read_reply(OutMessage, ReturnMessage) :-
	write_to_socket_stream(OutMessage),
	sleep(0.01),
	read_from_socket_stream(ReturnMessage).

%----------------------------------------------------------------------

write_to_socket_stream(Term) :-
	get_current_socket_stream(S),
	(   remote_regulus_debug_on ->
	    format('~N--- Sending message to Regulus language server: "~q"~n', [Term]) ;
	    true
	),
	format(S, '~q.~n', [Term]),
	flush_output(S),
	!.

read_from_socket_stream(Term) :-
	get_current_socket_stream(S),
	read(S, Term),
	(   remote_regulus_debug_on ->
	    format('~N--- Received message from Regulus language server: "~q"~n', [Term]) ;
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
	format('~N*** Error: no current Regulua language server socket defined.~n', []),
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
	format('~N*** Error: no current Regulus language server socket stream defined.~n', []),
	fail,
	!.

