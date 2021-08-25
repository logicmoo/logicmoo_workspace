%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE     : Env/env_mess.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code
%  TESTED : SWI Prolog 5.0.10 www.swi-prolog.org
%
% Capabilities for communicating with the messaging server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November 15, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This file assumes that the following is defined in env_gen.pl:
%
% -- start/0     : initialization of the environment (called when loaded)
% -- finalize/0  : finalization of the environment (called when exiting)
% -- main_dir/1  : obtain the root IndiGolog directory
% -- report_exog_event(A, M): 
%                  report exogenous event A with message M to the
%                  environment manager
% -- All compatibility libraries depending on the architecture such us:
%    -- compat_swi/compat_ecl compatibility libraries providing:
%
% -- The following two dynamic predicates should be available:
%    -- listen_to(Type, Name, Channel) 
%            listen to Channel of Type (stream/socket) with Name
%    -- terminate/0
%            order the termination of the application
%
% -- The following should be implemented here:
%
%  -- name_dev/1              : mandatory *
%  -- initializeInterfaces(L) : mandatory *
%  -- finalizeInterfaces(L)   : mandatory *
%  -- execute/4               : mandatory *
%  -- handle_steam/1          : as needed
%  -- listen_to/3             : as needed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER

:- dynamic bye_message_received/0. % game is over and the device may finish

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set name of the environment here. (THIS CONSTANT IS MANDATORY, DONT DELETE!)
name_dev(messenger). 

% Set verbose debug level
:- set_debug_level(5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initializeInterfaces(L) :- 
           % 1 - Obtain IP and Port from L
        member([ipmess,SIP], L),   
        member([portmess, SP], L),  	% Get Host and Port of CLIMA simulator
        member([agentLogin, AL], L),  		
        string_to_number(SP, Port),
	string_to_atom(SIP, IP),
	assert(mess_server(IP, Port)),		% Assert the messenger server location
	assert(agentID(AL)),			% Assert the agent login information
           % 2 - Setup communication with CLIMA game server
        report_message(system(2),'Establishing connection to MESSENGER server'),
        printKbInstructions,
        connectToMessServer.

finalizeInterfaces(_) :- 
        disconnectFromMessServer,
        report_message(system(2), 'Disconnection from CLIMA SIMULATOR successful').



% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the MESSENGER device manager'), 
    writeln('*   This window will show the communication'), 
    writeln('*   with the MESSENGER server'), 
    writeln('*********************************************************'),
    nl.


% Set main connection to CLIMA simulator socket at Host/Port
connectToMessServer :-
	mess_server(Host, Port),
	agentID(AgentUser),
	mess_connect(Host, Port, comm_mess), 
	mess_authenticate(AgentUser, R), !,
	(R = told(server, ok) -> 
	        report_message(system(2), 'Communication with MESSENGER server established'),
		assert(listen_to(socket, comm_mess, comm_mess)) 
	; 
		report_message(error, ['Cannot authenticate to MESSENGER server: ',R])
	).

% Finalize main connection to CLIMA simulator socket
disconnectFromMessServer :- 
	retractall(listen_to(socket, comm_mess, comm_mess)),
	mess_disconnect.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%
% OBS: handle_stream/1 must always end up succeeding!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% if messenger disconnects just drop connection
handle_stream(comm_mess) :- 
	get_socket_stream(comm_mess, read, Read), 
	at_end_of_stream(Read), !,
	disconnectFromMessServer.


% % if messenger disconnects, tries to re-connect (unless device is terminating)
handle_stream(comm_mess) :- 
	get_socket_stream(comm_mess, read, Read),
	at_end_of_stream(Read), 
	report_message(system(2), ['Messenger server disconnection']), !,
	(terminate -> 
		report_message(system(2), 
			['Termination message was already received. No reconnectoin'])
		
	;
		repeat,
		report_message(system(2), ['Reconnecting to messenger server...']),
		disconnectFromMessServer,
		(connectToMessServer -> true ; (sleep(2), fail))
	).


handle_stream(comm_mess) :- 
        mess_receive(Mess), !,
        report_exog_event(Mess, ['Exogenous action *',Mess,'* received from MESSENGER']).

handle_stream(comm_mess) :- 
	report_message(warning, ['Cannot handle data from MESSENGER server']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(Action, _, N, ok) :- 
        report_message(action, ['Executing action: *',(Action,N),'*']), 
	mess_send(Action), !.
execute(_, _, _, failed).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% DIRECT COMMUNICATION WITH MESSENGER SERVER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Connection/disconnection
mess_connect(Host, Port, ConnID) :-
	catch(socket(internet, stream, ConnID), E, 
		(report_message(error, ['Cannot create socket : ',E]),fail) ), 
	catch(connect(ConnID, Host/Port), E, 
		(report_message(error, ['Cannot connect to MESSENGER server: ',E]),fail) ).


mess_disconnect :- 
	mess_send(unregister),
	close_socket(comm_mess).

% Agent registration
mess_authenticate(AgentUser, Result) :-
	mess_send(register(AgentUser)),	!,
	(select([comm_mess], 10, [comm_mess]) ->
		mess_receive(Result)
	;	
		Result=timeout
	).

mess_send(Mess) :-
	report_message(system(5),['About to send to MESSENGER server: ',Mess]),
	get_socket_stream(comm_mess, write, Stream),
        write_term(Stream, Mess, [quoted(true)]),
        write(Stream, '.'),
        nl(Stream),
        flush(Stream).

mess_receive(Mess) :-
	report_message(system(5),['About to receive data from MESSENGER: ']),
	get_socket_stream(comm_mess, read, Stream),
        read_term(Stream, Mess, []),
	report_message(system(5),['Received from MESSENGER: ', Mess]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_mess.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%