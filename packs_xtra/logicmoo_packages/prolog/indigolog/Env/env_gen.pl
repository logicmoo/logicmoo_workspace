%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FILE      : Env/env_gen.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
% This files provides the core of a device manager. The user should later
% finish the device by providing the implementation of initializeInterface/0,
% finalizeInterface/0, handle_stream/1, and execute/4.
%
% This file is self-contained (automatically it loads the required
%  libraries). It should be called as follows:
%
%   eclipse host=<HOST> port=<PORT> -b env_rcx.pl -e start
%
% where HOST/PORT is the address of the environment manager socket.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             March 22, 2003
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%
% This file defines the following predicates:
%
% -- start      : initialization of the environment (called when loaded)
% -- finalize   : finalization of the environment (called when exiting)
% -- main_dir/1 : defines the IndiGolog root directory
% -- report_exog_event(+A, +M) 
% 		report exogenous event A with message M to the environment manager
% -- report_sensing(+A, +N, +S, +M)
%		report sensing outcome S from action A with number N and message M
% -- change_action_state(+A,+N,+State,+Sensing,+LExogEvents): 
%         change the state of Action-N to State, set its Sensing and the list of
%         exogenous events generated due to the action
%
%
% 
% plus handle_stream(env_manager) for handling messages from the 
% environment manager.
%
% Required: 
%
%    -- ECLIPSE compatibility library
%
%
% In order to complete a device manager the user should implement:
%
%  -- name_dev/1              : mandatory *
%  -- initializeInterfaces(L) : mandatory *
%  -- finalizeInterfaces(L)   : mandatory *
%  -- execute/4               : mandatory *
%  -- handle_steam/1          : as needed
%  -- listen_to/3             : as needed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic terminate/0,    % To signal when the environment should quit
           listen_to/3.    % Streams and Sockets to look after

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS and main_dir/1 definintion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include('../lib/systemvar'). % Global include code and Prolog init

wait_until_close(5). % how many seconds to wait until closing the device manager


% Close a stream and always succeed
safe_close(StreamId) :-
        catch_succ(myclose(StreamId), ['Could not close socket ', StreamId]).
myclose(Id) :- close(Id).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% START OF STANDARD SECTION %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start :- catch_fail(start2,'Main cycle for device manager got exception').
start :-  
	report_message(error, 'For some reason the environment has stopped'),
	halt_device.


% Run at the beginning of the environment setting
% It initializes the communication with the environment manager and
% it initializes every source of input (rcx, tcl/tk, etc.)
start2 :- 
        name_dev(EnvId), 
    	    report_message(system(1), ['Initializing environment ', EnvId]),  
               % 1 - Obtain Host and Port number of env. manager from command 
        get_argument(host, SHost),
        get_argument(port, SPort),
        string_to_atom(SHost, Host),
        string_to_number(SPort, Port),
        assert(env_manager(Host, Port)),  % Store info about manager
               % 2 - Set debug level if appropiate option was given
        (get_argument(debug, SDebugLevel) ->
		string_to_number(SDebugLevel, DebugLevel),
		set_debug_level(DebugLevel),
	        report_message(system(1), ['Setting debug level to ',DebugLevel])
	;
	        true
	),
               % 3 - Setup stream socket with environment manager
	        report_message(system(1),'Setting socket connection with env. manager'), 
        sleep(3),  % Give time to environment manager to wait for us
        catch_fail(socket(internet, stream, env_manager),'Cannot open socket'),
        catch_fail(connect(env_manager, Host/Port),'Cannot connect to EM'),
               % 4- We should listen to env_manager
        assert(listen_to(socket, env_manager, env_manager)),  
               % 5 - Initialize different interfaces
	         % The manager may have been called with special arguments
	         % Example: the IP and Port of the robot plataform
	         % Then we read all command line arguments into CLArg
        get_list_arguments(CLArgs), 
        report_message(system(1), 'Initializing required interfaces...'), 
        initializeInterfaces(CLArgs),   %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
               % 6 - Run the main cycle
        	report_message(system(1), 'Starting main cycle'), !,
        main_cycle,
               % 7 - Terminate environment
    	    report_message(system(1), 'Finalizing domain interfaces...'), !,
        finalize(CLArgs),
	        report_message(system(1), 'Device manager totally finished; about to halt...'),
	    halt_device.

halt_device :-
		(wait_until_close(Seconds) -> true ; Seconds = 5),
		sleep(Seconds),		% Hold for Seconds and then close
		halt.


% Run when the environment is closed. 
% It should close all sockets, streams, pipes opened
finalize(CLArgs) :- 
        report_message(system(3), 'Start closing device....'),
        finalizeInterfaces(CLArgs),  %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
        close_all_sockets.	    % Close all interfaces that were opened
        

% halt device after waiting for some seconds (so that one can read debug info)
break_device :-
        report_message(system(1), 'Device manager breaking..'),
	break.


% Close all sockets for which there is a listen_to/3 entry
close_all_sockets :-
        retract(listen_to(socket, _, X)),
        safe_close(X),
		fail.
close_all_sockets.



% MAIN CYCLE: Wait for data to arrive from data comming from the
%             environment manager or any interface that was initialized
%             and stored in listen_to/3.
%             Here we wait for the tcl/tk pipe and the env_manager socket
%
% listen_to(Type, Id, X) means that X should be checked at every cycle
main_cycle :- 
        repeat,
           % Make a set LStreams with all sockets and streams with data
        findall(Stream, listen_to(stream, _, Stream), LStreams1),
        findall(Stream, listen_to(socket, _, Stream), LStreams2),
        append(LStreams1, LStreams2, LStreams),
        report_message(system(3),['Waiting the following streams: '|LStreams]),
        stream_select(LStreams, block, ReadyStreams),   % Wait for input (block)
           % Handle all the streams that have data
        report_message(system(3),['Streams ready: '|ReadyStreams]),
        handle_streams(ReadyStreams),    
        (terminate -> true ; fail). 

% order termination of the device manager
order_device_termination :- terminate -> true ; assert(terminate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLERS FOR INPUT ON STREAMS (event manager and interfaces)
%
% This section implements how each stream is handled when input arrives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic handle_stream/1.  % NEEDED BECAUSE IT MAY BE DEFINED IN 2 FILES!

% Handle a list of streams where there is info waiting
handle_streams([]).
handle_streams([S|LS]) :- 
        handle_stream(S), !, 
        handle_streams(LS).

% Standard handler for the event manager stream: 
% called when the environment manager sent something 
% usually, there is no need to modify it, one should implement execute/3
handle_stream(env_manager) :- 
        report_message(system(3),'Handling data on env_manager'),
        receive_data_socket(env_manager, [_, Data]),
	((Data = [terminate] ; Data = [system, end_of_file]) -> 
             report_message(system(2), ['Termination requested. Reason: ', Data]),
             order_device_termination 
        ;
         Data = [execute, N, Type, Action] ->
	     change_action_state(Action, N, orderExecution, null, []),
	     report_message(system(3), ['About to execute *',(Action,N),'*']), 
             (execute(Action, Type, N, S) -> 
		report_message(system(3),['Action *',(Action,N),'* executed with outcome: ',S])
	     ; 
		report_message(error,['Action *',(Action,N),'* could not execute (assumed failed)']),
		S=failed
	     ),
%	     change_action_state(Action, N, finalExecution,S,[]),
             % Report the sensing if it was not "null" (null=not available yet)
             % If it was null, then the user should make sure that the
             % sensing outcome will be eventually reported to the manager
             (S \=null -> report_sensing(Action, N, S, _) ; true)
        ;
             report_message(warning,['Uknown message from Manager: ', Data])
        ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOOL FOR REPORTING EXOGENOUS EVENT AND SENSING TO THE ENVIRONMENT MANAGER
%
%  report_exog_event(A, M)
%       report exogenous event A and message M to the environment manager
%  report_sensing(A, N, S, M) 
%       report sensing outcome S for action A with number N and print
%       message M 
%
% The device managers use this tool to report the occurrence of exogenous
% events/actions and action sensing outcomes. 
% Message is a message that should be printed in the device manager output.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
report_exog_event(A, Message) :- var(Message), !,
        report_message(exogaction, ['Exogenous action ',A,' occurred']),
        send_data_socket(env_manager, [exog_action, A]).
report_exog_event(A, Message) :- 
        report_message(exogaction, Message),
        send_data_socket(env_manager, [exog_action, A]).

report_sensing(A, N, S, Message) :- var(Message), !,
        report_message(sensing, ['Sending sensing to manager:  ', (A,N,S)]),
        send_data_socket(env_manager,[sensing, N, S]).
report_sensing(_, N, S, Message) :- 
        report_message(sensing, Message),
        send_data_socket(env_manager,[sensing, N, S]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% change_action_state(A,N,State,Sensing,LExogEvents): 
%    change the state of Action-N to State, set its Sensing and the list of
%    exogenous events generated due to the action
%
% State can be:
%      orderExecution  : order of execution received but still not executed
%      finalExecute    : action finished execution with sensing outcome S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stores action number, state, sensing outcome, and associated exogenous events
:- dynamic actionState/4.   

change_action_state(_, N, orderExecution, _, _) :- !,
	assert(actionState(N, orderExecution, null, [])).
change_action_state(_, N, State, Sensing, LExog) :-
	retract(actionState(N,OldState,OldSensing,OldExog)),
	(var(State)   -> NewState=OldState ; NewState=State),
	(var(Sensing) -> NewSensing=OldSensing ; NewSensing=Sensing),
	(var(LExog)   -> NewExog=OldExog ; append(LExog,OldExog,NewExog)),
	assert(actionState(N,NewState,NewSensing,NewExog)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_gen.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%