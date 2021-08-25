%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_man.pl
%
%
%  AUTHOR : Sebastian Sardina (2004-2006)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code (SWI or ECLIPSE Prolog)
%  TESTED : ECLiPSe 5.3 on RedHat Linux 6.2-9.0
%                SWI Prolog 5.2.8 under RedHat Linux 6.2/9.0
%
% DESCRIPTION: The environment manager deals with many multiple devices
% by communicating with them via TCP/IP sockets
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             September, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000-2002 by The University of Toronto,
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
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following system independent predicates are provided:
%
% -- initialize_EnvManager
% -- finalize_EnvManager
% -- execute_action(A, H, T, S) : execute action A of type T at history H
%                                 and resturn sensing outcome S
% -- exog_occurs(LA)		: return a list LA of exog. actions that
%				  have occurred (synchronous)
% -- indi_exog(Action)          : asserted whenever exog Action occurred
% -- set_type_manager(+T)       : set the implementation type of the env manager
%
%
% The following code is required:
%
% FROM THE INDIGOLOG MAIN CYCLE:
%
% -- doing_step : IndiGolog is thinking a step
% -- abortStep  : abort the computation of the current step
% -- exog_action_occurred(LExoAction) 
%               : to report a list of exog. actions LExogAction to the top-level
%
% FROM THE DOMAIN DESCRIPTOR:
%
% -- server_port(Port)       
%	Port to set up the environment manager
% -- load_device(Env, Command, Options) 
%     	for each environemnt to be loaded
% -- how_to_execute(Action, Env, Code)
%       Action should be executed in environment Env with using code Code
% -- translateExogAction(Code, Action)  
%	Code is action Action
% -- exog_action(Action) 
%	Action is an exogenous action
%
% ELSEWHERE:
%
% -- report_message(T, M)       : report messsage M of type T
% -- send_data_socket/2
% -- receive_list_data_socket/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	got_sensing/2,      % stores sensing outcome for executed actions
	got_exogenous/1,    % stores occurred exogenous actions
	counter_actions/1,  % Carries a counter for each action performed
	env_data/3,         % (Env name, Pid, Socket) of each device
	executing_action/3, % Stores the current action being executed
	translateExogAction/2,
	translateSensing/3, % Translates sensing outcome to high-level sensing
	how_to_execute/3,   % Defines how to execute each high-level action
	type_manager/1,     % Defines the implementation type of the manager
	server_port/1,
	server_host/1.	    % Host and port for the environment manager

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_env(manager).   % We are the "environment manager"
counter_actions(0).  % Counter for (executed) actions

set_option('type_em : set type of environment manager: thread or signal').
set_option(type_em, T) 	:- 
	set_type_manager(T),
	report_message(system(0), ['** EM fixed to ', T]).


% Set the type of the environment manager: thread or signal based
set_type_manager(T) :- 
	atom(T),
	member(T, [thread,signal]),
        report_message(system(2), 
                       ['(EM) Setting environment manager type to: ',T]), 
        retractall(type_manager(_)), 
        assert(type_manager(T)).
set_type_manager(_) :- 
        report_message(warning, '(EM) Type of env. manager cannot be set!').

type_manager(thread). % Default execution for the env. manager


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INITIALIZATION AND FINALIZATION PART
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A - Initialize environment manager
initializeEM :- 
          report_message(system(2), '(EM) 1 - Resetting the number of actions...'),
	retractall(counter_actions(_)),
	assert(counter_actions(0)),
          report_message(system(1), '(EM) 2 - Openinig server-input socket...'),
    socket(internet, stream, em_socket), % signal when data comes
		% Build the Address where the manager will be listeling
	(server_host(ServerHost2) -> true ; gethostname(ServerHost2)),
    string_to_atom(ServerHost2, ServerHost),
    server_port(ServerPort),
    Address=ServerHost/ServerPort,
    bind(em_socket, Address),
    report_message(system(1), bind(em_socket, Address)),
          report_message(system(1),'(EM) 3 - Loading different devices...'),
    findall(Env, load_device(Env,_, _), LEnv),
    (LEnv=[] ->
               	report_message(warning,'(EM) No devices defined to load!') 
	;
				report_message(system(2),['(EM) Devices to load: '|LEnv])            
	),
    length(LEnv, LengthLEnv),
    listen(em_socket, LengthLEnv),
    start_env(LEnv, Address), !, % Start each of the devices used (LEnv)
          report_message(system(2),'(EM) 4 - Start EM cycle...'),
    type_manager(Type),
	start_env_cycle(Type).   % Start the env. manager main cycle


% The finalization of the EM involves:
%	1 - Close all the open device managers
%	2 - Terminate EM cycle
%	3 - Close EM server socket em_socket
%	4 - Report the number of actions that were executed
finalizeEM :- 
    	report_message(system(2),'(EM) 1 - Closing all device managers...'),
    setof(Dev, X^Y^env_data(Dev, X, Y), LDev), % Get all current open devices
	close_dev(LDev),	% Close all the devices found
	sleep(3), 			% Wait to give time to devices to finish cleanly
		!, report_message(system(2), '(EM) 2 - Terminating EM cycle...'), 
	catch(wait_for_children,_,true),
    type_manager(Type),
	finish_env_cycle(Type),   	% Terminate main env. manager cycle
		!, report_message(system(2),'(EM) 3 - Closing EM server socket...'), 
    safe_close(em_socket),		% Disconnect server socket
		!, report_message(system(2),'(EM) 4 - All finished...'),
    counter_actions(N),
    report_message(system(1),
       	['(EM) Finalization of EM completed with *', N, '* executed actions.']).


wait_for_children :-
	wait(PId, S), !,
	(ground(S) -> true ; S=free),
        report_message(system(3), ['(EM) Successful proccess waiting: ',(PId,S)]),
	wait_for_children.
wait_for_children.

% Close a stream and always succeed
safe_close(StreamId) :-
        catch_succ(myclose(StreamId), ['Could not close socket ', StreamId]).
myclose(Id) :- close(Id).



% B - Initialization and Finalization of the environment manager main cycle
%		start_env_cycle/0  : starts EM cycle
%		finish_env_cycle/0 : terminates EM cycle
%       The EM cycle basically waits for data arriving to any of the open
%       connections to the device managers. When it receives a message
%	(e.g., sensing outcome, exog. action, closing message) it hanldes it.

% B.1 THREAD IMPLEMENTATION (multithreading Prologs like SWI)
%	 There is a separated thread to handle incoming data. The thread
%	 should BLOCK waiting for data to arrive
start_env_cycle(thread) :- 
	thread_create(catch(em_cycle_thread,E,
		(E=finish -> 
		 	report_message(system(2),'(EM) EM cycle finished successfully')
		; 
		 	report_message(error,['EM Thread Error:',E])
		)),_,[alias(em_thread)]).
em_cycle_thread :- em_one_cycle(block), !, em_cycle_thread.
em_cycle_thread :-	 % if em_one_cycle/1 has nothing to wait, then just terminate
	 	report_message(system(5),'(EM) EM cycle finished, no more streams to wait for...').


finish_env_cycle(thread) :-
	(current_thread(em_thread, running) -> 
		% Signal thread explicitly to finish!
		thread_signal(em_thread, throw(finish))  
	;
		% The thread has already finished (because all devices were closed)
		true
	), 
	thread_join(em_thread,_),
	report_message(system(3),'(EM) Environment cycle (thread) finished').  




% C - em_one_cycle/1 : THIS IS THE MOST IMPORTANT PREDICATE FOR THE EM CYCLE
%
% 	em_one_cycle(HowToWait) does 1 iteration of the waiting cycle and
%	waits HowMuchToWait (seconds) for incoming data from the devices
% 	If HowMuchToWait=block then it will BLOCK waiting... (good for threads)
em_one_cycle(HowMuchToWait) :-
	report_message(system(5),'(EM) Waiting data to arrived at env. manager (block)'),
	% Get all the read-streams of the environments sockets
	setof(Socket, X^Y^env_data(X, Y, Socket), ListSockets),
	% Check which of these streams have data waiting, i.e., the "ready" ones
	report_message(system(5), ['(EM) Blocking on environments:: '|ListSockets]),
	stream_select(ListSockets, HowMuchToWait, ListSocketsReady),   !, % BLOCK or wait?
	% Get back the name of the environments of these "ready" streams
	setof(Env, S^X^(member(S, ListSocketsReady),
			env_data(Env, X, S)), ListEnvR),
	report_message(system(5), ['(EM) Handling messages in devices:: '|ListEnvR]),
	% Next, read all the events waiting from these devices
	get_events_from_env(ListEnvR, ListEvents),
	% Finally, handle all these events
	handle_levents(ListEvents).

% Given a list of devices that have information on their sockets
% collect all the data from them
get_events_from_env([], []).
get_events_from_env([Env|LEnv], TotalListEvents) :-
        env_data(Env, _, SocketEnv),
        receive_list_data_socket(SocketEnv, LEventsEnv),
        get_events_from_env(LEnv, RestEvents),
        append(LEventsEnv, RestEvents, TotalListEvents).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handle_levents(+LE,-LT): handle list of events LE and return their 
%                        types in list LT
%
% Up to now, events are either a exogenous events or unknown events.
%
% First handle all events. Then, if there were exogenous event report them
% in a list to top-level cycle with exog_action_occurred/1
%
% 2) Unknown Event: just inform it with message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prioritized serving of a list of occurred events

% 1 - First serve sensing outcomes 
handle_levents(L) :-
	member([Sender,[sensing, N, Outcome]], L),
	handle_event([Sender,[sensing, N, Outcome]]),
	fail.

% 2 - Second serve exogenous actions
handle_levents(L) :-
	member([Sender,[exog_action, CodeAction]], L),
	handle_event([Sender,[exog_action, CodeAction]]),
	fail.

% 3 - Third serve other events
handle_levents(L) :-
	member([Sender,[T|R]], L),
	\+ member(T, [sensing, exog_action]),
	handle_event([Sender,[T|R]]),
	fail.

% 4 - Finally, signal exog_action_occurred/1 if there were exogenous actions
handle_levents(_) :-
	findall(ExoAction, got_exogenous(ExoAction), LExoAction),
	LExoAction\=[],                   % There were exogenous actions
	retractall(got_exogenous(_)),
	exog_action_occurred(LExoAction), % Report all the actions to top-level
	fail.

% 5 - Always succeeds as the last step
handle_levents(_).









% handle_event/1: Handle each *single* event
handle_event([_, [sensing, N, OutcomeCode]]) :- !, 
	executing_action(N, Action, _),
	(translateSensing(Action, OutcomeCode, Outcome) ->  true ; Outcome=OutcomeCode),
	assert(got_sensing(N, Outcome)),
		report_message(system(5),
			['(EM) Sensing outcome arrived for action ',
			(N, Action), ' - Sensing Outcome:: ',(OutcomeCode,Outcome)]).

handle_event([_, [exog_action, CodeAction]]):- 
	(translateExogAction(CodeAction, Action) -> true ; Action=CodeAction),
    exog_action(Action), !,
	assert(got_exogenous(Action)),
        report_message(system(5),
	               ['(EM) Exogenous action occurred:: ', (CodeAction, Action)]).

handle_event([socket(Socket), [_, end_of_file]]) :- !,  % Env has been closed! 
        env_data(Env, _,Socket),                        % remove it
        report_message(system(2),['(EM) Device ',Env,' has reported termination!']),
        delete_dev(Env).

handle_event([Sender, [Type, Message]]):- !, % The event is unknown but with form
        report_message(system(5), 
        	['(EM) UNKNOWN MESSAGE! Sender: ', Sender,
                                   ' ; Type: ' ,Type,' ; Message: ', Message]).

handle_event(Data):-                         % The event is completely unknown
        report_message(system(5), ['(EM) UNKNOWN and UNSTRUCTURED MESSAGE!:: ', Data]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START AND CLOSE DEVICE MANAGERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start_env(LEnv, Address) : starts a list of device managers
%
%  each device is started in a separate xterm window. We pass the 
%  Address so that the device knows where to send its data
%  Also we receive from the device its address to know where to
%  send data to it. 
%  env_data/3 stores the Pid and Address of each device started
start_env([], _).
start_env([Env|LEnv], Address) :-
	Address = Host/Port,
	load_device(Env, Command, [Host,Port]),
	call_to_exec(unix, Command, Command2), % Select right command for exec
		report_message(system(5), ['(EM) Command to initialize device ', Env, ': ', Command2]),
	exec_group(Command2, [], Pid),
	accept(em_socket, From, Env), % Wait until the device connects to socket em_socket  
		report_message(system(1), ['(EM) Device ', Env, ' initialized at: ', From]),
	assert(env_data(Env, Pid, Env)),
	start_env(LEnv, Address).

% Tell each device to terminate
close_dev([]).
close_dev([Env|LEnv]) :-
        send_data_socket(Env, [terminate]), % Tell device to terminate
        % (delete_dev(Env) -> true ; true),   % not needed, will be deleted automatically
        close_dev(LEnv).

% Delete all information wrt device Env.
% delete_dev/1 is called automatically when the device has reported to be terminated
delete_dev(Env) :-
        retract(env_data(Env, Pid, SocketEnv)),
        %catch((wait(Pid, S) -> true ; true),E,true),
	(ground(S) -> true ; S=free),
       	report_message(system(3),['(EM) Environment *',Env,'* deleted!',
       				  ' - Waiting result: ',(Pid, S)]),
        safe_close(SocketEnv).	% Disconnect server socket



	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTION OF ACTIONS SECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tell the corresponding device to execute an action
% how_to_execute/3 says how (which device and which action code) to
%    execute a high-level action
% if the action is a sensing action, it waits until observing its outcome
execute_action(Action, H, Type, N2, Outcome) :-
		% Increment action counter by 1 and store action information
	retract(counter_actions(N)),
	N2 is N+1,
	assert(counter_actions(N2)),	% Update action counter
	assert(executing_action(N2, Action, H)), % Store new action to execute
		% Learn how Action should be executed (Env, Code of action)
	map_execution(Action, Env, Code),   % From domain spec
		% Send "execute" message to corresponding device
	report_message(system(2), 
		['(EM) Start to execute the following action: ',(N2, Action, Env, Code)]),
	env_data(Env, _, SocketEnv),
	send_data_socket(SocketEnv, [execute, N2, Type, Code]),
	report_message(system(3),
		['(EM) Action ',N2,' sent to device ',Env,' - Waiting for sensing outcome to arrive']),!,
		% Busy waiting for sensing outcome to arrive (ALWAYS)
	repeat,   
	got_sensing(N2, Outcome),
	retract(executing_action(N2, _, _)),
	retract(got_sensing(N2, _)), !,
	report_message(system(2), 
		['(EM) Action *', (N2, Action, Env, Code), '* completed with outcome: ',Outcome]).
execute_action(_, _, _, N, failed) :- counter_actions(N).


% Find an adequate device manager that can execute Action and it is active
map_execution(Action, Env, Code) :-
        how_to_execute(Action, Env, Code),
        env_data(Env, _, _), !.  % The device is running 
% Otherwise, try to run Action in the simulator device 
map_execution(Action, simulator, Action) :- env_data(simulator, _, _), !.

% Otherwise, try to run Action in the simulator device 
map_execution(Action, _, _) :-
        report_message(warning, ['(EM) Action *', Action, 
	                      '* cannot be mapped to any device for execution!']),
	fail.




% Exogenous actions are handled async., so there is no need to handle
% sync. exogenous actions. It is always empty.
exog_occurs([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_man.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%