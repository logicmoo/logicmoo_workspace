%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_java_swing.pl
%
%  AUTHOR    : Stefano Valentini
%  EMAIL     : 
%  WWW       : 
%  TESTED    : SWI Prolog 5.0.10 http://www.swi-prolog.org
%  TYPE CODE : 
%
% This files provides a .........
%
% This environment is self-contained (automatically it loads the required
%  libraries). It should be called as follows:
%
%   pl host=<HOST> port=<PORT> -b env_wumpus.pl -e start
%	idrun=<id for the run> idscenario=<id to load for fixed world>
%	size=<size of grid> ppits=<prob of pits> nogolds=<no of golds>
%	ipwumpus=<applet ip> portwumpus=<applet port>
%
% For example:
%
%   pl host='cluster1.cs.toronto.edu' port=9022 -b env_wumpus.pl -e start
%	idrun=test(10) idscenario=random
%	size=8 ppits=15 nogolds=1
%	ipwumpus='cluster1.cs.toronto.edu' portwumpus=9002
%
% where HOST/PORT is the address of the environment manager socket.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(env_gen).

name_dev(javaswing).

:- set_debug_level(3).

initializeInterfaces(L) :- 
        printKbInstructions,
	ground(L),
	set_debug_level(3),
	report_message(system(1), 'Building JAVA SWING Configuration'),
   	report_message(system(1), 'Building JAVA SWING Completed!'),
		% Get the locatio of the Java SWING (IP + PORT)
      	member([ipswing,SIP], L),   
      	string_to_atom(SIP, IP),
      	member([portswing, SP], L),
      	string_to_number(SP, Port),
	report_message(system(0), 'INITIALIZING INTERFACES!'),
	report_message(system(1), ['Connecting to JAVA SWING interface at ',IP,'/',Port]),
      	initializeJavaSwing(IP, Port),
	report_message(system(0), 'INITIALIZATION COMPLETED!').
	

finalizeInterfaces(_)   :- 
	report_message(system(0), 'FINALIZING INTERFACES!'),
	finalizeJavaSwing(_,_), 
	%finalizeStatistics,	% SEB: PREDICATE NOT DEFINED
	report_message(system(0), 'FINALIZATION COMPLETED!').







initializeJavaSwing(Host, Port):-
      	report_message(system(3), ['Establishing connection to JAVA SWING:',Host,'/',Port]), !,
      	socket(internet, stream, comm_java),
      	connect(comm_java, Host/Port),
      	assert(listen_to(socket, comm_java, comm_java)),
      	report_message(system(1), 'Connection to JAVA SWING port established successfully').

finalizeJavaSwing(_, _) :-
   	report_message(system(3), ['Disconnecting from JAVA SWING']), !,
	listen_to(socket, comm_java, comm_java), !,	% check it is open
	send_command_to_swing(end, _),
	sleep(1),
	closeJavaSwingCom,
	report_message(system(1), 'Connection to JAVA SWING port disconnected successfully').
	
finalizeJavaSwing(_, _).	% The swing was already down

closeJavaSwingCom :-
        retract(listen_to(socket, comm_java, comm_java)), % de-register interface
        close(comm_java).

handle_stream(comm_java) :- 
        read_response_from_swing(Data),
        string_to_atom(Data, A),
        (A = end_of_file ->
        	% Close socket communication with swing (but device manager keeps running with no GUI)
            closeJavaSwingCom   
        ;
            report_exog_event(A, _)
	).




execute(Action, T, _, Sensing) :- 
	member(T, [sensing, simsensing]), !,
        report_message(action, ['Executing sensing action: *',Action,'*']),
        send_command_to_swing('    ------------> Enter Sensing value, terminate with ".": '),
        read_response_from_swing(Sensing), nl.

execute(Action, _, _, ok) :- 
        report_message(action, ['Executing non-sensing action: *',Action,'*']).

:- type_prolog(ecl) -> 
	set_event_handler(170, my_system_error_handler/2) ; true.

my_system_error_handler(E, Goal) :-
        (
            errno_id(`Interrupted system call`),
%            errno_id(170, M), errno_id(M),  % M is "Unknown error 170" ??
            restartable_builtin(Goal)
        ->
            call(Goal)
        ;
            errno_id(M),
            report_message(error, M),
            read(_),
            error(default(E), Goal)
        ).

% Builtins that can raise EINTR and can be restarted after that
restartable_builtin(accept(_,_,_)).
restartable_builtin(cd(_)).
restartable_builtin(close(_)).
restartable_builtin(connect(_,_)).
restartable_builtin(select_stream(_,_,_)).
restartable_builtin(stream_select(_,_,_)).
restartable_builtin(wait(_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% COMMUNICATION WITH JAVA SWING%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



send_command_to_swing(_, ok) :- \+ javaSwingOn, !.
send_command_to_swing(Command, Response) :-
	any_to_string(Command, SCommand),
	write(comm_java, SCommand),
	nl(comm_java),
	flush(comm_java), !, Response=ok.
%	read_response_from_swing(Response).  % Read acknowledgment from WUMPUS
send_command_to_swing(_, failed).

% Read a line from swing
read_response_from_swing(_) :- \+ javaSwingOn, !.
read_response_from_swing(Command) :-
	read_string(comm_java, end_of_line,_, Command).


% Wumpus applet is running
javaSwingOn :- listen_to(socket, comm_java, comm_java).
	



	
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the JAVA SWING SIMULATOR environment*****'),
    writeln('*created by STEFANO VALENTINI ***************************'),
    writeln('*********************************************************'), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_java_swing.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    