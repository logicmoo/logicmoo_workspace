%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE     : Env/env_er1.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
% This files provides the device for working with the Evolution ER1 robot. 
%
%  An event-after event is used to implement some kind of "tracking" of
%  objects. After an object is seen, it is recorded in the database and
%  it is checked after every some time to check that he object was not lost.
%  This is done with an after_event/2 and event handlers
%
% This environment is self-contained (automatically it loads the required
%  libraries). It should be called as follows:
%
%   eclipse host=<HOST> port=<PORT> -b env_rcx.pl -e start
%
% where HOST/PORT is the address of the environment manager socket.
%
%
% Written for ECLiPSe Prolog (http://www.icparc.ic.ac.uk/eclipse/)
% running under Linux 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This predicate is used to state that an action should be executed
% whenever possible.
:- dynamic alreadySeen/2.
   
% Name of the environment: <RCX>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(er1). 


% Port where user-events are sent from ER1 to the ER1 device driver
port(events_er1, 9001).

% Set verbose debug level
:- set_debug_level(3).

% ( for this constants read OBJECT RECOGNITION PART below)
objectLostTime(3).   % The number of seconds and object has not been seen
                     % since to assume it was lost from vision
checkLostEvery(5).   % How many seconds to wait until checking for lost objects

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initializeInterfaces(L) :- 
        report_message(system(3),'Establishing connection to ER1 API port'),
           % 1 - Obtain IP and Port from L
        member([iper1,Host], L),   
        member([porter1, SPort], L),  % Get Host and Port of ER1 from L
        string_to_number(SPort, Port),
           % 2 - Start ER1 main communication and events communication
        initializeER1(Host, Port),
%        initializeER1_Events,
        report_message(system(2),
                       'Connection to ER1 API port established successfully'),
           % 3 - Set handler for the recognizing lost objects
        checkLostEvery(CheckEverySeconds),   
        set_event_handler(eobjects_lost, hobjects_lost/0),
        event_after_every(eobjects_lost, CheckEverySeconds),
        report_message(system(3),'After event for lost object started successfully'),
           % 4 - Start listening for events on ER1
        listen_to_er1,
        report_message(system(3),'Exogenous events report system started successfuly in ER1').

finalizeInterfaces(_) :- 
        finalizeER1,
%        finalizeER1_Events,
        report_message(system(3),'Disconnection from ER1 successful').

% Set main connection to API socket (port 9000 of ER1)
initializeER1(Host, Port) :-
        printKbInstructions,
        socket(internet, stream, comm_er1),
        connect(comm_er1, Host/Port),
        assert(listen_to(socket, comm_er1, comm_er1)).

% Set the extra user-event communication to ER1 (port 9001 of ER1)
initializeER1_Events :-
        socket(internet, datagram, events_er1),
        port(events_er1, PortEvents),
        bind(events_er1, 'localhost'/PortEvents),
        assert(listen_to(socket, events_er1, events_er1)). 

% Finalize main connection to API socket (port 9000 of ER1)
finalizeER1 :- 
        retract(listen_to(socket, comm_er1, comm_er1)),
        close(comm_er1).

% Finalize the extra user-event communication to ER1 (port 9001 of ER1)
finalizeER1_Events :-
        retract(listen_to(socket, events_er1, events_er1)),
        close(events_er1).


% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the ER1 device manager'), 
    writeln('*   This window will show the communication'), 
    writeln('*   with the ER1 Evolution Robot'), 
    writeln('*********************************************************'),
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle data comming from ER1: 'play done', 'move done', etc. (ER1 port 9000)
handle_stream(comm_er1) :- 
        read_response_from_er1(Data),
        string_to_atom(Data, A),
        (A=end_of_file ->
             true          % Socket closed!
        ;
             (isAnObject(A, O) ->
                  (alreadySeen(O, _) ->
                       updateObject(O)
                  ;
                       updateObject(O),
                       report_exog_event(A, _)
                  )
             ;
                  report_exog_event(A, _)
             )
        ).

% Handle data comming from ER1 User-Events socket (ER1 port 9001)
handle_stream(events_er1) :- 
        read_userevents_er1(Data), % Read a term from socket events_er1
        A=Data,
        (A=end_of_file ->
             true         % Socket closed!  
        ;
             report_exog_event(A, _)
        ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute(Action, T, N, Sensing) :- 
        send_command_to_er1('', _),
        executeER1(Action, T, N, Sensing),!,
        listen_to_er1.  % Set ER1 to the default state for reporting exog events
        

% Actual execution of Action in ER1
executeER1(Action, T, N, Sensing) :- member(T, [sensing, simsensing]), !,
        report_message(action, ['Executing sensing action: *',(Action,N),'*']),
        send_command_to_er1(Action, Response), !,  % SEND ACTION TO ER1!
        extract_sensing(Response, Sensing),
        report_message(action, 
                       ['RESPONSE for action: *',(Action,N),' : ', Sensing]).

executeER1(Action, _, N, Response) :- 
        report_message(action, 
                       ['Executing non-sensing action: *',(Action,N),'*']),
        send_command_to_er1(Action, Response). % SEND ACTION TO ER1!





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% COMMUNICATION WITH ER1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Send Command to ER1 and wait for Response from ER1
send_command_to_er1(Command, Response) :-
        write(comm_er1, Command),
        nl(comm_er1),
        flush(comm_er1), !, 
        read_response_from_er1(Response).  % Read acknowledgment from ER1
%        string_to_atom(OK, 'OK'),
%        substring(Response, OK, 1).
send_command_to_er1(_, failed).


% Read a line from ER1
read_response_from_er1(Command) :-
        read_string(comm_er1, end_of_line,_, Command).

% Sets ER1 to send all events
listen_to_er1 :-
        send_command_to_er1(events, _).

% Read Data from the User-Events socket 
read_userevents_er1(Data) :- 
        read(events_er1, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% OBJECT RECOGNITION PART %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ER1 reports an event "object <name object> ...." every time an object is
% seen such that the rate between features matched and total features of the
% object is greater than the confidence threshold. This may cause way too many
% object spot events to be reported to the environment manager.
% To solve this problem we take the following approach:
%
%    1) An exogenous event "object <name> ..." is reported whenever an object
% is seen for the first time
%    2) If an already seen object is seen again, nothing is reported
%    3) If an object has not been seen for a while, then a "lostObject <name>"
% event is reported to the environment manager.
%
% In this way we use two exogenous event: one for reporting the visualization
% of an object for the first time and another exogenous event to report that
% an already seen object has not been seen for a while. At that point, such
% object can be seen again.
%
% IMPLEMENTATION:
%
%    1) whenever an object is seen, the clause alreadySeen(Object, Time) is
% updated where Time is the current time when the Object has just been seen
%    2) there is an event_every_after/2 that checks every X seconds what
% objects has not been seen in the last Y seconds. In case an object has not
% been seen for Y seconds, a lostObject event is reported for hte object and
% its clause alreadySeen/2 is removed for such object.
%
%   X seconds is set with clause: checkLostEvery(Seconds)
%   Y seconds is set with clause: objectLostTime(Seconds)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Event is a "spot object" event
isAnObject(Event, Object) :- 
        get_object_event(Event, Object, _, _, _, _).

% Update alreadySeen/2 for Object
updateObject(Object) :- 
        retract_all(alreadySeen(Object, _)),
        get_flag(unix_time, Now),
        assert(alreadySeen(Object, Now)).

% Event handler for the lost object event "eobject_lost"
hobjects_lost :-
        get_flag(unix_time, Now),
        alreadySeen(Object, Time),
        Dif is Now-Time,
        objectLostTime(Seconds),
        Dif > Seconds,       % More than Seconds without seeing the object
        retract(alreadySeen(Object, Time)),
        concat_atom(['lostObject ',Object], LostEvent),
        report_exog_event(LostEvent,_),
        fail.
hobjects_lost.

       
% An object stop event has the following form:
%   "object <name> <no features matched> <total features> <x> <x> <distance>
% e.g., object "warning sign" 5 30 89 89 67.6
get_object_event(AString, Object, Rate, X, Y, Distance) :-
        string_to_atom(String, AString),
        string_to_atom(Quote,'\"'),
        string_to_atom(Space,' '),
        split_string(String, Space, Quote, List),
        List=[Type|RList],
        string_to_atom(Type, object), % Check it's an object spot event
        reverse(RList, [SDistance, SY, SX, SFT, SFM|LObject]),
        string_to_number(SX, X),        % Get coordinate X
        string_to_number(SY, Y),        % Get coordinate Y
        string_to_number(SFT, FT),      % Get total features
        string_to_number(SFM, FM),      % Get features matched
        Rate is (100*FM)/FT,            % Calculate rate
        string_to_number(SDistance, Distance), % Get distance to object
        reverse(LObject, LObject2),
        join_string(LObject2, Space, SObject),   % Build the object name
        string_to_atom(SObject, Object).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% OTHER CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_event_handler(170, my_system_error_handler/2).

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
restartable_builtin(select(_,_,_)).
restartable_builtin(wait(_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_er1.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
