%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE     : Env/env_rcx.pl
%
%  AUTHOR : Sebastian Sardina (2002)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent predicates (uses event_after to query RCX)
%  TESTED : ECLiPSe 5.3 on RedHat Linux 6.2-8.0
%
% This files provides the environment for working with the RCX.
%  An event-after event is used to talk to the RCX every some number of
%  seconds. At each talk, if there is an action pending to be executed
%  (to_execute/3), such action is sent to the RCX. Otherwise, the RCX is
%  queried for an occurred exogenous action.
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
% running under Linux 6.2-7.2-8.0
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
%
% -- All compatibility libraries depending on the architecture such us:
%    -- compat_swi/compat_ecl compatibility libraries providing:
%
% -- The following two dynamic predicates should be available:
%
%    -- listen_to(Type, Name, Channel) 
%            listen to Channel of Type (stream/socket) with Name
%    -- terminate/0
%            order the termination of the application
%
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

% Consult Lego low-level subsystem manager
%  This file is in charge of the actual communication with the RCX brick 
:- use_module(rcx_ecl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This predicate is used to state that an action should be executed
% whenever possible.
:- dynamic to_execute/3.
   
% Name of the environment: <RCX>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(rcx). 

% Set verbose debug level
:- set_debug_level(1).


% No of times we should retry when sending an action order to the RCX
noTries(10).

% Frequency to trigger a new comunication to the RCX
ask_rcx_every(3).    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initializeInterfaces(_) :- initializeExog(rcx).
finalizeInterfaces(_)   :- finalizeExog(rcx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMUNICATION Process with the RCX via after_every/2 event
%
% A special after_every/2 event is set to continously communicatate the PC
% with the RCX. Every some number of seconds the PC talks to the RCX:
%
%   If there is a pending action to be executed (to_execute/4), such action
%     is sent to the RCX. Sensing outcome is stored in the database.
%   If no action is pending, then the process asks the RCX for any exogenous
%     action. If there is an exogenous event from the RCX, it is handled 
%     by calling the corresponding handle_event/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up an after_every event to trigger event rcx_talk every Sec seconds
initializeExog(rcx) :-
        printKbInstructions,
        initializeRcx,
        set_event_handler(rcx_talk, rcx_com/0),
        ask_rcx_every(Sec), 
        event_after_every(rcx_talk, Sec),
        report_message(system(3), 'Opening RCX communication').

% Stop the after_every event
finalizeExogRcx(rcx) :- 
        cancel_after_event(rcx_talk),
        report_message(system(3), 'Closing RCX communication').

% If there is something pending to execute, execute it on the RCX
% Otherwise ask for possible exogenous events
rcx_com :- 
        retract(to_execute(Action, T, N)) -> 
            report_message(action,['Executing action: *', Action,'*']),
            executeRcx(Action, 1, T, S),
            report_sensing(Action, N, S, _)  % REPORT SENSING OUTCOME!
        ;
            report_message(system(2), 'Querying the RCX for exogenous events'),
            queryRcxExogAction, 
            report_message(system(3), 'Finished with RCX communication').

% Ask for exogenous events. If there is one, then send it to the
% environment manager via soutput
queryRcxExogAction:- 
        checkRcxExog(ExogList), !, 
        (ExogList == [] -> 
             true 
        ; 
             ExogList = [A], 
             Message = ['Exogenous action *',A,'* received from RCX'],
             report_exog_event(A, Message)    % REPORT EXOGENOUS ACTION!
        ).

queryRcxExogAction :- 
        report_message(warning, 'No reply from the RCX for exog. events').


% checkRcxExog(-RcxExogList): Check for occurrence of exogenous actions
%     at RCX. At present RCX can only report one exogenous action at a time
checkRcxExog([Action]) :- receiveRcxActionNumber([Action]), !.
checkRcxExog([]).    % No exogenous action from RCX

% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the RCX device manager'), 
    writeln('*   This window will show the communication'), 
    writeln('*   with the RCX brick'), 
    writeln('*********************************************************'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle tcl/tk stream: called when there is data comming from the tcl/tk app
handle_stream(tcltk) :- 
        read(tcltk, A),
        (A=end_of_file ->
             true          % Tcl/Tk finished
        ;
             Message = ['Exogenous action *',A,'* received from TCL/TK'],
             report_exog_event(A, Message)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, N, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Just assert the action to_execute/3 so that it is sent to the rcx
% in the next communication
% No sensing is sent now, but it will be sent in rcx_com/0.
execute(Action, Type, N, null) :- 
        report_message(action,['Action to be executed received: ', Action]),
        assert(to_execute(Action, Type, N)).


% executeRcx(Action, NoTry, Type, Sensing) 
%       execute Action for the NoTry time on the RCX
executeRcx(ActionCode, _, sim_sensing, SensingResult) :- 
        sendRcxActionNumber(ActionCode, _), !,
        write('---> Enter Sensing value, terminate with ".": '),
        read(SensingResult).

executeRcx(ActionCode, _, T, SensingResult) :- T\== sim_sensing,
        sendRcxActionNumber(ActionCode, SensingResult), !.

executeRcx(A, NoTry, T, S) :- 
        NoTry2 is NoTry+1,
        noTries(Limit),
        Limit >= NoTry2, !,
        report_message(warning, ['Error executing action ', A,  
                                 ' Trying again for the ', NoTry2, ' time.']), 
        executeRcx(A, NoTry2, T, S).

executeRcx(A, _, _, failed) :- 
        report_message(action, ['Execution of action ', A, ' has failed!']).



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
% EOF:  Env/env_rcx.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%