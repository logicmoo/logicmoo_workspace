%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_int.pl
%
%  AUTHOR : Sebastian Sardina (2002)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
% This files provides the environment for working with the internet/web
%
% This environment is self-contained (automatically it loads the required
%  libraries). It should be called as follows:
%
%   eclipse host=<HOST> port=<PORT> -b env_sim.pl -e start
%   pl host=<HOST> port=<PORT> -b env_sim.pl -e start
%
% where HOST/PORT is the address of the environment manager socket.
%
% Written for ECLiPSe Prolog (http://www.icparc.ic.ac.uk/eclipse/)
% and SWI Prolog (http://www.swi-prolog.org) running under Linux 6.2-8.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
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

% Name of the environment: <SIMULATOR>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(internet). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initializeInterfaces(_).
finalizeInterfaces(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TCL/TK EXOGENOUS ACTIONS  GENERATOR - from keyboard via Tcl/Tk interface
%
% This part implements a keyboard interface to enter exogenous events
% in an asynchronous manner.
%
% If an exogenous event arrives via the keyboard, it is handled as soon
% as possible by calling handle_event/1
%
% -- initializeExog(virtual): 
%                    perform any initialization of other sources of
%                    exogenous actions that is required
% -- finalizeExog(virtual):
%                    things to do for other sources of exogenous actions
%                    at end of program
% -- checkOtherExog(virtual,-ExogList): 
%                    check whether a request has been entered via keyboard
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initializeExog(tcltk): initialization for sources of exogenous actions from
%                        virtual interface
%
% An TCL/TK independent process is initiated to read exogenous events
%  the program exog.tcl writes each exogenous action entered to a special pipe.
% At that time, a sigio signal is assigned to such pipe so that whenever data
% arrives to the pipe an interrupt is triggered which can be cached
%  by the main cycle to handle the exog action entered.
tcltk_exog_file(File):- main_dir(Dir),
                        concat_atom([Dir,'Env/exog.tcl'], File).

initializeExog(tcltk) :- 
        printKbInstructions,
        tcltk_exog_file(File),
        concat_atom(['wish ', File], Command),
        % Run the command as a child and send its *output*
        % to the pipe "tcltk"
        exec_group(Command, [null, tcltk, null], P),
        sleep(2),    % Give time to TCL/TK program to appear
        assert(exog_proc(P)),     % Store child pid for later
        assert(listen_to(stream, tcltk, tcltk)).  % listen to tcltk

% finalizeExog: Things to do for sources of exogenous actions from the
%               virtual interface
finalizeExog(tcltk) :- 
        report_message(system(1), 'Closing Tcl/Tk interface.'), 
        retract(exog_proc(P)), 
        proc_kill(P).

% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the simulator environment'), 
    writeln('*   You can enter exogenous actions using the TCL/TK window.'), 
    writeln('*   Action execution will be printed here and sensing '), 
    writeln('*   outcome will be asked to the user'), 
    writeln('*********************************************************'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle all other events for stream S: sensing, exog. events, termination
handle_stream(S) :- 
             % There is data in stream S
	listen_to(_, action(A, N,P), S),   
        report_message(system(2), ['Handling data from action ',(A,N)]),
	read(S, Data),
	((Data= end_of_file ; Data=finish) ->    
             % Stream S is EOF or requested terminination
             terminate_action(N, P, S)  
        ;
        Data = [sensing, Outcome] ->
            report_sensing(A, N, Outcome, _)
	;
	Data = [exog_action, Action] ->
            report_exog_event(Action, ['Exogenous action ',Action,
                                       ' received from Web'])
	).

% Wrap-up action number N with process Pid and stream Stream
terminate_action(N, Pid, Stream) :- 
             close(Stream),
             (proc_term(Pid) -> proc_wait(Pid, _) ; proc_kill(Pid)),
             retract(listen_to(_, action(A, N, Pid), Stream)),
             report_message(system(2),
                            ['Action ',(A, N),' has completely finished']).

      


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This part implements the execution capabilities of the environment.
% It uses the library /lib/internet.pl where there are internet and
% system capabilities. Each action is a separated thread which can rise
% exogenous events at any time. 
% Executing Action means executing predicate Action in the file internet.pl
execute(Action, _, N, null) :- 
        report_message(action, ['Executing action: ', '*',Action,'*']),
        main_dir(Dir),
        string_to_term(ActionS, Action),
        string_to_list(ActionS, ActionL),
        ActionT=perform(ActionL),
        term_to_atom(ActionT, ActionA),
            % Execute Action using library internet.pl in a separate process
        concat_atom(['eclipse -g 4M -b ', Dir, 'lib/internet.pl -e ','\'', 
                     ActionA,'\''], Command),
        call_to_exec(unix, Command, Command2), % Select right command for exec
        exec(Command2, [null, Out, null], P),
            % Start watching out for process P of ation N through stream Out
            % because the action may generate exogenous events in the future
        assert(listen_to(stream, action(Action, N,P), Out)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_int.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
