%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_sim.pl
%
%  Author    : Sebastian Sardina
%  Time-stamp: <03/12/19 10:51:50 ssardina>
%  email     : ssardina@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/~ssardina
%  TESTED    : SWI Prolog 5.0.10 http://www.swi-prolog.org
%	       ECLiPSe 5.3 on RedHat Linux 6.2-7.2
%  TYPE CODE : system independent predicates
%
% This files provides a *simulted* environment interface with which
% it is possible to set exogenous events in an asynchronous ways using
% a TCL/TK application, type sensing outcome for actions
%
%   The interface to enter exogenous events from the keyboard is
%     achieved with a simple TCL/TK program where exogenous action can
%     be typed at any time. 
%
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
% FROM PROLOG DEPENDENT USER LIBRARY (SWI, ECLIPSE, LIBRARY):
%
% -- call_to_exec(+System, +Command, -Command2)
%      Command2 executes Command in plataform System
%
%
% Also, this device manager requires:
%
%    -- wish for running TCL/TK applications
%    -- exog.tcl TCL/TK script
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of the environment: <SIMULATOR>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(simulator). 

% Set verbose debug level
:- set_debug_level(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initializeInterfaces(_) :- initializeExog(tcltk).
finalizeInterfaces(_)   :- finalizeExog(tcltk).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	        % Run the command as a child and send its *output* to pipe "tcltk"
        tcltk_exog_file(File),
        concat_atom(['wish ', File], Command),
	call_to_exec(unix, Command, Command2),
        exec_group(Command2, [null, tcltk, null], P),
        sleep(2),    % Give time to TCL/TK program to appear
        assert(exog_proc(P)),     % Store child pid for later
        assert(listen_to(stream, tcltk, tcltk)).  % listen to tcltk

% finalizeExog: Things to do for sources of exogenous actions from the
%               virtual interface
finalizeExog(tcltk) :- 
	listen_to(stream, tcltk, tcltk), !,	% tcltk is still open
        report_message(system(1), 'Closing Tcl/Tk interface.'), 
        retract(listen_to(stream, tcltk, tcltk)),	% de-register interface
        retract(exog_proc(P)), 
        (proc_kill(P) -> true ; true).
finalizeExog(tcltk).	% It was already down

% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the SIMULATOR environment'), 
    writeln('*   You can enter exogenous actions using the TCL/TK window.'), 
    writeln('*   Action execution will be printed here and sensing '), 
    writeln('*   outcome will be asked to the user'), 
    writeln('*   Actions that are not executed in any other device are'), 
    writeln('*   executed here.'), 
    writeln('*********************************************************'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle tcl/tk stream: called when there is data comming from the tcl/tk app
handle_stream(tcltk) :- 
        read(tcltk, A),
        (A=end_of_file ->
             true          % Tcl/Tk finished
        ;
             report_exog_event(A, ['Exogenous action *',A,'* received from TCL/TK'])
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, N, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulate the execution of Action. 
% SensingResult is the sensing outcome of Action
execute(Action, T, _, Sensing) :- 
	member(T, [sensing, simsensing]), !,
        report_message(action, ['Executing sensing action: *',Action,'*']),
        write('    ------------> Enter Sensing value, terminate with ".": '),
        read(Sensing), nl.

execute(Action, _, _, ok) :- 
        report_message(action, ['Executing non-sensing action: *',Action,'*']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% OTHER CODE %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exogenous action window in SWI itself (instead of TCL/TK)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
:- use_module(library(pce)).

fileviewer(Dir) :-
        new(F, frame('File Viewer')),
        send(F, append(new(B, browser))),
        send(new(D, dialog), below(B)),
        send(D, append(button(view,
                              message(@prolog, view,
                                      B?selection?key)))),
        send(D, append(button(quit,
                              message(F, destroy)))),
        send(B, members(directory(Dir)?files)),
        send(F, open).

view(F) :-
        send(new(V, view(F)), open),
        send(V, load(F)).

	

%:- pce_autoload(file_item, library(file_item)).
 

edit_file_dialog :-
        new(D, dialog('Exogenous Events')),
        send(D, append, new(E, text_item(exog, @default, 
						and(message(@prolog, reportTea, @arg1),
					    	    message(@receiver,clear))
					))),
        send(D, append, button(send, 
				and(message(@prolog, reportTea, E?selection),
				    message(E,clear))
			)),
        send(D, append, button(cancel, message(D, destroy))),
        send(D, append, button(halt,   message(@prolog, terminateTea))),
        send(D, open).	

	
reportTea(E) :- report_message(action, ['Executing non-sensing action: *',E,'*']).
terminateTea :- report_message(action, terminate).



	%	ask_name(+Prompt, +Label, -Name)
	%	Put a prompter on the screen and wait till the user has
	%	entered a name.  Pressing cancel makes this predicate fail.
	%	Prompt is a long string, giving explanation; Label is a short
	%	label displayed for the text entry field.
	
	
	:- pce_global(@name_prompter, make_name_prompter).
	
	make_name_prompter(P) :-
		new(P, dialog),
		send(P, kind, transient),
		send(P, append, label(prompt)),
		send(P, append,
		        new(TI, text_item(name, '',
				 message(P?ok_member, execute)))),
		send(P, append, button(ok, message(P, return, TI?selection))),
		send(P, append, button(cancel, message(P, return, @nil))).
	
	
	ask_name(Prompt, Label, Name) :-
		send(@name_prompter?prompt_member, selection, Prompt),
		send(@name_prompter?name_member, label, Label),
		send(@name_prompter?name_member, clear),
		get(@name_prompter, confirm_centered, RawName),
		send(@name_prompter, show, @off),
		RawName \== @nil,
		Name = RawName.
	
	ask_name :-
		ask_name('Street', name, Street),
		writeln(Street).
	
		
create_fill_pattern_dialog :-
	new(Dialog, dialog('Fill Patterns')),
	send(Dialog, append,
		 new(M, menu(fill_pattern, cycle,
						 message(@prolog, write_ln, @arg1)))),
		send_list(M, append,
			[ menu_item(white,  @default, opcion1)
			, menu_item(grey12, @default, opcion2)
			, menu_item(grey25, @default, opcion3)
			, menu_item(grey50, @default, opcion4)
			]),
	send(Dialog, open).
		
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_sim.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%