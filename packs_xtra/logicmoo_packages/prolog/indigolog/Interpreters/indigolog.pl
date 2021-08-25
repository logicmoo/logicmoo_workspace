%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/indigolog.pl
%
%       IndiGolog Top Level Executor (Version 5)
%
%  AUTHOR : Sebastian Sardina (prev. Hector Levesque & Maurice Pagnucco)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%    This file provides the implementation for the top-level part of 
%    the online IndiGolog executor. 
%
%   The main tool provided in this file is the following predicate:        
%
% -- indigolog(E):  E is an IndiGolog program
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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
%         Copyright (c) 2002-2005 by The University of Toronto,
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This files provides:
%
% -- indigolog(+E)       
%       run IndiGolog program E in the main cycle
% -- now(-H)             
%       H is the current history
% -- pasthist(?H)        
%       H is a past situation w.r.t. the current one
% -- doingStep		  
%       the main cycle is computing a step
% -- exog_action_occurred(LExoAction) 
%	to report a list of exog. actions LExogAction to the top-level
%
% -- exists_pending_exog  
%       there are exogenous events pending to be dealt
% -- set_option(+O, +V)  
%       set option O to value V. Current options are:
%
%	+ wait_step 	number of seconds to wait between steps
%	+ debug_level 	level for debug messages
%	+ type_manager 	define the type of the environment manager (thread/signal)
%
% -- error(+M)            
%       an error has occurred with message M
% -- warn(+M)   
%       warn the user of event M
%
%
%
%  The following should be provided for this file:
%
% LANGUAGE CONSTRUCTS IMPLEMENTATION (transition system):
%
% -- trans(+P,+H,-P2,-H2) 
%       configuration (P,H) can perform a single step to configuration (P2,H2)
% -- final(+P,+H)        
%       configuration (P,H) is terminating
%
% FROM ENVIRONMENT MANAGER (eng_man.pl):
%
% -- execute_action(+A, +H, +T, -Id, -S)  
%	execute action A of type T at history H and resturn sens. 
%       	S is the sensing outcome, or "failed" if the execution failed
%		Id is the identification for the action from the EM
% -- exog_occurs(-L)
%	return a list L of exog. actions that have occurred (sync)
% -- initializeEM/0 
%	environment initialization
% -- finalizeEM/0  
%	environment finalization
% -- set_type_manager(+T)        
%       set the implementation type of the env manager
%
%
% FROM TEMPORAL PROJECTOR (evalxxx.pl):
%
% -- debug(+A, +H, -S)	    
%       debug routine
% -- pause_or_roll(+H1,-H2)  
%       check if the DB CAN roll forward  
% -- can_roll(+H1) 
%       check if the DB CAN roll forward
% -- must_roll(+H1) 
%       check if the DB MUST roll forward
% -- roll_DB(+H1) 
%       check if the DB MUST roll forward
% -- initializeDB/0
%       initialize projector
% -- finalizeDB/0
%       finalize projector
% -- handle_sensing(+A,+H,+Sr,-H2) 
%	change history H to H2 when action A is executed in history 
%	H with Sr as returning sensing value
% -- sensing(+A,-SL)	    : 
%       action A is a sensing action with possible sensing outcome list SL
% -- system_action(+A)      : 
%       action A is an action used by the system 
%       e.g., the projector may use action e(_,_) to store sensing outcomes
%
% FROM THE SPECIFIC DOMAIN OR APPLICATION:
%
% -- simulateSensing(+A)  
%       sensing outcome for action A is simulated
% -- type_prolog(+P)	 
%       name of prolog being used (ecl, swi, vanilla, etc)
%
% OTHERS TOOLS (PROLOG OR LIBRARIES):
%
% -- sleep(Sec)             : wait for Sec seconds
% -- turn_on_gc             : turns on the automatic garbage collector
% -- turn_off_gc            : turns off the automatic garbage collector
% -- garbage_collect        : perform garbage collection (now)
% -- report_message(+T, +M) : report message M of type T
% -- set_debug_level(+N)    : set debug level to N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic sensing/2,   	% There may be no sensing action
	indi_exog/1,		% Stores exogenous events not managed yet
	now/1,            	% Used to store the actual history
	rollednow/1,           	% Part of now/1 that was already rolled fwd
	wait_at_action/1, 	% Wait some seconds after each action
	doing_step/0,     	% A step is being calculated
	pause_step/0.     	% Pause the step being calculated
	
% Predicates that they have definitions here but they can defined elsewhere
:- multifile(set_option/1),	
   multifile(set_option/2),
   multifile(exog_action/1),	% Many modules can register exog. actions 
   multifile(system_action/1).  % Many modules can register system actions 


:- style_check(-discontiguous).
% :- style_check(-singleton).
% :- style_check(-atom).


:- ensure_loaded(transfinal).  % Load the TRANS and FINAL definitions
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    CONFIGURATION SECTION
%
% This tools allow the user to tune different global options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_option/1/2 are used to define parameters the user can set
% set_option/1 is used for the help tool set_option/0
% set_option/2 is the actual definition of the parameter configuration

set_option :-
	writeln('set_option(Option, V): 
		 sets Option to value V, where Options may be:'),
	nl,
	set_option(X),
	tab(1),
	writeln(X),
	fail.
set_option.

% Set the wait-at-action to pause after the execution of each prim action
set_option('wait_step : pause V seconds after each prim. action execution.').
set_option(wait_step, N) :- wait_step(N).

wait_step(S) :- 
	S==0,
	report_message(system(0), '** Wait-at-action disabled'),
	retractall(wait_at_action(_)).
wait_step(S) :- 
	number(S),
	report_message(system(0), ['** Wait-at-action enable to ',S, ' seconds.']), 
	retractall(wait_at_action(_)), 
	assert(wait_at_action(S)).
wait_step(_) :- 
	report_message(warning, 'Wait-at-action cannot be set!').


set_option('debug_level : set debug level to V.').
set_option(debug_level, N) 	:- 
	set_debug_level(N),
	report_message(system(0), ['** System debug level set to ',N]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    SOME SYSTEM BUILT-IN EXOGENOUS ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BUILT-IN exogenous actions that will be mapped to system actions for the cycle
exog_action(debug).	% Show debugging information	
exog_action(halt).		% Terminate program execution by jumping to the empty program
exog_action(abort).		% Abort program execution by jumping to ?(false) program
exog_action(break).		% Pause the execution of the program
exog_action(reset).		% Reset agent execution from scratch
exog_action(start).		% Start the execution of the program

exog_action(debug_exec).	% Show debugging information	
exog_action(halt_exec).		% Terminate program execution by jumping to the empty program
exog_action(abort_exec).		% Abort program execution by jumping to ?(false) program
exog_action(break_exec).	% Pause the execution of the program
exog_action(reset_exec).		% Reset agent execution from scratch
exog_action(start_exec).		% Start the execution of the program

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    MAIN LOOP
%
% The top level call is indigolog(E), where E is a program
% The history H is a list of actions (prim or exog), initially []
% Sensing reports are inserted as actions of the form e(fluent,value)
%
% indigo/2, indigo2/3, indigo3/3 implement the main architecture by 
%      defyining a 3-phase main cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :- 
%	set_option(debug_level,3),
	report_message(system(0),'Starting ENVIRONMENT MANAGER...'),
	initializeEM,    	  	% Initialization of environment
	report_message(system(0),'ENVIRONMENT MANAGER was started successfully.'),
	report_message(system(0),'Starting PROJECTOR...'),
	initializeDB,             	% Initialization of projector
	report_message(system(0),'PROJECTOR was started successfully.'),
	reset_indigolog_dbs.      	% Reset the DB wrt the controller

fin  :- 
	report_message(system(0),'Finalizing PROJECTOR...'),
	finalizeDB,               	% Finalization of projector
	report_message(system(0),'PROJECTOR was finalized successfully.'),
	report_message(system(0),'Finalizing ENVIRONMENT MANAGER...'),
	finalizeEM,      		% Finalization of environment
	report_message(system(0),'ENVIRONMENT MANAGER was finalized successfully.').


% Clean all exogenous actions and set the initial now/1 situation 
reset_indigolog_dbs :- 
	retractall(doing_step),
	retractall(indi_exog(_)), 
	retractall(rollednow(_)),
	retractall(now(_)),
	update_now([]),
	assert(rollednow([])),
	assert((indi_exog(_) :- fail)),
	fail.
reset_indigolog_dbs.


%%
%% (A) INTERFACE PREDICATE TO THE TOP LEVEL MAIN CYCLE
%%
indigolog :- indigolog(none).
indigolog(_) :-		% Used to require a program, now we start proc. main always (March 06)
	init,  !, 
	(proc(main, E) ->		% obtain main agent program
		report_message(system(0),'Starting to execute the main program'),
		indigo(E,[]), !
	;
		report_message(system(0),'No main program to execute')
	),
	report_message(system(0),'Program execution finished. Closing modules...'),
	fin, !,
	report_message(system(0),'Everything finished - HALTING TOP-LEVEL CONTROLLER').

%%
%% (B) MAIN CYCLE: check exog events, roll forward, make a step.
%%
indigo(E,H) :- 
	handle_rolling(H,H2), !,		% Must roll forward?
	handle_exog(H2,H3),   !, 		% Handle pending exog. events
	prepare_for_step,			% Prepare for step
	mayEvolve(E,H3,E4,H4,S), !,	% Compute next configuration evolution
	wrap_up_step,				% Finish step
	(S=trans -> indigo2(H3,E4,H4) ;
	 S=final -> report_message(program,  'Success.') ;
	 S=exog  -> (report_message(program, 'Restarting step.'), indigo(E,H3)) ; 
	 S=failed-> report_message(program,  'Program fails.') 
	).

%%
%% (C) SECOND phase of MAIN CYCLE for transition on the program
%% indigo2(+H1,+E,+H2): called from indigo/2 only after a successful Trans on the program
%% 	H1 is the history *before* the transition
%% 	E is the program that remains to execute
%% 	H2 is the history *after* the transition
%%
indigo2(H,E,H)          :- 
	indigo(E,H).	% The case of Trans for tests
indigo2(H,E,[sim(_)|H]) :- !, 
	indigo(E,H).	% Drop simulated actions
indigo2(H,E,[wait|H])   :- !,	
	pause_or_roll(H,H1), 
	doWaitForExog(H1,H2),
	indigo(E,H2).
indigo2(_,E,[debug_exec|H]) :- !, 
	report_message(system(0), 'Request for DEBUGGING'),
	debug(debug, H, null),  !,
	delete(H,debug,H2),
	indigo(E,H2).
indigo2(_,_,[halt_exec|H]) :- !, 
	report_message(system(0), 'Request for TERMINATION of the program'),
	indigo([], H).
indigo2(_,_,[abort_exec|H]) :- !, 
	report_message(system(0), 'Request for ABORTION of the program'),
	indigo([?(false)], H).
indigo2(_,E,[break_exec|H]) :- !, 
	report_message(system(0), 'Request for PAUSE of the program'),
	writeln(E),
	break,		% BREAK POINT (CTRL+D to continue execution)
	delete(H,pause,H2),
	indigo(E,H2).
indigo2(_,_,[reset_exec|_]) :- !, 
	report_message(system(0), 'Request for RESETING agent execution'),
	finalizeDB,
	initializeDB,
	proc(main, E),		% obtain main agent program
	indigo(E,[]).		% restart main with empty history
indigo2(H,E,[stop_interrupts|H]) :- !, 
	indigo(E,[stop_interrupts|H]).
indigo2(H,E,[A|H]) :- 
	indixeq(A, H, H1), 
	indigo(E, H1).  % DOMAIN ACTION

% This are special actions that if they are in the current history
% they are interpreted by the interpreter in a particular way
% This should be seen as meta-actions that deal with the interpreter itself
system_action(debug_exec).	% Special action to force debugging
system_action(halt_exec).	% Action to force clean termination
system_action(abort_exec).	% Action to force sudden nonclean termination
system_action(start_exec).	% Action to start execution
system_action(break_exec).	% Action to break the agent execution to top-level Prolog
system_action(reset_exec).	% Reset agent execution from scratch
	
% Wait continously until an exogenous action occurrs
doWaitForExog(H1,H2):- 	
        report_message(system(2), 'Waiting for exogenous action to happen'), 
        repeat, 
        handle_exog(H1,H2),
        (H2=H1 -> fail ; true).

% Predicates to prepare everthing for the computation of the next
% single step. Up to now, we just disable the GC to speed up the execution
prepare_for_step :- turn_off_gc.              % Before computing a step
wrap_up_step     :- retractall(doing_step),   % After computing a step
		    turn_on_gc, 
		    garbage_collect.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mayEvolve(E1,H1,E2,H2,S): perform transition from (E1,S1) to (E2,H2) with
%                        result S:
%
%                            trans = (E1,H1) performs a step to (E2,H2)
%                            final = (E1,H1) is a terminating configuration
%                            exog  = an exogenous actions occurred
%                            failed= (E1,H1) is a dead-end configuration
%                            system= system action transition
%
% There are two different implementations:
%
% * for Prolog's providing event handling (e.g., ECLIPSE, SWI)
% * any vanilla Prolog 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If the step is a system-action, then just propagate it
%mayEvolve(E1,[A|H1],E1,[A|H1], system):- type_action(A, system), !.

mayEvolve(E1,H1,E2,H2,S):- 
	type_prolog(T) -> mayEvolve(E1,H1,E2,H2,S,T) ; 
        		  mayEvolve(E1,H1,E2,H2,S,van).
abortStep :-  
	type_prolog(T) -> abortStep(T) ; abortStep(van).


%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) - for Prologs with ISO exception handling. (e.g., ECLIPSE and SWI)
%
% Notice that if a catch/3 is left via an throw/1 call, all current
% computations and bindings are lost (e.g., the bindings on E1,H1,S,E2,H2)
%
mayEvolve(E1,H1,E2,H2,S,T):- (T=ecl ; T=swi),
	catch(  (assert(doing_step),	% Assert flag doing_step
                 (exists_pending_exog_event -> abortStep(T) ; true),
                 (final(E1,H1,T)       -> S=final ;
                  trans(E1,H1,E2,H2,T) -> S=trans ;
                                          S=failed),
                 retract(doing_step)	% Retract flag doing_step
%                 (repeat, \+ pause_step)
                ), exog_action, (retractall(doing_step), S=exog) ).

% Abort mechanism for ECLIPSE: just throw exception
abortStep(ecl) :- throw(exog_action).  

% Abort mechanism for SWI: throw exception to main thread only
% 	OBS: abortStep(swi) is running in the env. manager thread
%		so by the time throw(exog_action) is executed, it could
%		be the case that thread main already retracted doing_step/0
%		from the DB and that mayEvolve/6 is already finished. In that
%		case the event should not be raised
abortStep(swi) :- thread_signal(main, (doing_step -> throw(exog_action) ; true)).

/* OBS: As it is, it is not working 100% because sometimes the execution
is aborted and the following message is written:
		ERROR: Unhandled exception: exog_action
		
		This happens because the "exog_action" event was rised
		outside the catch/3 clause!!!
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2) - for "vanilla" Prolog
%
mayEvolve(E1,H1,E2,H2,S,van):- 
	final(E1,H1)       -> S=final ;
	trans(E1,H1,E2,H2) -> S=trans ;  S=failed.

abortStep(van) :- true.  % No way of aborting a step in the vanilla version



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trans/5 and final/3 wrappers for the real trans/4 and final/3
%
% The last argument of trans/4 and final/3 is used to distinghuish
% trans and final under different plataforms: ECLPSE, SWI or vanilla Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ECLIPSE: execute trans/4 (final/2) and, then, grounds
% all remaining free variables using the provided fix_term/2
final(E,H,ecl)      :- mfinal(E,H), 
                       (fix_term((E,H)) -> true ; true).
trans(E,H,E1,H1,ecl):- mtrans(E,H,E1,H1), 
                       (fix_term((E1,H1)) -> true ; true).

% SWI: final/3 and trans/5 just reduce to final/2 and trans/4
final(E,H,swi)      :- mfinal(E,H).
trans(E,H,E1,H1,swi):- mtrans(E,H,E1,H1).

% vanilla Prolog: final/3 and trans/5 just reduce to final/2 and trans/4
final(E,H,van)      :- mfinal(E,H).
trans(E,H,E1,H1,van):- mtrans(E,H,E1,H1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  EXECUTION OF ACTIONS
%
%  indixeq(+Act,+H,-H2) is called when action Act should be executed at
%    history H. H2 is the new history after the execution of Act in H
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% type_action(Action, Type) : finds out the type of an action 
type_action(Act, sensing)    :- sensing(Act, _), !.
type_action(Act, system)     :- system_action(Act), !.
type_action(_, nonsensing).

indixeq(Act, H, H2) :-    % EXECUTION OF SYSTEM ACTIONS: just add it to history
        type_action(Act, system), !,
        H2 = [Act|H],
        update_now(H2).
indixeq(Act, H, H2) :-    % EXECUTION OF SENSING ACTIONS
        type_action(Act, sensing), !,
        report_message(system(1), ['Sending sensing Action *',Act,'* for execution']),
        execute_action(Act, H, sensing, IdAct, S), !,
	(S=failed -> 
		report_message(error, ['Action *', Act, '* FAILED to execute at history: ',H]),
		H2 = [abort,failed(Act)|H],	% Request abortion of program
	        update_now(H2)
	;
                report_message(action,  
                	['Action *', (Act, IdAct),'* EXECUTED SUCCESSFULLY with sensing outcome: ', S]),
	        wait_if_neccessary,
		handle_sensing(Act, [Act|H], S, H2),  % ADD SENSING OUTCOME!
		update_now(H2)
	).
indixeq(Act, H, H2) :-         % EXECUTION OF NON-SENSING ACTIONS
        type_action(Act, nonsensing), !, 
        report_message(system(1), ['Sending nonsensing action *',Act,'* for execution']),
        execute_action(Act, H, nonsensing, IdAct, S), !,
	(S=failed -> 
		report_message(error, ['Action *', Act, '* could not be executed at history: ',H]),
		H2 = [abort,failed(Act)|H],
	        update_now(H2)
	;
                report_message(action, ['Action *',(Act, IdAct),'* COMPLETED SUCCESSFULLY']),
		wait_if_neccessary,
                H2 = [Act|H],
		update_now(H2)
	).

% Simulated pause between execution of actions if requested by user
wait_if_neccessary :-
        wait_at_action(Sec), !,   % Wait Sec numbers of seconds
        report_message(system(2),['Waiting at step ',Sec,' seconds']), 
        sleep(Sec). 
wait_if_neccessary.

% Updates the current history to H
update_now(H):- 
        retract(now(_)) -> assert(now(H)) ; assert(now(H)).

action_failed(Action, H) :-
	report_message(error,['Action *', Action, '* could not be executed',
	                      ' at history: ',H]),
	halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  EXOGENOUS ACTIONS
%
%  Exogenous actions are stored in the local predicate indi_exog(Act)
%  until they are ready to be incorporated into the history
% History H2 is H1 with all pending exog actions placed at the front
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_exog(H1, H2) :- 
	save_exog,				% Collect on-demand exogenous actions
	exists_pending_exog_event,		% Any indi_exog/1 in the database?
		% 1 - Collect SYSTEM exogenous actions (e.g., debug)
	findall(A, (indi_exog(A), type_action(A, system)), LSysExog),
		% 2 - Collect NON-SYSTEM exogenous actions (e.g., domain actions)  
	findall(A, (indi_exog(A), \+ type_action(A, system)), LNormal),	
		% 3 - Append the lists to the current hitory (system list on front)
	append(LSysExog, LNormal, LTotal),
	append(LTotal, H1, H2), 
	update_now(H2),
		% 4 - Remove all indi_exog/1 clauses
	retractall(indi_exog(_)).
handle_exog(H1, H1). 	% No exogenous actions, keep same history


% Collect on-demand exogenous actions: reported  by exog_occurs/1 
save_exog :- exog_occurs(L) -> store_exog(L) ; true.

store_exog([]).  
store_exog([A|L]) :- assertz(indi_exog(A)), store_exog(L).

% Is there any pending exogenous event?
exists_pending_exog_event :- indi_exog(_).


% exog_action_occurred(L) : called to report the occurrence of a list L of 
% 				exogenous actions (called from env. manager)
% 
% First we add each exogenous event to the clause indi_exog/1 and
% in the end, if we are performing an evolution step, we abort the step.
exog_action_occurred([]) :- doing_step -> abortStep ; true.
exog_action_occurred([ExoAction|LExoAction]) :-
        assert(indi_exog(ExoAction)),   
        report_message(exogaction, ['Exog. Action *',ExoAction,'* occurred']),
	exog_action_occurred(LExoAction).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLING OF ROLLING FORWARD
%
% handle_rolling/2: mandatory rolling forward
% pause_or_roll/2: optional rolling forward
%
% Based on the following tools provided by the evaluator used:
%
%	must_roll(H): we MUST roll at H
%	can_roll(H) : we COULD roll at H (if there is time)
%	roll_db(H1,H2): roll from H1 to H2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_rolling(H1,H2) :- must_roll(H1), !, roll(H1, H2).
handle_rolling(H1,H1).

pause_or_roll(H1,H2) :- can_roll(H1), !, roll(H1, H2).
pause_or_roll(H1,H1).

roll(H1, H2) :-
        report_message(system(0),'Rolling down the river (progressing the database).......'), 
	roll_db(H1, H2), 
        report_message(system(0), 'done progressing the database!'), 
        report_message(system(3), ['New History: ', H2]), 
	update_now(H2), 			% Update the current history	
	retract(rollednow(HO)),			% Update the rollednow/1 predicate
	append(H1,HO,HN),			% rollednow(H): H is the full system history
	assert(rollednow(HN)),
	save_exog.	% Collect all exogenous actions

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  OTHER PREDICATES PROVIDED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% H is a past situation w.r.t. the actual situation (stored in clause now/1)
pasthist(H):- now(ActualH), before(H,ActualH).

% Deal with an unknown configuration (P,H)
error(M):- 
        report_message(error, M), 
        report_message(error,'Execution will be aborted!'), abort.

warn(M):- 
        report_message(warning, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/indigolog.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%