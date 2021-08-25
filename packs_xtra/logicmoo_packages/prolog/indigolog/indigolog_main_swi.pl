%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Wumpus/main_swi.pl
%
%  AUTHOR    : Stavros Vassos & Sebastian Sardina
%  email     : {stavros,ssardina}@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/cogrobo
%  TESTED    : SWI Prolog 4.0.5 under RedHat Linux 6.2/7.1 
%  TYPE CODE : system dependent predicates
%
% DESCRIPTION: This file is the main file of an IndiGolog application
%              of the Wumpus World.
%
% Written for SWI Prolog http://www.swi-prolog.org/) running under Linux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This is the top-level file for a Legolog application program.
% It consults all the necessary Legolog prolog files.
% In particular, the following is loaded:
%
%  (1) Load all libraries required. This includes the system dependant
%      ones for the specific Prolog plus general libraries
%  (2) Load the IndiGolog interpreter and the projector used
%  (3) Load the application code itself containing the background theory
%      of action plus the high-level program
%  (4) Specify which environments should be loaded and how 
%  (5) Specify how each action should be executed and how to translate
%      exogenous actions
%
% Moreover, the following is provided:
%
% -- main: Collects all the procedures named 'mainControl(id)' 
%	   and asks the user which one to run. Uses controller/1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
:- dynamic controller/1.	% Stores the user decision on the controller to run

:- ensure_loaded('lib/logicmoo_workarounds').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded('lib/systemvar'). % Global ensure_loaded code and Prolog init
:- ensure_loaded('lib/alpha_star'). % Alpha* path finding
%:- use_module(library(chr)).
%:- reset_backquoted_string.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult the IndiGolog system: top-level and evaluator
:- ensure_loaded('Interpreters/indigolog').     % IndiGolog interpreter 
:- ensure_loaded('Eval/eval_know').               % LP evaluator

% 2 - Consult environment manager 
:- ensure_loaded(['Env/env_man.pl']).         % Load environment manager

% 3 - Consult application
:- ensure_loaded(wumpus).                          % Application code in IndiGolog




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic wumpus_config/5.

% In this example the environment manager host:port is fixed
server_host('localhost').
server_port(_).

wumpus_config(indigolog(default),8,10,1,random).  % Default conf: 
%wumpus_config(indigolog(default),8,10,1,indigolog(default)). 
%wumpus_config(indigolog(default),8,10,1,none).  
%wumpus_config(rerun(82),8,10,1,nmar05test(82)).  % Default conf: 

% Load simulator, RCX and internet environments
:- ['Env/dev_managers'].              % Common facts (device_manager/4)
load_device(Env, Command, Address) :- 
       %  member((Env,Type), [(virtual_wumpus_silent, swi)]),
         member((Env,Type), [(virtual_wumpus, swi)]),  
       %  member((Env,Type), [(virtual_wumpus_vworld, swi)]),  
       % use virtual_wumpus to get the xterm console
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, Type, Command, [Host, Port]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code 
%        how_to_execute(Action, Environment, Code)     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, virtual_wumpus, Action).
%how_to_execute(Action, virtual_wumpus_silent, Action).
%how_to_execute(Action, virtual_wumpus_vworld, Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION   
%          translateExogAction(Code, Action)           
%          translateSensing(Action, Outcome, Value)    
% OBS: If not present, then the translation is 1-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :-  actionNum(Action, CodeAction).
%translateSensing(_, SensorValue, SensorValue).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets IndiGolog to evaluate a chosen mainControl procedure
main1:- retractall(controller(_)),
	bagof(X,Y^proc(mainControl(X),Y),L),
    	(L=[NoContr] -> 
		assert(controller(NoContr))
    	;
        	write('Available Controllers: '), write(L), nl,
         	write('Which controller do you want to execute? '), 
        	read(NoContr), 
	 	assert(controller(NoContr))
    	),
	indigolog.


main4:- retractall(controller(_)),
	bagof(X,Y^proc(mainControl(X),Y),L),
    	(L=[NoContr] -> 
		assert(controller(NoContr))
    	;
        	write('Available Controllers: '), write(L), nl,
         	write('Which controller do you want to execute? '), 
        	NoContr = 4, 
	 	assert(controller(NoContr))
    	),
	indigolog.



full_test :-
	member(Size,[8]),
	member(PPits,[10,15,20,30,40]),
	member(NoGolds,[1,2,4]),
	writeln('=================================================='),
	write('TEST WUMPUS: '), write((Size,PPits,NoGolds)), nl,
	once(retract(wumpus_config(_,_,_,_,_))),
	once(retract(gridsize(_))),
	assert(gridsize(Size)),
		% Set up Size, PPits and NoGolds (only)
	assert(wumpus_config(test,Size,PPits,NoGolds,scenario)),
	test(301,1),
	fail.
full_test :- 
	writeln('=================================================='),
	writeln('DONE!').


% test Wumpus Max number of times repetitively
test(Max,Max) :-!, 
	writeln('FINISHED TESTING....').
test(Max,N) :-
	write('TESTING INSTANCE: '), write(N),
	write(' (Out of '), write(Max), write(' runs)'), nl,
	tell('/dev/null'),
	retract(wumpus_config(_,Size,PPits,NoGolds,_)),
		% Assert the type of execution it is going to be done next!
%	assert(wumpus_config(evalnocut(N),Size,PPits,NoGolds,testnewholds(N))),
	assert(wumpus_config(nmar05test(N),Size,PPits,NoGolds,random)),
	indigolog(mainControl(4)),
	sleep(1),
	tell(user),
	N2 is N+1,
	test(Max, N2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_option(debug_level,4).
:- set_option(wait_step,0).
%:- set_option(debug_level,warn_off).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Wumpus/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-main.

