%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE: Wumpus/main_ecl.pl
%
%  AUTHOR    : Stavros Vassos & Sebastian Sardina
%  email     : {stavros,ssardina}@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/cogrobo
%  TESTED    : ECLIPSE PROLOG
%  TYPE CODE : system dependent predicates
%
% DESCRIPTION: This file is the main file of an IndiGolog application
%              of the Wumpus World.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
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
% -- main: Collects all the procedures named 'mainControl(N)' where
%          N is the number representing the N-th controller.
%          The user can select which controller to execute and the 
%          IndiGolog executor will be run on such controller
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* ECL
:- set_flag(debug_compile, off).   
:- set_flag(variable_names, off).  % To speed up execution
type_prolog(ecl).    % Type of Prolog being used
*/

:- ensure_loaded('../../lib/logicmoo_workarounds').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* ECL
:- lib(scattered).   % Clauses can be not consecutive
:- lib(fd).          % Load finite-domain constraint library
:- lib(fd_search).   % Load extra finite-domain search algorithms
%:- lib(lists).	     % Used for shuffle/2

:- use_module(library(tools_ecl)).       % General tools for Eclipse
*/

:- ['../../lib/systemvar'].              % Common facts (device_manager/4)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult the IndiGolog system: top-level and evaluator
:- ['../../Interpreters/indigolog'].

:- ['../../Interpreters/flux/flux'].

% 2 - Consult environment manager 
:- ['../../Env/env_man.pl'].

% 3 - Consult projector

% 4 - Consult application
:- ['wumpus'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic wumpus_config/5.

% Port of environment manager has to be fixed in SWI
server_port(_).
server_host('localhost').

wumpus_config(flux(default),8,10,1,random).  % Default conf: 

% 8x8 size, 10/100 prob of pit and 4 golds in grid
% This requires a file wumpustestbed.pl with all the conf info
%wumpus_config(rerun(82),8,10,1,nmar05test(9)).  

% Load simulator, RCX and internet environments
:- ['../../Env/dev_managers'].              % Common facts (device_manager/4)
load_device(Env, Command, Address) :- 
%       member((Env,Type), [(virtual_wumpus_silent, swi)]),
       member((Env,Type), [(virtual_wumpus, swi)]),
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, Type, Command, [Host, Port]).

           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           % HOW TO EXECUTE ACTIONS: Environment + low-level Code %
           %        how_to_execute(Action, Environment, Code)     %
           %        						  %
	   % Anything else is executed in the simulator           %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%how_to_execute(Action, virtual_wumpus_silent, Action).
how_to_execute(Action, virtual_wumpus, Action).

           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION   %
           %          translateExogAction(Code, Action)           %
           %          translateSensing(Action, Outcome, Value)    %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(Action, Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main:- 	init, 
	writeln('Starting to execute WUMPUS with FLUX'),
	(main_wumpus -> true ; true), 
	writeln('Finishing to execute WUMPUS with FLUX'),
	fin.
initializeDB.
finalizeDB.


full_test :-
	member(Size,[8]),
	member(PPits,[20,30,40]),
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
	assert(wumpus_config(nmar05testFlux(N),Size,PPits,NoGolds,nmar05test(N))),
%	assert(wumpus_config(fluxtest(N),Size,PPits,NoGolds,random)),
	main,	% RUN FLUX PROGRAM!
	sleep(1),
	tell(user),
	N2 is N+1,
	test(Max, N2).	
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_option(debug_level,2).
:- set_option(wait_step,0).
:- set_option(type_em,signal).
%:- set_option(type_em,eventafter).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTION OF ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- dynamic now/1.
perform(A, S) :-
	now(H),
	perform(A,H,CS),	% CS is a list of sensing codes (0/1)
	maplist(translateSensing,CS,S),
	update_now([o(A,S)|H]).

perform(sense, H, [S2,S1,S3]) :-
	execute_action(smell, H, S1), S1\=failed,
	execute_action(senseBreeze, H, S2), S2\=failed,
	execute_action(senseGold, H, S3), S3\=failed.
perform(turn, H, []) :-
	execute_action(turn, H, S), S\=failed.
perform(enter, H, S2) :-
	execute_action(enter, H, S), S\=failed,
	perform(sense, H, S2).
perform(exit, H, []) :-
	execute_action(climb, H, S), S\=failed.
perform(shoot, H, [S]) :-
	execute_action(shoot, H, S), S\=failed.
perform(go, H, S2) :-
	execute_action(moveFwd, H, S), S\=failed,
	perform(sense, H, S2).
perform(grab, H, []) :-
	execute_action(pickGold, H, S), S\=failed.

translateSensing(0, false).
translateSensing(1, true).

% interface to the execute_action/5 in the EM (env_man.pl)
execute_action(Action, H, Outcome) :-
	execute_action(Action, H, _, _, Outcome).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Wumpus/main_ecl.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
