%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	ALERT!! ECLIPSE IS NOT SUPPORTED ANYMORE AS AN INDIGOLOG INTERPRETER!
%
% FILE: Elevator/main_ecl.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code
%  TESTED : ECLiPSe 5.5 on RedHat Linux 6.2-8.0
%
% DESCRIPTION: This file is the main file for controlling the ER1 robot
%
% Written for ECLiPSe Prolog (http://www.icparc.ic.ac.uk/eclipse/)
% running under Linux
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic controller/1.	% Stores the user decision on the controller to run
:- set_flag(debug_compile, off).   
:- set_flag(variable_names, off).  % To speed up execution


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- lib(scattered).   % Clauses can be not consecutive
:- lib(fd).          % Load finite-domain constraint library
:- lib(fd_search).   % Load extra finite-domain search algorithms

:- use_module(library(tools_ecl)).       % General tools for Eclipse
:- ['../../lib/systemvar'].              % Common facts (device_manager/4)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult top-level IndiGolog system
:- ['../../Interpreters/indigolog'].     

% 2 - Consult environment manager 
:- ['../../Env/env_man.pl'].   

% 3 - Consult projector
:- ['../../Eval/evalbat'].     

% 4 - Consult application
:- [er1test].                     % IndiGolog ER1 example
:- ['../../lib/er1-actions.pl'].  % Action translation for ER1


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Port of environment manager is left free and assigned by the OS
server_port(X).

% Load simulator and ER1 environments
:- ['../../Env/dev_managers'].              % Common facts (device_manager/4)
load_device(Env, Command, Address) :- 
%        member((Env,Type), [(er1,eclipse), (simulator,eclipse)]),
        member((Env,Type), [(simulator,eclipse)]),
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

how_to_execute(Action, er1, Code) :- 
        actionNum(Action, Code), !.   

           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION   %
           %          translateExogAction(Code, Action)           %
           %          translateSensing(Action, Outcome, Value)    %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :- 
        actionNum(Action, CodeAction).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets IndiGolog to evaluate a chosen mainControl procedure
main:- 	retractall(controller(_)),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_option(debug_level,2).
:- set_option(wait_step,3).
:- set_option(type_em,signal).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Elevator/main_ecl.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
