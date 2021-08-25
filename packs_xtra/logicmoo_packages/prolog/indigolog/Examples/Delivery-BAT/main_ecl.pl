%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Elevator/main_ecl.pl
%
%  Author    : Sebastian Sardina
%  Time-stamp: <03/03/31 20:14:37 ssardina>
%  email     : ssardina@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/~ssardina
%  TESTED    : ECLiPSe 5.5 on RedHat Linux 6.2-8.0
%  TYPE CODE : system independent predicates
%
% DESCRIPTION: This file is the main file of an IndiGolog application
%              delivery controller with conditional search and planning 
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
% -- main: Collects all the procedures named 'mainControl(N)' where
%          N is the number representing the N-th controller.
%          The user can select which controller to execute and the 
%          IndiGolog executor will be run on such controller
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_flag(debug_compile, off).   
:- set_flag(variable_names, off).  % To speed up execution

type_prolog(ecl).    % Type of Prolog being used

% Path is the root path of the IndiGolog system
main_dir(Path):- getenv('PATH_INDIGOLOG',Pwd),
                 (string(Pwd) -> atom_string(APwd, Pwd) ; APwd=Pwd),
                 concat_atom([APwd, '/'], Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- lib(scattered).   % Clauses can be not consecutive
:- lib(fd).          % Load finite-domain constraint library
:- lib(fd_search).   % Load extra finite-domain search algorithms

:- use_module(library(tools_ecl)).

:- ['../../lib/systemvar'].              % Common facts (device_manager/4)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult IndiGolog system
:- ['../../Interpreters/indigolog'].     % IndiGolog interpreter
:- ['../../Eval/evalbat'].               % BAT evaluator

% 2 - Consult environment manager 
:- ['../../Env/env_man.pl'].         % Load environment manager

% 4 - Consult application
:- [delivery].                 % Golog elevator controller


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Port of environment manager is left free and assigned by the OS
server_port(X).

% Load simulator, RCX and internet environments
load_environment(Env, Command, Address) :- 
        member(Env, [simulator]),
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, eclipse, Command, [Host, Port]).
        
        
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           % HOW TO EXECUTE ACTIONS: Environment + low-level Code %
           %        how_to_execute(Action, Environment, Code)     %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, simulator, Action). % Everything else


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION   %
           %          translateExogAction(Code, Action)           %
           %          translateSensing(Action, Outcome, Value)    %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :- actionNum(Action, CodeAction).

translateSensing(senseDoor(_), SensorValue, true):- SensorValue<30, !.
translateSensing(senseDoor(_), SensorValue, true):- SensorValue>40, !.
translateSensing(senseDoor(_), _, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main: Gets IndiGolog to evaluate main control procedure
main:- bagof(X,Y^proc(mainControl(X),Y),L),
    (L=[NoContr] -> 
         indigolog(mainControl(NoContr)) 
    ;
         write('Available Controllers: '), write(L), nl,
         write('Which controller do you want to execute? '), 
         read(NoContr), 
         indigolog(mainControl(NoContr)) 
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Elevator/main_ecl.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
