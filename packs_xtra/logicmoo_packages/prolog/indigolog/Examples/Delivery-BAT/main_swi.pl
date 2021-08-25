%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Delivery-BAT/main_swi.pl
%
%  Author    : Sebastian Sardina
%  Time-stamp: <04/01/05 12:59:20 ssardina>
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
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             March 20, 2001
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This is the top-level file for a Legolog program.
% It consults the necessary Legolog prolog files and
% defines any additional system-dependent predicates that are required.
%
% For this example this file defines the following predicates:
% -- initializeExog: perform any initialization of other sources of
%      exogenous actions that is required
% -- finalizeExog: things to do for other sources of exogenous actions
%      at end of program
% -- checkOtherExog(-ExogList): check whether a request has been
%      entered via keyboard
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include('../../lib/systemvar'). % Global include code and Prolog init
%:- reset_backquoted_string.

:- use_module(library(libgraph)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult IndiGolog system
:- consult('../../Interpreters/indigolog').     % IndiGolog interpreter 
:- consult('../../Eval/evalbat').               % BAT evaluator

% 2 - Consult environment manager 
:- consult(['../../Env/env_man.pl']).         % Load environment manager

% 3 - Consult application
:- consult(delivery).                         % Application code in IndiGolog


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Port of environment manager has to be fixed in SWI
server_port(9113).

% Load simulator, RCX and internet environments
:- ['../../Env/dev_managers'].              % Common facts (device_manager/4)
load_environment(Env, Command, Address) :- 
%        member((Env,Type), [(simulator,eclipse), (rcx,eclipse), (internet,swi)]),
        member((Env,Type), [(simulator,eclipse), (rcx,eclipse)]),
%        member((Env,Type), [(simulator,eclipse)]),
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, Type, Command, [Host, Port]).


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           % HOW TO EXECUTE ACTIONS: Environment + low-level Code %
           %        how_to_execute(Action, Environment, Code)     %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%how_to_execute(Action, internet, Code) :- 
%        actionNum(Action, Code),
%        member(Action, [say(_)]), !.   
how_to_execute(Action, rcx, Code) :- 
        actionNum(Action, Code),
        \+ member(Action, [thermo]), !.       % Exceptions 


           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION   %
           %          translateExogAction(Code, Action)           %
           %          translateSensing(Action, Outcome, Value)    %
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translateExogAction(CodeAction, Action) :- actionNum(Action, CodeAction).

%translateSensing(check(_,_), SensorValue, true):- 
%	number(SensorValue),
%	(SensorValue<30 ; SensorValue>40), !.
%translateSensing(check(_,_), N, false) :- number(N).


        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%main: Gets IndiGolog to evaluate main control procedure
main:- bagof(X,Y^proc(mainControl(X),Y),L),
    (L=[NoContr] -> 
         indigolog(mainControl(NoContr)) 
    ;
         write('Available Controllers: '), write(L), nl,
         write('Which controller do you want to execute? '), 
         read(NoContr), 
         indigolog(mainControl(NoContr)) 
    ).

:- set_option(debug_level,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Delivery-BAT/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%









