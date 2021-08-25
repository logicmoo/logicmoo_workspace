%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: CLIMA/main_swi.pl
%
%  AUTHOR	: Sebastian Sardina
%  email	: ssardina@cs.toronto.edu
%  WWW		: www.cs.toronto.edu/cogrobo
%  TYPE CODE	: system dependent predicates
%  TESTED	: SWI Prolog 5.6.24 under FC6 
%
% IndiGolog agent player for CLIMA-07
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
% -- main: Collects all the procedures named 'mainControl(N)' where
%          N is the number representing the N-th controller.
%          The user can select which controller to execute and the 
%          IndiGolog executor will be run on such controller
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include('../../lib/systemvar').  % Global include code and Prolog init
:- consult('../../lib/alpha_star'). % Alpha* path finding
%:- use_module(library(chr)).
%:- reset_backquoted_string.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult the IndiGolog system: top-level and evaluator
:- consult('../../Interpreters/indigolog').     % IndiGolog interpreter 
% :- consult('../../Eval/eval_know').               % LP evaluator
:- consult('../../Eval/evalbat').               % LP evaluator

% 2 - Consult environment manager 
:- consult(['../../Env/env_man.pl']).         % Load environment manager

% 3 - Consult application
:- consult(agent_bat).                          % Application code in IndiGolog


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	clima_agentID/2, 
	teammember/1, 
	mess_location/2,
	clima_location/2.


% This is the address of the CLIMA GAME server environment
clima_location('tea.dyndns.org', 12300).
%clima_location(teahp, 12300).
%clima_location(localhost, 12300).
%clima_location('agentmaster.in.tu-clausthal.de', 12300).

clima_agentID(participant1,1).	% default

% set the agent ID and PASSWORD and the corresponding teammates
set_agentID(AgentId,PassId) :-
	retractall(clima_agentID(_,_)),
	assert(clima_agentID(AgentId, PassId)).


% This is the address and information for the MESSENGER environment
mess_location('tea.dyndns.org', 12340).
%mess_location('eon.cs.toronto.edu', 12340).
%mess_location('teahp, 12340).
%mess_location(localhost, 12340).
agentID(Id) :- clima_agentID(Id,_).
teammember(participant1).	% Default team-members
teammember(participant2).
teammember(participant3).
teammember(participant4).
teammember(participant5).
teammember(participant6).

% set the team
set_team(ListPlayers) :- 
	retractall(teammember(_)), 
	member(Player, ListPlayers),
	assert(teammember(Player)),
	fail.
set_team(_).


% Port of environment manager has to be fixed in SWI
server_port(_).
%server_host('127.0.0.1').
server_host(localhost).


% Define what environment managers the application will use
:- ['../../Env/dev_managers'].              % Common facts (device_manager/4)
load_device(Env, Command, Address) :- 
	(clima_agentID(boss,_) ->	
	        member((Env,Type), [(messenger([quiet,debug(0)]), swi)])
	;
        	member((Env,Type), 
			[(clima07([debug(3)]), swi),(messenger([quiet,debug(0)]), swi)])
	),
        %member((Env,Type), [(clima07([debug(5)]), swi),(messenger([]), swi)]),
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, Type, Command, [Host, Port]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, messenger(_), Action) :-
	member(Action, [tell(_,_), broadcast(_)]), !.
how_to_execute(Action, clima07(_), Action) :-
	clima_action(Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%          translateExogAction(Code, Action)
%          translateSensing(Action, Outcome, Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :- 
	actionNum(Action, CodeAction).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% golog team
golog1 :- set_golog_team, login(1,UserName, Pass), main(UserName,Pass).
golog2 :- set_golog_team, login(2,UserName, Pass), main(UserName,Pass).
golog3 :- set_golog_team, login(3,UserName, Pass), main(UserName,Pass).
golog4 :- set_golog_team, login(4,UserName, Pass), main(UserName,Pass).
golog5 :- set_golog_team, login(5,UserName, Pass), main(UserName,Pass).
golog6 :- set_golog_team, login(6,UserName, Pass), main(UserName,Pass).
boss :- set_golog_team, login(7,UserName, Pass), main(UserName,Pass).




% for the test server
%login(1,'GOLOGteam1',va5Liove).
%login(2,'GOLOGteam2','Aerai6Pa').
%login(3,'GOLOGteam3','Efool1lu').
%login(4,'GOLOGteam4',cahk6Oi7).
%login(5,'GOLOGteam5',iuY1soj5).
%login(6,'GOLOGteam6',deiZak5f).

% for the contest server
login(1,'GOLOGteam1',lH9fsomB).
login(2,'GOLOGteam2',gEvak2OS).
login(3,'GOLOGteam3',nv1w2hTh).
login(4,'GOLOGteam4','4qRgWlgr').
login(5,'GOLOGteam5',j6J2iyYI).
login(6,'GOLOGteam6',z54IqyLb).
login(7,boss,null).

set_golog_team :- 
	set_team(['GOLOGteam1','GOLOGteam2','GOLOGteam3','GOLOGteam4','GOLOGteam5','GOLOGteam6',boss]).




% set an agent player and go!
main(AgentId, PassId) :-
	set_agentID(AgentId,PassId), !,
	indigolog.	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_option(debug_level,0).
:- set_option(wait_step,0).
:- set_option(debug_level,warn_off).


run_firefox :-
        (    fork(child),
             exec(xterm)
        ;    true
        ).



end_of_file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: CLIMA/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-initializeEM.
thread_create(C):-thread_create(C,_,[alias(C)]).

/*:- thread_create(golog1). 
:- thread_create(golog2). 
:- thread_create(golog3). 
:- thread_create(golog4). 
:- thread_create(golog5). 
:- thread_create(golog6). 
:- boss.
*/

%% :-golog1.
