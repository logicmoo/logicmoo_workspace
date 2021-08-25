%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       World Smallest Iterative Deeping Forward Filtering 
%		Conditional Planner		
%		(Version with constraints for ECLIPSE Prolog) 
%       Tested with ECLIPSE 5.3 and SWI Prolog over Linux RH 7.0-7.2
%
%	c) 	Hector J. Levesque      		Many rights reserved	(Nov 2001)
% 		Modified by Sebastian Sardina    Many rights reserved	(Jan 2002)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file provides the following:
% 
%  --- wscp(Name,Goal,Max,IA,SimNo,S,Plan):  Plan for Goal up to depth Max
%      wscp(Name,Goal,Max,S,Plan)
%            Name : symbolic name of the planning problem
%            Goal : goal 
%            Max  : max depth to search for 
%            IA   : initial set of actions to use
%            SimNo: Use SimNo simulator controller for exogenous actions
%            S    : initial history/situation
%            Plan : plan
%
%  --- pplan(Name,Goal,Max,End): 
%      pplan(Goal,Max) [Name=Goal and End=true]
%      pplan(Name,Goal,Max) [End=true]
%           plan for Goal up to Max depth. At the end print
%              the plan using End as the final condition to print out
%
%  --- run(CP,H,H2): H2 is a possible extension of H by executing CP
%
% The following predicates are required:
%  --- prim_action(action) - for each primitive action
%  --- poss(action,cond)   - when cond holds, action is executable
%  --- sensing(A,VL) - VL is a list of possible sensing values of action A
%
%  --- simulator(Id,C,A) : Under simulator Id, exog action A must happens if C holds (optional)
%  --- inconsistent(H) - last action make history H inconsistent
%  --- restrict_actions(Name,Goal,N,AA,C,NA) - 
%                 In the planning Name, when C holds, restrict to 
%                 actions in NA when planning for Goal at the Nth level 
%                 with initial set of actions AA
%       To achieve no Filtering: restrict_actions(name,_,_,_,false,_). 
%                       
%  --- eval(P,H,B):  B is the truth value of P at history H (MAIN PREDICATE)
%  --- handle_sensing(A,H,Sr,H2): alter the history H to encode the sensing 
%                                 result of action A at H
%  --- fix_term(A): fix all of some of the variables in A (optional)
%					used with theories with constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic simulator/3,          % These predicates may be not defined
           fix_term/1,
           restrict_actions/6. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The planner (extended)
% Differences with the original version:
%    1) change good_state/3 for the more general restrict_actions/5
%    2) simulate exogenous action after each step using user provided
%       exogenous action simulator via simulator/3
%    3) sensing handled more general by using handle_sensing/3 depending
%       on what theory of action is used
%    4) inconsistent situations is now handle via type-theory inconsistent/1 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	wscp(+Name,+Goal,+Max,+LAct,+InitState,-Plan) 
%
% Top-level planner predicate wscp/5 wscp/7
%	--- Name id of the planning process type (used for forward filtering)
%  	--- Goal is the condition to planfor
%  	--- Max is the maximum level allowed 
%  	--- InitState is the initial history we start on
%  	--- Plan is the computed (output) plan
% 	---	LOptions  is the list of options:
% 			- simid(Id) : Id of the exog. simulator to be used (none = no simulator)
% 			- mess(Mess) : description of the planning work to be done (to be printed)
%			- actions(LAct) : legal actions that mayb be used  
%
% wscp/5 Assumes no exogenous action simulator and all actions allowed
% wscp/7 Uses the exogenous action simulator with id SimId  

wscp(Name,Goal,Max,InitState,Plan,LOptions) :-	% THE MAIN RULE
	extract_option(LOptions,simid,SimId,none), 
	extract_option(LOptions,actions,LAct,_AnyLAct), 
	idplan(Name,Goal,0,Max,InitState,Plan,LAct,SimId).


% Iterative deeping planner (on top of depth-search dfplan/7)
idplan(Name,Goal,N,_Max,InitState,Plan,LAct,SimId) :- 
	dfplan(Name,Goal,N,InitState,Plan,LAct,SimId).
idplan(Name,Goal,N,Max,InitState,Plan,LAct,SimId) :- 
	N < Max, N1 is N+1, 
	idplan(Name,Goal,N1,Max,InitState,Plan,LAct,SimId).


% Depth-first planner for Goal, up to depth level N
% Simulated actions are "added" to both the situation S and the Plan
% Usually, simulated actions will be stated via the sim(_) construct
dfplan(_Name,Goal,_N,S,[],_LAct,_SimId) :- holds_wscp(Goal,S). 
dfplan(Name,Goal,N,S,Plan,LAct,SimId) :- N > 0, 
    filter(Name,Goal,N,LAct,S,LAct2), LAct2\=[], % Actions forward filtering
    simulate_exog(S,SimId,SE), 
    append(SE,S,S2),	% Add simulated actions to the current situation 
    append(SE,Plan2,Plan),	% Add simulated actions to plan
    prim_action(A), allowed(A,LAct2),	% Pick an allowed action 
    poss(A,C2), holds_wscp(C2,S2), 
    (fix_term(A) -> true ; true),    % Ground the action A if possible
    N1 is N-1, 
    try_action(Name,Goal,N1,S2,A,Plan2,LAct,SimId).
  
% Try sensing action A at level N
try_action(Name,Goal,N,S,A,[A,case(A,BL)],AA,SimNo) :- 
               sensing(A,VL), !, build_ifs(Name,Goal,N,S,A,VL,BL,AA,SimNo).
% Try non-sensing action A at level N
try_action(Name,Goal,N,S,A,[A|RPlan],AA,SimNo)      :- 
               dfplan(Name,Goal,N,[A|S],RPlan,AA,SimNo).

% Build case structure using the list [V|VL] of sensing results for A
build_ifs(_,_,_,_,_,[],[],_,_).
build_ifs(Name,Goal,N,S,A,[V|VL],[if(V,Plan)|BL],AA,SimNo) :-
	handle_sensing(A,[A|S],V,S2),
	( inconsistent(S2) -> Plan=[inc] ; 
                              once(dfplan(Name,Goal,N,S2,Plan,AA,SimNo)) ),
	build_ifs(Name,Goal,N,S,A,VL,BL,AA,SimNo).

% Perform the forward filtering. LAct3 is the new set of possible actions
filter(Name,Goal,N,LAct,S,LAct3):- 
	restrict_actions(Name,Goal,N,LAct,C1,LAct2), !,
	(holds_wscp(C1,S) -> LAct3=LAct2 ; LAct3=LAct2).
filter(_,_,_,LAct,_,LAct).        % No forward filtering


%%	allowed(+A,+LAct): Action A is an allowed action in list LAct
allowed(A,AA):- \+ \+ member(A,AA), !.

%%	simulate_exog(+S,+SimId,-S2):-
% 
% 	Given situation S, and exog. simulator with id SimId, S2 will
%	be the next situation with all simulated exogenous actions included
simulate_exog(S,SimId,[A|SE]):- 
	ground(SimId), 
	SimId\=none,
	simulator(SimId,C,A), holds_wscp(C,S), !,
	simulate_exog([A|S],SimId,SE).
simulate_exog(_,_,[]).	% no action simulated


% WSCP only considers true projection. Ignore false or unknowns.
holds_wscp(C,H):- eval(C,H,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Plan & Print plan (Name is the name of the planning problem)
% pplan(Name,Goal,Max)    : plan for Goal up to Max depth
% pplan(Name,Goal,Max,End): plan for Goal up to Max depth. At the end print
%                   the plan using End as the final condition to print out
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pplan(Goal,Max)      :- pplan(Goal, Goal,Max,true).  % Make Name=Goal
pplan(Name,Goal,Max) :- pplan(Name,Goal,Max,true).
pplan(Name,Goal,Max,End) :- wscp(Name,Goal,Max,[],Plan), nl, 
	write('Planning name is '), write(Name), nl,
	write('Goal is '), write(Goal), nl, nl, pp(0,Plan,[],End), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  pp(I,Plan,S,E): Pretty print of a plan Pt
%         - I is the initial indentation
%         - Plan is the plan to print
%         - S is the initial situation
%         - E is the final condition to print out
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pp(_,[],_,true) :- !.
pp(N,[],S,End) :- subf(End,P,S), tab(N), write('*** '), call(P), nl.
pp(N,[case(A,L)],S,End) :- !, tab(N), 
	write(A), nl, N2 is N+1, pp2(N2,L,A,S,End).
pp(N,[A|L],S,End) :- !, tab(N), write(A), nl, pp(N,L,[A|S],End).

pp2(_,[],_,_,_).
pp2(N,[if(V,P)|L],A,S,End) :- tab(N), write(V), write(' => '), nl, N2 is N+1, 
	pp(N2,P,[e(_,V)|S],End), pp2(N,L,A,S,End).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  run/3 extracts each potential history-path in a conditional plan 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% H is a potential history after executing the CPP 
% (i.e, H is a history after executing "some" branch in CPP)
run(CPP,H):- run(CPP,[],H).
run([],H,H).
run([A,case(A,BL)],H,H3):-!, member(if(V,PV),BL), 
                          handle_sensing(A,[A|H],V,H2), 
			  run(PV,H2,H3).
run([A|R],H,H2):- run(R,[A|H],H2).

% FL is the length (i.e, number of actions) of history H
% FL is the real length minus the number of sensing results in H
hist_length(H,N):- findall(A, (member(A,H), prim_action(A)),LA),
	           length(LA,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/wscplan.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%