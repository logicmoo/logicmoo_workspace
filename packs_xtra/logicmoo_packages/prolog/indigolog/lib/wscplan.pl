%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       World Smallest Iterative Deeping Forward Filtering 
%		Conditional Planner		
%		(Version with constraints for ECLIPSE Prolog) 
%       Tested with ECLIPSE 5.3 and SWI Prolog over Linux RH 7.0-7.2
%
%	c) Hector J. Levesque      Many rights reserved		(Nov 2001)
% Modified by Sebastian Sardina    Many rights reserved		(Jan 2002)
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
%  --- simulator(N,P) - P is the N exogenous action simulator (optional)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic simulator/3,          % These predicates may be not defined
           fix_term/1,
           restrict_actions/6. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The planner (extended)
% Differences with the original version:
%    1) change good_state/3 for the more general restrict_actions/5
%    2) simulate exogenous action after each step using user provided
%       exogenous action simulator via simulator/2
%    3) sensing handlede more general by using handle_sensing/3 depending
%       on what theory of action is used
%    4) inconsistent situations is now handle via type-theory inconsistent/1 
%    5) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PLANNER TOP LEVEL.
% wscp/5 plans for a Goal up to some Max depth. 
%        Assumes no exogenous action simulator and all actions allowed
% wscp/7 plans for a Goal up to some Max depth. 
%        It uses the SimNo^th exogenous action simulator and 
%        IA is the list of actions allowed for the planning problem
%        S is the initial history to plan from
%        Name is a descriptive name for the planning (link restrict_actions)
wscp(Name,Goal,Max,S,Plan)          :- wscp(Name,Goal,Max,all,_,S,Plan).
wscp(Name,Goal,Max,IA,SimNo,S,Plan) :- idplan(Name,Goal,0,Max,S,Plan,IA,SimNo).

% Iterative deeping planner
%  --- Goal is the condition to planfor
%  --- N is the current level
%  --- M is the maximum level allowed 
%  --- Ini is the initial history
%  --- Plan is the computed plan
%  --- IA is the initial set (list) of legal actions to use (all=all actions)  
%  --- SimNo is the number of the exogenous action simulator to use (if any)
idplan(Name,Goal,N,_,Ini,Plan,IA,SimNo) :- 
                     dfplan(Name,Goal,N,Ini,Plan,IA,SimNo).
idplan(Name,Goal,N,M,Ini,Plan,IA,SimNo) :- N < M, N1 is N+1, 
                     idplan(Name,Goal,N1,M,Ini,Plan,IA,SimNo).

% Depth-first planner for Goal, up to depth level N
% Simulated actions are "added" to both the situation S and the Plan
% Usually, simulated actions will be stated via the sim(_) construct
dfplan(_,Goal,_,S,[],_,_)           :- holds_wscp(Goal,S). 
dfplan(Name,Goal,N,S,Plan,AA,SimNo) :- N > 0, 
    filter(Name,Goal,N,AA,S,AA3), AA3\=[],
    simulate_exog(S,SimNo,SE), append(SE,S,S2), append(SE,Plan2,Plan),
    prim_action(A), allowed(A,AA3), 
    poss(A,C2), holds_wscp(C2,S2), 
    (fix_term(A) -> true ; true),    % Ground the action A if possible
    N1 is N-1, try_action(Name,Goal,N1,S2,A,Plan2,AA,SimNo).

% Perform the forward filtering. AA3 is the new set of possible actions
filter(Name,Goal,N,AA,S,AA3):- restrict_actions(Name,Goal,N,AA,C1,AA2), !,
                               (holds_wscp(C1,S) -> AA3=AA2 ; AA3=AA).
filter(_,_,_,A,_,A).        % Assume no forward filtering
   
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


% allowed(A,AA): Action A is an allowed action w.r.t. AA
% If AA=[], then every action of the domain is allowed
allowed(_,all):- !.
allowed(A,AA):- \+ \+ member(A,AA), !.

% Given situation S, and simulator number SimNo, S2 will be the next
% situation that will contain all simulated exogenous actions
simulate_exog(S,SimNo,[A|SE]):- \+ var(SimNo), 
                                simulator(SimNo,C,A), holds_wscp(C,S), !,
	                        simulate_exog([A|S],SimNo,SE).
simulate_exog(_,_,[]).


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

