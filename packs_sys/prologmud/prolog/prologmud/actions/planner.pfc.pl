% planner.pl
% July 1, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This file defines the agents action of planning and carries out plan. 
% Very simple... but kept separate to maintain modularity
%
*/


:- swi_module(modPlan, []).
:- include(prologmud(mud_header)).
% :- register_module_type (mtCommand).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Mostly Required] Load the Logicmoo Plan Generator System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- absolute_file_name(library(logicmoo_planner),P,[file_type(prolog),access(read)]),dmsg(library(logicmoo_planner)=P).
:- load_library_system(library(logicmoo_planner)).


:- dynamic on_command_show/3.
:- multifile on_command_show/3.


on_command_show(Agent,actLook,goals=nop(SHOW)):- findall(Value,agentGOAL(Agent,Value),SHOW),SHOW\==[].
on_command_show(Agent,actLook,todo=nop(SHOW)):- findall(Value,agentTODO(Agent,Value),SHOW),SHOW\==[].

baseKB:action_info(actPlan(ftTerm),"carry out a plan").

% Plan something
baseKB:agent_call_command(Agent,actPlan(Goal)) :-
   ain(agentGOAL(Agent,Goal)),
   dmsg(call(listing(agentGOAL(Agent,_)))),
   pddl_idea(Agent,Act),
   dmsg(call(listing(agentTODO(Agent,_)))),
   pddl_vette_idea(Agent,Act,_ActV).
  

update_charge(Agent,actPlan) :- padd(Agent,mudEnergy(+ -1)).



tpf_sanity:-flag(time_used,_,0),
  must(if_defined((parseDomain(string("

(define (domain domAgentVehical)
  (:requirements :strips) 
  (:predicates 	(tItem ?obj)
	       	(tVehical ?truck)
               	(tRegion ?loc)
		(tAgent ?d)
		(inRegion ?obj ?loc)
		(mudInside ?obj1 ?obj)
		(mudDriving ?d ?v)
		(mudVehicalPath ?x ?y) (mudFootPath ?x ?y)
		(is_Empty ?v)
)


(:action LOAD-VEHICAL
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (tItem ?obj) (tVehical ?truck) (tRegion ?loc)
   (inRegion ?truck ?loc) (inRegion ?obj ?loc))
  :effect
   (and (not (inRegion ?obj ?loc)) (mudInside ?obj ?truck)))

(:action UNLOAD-VEHICAL
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (tItem ?obj) (tVehical ?truck) (tRegion ?loc)
        (inRegion ?truck ?loc) (mudInside ?obj ?truck))
  :effect
   (and (not (mudInside ?obj ?truck)) (inRegion ?obj ?loc)))

(:action BOARD-VEHICAL
  :parameters
   (?tAgent
    ?truck
    ?loc)
  :precondition
   (and (tAgent ?tAgent) (tVehical ?truck) (tRegion ?loc)
   (inRegion ?truck ?loc) (inRegion ?tAgent ?loc) (is_Empty ?truck))
  :effect
   (and (not (inRegion ?tAgent ?loc)) (mudDriving ?tAgent ?truck) (not (is_Empty ?truck))))

(:action DISEMBARK-VEHICAL
  :parameters
   (?tAgent
    ?truck
    ?loc)
  :precondition
   (and (tAgent ?tAgent) (tVehical ?truck) (tRegion ?loc)
        (inRegion ?truck ?loc) (mudDriving ?tAgent ?truck))
  :effect
   (and (not (mudDriving ?tAgent ?truck)) (inRegion ?tAgent ?loc) (is_Empty ?truck)))

(:action actDrive
  :parameters
   (?truck
    ?loc-from
    ?loc-to
    ?tAgent)
  :precondition
   (and (tVehical ?truck) (tRegion ?loc-from) (tRegion ?loc-to) (tAgent ?tAgent) 
   (inRegion ?truck ?loc-from)
   (mudDriving ?tAgent ?truck) (mudVehicalPath ?loc-from ?loc-to))
  :effect
   (and (not (inRegion ?truck ?loc-from)) (inRegion ?truck ?loc-to)))

(:action actWalk
  :parameters
   (?tAgent
    ?loc-from
    ?loc-to)
  :precondition
   (and (tAgent ?tAgent) (tRegion ?loc-from) (tRegion ?loc-to)
	(inRegion ?tAgent ?loc-from) (mudFootPath ?loc-from ?loc-to))
  :effect
   (and (not (inRegion ?tAgent ?loc-from)) (inRegion ?tAgent ?loc-to)))

)
"
),DD)))),(if_defined((parseProblem(string("
(define (problem DLOG-2-2-2)
	(:domain domAgentVehical)
	(:objects
	tAgent1
	tAgent2
	truck1
	truck2
	iPackage1
	iPackage2
	s0
	s1
	s2
	p1-0
	p1-2
	)
	(:init
	(inRegion tAgent1 s2)
	(tAgent tAgent1)
	(inRegion tAgent2 s2)
	(tAgent tAgent2)
	(inRegion truck1 s0)
	(is_Empty truck1)
	(tVehical truck1)
	(inRegion truck2 s0)
	(is_Empty truck2)
	(tVehical truck2)
	(inRegion iPackage1 s0)
	(tItem iPackage1)
	(inRegion iPackage2 s0)
	(tItem iPackage2)
	(tRegion s0)
	(tRegion s1)
	(tRegion s2)
	(tRegion p1-0)
	(tRegion p1-2)
	(mudFootPath s1 p1-0)
	(mudFootPath p1-0 s1)
	(mudFootPath s0 p1-0)
	(mudFootPath p1-0 s0)
	(mudFootPath s1 p1-2)
	(mudFootPath p1-2 s1)
	(mudFootPath s2 p1-2)
	(mudFootPath p1-2 s2)
	(mudVehicalPath s0 s1)
	(mudVehicalPath s1 s0)
	(mudVehicalPath s0 s2)
	(mudVehicalPath s2 s0)
	(mudVehicalPath s2 s1)
	(mudVehicalPath s1 s2)
)
	(:goal (and
	(inRegion tAgent1 s1)
	(inRegion truck1 s1)
	(inRegion iPackage1 s0)
	(inRegion iPackage2 s0)
	))


)
"
),PP)))), !,if_defined(solve_files_ddpp(DD, PP)),
   show_call(flag(time_used,W,W)).

% :-tpf_sanity.

%:- test_domain('domains_ocl/chameleonWorld/domain*').
%:- test_all(7).


%:-prolog.

:- include(prologmud(mud_footer)).


