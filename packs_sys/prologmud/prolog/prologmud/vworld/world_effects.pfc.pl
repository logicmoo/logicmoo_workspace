/* * <module> 
% This file gives a common place where world effects 
% (such as carrying  shield or being drunk) are implemented
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(world_effects,[]).

:- include(prologmud(mud_header)).

prologHybrid(mudActAffect/3).

% Used by eat.pl and take.pl
% Is the object worth anything (either scored points or charge)
% Score any points?


do_act_affect(Agent,Action,Obj) :-
	props(Obj,mudActAffect(Action,mudScore(S))),
	ain(mudScore(Agent,+S)),
	fail. % fail to check for charge too
% Charge up those batteries
do_act_affect(Agent,Action,Obj) :-
          props(Obj,mudActAffect(Action,mudEnergy(NRG))),
	req1(mudEnergy(Agent,Chg)),
	req1(mudStm(Agent,Stm)),
	predInstMax(Agent,mudEnergy,Max),
	(Chg + NRG) < (((Stm * 10) -20) + Max),
	ain(mudEnergy(Agent,+NRG)),
	fail. % fail to check for healing
% Heal
do_act_affect(Agent,Action,Obj) :-
           props(Obj,mudActAffect(Action,heal(Hl))),
	req1((mudHealth(Agent,Dam),
             mudStm(Agent,Stm),
             mudStr(Agent,Str))),
	req1(predInstMax(Agent,mudHealth,Max)),
	(Dam + Hl) < ((((Stm * 10) -20) + ((Str * 5) - 10)) + Max),
	ain(mudEnergy(Agent,+Hl)),
	!.
do_act_affect(_,_,_).


% Check to see if last action was successful or not
:-export(wasSuccess/2).
%:-start_rtrace.
wasSuccess(Agent,What,YN) :- ((mudCmdFailure(Agent,What) -> YN=vFalse ; YN=vTrue)).
%:-stop_rtrace.
%:-prolog.

:-export(add_cmdfailure/2).
add_cmdfailure(Agent,What):-ain(mudCmdFailure(Agent,What)).

% Initialize world.
% This keeps the old databases messing with new runs.
           

:-dynamic(spawn_objects/1).

% Check to see if any of the objects should be placed in the world as it runs.

:-export(call_update_charge/2).
call_update_charge(Agent,What):- padd(Agent,mudLastCmdSuccess(What,vTrue)), doall(must(update_charge(Agent,What))),!.

:-export(call_update_stats/2).
call_update_stats(Agent,What):- padd(Agent,mudLastCmdSuccess(What,vTrue)), doall(must(update_stats(Agent,What))),!.

set_stats(Agent,[]) :- set_stats(Agent,[mudStr(2),mudHeight(2),mudStm(2),mudSpd(2)]).

set_stats(Agent,Traits) :-
        clr(stat_total(Agent,_)),
        ain(stat_total(Agent,0)),	
	forall(member(Trait,Traits),
	       ignore(catch(process_stats(Agent,Trait),E,dmsg(E:process_stats(Agent,Trait))))),
               ignore(catch(check_stat_total(Agent),E2,dmsg(E2:check_stat_total(Agent)))).
set_stats(Agent,Traits):-dmsg(warn(failed(set_stats(Agent,Traits)))).

process_stats(Agent,mudStr(Y)) :-
	ain(mudStr(Agent,Y)),
	must((mudHealth(Agent,Dam),number(Dam)))->
	NewDam is (Dam + ((Y * 5) - 10)),
	ain(mudHealth(Agent,NewDam)),
	ain(stat_total(Agent,+Y)).

process_stats(Agent,mudHeight(Ht)) :-
	ain(mudHeight(Agent,Ht)),
	ain(stat_total(Agent,+Ht)).

process_stats(Agent,mudStm(Stm)) :-
	ain(mudStm(Agent,Stm)),
	req1(mudHealth(Agent,Dam)),
	NewDam is (((Stm * 10) - 20) + Dam),
	ain(mudHealth(Agent,NewDam)),
	req1(mudEnergy(Agent,NRG)),
	Charge is (((Stm * 10) - 20) + NRG),
	ain(mudEnergy(Agent,Charge)),
	ain(stat_total(Agent,+Stm)).

process_stats(Agent,mudSpd(Spd)) :-
	ain(mudSpd(Agent,Spd)),
	ain(stat_total(Agent,+Spd)).

process_stats(Agent,Stat) :- ain(props(Agent,[Stat])).

check_stat_total(Agent) :-
	req1(stat_total(Agent,Total)),
	Total > 12,!,
	nl,
	write('Agent '),
	write(Agent),
	write(' has more than 12 points in their triats.'),
	nl,
	write('Exiting....'),
	nl,
	abort.
check_stat_total(_).
