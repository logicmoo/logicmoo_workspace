% attack.pl
% June 18, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This file defines the agents action of attacking. 
% Comments below document the basic idea.
%
*/

% :-swi_module(user). 
% :-module(modAttack, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% attack joe ->translates-> attack nw
vtActionTemplate(actAttack(vtDirection)).

% check to make sure the canonicalizer left the compound..
:- sanity(clause(baseKB:vtActionTemplate(actAttack(vtDirection)),true)).
% instead of replacing with..
:- sanity( \+ clause(baseKB:vtActionTemplate(actAttack),true)).



baseKB:agent_call_command(Agent,actAttack(Dir)):- once(actAttack(Agent,Dir)).

% Attack
% Successful Attack
actAttack(Agent,Dir) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),
	damage_foe(Agent,What,hit),
	call_update_charge(Agent,actAttack).

% Destroy small objects (food, etc.)
actAttack(Agent,Dir) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),	
	props(What,mudWeight(Was)),
        Was =< 1,
	destroy_object_via_attack(XXYY,What),
	call_update_charge(Agent,actAttack).

% Hit a big object... causes damage to agent attacking
actAttack(Agent,Dir) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),
	props(What,mudWeight(_)),
	call_update_stats(Agent,actBash),
	call_update_charge(Agent,actAttack).

% Hit nothing (empty space)... causes a little damage
actAttack(Agent,Dir) :-
	from_dir_target(Agent,Dir,XXYY),
	not(mudAtLoc(_,XXYY)),
	call_update_stats(Agent,wiff),
	call_update_charge(Agent,actAttack).

% Check to see if agent being attacked is carrying an 
% object which provides defence
check_for_defence(Agent,Def) :-
	findall(Poss,mudPossess(Agent,Poss),Inv),
	member(Obj,Inv),
	props(Obj,mudActAffect(_,mudArmor(Def))).
check_for_defence(_,0).

% Check to see if attacking agent has a weapon
check_for_weapon(Agent,Wpn) :-
	findall(Poss,mudPossess(Agent,Poss),Inv),
        member(Obj,Inv),
        props(Obj,mudActAffect(_,mudAttack(Wpn))).

check_for_weapon(_,0).

destroy_object_via_attack(LOC,What) :-
	del(mudAtLoc(What,LOC)),
        destroy_instance(What),!.

% Does damage to other agent
damage_foe(Agent,What,hit) :-
	del(mudHealth(What,OldDam)),
	mudStr(Agent,Str),
	check_for_defence(What,Def),
	BaseAtk is Str * 2,
	check_for_weapon(Agent,Wpn),
	Atk is (Wpn + BaseAtk),
	NewDam is (OldDam - (Atk - Def)),
	ain(mudHealth(What,NewDam)).

:- if(baseKB:startup_option(datalog,sanity);baseKB:startup_option(clif,sanity)).
:- must(prologBuiltin(damage_foe/3)).
:- must(prologBuiltin(check_for_weapon/2)).

%prologBuiltin(upprop/1).
%prologBuiltin(upprop/2).

:- endif.


update_charge(A,B):-update_charge_0(A,B).
% Record keeping
update_charge_0(Agent,actAttack) :- upprop(Agent,mudEnergy(+ -5)).

update_stats(A,B):-update_stats_0(A,B).
update_stats_0(Agent,actBash) :-  upprop(Agent,mudHealth(+ -2)),
	(add_cmdfailure(Agent,actBash)).
update_stats_0(Agent,wiff) :- 
	del(mudHealth(Agent,Old)),
	New is Old - 1,
	ain(mudHealth(Agent,New)),
	(add_cmdfailure(Agent,actBash)).

:- include(prologmud(mud_footer)).
