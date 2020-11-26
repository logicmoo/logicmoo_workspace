% drop.pl
% May 18, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/* * module * 
% This file defines the basic drop predicate
% 
*/
% :-swi_module(user). 
:-swi_module(modDrop, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% orderedBy(tDropAble,tNearestReachableItem).

action_info(actDrop(isOneOf([tDropAble,tNearestReachableItem,tObj,ftID])),"Drop an item").

agent_call_command(Agent,actDrop(Obj)):- once(actDrop(Agent,Obj)).

% Drop something
actDrop(Agent,Obj) :-
	mudPossess(Agent,Obj),
        mudAtLoc(Agent,LOC),
        clr(mudPossess(Agent,Obj)),
        must(\+((mudPossess(Agent,Obj)))),
        ain(mudAtLoc(Obj,LOC)),
	must(call_update_charge(Agent,actDrop)),!.

%Nothing to drop
actDrop(Agent,_) :-
	call_update_charge(Agent,actDrop),
	(add_cmdfailure(Agent,actDrop)).

% Record keeping
update_charge(Agent,actDrop) :- ain(mudEnergy(Agent, + -1)).

agent_text_command(Agent,["drop",X],Agent,actDrop(parseForFn(tDroppable,X))).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

:- include(prologmud(mud_footer)).
