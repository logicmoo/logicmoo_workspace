% eat.pl
% July 1, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/*
% This file defines the agents action of eating. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
*/

% :-swi_module(user). 
:-swi_module(modEat, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

genls(tFood,tEatAble).
baseKB:action_info(actEat(tEatAble),"nourish oneself").


agent_coerce_for(Pred,_TC,Agent,String,Obj):-
   \+ call(Pred,Agent,String),
      call(Pred,Agent,Obj),
      match_object(String,Obj).


% Eat something held
baseKB:agent_call_command(Agent,actEat(String)) :- 
      agent_coerce_for(mudPossess,tEatAble,Agent,String,Obj),!,
      baseKB:agent_call_command(Agent,actEat(Obj)).

% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
baseKB:agent_call_command(Agent,actEat(Obj)) :-
  ((must_det((
	mudPossess(Agent,Obj),
	must((do_act_affect(Agent,actEat,Obj))),
        must((clr(mudStowing(Agent,Obj)))),
        % dmsg_show(_),
        destroy_instance(Obj),!,
        sanity(\+ (mudPossess(Agent,Obj))),
	must((call_update_charge(Agent,actEat))))))),!.

update_charge(Agent,actEat) :- padd(Agent,mudEnergy(+ -1)).

:- include(prologmud(mud_footer)).
