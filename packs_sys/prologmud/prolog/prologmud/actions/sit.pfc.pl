% :-swi_module(user). 
:-swi_module(modSit, []).
/* * module * Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

ttValueType(vtPosture).

==>prologSingleValued(mudStance(tAgent,vtPosture),[prologHybrid,relationMostInstance(tAgent,vStand)]).

prologHybrid(actSetsPosture(vtVerb,vtPosture)).

vtPosture(vSit).
vtPosture(vStand).
vtPosture(vLay).
vtPosture(vSleep).
vtPosture(vKneel).

% converts vtPosture:"vSleep" to Action vtVerb:"actSleep"
vtPosture(PostureState)/i_name(act,PostureState,Action) ==>
   actSetsPosture(Action,PostureState).

actSetsPosture(Action,PostureState) ==>
   {DO=..[Action,tFurniture]}, 
  action_info_prefered(DO,txtConcatFn(PostureState, " on ", tFurniture)).

actSetsPosture(Action,PostureState) ==> 
  action_info_prefered(Action,txtConcatFn("sets agent's stance to ",PostureState)).

action_info(A,I):-action_info_prefered(A,I).

% Become PostureState on Something.
agent_call_command(Agent,actOnto(Where,PostureState)):-
        fmt('agent ~w is now ~wing on ~w',[Agent,PostureState,Where]),
        padd(Agent,mudStance(PostureState)),
        padd(Agent,localityOfObject(Where)),
	call_update_charge(Agent,PostureState).

% PostureState Action Direct
agent_call_command(Agent,Action):- callable(Action),
     functor(Action,Act,_),
     actSetsPosture(Act,PostureState),
     (compound(Action)->arg(1,Action,Where);Where=vHere),
     agent_call_command(Agent,actOnto(Where,PostureState)).


update_charge(Agent,PostureState) :- vtPosture(PostureState), padd(Agent,mudEnergy(+ -1)).

:- include(prologmud(mud_footer)).
