:- module(sim_pipes,
  [precepts_to_sim/2]).

precepts_to_sim(Precepts,VWorld):- create_empty_sim(VWorld), run_narrative(VWorld, Precepts, VWorld).

narrative_to_sim(Narrative,VWorld):- create_empty_sim(VWorld), run_narrative(VWorld, Narrative, VWorld).

run_narrative(VWorldIn, Narrative, VWorldOut):-
  forall(elementOf(E,Narrative), 
   (resolve_narrative(E,VWorldIn,E2),
    add_narrative(E2,VWorldOut))).

% currently these are the same
precepts_to_narrative(Precepts, Narrative):- copy_term(Precepts, Narrative).
narrative_to_precepts(Precepts, Narrative):- copy_term(Narrative, Precepts).

create_empty_sim(VWorld):- 
  copy_prolog_sim(empty_sim, VWorld).

copy_prolog_sim(World1, World2):- 
  object_to_props(sim, World1, World1Props),
  copy_term(World1Props,World2Props),
  create_object(sim, World2, World2Props).

create_object(Type, Obj, ObjProps):- 
  (\+ ground(Obj)->gen_obj_sym(Type, Obj); true),
  setprops(Obj,type(Type)),
  setprops(Obj,ObjProps).



%LOGICMOO perceives events in the real world (well in PrologMUD!) which become event sequences (we will call narratives).
?- copy_prolog_sim(real_word, RealWorld), sim_get_precepts(RealWorld, Precepts), precepts_to_narrative(Precepts, Narrative).
%Narratives may be replayed to create a copy of that of that original PrologMUD
?- narrative_to_sim($Narrative, VWorld).

%This PrologMUD can be thought of as a Virtual World which LOGICMOO hallucinates.
?- copy_prolog_sim($VWorld, Hallucination).
%This Virtual World can be re-experienced just the previous world was (as perceived events.)
?- sim_get_precepts($Hallucination, HallucinatedPrecepts).
%Those perceived events can be re- “played” to create a copy in which LOGICMOO hallucinates yet another Virtual World of the Virtual World.
?- precepts_to_sim($HallucinatedPrecepts,HallucinatedVWorld).
%This can be done indefinitely; those copies may become simpler or more complex (will be explained later (as pipelines))
%The event sequences (which are narratives) are equivalent to “internal dialog “
%“internal dialog” may be modified and then “played back” to created Imagined Worlds
%An Imagined World can be re-experienced and thus create a new set of perceived Imagined events
%Those perceived Imagined events can be re-“played” to create a copies of those Imagined Worlds
%Those Imagined Worlds can be compared to each other the differences can constitute a hybrid PrologMUD
%Internal dialog can be compared to other narratives without involving worlds at all.
%The differences can be made into other narratives (thus internal dialogs)
%This is used to generalize, specialize or condense Internal Dialogs.
%It also may further isolate out the actions and recombine them to perceive them as new action sequences
%Those perceived action sequences can be “played” into copies of PrologMUD
%We may create pipelines between the above elements
%Those elements again are: Events, Internal Dialog, Actions,
%Pipelines may combine, split and recombine these into Events/Actions, and Internal Dialog 
%Thus creating Worlds/PrologMUDs/Imagined Worlds.
%Increasing and decreasing specificity within the narrative pipelines
%Can produce both generalized and condensed versions of Internal dialog.
%Douglas Miles claims this was integral to his solving The Egg Cracking problem
