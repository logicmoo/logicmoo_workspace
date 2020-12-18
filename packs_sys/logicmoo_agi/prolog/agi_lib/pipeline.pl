precepts_to_mud(Precepts,VirtWorld):- create_empty_mud(VirtWorld), mud_from_precepts(VirtWorld, Precepts).

%LOGICMOO perceives events in the real world (well in PrologMUD!) which become event sequences (we will call narratives).
?- copy_prolog_mud(real_word, RealWorld), mud_get_precepts(RealWorld, Precepts).
%Narratives may be replayed to create a copy of that of that original PrologMUD
?- create_empty_mud(VirtWorld), precepts_to_mud($VirtWorld, $Precepts).


%This PrologMUD can be thought of as a Virtual World which LOGICMOO hallucinates.
?- copy_prolog_mud($VirtWorld, Hallucination).
%This Virtual World can be re-experienced just the previous world was (as perceived events.)
?- mud_get_precepts($Hallucination, HallucinatedPrecepts).
%Those perceived events can be re- “played” to create a copy in which LOGICMOO hallucinates yet another Virtual World of the Virtual World.
?- create_empty_mud(HallucinatedVirtWorld), mud_from_precepts($VirtWorld, $Precepts).
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
