%%%
%%% Affect manager
%%% Listens for events that affect affect.
%%%

standard_concern(affect_manager, 1).

on_event(E,
	 affect_manager, _, 
	 affective_reaction(P, PV, N, NV)) :-
   affective_event(E, P, PV, N, NV).

:- external hot_button_topic/1, personally_affective_event/5.
affective_event(E, P, PW, N, NW) :-
   personally_affective_event(E, P, PW, N, NW).
affective_event(mention(X), 0, 0, 1, 0.3) :-
   hot_button_topic(X).

affective_reaction(P, PW, N, NW) :-
   %log(reaction),
   $this.'AffectiveEvent'(P, PW, N, NW).