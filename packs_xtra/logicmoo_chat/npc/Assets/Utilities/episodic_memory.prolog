%%%
%%% Simple episodic memory support
%%%

:- public character_remembers/2, character_remembers_recently/2.

:- dynamic past_event/1.
:- external memorable_event/1.

maybe_remember_event(Event) :-
   memorable_event(Event),
   asserta(past_event(Event)),
   !,
   forall(on_memorable_event(Event),
	  true).
maybe_remember_event(_).

%% past_event(?Event)
%  Event is in current character's episodic memory.

%% character_remembers(?Character, ?Event)
%  Character remembers Event.
character_remembers(Character, Event) :-
   character(Character),
   Character::past_event(Event).

%% character_remembers_recently(+Character, +Event)
%  True if Event is the most recent event in Character's episodic memory.
character_remembers_recently(Character, Event) :-
   Character::past_event(E),
   !,
   E=Event.

