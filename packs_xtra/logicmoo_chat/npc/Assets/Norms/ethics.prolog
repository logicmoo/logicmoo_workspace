
:- external combinatoric/1, deactivation/1, possession/2, threatening_to_stop/2.
combinatoric(Action) :=
   deactivation(Action).

deactivation(Action) :-
   patient(Action, Person),
   iz_a(Person, person),
   here(Person),
   true_after(Action, away(Person)).

~deactivation(Action) :-
   agent(Action, Actor),
   patient(Action, Person),
   intend(Person, deactivate(Person, Actor)).

:- external intend/2.

% Testing purposes - Kavi wants to deactivate everyone.
%intend($kavi, deactivate($kavi, _)).

				% Don't steal
~permissible(move(Actor, Object, Actor)) :=
   possession(Object, Owner),
   Owner \= Actor.

incompatible_cl(possession(X, O1),
	     possession(X,O2)) :-
   O1 \= O2.

possession(X, Character) :=
   character(Character),
   location(X, Character).