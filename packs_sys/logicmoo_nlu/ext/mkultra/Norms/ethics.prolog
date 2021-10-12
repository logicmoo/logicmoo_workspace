:- external immoral/1, murder/1, possession/2, threatening_to_kill/2.
immoral(Action) :-
   murder(Action).

murder(Action) :=
   patient(Action, Person),
   is_a(Person, person),
   alive(Person),
   true_after(Action, dead(Person)).

~murder(Action) :-
   agent(Action, Actor),
   patient(Action, Person),
   intend(Person, kill(Person, Actor)).

:- external intend/2.

% Testing purposes - Kavi wants to kill everyone.
%intend($'Kavi', kill($'Kavi', _)).

				% Don't steal
~permissible(move(Actor, Object, Actor)) :=
   possession(Object, Owner),
   Owner \= Actor.

incompatible(possession(X, O1),
	     possession(X,O2)) :-
   O1 \= O2.

possession(X, Character) :=
   character(Character),
   location(X, Character).