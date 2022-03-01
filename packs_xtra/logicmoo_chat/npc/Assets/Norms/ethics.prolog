:- external immoral/1, murder/1, possession/2, threatening_to_stop/2.
immoral(Action) :-
   mean(Action).

murder(Action) :=
   patient(Action, Person),
   is_a(Person, person),
   here(Person),
   true_after(Action, away(Person)).

~murder(Action) :-
   agent(Action, Actor),
   patient(Action, Person),
   intend(Person, freeze(Person, Actor)).

:- external intend/2.

% Testing purposes - Kavi wants to freeze everyone.
%intend($'Kavi', freeze($'Kavi', _)).

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