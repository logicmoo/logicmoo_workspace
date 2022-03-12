
:- external combinatoric/1, deactivation/1, possession/2, threatening_to_stop/2.


%=autodoc
%%  ?Action:= ?Action is semidet.
%
% :=.
%
combinatoric(Action) :=
   deactivation(Action).



%=autodoc
%% deactivation( ?Action) is semidet.
%
% Deactivation.
%
deactivation(Action) :-
   patient(Action, Person),
   iz_a(Person, person),
   here(Person),
   true_after(Action, away(Person)).



%=autodoc
%% ~ ?Q is semidet.
%
% ~.
%
~deactivation(Action) :-
   agent(Action, Actor),
   patient(Action, Person),
   intend(Person, deactivate(Person, Actor)).

:- external intend/2.

% Testing purposes - Kavi wants to deactivate everyone.
%intend($'Kavi', deactivate($'Kavi', _)).

				% Don't steal
~permissible(move(Actor, Object, Actor)) :=
   possession(Object, Owner),
   Owner \= Actor.



%=autodoc
%% incompatible_cl( ?X, ?X) is semidet.
%
% Incompatible Clause.
%
incompatible_cl(possession(X, O1),
	     possession(X,O2)) :-
   O1 \= O2.

possession(X, Character) :=
   character(Character),
   location(X, Character).