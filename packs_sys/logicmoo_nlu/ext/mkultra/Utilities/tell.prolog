%%%
%%% Simple forward-chaining system
%%%

:- op(1200, xfx, '==>').
:- external (==>)/2.

tell(P) :-
   P,
   !.
tell(P) :-
   tell_assertion(P),
   forall(when_added(P, Action),
	  begin(maybe_log_when_added_action(P, Action),
		Action)).

:- external log_when_added_action/2.

maybe_log_when_added_action(P, Action) :-
   log_when_added_action(P, Action) -> log((P ==> Action)) ; true.

when_added(P, tell(Q)) :-
   (P ==> Q).

:- external tell_globally/1.
tell_assertion(P) :-
   tell_globally(P) -> assert($global::P) ; assert(P).
