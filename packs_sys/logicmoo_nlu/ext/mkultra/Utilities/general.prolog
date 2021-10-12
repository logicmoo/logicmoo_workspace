assertion(P, _) :-
   P,
   !.
assertion(_, Message) :-
   throw(error(Message)).