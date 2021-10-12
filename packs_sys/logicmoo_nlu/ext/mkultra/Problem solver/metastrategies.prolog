strategy(resolve_conflict(_, List), X) :-
   % Pick randomly; need once, or it just regenerates the whole list.
   once(random_member(X, List)).