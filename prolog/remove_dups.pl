:- module(remove_dups, [remove_dups/2,
			remove_dups/4]).

remove_dups(L, U) :-
    remove_dups(L, [], U, []).

remove_dups([E|L], H0, R0, T) :-
    ( \+ memberchk(E, H0) ->
      R0 = [E|R],
      H = [E|H0 ]
    ; R = R0,
      H = H0
    ),
    remove_dups(L, H, R, T).
remove_dups([], _, T, T).

