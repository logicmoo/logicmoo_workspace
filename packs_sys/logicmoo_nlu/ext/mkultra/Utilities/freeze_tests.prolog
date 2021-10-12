test(freeze(wakes_up_on_binding),
     [ true(Y == 1) ]) :-
   freeze(X, Y = 1),
   X = 1.
     