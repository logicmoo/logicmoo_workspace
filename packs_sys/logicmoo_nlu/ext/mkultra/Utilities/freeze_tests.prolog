test(freeze(wakes_on_binding),
     [ true(Y == 1) ]) :-
   freeze(X, Y = 1),
   X = 1.

test(freeze(runs_goal_when_variable_already_bound),
     [ true(Y == 1) ]) :-
   X = 1,
   freeze(X, Y = 1).

test(freeze(frozen_retrieves_goal),
     [ true(Z == 1) ]) :-
   freeze(X, Y = 1),
   frozen(X, Y = Z).

test(freeze(frozen_fails_when_goal_pattern_doesnt_match)) :-
   freeze(X, _Y = 1),
   \+ frozen(X, a).

test(freeze(doesnt_wake_without_binding),
     [ true(var(Y)) ]) :-
   freeze(_X, Y = 1).

test(freeze(thawing),
     [ true(Y == 1) ]) :-
   freeze(X, Y = 1),
   thaw(X).

test(freeze(composition_of_frozen_goals),
     [ true(X == 1),
       true(Y == 2) ]) :-
   freeze(A, X = 1),
   freeze(B, Y = 2),
   A = B,
   A = 1.

test(freeze(dif_vars_are_different)) :-
   dif(X,_Y),
   X = 1.

test(freeze(dif_vars_bound_independently_to_same_value)) :-
   \+ (dif(X,Y),
       X = 1,
       Y = 1).

test(freeze(dif_vars_bound_to_different_values)) :-
   dif(X,Y),
   X = 1,
   Y = 2.

test(freeze(dif_vars_bound_to_same_third_var)) :-
   \+ (dif(X,Y),
       X = Z,
       Y = Z).
     