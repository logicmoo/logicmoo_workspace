strategy(a, b).
strategy(b, c).
strategy(b, d).

test(problem_solver(single_match),
     [ true(L == [b]) ]) :-
   matching_strategies(L, a).