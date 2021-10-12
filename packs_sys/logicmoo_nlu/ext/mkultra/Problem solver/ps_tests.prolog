strategy(a, b).
strategy(b, c).
strategy(b, d).

ignore_undeclared_task(c, 0).
ignore_undeclared_task(d, 0).

test(problem_solver(single_match),
     [ true(L == [b]) ]) :-
   matching_strategies(L, a).

test(problem_solver(double_match),
     [ true(L == [c, d]) ]) :-
   matching_strategies(L, b).

test(problem_solver(no_match),
     [ true(L == []) ]) :-
   matching_strategies(L, c).

test(problem_solver(default_conflict_resolution),
     [ true(L == [pick_randomly(ConflictSet)]) ]) :-
   ConflictSet = [w, x, y, z],
   matching_strategies(L, resolve_match_failure(resolve_conflict(a, ConflictSet))).




