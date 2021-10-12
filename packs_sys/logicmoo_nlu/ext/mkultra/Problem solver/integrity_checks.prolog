build_reduction_cross_reference :-
   $global::retractall(reduces_to_aux(_,_,_,_)),
   forall(reduction_clause(Goal, Reduction),
	  assert_reductions(Goal, Reduction)).

reduction_clause(Goal, Reduction) :-
   clause(strategy(Goal, Reduction),
	  _Guard).
reduction_clause(Goal, Reduction) :-
   clause(default_strategy(Goal, Reduction),
	  _Guard).
reduction_clause(Goal, Reduction) :-
   clause(normalize_task(Goal, Reduction), _Guard).

assert_reductions(Goal, _) :-
   var(Goal),
   !.
assert_reductions(Goal, Subgoal) :-
   var(Subgoal),
   !,
   functor(Goal, Name, Arity),
   ((Name = begin) ->
       true   % ignore begin
       ;
       ensure(reduces_to_aux(Name, Arity, reduction_is_a_variable, 0))).
assert_reductions(Goal, (X, Y)) :-
   !,
   assert_reductions(Goal, X),
   assert_reductions(Goal, Y).
assert_reductions(X, Y) :-
   functor(Y,begin,_),
   !,
   forall(arg(_,Y, Z),
	  assert_reductions(X,Z)).
assert_reductions(X, if(_Condition,T,E)) :-
   assert_reductions(X,T),
   assert_reductions(X,E),
   !.
assert_reductions(Goal, Subgoal) :-
   functor(Goal, GN, GA),
   functor(Subgoal, SN, SA),
   ensure(reduces_to_aux(GN, GA, SN, SA)).

reduces_to(G/GA, S/SA) :-
   reduces_to_aux(G, GA, S, SA).

:- external ignore_undeclared_task/2.

bad_reduction(G/GA, R/RA) :-
   reduces_to_aux(G, GA, R, RA),
   \+ primitive_task(R, RA),
   \+ reduces_to_aux(R, RA, _, _),
   \+ ignore_undeclared_task(R, RA).

primitive_task(reduction_is_a_variable, 0).
primitive_task(R, RA) :-
   functor(S, R, RA),
   primitive_task(S).

test(problem_solver(undeclared_tasks),
     [ setup(build_reduction_cross_reference),
       problem_list("The following tasks have no reductions",
		    BadReductions) ]) :-
   all(Reduction,
       bad_reduction(_Goal, Reduction),
       BadReductions).
