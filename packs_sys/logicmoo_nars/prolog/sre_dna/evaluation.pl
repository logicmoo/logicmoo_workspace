% ------------------------------------------------
% January 1999
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Fitness evaluation.
% 
% Evaluator/2 is user-supplied fitness function.
% It is then applied to initial population, which
% are reasserted with their fitness scores.
%
% The problem-specific evaluator should assign individuals a standardized
% fitness value (lower score is better, 0 is perfect). 
% It's syntax must be: evaluator(Expr, Val)


genesis :-
	population_size_P(InitPopSize, PopSize),
	ramped_population(InitPopSize),
	nl, evalInitialPopn,
	writel([nl, '*** Culling population', nl]),
	cull_population(InitPopSize, PopSize),
	collect_stats(0-culled),
	!.

% following only used for initial population...

evalInitialPopn :-
	retract(individual(ID, Fitness, Expression)),
	(var(Fitness) -> % only reevaluate if not scored 
		(eval_with_ID_P(yes) -> 
			evaluator(ID, Expression, Fitness)
			;
			evaluator(Expression, Fitness))
		;
		true),
	assertz(individual(ID, Fitness, Expression)),
	write('?'), ttyflush,
	fail.
evalInitialPopn :-
	collect_stats(0-genesis).

cull_population(PopSize, PopSize) :- !.
cull_population(InitPopSize, PopSize) :-
	InitPopSize < PopSize,
	!,
	writel(['Error: init pop size ', InitPopSize, '< pop size', 
		 PopSize, nl]),
	fail.
cull_population(_, PopSize) :-
	cull_method_P(elite),
	!,
	write('Culling...'),nl,
	setof((V,K,E), individual(K,V,E), Set),
	first_K(0, PopSize, Set, Set2),
	retractall(individual(_,_,_)),
	assert_elite(Set2).
cull_population(CurrPopSize, PopSize) :-
	tournament_select(worst, CurrPopSize, ID, _),
	write('x'), ttyflush,
	retract(individual(ID, _, _)),
	(ID \== CurrPopSize ->
		retract(individual(CurrPopSize, Fit, Expr)),
		assert(individual(ID, Fit, Expr))
		;
		true),
	NewPopSize is CurrPopSize - 1,
	!,
	cull_population(NewPopSize, PopSize).

% save best in run and best so far (session)

set_best_in_run(Gen) :-
        bagof(V, E^ID^individual(ID, V, E), L),
	min_list(L, Min),
	best_in_run(_, BestSoFar, _),
	Min < BestSoFar,
	!,
	individual(_, Min, Expression),
	retract(best_in_run(_, _, _)),
	assert(best_in_run(Gen, Min, Expression)).
set_best_in_run(_).

set_best_so_far(Run) :-
	best_in_run(Gen, Value, Expr),
	best_so_far(_, _, BV, _),
	Value < BV,
	!,
	retract(best_so_far(_, _, _, _)),
	assert(best_so_far(Run, Gen, Value, Expr)).
set_best_so_far(_).

% assert_elite asserts individuals into population....

assert_elite([]) :- !.
assert_elite([(V,K,E)|R]) :- 
	assert(individual(K,V,E)),
	assert_elite(R),
	!.
