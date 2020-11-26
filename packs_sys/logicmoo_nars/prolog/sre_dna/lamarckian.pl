% ------------------------------------------------
% January 1999
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Lamarckian evolution: lamarckian_P(P, K, Select, PCross)
% Performs Lamarckian evolution on P% of population, iterating 
% each K times using hill-climbing. Hill-climber uses mutation; it is
% recommended that prob_terminal_mutation_P parameter be high, or else 
% internal mutation will not create good search performance.
% Select can be tournament (best, worst) or random.
% PCross is prob crossover (prob mutation = 1 - PCross).

% lamarckian_evolution also asserts gp_stats with improvement gain obtained
% First clause efficiently processes entire population.
% Second case is if less than entire population to be used, in which case
% selection must be performed.

lamarckian_evolution(Gen) :-
	lamarckian_P(Percent, K, _, _),
	Percent >= 1.0,
	writel([nl,'Lamarckian evolution...', nl]),
	population_size_P(_, PopSize),
	num_list(PopSize, IDs),
	lamarck_loop(IDs, 0, FitImpr, 0, MaxImpr, 0, NumGain, K),
	assertz(gp_stats(Gen,_,_,_,_,_,_,lamarck(FitImpr,MaxImpr,NumGain))),
	!.
lamarckian_evolution(Gen) :-
	lamarckian_P(Percent, K, Select, _),
	Percent < 1.0,
	population_size_P(_, PopSize),
	N is integer(Percent * PopSize),
	writel([nl,'Lamarckian evolution...', nl]),
	get_unique_IDs(Select, N, PopSize, [], IDs),
	lamarck_loop(IDs, 0, FitImpr, 0, MaxImpr, 0, NumGain, K),
	assertz(gp_stats(Gen,_,_,_,_,_,_,lamarck(FitImpr,MaxImpr,NumGain))),
	!.

% get_unique_IDs retrieves a list of N unique individual ID's, 
% selecting each one via Type (random or best/worst tournament selection).

get_unique_IDs(_, 0, _, IDs, IDs) :- !.
get_unique_IDs(Type, N, PopSize, SoFar, IDs) :-
	repeat,  % in case number is repeated (member below)
	(Type = random ->
		my_random(PopSize, ID)
		;
		tournament_select(Type, PopSize, ID, _)),
	\+ member(ID, SoFar),
	M is N - 1,
	get_unique_IDs(Type, M, PopSize, [ID|SoFar], IDs),
	!.

% lamark_loop(List, ImprSoFar, FitImpr, MaxSoFar, MaxImpr, 
%		NumSoFar, NumGain, Iter) does best-first Lamarckian evolution.
% List = ordered list of individuals+Fitnesses
% ImprSoFar, FitImr = Total fitness gain so far / final
%  MaxSoFar, MaxImpr = best fitness gain so far/final
% NumSoFar, NumGain = number that have been changed so far/final
% Iter = # iterations to do
%
% Note: even if no overall fitness gain achieved, if an altered expression 
% was found, it is asserted and treated like a gain: will improve genetic
% diversity in population due to its syntactic variation.

lamarck_loop([], FitImpr, FitImpr, MaxImpr, MaxImpr, NumGain, NumGain, _) :- !.
lamarck_loop([ID|Rest], ImprSoFar, FitImpr, MaxSoFar, MaxImpr, 
		NumSoFar, NumGain, Iter) :-
	individual(ID, Fit, Expr),
	% writel(['L ID=',ID,'* ']),
	hill_climb(Iter, (Fit, Expr), (NewFit, NewExpr)),
	((NewFit >= Fit ; \+ legal(NewExpr,lamarck))
	 	   -> % don't add
		writel('-'),
		(NewFitImpr,NewMaxImpr,NumSoFar2)=(ImprSoFar,MaxSoFar,NumSoFar)
		;
		retract(individual(ID, _, _)),  
		assert(individual(ID, NewFit, NewExpr)),
		NewFitImpr is ImprSoFar + Fit - NewFit,
		NewMaxImpr is max(MaxSoFar, Fit - NewFit),
		NumSoFar2 is NumSoFar + 1,
		writel('+')),
	lamarck_loop(Rest, NewFitImpr, FitImpr, NewMaxImpr, MaxImpr, 
			NumSoFar2, NumGain, Iter),
	!.

% hill_climb(K, BestSoFar, Item) does hill-climbing search for 
% K iterations.  BestSoFar contains best expression obtained so far with 
% mutation,  and it and Item have (Fitness, Expression, Adf) structure.
% Note: Failed mutation and repeated mutation is counted as an iteration
%       Also, improved hillclimbing step does not count as an iteration.

hill_climb(K, Item, Item) :- K =< 0, !.
hill_climb(K, (TopFit, TopExpr), Soln) :- % crossover?
	lamarckian_P(_, _, _, PC),
	maybe(PC),
	population_size_P(_, PopSize),
	tournament_select(best, PopSize, _, Expr2),
	crossover(TopExpr, Expr2, NewExpr1, NewExpr2),
	evaluator(NewExpr1, NewFit1),
	evaluator(NewExpr2, NewFit2),
	select_best((NewFit1, NewExpr1), (TopFit, TopExpr), BestSoFar1),
	select_best((NewFit2, NewExpr2), BestSoFar1, BestSoFar2),
	(((NewFit1 < TopFit) ; (NewFit2 < TopFit)) -> K2 = K ; K2 is K - 2),
	hill_climb(K2, BestSoFar2, Soln),
	!.
hill_climb(K, (TopFit, TopExpr), Soln) :- % mutation?
	sre_mutation(TopExpr, NewExpr),
	evaluator(NewExpr, NewFit),
	select_best((NewFit, NewExpr), (TopFit, TopExpr), BestSoFar),
	(NewFit < TopFit -> K2 = K ; K2 is K - 1),
	%K2 is K - 1,
	hill_climb(K2, BestSoFar, Soln),
	!.
hill_climb(K, BestSoFar, Soln) :-
	K2 is K - 1,
	hill_climb(K2, BestSoFar, Soln), 
	!.

% select best of expression pairs

select_best((F1, E1), (F2, _), (F1, E1)) :- F1 =< F2, !.
select_best(_, X, X).


sre_mutation(I,C):- mutation(I,C).

% some debugging code...

test_best_first(Iter, ID) :-
	population_size_P(_, PopSize),
	tournament_select(best, PopSize, ID, _),
	individual(ID, Fit, Expr),
	hill_climb(Iter, (Fit, Expr), (NewFit, NewExpr)),
	writel(['Initial: ', nl,
		'  Fit = ', Fit, nl,
		' Expr = ', Expr, nl,
		'New: ', nl,
		'  Fit = ', NewFit, nl,
		' Expr = ', NewExpr, nl]),
	!.
	
