% ------------------------------------------------
% January 1999
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Genetic Programming engine II.
% 
% Tournament, steady state, Lamarckian hill-climbing option.
% Parameters set in 'gp_defn' file.
% Fitness: lower scores better, 0 is perfect.
% Population represented in program database with:
%   individual(ID_number, Fitness, Expression)
%   newindividual(ID_number, Fitness, Expression) (for separate gen)

gp :-   clean_up_1,        
	assert(best_so_far(_, _, 1000, _)),
	max_runs_P(MaxRuns, RunType, _), % from gp_defn file
	!,
	meta_run_loop(1, MaxRuns, RunType),
	writel(['*** END ***', nl, nl]).

meta_run_loop(Runs, MaxRuns, _) :-     
	Runs > MaxRuns, !,
	best_so_far(Run, Gen, Fitness, Expr),
	writel([nl,'--> Max run', MaxRuns, ' reached.',nl,
		'Best found in run ', Run, ' gen ', Gen, ':', nl,
		'   Expr = ', Expr, nl,
		'   Fitness = ', Fitness, nl, nl]),
	writel(['--> Finished runs <--', nl, nl]),
	!.
meta_run_loop(Run, MaxRuns, RunType) :-        
	Run =< MaxRuns,
	population_size_P(_, PopSize),  % gp_parameters
	max_runs_P(_, _, MaxGen),   % gp_parameters
	writel([nl, '---------------------  Run ', Run, 
		    ' ---------------------', nl]),
        since_last_datime(total,retract, _Hour,_Minute,_Sec),
        since_last_datime(generation, retract, _, _, _),
	do_the_run(0, MaxGen, PopSize),
	write('Dumping stats... '),
	dump_stats(Run),
	write('done'), nl,
	set_best_so_far(Run),
	((RunType == solution, solved_run) ->
		true
		;
		Run2 is Run + 1,
		meta_run_loop(Run2, MaxRuns, RunType)).

do_the_run(Gen, MaxGen, _) :- Gen > MaxGen, !.
do_the_run(_, _, _) :- solved_run, !.
do_the_run(0, MaxGen, PopSize) :-        
	clean_up_2,
	assert(best_in_run(_, 1000, _)),
	writel([nl, '********* Generation ', 0, '*********', nl]),
	evaluator_reset(0),
	genesis,
	set_best_in_run(0),
	print_tourn_stats(0),
	% dump_population(0),
	garbage_collect,
	!,
	do_the_run(1, MaxGen, PopSize).
do_the_run(Gen, MaxGen, PopSize) :-
	writel([nl, '********* Generation ', Gen, '*********', nl]),
	evaluator_reset(Gen),
	elite_migration(1, StartSize), % new: May/00
	tournament_loop(StartSize, PopSize),
	rename_new_popn,
	((lamarckian_P(P,_,_,_), P > 0) -> lamarckian_evolution(Gen) ; true),
	set_best_in_run(Gen),
	print_tourn_stats(Gen),
	Gen2 is Gen + 1,
	% dump_population(Gen),
	garbage_collect,
	!,
	do_the_run(Gen2, MaxGen, PopSize).

% tournament_loop(NumNew, PopSize) runs until NumNew changes
% done reaches PopSize OR run found solution.
% Possible that crossover fails (can't find similar nodes in choices, or
% children too large), and crossover will fail. 
% Else add each child.
% Mutation happens if crossover didn't.

tournament_loop(K, PopSize) :- K > PopSize, !.
tournament_loop(_, _) :- solved_run, !.
tournament_loop(K, PopSize) :-
	prob_crossover_P(PC),
	maybe(PC),		% do crossover ?
	tournament_select(best, PopSize, _, Expr1),
	tournament_select(best, PopSize, _, Expr2), % might be same ID
	(crossover(Expr1, Expr2, NewExpr1, NewExpr2) ->
		add_child(c, K, K2, PopSize, NewExpr1),
		add_child(c, K2, K3, PopSize, NewExpr2) 
		; 
		K = K3),  % in case crossover didn't succeed
	tournament_loop(K3, PopSize).
tournament_loop(K, PopSize) :-  % do mutation
	tournament_select(best, PopSize, _, Expr),
	(mutation(Expr, NewExpr) ->
		add_child(m, K, K2, PopSize, NewExpr)
		; 
		K = K2),  % in case mutation didn't succeed
	tournament_loop(K2, PopSize).

% tournament_select(Type, PopSize, ID, Expression) selects the 
% Type=best/worst Expression from Num randomly selected individuals 
% from population of size PopSize

tournament_select(best, PopSize, ID, Expression) :-
	tournament_size_P(Num, _),
	select_random_IDs(0, Num, PopSize, [], IDs),
	select(best, IDs, ID, Expression),
	!.
tournament_select(worst, PopSize, ID, Expression) :-
	tournament_size_P(_, Num),
	select_random_IDs(0, Num, PopSize, [], IDs),
	select(worst, IDs, ID, Expression),
	!.

% select_random_IDs(N, Size, PopSize, SoFar, Result) selects Size unique
% individual ID's from 1 to PopSize; N is size of temp answer SoFar.

select_random_IDs(Size, Size, _, Result, Result) :- !.
select_random_IDs(N, Size, PopSize, SoFar, Result) :-
	repeat,
	my_random(PopSize, K),
	\+ member(K, SoFar),
	N2 is N + 1,
	select_random_IDs(N2, Size, PopSize, [K|SoFar], Result).

% select the best or worst in tournament
% If a fair worst selection, then all have a chance  to be replaced in
% proportion to the number of best individuals in the population.

select(Type, [ID1|Rest], ID, Expression) :-
	individual(ID1, Fit1, _),
	select2(Type, Fit1, ID1, Rest, ID, Expression).

select2(_, _, ID, [], ID, Expression) :-
	individual(ID, _, Expression),
	!.
select2(Type, Fit1, _, [ID2|Rest], ID, Expression) :-
	individual(ID2, Fit2, _),
	((Type == best, Fit2 < Fit1);(Type == worst, Fit2 > Fit1)),
	!,
	select2(Type, Fit2, ID2, Rest, ID, Expression).
select2(Type, Fit1, ID1, [_|Rest], ID, Expression) :- 
	select2(Type, Fit1, ID1, Rest, ID, Expression).

% adding to population (replacing a weak member) if legal.
% Use a reverse tournament selection, finding indiv to replace with child.


add_child(T, K, K2, PopSize, Expr) :-
	(\+ legal(Expr,main) ->
		K2 = K
		;
		(eval_with_ID_P(yes) ->
			evaluator(K, Expr, Fitness)
			;
			evaluator(Expr, Fitness)),
		add_individual(PopSize, Fitness, Expr),
		writel(T),    % T=first arg of add_child
		K2 is K + 1),
	!.
		
add_individual(_, Fitness, NewExpr) :-
	gen_type_P(separate),
	!,
	assert(newindividual(_, Fitness, NewExpr)).
add_individual(PopSize, Fitness, NewExpr) :-
	tournament_select(worst, PopSize, ID, _),
        retract(individual(ID, _, _)),
	assert(individual(ID, Fitness, NewExpr)).

% Expression is legal if:
% 1. If unique population option is on, then if child exists in population, 
%    don't add it
% 2. If size of child exceeds max, don't add.
% 3. If expression modes set, don't add if expression fails them.
% Flag set to 'main' if called in main GP loop; else set to 'lamarck'
% (affects if newindividual exists or not; sloppy).

legal(Expr,Flag) :-
	check_unique(Expr,Flag), 
	check_depth(Expr),
	!.

check_unique(_, _) :-
	\+ unique_population_P(yes),
	!.
check_unique(Expr, main) :-
	gen_type_P(separate),
	!,
	\+ newindividual(_, _, Expr).
check_unique(Expr, _) :-
	\+ individual(_, _, Expr).

% succeed if Expression depth within limits

check_depth(Expr) :-
	max_depth_P(_, MaxDepth),
	tree_depth(Expr, D),
	D =< MaxDepth,
	!.

% succeed if solution criteria satisfied

solved_run :-
	best_in_run(_, BFitness, _),
	error_tolerance_P(Err),
	BFitness =< Err,
	!.
	
clean_up_1 :-
	set_random_number_gen,
	retractall(start_time(_)),
	retractall(best_so_far(_, _, _, _)),
	garbage_collect,
	!.

clean_up_2 :-
	retractall(best_in_run(_, _, _)),
	retractall(gp_stats(_, _, _, _, _, _, _)),
	retractall(individual(_, _, _)),
	retractall(newindividual(_, _, _)),
	retractall(popn_size(_)),
	% retractall(trace_count(_,_)),
	% retractall(saved_trace(_)),
	retractall(popn_cnt(_)),
	retractall(temp(_)),
	garbage_collect,
	!.


% for interactive exec...

clean_up :- clean_up_1, clean_up_2.

% If evaluator_reset_P(Gen) is set to a routine name, then call it before
% each generation ensues. It is called if it is the Nth generation (1st gen
% is first one a set is created).

evaluator_reset(_) :-
	evaluator_reset_P(_, no),
	!.
evaluator_reset(G) :-
	evaluator_reset_P(C, N),
	0 is mod(G, N),
	call(C),
	!.
evaluator_reset(_).
/*
evaluator_reset :-
	evaluator_reset_P(no),
	!.
evaluator_reset :-
	evaluator_reset_P(C),
	call(C),
	!.
evaluator_reset.
*/

% If a separate population scheme is being used, then rename the 
% newindividual's to individuals, and give them ID numbers.

rename_new_popn :-
	gen_type_P(separate),
	!,
	retractall(individual(_,_,_)),
	renumber_population.
rename_new_popn.

% If elite migration is on, and gen_type is separate, then migrate the
% N best individuals into new population.
% If ReEval = yes, then each has fitness recomputed (assume new generation of
% testset done beforehand elsewhere).

elite_migration(_, StartSize) :- 
	gen_type_P(separate),
	elite_migrate_P(N, ReEval), 
	N > 0,
	!,
	setof((V,K), E^individual(K,V,E), Set),
	first_K(0, N, Set, Elite),
	copy_elite(Elite, ReEval),
	StartSize is N + 1.
elite_migration(K, K) :- !. % else not done

copy_elite([], _) :- !.
copy_elite([(V,K)|B], ReEval) :-
	individual(K,_,E),
	(ReEval=yes -> 
		(eval_with_ID_P(yes) -> 
			evaluator(K, E, V2)
			;
			evaluator(E, V2)), 
		write('?')
		; 
		V=V2),
	assert(newindividual(K,V2,E)),
	!,
	copy_elite(B, ReEval).

evaluator(_K, E, V2):- evaluator(E, V2).
