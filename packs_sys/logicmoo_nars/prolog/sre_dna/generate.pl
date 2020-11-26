% ------------------------------------------------
% Jan 99 
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Random generation of CCS expressions.
% As in Koza, a ramped half-and-half approach used: 
%
% If depth = M, then equal spread of trees of approx. depth 2, 3, ..., M.
% Tree sizes not precise: list terms (restrict, relabel) can vary as much
% as +4 (or more?), because I'm avoiding backtracking for tree depth size.
% For each depth category, rougly equal split attempted between full 
% and grow trees.
% Full tries to get full depth on all branches.
% Growth trees are irregularly shaped.
% The generation cycles between them and depth size. Cycling done to
% reduce duplicates that are common with grow tree generation.
%
% ramped_population: create a population using ramped approach.
% Equal spread of trees using full and growth generation, and of size
% 2, 3, ..., MaxDepth.
% First, old population in individual/3 retracted.
% All expressions asserted into: individual(ID, Val, Expr). 

ramped_population(PopSize) :-
	retractall(individual(_,_,_)),
	max_depth_P(MaxDepth, _),
	dctg_root_P(Root),
	setof(D, X^Y^Z^(fast:dctg_rule_info(Root,X,Y,D,Z)), L),
	max_list(L, MinDepth),
	populate(MinDepth, MaxDepth, MinDepth, grow, 0, PopSize),
	number_population,
	!.

% populate(D, MaxDepth, MinDepth, Type, CurrPopn, PopSize) loops until PopSize
% individuals created. Depth D goes between 2 and MaxDepth. Type toggles
% between grow and full.

populate(_, _, _, _, MaxPopn, MaxPopn) :- !.
populate(D, MaxD, MinD, Type, Popn, MaxPopn) :- 
	D > MaxD,
	!,
	populate(MinD, MaxD, MinD, Type, Popn, MaxPopn).
populate(D, MaxD, MinD, grow, Popn, MaxPopn) :-
	prob_grow_P(Pgrow),
	maybe(Pgrow),		% new! May/00: only Pgrow% chance of grow tree
	make_individual(D, grow, Popn, Popn2),
	!,
	populate(D, MaxD, MinD, full, Popn2, MaxPopn).
/*
populate(D, MaxD, MinD, full, Popn, MaxPopn) :-
	make_individual(D, full, Popn, Popn2),
	D2 is D + 1,
	!,
	populate(D2, MaxD, MinD, grow, Popn2, MaxPopn).
*/
populate(D, MaxD, MinD, Type, Popn, MaxPopn) :-
	make_individual(D, full, Popn, Popn2),
	D2 is D + 1,
	!,
	toggle_type(Type, Type2),	
	populate(D2, MaxD, MinD, Type2, Popn2, MaxPopn).
populate(D, MaxD, MinD, Type, Popn, MaxPopn) :- % new: June 11/99
	!,
	populate(D, MaxD, MinD, Type, Popn, MaxPopn).

toggle_type(grow, full).
toggle_type(full, grow).

% make_individual(Depth, Type, Popn, NewPopn) 
% makes an individual of Type tree of Depth size. 
% current Popn size updated to NewPopn, but might not change
% if expression rejected (not unique?)
% Each individual Expr is asserted into: 
%      individual(x, _,  Expr, Expr2)
% where Expr is main body, Expr2 is adf expression ('0' if unused).
% ID and fitness will eventually replace first 2 fields.

make_individual(Depth, Type, Popn, NewPopn) :-
	dctg_root_P(Root),
	user_args_P(UserArgs),
	!,
	generate_tree(Root, Type, Depth, UserArgs, Expr, _), % last arg is list notn.
	((unique_population_P(yes) ; unique_population_P(init)),
		individual(_, _, Expr) ->
		NewPopn = Popn
		;
		(Type == full -> writel('f') ; writel('g')), 
		assert(individual(x, _, Expr)),
		NewPopn is Popn + 1),
	!.

% consecutively numbers all the population with unique ID numbers

number_population :-
	assert(popn_cnt(0)),
	retract(individual(x, V, E)),
	retract(popn_cnt(K)),
	K2 is K + 1,
	assert(popn_cnt(K2)),
	assert(individual(K2, V, E)),
	fail.
number_population :-
	retract(popn_cnt(_)),
	!.

% consecutively renumbers all the new population with unique ID numbers

renumber_population :-
	assert(popn_cnt(0)),
	retract(newindividual(_, V, E)),
	retract(popn_cnt(K)),
	K2 is K + 1,
	assert(popn_cnt(K2)),
	assert(individual(K2, V, E)),
	fail.
renumber_population :-
	retract(popn_cnt(_)),
	!.


