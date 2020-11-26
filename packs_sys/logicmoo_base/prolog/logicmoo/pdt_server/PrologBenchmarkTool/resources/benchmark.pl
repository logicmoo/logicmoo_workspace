pbt_performance(Goal, Time, Count, Inferences, unique) :-
	!,
	performance(Goal, Time, _, Inferences),
	findall(Goal, Goal, Results),
	sort(Results, Set),
	length(Set, Count).
	
pbt_performance(Goal, Time, Count, Inferences, _) :-
	performance(Goal, Time, Count, Inferences).
	