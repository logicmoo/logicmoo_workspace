?- dynamic testset/2.

% Experiment: evolve a SRE to conform with this stoch. regular grammar
% (from Carrasco & Forcado 96)
% 
%   S -> a S  (0.2) 
%   S -> b A  (0.8) 
%   A -> a B  (0.7) 
%   A -> b S  (0.3) 
%   B -> a A  (0.4) 
%   B -> b B  (0.1) 
%   B -> []   (0.5)
%
% Fitness: 
% - Mine expression K times, and compare distribution with test set. 
% - maximum string length enforced
% - chi-square 2-bin test to compare distributions


evaluator(Expr, Fitness) :-
	testset(_, TestSet),
	gen_set_size_P(Size),
	mine(Expr, Size, MineSet),
	%sre_mintestcnt_P(MC), % new
	tabulate_set(MineSet, 0, _, MineSet2),
	chisquare_b(MineSet2, TestSet, Size, 0.0, Fitness),
	% chisquare_2bins(MineSet2, TestSet, 0.0, Fitness),
	!.

mine(_, 0, []) :- !.
mine(Expr, K, [String|Rest]) :-
	Expr^^raw_generate(String, 0, _),
	K2 is K-1,
	mine(Expr, K2, Rest),
	!.

normalize(_, [], []) :- !.
normalize(Size, [(A,C)|R], [(A,P)|R2]) :-
	P is C/Size,
	normalize(Size, R, R2),
	!.

chisquare_b(_, [], _, Fit, Fit) :- !.
chisquare_b(MineSet, [(Test,Prob)|Rest], Sum, FitSoFar, Fitness) :-
	member_remove((Test,Count2), MineSet, MineSet2),	
	X is Prob*Sum,
	T is Count2-X,
	Fit2 is FitSoFar + ((T*T)/X),
	chisquare_b(MineSet2, Rest, Sum, Fit2, Fitness),
	!.
chisquare_b(MineSet, [(_,Prob)|Rest], Sum, FitSoFar, Fitness) :-
	Fit2 is FitSoFar + (Prob*Sum),
	chisquare_b(MineSet, Rest, Sum, Fit2, Fitness),
	!.

member_remove(X, [X|Y], Y) :- !.
member_remove(X, [Y|Z], [Y|W]) :- member_remove(X, Z, W).

count_and_remove(_, [], [], 0) :- !.
count_and_remove(A, [A|R], S, Count) :-
	!,
	count_and_remove(A, R, S, Count2),
	Count is Count2+1.
count_and_remove(A, [B|R], [B|S], Count) :-
	!,
	count_and_remove(A, R, S, Count).

% This should be called once per GP generation.

generate_testset :-
	(retract(testset(_,_)) ; true),
	gen_set_size_P(Size),
	gen_set(Size, S), % was 250
	sre_mintestcnt_P(MC),
	tabulate_set(S, MC, Sum, T),
	normalize(Sum, T, T2),
	assert(testset(Sum, T2)),
	!.

gen_set(0, []).
gen_set(K, [S|R]) :-
	K > 0,
	repeat,
	max_string_length_P(Max),
	gen_string(s, 0, Max, S),
	K2 is K-1,
	!,
	gen_set(K2, R).

gen_string(NonTerm, Len, Max, String) :-
	Len =< Max,
	(production(NonTerm, Out, NextNonTerm) ->
		Len2 is Len+1,
		gen_string(NextNonTerm, Len2, Max, R),
		String = [Out|R]
		;
		String = []).


% production(NonTerm, Output, NextNonTerm).

production(s, a, s) :- maybe(0.2).
production(s, b, a).
production(a, a, b) :- maybe(0.7).
production(a, b, s).
production(b, a, a) :- maybe(0.4).
production(b, b, b) :- maybe(0.167).

% pre-filter and count test set, rather than repeatedly process
% it during fitness evaluation of population. User can specify minimum
% count for processing.

tabulate_set([], _, 0, []).
tabulate_set([A|R], Min, Sum, [(A,C2)|S]) :-
	once(count_and_remove(A, R, R2, C)),
	C2 is C+1,
	C2 > Min,
	!,
	tabulate_set(R2, Min, Sum2, S),
	Sum is Sum2 + C2.
tabulate_set([_|R], Min, Sum, S) :-
	!,
	tabulate_set(R, Min, Sum, S).

:- multifile(chisquare/4).
:- dynamic(chisquare/4).
temp_test(Fitness) :-
	generate_tree(expr, full, 12, _, Tree1, _),
	write('Mining 1... '),
	mine(Tree1, 500, MineSet1),
	repeat,
	generate_tree(expr, full, 12, _, Tree2, _),
	nl,
	write('Mining 2... '),
	mine(Tree2, 500, MineSet2),
	nl,
	write('chi square...'),
	chisquare(MineSet1, MineSet2, 0.0, Fitness),
	nl,
	Tree1^^construct(Expr1),
	Tree2^^construct(Expr2),
	write('Expr 1:'),
	sre_pp(Expr1),
	write('Expr 2:'),
	sre_pp(Expr2).

/*
test_chi(F) :-
	generate_testset,
	testset(_,A),
	generate_testset,
	testset(_,B),
	chisquare(A,B,0,F).
*/


chisquare_2bins([], [], Fit, Fit) :- !.
chisquare_2bins([(_,Count)|Rest], [], FitSoFar, Fit) :- 
	Fit2 is FitSoFar + Count,
	chisquare_2bins(Rest, [], Fit2, Fit),
	!.
chisquare_2bins(MineSet, [(Test,Count)|Rest], FitSoFar, Fitness) :-
	member_remove((Test,Count2), MineSet, MineSet2),	
	T is (Count2-Count),
	Fit2 is FitSoFar + ((T*T)/(Count+Count2)),
	chisquare_2bins(MineSet2, Rest, Fit2, Fitness),
	!.
chisquare_2bins(MineSet, [(_,Count)|Rest], FitSoFar, Fitness) :-
	Fit2 is FitSoFar + Count,
	chisquare_2bins(MineSet, Rest, Fit2, Fitness),
	!.

