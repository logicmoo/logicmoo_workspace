% ------------------------------------------------
% January 1999
% Author: Brian Ross
% Dept. of Computer Science, Brock University
%
% Misc prolog predicates.
%
% Sicstus: comment out member
%	- add routines for random numbers (seed, maybe, ...)

?- dynamic been_here/0.

%append([], A, A).
%append([A|X], Y, [A|Z]) :- append(X, Y, Z).

%member(A, [A|_]).
%member(A, [_|B]) :- member(A, B).

memberd(A, L) :- once(member(A,L)).

% my_random(R, N) finds a random integer N between 1 and R
% Note: N must be uninstantiated!

my_random(R, N) :-
       % S is R + 1,
	random_between(1, R, N),
	!.

% probability2(P, M) satisfied when random number between 0 and M
% is less or equal to probability P.

probability2(P, M) :-
	random(X),
	Y is X*M,
	Y =< P,
	!.

size_of([], 0).
size_of([_|R], K) :-
	size_of(R, L),
	K is L + 1,
	!.

% once(P) :- P, !.

writel([]) :- !, ttyflush.
writel([A|R]) :- var(A), !, write(A), write(' '), writel(R).
writel([nl|R]) :- !, nl, writel(R).
writel([A|R]) :- !, write(A), write(' '), writel(R).
writel(A) :- write(A), ttyflush, !.

writel2([]) :- !, ttyflush.
writel2([A|R]) :- var(A), !, write(A), writel2(R).
writel2([nl|R]) :- !, nl, writel2(R).
writel2([A|R]) :- !, write(A), writel2(R).
writel2(A) :- write(A), ttyflush, !.

/*
sum_list([], 0).
sum_list([A|L], N) :-
	sum_list(L, M),
	N is M + A,
	!.

max_list([A|L], Max) :-
	once(max_list2(A, L, Max)).

max_list2(A, [], A).
max_list2(A, [B|L], Max) :-
	B > A,
	max_list2(B, L, Max).
max_list2(A, [_|L], Max) :-
	max_list2(A, L, Max).

min_list([A|L], Min) :-
	once(min_list2(A, L, Min)).

min_list2(A, [], A).
min_list2(A, [B|L], Min) :-
	B < A,
	min_list2(B, L, Min).
min_list2(A, [_|L], Min) :-
	min_list2(A, L, Min).
*/

copy_struct(S, T) :-
	assert(temp(S)),
	retract(temp(T)),
	!.

count(_, [], 0).
count(X, [X|Y], C) :-
	!,
	count(X, Y, C2),
	C is C2 + 1.
count(X, [_|Y], C) :-
	count(X, Y, C).

round(X, Y) :- Y is integer(X+0.5), !.

set_random_number_gen :-
	been_here,
	!.
set_random_number_gen :-
	assert(been_here),
	!,
	seed_P(X,Y),
	set_seed(X,Y).

set_seed(default, _) :- !.
set_seed(random, _) :-
	datime(datime(Year,Month,Day,Hour,Min,Sec)),
	% N is approx number of seconds since Jan 1 1970
	N is (((((Year-1970)*12+Month)*30+Day)*24+Hour)*60+Min)*60+Sec,
	R1 is mod(N, 30270) + 1,  % between 1 and 30000
	N2 is abs(N >> 2), 	  % shift right 2 bits
	R2 is mod(N2, 30308) + 1,
	N3 is abs(N >> 4),        % shift right 4 bits
	R3 is mod(N3, 30324) + 1,
	setrand(rand(R1,R2,R3)),
	retract(seed_P(_,_)),
	assert(seed_P(random, (R1,R2,R3))),
	!.
set_seed(manual, (X, Y, Z)) :-
	setrand(rand(X,Y,Z)).
	

/*
set_seed(default, _) :- !.
set_seed(random, _) :-
	now(N),
	R1 is mod(N, 30000) + 1,  % between 1 and 30000
	N2 is abs(N >> 2), 	  % shift right 2 bits
	R2 is mod(N2, 30000) + 1,
	N3 is abs(N >> 4),        % shift right 4 bits
	R3 is mod(N3, 30000) + 1,
	getrand(random(_,_,_,B)),
	setrand(random(R1,R2,R3,B)),
	retract(seed_P(_,_)),
	assert(seed_P(random, (R1,R2,R3))),
	!.
set_seed(manual, (X, Y, Z)) :-
	getrand(random(_,_,_,B)), % use default bit string (could be changed)
	setrand(random(X,Y,Z,B)),
	!.
*/

debug_echo(L) :- debug_set_P(yes), !, writel(L).
debug_echo(_).

rem_dups([], []).
rem_dups([A|R], R2) :- 
	member(A, R),
	!,
	rem_dups(R, R2).
rem_dups([A|R], [A|R2]) :-
	rem_dups(R, R2),
	!.

average(M, Avg) :-
	sum_list(M, Sum),
	size_of(M, N),
	Avg is Sum / N,
	!.

% keep appending B to A until A is at least length K.

extend_list(A, _, K, A) :-
	length(A, K2),
	K2 >= K,
	!.
extend_list(A, B, K, A2) :-
	append(A, B, A3),
	extend_list(A3, B, K, A2),
	!.

num_list(0, []) :- !.
num_list(N, [N|R]) :-
	M is N - 1,
	num_list(M, R).

remove(_, [], []).
remove(A, [A|B], B).
remove(A, [X|B], [X|C]) :- remove(A, B, C).

remove_all(_, [], []).
remove_all(A, [A|B], C) :- !, remove_all(A, B, C).
remove_all(A, [X|B], [X|C]) :- remove_all(A, B, C).


intersect([], _,[]).
intersect([X|Y], R, [X|Z]) :-
	member(X, R),
	!,
	intersect(Y, R, Z).
intersect([_|Y], R, Z) :-
	intersect(Y, R, Z).

set_diff([], T, T) :- !.
set_diff(T, [], T) :- !.
set_diff([A|B], T, Diff) :-
	member(A, T),
	!,
	remove_all(A, T, T2),
	set_diff(B, T2, Diff).
set_diff([A|B], T, [A|R]) :-
	set_diff(B, T, R).

remove_list([], B, B) :- !.
remove_list([A|B], C, D) :-
	remove(A, C, E),
	!,
	remove_list(B, E, D).

writelist([]) :- nl, !.
writelist([A|R]) :- write(A), nl, writelist(R), !.

maybe :- maybe(0.5), !.
	
maybe(X) :-
	random(Y),
	Y < X,
	!.

random_permutation(L, Perm) :-
	length(L, Len),
	random_permutation2(L, Len, Perm),
	!.

random_permutation2([], _, []).
random_permutation2(L, Len, [X|Perm]) :-
	random(0, Len, R),
	remove_nth(R, L, X, L2),
	Len2 is Len-1,
	random_permutation2(L2, Len2, Perm),
	!.

remove_nth(0, [X|Y], X, Y) :- !.
remove_nth(N, [X|Y], Z, [X|W]) :-
	N2 is N-1,
	remove_nth(N2, Y, Z, W),
	!.

select_rand(L, R) :-
	length(L, Len),
	Len > 0,
	random(0, Len, Rand),
	remove_nth(Rand, L, R, _),
	!.


% first_K(M, N, List, Grabbed) counts from M to N, grabbing the first N
% entries of List, returning them in Grabbed.

first_K(M, N, _, []) :- M >= N, !.
first_K(M, N, [A|R], [A|S]) :- 
	M2 is M + 1,
	first_K(M2, N, R, S),
	!.

