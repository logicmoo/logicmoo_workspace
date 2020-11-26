
% Nov/99
% DNA data processing
%	1. remove duplicates
%	2. break up upper-case constants to lists of lower-case constants

dna_proc(List, List3) :-
	remove_dups(List, List2),
	conv_to_lc_atoms(List2, List3),
	make_new_alpha(List3).

% dna_proc2 done by unaligned processing. Pad examples with random
% alphabet. Don't filter alphabet either, like in dna_proc.

dna_proc2(List, List4) :-  
	remove_dups(List, List2),
	conv_to_lc_atoms(List2, List3),
	unaligned_P(_, StrSize),
	List3 = [S|_],
	length(S, L),
	PadSize is integer((StrSize-L)/2),
	pad_randomly(List3, PadSize, List4).	

remove_dups([], []) :- !.
remove_dups([A|R], S) :-
	member(A, R),
	!,
	remove_dups(R, S).
remove_dups([A|R], [A|S]) :-
	remove_dups(R, S).

conv_to_lc_atoms([], []) :- !.
conv_to_lc_atoms([A|R], [B|S]) :-
	name(A, L),	
	conv_to_lc_atoms2(L, B),
	conv_to_lc_atoms(R, S).

conv_to_lc_atoms2([], []) :- !.
conv_to_lc_atoms2([A|R], [B|S]) :-
	A2 is A+32,  % upper-case ascii to lower ascii
	name(B, [A2]),
	conv_to_lc_atoms2(R, S).

sum_lengths([], 0) :- !.
sum_lengths([A|R], S) :-
	sum_lengths(R, T),
	length(A, L),
	S is T + L.

% makes N random sequences of size =< A.

make_random_strings(N, _, _, []) :-
	N =< 0, 
	!.
make_random_strings(N, L, Plist, [S|R]) :-
	M is N - 1,
	make_random_strings(M, L, Plist, R),
	repeat,
	make_randstring(L, S),
	\+ member(S, R),
	\+ member(S, Plist),
	!.

make_randstring(N, []) :-
	(N =< 0 ; maybe),
	!.
make_randstring(N, [A|R]) :-
	alphabet_P(_, L),
	select_rand(L, A),
	M is N - 1,
	make_randstring(M, R).


make_new_alpha(List) :-
	append_all(List, [], All),
	remove_dups(All, All2),
	length(All2, L),
	retract(alphabet_P(_, _)),
	assert(alphabet_P(L, All2)),
	!.

append_all([], A, A) :- !.
append_all([A|R], B, C) :-
	append(A, B, D),
	append_all(R, D, C),
	!.

pad_randomly([], _, []) :- !.
pad_randomly([S|R], Size, [S2|R2]) :- 
	make_randstring3(Size, Left),
	make_randstring3(Size, Right),
	append(Left, S, T),
	append(T, Right, S2),
	!,
	pad_randomly(R, Size, R2).

make_randstring3(N, []) :- N =< 0, !.
make_randstring3(N, [A|R]) :-
	alphabet_P(_, L),
	select_rand(L, A),
	M is N - 1,
	make_randstring3(M, R).

