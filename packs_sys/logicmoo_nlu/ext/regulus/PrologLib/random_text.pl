
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/utilities').
       
:- use_module(library(lists)).
:- use_module(library(random)).

load(finnegans_wake) :-
	load_text('d:/cygwin/home/Manny/GR/finnegans_wake_text.txt').
load(pride_and_prejudice) :-
	load_text('d:/cygwin/home/Manny/GR/pride_and_prejudice.txt').

:- dynamic letter_assoc/2.
:- dynamic letter_assoc_grouped/2.

load_text(File) :-
	safe_absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	length(String, N),
	format('~N--- Read text, ~d chars, from ~w~n', [N, AbsFile]),
	make_letter_assoc(String).

%--------------------------------------------------------------------

generate(N, Start) :-
	(   get_last_three(Start, LastThree) ->
	    true
	;
	    format('~N*** Error: start string ~s need to be at least chars long~n', [Start]),
	    fail
	),
	format('~n~s', [Start]),
	generate1(LastThree, N).

get_last_three([A, B, C], [A, B, C]) :-
	!.
get_last_three([_F | R], LastThree) :-
	!,
	get_last_three(R, LastThree).

generate1(_, N) :-
	N =< 0,
	format('~n~n', []),
	!.
generate1([A, B, C], N) :-
	next_letter([A, B, C], Next),
	format('~s', [[Next]]),
	N1 is N - 1,
	!,
	generate1([B, C, Next], N1).

next_letter([A, B, C], Next) :-
	(   letter_assoc_grouped([A, B, C], Group) ->
	    true
	;
	    letter_assoc_grouped([_A, B, C], Group) ->
	    true
	;
	    otherwise ->
	    letter_assoc_grouped([_A, _B, C], Group)
	),	    
	random_member(Next, Group),
	!.
next_letter(In, Next) :-
	format('~N*** Bad call: ~w~n', [next_letter(In, Next)]),
	fail.

%--------------------------------------------------------------------

make_letter_assoc([A, B, C | Rest]) :-
	retractall(letter_assoc(_, _)),
	retractall(letter_assoc_grouped(_, _)),
	make_letter_assoc1([A, B, C], Rest, 3),
	group_letter_assoc.

make_letter_assoc1(_, [], N) :-
	format('~NStored letters, ~d chars~n', [N]).
make_letter_assoc1([A, B, C], [F | R], N) :-
	assertz(letter_assoc([A, B, C], F)),
	N1 is N + 1,
	(   ( 0 is N1 mod 10000, N > 0 ) ->
	    
	    format('(~d) ', [N1]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	make_letter_assoc1([B, C, F], R, N1).

group_letter_assoc :-
	get_all_lefts(Lefts),
	group_letter_assoc(Lefts, 0),
	format('~N--- Stored grouped assocs~n', []),
	!.

get_all_lefts(Lefts) :-
	findall(L, letter_assoc(L, _), Lefts0),
	sort(Lefts0, Lefts),
	length(Lefts, N),
	format('~N--- Found ~d lefts~n', [N]),
	!.

group_letter_assoc([], _).
group_letter_assoc([Left | R], N) :-
	findall(Right, letter_assoc(Left, Right), Rights),
	assertz(letter_assoc_grouped(Left, Rights)),
	N1 is N + 1,
	(   ( 0 is N1 mod 100, N > 0 ) ->
	    
	    format('(~d) ', [N1]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	group_letter_assoc(R, N1).



	

