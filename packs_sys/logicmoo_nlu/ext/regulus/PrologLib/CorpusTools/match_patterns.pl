
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(match_patterns,
	  [match/2
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

match(Pattern, Line) :-
	functor(Pattern, and, N),
	!,
	match_and(N, Pattern, Line).
match(Pattern, Line) :-
	functor(Pattern, or, N),
	!,
	match_or(N, Pattern, Line).
match(not(Pattern), Line) :-
	!,
	\+ match(Pattern, Line).
match(Pattern, Line) :-
	is_substring(Pattern, Line),
	!.

match_and(I, _Pattern, _Line) :-
	I < 1,
	!.
match_and(I, Pattern, Line) :-
	arg(I, Pattern, Arg),
	match(Arg, Line),
	I1 is I - 1,
	!,
	match_and(I1, Pattern, Line).

match_or(I, Pattern, Line) :-
	I >= 1,
	arg(I, Pattern, Arg),
	(   match(Arg, Line) ->
	    true
	;
	    I1 is I - 1,
	    !,
	    match_or(I1, Pattern, Line)
	).
