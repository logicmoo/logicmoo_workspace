
:- module(resolve_dialogue_move,
	[resolve_dialogue_move/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').
 
%======================================================================

resolve_dialogue_move(PatternIn, _Context, PatternOut) :-
	PatternIn = PatternOut,
	!.
resolve_dialogue_move(PatternIn, Context, PatternOut) :-
	format('~N*** Error: bad call: ~w~n',
	       [resolve_dialogue_move(PatternIn, Context, PatternOut)]),
	fail.

%======================================================================
