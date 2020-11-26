
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(input_manager,
	[lf_to_dialogue_move/5]
    ).

:- use_module('$REGULUS/Prolog/riacs_sem_postproc').
:- use_module('$REGULUS/Prolog/lf_rewrite_runtime').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================
 
% INPUT MANAGEMENT: LF TO DIALOGUE MOVE
 
lf_to_dialogue_move(LF, _WordList, _InState, Move, [LF1, LF2, LF3]) :-
	add_variables(LF, LF1),
	scope_variables(LF1, LF2),
	rewrite_lf(LF2, LF3),
	simplify_scoped_form(LF3, LF4),
	LF4 = Move,
	!.
lf_to_dialogue_move(_LF, _WordList, _InState, _DialogueMove, _IntermediateResults) :-
	format('~N~nLF to dialogue move failed~n', []),
	fail.
 
