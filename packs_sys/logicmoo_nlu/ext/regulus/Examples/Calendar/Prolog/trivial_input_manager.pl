
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(input_manager,
	[lf_to_dialogue_move/2]
    ).

%======================================================================
 
% INPUT MANAGEMENT: LF TO DIALOGUE MOVE
 
lf_to_dialogue_move(LF, Move) :-
	LF = Move,
	!.
lf_to_dialogue_move(_LF, _DialogueMove) :-
	format('~N~nLF to dialogue move failed~n', []),
	fail.
 