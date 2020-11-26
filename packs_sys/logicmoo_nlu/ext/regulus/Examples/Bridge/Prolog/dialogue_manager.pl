
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/5]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').

%======================================================================

% Initial dialogue state = initial world state
initial_dialogue_state([]).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(_Pattern, _LF, InS, AbsAct, OutS) :-
	AbsAct = say(i_dont_understand, present),
	OutS = InS,
	!.
update_dialogue_state(_Move, _LF, _InS, _AbsAct, _OutS) :-
	format('~N~nUnable to update.~n', []),
	fail.
