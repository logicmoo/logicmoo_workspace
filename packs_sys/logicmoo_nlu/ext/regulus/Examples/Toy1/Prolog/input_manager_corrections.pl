
:- module(input_manager,
	[lf_to_dialogue_move/4]
    ).


%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

% INPUT MANAGEMENT: LF TO DIALOGUE MOVE

lf_to_dialogue_move([[control, undo]], _Words, _CurrentState, undo) :-
	!.
lf_to_dialogue_move(LF, _Words, CurrentState, Move) :-
	get_utterance_type(LF, UtteranceType),
	get_device_type(LF, Device),
	get_location(LF, Location),
	get_onoff(LF, OnOff),
	get_intensity(LF, UtteranceType, Intensity),
	Move0 = [UtteranceType, dev(Device, Location, OnOff, Intensity)],
	do_ellipsis_processing_if_necessary(Move0, CurrentState, Move1),
	add_correction_wrapper_if_necessary(LF, Move1, Move),
	!.
lf_to_dialogue_move(_LF, _Words, _PreviousMove, _Move) :-
	format('~N~nUnable to convert to dialogue move~n', []),
	fail.

%======================================================================

do_ellipsis_processing_if_necessary(Move0, CurrentState, Move) :-
	Move0 = [phrase, _],
	get_from_state(last_dialogue_move, CurrentState, PreviousMove),
	do_ellipsis_processing(Move0, PreviousMove, Move),
	!.
do_ellipsis_processing_if_necessary(Move, _CurrentState, Move).

do_ellipsis_processing(Move0, PreviousMove, Move) :-
	Move0 = [_UtteranceType0, dev(Device0, Location0, OnOff0, Intensity0)],
	PreviousMove = [UtteranceType1, dev(Device1, _Location1, OnOff1, Intensity1)],
	
	apply_default(Device0, Device1, Device),
	% Don't apply default to location
	apply_default(OnOff0, OnOff1, OnOff),
	apply_default(Intensity0, Intensity1, Intensity),

	Move = [UtteranceType1, dev(Device, Location0, OnOff, Intensity)],
	!.

apply_default(Current, Old, New) :-
	(   var(Current) ->
	    New = Old ;
	    New = Current
	),
	!.

%======================================================================

add_correction_wrapper_if_necessary(LF, Move, [correction, Move]) :-
	get_correction(LF),
	!.
add_correction_wrapper_if_necessary(_LF, Move, Move).

%======================================================================

get_utterance_type(LF, UtteranceType) :-
	member([utterance_type, UtteranceType], LF),
	!.

get_device_type(LF, Device) :-
	member([device, Device], LF),
	!.
get_device_type(_LF, _Device).

get_location(LF, Location) :-
	member([location, Location], LF),
	!.
get_location(_LF, _Location).

get_onoff(LF, OnOff) :-
	member([onoff, OnOff], LF),
	!.
get_onoff(LF, OnOff) :-
	member([action, dim], LF),
	OnOff = on,
	!.
get_onoff(_LF, _OnOff).

% For a command, "on" means 100
get_intensity(LF, command, Intensity) :-
	member([onoff, on], LF),
	Intensity = 100,
	!.
% But for a query, any value is OK
get_intensity(LF, query, Intensity) :-
	member([onoff, on], LF),
	Intensity = _,
	!.
get_intensity(LF, _DoOrQuery, Intensity) :-
	member([onoff, off], LF),
	Intensity = 0,
	!.
get_intensity(LF, _DoOrQuery, Intensity) :-
	member([action, dim], LF),
	Intensity = 50,
	!.
get_intensity(_LF, _DoOrQuery, _Intensity).

get_correction(LF) :-
	member([control, correction], LF).


