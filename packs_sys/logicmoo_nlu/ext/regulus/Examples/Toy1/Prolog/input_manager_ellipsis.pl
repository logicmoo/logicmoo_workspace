
:- module(input_manager,
	[lf_to_dialogue_move/4]
    ).


%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

% INPUT MANAGEMENT: LF TO DIALOGUE MOVE

lf_to_dialogue_move(LF, _Words, CurrentState, DialogueMove) :-
	get_utterance_type(LF, UtteranceType),
	get_device_type(LF, Device),
	get_location(LF, Location),
	get_onoff(LF, OnOff),
	% We only want a specific intensity in a command - for a question, we prefer to leave it vague.
	get_onoff(LF, OnOff),
	(   UtteranceType = command ->
	    get_intensity(LF, Intensity) ;
	    true
	),
	DialogueMove0 = [UtteranceType, dev(Device, Location, OnOff, Intensity)],
	do_ellipsis_processing_if_necessary(DialogueMove0, CurrentState, DialogueMove),
	!.
lf_to_dialogue_move(_LF, _Words, _PreviousDialogueMove, _DialogueMove) :-
	format('~N~nUnable to convert to dialogue move~n', []),
	fail.

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

get_intensity(LF, Intensity) :-
	member([onoff, on], LF),
	Intensity = 100,
	!.
get_intensity(LF, Intensity) :-
	member([onoff, off], LF),
	Intensity = 0,
	!.
get_intensity(LF, Intensity) :-
	member([action, dim], LF),
	Intensity = 50,
	!.
get_intensity(_LF, _Intensity).

%======================================================================

do_ellipsis_processing_if_necessary(DialogueMove0, CurrentState, DialogueMove) :-
	DialogueMove0 = [phrase, _],
	get_from_state(last_dialogue_move, CurrentState, PreviousDialogueMove),
	do_ellipsis_processing(DialogueMove0, PreviousDialogueMove, DialogueMove),
	!.
do_ellipsis_processing_if_necessary(DialogueMove, _CurrentState, DialogueMove).

do_ellipsis_processing(DialogueMove0, PreviousDialogueMove, DialogueMove) :-
	DialogueMove0 = [_UtteranceType0, dev(Device0, Location0, OnOff0, Intensity0)],
	PreviousDialogueMove = [UtteranceType1, dev(Device1, _Location1, OnOff1, Intensity1)],
	
	apply_default(Device0, Device1, Device),
	% Don't apply default to location
	apply_default(OnOff0, OnOff1, OnOff),
	apply_default(Intensity0, Intensity1, Intensity),

	DialogueMove = [UtteranceType1, dev(Device, Location0, OnOff, Intensity)],
	!.

apply_default(Current, Old, New) :-
	(   var(Current) ->
	    New = Old ;
	    New = Current
	),
	!.
