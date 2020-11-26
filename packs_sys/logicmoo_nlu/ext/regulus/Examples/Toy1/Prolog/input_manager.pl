
:- module(input_manager,
	[lf_to_dialogue_move/3]
    ).


%======================================================================

:- use_module(library(lists)).

%======================================================================

% INPUT MANAGEMENT: LF TO DIALOGUE MOVE

lf_to_dialogue_move(LF, _PreviousState, Move) :-
	get_do_or_query(LF, DoOrQuery),
	get_device_type(LF, Device),
	get_location(LF, Location),
	get_onoff(LF, OnOff),
	get_intensity(LF, DoOrQuery, Intensity),
	Move =
	   [DoOrQuery,
            dev(Device, Location, OnOff, Intensity)],
	!.
lf_to_dialogue_move(_LF, _DialogueMove) :-
	format('~N~nLF to dialogue move failed~n', []),
	fail.
 
get_do_or_query(LF, DoOrQuery) :-
	member([utterance_type, DoOrQuery], LF),
	!.

get_device_type(LF, Device) :-
	member([device, Device], LF),
	!.

% Location is optional
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
