
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4,
	 print_dialogue_state/1]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

initial_dialogue_state(InitialState) :-
	empty_dialogue_state(State0),
	initial_device_state(Devices),
	set_in_state(devices, State0, Devices, InitialState).

% Initial device state = initial state of the simulated world
initial_device_state([
	       % Light in kitchen, off
	       dev(light, kitchen, off, 0),

	       % Light in living room, off
	       dev(light, living_room, off, 0),

	       % Fan in kitchen, off
	       dev(fan, kitchen, off, 0)]).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(DialogueMove, InState, AbstractAction, OutState) :-
	set_in_state(last_state, InState, InState, State1),
	set_in_state(last_dialogue_move, State1, DialogueMove, State2),
	findall([SomeAbstractAction, SomeOutState],
		possible_update_dialogue_state(DialogueMove, State2, SomeAbstractAction, SomeOutState),
		Pairs),
	update_dialogue_state1(DialogueMove, Pairs, State2, AbstractAction, OutState),
	!.
update_dialogue_state(_DialogueMove, _InState, _AbstractAction, _OutState) :-
	format('~N~nUnable to formulate abstract action.~n', []),
	fail.

update_dialogue_state1(DialogueMove, Pairs, InState, AbstractAction, OutState) :-
	DialogueMove = [query, _Pattern],
	Pairs = [],
	AbstractAction = say(no),
	InState = OutState,
	!.
update_dialogue_state1(DialogueMove, Pairs, InState, AbstractAction, OutState) :-
	DialogueMove = [command, _Pattern],
	Pairs = [],
	AbstractAction = say(unable_to_interpret),
	InState = OutState,
	!.
update_dialogue_state1(_DialogueMove, Pairs, _InState, AbstractAction, OutState) :-
	Pairs = [[SingleAbstractAction, SingleOutState]],
	AbstractAction = SingleAbstractAction,
	OutState = SingleOutState,
	!.
update_dialogue_state1(_DialogueMove, Pairs, InState, AbstractAction, OutState) :-
	length(Pairs, NumberOfPossibleAbstractActions),
	NumberOfPossibleAbstractActions > 1,
	AbstractAction = say(ambiguous),
	InState = OutState,
	!.

%----------------------------------------------------------------------

possible_update_dialogue_state([query, Pattern], InState, AbstractAction, OutState) :-
	get_from_state(devices, InState, Devices),
	member(Pattern, Devices),
	AbstractAction = say(Pattern),
	OutState = InState.

possible_update_dialogue_state([command, Pattern], InState, AbstractAction, OutState) :-
	get_from_state(devices, InState, InDevices),
	update_devices_from_pattern(InDevices, Pattern, AbstractAction, OutDevices),
	set_in_state(devices, InState, OutDevices, OutState).

update_devices_from_pattern([dev(Device, Location, OldOnOff, OldIntensity) | Rest],
			    dev(Device, Location, OnOff, Intensity), AbstractAction, 
			    [dev(Device, Location, OnOff, Intensity) | Rest]) :-
	dif([OldOnOff, OldIntensity], [OnOff, Intensity]),
	AbstractAction = say(dev(Device, Location, OnOff, Intensity)).
	
update_devices_from_pattern([First | OldRest], Pattern, AbstractAction, [First | NewRest]) :-
	update_devices_from_pattern(OldRest, Pattern, AbstractAction, NewRest).

%======================================================================

print_dialogue_state(State) :-
	(   get_from_state(devices, State, Devices) ->
	    true ;
	    Devices = undefined
	),
	(   get_from_state(last_dialogue_move, State, LastMove) ->
	    true ;
	    LastMove = undefined
	),
	format('~N          Devices: ~w~n', [Devices]),
	format('~N        Last move: ~w~n', [LastMove]),
	!.
print_dialogue_state(State) :-
	format('~N~w', [State]),
	!.
