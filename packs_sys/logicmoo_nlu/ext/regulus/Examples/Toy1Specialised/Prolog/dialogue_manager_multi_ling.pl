
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Toy1Specialised/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

% Initial dialogue state = initial state of the simulated world
initial_dialogue_state(InitialState) :-
	InitialDevices = [
				% Light in kitchen, off
			  device(light, kitchen, off, 0),

				% Light in living room, off
			  device(light, living_room, off, 0),

				% Fan in kitchen, off
			  device(fan, kitchen, off, 0)],
	%InitialLanguage = english,
	InitialLanguage = french,
	
	empty_dialogue_state(State0),
	set_in_state(language, State0, InitialLanguage, State1),
	set_in_state(devices, State1, InitialDevices, InitialState).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(DialogueMove, InState, AbstractAction, OutState) :-
	get_from_state(language, InState, Language),
	findall([SomeAbstractAction, SomeOutState],
		possible_update_dialogue_state(DialogueMove, InState, Language, SomeAbstractAction, SomeOutState),
		Pairs),
	update_dialogue_state1(DialogueMove, Pairs, InState, Language, AbstractAction, OutState),
	!.
update_dialogue_state(_DialogueMove, _InState, _AbstractAction, _OutState) :-
	format('~N~nUnable to formulate abstract action.~n', []),
	fail.

update_dialogue_state1(DialogueMove, Pairs, InState, Language, AbstractAction, OutState) :-
	DialogueMove = [query, _Pattern],
	Pairs = [],
	AbstractAction = say(Language, no),
	InState = OutState,
	!.
update_dialogue_state1(DialogueMove, Pairs, InState, Language, AbstractAction, OutState) :-
	DialogueMove = [command, _Pattern],
	Pairs = [],
	AbstractAction = say(Language, unable_to_interpret),
	InState = OutState,
	!.
update_dialogue_state1(_DialogueMove, Pairs, _InState, _Language, AbstractAction, OutState) :-
	Pairs = [[SingleAbstractAction, SingleOutState]],
	AbstractAction = SingleAbstractAction,
	OutState = SingleOutState,
	!.
update_dialogue_state1(_DialogueMove, Pairs, InState, Language, AbstractAction, OutState) :-
	length(Pairs, NumberOfPossibleAbstractActions),
	NumberOfPossibleAbstractActions > 1,
	AbstractAction = say(Language, ambiguous),
	InState = OutState,
	!.

%----------------------------------------------------------------------

possible_update_dialogue_state([query, Pattern], InState, Language, AbstractAction, OutState) :-
	get_from_state(devices, InState, Devices),
	member(Pattern, Devices),
	AbstractAction = say(Language, Pattern),
	OutState = InState.

possible_update_dialogue_state([command, Pattern], InState, Language, AbstractAction, OutState) :-
	get_from_state(devices, InState, InDevices),
	update_devices_from_pattern(InDevices, Pattern, Language, AbstractAction, OutDevices),
	set_in_state(devices, InState, OutDevices, OutState).

update_devices_from_pattern([device(Device, Location, OldOnOff, OldIntensity) | Rest],
			    device(Device, Location, OnOff, Intensity), Language, AbstractAction, 
			    [device(Device, Location, OnOff, Intensity) | Rest]) :-
	dif([OldOnOff, OldIntensity], [OnOff, Intensity]),
	AbstractAction = say(Language, device(Device, Location, OnOff, Intensity)).
	
update_devices_from_pattern([First | OldRest], Pattern, Language, AbstractAction, [First | NewRest]) :-
	update_devices_from_pattern(OldRest, Pattern, Language, AbstractAction, NewRest).
