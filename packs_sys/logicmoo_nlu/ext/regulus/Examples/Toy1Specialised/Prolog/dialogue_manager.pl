
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4]
    ).

%======================================================================

:- use_module(library(lists)).

%======================================================================

% Initial dialogue state = initial state of the simulated world
initial_dialogue_state([
	       % Light in kitchen, off
	       device(light, kitchen, off, 0),

	       % Light in living room, off
	       device(light, living_room, off, 0),

	       % Fan in kitchen, off
	       device(fan, kitchen, off, 0)]).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(DialogueMove, InState, AbstractAction, OutState) :-
	findall([SomeAbstractAction, SomeOutState],
		possible_update_dialogue_state(DialogueMove, InState, SomeAbstractAction, SomeOutState),
		Pairs),
	update_dialogue_state1(DialogueMove, Pairs, InState, AbstractAction, OutState),
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
	member(Pattern, InState),
	AbstractAction = say(Pattern),
	OutState = InState.

possible_update_dialogue_state([command, Pattern], InState, AbstractAction, OutState) :-
	update_state_from_pattern(InState, Pattern, AbstractAction, OutState).

update_state_from_pattern([device(Device, Location, OldOnOff, OldIntensity) | Rest],
			  device(Device, Location, OnOff, Intensity), AbstractAction, 
			  [device(Device, Location, OnOff, Intensity) | Rest]) :-
	dif([OldOnOff, OldIntensity], [OnOff, Intensity]),
	AbstractAction = say(device(Device, Location, OnOff, Intensity)).
	
update_state_from_pattern([First | OldRest], Pattern, AbstractAction, [First | NewRest]) :-
	update_state_from_pattern(OldRest, Pattern, AbstractAction, NewRest).
