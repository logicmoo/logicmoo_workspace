
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4,
	 print_dialogue_state/1]
    ).

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

initial_dialogue_state(InitialS) :-
	empty_dialogue_state(S0),
	initial_device_state(Devices),
	set_in_state(devices, S0, Devices, InitialS).

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

update_dialogue_state(undo, InS, AbsAct, OutS) :-
	get_from_state(last_state, InS, OldS),
	OutS = OldS,
	AbsAct = say(reverted),
	!.
update_dialogue_state([correction, Move], InS, AbsAct, OutS) :-
	get_from_state(last_state, InS, OldS),
	update_dialogue_state(Move, OldS, AbsAct, OutS),
	!.
update_dialogue_state(Move, InS, AbsAct, OutS) :-
	set_in_state(last_state, InS, InS, S1),
	set_in_state(last_dialogue_move, S1, Move, S2),
	findall([SomeAbsAct, SomeOutS],
		possible_update(Move, S2, SomeAbsAct, SomeOutS),
		Pairs),
	update_state1(Move, Pairs, S2, AbsAct, OutS),
	!.
update_dialogue_state(_Move, _InS, _AbsAct, _OutS) :-
	format('~N~nUnable to update.~n', []),
	fail.

update_state1(Move, Pairs, InS, AbsAct, OutS) :-
	Move = [query, _Pattern],
	Pairs = [],
	AbsAct = say(no),
	InS = OutS,
	!.
update_state1(Move, Pairs, InS, AbsAct, OutS) :-
	Move = [command, _Pattern],
	Pairs = [],
	AbsAct = say(unable_to_interpret),
	InS = OutS,
	!.
update_state1(_Move, Pairs, _InS, AbsAct, OutS) :-
	Pairs = [[SingleAbsAct, SingleOutS]],
	AbsAct = SingleAbsAct,
	OutS = SingleOutS,
	!.
update_state1(_Move, Pairs, InS, AbsAct, OutS) :-
	length(Pairs, NumberOfPossibleAbsActs),
	NumberOfPossibleAbsActs > 1,
	AbsAct = say(ambiguous),
	InS = OutS,
	!.

%----------------------------------------------------------------------

possible_update([query, Pattern], InS, AbsAct, OutS) :-
	get_from_state(devices, InS, Devs),
	member(Pattern, Devs),
	AbsAct = say(Pattern),
	OutS = InS.

possible_update([command, Pattern], InS, AbsAct, OutS) :-
	get_from_state(devices, InS, InDevs),
	update_devices_from_pattern(InDevs, Pattern, AbsAct, OutDevs),
	set_in_state(devices, InS, OutDevs, OutS).

update_devices_from_pattern([dev(Dev, Loc, OldOnOff, OldIntensity) | Rest],
			    dev(Dev, Loc, OnOff, Intensity), AbsAct, 
			    [dev(Dev, Loc, OnOff, Intensity) | Rest]) :-
	dif([OldOnOff, OldIntensity], [OnOff, Intensity]),
	AbsAct = say(dev(Dev, Loc, OnOff, Intensity)).
	
update_devices_from_pattern([First | OldRest], Pattern, AbsAct, [First | NewRest]) :-
	update_devices_from_pattern(OldRest, Pattern, AbsAct, NewRest).

%======================================================================

print_dialogue_state(S) :-
	(   get_from_state(devices, S, Devs) ->
	    true ;
	    Devs = undefined
	),
	(   get_from_state(last_dialogue_move, S, LastMove) ->
	    true ;
	    LastMove = undefined
	),
	format('~N             Devs: ~w~n', [Devs]),
	format('~N        Last move: ~w~n', [LastMove]),
	!.
print_dialogue_state(S) :-
	format('~N~w', [S]),
	!.
