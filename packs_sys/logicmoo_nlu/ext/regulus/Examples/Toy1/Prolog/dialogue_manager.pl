
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4]
    ).

%======================================================================

:- use_module(library(lists)).

%======================================================================

% Initial dialogue state = initial world state
initial_dialogue_state([
	       % Light in kitchen, off
	       dev(light, kitchen, off, 0),

	       % Light in living room, off
	       dev(light, living_room, off, 0),

	       % Fan in kitchen, off
	       dev(fan, kitchen, off, 0)]).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(Move, InS, AbsAct, OutS) :-
	findall([SomeAbsAct, SomeOutS],
		possible_update(Move, InS,
				SomeAbsAct, SomeOutS),
		Pairs),
	update_state1(Move, Pairs, InS, AbsAct, OutS),
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
	length(Pairs, NPossibleAbsActs),
	NPossibleAbsActs > 1,
	AbsAct = say(ambiguous),
	InS = OutS,
	!.

%----------------------------------------------------------------------

possible_update([query, Pattern], InS, AbsAct, OutS) :-
	member(Pattern, InS),
	AbsAct = say(Pattern),
	OutS = InS.

possible_update([command, Pattern], InS, AbsAct, OutS) :-
	apply_pattern(InS, Pattern, AbsAct, OutS).

apply_pattern([dev(Dev, Loc, OldOnOff, OldIntense) | R],
	      dev(Dev, Loc, OnOff, Intense), AbsAct, 
	      [dev(Dev, Loc, OnOff, Intense) | R]) :-
	dif([OldOnOff, OldIntense], [OnOff, Intense]),
	AbsAct = say(dev(Dev, Loc, OnOff, Intense)).
	
apply_pattern([F | OldR], Pattern, AbsAct, [F | NewR]) :-
	apply_pattern(OldR, Pattern, AbsAct, NewR).
