
:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/4]
    ).

%======================================================================
:- use_module('$REGULUS/Examples/EnglishRobots/Prolog/dialogue_state').

:- use_module(library(lists)).

%======================================================================

initial_dialogue_state(InitialState):-
	InitialObjects = [obj(human,lost),
	                  obj(friend,lost),
	obj(victim,lost)],
	InitialLanguage = english,
	%InitialLanguage = german,
	
	empty_dialogue_state(State0),
	set_in_state(language, State0, InitialLanguage, State1),
	set_in_state(objects, State1, InitialObjects, InitialState).

%======================================================================

% DIALOGUE MANAGEMENT

update_dialogue_state(Move, InS, AbsAct, OutS) :-
	get_from_state(language, InS, Language),
	get_from_state(objects, InS, InObjects),
	findall([SomeAbsAct, SomeOutS],
		possible_update(Move, InObjects, Language,
				SomeAbsAct, SomeOutS),
		Pairs),
	update_state1(Move, Pairs, InObjects, Language, AbsAct, OutObjects),
	set_in_state(objects, InS, OutObjects, OutS),
	!.

update_dialogue_state(_Move, _InS, _AbsAct, _OutS) :-
	format('~N~nUnable to update.~n', []),
	fail.


update_state1(Move, Pairs, InObjects, Language, AbsAct, OutObjects) :-
	Move = [ynq | _Pattern],
	Pairs = [],
	AbsAct = say(Language, no),
	InObjects = OutObjects,
	!.
update_state1(Move, Pairs, InObjects, Language, AbsAct, OutObjects) :-
	Move = [imp | _Pattern],
	Pairs = [],
	AbsAct = say(Language, unable_to_interpret),
	InObjects = OutObjects,
	!.
update_state1(_Move, Pairs, _InObjects, _Language, AbsAct, OutObjects) :-
	Pairs = [[SingleAbsAct, SingleOutObjects]],
	AbsAct = SingleAbsAct,
	OutObjects = SingleOutObjects,
	!.

update_state1(_Move, Pairs, InObjects, Language, AbsAct, OutObjects) :-
	length(Pairs, NPossibleAbsActs),
	NPossibleAbsActs > 1,
	AbsAct = say(Language, ambiguous),
	InObjects = OutObjects,
	!.

%ACTIONS ----------------------------------------------------------------------

%Move =
%		[DoOrQuery, Action, Shape, Direction, Rotation,
%            obj(Object,LostOrFound),Location, Cardinal, Weapon]
%		
/*
%QUERIES

possible_update([ynq, _Action, _Shape, _Direction, _Rotation, Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	get_from_state(objects, InS, StateObjects),
	member(Objects, StateObjects),
	AbsAct = say(Language, Objects),
	OutS = InS.



%ACTIONS RELATED TO INTERACTING WITH FOUND OBJECTS

possible_update([imp, surround, _Shape, _Direction, _Rotation, obj(Object, found), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [surround, Object]),
	OutS = InS.

possible_update([imp, point, _Shape, _Direction, _Rotation, obj(Object, found),_Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [point, Object]),
	OutS = InS.

possible_update([imp, circle, _Shape, _Direction, _Rotation, obj(Object, found), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [circle, Object]),
	OutS = InS.
	
	
*/	




%ACTIONS RELATED TO MAKING SHAPES

possible_update([imp, make, Shape, _Direction, _Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [make, Shape]),
	OutS = InS.







%ACTIONS RELATED TO MOTION


possible_update([imp, move, _Shape, Direction, _Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Direction),
	AbsAct = say(Language, [move, Direction]),
	OutS = InS.
	
possible_update([imp, move, _Shape, _Direction, _Rotation, _Objects, _Location, Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Cardinal),
	AbsAct = say(Language, [move, Cardinal]),
	OutS = InS.
/*
possible_update([imp, move, _Shape, _Direction, _Rotation, obj(Object,found), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [move, Object]),
	OutS = InS.
*/
possible_update([imp, rotate, _Shape, Direction,_Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Direction),
	AbsAct = say(Language, [face, Direction]),
	OutS = InS.

possible_update([imp, rotate, _Shape, _Direction, _Rotation, _Objects, _Location, Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Cardinal),
	AbsAct = say(Language, [face, Cardinal]),
	OutS = InS.

possible_update([imp, rotate, _Shape, _Direction, _Rotation, obj(Object,found), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Object),
	AbsAct = say(Language, [face, Object]),
	OutS = InS.	
/*
possible_update([imp, go, _Shape, _Direction, _Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [move]),
	OutS = InS.
*/




%ACTION TO STOP ACTION (Dept. of Redundancy Dept.)

possible_update([imp, stop, _Shape, _Direction, _Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [stop]),
	OutS = InS.
	
possible_update([imp, reset,_Shape, _Direction, _Rotation, _Objects, _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [reset]),
	OutS = InS.	
	





%ACTIONS FOR OUR AMUSEMENT

possible_update([imp, fire, _Shape, _Direction, _Rotation, _Objects, _Location, _Cardinal, Weapon], InS, Language, AbsAct, OutS) :-
	nonvar(Weapon),
	AbsAct = say(Language, [fire, Weapon]),
	OutS = InS.

/*
possible_update([imp, attack_formation_delta, _Rest], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [attack_formation_delta]),
	OutS = InS.	
*/



/*
%ACTIONS RELATED TO FINDING OBJECTS

possible_update([imp, find, _Shape, _Direction, _Rotation, obj(Object, lost), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [find, Object]),
	OutS = InS.

possible_update([imp, lose, _Shape, _Direction, _Rotation, obj(Object, found), _Location, _Cardinal, _Weapon], InS, Language, AbsAct, OutS) :-
	AbsAct = say(Language, [lose, Object]),
	OutS = InS.

apply_pattern([obj(Object, OldLostOrFound) | R],
	      obj(Object, LostOrFound), AbsAct, 
	      [obj(Object, LostOrFound) | R]) :-
	dif([OldLostOrFound], [LostOrFound]),
	AbsAct = say(obj(Object, LostOrFound)).
	

apply_pattern([F | OldR], Pattern, AbsAct, [F | NewR]) :-
	apply_pattern(OldR, Pattern, AbsAct, NewR).

update_state_from_pattern([F | OldR], Pattern, AbsAct, [F | NewR]) :-
	update_state_from_pattern(OldR, Pattern, AbsAct, NewR).
*/
