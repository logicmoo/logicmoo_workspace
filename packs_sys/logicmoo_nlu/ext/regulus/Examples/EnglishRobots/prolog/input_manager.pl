:- module(input_manager,
	[lf_to_dialogue_move/3]
    ).


%======================================================================

:- use_module(library(lists)).

%======================================================================

% INPUT MANAGEMENT: LF TO DIALOGUE MOVE

lf_to_dialogue_move(LF, _PreviousState, Move) :-
	get_utterance_type(LF, DoOrQuery),
	get_object_type(LF, Object),
	get_location_type(LF,Location),
	get_shape_type(LF, Shape),
	get_direction(LF, Direction),
	get_cardinal(LF,Cardinal),
	get_rotation(LF,Rotation),
  get_is_found(LF, LostOrFound),
	get_action(LF, Action),
	get_weapon(LF, Weapon),
	Move =
		[DoOrQuery, Action, Shape, Direction, Rotation,
            obj(Object, LostOrFound),Location, Cardinal, Weapon],
       !.

    
       
lf_to_dialogue_move(_LF, _DialogueMove) :-
	format('~N~nLF to dialogue move failed~n', []),
	fail.
 
get_utterance_type(LF, DoOrQuery) :-
	member([utterance_type, DoOrQuery], LF),
	!.


	
get_object_type(LF, Object) :-
	member([object,Object], LF),
	!.
get_object_type(_LF, _Object).



get_location_type(LF, Location) :-
	member([location, Location], LF),
	!.
get_location_type(_LF, _Location).



get_shape_type(LF, Shape) :-
	member([shape, Shape], LF),
	!.
get_shape_type(_LF,_Shape).




%Lostorfound is optional. We don't want this if we're just cycling through
%formations or telling directions like "Make a triangle" or "Face to the North".

get_is_found(LF, LostOrFound) :-
	member([action,find], LF),
	LostOrFound = found,
	!.
	
get_is_found(LF, LostOrFound) :-
	member([action,lose], LF),
	LostOrFound = lost,
	!.
get_is_found(_LF, _LostOrFound).
	



get_direction(LF, Direction) :-
	member([direction, Direction], LF),
	!.
get_direction(_LF, _Direction).	



get_cardinal(LF, Cardinal) :-
	member([cardinal, Cardinal], LF),
	!.
get_cardinal(_LF, _Cardinal).


get_rotation(LF, Rotation) :-
	member([rotation, Rotation], LF),
	!.
get_rotation(_LF, _Rotation).


get_action(LF, Action) :-
	member([action,Action], LF),
	!.

get_weapon(LF, Weapon) :-
	member([weapon,Weapon],LF),
	!.
get_weapon(_LF, _Weapon).

