:- public prop/1, character/1, world_object/1, nearest/2.

prop(GameObject) :-
    % An object is a prop if it has a DockingRegion component.
    component_of_gameobject_with_type(_Component, GameObject, $dockingregion).

character(GameObject) :-
    % An object is a character if it has a SimController component.
    component_of_gameobject_with_type(_Component, GameObject, $simcontroller).

world_object(WorldObject) :-
    prop(WorldObject) ; character(WorldObject).

% GameObject is the nearest object satisfying Constraint.
nearest(GameObject, Constraint) :-
    arg_min(GameObject,
	    Distance,
	    (Constraint, Distance is distance(GameObject, $game_object))).

% Called once by SimController.Start()
do_all_character_initializations :-
    (character_initialization, fail) ; true.
