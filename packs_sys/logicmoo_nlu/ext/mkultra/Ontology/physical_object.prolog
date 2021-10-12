location(X, Location) :-
   X == player,
   !,
   location($me, Location).
location(kavis_house, the_world).
location(the_world, the_game).
location(PhysicalObject, Location) :-
   /perception/location/PhysicalObject:Location.

unique_answer(X, location(Object, X)) :-
   var(X),
   nonvar(Object).

incompatible(location(X, Y),
	     location(X, Z)) :-
   Y \= Z.

in_room(PhysicalObject, Room) :-
   location(PhysicalObject, Room),
   room(Room).

top_level_container(Space, Space) :-
   is_a(Space, architectural_space).
top_level_container(PhysicalObject, Container) :-
   location(PhysicalObject, C),
   (room(C) ->
       Container=PhysicalObject
       ;
       top_level_container(C, Container) ).

contained_in(X,Y) :-
   var(X),
   var(Y),
   throw(error("contained_in/2 called with no instantiated argument.")).
contained_in(PhysicalObject, Location) :-
   location(PhysicalObject, Location).
contained_in(PhysicalObject, Location) :-
   nonvar(PhysicalObject),
   !,
   location(PhysicalObject, Container),
   contained_in(Container, Location).
contained_in(PhysicalObject, Location) :-
   nonvar(Location),
   location(Container, Location),
   contained_in(PhysicalObject, Container).

%% room(?X) is nondet
%  X is a room.
room(X) :-
   is_a(X, room).
