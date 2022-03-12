:- external owner/2.

:- dynamic(location/2).


%=autodoc
%% location( ?X, ?Location) is semidet.
%
% Location.
%
location(X, Location) :-
   X == player,
   !,
   location($me, Location).
location(kavis_house, the_world).
location(the_world, the_game).
location(PhysicalObject, Location) :-
   /perception/location/PhysicalObject:Location.



%=autodoc
%% located_object( ?Type, ?Where) is semidet.
%
% Located Object.
%
located_object(Type, Where):- located_object(_, Type, Where).


%=autodoc
%% located_object( ?Obj, ?Type, ?Where) is semidet.
%
% Located Object.
%
located_object(Obj, Type, Where):-
  (iz_a(Obj,Type) -> true ; (ignore(Obj = #(Type)), register_prop(Obj,Type,[]))),!,
  register_prop(Obj,Type,[]),assert_if_new(location(Obj,Where)).





%=autodoc
%% unique_answer( ?Value, ?Object) is semidet.
%
% Unique Answer.
%
unique_answer(X, location(Object, X)) :-
   var(X),
   nonvar(Object).



%=autodoc
%% incompatible_cl( ?X, ?X) is semidet.
%
% Incompatible Clause.
%
incompatible_cl(location(X, Y),
	     location(X, Z)) :-
   Y \= Z.



%=autodoc
%% in_room( ?PhysicalObject, ?Room) is semidet.
%
% In Room.
%
in_room(PhysicalObject, Room) :-
   room(Room),
   location(PhysicalObject, Room).



%=autodoc
%% top_level_container( ?ARG1, ?Space) is semidet.
%
% Top Level Container.
%
top_level_container(Space, Space) :-
   iz_a(Space, architectural_space).
top_level_container(PhysicalObject, Container) :-
   location(PhysicalObject, C),
   (room(C) ->
       Container=PhysicalObject
       ;
       top_level_container(C, Container) ).



%=autodoc
%% contained_in( ?X, ?Y) is semidet.
%
% Contained In.
%
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
   iz_a(X, room).
