:- external owner/2.

:- dynamic(location/2).


%=autodoc
%% location( ?X, ?Location) is semidet.
%
% Location.
%
t(location, X, Location) :- 
  X==player,  !, 
  t(location, $me, Location).
t(location, kavis_house, the_world).
t(location, the_world, the_game).
t(location, PhysicalObject, Location) :-  
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
located_object(Obj, Type, Where) :- 
  ( iz_a(Obj, Type)->true ; ignore(Obj= #(Type)), register_prop(Obj, Type, [])) ,  !, 
  register_prop(Obj, Type, []), 
  assert_if_new(t(location, Obj, Where)).





%=autodoc
%% unique_answer( ?Value, ?Object) is semidet.
%
% Unique Answer.
%
unique_answer(X, t(location, Object, X)) :- 
  var(X), 
  nonvar(Object).



%=autodoc
%% incompatible_cl( ?X, ?X) is semidet.
%
% Incompatible Clause.
%
incompatible_cl(t(location, X, Y), t(location, X, Z)) :-  
  Y\=Z.



%=autodoc
%% in_room( ?PhysicalObject, ?Room) is semidet.
%
% In Room.
%
in_room(PhysicalObject, Room) :- 
  room(Room), 
  t(location, PhysicalObject, Room).



%=autodoc
%% top_level_container( ?ARG1, ?Space) is semidet.
%
% Top Level Container.
%
top_level_container(Space, Space) :-
   iz_a(Space, architectural_space).
top_level_container(PhysicalObject, Container) :- 
  t(location, PhysicalObject, C), 
  ( room(C)->Container=PhysicalObject ; top_level_container(C, Container)) .



%=autodoc
%% contained_in( ?X, ?Y) is semidet.
%
% Contained In.
%
t(contained_in, X, Y) :- 
  fail, 
  var(X), 
  var(Y), 
  throw(error("contained_in/2 called with no instantiated argument.")).
t(contained_in, PhysicalObject, Location) :-
    t(location, PhysicalObject, Location).
t(contained_in, PhysicalObject, Location) :- 
  nonvar(PhysicalObject),  !, 
  t(location, PhysicalObject, Container), 
  t(contained_in, Container, Location).
t(contained_in, PhysicalObject, Location) :- 
  nonvar(Location), 
  t(location, Container, Location), 
  t(contained_in, PhysicalObject, Container).

%% room(?X) is nondet
%  X is a room.
room(X) :-
   iz_a(X, room).
