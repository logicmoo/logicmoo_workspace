:- external owner/2.

:- dynamic(location/2).


%% location( ?X, ?Location) is semidet.
%
% Location.
%
t(location, X, Location) :- 
  X == $user, 
  t(location, $me, Location).

t(location, sophias_sourcecode, the_metaverse).
t(location, MetaverseObject, Location) :-  
  /perception/location/MetaverseObject:Location.



%=autodoc
%% set_located_object( ?Type, ?Where) is semidet.
%
% Located Object.
%
set_located_object(ObjIn, Where):- 
  extract_object(ObjIn, Type, Obj),
  set_located_object(Obj, Type, Where),!.


%=autodoc
%% set_located_object( ?Obj, ?Type, ?Where) is semidet.
%
% Located Object.
%
set_located_object(ObjIn, Type, Where) :- 
  extract_object(ObjIn, Type, Obj),  !, 
  register_prop(Obj, Type, []),
  register_prop(Where, container, []),
  assert_if_new(t(location, Obj, Where)),!.

extract_object(ObjIn, Type, Obj):-
  atom(ObjIn), iz_a(ObjIn, kind),!,
  Type = ObjIn, (Obj= #(Type)), register_prop(Obj, Type, []).

extract_object(ObjIn, Type, Obj):-
 iz_a(ObjIn, Type)-> ObjIn = Obj ; (ignore(Obj= #(Type)), register_prop(Obj, Type, [])).



%% unique_answer( ?Value, ?Condition) is semidet.
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
%% in_program( ?MetaverseObject, ?Program) is semidet.
%
% In Program.
%
in_program(MetaverseObject, Program) :- 
  program(Program), 
  t(location, MetaverseObject, Program).



%=autodoc
%% top_level_container( ?ARG1, ?Space) is semidet.
%
% Top Level Container.
%
top_level_container(Space, Space) :-
   iz_a(Space, architectural_space).
top_level_container(MetaverseObject, Container) :- 
  t(location, MetaverseObject, C), 
  ( program(C)->Container=MetaverseObject ; top_level_container(C, Container)) .



%% contained_in( ?X, ?Y) is semidet.
%
% Contained In.
%
t(contained_in, X, Y) :- 
  fail, 
  var(X), 
  var(Y), 
  throw(error("contained_in/2 called with no instantiated argument.")).
t(contained_in, MetaverseObject, Location) :-
    t(location, MetaverseObject, Location).
t(contained_in, MetaverseObject, Location) :- 
  nonvar(MetaverseObject),  !, 
  t(location, MetaverseObject, Container), 
  t(contained_in, Container, Location).
t(contained_in, MetaverseObject, Location) :- 
  nonvar(Location), 
  t(location, Container, Location), 
  t(contained_in, MetaverseObject, Container).

%% program(?X) is nondet
%  X is a program.
program(X) :-
   iz_a(X, program).
