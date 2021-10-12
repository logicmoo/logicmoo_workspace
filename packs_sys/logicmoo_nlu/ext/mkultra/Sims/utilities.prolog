%%%
%%% Finding and testing Unity GameObjects
%%%

:- public prop/1, character/1, world_object/1, nearest/2, docked_with/1, after_time/1.

:- public register_room/2, register_prop/3, register_character/1.

%% register_room(*Room, *CommonNoun, *Plural)
%  IMPERATIVE
%  Add Room to the database, ensuring its singular and plural nouns are registered in the lexicon.
%  Called from Start() routine of Room.cs
register_room(Room, Kind) :-
   assert(declare_value(Room, building, kavis_house)),  %KLUGE
   asserta(location(Room, kavis_house)),
   ensure(declare_kind(Room, Kind)).

%% register_prop(*Prop, *CommonNoun, *Plural, Adjectives)
%  IMPERATIVE
%  Add Prop to the database, ensuring its singular and plural nouns are registered in the lexicon.
%  Called from Start() routine of PropInfo.cs
register_prop(Prop, Kind, Adjectives) :-
   assertion(kind(Kind), prop_has_unknown_kind(Prop, Kind)),
   ensure(prop(Prop)),
   ensure(declare_kind(Prop, Kind)),
   forall(member(A, Adjectives), ensure([A, Prop])),
   forall(is_a(Prop, K),
	  ignore(initialize_prop(Prop, K))).

%% register_character(*Character)
%  IMPERATIVE
%  Add Character to database.
%  Called from Start() routine of SimController.cs
register_character(Character) :-
   ensure(character(Character)).

%% ensure(+Fact)
%  IMPERATIVE
%  Adds Fact to database, if it is not already there.
ensure([Functor | Arguments]) :-
   !,
   Predication =.. [Functor | Arguments],
   ensure(Predication).
ensure(Assertion) :-
   functor(Assertion, F, A),
   external(F/A),
   (Assertion ; assertz($global::Assertion)).

%% world_object(?GameObject)
%  GameObject is a prop or character.
world_object(WorldObject) :-
    prop(WorldObject) ; character(WorldObject).

%% nearest(-GameObject, :Constraint)
%  GameObject is the nearest object satisfying Constraint.
nearest(GameObject, Constraint) :-
    arg_min(GameObject,
	    Distance,
	    ( Constraint,
	      exists(GameObject),
	      Distance is distance(GameObject, $me))).

%% elroot(+GameObject, -Root)
%  Returns the root of the EL database for GameObject, if there is one.

elroot(GameObject, Root) :-
   component_of_gameobject_with_type(KB, GameObject, $'KB'),
   Root is KB.'KnowledgeBase'.'ELRoot' .

:- public exists/1.

%% exists(*GameObject)
%  The specified game object has not been destroyed
exists(X) :-
   is_class(X, $'GameObject'),
   ( component_of_gameobject_with_type(C, X, $'PhysicalObject') ->
        C.'Exists'
        ;
        true ).
~exists(X) :-
   is_class(X, $'GameObject'),
   component_of_gameobject_with_type(C, X, $'PhysicalObject'),
   \+ C.'Exists'.

:- public true_location/2.

%% true_location(+GameObject, -Container)
%  Returns the true location of GameObject, bypassing the perceptual system.
true_location(GameObject, Container) :-
   component_of_gameobject_with_type(C, GameObject, $'PhysicalObject'),
   property(C, "Container", Container).

:- public existing/2.

%% existing(*Kind, ?GameObject)
%  GameObject is an undestroyed instance of Kind
existing(Kind, Object) :-
   is_a(Object, Kind),
   exists(Object).

%% kill(+Character)
%  Kills (destroys) the character.  The character will stop updating.
:- public kill/1.
kill(Character) :-
   component_of_gameobject_with_type(SimController, Character, $'SimController'),
   call_method(SimController, destroy, _).

:- public dead/1, alive/1.

%% dead(?X)
%  X is a dead (nonexisting) person.
dead(X) :- is_a(X, person), ~exists(X).
~dead(X) :- is_a(X, person), exists(X).

%% alive(?X)
%  X is a living (undestroyed) person
alive(X) :- is_a(X, person), exists(X).
~alive(X) :- is_a(X, person), ~exists(X).

%% docked_with(?GameObject)
%  The character is currently docked with GameObject or its top-level container.
docked_with(WorldObject) :-
   /perception/docked_with:WorldObject,
   !.
docked_with(WorldObject) :-
   top_level_container(WorldObject, Container),
   WorldObject \= Container,
   docked_with(Container).

%% after_time(+Time)
%  The current time is after Time.
after_time(Time) :-
   $now > Time.

%%%
%%% Hidden objects
%%%

hidden(X) :-
   is_class(X, $'GameObject'),
   component_of_gameobject_with_type(PhysicalObject, X, $'PhysicalObject'),
   PhysicalObject.'IsHidden'.

reveal(X) :-
   component_of_gameobject_with_type(PhysicalObject, X, $'PhysicalObject'),
   PhysicalObject.'SetHidden'(false).

hidden_contents(Container, HiddenObject) :-
   parent_of_gameobject(HiddenObject, Container),
   hidden(HiddenObject).



%%%
%%% Character initialization.
%%%

:- public do_all_character_initializations/0.

%% do_all_character_initializations
%  IMPERATIVE
%  Called once by SimController.Start().
%  DO NOT CALL!
do_all_character_initializations :-
    (character_initialization, fail) ; true.

%% character_initialization
%  IMPERATIVE
%  All rules for this will be called once when the game object receives a Start() message.
:- external character_initialization/0.

%%%
%%% UID generation
%%%

%% allocate_UID(UID)
%  IMPERATIVE
%  UID is a unique integer not previously allocated within this character.
allocate_UID(UID) :-
    begin(/next_uid:UID,
	  NextUID is UID+1,
	  assert(/next_uid:NextUID)).

