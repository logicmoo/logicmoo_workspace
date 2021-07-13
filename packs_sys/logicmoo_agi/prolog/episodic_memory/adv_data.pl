/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty"s Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/

:- '$set_source_module'(mu).

% Some Inform properties:
% light - rooms that have light in them
% can(eat) - can be eaten
% static - can"t be taken or moved
% scenery - assumed to be in the room description (implies static)
% concealed - obscured, not listed, not part of "all", but there
% found_in - lists places where scenery objects are seen
% absent - hides object entirely
% clothing - can be worn
% worn - is being worn
% container
% (opened = t) - container is open (must_mw be opened) to be used. there is no "closed").
% can(open) - can be opened and closed
% capacity(N) - number of objects a container or supporter can hold
% state(locked) - cannot be opened
% can(lock), with_key
% enterable
% supporter
% article - specifies indefinite article ("a", "le")
% cant_go
% daemon - called each turn, if it is enabled for this object
% description
% inside_description
% invent - code for inventory listing of that object
% list_together - way to handle "5 fish"
% plural - pluralized-name =  if different from singular
% when_closed - description when closed
% when_open - description when (opened = t)
% when_on, when_off - like when_closed, etc.
% Some TADS properties:
% thedesc
% pluraldesc
% is_indistinguishable
% is_visible(vantage)
% touchable($agent, actor)
% valid(verb) - is object seeable, touchable, etc.
% verification(verb) - is verb logical for this object
% Parser disambiguation:
% eliminate objs not visalbe, touchable, etc.
% check preconditions for acting on a candidate object


:- op(1199, xfx, type_props).
:- op(1199, xfx, type).
:- op(900, fx, ~).


dest_target(spatially(in, Dest), Target):- nonvar(Dest), !, dest_target(Dest, Target).
dest_target(spatially(to, Dest), Target):- nonvar(Dest), !, dest_target(Dest, Target).
dest_target(loc(_, _, _, Target), Target):- nonvar(Target), !.


% empty intial state
:- ensure_state_context(istate).
:- clear_state_context(istate).
:- set_state_context(istate).



term_expansion(StateInfo, Pos, (:- push_to_state(StateInfo)), PosO):- mu:is_state_info(StateInfo), PosO=Pos.

%:- listing(term_expansion).

%:- rtrace.

% type/2s are noticed by the term_expansion is_state_info
door type      % xformed: type_props(door, ... the below ... )
  ~can(take), % xformed: can(actTake)= f
   can(open), % xformed: can(actOpen)= t
   can(close), % xformed: can(actClose)= t
   (opened = f),
   nouns(door), % xformed: nouns(["door"])
   fully_corporial.  % xformed:  inherits(fully_corporial)

food type
   can(eat),
   moveable, % xformed:  inherits(moveable)
   measurable.  % xformed:  inherits(measurable)

basement type_props place,
   desc("This is a very dark basement."),
   (dark= t).

dining_room type_props place.


:- push_to_state([

   type_props(garden,
     [place,
   % goto($agent, Prep, Dir, dir, result) provides special handling for going in a direction.
   cant_go($agent, up, "You lack the ability to fly."),
   oper(/*garden, */ ( act3('go__dir', $agent ,[ _, south])),
   % precond(Test, FailureMessage)
     precond(getprop(screendoor, (opened = t)), ["you must_mw open the door first"]),
   % body(clause)
     body(inherited)),
   % cant_go provides last-ditch special handling for Go.
   desc = "this is the garden",
   cant_go($agent, _Dir, "The fence surrounding the garden is too tall and solid to pass.")]),

   type_props(kitchen, [inherit(place), desc("cooking happens here")]),

   on(table, table_leg),
   on(box, table),
   in(bowl, box),
   in(flour, bowl),
   in(table, kitchen), % a table is in kitchen
   on(lamp, table), % a lamp is on the table

   in(sink, kitchen),
   in(plate, sink),
   in(cabinate, kitchen),
   in(cup, cabinate),

end_of_list]).

type_props(living_room, [inherit(place)]).

type_props(pantry, [
   volume_capacity = 1000,
   nouns(closet),
   nominals(kitchen),
   desc("You're in a dark kitchen pantry."),
   dark = t,
   inherit(place)
]).

% Things

type_props(brklamp,
   inherit(broken),
   name = ("possibly broken lamp"),
   effect( switch(on), print_(_Agent, "Switch is flipped")),
   effect(hit, [print_("Hit brklamp"), setprop($self, inherit(broken))]),
   inherit(lamp)).


type_props(screendoor, [
   % see DM4
   door_to(kitchen),
   door_to(garden),
   opened = f,
   inherit(door)
]).


:- push_to_state([
 type_props(broken, [
    name = ("definately broken"),
    effect( switch(on), true),
    effect( switch(off), true),
    can(switch),
    nominals(broken),
    adjs($class),
    adjs([dented])
 ]),

 type_props(mushroom, [
   % See DM4
   name = ("speckled mushroom"),
   % singular,
   food,
   nouns(([mushroom, fungus, toadstool])),
   adjs([speckled]),
   % initial(description used until initial state changes)
   initial("A speckled mushroom grows out of the sodden earth, on a long stalk."),
   % description(examination description)
   desc("The mushroom is capped with blotches, and you aren't at all sure it's not a toadstool."),
   can(eat),
   % before(VERB, CODE) -- Call CODE before default code for VERB.
   %  If CODE succeeds, don"t call VERB.
   before(eat, (random100 =< 30, die("It was poisoned!"); "yuck!")),
   after(take,
    (initial, "You pick the mushroom, neatly cleaving its thin stalk."))]),

 type_props(door, [
   % ~can(take),
    inherit(furnature),
    can(open),
    can(close),
    (opened = f),
    nouns($class),
    inherit(fully_corporial)]),

 type_props(unthinkable, [
   ~can(examine),
    adjs($class),
    class_desc(["kind is normally unthinkable"])]),

 type_props(thinkable, [
    can(examine),
    % nouns($self),
    adjs($class),
    class_desc(["kind is normally thinkable"])]),

 type_props(noncorporial, [
    can(examine)=f,
   ~can(touch),
    inherit(thinkable),
    adjs($class),
   ~inherit(fully_corporial),
    class_desc(["direct inheriters are completely noncorporial"])]),

 type_props(only_conceptual, [
   adjs($class),
   inherit(noncorporial),
   inherit(thinkable),
   class_desc(["kind is only conceptual"])]),

 type_props(partly_noncorporial, [
   inherit(fully_corporial),
   adjs($class),
   inherit(noncorporial),
   class_desc(["kind is both partly corporial and non-corporial"])]),

 type_props(fully_corporial, [
   can(touch),
   can(examine),
   inherit(thinkable),
   cleanliness = clean,
   adjs($class),
   class_desc(["kind is corporial"])]),

 type_props(moveable, [
    can(examine),
    adjs(physical),
    adjs($class),
    traits(object),
    can(move),
    inherit(fully_corporial),
    inherit(thinkable),
    class_desc(["kind is an Movable Object"])]),

 type_props(untakeable, [
    adjs($class),
   ~can(take),
    class_desc(["kind is an Immobile Object"])]),


 type_props(furnature, [
   can(examine),
   nouns(furnature),
   inherit(untakeable),
   inherit(fully_corporial),
   inherit(surface),
   inherit(thinkable),
   adjs(physical),
   class_desc(["kind is furnature"])]),


 % People
 type_props(floyd, [name = ("Floyd the robot"), powered = f, inherit(autonomous), inherit(robot)]),

 type_props(telnet, [adjs(remote), inherit(player), nouns([player])]),
 type_props(player, [name = ($self),
   % 1 = look at object  2 = glance at child_list 3 = glance at grandchildren
   model_depth = 3, % how much of the model to get
   % 5 = save game |  4 = debug | 3 = look at Obj | 2 =  | 1 = basic fun info
   % prop_depth = 3, % what prop level to get
   % Basic fun type_props
   inherit(character),
   inherit(autoscan),
   look_depth = 3,
   user_mode = 2, % 1 = fun-only, normal, debug
   access_level = admin, % guest, user, admin, wizard
   inherit(console), inherit(humanoid)]),
 type_props(console, [adjs(physical), traits([console]), nouns([player])]),

 % p(a, b, c).
 % c1_p_a1(a), c1_p_a1(b), c1_p_a1(c):-



 type_props(humanoid, [
   knows_verbs(eat),
   volume = 50, % liters  (water is 1 kilogram per liter)
   mass = 50, % kilograms
   inherit(character),
   inherit(memorizer),


   % players use power but cant be powered down
   can( switch(off), f), powered = t
  ]),

 type_props(autonomous, [inherit(autoscan), inherit(impulsive),
   class_desc(["like Floyd the robot will, instances will automatically use its planner
        about planning to decide on what to do"])]),

 type_props(decider, [
       class_desc(["agents of this type/class call decide_action/3 hooks (and per plugin)"])]),

  type_props(nomicmu_plugin, [
        nouns(plugin),
        nouns(unthinkable),
        prefix ='$error'("required config var"),
        class_desc(["Nomicmu plugin"])]),

  type_props(decider_plugin, [
        traits(decider),
        inherit(nomicmu_plugin),
        class_desc(["plugins that contain decide_action hooks"])]),

 type_props(autoscan, [
      adjs(perceptive),
        inherit(autolook),
           class_desc(["Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)"])]),

 type_props(autolook, [
      adjs($class),
          class_desc(["When entering a new area the Agent will automatically
            get an overview of the env (without purposeful looking)"])]),


 type_props(character, [
   has_rel(worn_by),
   has_rel(held_by),
   % overridable defaults
   model_depth = 3,
   mass = 50, volume = 50, % liters  (water is 1 kilogram per liter)
   has_sense(see),
 
    inherit(perceptq),
    %inherit(no_perceptq),
    
   inherit(memorizer),
   inherit(actor),
   inherit(autoscan),
   inherit(partly_noncorporial)
  ]),

 type_props(actor, [
   knows_verbs(examine),
   inherit(partly_noncorporial)
  ]),

 type_props(robot, [
  ~knows_verbs(eat),
   inherit(autonomous),
   emitting(see, light),
   volume = 50, mass = 200, % density 4 % kilograms per liter
   nouns([robot]),
   adjs([metallic]),
   desc("Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever."),
   can(switch),
   iza(memorizer),
   nouns(robot),
   inherit(shiny),
   inherit(character),
   powered = t,
   % TODO: floyd should `( act3('look', $agent ,[]))` when turned back on.
   effect( switch(on), setprop($self, powered = t)),
   effect( switch(off), setprop($self, (powered= f)))
  ]),

  type_props(natural_force, [
   ~knows_verbs(eat),
   ~can(touch),
   ~has_rel(held_by),
   ~has_rel(worn_by),
    has_sense(see),
    % inherit(no_perceptq),
    inherit(noncorporial),
    inherit(actor)
   ]),


  type_props(no_perceptq, [
    ~inherit(perceptq)
   ]),

  type_props(perceptq, [
    ~inherit(no_perceptq)
   ]),

   % Places
 type_props(place, [
   volume_capacity = 10000,
   default_rel = in,
   desc = "this is a place",
   has_rel(in),
   nouns([here]),
   %inherit(($(self))),
   %adjs(local),
  ~can(move),
  ~can(take),
   oper( ( act3('discard', $agent ,[ Thing])),
    precond(h(spatial, child, $agent, Thing), ["dont have"]), % precond(Test, FailureMessage)
    body( ( act3('take', $agent,[ Thing, in, $self])))), % body(clause)
   % inherit(container),
   has_rel(fn(exit,_))
  ]),

 type_props(container, [
   default_rel = in,
   opened = f,
   can(open),
   has_rel(in),
  oper( ( act3('put', $agent ,[ Thing, in, $self])),
   precond(~getprop(Thing, inherit(liquid)), ["liquids would spill out"]), % precond(Test, FailureMessage)
   body( ( act3('take', $agent,[ Thing, in, $self]))))  % body(clause)
  ]),


 type_props(bag, [
   volume_capacity = 10,
   inherit(container),
   inherit(moveable)
  ]),

 type_props(cup, [inherit(flask)]),

 type_props(flask, [
   adjs(physical),
  oper( ( act3('put', $agent ,[ Thing, in, $self])),
   % precond(Test, FailureMessage)
   precond(getprop(Thing, inherit(fully_corporial)), ["non-physical would spill out"]),
   % body(clause)
   body( ( act3('take', $agent,[ Thing, in, $self])))),
   inherit(container),
   (opened = t),
   inherit(moveable)
  ]),

 type_props(bowl, [
   inherit(uncloseable),
   inherit(flask),
   volume_capacity = 2,
   breaks_into = shards,
   cleanliness = dirty,
   name = ("porcelain bowl"),
   desc("This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.")
  ]),

 type_props(plate, [
   inherit(surface),
   inherit(moveable),
   volume_capacity = 2,
   breaks_into = shards,
 
   %name($class),
  cleanliness = dirty]),

 type_props(fireplace, [
  ~has_rel(on),
   has_rel(over),
   inherit(uncloseable),
   volume_capacity = 20,
   inherit(furnature)
  ]),

 type_props(box, [
   opened = f,
   volume_capacity = 11,
   inherit(container),
   inherit(moveable),
   inherit(cardboard)
  ]),

 type_props(crate, [
    inherit(container),
    inherit(moveable),
    volume_capacity = 13,
    inherit(wooden),
    (opened = t)
  ]),

 type_props(locker, [
    inherit(container),
    inherit(moveable),
    volume_capacity = 13,
    inherit(metal),
    opened = f
  ]),
 type_props(wooden, [
   breaks_into = splinters,
   can(burn)
  ]),

 type_props(metal, [
  ~can(burn)
  ]),

 type_props(cardboard, [
   inherit(paper)
  ]),

 type_props(paper, [
   can(burn)
  ]),

 type_props(sink, [
   cleanliness = dirty,
   inherit(uncloseable),
   inherit(flask),
   inherit(furnature),
   volume_capacity = 5
  ]),

 type_props(uncloseable, [
   opened = t,
  ~can(close),
  ~can(open),
   inherit(container)
  ]),

 type_props(cabinate, [
   inherit(container),
   inherit(furnature),
   volume_capacity = 10
  ]),

 type_props(fountain, [
   volume_capacity = 150,
   inherit(place),
   inherit(sink)
  ]),

 type_props(measurable, [adjs($class), ammount = some]),


   % shiny things are fully_corporial
 type_props(shiny, [adjs($class), inherit(moveable), inherit(fully_corporial)]),

 type_props(coins, [inherit(shiny), inherit(measurable)]),

 type_props(flour, [inherit(food), inherit(measurable)]),

 type_props(lamp, [
   name = ("shiny brass lamp"),
   powered = t,
   can(switch),
   nouns(light),
   adjs(brass),
   inherit(shiny),
   inherit(moveable),
   emitting(see, light),
   effect( switch(on), setprop($self, emitting(see, light))),
   effect( switch(off), delprop($self, emitting(see, light))),
   breaks_into = (broken_lamp)
  ]),

 type_props(broken_lamp, [
   name = ("dented brass lamp"),
   % TODO: prevent user from referring to "broken_lamp"
   nouns(light),
   traits(brass),
   inherit(broken),
   adjs(dented),
   can(switch),
   effect( switch(on), true),
   effect( switch(off), true) % calls true(S0, S1) !
  ]),

 type_props(surface, [has_rel(on), default_rel = on, adjs(physical), cleanliness=clean]),

 type_props(shelf, [inherit(surface), adjs(physical), inherit(furnature)]),

 type_props(table, [inherit(surface), adjs(physical), default_rel=on])
 ]).

 type_props(wrench, [inherit(shiny)]).

 type_props(videocamera, [
   inherit(memorizer),
   inherit(perceptq),
   inherit(memorize_perceptq),
   can(switch),
   effect( switch(on), setprop($self, powered = t)),
   effect( switch(off), setprop($self, (powered= f))),
   powered = t,
   has_sense(see),
   breaks_into = (broken_videocam)
  ]).

  type_props(broken_videocam, [~can(switch), (powered= f), inherit(videocamera)]).

% this hacks the state above
:- push_to_state([in(floyd, pantry)]).

:- push_to_state([


% Relationships

in(player, kitchen),
worn_by(watch, player),
held_by(bag, player),

in(coins, bag),
held_by(wrench, floyd),

% eng2log("A pantry exits south to a kitchen", exit(south, pantry, kitchen)),
% add_e2c_trans("?NP1 exits ?DIR to ?NP2", exit(DIR, NP1, NP2)),
exit(north, kitchen, pantry), exit(south, pantry, kitchen),
exit(down, pantry, basement), exit(up, basement, pantry),
exit(south, kitchen, garden), exit(north, garden, kitchen),
exit(west, kitchen, dining_room), exit(east, dining_room, kitchen),
exit(west, dining_room, living_room), exit(east, living_room, dining_room), 


in(shelf, pantry), % the shelf is in the pantry
in(locker, pantry), % the locker is in the  pantry
in(rock, garden),  % xformed:  in(rock_X1, garden).
% there are rocks in the garden
in( x(rock, 2), garden),    % xformed:  in(rock_X2, garden).
%in( a(rock), garden),    % xformed:  in(rock_X2, garden).
%in( a(rock), garden),    % in(rock_X3, garden).
% in({atLeast(2)}/in(a(rock), garden)).
                         %
in(fountain, garden),
in(water, fountain),
in(mushroom, garden),
in(shovel, basement), % FYI shovel has no type_props (this is a lttle test to see what happens)
in(videocamera, living_room),
in(fireplace, living_room),
in(screendoor, kitchen),
in(crate, kitchen),
in(apple, crate),
in(screendoor, garden),
in(brklamp, garden)

]).

fn_pred(attached,nw,"north west").
fn_pred(attached,nn,"north").
fn_pred(attached,ne,"north east").
fn_pred(attached,ww,"west").
fn_pred(attached,cc,"center").
fn_pred(attached,ee,"east").
fn_pred(attached,sw,"south west").
fn_pred(attached,ss,"south").
fn_pred(attached,se,"south east").

add_compass_attachments(TTT_table):- 
 forall(fn_pred(Attached,SE,Southeast),
  ( atom_concat(SE,'_subregion',Se_subregion),
  push_to_state([
    t(Attached,SE,Se_subregion,TTT_table),
    type_props(Se_subregion, [desc=Southeast,subregion])]))).

:- push_to_state([
    % subregions are physical surfaces that are attached to something
    type_props(subregion, [inherit(physical),inherit(surface),has_rel(attached)]),  
    % Blue pebbles are pebbles that are the color blue
    type_props(blue_pebble, [pebble,color(blue)]),
    % A red pebble is type of pebble that is the color red
    type_props(red_pebble, [pebble,color(red)]),
    % Pebbles are takeable rocks
    type_props(pebble, [can(take),rock])]).

% TTT world
ttt_world(TTT_table,Where,Red,Blue):-
  push_to_state([
   % TTT_table is a class of table describable as Tic-Tac-Toe Table
     type_props(TTT_table, [desc="Tic-Tac-Toe Table",table]),
   % There is a TTT table in Where
   in(TTT_table, Where)]),
  % There are 5 blue pebbles held by the Blue player
  forall(between(1,5,N),push_to_state(held_by( x(blue_pebble, N), Blue))),
  % There are 5 red pebbles held by the Red player
  forall(between(1,5,N),push_to_state(held_by( x(red_pebble, N), Red))),
    % shall we support this syntax?
    % forall(between(1,5,N),push_to_state(held_by( x([pebble,color(pink)], N), Red))),
  % add subregion attachments to TTT_table
  add_compass_attachments(TTT_table),!.


:- ttt_world(ttt_table,kitchen,player,floyd).

:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).

extra_decl(T, PP):- extra_decl0(T, P), correct_props(T, P, PP).
extra_decl0(_T, _P):-  fail.
% extra_decl0(T, P):-  member(type_props(T, P), [  ]).

:- op(0, xfx, type_props).

%:- listing(istate).
%:- istate(IState), sort(IState, SIState), reverse(SIState, RIState), pprint(RIState, always).

