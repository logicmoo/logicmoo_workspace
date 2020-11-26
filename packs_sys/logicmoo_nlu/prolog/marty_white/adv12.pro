/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */



% Marty's Prolog Adventure Prototype
% Copyright (C) 2004 Marty White under the GNU GPL
% Main file.

security_of(_Agent, admin) :- true.  % Potential security_of hazzard.
security_of(_Agent, wizard) :- true. % Potential to really muck up game.

extra.

:- ensure_loaded('poor_bugger.pl').
:- use_module('adv_io.pl').
:- ensure_loaded('adv_util.pl').
:- ensure_loaded('adv_debug.pl').

% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back.  The state of the simulation consists of:
%   object properties
%   object relations
%   percept queues for agents
%   memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
%   ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
%   store initial state as clauses which are collected up and put into a list,
%     like the operators are, to provide proper prolog variable management.

:- op(900, xfx, props).
:- op(300, fx, ~).


istate([
  % Relationships

  related(exit(south), pantry, kitchen), % pantry exits south to kitchen
  related(exit(north), kitchen, pantry),
  related(exit(down), pantry, basement),
  related(exit(up), basement, pantry),
  related(exit(south), kitchen, garden),
  related(exit(north), garden, kitchen),
  related(exit(east), kitchen, dining_room),
  related(exit(west), dining_room, kitchen),
  related(exit(north), dining_room, living_room),
  related(exit(east), living_room, dining_room),
  related(exit(south), living_room, kitchen),
  related(exit(west), kitchen, living_room),

  related(in, shelf, pantry), % shelf is in pantry
  related(on, lamp, table),
  related(in, floyd, pantry),
  related(held_by, wrench, floyd),
  related(in, rock, garden),
  related(in, mushroom, garden),
  related(in, player, kitchen),
  related(worn_by, watch, player),
  related(held_by, bag, player),
  related(in, coins, bag),
  related(in, table, kitchen),
  related(on, box, table),
  related(in, bowl, box),
  related(in, flour, bowl),
  related(in, shovel, basement),
  related(in, videocamera, living_room),
  related(in, screendoor, kitchen),
  related(in, screendoor, garden),

  % People

  character props [has_rel(held_by), has_rel(worn_by)],

  props(floyd, [
    inherit(character),
    agent_type(autonomous),
    emits_light,
    volume(50), mass(200), % density(4) % kilograms per liter
    name('Floyd the robot'),
    nouns(robot),
    adjs(metallic),
    desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),
    can_be(switched(OnOff), t),
    on,
    % TODO: floyd should `look` when turned back on.
    effect(switch(On), setprop($(self), state(on, t))),
    effect(switch(Off), setprop($(self), state(on, f))),
    end_of_list
  ]),
  props(player, [
    inherit(character),
    agent_type(console),
    volume(50), % liters     (water is 1 kilogram per liter)
    mass(50), % kilograms
    can_eat
  ]),

  % Places

  place props [can_be(move, f), has_rel(in)],

  props(basement, [
    inherit(place),
    desc('This is a very dark basement.'),
    dark
  ]),
  props(dining_room, [inherit(place)]),
  props(garden,    [
    inherit(place),
    % goto(Agent, walk, dir, result) provides special handling for going in a direction.
    goto(Agent, walk, up, 'You lack the ability to fly.'),
    effect(goto(Agent, walk, _, north), getprop(screendoor, open)),
    oper(/*garden, */ goto(Agent, walk, _, north),
         % precond(Test, FailureMessage)
         precond(getprop(screendoor, open), ['you must_mw open the door first']),
         % body(clause)
         body(inherited)
    ),
    % cant_go provides last-ditch special handling for Go.
    cant_goto(Agent, walk, 'The fence surrounding the garden is too tall and solid to pass.')
  ]),
  props(kitchen,   [inherit(place)]),
  props(living_room, [inherit(place)]),
  props(pantry, [
    inherit(place),
    nouns(closet),
    nominals(kitchen),
    desc('You\'re in a dark pantry.'),
    dark
  ]),

  % Things

  props(bag, [
    has_rel(in),
    volume_capacity(10),
    dark
  ]),
  props(bowl, [
    has_rel(in),
    volume_capacity(2),
    fragile(shards),
    name('porcelain bowl'),
    desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  props(box, [
    has_rel(in),
    volume_capacity(15),
    fragile(splinters),
    %openable,
    closed(true),
    %lockable,
    locked(fail),
    dark
  ]),
  coins props [shiny],
  flour props [edible],
  props(lamp, [
    name('shiny brass lamp'),
    nouns(light),
    nominals(brass),
    adjs(shiny),
    shiny,
    can_be(switched(OnOff), t),
    state(on, t),
    emits_light,
    effect(switch(On), setprop(Agent, emits_light)),
    effect(switch(Off), delprop(Agent, emits_light)),
    fragile(broken_lamp)
  ]),
  broken_lamp props [
    name('dented brass lamp'),
    % TODO: prevent user from referring to 'broken_lamp'
    nouns(light),
    nominals(brass),
    adjs(dented),
    can_be(switched(OnOff), t)
    %effect(switch(On), true),
    %effect(switch(Off), true) % calls true(S0, S1) !
  ],
  mushroom props [
    % See DM4
    name('speckled mushroom'),
    singular,
    nouns([mushroom, fungus, toadstool]),
    adjs([speckled]),
    % initial(description used until initial state changes)
    initial('A speckled mushroom grows out of the sodden earth, on a long stalk.'),
    % description(examination description)
    desc('The mushroom is capped with blotches, and you aren\'t at all sure it\'s not a toadstool.'),
    edible,
    % before(VERB, CODE) -- Call CODE before default code for VERB.
    %                      If CODE succeeds, don't call VERB.
    before(eat, (random(100) =< 30, die('It was poisoned!'); 'yuck!')),
    after(take,
          (initial, 'You pick the mushroom, neatly cleaving its thin stalk.'))
  ],
  screendoor props [
    can_be(move, f),
    % see DM4
    door_to(garden),
    %openable
    closed(true)
  ],
  props(shelf , [has_rel(on), can_be(move, f)]),
  props(table , [has_rel(on), has_rel(under)]),
  wrench props [shiny],
  videocamera props [
    agent_type(recorder),
    can_be(switched(OnOff), t),
    effect(switch(On), setprop(Agent, on)),
    effect(switch(Off), delprop(Agent, on)),
    fragile(broken_videocam)
  ],
  broken_videocam props [can_be(switched(OnOff), t)],

  end_of_list
]):- On=on, Off = off, OnOff = on, Agent= ($(self)).

% Some Inform properties:
%   light - rooms that have light in them
%   edible - can be eaten
%   static - can't be taken or moved
%   scenery - assumed to be in the room description (implies static)
%   concealed - obscured, not listed, not part of 'all', but there
%   found_in - lists places where scenery objects are seen
%   absent - hides object entirely
%   clothing - can be worn
%   worn - is being worn
%   container
%   open - container is open (must_mw be open to be used. there is no "closed").
%   openable - can be opened and closed
%   capacity - number of objects a container or supporter can hold
%   locked - cannot be opened
%   lockable, with_key
%   enterable
%   supporter
%   article - specifies indefinite article ('a', 'le') 
%   cant_go
%   daemon - called each turn, if it is enabled for this object
%   description
%   inside_description
%   invent - code for inventory listing of that object
%   list_together - way to handle "5 fish"
%   plural - pluralized-name if different from singular
%   when_closed - description when closed
%   when_open - description when open
%   when_on, when_off - like when_closed, etc.
% Some TADS properties:
%   thedesc
%   pluraldesc
%   is_indistinguishable
%   is_can_sense(Sense, vantage)
%   is_reachable(actor)
%   valid(verb) - is object visible, reachable, etc.
%   verification(verb) - is verb logical for this object
% Parser disambiguation:
%   eliminate objs not visible, reachable, etc.
%   check preconditions for acting on a candidate object

% TODO: change agent storage into a term:
%   mind(AgentName, AgentType, History, Model, Goals /*, ToDo*/)
create_agent(Agent, AgentType, S0, S2) :-
  % As events happen, percepts are entered in the percept queue of each agent.
  % Each agent empties their percept queue as they see fit.
  declare(perceptq(Agent, []), S0, S1),
  % Most agents store memories of percepts, world model, goals, etc.
  declare(memories(Agent, [
    timestamp(0),
    model([]),
    goals([]),
    todo([]),
    agent(Agent),
    agent_type(AgentType)
  ]), S1, S2).

% -----------------------------------------------------------------------------
% State may be implemented differently in the future (as a binary tree or
% hash table, etc.), but for now is a List.  These (backtrackable) predicates
% hide the implementation:
% assert/record/declare/memorize/think/associate/know/retain/affirm/avow/
%   insist/maintain/swear/posit/postulate/allege/assure/claim/proclaim
% retract/erase/forget/un-declare/unthink/repress/supress
% retrieve/remember/recall/ask/thought/think-of/reminisc/recognize/review/
%   recollect/remind/look-up/research/establish/testify/sustain/attest/certify/
%   verify/prove
% simulation: declare/undeclare/declared
% perception:
% memory: memorize/forget/thought

% Like select_from, but always succeeds, for use in deleting.
select_always(Item, List, ListWithoutItem) :-
  select_from(Item, List, ListWithoutItem),
  !.
select_always(_Item, ListWithoutItem, ListWithoutItem).

% Like select_from, but with a default value if not found in List..
%select_default(Item, _DefaultItem, List, ListWithoutItem) :-
%  select_from(Item, List, ListWithoutItem).
%select_default(DefaultItem, DefaultItem, ListWithoutItem, ListWithoutItem).

% Manipulate simulation state
declare(Fact, State, NewState) :- append([Fact], State, NewState).
undeclare(Fact, State, NewState)   :- select_from(Fact, State, NewState).
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).
declared(Fact, State) :- member(Fact, State).

% Retrieve Prop.
getprop(Object, Prop, State) :-
  declared(props(Object, PropList), State),
  member(Prop, PropList).
getprop(Object, Prop, State) :-
  declared(props(Object, PropList), State),
  member(inherit(Delegate), PropList),
  getprop(Delegate, Prop, State).

% Replace or create Prop.
setprop(Object, Prop, S0, S2) :-
  undeclare(props(Object, PropList), S0, S1),
  select_always(Prop, PropList, PropList2),
  append([Prop], PropList2, PropList3),
  declare(props(Object, PropList3), S1, S2).
setprop(Object, Prop, S0, S2) :-
  declare(props(Object, [Prop]), S0, S2).

% Remove Prop.
delprop(Object, Prop, S0, S2) :-
  undeclare(props(Object, PropList), S0, S1),
  select_from(Prop, PropList, NewPropList),
  declare(props(Object, NewPropList), S1, S2).

% Manipulate simulation percepts
queue_percept(Agent, Event, S0, S2) :-
  select_from(perceptq(Agent, Queue), S0, S1),
  append(Queue, [Event], NewQueue),
  append([perceptq(Agent, NewQueue)], S1, S2).

queue_event(Event, S0, S2) :-
  queue_percept(player, Event, S0, S1),
  queue_percept(floyd, Event, S1, S2).

queue_local_percept(Agent, Event, Places, S0, S1) :-
  member(Where, Places),
  related(open_traverse, Agent, Where, S0),
  queue_percept(Agent, Event, S0, S1).
queue_local_percept(_Agent, _Event, _Places, S0, S0).

queue_local_event(Event, Places, S0, S2) :-
  queue_local_percept(player, Event, Places, S0, S1),
  queue_local_percept(floyd , Event, Places, S1, S2).

% A percept or event:
%   - is a logical description of what happened
%   - includes English or other translations
%   - may be queued for zero, one, many, or all agents.
%   - may have a timestamp
% queue_percpt(Agent, [Logical, English|_], S0, S9).
%   where Logical is always first, and other versions are optional.
%   Logical should be a term, like sees(Thing).
%   English should be a list.

% Inform notation
%   'c'        character)
%   "string"   string
%   "~"        quotation mark
%   "^"        newline
%   @          accent composition, variables 00 thru 31
%   \          line continuation
% Engish messages need to be printable from various perspectives:
%   person (1st/2nd/3rd), tense(past/present)
%   "You go south." / "Floyd wanders south."
%       {'$agent $go $1', ExitName }
%       { person(Agent), tense(go, Time), ExitName, period }
%       {'$p $t $w', Agent, go, ExitName}
%   "You take the lamp." / "Floyd greedily grabs the lamp."
%       Agent=floyd, {'%p quickly grab/T %n', Agent, grab, Thing }
%               else {'%p take/T %n', Agent, take, Thing }
%   %p  Substitute parameter as 1st/2nd/3rd person ("I"/"you"/"Floyd").
%         Implicit in who is viewing the message.
%         Pronouns: gender, reflexive, relative, nominative, demonstratve...?
%   %n  Substitute name/description of parameter ("the brass lamp").
%   /T  Modify previous word according to tense ("take"/"took").
%         Implicit in who is viewing the message?  Context when printed?
%   /N  Modify previous word according to number ("coin"/"coins").
%         What number?
%   %a  Article - A or An (indefinite) or The (definite) ?
%
%  I go/grab/eat/take
%  you go/grab/eat/take
%  she goes/grabs/eats/takes
%  floyd goes/grabs/eats/takes
%
%  eng(subject(Agent), 'quickly', verb(grab, grabs), the(Thing))
%  [s(Agent), 'quickly', v(grab, grabs), the(Thing)]

capitalize([First|Rest], [Capped|Rest]) :-
  capitalize(First, Capped).
capitalize(Atom, Capitalized) :-
  atom(Atom), % [] is an atom
  downcase_atom(Atom, Lower),
  atom_chars(Lower, [First|Rest]),
  upcase_atom(First, Upper),
  atom_chars(Capitalized, [Upper|Rest]).

% compile_eng(Context, Atom/Term/List, TextAtom).
%  Compile Eng terms to ensure subject/verb agreement:
%  If subject is agent, convert to 2nd person, else use 3rd person.
%  Context specifies agent, and (if found) subject of sentence.
compile_eng(Context, subj(Agent), Person) :-
  member(agent(Agent), Context),
  member(person(Person), Context).
compile_eng(Context, subj(Other), Compiled) :-
  compile_eng(Context, Other, Compiled).
compile_eng(Context, Agent, Person) :-
  member(agent(Agent), Context),
  member(person(Person), Context).
compile_eng(Context, person(Second, _Third), Compiled) :-
  member(subj(Agent), Context),
  member(agent(Agent), Context),
  compile_eng(Context, Second, Compiled).
compile_eng(Context, person(_Second, Third), Compiled) :-
  compile_eng(Context, Third, Compiled).
compile_eng(Context, cap(Eng), Compiled) :-
  compile_eng(Context, Eng, Lowercase),
  capitalize(Lowercase, Compiled).
compile_eng(_Context, silent(_Eng), '').
compile_eng(_Context, [], '').
compile_eng(Context, [First|Rest], [First2|Rest2]) :-
  compile_eng(Context, First, First2),
  compile_eng(Context, Rest, Rest2).
compile_eng(_Context, Atom, Atom).

nospace(_, ', ').
nospace(_, ';').
nospace(_, ':').
nospace(_, '.').
nospace(_, '?').
nospace(_, '!').
nospace(_, '\'').
nospace('\'', _).
nospace(_, '"').
nospace('"', _).
nospace(_, Letter) :- system:char_type(Letter, space).
nospace(Letter, _) :- char_type(Letter, space).

no_space_words('', _).
no_space_words(_, '').
no_space_words(W1, W2) :-
  atomic(W1),
  atomic(W2),
  atom_chars(W1, List),
  last(List, C1),
  atom_chars(W2, [C2|_]),
  nospace(C1, C2).

insert_spaces([W], [W]).
insert_spaces([W1, W2|Tail1], [W1, W2|Tail2]) :-
  no_space_words(W1, W2),
  !,
  insert_spaces([W2|Tail1], [W2|Tail2]).
insert_spaces([W1, W2|Tail1], [W1, ' ', W3|Tail2]) :-
  insert_spaces([W2|Tail1], [W3|Tail2]).
insert_spaces([], []).

make_atomic(Atom, Atom) :-
  atomic(Atom), !.
make_atomic(Term, Atom) :-
  term_to_atom(Term, Atom).

eng2txt(Agent, Person, Eng, Text) :-
  % Find subject, if any.
  findall(subj(Subject), call(findterm(subj(Subject), Eng)), Context),
  % Compile recognized structures.
  maplist(compile_eng([agent(Agent), person(Person)|Context]), Eng, Compiled),
  % Flatten any sub-lists.
  flatten(Compiled, FlatList),
  % Convert terms to atom-strings.
  findall(Atom, (member(Term, FlatList), make_atomic(Term, Atom)), AtomList),
  findall(Atom2, (member(Atom2, AtomList), Atom2\=''), AtomList2),
  % Add spaces.
  bugout('insert_spaces(~w)~n', [AtomList2], printer),
  insert_spaces(AtomList2, SpacedList),
  % Return concatenated atoms.
  concat_atom(SpacedList, Text).
eng2txt(_Agent, _Person, Text, Text).

%portray(ItemToPrint) :- print_item_list(ItemToPrint).  % called by print.

list2eng([], ['<nothing>']).
list2eng([Single], [Single]).
list2eng([Last2, Last1], [Last2, 'and', Last1]).
list2eng([Item|Items], [Item, ', '|Tail]) :-
  list2eng(Items, Tail).

prop2eng( Obj, emits_light, ['The', Obj, 'is glowing.']).
prop2eng(_Obj, edible,     ['It looks tasty!']).
prop2eng(_Obj, fragile(_), ['It looks fragile.']).
prop2eng(_Obj, closed(true), ['It is closed.']).
prop2eng(_Obj, closed(fail), ['It is open.']).
prop2eng(_Obj, open(fail), ['It is closed.']).
prop2eng(_Obj, open(true), ['It is open.']).
prop2eng(_Obj, open,       ['It is open.']).
prop2eng(_Obj, closed,     ['It is closed.']).
prop2eng(_Obj, locked,     ['It is locked.']).
prop2eng(_Obj, shiny,      ['It\'s shiny!']).
prop2eng(_Obj, _Prop,      []).

proplist2eng(_Obj, [], []).
proplist2eng(Obj, [Prop|Tail], Text) :-
  prop2eng(Obj, Prop, Text1),
  proplist2eng(Obj, Tail, Text2),
  append(Text1, Text2, Text).

logical2eng(_CAgent, exits_are(At, Here, Exits), 
            [cap(At), 'the', subj(Here), ', exits are', ExitText, '.', '\n']) :-
  list2eng(Exits, ExitText).

logical2eng(Agent, can_sense_from_here(Agent, At, Here, Sense, Nearby),
            ['From', At, cap(subj(Here)), cap(subj(Agent)), ',',  'can', person(Sense, es(Sense)), ':', SeeText, '.']) :-
  findall(X, (member(X, Nearby), X\=Agent), OtherNearby),
  list2eng(OtherNearby, SeeText).

logical2eng(Agent, rel_to(held_by, Items),
            [cap(subj(Agent)), person(are, is), 'carrying:'|Text]) :-
  list2eng(Items, Text).
logical2eng(Agent, sense_childs(Agent, _Sense, _Parent, _At, []), []).
logical2eng(Agent, sense_childs(Agent, Sense, Parent, At, List),
            [cap(subj(Agent)), At, cap(subj(Parent)), person(Sense, es(Sense)), ':'|Text]) :-
  list2eng(List, Text).
logical2eng(_Agent, moved(What, From, At, To),
            [cap(subj(What)), 'moves from', From, 'to', At, To]).
logical2eng(_Agent, transformed(Before, After), [Before, 'turns into', After, .]).
logical2eng(_Agent, destroyed(Thing), [Thing, 'is destroyed.']).
logical2eng(Agent, sense_props(Agent, Sense, Object, PropList),
            [cap(subj(Agent)), person(Sense, es(Sense)), Desc, '.'|PropDesc] ) :-
  member(name(Desc), PropList),
  proplist2eng(Object, PropList, PropDesc).
logical2eng(Agent, sense_props(Agent, Sense, Object, PropList),
            [cap(subj(Agent)), person(Sense, es(Sense)), 'a', Object, '.'|PropDesc] ) :-
  proplist2eng(Object, PropList, PropDesc).
logical2eng(_Agent, say(Speaker, Eng), [cap(subj(Speaker)), ': "', Text, '"']) :-
  eng2txt(Speaker, 'I', Eng, Text).
logical2eng(_Agent, talk(Speaker, Audience, Eng),
    [cap(subj(Speaker)), 'says to', Audience, ', "', Text, '"']) :-
  eng2txt(Speaker, 'I', Eng, Text).
logical2eng(_Agent, time_passes, ['Time passes.']).
logical2eng(_Agent, failure(Action), ['Action failed:', Action]).
logical2eng(_Agent, Logical, ['percept:', Logical]).

percept2txt(Agent, [_Logical, English|_], Text) :-
  eng2txt(Agent, you, English, Text).
percept2txt(Agent, [Logical|_], Text) :-
  logical2eng(Agent, Logical, Eng),
  eng2txt(Agent, you, Eng, Text).

the(State, Object, Text) :-
  getprop(Object, name(D), State),
  atom_concat('the ', D, Text).

an(State, Object, Text) :-
  getprop(Object, name(D), State),
  atom_concat('a ', D, Text).

num(_Singular, Plural, [], Plural).
num(Singular, _Plural, [_One], Singular).
num(_Singular, Plural, [_One, _Two|_Or_More], Plural).

expand_english(State, the(Object), Text) :-
  the(State, Object, Text).
expand_english(State, an(Object), Text) :-
  an(State, Object, Text).
expand_english(_State, num(Sing, Plur, List), Text) :-
  num(Sing, Plur, List, Text).
expand_english(_State, [], '').
expand_english(State, [Term|Tail], [NewTerm|NewTail]) :-
  expand_english(State, Term, NewTerm),
  expand_english(State, Tail, NewTail).
expand_english(_State, Term, Term).

% -----------------------------------------------------------------------------
               
subrelation(in, child).
subrelation(on, child).
subrelation(worn_by, child).
subrelation(held_by, child).

has_rel(At, X, State) :-
  getprop(X, has_rel(At), State).
has_rel(At, X, State) :-
  getprop(X, has_rel(Specific), State),
  subrelation(Specific, At).

related(At, X, Y, State) :- declared(t(At, X, Y), State).
related(child, X, Y, State) :- subrelation(At, child), related(At, X, Y, State).
related(descended, X, Z, State) :-
  related(child, X, Z, State).
related(descended, X, Z, State) :-
  related(child, Y, Z, State),
  related(descended, X, Y, State).
related(open_traverse, X, Z, State) :-
  related(child, X, Z, State).
related(open_traverse, X, Z, State) :-
  related(child, Y, Z, State),
  \+ is_closed(Y, State),
  related(open_traverse, X, Y, State).
related(inside, X, Z, State) :- related(in, X, Z, State).
related(inside, X, Z, State) :- related(in, Y, Z, State),
                               related(descended, X, Y, State).
related(exit(Out), Inner, Outer, State) :- in_out(In,Out),
  related(child, Inner, Outer, State),
  has_rel(In, Inner, State),
  has_rel(child, Outer, State),
  \+ is_closed(Inner, State).
related(exit(Off), Inner, Outer, State) :- on_off(On,Off),
  related(child, Inner, Outer, State),
  has_rel(On, Inner, State),
  has_rel(child, Outer, State).
related(exit(Escape), Inner, Outer, State) :- escape_rel(Escape),
  related(child, Inner, Outer, State),
  has_rel(child, Inner, State),
  has_rel(child, Outer, State).

in_out(in,out).
on_off(on,off).
escape_rel(escape).

is_prop_public(P) :-
  member(P, [has_rel(_),
     emits_light, edible, name(_), desc(_), fragile(_),
             can_be(move, f), openable, open, closed(_), lockable, locked, locked(_),
             shiny]).

related_with_prop(At, Object, Place, Prop, State) :-
  related(At, Object, Place, State),
  getprop(Object, Prop, State).

is_closed(Object, State) :-
  getprop(Object, closed(true), State).
%  getprop(Object, openable, State),
%  \+ getprop(Object, open, State).


can_sense(visually, Agent, State) :-
  open_traverse(Agent, Here, State),
  (getprop(Here, dark, State) ->
   related_with_prop(open_traverse, _Obj, Here, emits_light, State);
    true).

in_scope(Thing, Agent, State) :-
  open_traverse(Agent, Here, State),
  (Thing=Here;  open_traverse(Thing, Here, State)).

can_sense(Sense, Thing, Agent, State) :-
  can_sense(Sense, Agent, State),
  open_traverse(Agent, Here, State),
  (Thing=Here;  open_traverse(Thing, Here, State)).

touchable(Thing, Agent, State) :-
  related(child, Agent, Here, State), % can't reach out of boxes, etc.
  (Thing=Here;  open_traverse(Thing, Here, State)).

moveto(Object, At, Dest, Vicinity, Msg, State, S9) :-
  undeclare(related(_, Object, Here), State, VoidState),
  declare(related(At, Object, Dest), VoidState, S2),
  queue_local_event([moved(Object, Here, At, Dest), Msg], Vicinity, S2, S9).

moveallto([], _R, _D, _V, _M, S, S).
moveallto([Object|Tail], Relation, Destination, Vicinity, Msg, S0, S2) :-
  moveto(Object, Relation, Destination, Vicinity, Msg, S0, S1),
  moveallto(Tail, Relation, Destination, Vicinity, Msg, S1, S2).

disgorge(Container, At, Here, Vicinity, Msg, S0, S9) :-
  findall(Inner,  related(child, Inner, Container, S0), Contents),
  bugout('~p contained ~p~n', [Container, Contents], general),
  moveallto(Contents, At, Here, Vicinity, Msg, S0, S9).
disgorge(_Container, _At, _Here, _Vicinity, _Msg, S0, S0).

thrown(Thing, _Target, At, Here, Vicinity, S0, S9) :-
  getprop(Thing, fragile(Broken), S0),
  bugout('object ~p is fragile~n', [Thing], general),
  undeclare(related(_, Thing, _), S0, S1),
  declare(related(At, Broken, Here), S1, S2),
  queue_local_event([transformed(Thing, Broken)], Vicinity, S2, S3),
  disgorge(Thing, At, Here, Vicinity, 'Something falls out.', S3, S9).
thrown(Thing, _Target, At, Here, Vicinity, S0, S9) :-
  moveto(Thing, At, Here, Vicinity, 'Thrown.', S0, S9).

hit(Target, _Thing, Vicinity, S0, S9) :-
  getprop(Target, fragile(Broken), S0),
  bugout('target ~p is fragile~n', [Target], general),
  undeclare(related(At, Target, Here), S0, S1),
  queue_local_event([transformed(Target, Broken)], Vicinity, S1, S2),
  declare(related(At, Broken, Here), S2, S3),
  disgorge(Target, At, Here, Vicinity, 'Something falls out.', S3, S9).
hit(_Target, _Thing, _Vicinity, S0, S0).

% drop -> move -> touch
subsetof(touch, touch).
subsetof(move, touch).
subsetof(drop, move).
subsetof(eat,  touch).
subsetof(hit,  touch).
subsetof(put,  drop).
subsetof(give, drop).
subsetof(take, move).
subsetof(throw, drop).
subsetof(open, touch).
subsetof(close, touch).
subsetof(lock, touch).
subsetof(unlock, touch). 
 
subsetof(examine, examine).

% proper subset - C may not be a subset of itself.
psubsetof(A, B) :- subsetof(A, B).
psubsetof(A, C) :-
  subsetof(A, B),
  subsetof(B, C).

anonmous_verb(Verb):-
 member(Verb, [agent, create, delprop, destroy, echo, quit, memory, model, path, properties, setprop, state, trace, notrace, whereami, whereis, whoami]).

action_agent_thing(Action, Verb, Agent, Thing):-
  Action=..[Verb,Agent|Args], \+ anonmous_verb(Verb), !,
  (Args=[Thing]->true;Thing=_),!.

action_agent_verb_subject_prep_object(Action, Agent, Verb, Thing, At, Thing2):-
  Action=..[Verb,Agent, Thing|Args], \+ anonmous_verb(Verb), !,
  preposition(_,At),
  append(_,[Thing2],Args).

reason2eng(cant(sense(visually, _It)),      'You can''t see that here.').
reason2eng(cant(reach(_It)),    'You can''t reach it.').
reason2eng(cant(manipulate(self)), 'You can''t manipulate yourself like that.').
reason2eng(alreadyhave(It),     ['You already have the', It, '.']).
reason2eng(mustgetout(_It),     'You must_mw get out/off it first.').
reason2eng(self_relation(_It),  'Can\'t put thing inside itself!').
reason2eng(moibeus_relation(_, _), 'Topological error!').
reason2eng(toodark,             'It''s too dark to see!').
reason2eng(mustdrop(_It),       'You will have to drop it first.').
reason2eng(can_be(_It, move, f),      'Sorry, it\'s immobile.').
reason2eng(cantdothat,          'Sorry, you can\'t do that.').
reason2eng(R, R).

cant( Action, ~in_scope(Thing, Agent, State), State) :-
  action_agent_thing(Action, Verb, Agent, Thing),
  psubsetof(Verb, _),
  \+ in_scope(Thing, Agent, State).
cant( Action, cant(sense(Sense, Thing)), State) :-
  action_agent_thing(Action, Verb, Agent, Thing),
  psubsetof(Verb, examine),
  \+ can_sense(Sense, Thing, Agent, State).
cant( Action, cant(reach(Thing)), State) :-
  action_agent_thing(Action, Verb, Agent, Thing),
  psubsetof(Verb, touch),
  \+ touchable(Thing, Agent, State).

cant( Action, props(Thing,[can_be(move, f)]), State) :-
  action_agent_thing(Action, Verb, _, Thing),
  psubsetof(Verb, move),
  getprop(Thing, can_be(move, f), State).

cant( Action, musthave(Thing), State) :-
  action_agent_thing(Action, Verb, Agent, Thing),
  psubsetof(Verb, drop),
  \+  open_traverse(Thing, Agent, State).

cant( Action, cant(manipulate(self)), _) :- \+ extra,
  action_agent_thing(Action, Verb, Agent, Thing),
  Agent == Thing,
  psubsetof(Verb, touch).

cant( take(Agent, Thing), alreadyhave(Thing), State) :-
  related(descended, Thing, Agent, State).

cant( take(Agent, Thing), mustgetout(Thing), State) :-
  related(descended, Agent, Thing, State).

cant( Action, Why, S0):-   
   action_agent_verb_subject_prep_object(Action, _Agent, Verb, Thing1, _At, Thing2),
   psubsetof(Verb, drop),
   Thing1 = Thing2  -> Why = self_relation(Thing1) ;
   related(descended, Thing2, Thing1, S0) -> Why = moibeus_relation(Thing1, Thing2).

cant( look(Agent), toodark, State) :-
  % Perhaps this should return a logical description along the lines of
  %   failure(look, requisite(look, getprop(SomethingNearby, emits_light)))
  \+ can_sense(visually, Agent, State).

cant( inventory(Agent), toodark, State) :-
  \+ can_sense(visually, Agent, State).

cant( examine(Agent, Sense, _), toodark, State) :-
  \+ can_sense(Sense, Agent, State).

cant( examine(Agent, Sense, Thing), cant(sense(Sense, Thing)), State) :-
  \+ can_sense(Sense, Thing, Agent, State).

cant( goto(Agent, walk, _Relation, Object), mustdrop(Object), State) :-
  related(descended, Object, Agent, State).

cant( eat(Agent, _), cantdothat, State) :-
  \+ getprop(Agent, can_eat, State).
  

% ---- act( Action, State, NewState)
%  where the states also contain Percepts.
% In Inform, actions work in the following order:
%   game-wide preconditions
%   player preconditions
%   objects-in-vicinity react_before conditions
%   room before-conditions
%   direct-object before-conditions
%   verb
%   objects-in-vicinity react_after conditions
%   room after-conditions
%   direct-object after-conditions
%   game-wide after-conditions
% In TADS:
%   "verification" methods perferm tests only


trival_act(look(_)).
trival_act(goto(_,_,_,_)).
trival_act(examine(_,see,_,depth(2))).
trival_act(examine(_,see,_,depth(1))).

apply_act( Action, _State, _NewState):- \+ trival_act(Action),notrace((bugout(apply_act( Action), action))),fail.

apply_act( Action, State, NewState) :- 
 action_agent_thing(Action, _Verb, Agent, _Thing),
 cant( Action, Reason, State),
 % log2eng(Agent, Reason, Eng),
 queue_percept(Agent, [failure(Action, Reason)], State, NewState), !.

apply_act( Action, S0, S1) :- 
 action_agent_thing(Action, _Verb, Agent, _Thing),
 do_introspect(Action, Answer, S0),
 queue_percept(Agent, [answer(Answer), Answer], S0, S1), !.
 %player_format('~w~n', [Answer]).

apply_act( Action, State, NewState):- act( Action, State, NewState), !.

apply_act( Action, State, NewState):- fail, 
  action_agent_thing(Action, _Verb, Agent, _Thing),
  copy_term(Action,ActionG),
  related( child, Agent, Here, State),  
  % queue_local_event(spatial, [attempting(Agent, Action)], [Here], S0, S1),
  act( Action, State, S0), !,
  queue_local_event( [emoted(Agent, act, '*'(Here), ActionG)], [Here], S0, NewState).
  
apply_act( Act, State, NewState):- ((cmd_workarround(Act, NewAct) -> Act\==NewAct)), !, apply_act( NewAct, State, NewState).
apply_act( Action, State, State):- notrace((bugout(failed_act( Action), general))),!, \+ tracing.

must_act( Action, State, NewState):- dmust_tracing(apply_act( Action, State, NewState)) *-> ! ; fail.
% must_act( Action, S0, S1) :- rtrace(apply_act( Action, S0, S1)), !.
must_act( Action, S0, S1) :-
 action_agent_thing(Action, _Verb, Agent, _Thing),
 queue_percept(Agent, [failure(Action, unknown_to(Agent,Action))], S0, S1).

cmd_workarround(VerbObj, VerbObj2):-
 VerbObj=..VerbObjL,
 notrace(cmd_workarround_l(VerbObjL, VerbObjL2)),
 VerbObj2=..VerbObjL2.

cmd_workarround_l([Verb|ObjS], [Verb|ObjS2]):-
 append(ObjS2, ['.'], ObjS).
cmd_workarround_l([Verb|ObjS], [Verb|ObjS2]):- fail,
 append(Left, [L, R|More], ObjS), atom(L), atom(R),
 current_atom(Atom), atom_concat(L, RR, Atom), RR=R,
 append(Left, [Atom|More], ObjS2).
% look(Agent, Spatial) at screendoor
cmd_workarround_l([Verb, Relation|ObjS], [Verb|ObjS]):- is_ignorable(Relation), !.
% look(Agent, Spatial) at screen door
cmd_workarround_l([Verb1|ObjS], [Verb2|ObjS]):- verb_alias(Verb1, Verb2), !.

is_ignorable(Var):- var(Var),!,fail.
is_ignorable(at). is_ignorable(in). is_ignorable(to). is_ignorable(the). is_ignorable(a). is_ignorable(spatial).

verb_alias(look, examine) :- fail.


act( Action, State, NewState):-
  player_format('~Ncall ~p.~n', [act( Action, State, NewState)]), fail.

act( Action, State, NewState) :-
  cant( Action, Reason, State),
  action_agent_thing(Action, _Verb, Agent, _Thing),
  reason2eng(Reason, Eng),
  queue_percept(Agent, [failure(Action, Reason), Eng], State, NewState).

act( look(Agent), State, NewState) :-
  Sense = visually,
  related(At, Agent, Here, State),
  findall(What,
          (related(child, What, Here, State),
           can_sense(Sense, What, Agent, State)),
          %( related(descended, What, Here, State),
           %\+ (related(inside, What, Container, State),
           %    related(descended, Container, Here, State))),
          Nearby),
  findall(Direction,  related(exit(Direction), Here, _, State), Exits),
  !,
  queue_percept(Agent,
                [can_sense_from_here(Agent, At, Here, Sense, Nearby),exits_are(At, Here, Exits)],
                State, NewState).

act( inventory(Agent), State, NewState) :-
  findall(What,  related(child, What, Agent, State), Inventory),
  queue_percept(Agent, [rel_to(held_by, Inventory)], State, NewState).

act( examine(Agent, Sense, Object), S0, S2) :-
  %declared(props(Object, PropList), S0),
  findall(P, (getprop(Object, P, S0), is_prop_public(P)), PropList),
  queue_percept(Agent, [sense_props(Agent, Sense, Object, PropList)], S0, S1),
  (has_rel(At, Object, S1); At='<unrelatable>'),
  % Remember that Agent might be on the inside or outside of Object.
  findall(What,
          (  related(child, What, Object, S1), once(can_sense(Sense, What, Agent, S1))),
          Children),
  queue_percept(Agent, [sense_childs(Agent, Sense, Object, At, Children)], S1, S2).



act( goto(Agent, walk, _At, ExitName), S0, S9) :-         % go n/s/e/w/u/d/in/out
  related(child, Agent, Here, S0),
  related(exit(ExitName), Here, There, S0),
  %member(At, [*, to, at, through, thru]),
  has_rel(AtThere, There, S0),
  moveto(Agent, AtThere, There,
         [Here, There],
         [cap(subj(Agent)), person(go, goes), ExitName],
         S0, S1),
  add_look(Agent, S1, S9).
act( goto(Agent, walk, At, Room), S0, S9) :-              % go in (adjacent) room
  has_rel(At, Room, S0),
  open_traverse(Agent, Here, S0),
  related(exit(ExitName), Here, Room, S0),
  moveto(Agent, At, Room, [Room, Here],
    [cap(subj(Agent)), person(go, goes), ExitName], S0, S1),
  add_look(Agent, S1, S9).
act( goto(Agent, walk, *, Room), S0, S9) :-              % go to (adjacent) room
  has_rel(At, Room, S0),
  open_traverse(Agent, Here, S0),
  related(exit(ExitName), Here, Room, S0),
  moveto(Agent, At, Room, [Room, Here],
    [cap(subj(Agent)), person(go, goes), ExitName], S0, S1),
  add_look(Agent, S1, S9).
act( goto(Agent, walk, At, Object), S0, S2) :-            % go in/on object
  has_rel(At, Object, S0),                
  open_traverse(Agent, Here, S0),
  open_traverse(Object, Here, S0),
  \+ is_closed(Object, S0),
  moveto(Agent, At, Object, [Here],
    [subj(Agent), person(get, gets), At, the, Object, .], S0, S1),
  add_look(Agent, S1, S2).
act( goto(Agent, walk, At, Dest), S0, S1) :-
  queue_percept(Agent,
                [failure(goto(Agent, walk, At, Dest)), 'You can\'t go that way'],
                S0, S1).

%  sim(verb(args...), preconds, effects)
%    Agent is substituted for Agent.
%    preconds are in the implied context of a State.
%  In Inform, the following are implied context:
%    actor, action, noun, second
%  Need:
%    actor/agent, verb/action, direct-object/obj1, indirect-object/obj2,
%      preposition-introducing-obj2
%sim(put(Obj1, Obj2),
%    (  related(descended, Thing, Agent),
%      can_sense(Sense, Agent, Where),
%      has_rel(Relation, Where),
%      related(descended, Agent, Here)),
%    moveto(Thing, Relation, Where, [Here],
%      [cap(subj(Agent)), person('put the', 'puts a'),
%        Thing, Relation, the, Where, '.'])).

act( take(Agent, Thing), S0, S1) :-
  open_traverse(Agent, Here, S0),          % Where is Agent now?
  moveto(Thing, held_by, Agent, [Here],
    [silent(subj(Agent)), person('Taken.', [cap(Agent), 'grabs the', Thing, '.'])],
    S0, S1).
%act( get(Agent, Thing), State, NewState) :-
%  act( take(Agent, Thing), State, NewState).
act( drop(Agent, Thing), State, NewState) :-
  related(At, Agent, Here, State),
  has_rel(At, Here, State),
  moveto(Thing, At, Here, [Here],
    [cap(subj(Agent)), person('drop the', 'drops a'), Thing, '.'], State, NewState).
act( put(Agent, Thing1, Relation, Thing2), State, NewState) :-
  has_rel(Relation, Thing2, State),
  (Relation \= in ; \+ is_closed(Thing2, State)),
  touchable(Thing2, Agent, State), % what if "under" an "untouchable" thing?
  % OK, put it
  open_traverse(Agent, Here, State),
  moveto(Thing1, Relation, Thing2, [Here],
      [cap(subj(Agent)), person('put the', 'puts a'), Thing1,
          Relation, the, Thing2, '.'],
      State, NewState).
act( give(Agent, Thing, Recipient), S0, S9) :-
  has_rel(held_by, Recipient, S0),
  touchable(Recipient, Agent, S0),
  % OK, give it
  open_traverse(Agent, Here, S0),
  moveto(Thing, held_by, Recipient, [Here],
    [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
    S0, S9).
act( throw(Agent, Thing, at, Target), S0, S9) :-
  can_sense(visually, Target, Agent, S0),
  % OK, throw it
  related(At, Agent, Here, S0),
  thrown(Thing, Target, At, Here, [Here], S0, S1),
  hit(Target, Thing, [Here], S1, S9).
act( throw(Agent, Thing, ExitName), S0, S9) :-
  related(_At, Agent, Here, S0),
  related(exit(ExitName), Here, There, S0),
  has_rel(AtThere, There, S0),
  thrown(Thing, There, AtThere, There, [Here, There], S0, S9).
act( hit(Agent, Thing), S0, S9) :-
  related(_At, Agent, Here, S0),
  hit(Thing, Agent, [Here], S0, S1),
  queue_percept(Agent, [true, 'OK.'], S1, S9).
act( dig(Agent, Hole, Where, Tool), S0, S9) :-
  memberchk(Hole, [hole, trench, pit, ditch]),
  memberchk(Where, [garden]),
  memberchk(Tool, [shovel, spade]),
  open_traverse(Tool, Agent, S0),
  related(in, Agent, Where, S0),
  \+  related(_At, Hole, Where, S0),
  % OK, dig the hole.
  declare(related(in, Hole, Where), S0, S1),
  setprop(Hole, has_rel(in), S1, S2),
  setprop(Hole, can_be(move, f), S2, S3),
  declare(related(in, dirt, Where), S3, S8),
  queue_event(
    [ created(Hole, Where),
      [cap(subj(Agent)), person(dig, digs), 'a', Hole, 'in the', Where, '.']],
    S8, S9).
act( eat(Agent, Thing), S0, S9) :-
  getprop(Thing, edible, S0),
  undeclare(related(_, Thing, _), S0, S1),
  queue_percept(Agent, [destroyed(Thing), 'Mmmm, good!'], S1, S9).
act( eat(Agent, Thing), S0, S9) :-
  queue_percept(Agent, [failure(eat(Thing)), 'It''s inedible!'], S0, S9).

act( switch(Agent, OnOff, Thing), S0, S) :-
  touchable(Thing, Agent, S0),
  getprop(Thing, can_be(switched(OnOff), t), S0),
  getprop(Thing, effect(switch(OnOff), Term0), S0),
  subst(equivalent, ($(self)), Thing, Term0, Term),
  call(Term, S0, S1),
  queue_percept(Agent, [true, 'OK'], S1, S).
act( open(Agent, Thing), S0, S) :-
  touchable(Thing, Agent, S0),
  %getprop(Thing, openable, S0),
  %\+ getprop(Thing, open, S0),
  delprop(Thing, closed(true), S0, S1),
  %setprop(Thing, open, S0, S1),
  setprop(Thing, closed(fail), S1, S2),
  open_traverse(Agent, Here, S2),
  queue_local_event([setprop(Thing, closed(fail)), 'Opened.'], [Here], S2, S).
act( close(Agent, Thing), S0, S) :-
  touchable(Thing, Agent, S0),
  %getprop(Thing, openable, S0),
  %getprop(Thing, open, S0),
  delprop(Thing, closed(fail), S0, S1),
  %delprop(Thing, open, S0, S1),
  setprop(Thing, closed(true), S1, S2),
  open_traverse(Agent, Here, S2),
  queue_local_event([setprop(Thing, closed(true)), 'Closed.'], [Here], S2, S).

act( talk(Agent, Object, Message), S0, S1) :-  % directed message
  can_sense(audio, Object, Agent, S0),
  open_traverse(Agent, Here, S0),
  queue_local_event([talk(Agent, Object, Message)], [Here], S0, S1).
act( say(Agent, Message), S0, S1) :-          % undirected message
  open_traverse(Agent, Here, S0),
  queue_local_event([say(Agent, Message)], [Here], S0, S1).

act( touch(Agent, _Thing), S0, S9) :-
  queue_percept(Agent, [true, 'OK.'], S0, S9).
act( wait(Agent), State, NewState) :-
  queue_percept(Agent, [time_passes], State, NewState).
act(print_(Agent, Msg), S0, S1) :-
  related(descended, Agent, Here, S0),
  queue_local_event([true, Msg], [Here], S0, S1).
act( true, S, S).
act( Action, S0, S1) :-
   action_agent_thing(Action, _Verb, Agent, _Thing),
  queue_percept(Agent, [failure(Action), 'You can''t do that.'], S0, S1).

% Protocol:
%   Agent: request(Action, Action_Id)
%   Simulation: respond(Action_Id, LogicalResponse/Percept, EnglishResponse)
%   Action(Verb, ...)
%   failure(Reason)
%   moved(obj, from, how, to)

% -----------------------------------------------------------------------------
% The state of an Agent is stored in its memory.
% Agent memory is stored as a list in reverse chronological order, implicitly
%   ordering and timestamping everything.
% Types of memories:
%   agent(A)        - identity of agent (?)
%   timestamp(T)    - agent may add a new timestamp whenever a sequence point
%                     is desired.
%   [percept]       - received perceptions.
%   model([...])    - Agent's internal model of the world.
%                     Model is a collection of timestampped relations.
%   goals([...])    - states the agent would like to achieve, or
%                     acts the agent would like to be able to do.
%   plan(S, O, B, L)   - plans for achieving goals.
%   affect(...)     - Agent's current affect.
% Multiple plans, goals, models, affects, etc. may be stored, for introspection
%   about previous internal states.

% Manipulate memories (M stands for Memories)
memorize(_Agent, Figment, M0, M1) :- append([Figment], M0, M1).
memorize_list(_Agent, FigmentList, M0, M1) :- append(FigmentList, M0, M1).
forget(_Agent, Figment, M0, M1) :- select_from(Figment, M0, M1).
forget_always(_Agent, Figment, M0, M1) :- select_always(Figment, M0, M1).
%forget_default(Figment, Default, M0, M1) :-
%  select_default(Figment, Default, M0, M1).
thought(_Agent, Figment, M) :- member(Figment, M).

in_model(Pred,List):- member(Pred,List).

agent_thought_model(Agent,Model,List):- dmust((memberchk(agent(Agent),List), member(model(Model),List))).

% -------- Model updating predicates (here M stands for Model)

% Fundamental predicate that actually modifies the list:
update_relation(NewAt, Item, NewParent, Timestamp, M0, M2) :-
  select_always(holds_at(related(_At, Item, _Where)), M0, M1),
  append([holds_at(related(NewAt, Item, NewParent), Timestamp)], M1, M2).

% Batch-update relations.
update_relations(_NewAt, [], _NewParent, _Timestamp, M, M).
update_relations(NewAt, [Item|Tail], NewParent, Timestamp, M0, M2) :-
  update_relation(NewAt, Item, NewParent, Timestamp, M0, M1),
  update_relations(NewAt, Tail, NewParent, Timestamp, M1, M2).

% If dynamic topology needs remembering, use
%      related(exit(E), Here, [There1|ThereTail], Timestamp)
update_exit(At, From, Timestamp, M0, M2) :-
  select_from( holds_at(related(At, From, To), _), M0, M1),
  append([holds_at(related(At, From, To), Timestamp)], M1, M2).
update_exit(At, From, Timestamp, M0, M1) :-
  append([holds_at(related(At, From, '<unexplored>'), Timestamp)], M0, M1).

update_exit(At, From, To, Timestamp, M0, M2) :-
  select_always( holds_at(related(At, From, _To), _), M0, M1),
  append([holds_at(related(At, From, To), Timestamp)], M1, M2).

update_exits([], _From, _T, M, M).
update_exits([Exit|Tail], From, Timestamp, M0, M2) :-
  update_exit(Exit, From, Timestamp, M0, M1),
  update_exits(Tail, From, Timestamp, M1, M2).

%butlast(List, ListButLast) :-
%  %last(List, Item),
%  append(ListButLast, [_Item], List).

% Match only the most recent Figment in Memory.
%last_thought(Agent, Figment, Memory) :-  % or member1(F, M), or memberchk(Term, List)
%  copy_term(Figment, FreshFigment),
%  append(RecentMemory, [Figment|_Tail], Memory),
%  \+ member(FreshFigment, RecentMemory).

update_model(Agent, rel_to(held_by, Objects), Timestamp, _Memory, M0, M1) :-
  update_relations(held_by, Objects, Agent, Timestamp, M0, M1).
update_model(Agent, sense_childs(Agent, _Sense, Object, At, Children), Timestamp, _Mem, M0, M1) :-
  update_relations(At, Children, Object, Timestamp, M0, M1).
update_model(Agent, sense_props(Agent, _Sense, Object, PropList), Stamp, _Mem, M0, M2) :-
  select_always(holds_at(props(Object, _),_), M0, M1),
  append([holds_at(props(Object, PropList), Stamp)], M1, M2).
update_model(_Agent, exits_are(_At, Here, Exits), Timestamp, _Mem, M0, M4) :-
  % Don't update map here, it's better done in the moved() clause.
  findall(exit(E), member(E, Exits), ExitRelations),
  update_exits(ExitRelations, Here, Timestamp, M0, M4).% Model exits from Here.
update_model(Agent, moved(Agent, There, At, Here), Timestamp, Mem, M0, M2) :-
  % According to model, where was I?
  in_model(holds_at(t(_, Agent, There), _T0), M0),
  % TODO: Handle goto(Agent, walk, on, table)
  % At did I get Here?
  append(RecentMem, [did(goto(Agent, walk, _AtGo, ExitName))|OlderMem], Mem), % find figment
  \+ member(did(goto(Agent, walk, _, _)), RecentMem),               % guarrantee recentness
  memberchk(timestamp(_T1), OlderMem),               % get associated stamp
  %player_format(Agent, '~p moved: goto(Agent, walk, ~p, ~p) from ~p leads to ~p~n',
  %       [Agent, AtGo, Dest, There, Here]),
  update_exit(exit(ExitName), There, Here, Timestamp, M0, M1), % Model the path.
  update_relation(At, Agent, Here, Timestamp, M1, M2). % And update location.
update_model(_Agent, moved(Object, _From, At, To), Timestamp, _Mem, M0, M1) :-
  update_relation(At, Object, To, Timestamp, M0, M1).
update_model(_Agent, _Percept, _Timestamp, _Memory, M, M).

% update_model_all(Agent, PerceptsList, Stamp, ROMemory, OldModel, NewModel)
update_model_all(_Agent, [], _Timestamp, _Memory, M, M).
update_model_all(Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
  update_model(Agent, Percept, Timestamp, Memory, M0, M1),
  update_model_all(Agent, Tail, Timestamp, Memory, M1, M2).

path2directions([Here, There], [goto(_Agent, walk, *, ExitName)], Model) :-
  in_model(related(exit(ExitName), Here, There), Model).
path2directions([Here, There], [goto(_Agent, walk, in, There)], Model) :-
  in_model(related(descended, Here, There), Model).
path2directions([Here, Next|Trail], [goto(_Agent, walk, *, ExitName)|Tail], Model) :-
  in_model(related(exit(ExitName), Here, Next), Model),
  path2directions([Next|Trail], Tail, Model).
path2directions([Here, Next|Trail], [goto(_Agent, walk, in, Next)|Tail], Model) :-
  in_model( related(descended, Here, Next), Model),
  path2directions([Next|Trail], Tail, Model).

find_path1( [First|_Rest], Dest, First, _Model) :-
  First = [Dest|_].
find_path1([[Last|Trail]|Others], Dest, Route, Model) :-
  findall([Z, Last|Trail],
          (in_model(related(_At, Last, Z), Model), \+ member(Z, Trail)),
          List),
  append(Others, List, NewRoutes),
  find_path1(NewRoutes, Dest, Route, Model).
find_path( Start, Dest, Route, Model) :-
  find_path1( [[Start]], Dest, R, Model),
  reverse(R, RR),
  path2directions(RR, Route, Model).

% --------

precond_matches_effect(Cond, Cond).

precond_matches_effects(path(Here, There), StartEffects) :-
  find_path(Here, There, _Route, StartEffects).
precond_matches_effects(exists(Object), StartEffects) :-
  in_model(related(_, Object, _), StartEffects)
  ;
  in_model(related(_, _, Object), StartEffects).
precond_matches_effects(_Agent, Cond, Effects) :-
  member(E, Effects),
  precond_matches_effect(Cond, E).

% Return an operator after substituting Agent for Agent.
oper_act( goto(Agent, walk, *, ExitName),
     [ Here \= Agent, There \= Agent,
       related(in, Agent, Here),
       related(exit(ExitName), Here, There)], % path(Here, There)
     [ related(in, Agent, There),
       ~related(in, Agent, Here)]).
oper_act( take(Agent, Thing), % from same room
     [ Thing \= Agent, exists(Thing),
       There \= Agent,
       related(At, Thing, There),
       related(At, Agent, There)],
     [ related(held_by, Thing, Agent),
       ~related(At, Thing, There)]).
oper_act( take(Agent, Thing), % from something else
     [ Thing \= Agent, exists(Thing),
       related(At, Thing, What),
       related(At, What, There),
       related(At, Agent, There) ],
     [ related(held_by, Thing, Agent),
       ~related(At, Thing, There)]) :- fail, extra.
oper_act( drop(Agent, Thing),
     [ Thing \= Agent, exists(Thing),
       related(held_by, Thing, Agent)],
     [ ~related(held_by, Thing, Agent)] ).
oper_act( talk(Agent, Player, [please, give, me, the, Thing]),
     [ Thing \= Agent, exists(Thing),
       related(held_by, Thing, Player),
       related(At, Player, Where),
       related(At, Agent, Where) ],
     [ related(held_by, Thing, Agent),
       ~related(held_by, Thing, Player)] ) :- extra.
oper_act( give(Agent, Thing, Recipient),
     [ Thing \= Agent, Recipient \= Agent,
       exists(Thing), exists(Recipient),
       Where \= Agent,
       related(held_by, Thing, Agent),
       related(in, Recipient, Where), exists(Where),
       related(in, Agent, Where)],
     [ related(held_by, Thing, Recipient),
       ~related(held_by, Thing, Agent)
     ] ).
oper_act( put(Agent, Thing, Relation, What), % in something else
     [ Thing \= Agent, What \= Agent, Where \= Agent,
       Thing\=What, What\=Where, Thing\=Where,
       related(held_by, Thing, Agent), exists(Thing),
       related(in, What, Where), exists(What), exists(Where),
       related(in, Agent, Where)],
     [ related(Relation, Thing, What),
       ~related(held_by, Thing, Agent)] ).
%oper_act( put(Agent, Thing, At, Where), % in room
%     [ Thing \= Agent, exists(Thing),
%       related(held_by, Thing, Agent),
%       related(At, Agent, Where],
%     [ related(At, Thing, Where),
%       ~related(held_by, Thing, Agent)] ).

% Return the initial list of operators.
initial_operators(Agent, Operators) :-
  findall(oper(Agent, Action, Conds, Effects),
          oper_act( Action, Conds, Effects),
          Operators).

precondition_matches_effect(Cond, Effect) :-
  % player_format(Agent, '      Comparing cond ~w with effect ~w: ', [Cond, Effect]),
  Cond = Effect. %, player_format(Agent, 'match~n', []).
%precondition_matches_effect(~ ~ Cond, Effect) :-
%  precondition_matches_effect(Cond, Effect).
%precondition_matches_effect(Cond, ~ ~ Effect) :-
%  precondition_matches_effect(Cond, Effect).
precondition_matches_effects(Cond, Effects) :-
  member(E, Effects),
  precondition_matches_effect(Cond, E).
preconditions_match_effects([Cond|Tail], Effects) :-
  precondition_matches_effects(Cond, Effects),
  preconditions_match_effects(Tail, Effects).

% plan(steps, orderings, bindings, links)
% step(id, operation)
new_plan(CurrentState, GoalState, Plan) :-
  Plan = plan([step(start , oper( true, [], CurrentState)),
               step(finish, oper( true, GoalState, []))],
              [before(start, finish)],
              [],
              []).

isbefore(I, J, Orderings) :-
  member(before(I, J), Orderings).
%isbefore(I, K, Orderings) :-
%  select_from(before(I, J), Orderings, Remaining),
%  isbefore(J, K, Remaining).

% These will fail to create inconsistent orderings.
%add_ordering(B, Orderings, Orderings) :-
%  member(B, Orderings), !.
%add_ordering(before(I, K), Orderings, [before(I, K)|Orderings]) :-
%  I \= K,
%  \+ isbefore(K, I, Orderings),
%  bugout('    ADDED ~w to orderings.~n', [before(I, K)], planner).
%add_ordering(B, O, O) :-
%  bugout('    FAILED to add ~w to orderings.~n', [B], planner),
%  fail.

add_ordering(B, Orderings, Orderings) :-
  member(B, Orderings), !.
add_ordering(before(I, J), Order0, Order1) :-
  I \= J,
  \+ isbefore(J, I, Order0),
  add_ordering3(before(I, J), Order0, Order0, Order1).
add_ordering(B, Order0, Order0) :-
  once(pick_ordering(Order0, List)),
  bugout('  FAILED add_ordering ~w to ~w~n', [B, List], planner),
  fail.

% add_ordering3(NewOrder, ToCheck, OldOrderings, NewOrderings)
add_ordering3(before(I, J), [], OldOrderings, NewOrderings) :-
  union([before(I, J)], OldOrderings, NewOrderings).
add_ordering3(before(I, J), [before(J, K)|Rest], OldOrderings, NewOrderings) :-
  I \= K,
  union([before(J, K)], OldOrderings, Orderings1),
  add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, I)|Rest], OldOrderings, NewOrderings) :-
  H \= J,
  union([before(H, J)], OldOrderings, Orderings1),
  add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, K)|Rest], OldOrderings, NewOrderings) :-
  I \= K,
  H \= J,
  add_ordering3(before(I, J), Rest, OldOrderings, NewOrderings).

% insert(E, L, L1) inserts E into L producing L1
% E is not added it is already there.
insert(X, [], [X]).
insert(A, [A|R], [A|R]).
insert(A, [B|R], [B|R1]) :-
   A \== B,
   insert(A, R, R1).

add_orderings([], Orderings, Orderings).
add_orderings([B|Tail], Orderings, NewOrderings) :-
  add_ordering(B, Orderings, Orderings2),
  add_orderings(Tail, Orderings2, NewOrderings).

del_ordering_node(I, [before(I, _)|Tail], Orderings) :-
  del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(_, I)|Tail], Orderings) :-
  del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(X, Y)|Tail], [before(X, Y)|Orderings]) :-
  X \= I,
  Y \= I,
  del_ordering_node(I, Tail, Orderings).
del_ordering_node(_I, [], []).

ordering_nodes(Orderings, Nodes) :-
  setof(Node,
        Other^(isbefore(Node, Other, Orderings);isbefore(Other, Node, Orderings)),
        Nodes).

pick_ordering(Orderings, List) :-        
  ordering_nodes(Orderings, Nodes),
  pick_ordering(Orderings, Nodes, List).

pick_ordering(Orderings, Nodes, [I|After]) :-
  select_from(I, Nodes, RemainingNodes),
  forall(member(J, RemainingNodes), \+ isbefore(J, I, Orderings) ),
  pick_ordering(Orderings, RemainingNodes, After).
pick_ordering(_Orderings, [], []).

test_ordering :-
  bugout('ORDERING TEST:~n', planner),
  once(add_orderings(
   [ before(start, finish),
     before(start, x),
     before(start, y), before(y, finish),
     before(x, z),
     before(z, finish)
   ],
   [],
   Orderings)),
  bugout('  ordering is ~w~n', [Orderings], planner),
  pick_ordering(Orderings, List),
  bugout('  picked ~w~n', [List], planner),
  fail.
test_ordering :- bugout('  END ORDERING TEST~n', planner).

cond_is_achieved(step(J, _Oper), C, plan(Steps, Orderings, _, _)) :-
  member(step(I, oper( _, _, Effects)), Steps),
  precondition_matches_effects(C, Effects),
  isbefore(I, J, Orderings),
  bugout('      Cond ~w of step ~w is achieved!~n', [C, J], planner).
cond_is_achieved(step(J, _Oper), C, plan(_Steps, _Orderings, _, _)) :-
  bugout('      Cond ~w of step ~w is NOT achieved.~n', [C, J], planner),
  !, fail.

% Are the preconditions of a given step achieved by the effects of other
% steps, or are already true?
step_is_achieved(step(_J, oper( _, [], _)), _Plan).  % No conditions, OK.
step_is_achieved(step(J, oper( _, [C|Tail], _)), plan(Steps, Orderings, _, _)) :-
  cond_is_achieved(step(J, _), C, plan(Steps, Orderings, _, _)),
  step_is_achieved(step(J, oper( _, Tail, _)), plan(Steps, Orderings, _, _)).
  
all_steps_are_achieved([Step|Tail], Plan) :-
  step_is_achieved(Step, Plan),
  all_steps_are_achieved(Tail, Plan).
all_steps_are_achieved([], _Plan).

is_solution(plan(Steps, O, B, L)) :-
  all_steps_are_achieved(Steps, plan(Steps, O, B, L)).

% Create a new step given an operator.
operator_as_step(oper( Act, Cond, Effect), step(Id, oper( Act, Cond, Effect))) :-
  Act =.. [Functor|_],
  atom_concat(Functor, '_step_', Prefix),
  gensym(Prefix, Id).

% Create a list of new steps given a list of operators.
operators_as_steps([], []).
operators_as_steps([Oper | OpTail], [Step | StepTail]) :-
  copy_term(Oper, FreshOper), % Avoid instantiating operator database.
  operator_as_step(FreshOper, Step),
  operators_as_steps(OpTail, StepTail).

cond_as_goal(ID, Cond, goal(ID, Cond)).
conds_as_goals(_, [], []).
conds_as_goals(ID, [C|R], [G|T]) :-
  cond_as_goal(ID, C, G),
  conds_as_goals(ID, R, T).

cond_equates(Cond0, Cond1) :- Cond0 = Cond1.
cond_equates(related(X, Y, Z), related(X, Y, Z)).
cond_equates(~(~(Cond0)), Cond1) :- cond_equates(Cond0, Cond1).
cond_equates(Cond0, ~(~(Cond1))) :- cond_equates(Cond0, Cond1).
cond_negates(~Cond0, Cond1) :- cond_equates(Cond0, Cond1).
cond_negates(Cond0, ~Cond1) :- cond_equates(Cond0, Cond1).

% Protect 1 link from 1 condition
% protect(link_to_protect, threatening_step, threatening_cond, ...)
protect(causes(StepI, _Cond0, _StepJ), StepI, _Cond1, Order0, Order0) :-
  !. % Step does not threaten itself.
protect(causes(_StepI, _Cond0, StepJ), StepJ, _Cond1, Order0, Order0) :-
  !. % Step does not threaten itself.
%protect(causes(_StepI, Cond, _StepJ), _StepK, Cond, Order0, Order0) :-
%  !. % Cond does not threaten itself.
protect(causes(_StepI, Cond0, _StepJ), _StepK, Cond1, Order0, Order0) :-
  \+ cond_negates(Cond0, Cond1),
  !.
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
  bugout('  THREAT: ~w <> causes(~w, ~w, ~w)~n',
         [StepK, StepI, Cond0, StepJ], planner),
  fail.
protect(causes(StepI, _Cond0, StepJ), StepK, _Cond1, Order0, Order1) :-
  % Protect by moving threatening step before or after this link.
  add_ordering(before(StepK, StepI), Order0, Order1),
  bugout('    RESOLVED with ~w~n', [before(StepK, StepI)], planner)
  ;
  add_ordering(before(StepJ, StepK), Order0, Order1),
  bugout('    RESOLVED with ~w~n', [before(StepJ, StepK)], planner).
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
  bugout('  FAILED to resolve THREAT ~w <> causes(~w, ~w, ~w)~n',
         [StepK, StepI, Cond0, StepJ], planner),
  once(pick_ordering(Order0, Serial)),
  bugout('    ORDERING is ~w~n', [Serial], planner),
  fail.

% Protect 1 link from 1 step's multiple effects
protect_link(_Link, _StepID, [], Order0, Order0).
protect_link(Link, StepID, [Cond|Effects], Order0, Order2):-
  protect(Link, StepID, Cond, Order0, Order1),
  protect_link(Link, StepID, Effects, Order1, Order2).

% Protect all links from 1 step's multiple effects
% protect_links(links_to_protect, threatening_step, threatening_cond, ...)
protect_links([], _StepID, _Effects, Order0, Order0).
protect_links([Link|Tail], StepID, Effects, Order0, Order2) :-
  protect_link(Link, StepID, Effects, Order0, Order1),
  protect_links(Tail, StepID, Effects, Order1, Order2).

% Protect 1 link from all steps' multiple effects
protect_link_all(_Link, [], Order0, Order0).
protect_link_all(Link, [step(StepID, oper( _, _, Effects))|Steps], Order0, Order2) :-
  protect_link(Link, StepID, Effects, Order0, Order1),
  protect_link_all(Link, Steps, Order1, Order2).

%add_binding((X\=Y), Bindings0, Bindings) :-
%  X \= Y, % if they can't bind, don't bother to add them.
add_binding((X\=Y), Bindings, [(X\=Y)|Bindings]) :-
  X \== Y, % if they're distinct,
  % \+ \+ X=Y, % but could bind
  bindings_valid(Bindings).

bindings_valid([]).
bindings_valid([(X\=Y)|Bindings]) :-
  X \== Y,
  bindings_valid(Bindings).
%bindings_valid(B) :-
%  bugout('  BINDINGS are *INVALID*: ~w~n', [B], planner),
%  fail.

bindings_safe([]) :- bugout('  BINDINGS are SAFE~n', planner).
bindings_safe([(X\=Y)|Bindings]) :-
  X \= Y,
  bindings_safe(Bindings).
%bindings_safe(B) :-
%  bugout('  BINDINGS are *UNSAFE*: ~w~n', [B], planner),
%  fail.

choose_operator([goal(GoalID, GoalCond)|Goals0], Goals0,
                 _Operators,
                 plan(Steps, Order0, Bindings, OldLinks),
                 plan(Steps, Order9, Bindings, NewLinks),
                 Depth, Depth ) :-
  % Achieved by existing step?
  member(step(StepID, oper( _Action, _Preconds, Effects)), Steps),
  precondition_matches_effects(GoalCond, Effects),
  add_ordering(before(StepID, GoalID), Order0, Order1),
  % Need to protect new link from all existing steps
  protect_link_all(causes(StepID, GoalCond, GoalID), Steps, Order1, Order9),
  union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
  bindings_valid(Bindings),
  bugout('  EXISTING step ~w satisfies ~w~n', [StepID, GoalCond], planner).
choose_operator([goal(_GoalID, X \= Y)|Goals0], Goals0,
                 _Operators,
                 plan(Steps, Order, Bindings, Links),
                 plan(Steps, Order, NewBindings, Links),
                 Depth, Depth ) :-
  add_binding((X\=Y), Bindings, NewBindings),
  bugout('  BINDING ADDED: ~w~n', [X\=Y], planner).
choose_operator([goal(GoalID, ~ GoalCond)|Goals0], Goals0,
                 _Operators,
                 plan(Steps, Order0, Bindings, OldLinks),
                 plan(Steps, Order9, Bindings, NewLinks),
                 Depth, Depth ) :-
  % Negative condition achieved by start step?
  memberchk(step(start, oper( _Action, _Preconds, Effects)), Steps),
  \+ precondition_matches_effects(GoalCond, Effects),
  add_ordering(before(start, GoalID), Order0, Order1),
  % Need to protect new link from all existing steps
  protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
  union([causes(start, ~GoalCond, GoalID)], OldLinks, NewLinks),
  bindings_valid(Bindings),
  bugout('  START SATISFIES NOT ~w~n', [GoalCond], planner).
choose_operator([goal(GoalID, exists(GoalCond))|Goals0], Goals0,
                 _Operators,
                 plan(Steps, Order0, Bindings, OldLinks),
                 plan(Steps, Order9, Bindings, NewLinks),
                 Depth, Depth ) :-
  memberchk(step(start, oper( _Action, _Preconds, Effects)), Steps),
  ( in_model(related(_At, GoalCond, _Where, _), Effects);
    in_model(related(_At, _What, GoalCond, _), Effects)),
  add_ordering(before(start, GoalID), Order0, Order1),
  % Need to protect new link from all existing steps
  protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
  union([causes(start, exists(GoalCond), GoalID)], OldLinks, NewLinks),
  bindings_valid(Bindings),
  bugout('  START SATISFIES exists(~w)~n', [GoalCond], planner).
choose_operator([goal(GoalID, GoalCond)|Goals0], Goals2,
                 Operators,
                 plan(OldSteps, Order0, Bindings, OldLinks),
                 plan(NewSteps, Order9, Bindings, NewLinks),
                 Depth0, Depth ) :-
  % Condition achieved by new step?
  Depth0 > 0,
  Depth is Depth0 - 1,
  %operators_as_steps(Operators, FreshSteps),
  copy_term(Operators, FreshOperators),
  % Find a new operator.
  %member(step(StepID, oper( Action, Preconds, Effects)), FreshSteps),
  member(oper( Action, Preconds, Effects), FreshOperators),
  precondition_matches_effects(GoalCond, Effects),
  operator_as_step(oper( Action, Preconds, Effects),
                   step(StepID, oper( Action, Preconds, Effects)) ),
  % Add ordering constraints.
  add_orderings([before(start, StepID),
                 before(StepID, GoalID),
                 before(StepID, finish)],
                Order0, Order1),
  % Need to protect existing links from new step.
  protect_links(OldLinks, StepID, Effects, Order1, Order2),
  % Need to protect new link from all existing steps
  protect_link_all(causes(StepID, GoalCond, GoalID), OldSteps, Order2, Order9),
  % Add the step.
  append(OldSteps, [step(StepID, oper( Action, Preconds, Effects))], NewSteps),
  % Add causal constraint.
  union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
  % Add consequent goals.
  conds_as_goals(StepID, Preconds, NewGoals),
  append(Goals0, NewGoals, Goals2),
  bindings_valid(Bindings),
  bugout('  ~w CREATED ~w to satisfy ~w~n',
         [Depth, StepID, GoalCond], autonomous),
  pprint(oper( Action, Preconds, Effects), planner),
  once(pick_ordering(Order9, List)),
  bugout('    Orderings are ~w~n', [List], planner).
choose_operator([goal(GoalID, GoalCond)|_G0], _G2, _Op, _P0, _P2, D, D) :-
  bugout('  CHOOSE_OPERATOR FAILED on goal:~n    goal(~w, ~w)~n',
         [GoalID, GoalCond], planner),
  !, fail.
choose_operator(G0, _G2, _Op, _P0, _P2, D, D) :-
  bugout('  !!! CHOOSE_OPERATOR FAILED: G0 = ~w~n', [G0], planner), !, fail.

planning_loop([], _Operators, plan(S, O, B, L), plan(S, O, B, L), _Depth, _TO ) :-
  bugout('FOUND SOLUTION?~n', planner),
  bindings_safe(B).
planning_loop(Goals0, Operators, Plan0, Plan2, Depth0, Timeout) :-
  %Limit > 0,
  get_time(Now),
  (Now > Timeout -> throw(timeout(planner)); true),
  bugout('GOALS ARE: ~w~n', [Goals0], planner),
  choose_operator(Goals0, Goals1, Operators, Plan0, Plan1, Depth0, Depth),
  %Limit2 is Limit - 1,
  planning_loop(Goals1, Operators, Plan1, Plan2, Depth, Timeout).
%planning_loop(_Goals0, _Operators, Plan0, Plan0, _Limit) :-
%  Limit < 1,
%  bugout('Search limit reached!~n', planner),
%  fail.

serialize_plan( plan([], _Orderings, _B, _L), []) :- !.

serialize_plan(plan(Steps, Orderings, B, L), Tail) :-
  select_from(step(_, oper( true, _, _)), Steps, RemainingSteps),
  !,
  serialize_plan(plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(plan(Steps, Orderings, B, L), [Action|Tail]) :-
  select_from(step(StepI, oper( Action, _, _)), Steps, RemainingSteps),
  \+ (member(step(StepJ, _Oper), RemainingSteps),
      isbefore(StepJ, StepI, Orderings)),
  serialize_plan(plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(plan(_Steps, Orderings, _B, _L), _) :-
  bugout('serialize_plan FAILED!~n', planner),
  pick_ordering(Orderings, List),
  bugout('  Orderings are ~w~n', [List], planner),
  fail.

select_unsatisfied_conditions([], [], _Model) :- !.
select_unsatisfied_conditions([Cond|Tail], Unsatisfied, Model) :-
  precondition_matches_effects(Cond, Model),
  !,
  select_unsatisfied_conditions(Tail, Unsatisfied, Model).
select_unsatisfied_conditions([(~Cond)|Tail], Unsatisfied, Model) :-
  \+ precondition_matches_effects(Cond, Model),
  !,
  select_unsatisfied_conditions(Tail, Unsatisfied, Model).
select_unsatisfied_conditions([Cond|Tail], [Cond|Unsatisfied], Model) :-
  !,
  select_unsatisfied_conditions(Tail, Unsatisfied, Model).

depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
                    Depth, Timeout) :-
  bugout('PLANNING DEPTH is ~w~n', [Depth], autonomous),
  planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan, Depth, Timeout),
  !.
depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
                    Depth0, Timeout) :-
  Depth0 =< 7,
  Depth is Depth0 + 1,
  depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
                      Depth, Timeout).

generate_plan(FullPlan, Mem0) :-
  thought(Agent, agent(Agent), Mem0),
  initial_operators(Agent, Operators),
  bugout('OPERATORS are:~n', planner), pprint(Operators, planner),
  thought(Agent, model(Model0), Mem0),
  %bugout('CURRENT STATE is ~w~n', [Model0], planner),
  thought(Agent, goals(Goals), Mem0),
  new_plan(Model0, Goals, SeedPlan),
  bugout('SEED PLAN is:~n', planner), pprint(SeedPlan, planner),
  !,
  %planning_loop(Operators, SeedPlan, FullPlan),
  conds_as_goals(finish, Goals, PlannerGoals),
  get_time(Now),
  Timeout is Now + 60, % seconds
  catch(
    depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
                        1, Timeout),
    timeout(planner),
    (bugout('PLANNER TIMEOUT~n', autonomous), fail)
  ),
  bugout('FULL PLAN is:~n', planner), pprint(FullPlan, planner).

% ---- 

add_goal(Agent, Goal, Mem0, Mem2) :-
  bugout('adding goal ~w~n', [Goal], planner),
  forget(Agent, goals(OldGoals), Mem0, Mem1),
  append([Goal], OldGoals, NewGoals),
  memorize(Agent, goals(NewGoals), Mem1, Mem2).

add_goals(Goals, Mem0, Mem2) :-
  forget(Agent, goals(OldGoals), Mem0, Mem1),
  append(Goals, OldGoals, NewGoals),
  memorize(Agent, goals(NewGoals), Mem1, Mem2).

add_todo(Auto, Mem0, Mem3) :- Auto = auto(Agent),
 %dmust(member(inst(Agent), Mem0)),
 autonomous_decide_action(Agent, Mem0, Mem3),!.

add_todo( Action, Mem0, Mem2) :-
  forget(Agent, todo(OldToDo), Mem0, Mem1),
  append(OldToDo, [Action], NewToDo),
  memorize(Agent, todo(NewToDo), Mem1, Mem2).

add_todo_all([], Mem0, Mem0).
add_todo_all([Action|Rest], Mem0, Mem2) :-
  add_todo( Action, Mem0, Mem1),
  add_todo_all(Rest, Mem1, Mem2).

% For now, agents will attempt to satisfy all commands.
%consider_request(_Speaker, Agent, take(Object), M0, M1) :-
%  add_goal(Agent, related(held_by, Object, Agent), M0, M1).
consider_request(_Speaker, Agent, Action, M0, M0) :-
  bugout('~w: considering request: ~w.~n', [Agent, Action], autonomous),
  fail.
consider_request(Requester, Agent, Query, M0, M1) :-
  do_introspect(Query, Answer, M0),
  %add_todo( print_(Agent, Answer), M0, M1).
  add_todo( talk(Agent, Requester, Answer), M0, M1).
consider_request(_Speaker, Agent, forget(Agent, goals), M0, M2) :-
  bugout('~w: forgetting goals.~n', [Agent], autonomous),
  forget_always(Agent, goals(_), M0, M1),
  memorize(Agent, goals([]), M1, M2).
consider_request(_Speaker, Agent, goto(Agent, walk, *, ExitName), M0, M1) :-
  bugout('Queueing action ~w~n', goto(Agent, walk, *, ExitName), autonomous),
  add_todo( goto(Agent, walk, *, ExitName), M0, M1).
consider_request(Speaker, Agent, fetch(Object), M0, M1) :-
  % Bring object back to Speaker.
  add_goal(Agent, related(held_by, Object, Speaker), M0, M1).
consider_request(_Speaker, Agent, put(Thing, Relation, Where), M0, M) :-
  add_goal(Agent, related(Relation, Thing, Where), M0, M).
consider_request(_Speaker, Agent, take(Thing), M0, M) :-
  add_goal(Agent, related(held_by, Thing, Agent), M0, M).
consider_request(_Speaker, Agent, Action, M0, M1) :-
  bugout('Finding goals for action: ~w~n', [Action], autonomous),
  initial_operators(Agent, Operators),
  findall(Effects,
          member(oper( Action, _Conds, Effects), Operators),
          [UnambiguousGoals]),
  bugout('Request: ~w --> goals ~w.~n', [Action, UnambiguousGoals], autonomous),
  add_goals(UnambiguousGoals, M0, M1).
consider_request(_Speaker, _Agent, Action, M0, M1) :-
  bugout('Queueing action: ~w~n', [Action], autonomous),
  add_todo( Action, M0, M1).
consider_request(Speaker, Agent, Action, M0, M0) :-
  bugout('~w: did not understand request from ~w: ~w~n', [Agent, Speaker, Action], autonomous).

% Autonomous logical percept processing.
process_percept_auto(Agent, [say(Agent, _)|_], _Stamp, Mem0, Mem0).
process_percept_auto(Agent, [talk(Agent, _, _)|_], _Stamp, Mem0, Mem0).
process_percept_auto(Agent, talk(Speaker, Agent, Words), _Stamp, Mem0, Mem1) :-
  parse_command(Agent, Words, Action, Mem0),
  consider_request(Speaker, Agent, Action, Mem0, Mem1).
process_percept_auto(Agent, say(Speaker, [Agent|Words]), _Stamp, Mem0, Mem1) :-
  parse_command(Agent, Words, Action, Mem0),
  consider_request(Speaker, Agent, Action, Mem0, Mem1).
process_percept_auto(Agent, Percept, _Stamp, Mem0, Mem0) :-
  Percept =.. [Functor|_],
  member(Functor, [talk, say]),
  bugout('~w: Ignoring ~w~n', [Agent, Percept], autonomous).
process_percept_auto(Agent, sense_props(Agent, Sense, Object, PropList), _Stamp, Mem0, Mem2) :-
  bugout('~w: ~w~n', [Agent, sense_props(Agent, Sense, Object, PropList)], autonomous),
  member(shiny, PropList),
  member(model(Model), Mem0),
  \+  related(descended, Object, Agent, Model), % Not holding it?
  add_todo_all( [take(Agent, Object), print_('My shiny precious!')], Mem0, Mem2).

process_percept_auto(Agent, can_sense_from_here(Agent, _At, _Here, Sense, Objects), _Stamp, Mem0, Mem2) :-
  member(model(Model), Mem0),
  findall(examine(Sense, Obj),
          ( member(Obj, Objects),
            \+ member(holds_at(props(Obj, _),_), Model)),
          ExamineNewObjects),
  add_todo_all(ExamineNewObjects, Mem0, Mem2).
process_percept_auto(_Agent, _Percept, _Stamp, Mem0, Mem0).

process_percept_player(Agent, [say(Agent, _)|_], _Stamp, Mem0, Mem0).
process_percept_player(Agent, [talk(Agent, _, _)|_], _Stamp, Mem0, Mem0).
  % Ignore own speech.
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :-
  percept2txt(Agent, Percept, Text),
  player_format(Agent, '~w~n', [Text]).
  

process_percept_main(Agent, Percept, Stamp, Mem0, Mem4) :-
  forget(Agent, model(Model0), Mem0, Mem1),
  update_model(Agent, Percept, Stamp, Mem1, Model0, Model1),
  memorize(Agent, model(Model1), Mem1, Mem2),
  process_percept_auto(Agent, Percept, Stamp, Mem2, Mem3),
  process_percept_player(Agent, Percept, Stamp, Mem3, Mem4).
process_percept_main(_Agent, Percept, _Stamp, Mem0, Mem0) :-
  bugout('process_percept_main(~w) FAILED!~n', [Percept], general), !.

% caller memorizes PerceptList
process_percept_list(Agent, _, _Stamp, Mem, Mem) :-
  thought(Agent, agent_type(recorder), Mem),
  !.
process_percept_list(Agent, [Percept|Tail], Stamp, Mem0, Mem4) :-
  %bugout('process_percept_list([~w|_])~n', [Percept], autonomous),
  %!,
  process_percept_main(Agent, Percept, Stamp, Mem0, Mem1),
  process_percept_list(Agent, Tail, Stamp, Mem1, Mem4).
process_percept_list(_Agent, [], _Stamp, Mem0, Mem0).
process_percept_list(_Agent, _, _Stamp, Mem0, Mem0) :-
  bugout('process_percept_list FAILED!~n', general).

% -----------------------------------------------------------------------------
:- dynamic(useragent/1).
useragent(player).

cmdalias(d, down).
cmdalias(e, east).
cmdalias(i, inventory).
cmdalias(l, look).
cmdalias(n, north).
cmdalias(s, south).
cmdalias(u, up).
cmdalias(w, west).
cmdalias(x, examine).
cmdalias(z, wait).

preposition(switch, P) :- !,
  member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with]).
preposition(walk, P) :- !,
  member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with]).
preposition(_, P) :-
  member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with]).
compass_direction(D) :-
  member(D, [north, south, east, west]).

reflexive(W) :- member(W, [self, me, myself]). % 'i' inteferes with inventory

strip_noise_words(Tokens, NewTokens) :-
  findall(Token,
          ( member(Token, Tokens),
            \+ member(Token, ['please', 'the', 'a', 'an'])),
          NewTokens).

convert_reflexive(Agent, Words, NewWords) :-
  % Substitute Agent for 'self'.
  findall(Token,
          ( member(Word, Words),
            ( reflexive(Word), Token = Agent;
              Token = Word )),
          NewWords).

% -- parse_command(WordList, ActionOrQuery, Memory)
parse_command(Agent, Tokens, Action, Memory) :-
  parse(Agent, Tokens, ActionP, Memory),
  ActionP =.. [F|ActionL],
  Action =.. [F,Agent|ActionL].
  
 
parse(Agent, Tokens, Action, Memory) :-
  strip_noise_words(Tokens, Tokens2),
  parse2logical(Agent, Tokens2, Action, Memory).

parse2logical(Agent, [ask, Object | Msg], talk(Agent, Object, Msg), _M).
parse2logical(Agent, [request, Object | Msg], talk(Agent, Object, Msg), _M).
parse2logical(Agent, [tell, Object | Msg], talk(Agent, Object, Msg), _M).
parse2logical(Agent, [talk, Object | Msg], talk(Agent, Object, Msg), _M).
parse2logical(Agent, [say|Msg], say(Agent, Msg), _M).
parse2logical(Agent, [Object, ', ' | Msg], talk(Agent, Object, Msg), Mem) :-
  agent_thought_model(Agent, Model, Mem),
  in_model(related(_, Object, _), Model).
parse2logical(Agent, Words, Action, Mem) :-
  % If not talking to someone else, substitute Agent for 'self'.
  append(Before, [Self|After], Words),
  reflexive(Self),
  append(Before, [Agent|After], NewWords),
  parse2logical(Agent, NewWords, Action, Mem).
parse2logical(Agent, [dig, Hole], dig(Agent, Hole, Where, Tool), Mem) :-
  agent_thought_model(Agent, Model, Mem),
  in_model(related(_, Agent, Where), Model),
  Tool=shovel.
parse2logical(Agent, [get, Prep], goto(Agent, walk, *, Prep), _Mem) :-
  preposition(walk, Prep).

parse2logical(Agent, [get, Prep, Object], goto(Agent, walk, Prep, Object), _Mem) :-
  preposition(walk, Prep).

parse2logical(Agent, [get, Object], take(Agent, Object), _Mem).
parse2logical(Agent, [give, Object, to, Recipient], give(Agent, Object, Recipient), _Mem).
parse2logical(Agent, [go, escape], goto(Agent, walk, *, escape), _Mem).
parse2logical(Agent, [go, Dir], goto(Agent, walk, *, Dir), _Mem) :-
  compass_direction(Dir).
parse2logical(Agent, [go, Prep], goto(Agent, walk, *, Prep), _Mem) :-
  preposition(switch, Prep).
parse2logical(Agent, [go, ExitName], goto(Agent, walk, *, ExitName), Mem) :-
  agent_thought_model(Agent, Model, Mem),
  in_model(related(exit(ExitName), _, _), Model).
parse2logical(Agent, [go, Dest], goto(Agent, walk, *, Dest), Mem) :-
  agent_thought_model(Agent, Model, Mem),
  in_model(related(_, _, Dest), Model).
  % getprop(Dest, has_rel(At), Model).

parse2logical(Agent, [light, Thing], switch(Agent, on, Thing), _Mem).
parse2logical(Agent, [switch, Thing, OnOff], switch(Agent, OnOff, Thing), _Mem) :-
  preposition(switch, OnOff).
parse2logical(Agent, [switch, OnOff, Thing], switch(Agent, OnOff, Thing), _Mem) :-
  preposition(switch, OnOff).
parse2logical(Agent, [turn, Thing, OnOff], switch(Agent, OnOff, Thing), _Mem) :-
  preposition(switch, OnOff).
parse2logical(Agent, [turn, OnOff, Thing], switch(Agent, OnOff, Thing), _Mem) :-
  preposition(switch, OnOff).

parse2logical(Agent, [what| REST], whatis(Thing), Mem):-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [whereami|REST], whereis(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [where, am, i|REST], whereis(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [where|REST], whereis(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [whoami|REST], whois(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [who, am, i|REST], whois(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [model|REST], model(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).
parse2logical(Agent, [memory|REST], memory(Thing), Mem) :-
  rest_maybe_thing(Agent, REST, Thing, Mem).

parse2logical(Agent, [CmdAlias|Tail], Action, Mem) :-
  cmdalias(CmdAlias, Verb),
  parse2logical(Agent, [Verb|Tail], Action, Mem).

parse2logical(Agent, [escape], goto(Agent, walk, *, escape), _Mem).
parse2logical(Agent, [Dir], goto(Agent, walk, *, Dir), _Mem) :-
  compass_direction(Dir).
parse2logical(Agent, [Prep], goto(Agent, walk, *, Prep), _Mem) :-
  preposition(switch, Prep).
parse2logical(Agent, [ExitName], goto(Agent, walk, *, ExitName), Mem) :-
  agent_thought_model(Agent, Model, Mem),
  in_model(related(exit(ExitName), _, _), Model).
parse2logical(_Agent, [Verb|Args], Action, _M) :-
  
  %member(Verb, [agent, create, delprop, destroy, echo, quit, memory, model, path, properties, setprop, state, trace, notrace, whereami, whereis, whoami]),
  Action =.. [Verb|Args].

rest_maybe_thing(_Agent, [Thing], Thing, _Mem):- !.
rest_maybe_thing(Agent,     _,    Agent, _Mem):- !.

% do_introspect(Query, Answer, Memory)
do_introspect(path(There), Answer, Memory) :-
  agent_thought_model(Agent, Model, Memory),
  in_model(related(_At, Agent, Here), Model),
  find_path(Here, There, Route, Model),
  Answer = ['Model is', Model, '\nShortest path is', Route].
do_introspect(whereis(Thing), Answer, Memory) :-
  agent_thought_model(Agent, Model, Memory),
  in_model(holds_at(related(At, Thing, Where), T), Model),
  At \= exit(_),
  Answer = ['At time', T, subj(Agent), 'saw the', Thing, At, the, Where, .].
do_introspect(whereis(Here), Answer, Memory) :-
  agent_thought_model(Agent, Model, Memory),
  in_model(related(_At, Agent, Here), Model),
  Answer = 'Right here.'.
do_introspect(whereis(There), Answer, Memory) :-
  agent_thought_model(Agent, Model, Memory),
  in_model(related(_At, Agent, Here), Model),
  find_path(Here, There, Route, Model),
  Answer = ['To get to the', There, ', ', Route].
do_introspect(whereis(There), Answer, Memory) :-
  agent_thought_model(_Agent, Model, Memory),
  ( in_model(related(exit(_), _, There), Model);
    in_model(related(exit(_), There, _), Model)),
  Answer = 'Can''t get there from here.'.
do_introspect(whereis(X), Answer, Memory) :-
  agent_thought_model(Agent, _Model, Memory),
  Answer = [subj(Agent), person('don\'t', 'doesn\'t'),
            'recall ever seeing a "', X, '".'].
do_introspect(whois(X), Answer, Memory) :-
  do_introspect(whereis(X), Answer, Memory).
do_introspect(whois(X), [X, is, X, .], _Memory).
do_introspect(whatis(X), Answer, Memory) :-
  do_introspect(whereis(X), Answer, Memory).
do_introspect(whatis(X), [X, is, X, .], _Memory).

save_term(Filename, Term) :-
  \+ access_file(Filename, exist),
  open(Filename, write, FH),
  write(FH, Term),
  close(FH),
  player_format('Saved to file "~w".~n', [Filename]).
save_term(Filename, _) :-
  access_file(Filename, exist),
  player_format('Save FAILED! Does file "~w" already exist?~n', [Filename]).
save_term(Filename, _) :-
  player_format('Failed to open file "~w" for saving.~n', [Filename]).


%printable_state(L,S):- sort(L,S).
printable_state(S,S).


print_english(Doer, Logic):- is_list(Logic),!, maplist(print_english(Doer), Logic).
print_english(Doer, Logic):- logical2eng(Doer, Logic, Eng),dmust((eng2txt(Doer, Doer, Eng, Text))), pprint(Text,always).

%meta_pprint(Doer, Logic, always):- xtreme_english,!, print_english(Doer, Logic).
meta_pprint(_Doer, D,K):- pprint(D,K).

maybe_pause(_).

:- dynamic(adv:useragent/2).
% do_metacmd(Doer, Action, S0, S1)
do_metacmd(Doer, logout(Agent), S0, S1) :-
  (security_of(Doer, admin); Agent == Doer), !,
  declare(quit(Doer), S0, S1),
  player_format(Doer, 'Bye!~n', []).
do_metacmd(Doer, agent(NewAgent), S0, S0) :-
  security_of(Doer, wizard),
  retractall(adv:useragent(Doer, _)),
  asserta(adv:useragent(Doer, NewAgent)).

do_metacmd(Doer, trace, S0, S0) :- security_of(Doer, admin), trace.
do_metacmd(Doer, notrace, S0, S0) :- security_of(Doer, admin), notrace.
do_metacmd(Doer, spy(Pred), S0, S0) :- security_of(Doer, admin), spy(Pred).
do_metacmd(Doer, nospy(Pred), S0, S0) :- security_of(Doer, admin), nospy(Pred).
do_metacmd(Doer, Echo, S0, S0) :-
  security_of(Doer, admin),
  Echo =.. [echo|Args],
  player_format(Doer, '~w~n', [Args]).
do_metacmd(Doer, state, S0, S0) :-
 security_of(Doer,wizard),
 printable_state(S0,S),
 meta_pprint(Doer, S, always),
 maybe_pause(Doer).
do_metacmd(Doer, props, S0, S0) :-
 security_of(Doer,wizard),
 printable_state(S0,S),
 include(@=<(props(_,_)),S,SP),
 reverse(SP,SPR),
 meta_pprint(Doer, SPR, always),
 maybe_pause(Doer).
do_metacmd(Doer, mem, S0, S0) :-
 security_of(Doer,wizard),
 printable_state(S0,S),
 include(@>=(props(_,_)),S,SP),
 reverse(SP,SPR),
 meta_pprint(Doer, SPR, always),
 maybe_pause(Doer).

do_metacmd(Doer, make, S0, S0) :-
 security_of(Doer,wizard),
 thread_signal(main,make).

do_metacmd(Doer, prolog, S0, S0) :-
 security_of(Doer,wizard),
 '$current_typein_module'(Was),
 setup_call_cleanup('$set_typein_module'(mu),prolog,'$set_typein_module'(Was)).

do_metacmd(Doer, CLS, S0, S0) :- security_of(Doer,wizard), 
 current_predicate(_, CLS), 
 (is_main_console -> catch(CLS,E,(bugout(CLS:- throw(E)),fail)) ;
 (redirect_error_to_string(catch(CLS,E,(bugout(CLS:- throw(E)),fail)),Str),!, write(Str))),!.
do_metacmd(Doer, memory(TargetAgent), S0, S0) :-
  security_of(Doer, wizard),
  declared(memories(TargetAgent, Memory), S0),
  pprint(Memory, general).
do_metacmd(Doer, model(TargetAgent), S0, S0) :-
  security_of(Doer, wizard),
  declared(memories(TargetAgent, Memory), S0),
  agent_thought_model(TargetAgent, Model, Memory),
  pprint(Model, general).

do_metacmd(Doer, create(Object), S0, S1) :-
  security_of(Doer, wizard),
  useragent(Doer),
  related(At, Doer, Here, S0),
  declare(related(At, Object, Here), S0, S1),
  player_format('You now see a ~w.~n', [Object]).
do_metacmd(Doer, destroy(Object), S0, S1) :-
  security_of(Doer, wizard),
  undeclare(related(_, Object, _), S0, S1),
  player_format('It vanishes instantly.~n', []).
do_metacmd(Doer, AddProp, S0, S1) :-
  security_of(Doer, wizard),
  AddProp =.. [setprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  setprop(Object, Prop, S0, S1),
  player_format('Properties of ~p now include ~w~n', [Object, Prop]).
do_metacmd(Doer, DelProp, S0, S1) :-
  security_of(Doer, wizard),
  DelProp =.. [delprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  delprop(Object, Prop, S0, S1),
  player_format('Deleted.~n', []).
do_metacmd(Doer, properties(Object), S0, S0) :-
  security_of(Doer, wizard),
  declared(props(Object, PropList), S0),
  player_format(Doer, 'Properties of ~p are now ~w~n', [Object, PropList]).
do_metacmd(_Doer, undo, S0, S1) :-
  declare(undo, S0, S1),
  player_format('undo...OK~nKO...odnu~n', []).
do_metacmd(_Doer, save(Basename), S0, S0) :-
  atom_concat(Basename, '.adv', Filename),
  save_term(Filename, S0).

do_command(Agent, Action, S0, S1) :-
  do_metacmd(Agent, Action, S0, S1), !.
do_command(Agent, Action, S0, S1) :-
  declared(memories(Agent, Mem), S0),
  do_introspect(Action, Answer, Mem),!,
  queue_percept(Agent, [answer(Answer), Answer], S0, S1).
  %player_format(Agent, '~w~n', [Answer]).
do_command(Agent, Action, S0, S3) :-
  undeclare(memories(Agent, Mem0), S0, S1),
  memorize(Agent, did(Action), Mem0, Mem1),
  declare(memories(Agent, Mem1), S1, S2),
  apply_act( Action, S2, S3).
do_command(Agent, Action, S0, S0) :-
  player_format(Agent, 'Failed or No Such Command: ~w~n', Action), !.

% --------

do_todo(Agent, S0, S9) :-
  undeclare(memories(Agent, Mem0), S0, S1),
  forget(Agent, todo(OldToDo), Mem0, Mem1),
  append([Action], NewToDo, OldToDo),
  memorize(Agent, todo(NewToDo), Mem1, Mem2),
  declare(memories(Agent, Mem2), S1, S2),
  do_command(Agent, Action, S2, S9).
do_todo(_Agent, S0, S0).

%do_todo_while(Agent, S0, S9) :-
%  declared(memories(Agent, Mem0), S0),
%  thought(Agent, todo(ToDo), Mem0),
%  append([Action], NewToDo, OldToDo),

extra_look_around(Agent, S0, S9) :-
  undeclare(memories(Agent, Mem0), S0, S1),
  memorize_list(Agent, [did(look(Agent)), did(inventory(Agent))], Mem0, Mem1),
  declare(memories(Agent, Mem1), S1, S2),
  add_look(Agent, S2, S3),
  apply_act( inventory(Agent), S3, S9).

random_noise(Agent, [cap(subj(Agent)), Msg]) :-
  random_member([
    'hums quietly to himself.',
    'checks his inspection cover.',
    'buffs his chestplate.',
    'fidgets uncomfortably.'
    ], Msg).


:- dynamic(adv:agent_last_action/3).
 

do_autonomous_cycle(Agent):- time_since_last_action(Agent,When), When > 10, !.
do_autonomous_cycle(Agent):- 
 time_since_last_action(Other,When),
 Other \== Agent, When < 1, !, 
 retractall(adv:agent_last_action(Other,_,_)),
 nop(bugout(time_since_last_action_for(Other,When,Agent))).


% Is powered down
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem0) :- 
 getprop(Agent, status(powered, f), advstate),!.

maybe_autonomous_decide_goal_action(Agent, Mem0, Mem1) :- notrace((do_autonomous_cycle(Agent),
 set_last_action(Agent,[auto(Agent)]))),
 autonomous_decide_goal_action(Agent, Mem0, Mem1),!.
maybe_autonomous_decide_goal_action(_Agent, Mem0, Mem0).


% ......
autonomous_decide_goal_action(Agent, Mem0, Mem3) :-
 dmust((
    forget(goals(Goals), Mem0, Mem1),
    thought_model(ModelData, Mem1),
    select_unsatisfied_conditions(Goals, Unsatisfied, ModelData),
    subtract(Goals,Unsatisfied,Satisfied),
    memorize(goals(Unsatisfied), Mem1, Mem1a),
    (Satisfied==[] -> Mem1a=Mem2 ; memorize(satisfied(Satisfied), Mem1a, Mem2)),
    autonomous_decide_action(Agent, Mem2, Mem3))).

autonomous_decide_action(Agent, Mem0, Mem0) :-
  % If actions are queued, no further thinking required.
  thought(Agent, todo([Action|_]), Mem0),
  bugout('~w: about to: ~w~n', [Agent, Action], autonomous).
autonomous_decide_action(Agent, Mem0, Mem1) :-
  % If goals exist, try to solve them.
  thought(Agent, goals([_|_]), Mem0),
  bugout('~w: goals exist: generating a plan...~n', [Agent], autonomous),
  generate_plan(NewPlan, Mem0), !,
  serialize_plan(NewPlan, Actions), !,
  bugout('Planned actions are ~w~n', [Actions], autonomous),
  Actions = [Action|_],
  add_todo( Action, Mem0, Mem1).
autonomous_decide_action(Agent, Mem0, Mem2) :-
  forget(Agent, goals([_|_]), Mem0, Mem1),
  memorize(Agent, goals([]), Mem1, Mem2),
  bugout('~w: Can\'t solve goals.  Forgetting them.~n', [Agent], autonomous).
autonomous_decide_action(Agent, Mem0, Mem1) :-
  % If no actions or goals, but there's an unexplored exit here, go that way.
  agent_thought_model(Agent, Model, Mem0),
  in_model(related(_At, Agent, Here), Model),
  in_model(related(exit(ExitName), Here, '<unexplored>'), Model),
  add_todo( goto(Agent, walk, *, ExitName), Mem0, Mem1).
autonomous_decide_action(Agent, Mem0, Mem1) :-
  % Follow player to adjacent rooms.
  agent_thought_model(Agent, Model, Mem0),
  in_model(related(_, Agent, Here), Model),
  in_model(related(_, player, There), Model),
  in_model(related(exit(ExitName), Here, There), Model),
  add_todo( goto(Agent, walk, *, ExitName), Mem0, Mem1).
autonomous_decide_action(Agent, Mem0, Mem1) :- fail,
  %%is(ZERO , ),
  call(call,(ZERO is random(5))), ZERO == 0,!,
  random_noise(Agent, Msg),
  add_todo(print_(Agent, Msg), Mem0, Mem1).
autonomous_decide_action(Agent, Mem0, Mem0) :-
  bugout('~w: Can\'t think of anything to do.~n', [Agent], autonomous).% trace.



console_decide_action(Agent, Mem0, Mem1):- 
 %thought(timestamp(T0), Mem0),
 %bugout(read_pending_codes(In,Codes,Found,Missing)),
 repeat,
 notrace((
 ttyflush,
 agent_to_input(Agent,In),
 dmust(is_stream(In)),
 setup_console,
 ensure_has_prompt(Agent),
 read_line_to_tokens(Agent, In,[], Words0), 
 (Words0==[]->(Words=[wait],makep);Words=Words0))),
 parse_command(Agent, Words, Action, Mem0),      
 !,
 if_tracing(bugout('Console TODO ~p~n', [Agent: Words->Action], telnet)),
 add_todo(Action, Mem0, Mem1), ttyflush, !.

makep:- 
 locally(set_prolog_flag(verbose_load,true),
 with_no_dmsg(make:((
  
  '$update_library_index',
 findall(File, make:modified_file(File), Reload0),
 list_to_set(Reload0, Reload),
 ( prolog:make_hook(before, Reload)
 -> true
 ; true
 ),
 print_message(silent, make(reload(Reload))),
 maplist(reload_file, Reload),
 print_message(silent, make(done(Reload))),
 ( prolog:make_hook(after, Reload)
 -> true
 ; nop(list_undefined),
  nop(list_void_declarations)
 ))))).



decide_action(Agent, Mem0, Mem0) :- 
 thought(todo([Action|_]), Mem0),
 (declared(h(_Spatial, in, Agent, Here), advstate)->true;Here=somewhere),
 (trival_act(Action)->true;bugout('~w @ ~w: already about todo: ~w~n', [Agent, Here, Action], autonomous)).

% Telnet client
decide_action(Agent, Mem0, Mem1) :-
 notrace(declared(inherits(telnet), Mem0)),!,
 dmust(telnet_decide_action(Agent, Mem0, Mem1)).

% Stdin Client
decide_action(Agent, Mem0, Mem1) :-
 thought(Agent, agent_type(console), Mem0),
 %thought(Agent, timestamp(T0), Mem0),
 ensure_has_prompt(Agent),
 agent_to_input(Agent,In),
 (tracing->catch(wait_for_input([In,user_input],Found,20),_,(nortrace,notrace,break));wait_for_input([In,user_input],Found,2)),
 (Found==[] -> (Mem0=Mem1) ;  quietly(((console_decide_action(Agent, Mem0, Mem1))))).

decide_action(Agent, Mem0, Mem3) :-
  thought(Agent, agent_type(autonomous), Mem0),
 maybe_autonomous_decide_goal_action(Agent, Mem0, Mem3).

decide_action(Agent, Mem, Mem) :-
  thought(Agent, agent_type(recorder), Mem).  % recorders don't decide much.
decide_action(Agent, Mem0, Mem0) :-
  bugout('decide_action(~w) FAILED!~n', [Agent], general).

run_agent(Agent, S0, S) :-
  undeclare(memories(Agent, Mem0), S0, S1),
  undeclare(perceptq(Agent, PerceptQ), S1, S2),
  thought(Agent, timestamp(T0), Mem0),
  T1 is T0 + 1,
  memorize(Agent, timestamp(T1), Mem0, Mem1),
  process_percept_list(Agent, PerceptQ, T1, Mem1, Mem2),
  memorize_list(Agent, PerceptQ, Mem2, Mem3),
  decide_action(Agent, Mem3, Mem4),
  declare(memories(Agent, Mem4), S2, S3),
  declare(perceptq(Agent, []), S3, S4),
  do_todo(Agent, S4, S).
run_agent(Agent, S0, S0) :-
  bugout('run_agent(~w) FAILED!~n', [Agent], general).


% --------

:- dynamic(undo/1).
undo([u, u, u, u, u, u, u, u]).
:- dynamic(advstate/1).
%advstate([]).

run_all_agents([], S0, S0).
run_all_agents([Agent|AgentTail], S0, S2) :-
  run_agent(Agent, S0, S1),
  !, % Don't allow future failure to redo successful agents.
  run_all_agents(AgentTail, S1, S2).

create_agents([], S0, S0).
create_agents([agentspec(Agent, Type)|Tail], S0, S2) :-
  create_agent(Agent, Type, S0, S1),
  create_agents(Tail, S1, S2).

init_agents(S0, S2) :-
  findall(agentspec(Agent, Type),
          getprop(Agent, agent_type(Type), S0),
          AgentList),
  create_agents(AgentList, S0, S2).

main(S0, S2) :-
  findall(Agent1, getprop(Agent1, agent_type(console), S0), AgentList1),
  findall(Agent2,
          ( getprop(Agent2, agent_type(autonomous), S0),
            ( getprop(Agent2, can_be(switched(on), t), S0) -> \+ getprop(Agent2, state(on, f), S0) ; true )
          ), AgentList2),
  append(AgentList1, AgentList2, AllAgents),
  run_all_agents(AllAgents, S0, S2),
  !. % Don't allow future failure to redo main.
main(S0, S0) :-
  bugout('main FAILED~n', general).

mainloop :-
  repeat,
    retract(advstate(S0)),
    main(S0, S1),
    asserta(advstate(S1)),
    must_output_state(S1),
    declared(quit, S1),
  !. % Don't allow future failure to redo mainloop.

% TODO: try converting this to a true "repeat" loop.
main_loop(State) :-
  declared(quit, State).
main_loop(State) :-
  declared(undo, State),
  retract(undo([_, Prev|Tail])),
  assertz(undo(Tail)),
  !,
  main_loop(Prev).
main_loop(S0) :-
  %repeat,
  retract(undo([U1, U2, U3, U4, U5, U6|_])),
  assertz(undo([S0, U1, U2, U3, U4, U5, U6])),
  run_agent(player, S0, S4),
  run_agent(floyd, S4, S5),
  %user_interact(S3, S4), !,
  %automate_agent(floyd, S4, S5),
  !,
  main_loop(S5).
main_loop(_) :-
  bugout('main_loop() FAILED!~n', general).

/*
init_logging_pro :-
  get_time(StartTime),
  convert_time(StartTime, StartTimeString),
  open('input.log', append, FH),
  format(FH, '\n==== ADVENTURE INPUT, ~w\n', [StartTimeString]),
  asserta(adv:input_log(FH)).
*/
add_look(_Agent, S1, S1).

adventure :-
  %guitracer,
  test_ordering,
  init_logging,
  (retractall(advstate(_));true),
  istate(S0),
  init_agents(S0, S1),
  %add_look(player,S1),
  %add_look(floyd),
  %act(floyd, look, S2, S3),
  S1= S3,
  asserta(advstate(S3)),
  player_format(Agent, '=============================================~n', []),
  player_format(Agent, 'Welcome to Marty\'s Prolog Adventure Prototype~n', []),
  player_format(Agent, '=============================================~n', []),
  mainloop,
  %main_loop(S3),
  adv:input_log(FH),
  close(FH),
  notrace.
adventure :-
  adv:input_log(FH),
  close(FH),
  format('adventure FAILED~n', []),
  !, fail.

:- debug.
%%:- initialization(adventure).
%:- make.
:- list_undefined([]).

