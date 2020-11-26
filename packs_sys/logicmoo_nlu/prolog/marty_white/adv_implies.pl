/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Feb 20, 2020 - Andrew Dougherty
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
%
*/
:- op(500, fx, ~).



:- discontiguous(implications/4).

implications(does, go_dir(Agent, Walk, ExitName),
     [ h(In, Agent, Here), h(exit(ExitName), Here, There) ],
     [ event(moving_in_dir(Agent, Walk, ExitName, In, Here, In, There)) ]):- fail.


implications(event, moving_in_dir(Object, Manner, ExitName, From, Here, To, There),
     [ Here \= There, h(exit(ExitName), Here, There), h(exit(ReverseExit), There, Here) ],
     [  event(departing(Object, From, Here, Manner, ExitName)),
        event(arriving(Object, To, There, Manner, ReverseExit))
        ]).

implications(event, departing(Agent, In, There, _Walk, ExitName),
    [ h(In, Agent, There), h(exit(ExitName), There, _)], [~h(In, Agent, There)]).

implications(event, arriving(Agent, In, Here, _Walk, ReverseExit),
    [~h(In, Agent, Here), h(exit(ReverseExit), Here, _)], [ h(In, Agent, Here)]).


only_goto:- fail, true.

:- discontiguous(oper_db/4).
% oper_db(_Self, Action, Preconds, Effects)
oper_db(Self, Action, Preconds, Effects):- fail, % Hooks to KR above
  sequenced(Self, Whole),
 append(Preconds, [did(Action)|Effects], Whole).

oper_db(Agent, do_nothing(Agent), [], []).

oper_db(Agent, go_dir(Agent, Walk, ExitName),
   [ Here \= Agent, There \= Agent, Here \= There,
    k(In, Agent, Here),
    b(exit(ExitName), Here, _),
    h(exit(ExitName), Here, There),
    ReverseExit \= ExitName,
    h(exit(ReverseExit), There, Here)],
   [
     % implies believe(Agent, ~h(in, Agent, Here)),
     percept_local(Here, departing(Agent, In, Here, Walk, ExitName)),
   ~h(In, Agent, Here),
    h(In, Agent, There),
   %b(exit(ExitName), Here, There),
   %b(exit(ReverseExit), There, Here),
   % implies, believe(Agent, h(in, Agent, There)),
    percept_local(There, arriving(Agent, In, There, Walk, ReverseExit))
   %  ~b(In, Agent, Here),
   %   b(In, Agent, There),
     % There \= Here
     ]):- dif(ExitName, escape).


% Return an operator after substituting Agent for Agent.
oper_db(Agent, go_dir(Agent, _Walk, ExitName),
     [ b(in, Agent, Here),
       b(exit(ExitName), Here, There),
       Here \= Agent, There \= Agent, Here \= There
       ], % path(Here, There)
     [ ~b(in, Agent, Here),
        b(in, Agent, There)
     ]):- fail.

% equiv(percept_local(Here, departing(Agent, In, Here, Walk, ExitName)))  ~h(in, Agent, Here)

oper_db(Agent, go_dir(Agent, Walk, Escape),
     [ Object \= Agent, Here \= Agent,
       k(OldIn, Agent, Object),
       h(NewIn, Object, Here),
       Object \= Here
     ],
     [
        percept_local(Object, departing(Agent, OldIn, Object, Walk, Escape)),
        % implies believe(Agent, ~h(in, Agent, Object)),
       ~k(OldIn, Agent, Object),
        k(NewIn, Agent, Here),
        percept_local(Here, arriving(Agent, NewIn, Here, Walk, EscapedObject))
        % implies, believe(Agent, h(in, Agent, Here))
     ]) :- Escape = escape, EscapedObject = escaped, \+ only_goto.


% Looking causes Percepts
oper_db(Agent, looky(Agent),
     [ Here \= Agent,
       % believe(Agent, h(_, Agent, _)),
       h(Sub, Agent, Here)
       ],
     [ foreach(
         (h(Sub, Child, Here), must_mw1(h(At, Child, Where))),
             percept(Agent, h(At, Child, Where))) ] ) :- \+ only_goto.



% the World agent has a *goal that no events go unhandled
oper_db(world, handle_events(Here),
     [ percept_local(Here, Event)],
     [ ~percept_local(Here, Event),
       foreach((h(in, Agent, Here), prop(Agent, inherited(perceptq))), percept(Agent, Event))]):- \+ only_goto.


% deducer Agents who preceive leavers from some exit believe the departing point is an exit
oper_db(Agent, percept(Agent, departing(Someone, In, Here, Walk, ExitName)),
     [ did(go_dir(Someone, Walk, ExitName)),
       prop(Agent, inherited(deducer)),
       h(In, Agent, Here) ],
     [ believe(Agent, h(exit(ExitName), Here, _)),
       believe(Agent, prop(Someone, inherited(actor)))]):- \+ only_goto.

% deducer Agents who preceive arivers from some entrance believe the entry location is an exit
oper_db(Agent, percept(Agent, arriving(Someone, In, Here, Walk, ExitName)),
     [ did(go_dir(Someone, Walk, ExitName)),
       prop(Agent, inherited(deducer)),
       believe(Agent, h(In, Agent, Here)) ],

     [ believe(Agent, h(exit(ExitName), Here, _)),
       believe(Agent, did(go_dir(Someone, Walk, ExitName))), % also belive someone did soething to make it happen
       believe(Agent, h(In, Someone, Here)),
       believe(Agent, prop(Someone, inherited(actor)))]):- \+ only_goto.

% deducer Agents who preceive arivers from some entrance believe the entry location is an exit
oper_db(Agent, percept(Agent, arriving(Someone, In, Here, Walk, ExitName)),
     [ did(go_dir(Someone, Walk, ExitName)),
       isa(Agent, deducer),
       b(Agent,
                [percept_local(There, departing(Someone, In, There, Walk, EnterName)),
                in(Agent, Here)]) ],
     [ b(Agent,
                [exit(ExitName, Here, There),
                did(go_dir(Someone, Walk, EnterName)),
                in(Someone, Here),
                isa(Someone, actor)])]):- \+ only_goto.

% ~h(Prep, '<mystery>'(closed, Prep, Object), Object)

% h = is really true
% b = is belived
% k = is belived and really true
oper_db(Agent, take(Agent, Thing), % from same room
  [ Thing \= Agent, exists(Thing),
    There \= Agent,
   k(takeable, Agent, Thing),
   h(At, Thing, There)
  ],
  [ ~ k(At, Thing, There),
      moves( At, Thing, There, take, held_by, Thing, Agent),
      k(held_by, Thing, Agent)]):- \+ only_goto.

oper_db(Agent, drop(Agent, Thing),
  [ Thing \= Agent, exists(Thing),
      k(held_by, Thing, Agent),
      k(At, Agent, Where)],
  [ ~ h(held_by, Thing, Agent),
      moves(held_by, Thing, Agent, drop, At, Thing, Where),
      k(At, Thing, Where)] ):- \+ only_goto.

oper_db(Agent, put(Agent, Thing, Relation, Where), % in somewhere
  [ Thing \= Agent, exists(Thing), exists(Where),
      k(held_by, Thing, Agent),
      k(touchable, Agent, Where),
      has_rel(Relation, Where),
    ~ is_closed(Relation, Where)],
  [ ~ k(held_by, Thing, Agent),
      moves(held_by, Thing, Agent, put, Relation, Thing, Where),
      k(Relation, Thing, Where)] ):- \+ only_goto.


oper_db(Agent, give(Agent, Thing, Recipient), % in somewhere
  [ Thing \= Agent, Recipient \= Agent,
      exists(Thing), exists(Recipient),
      k(held_by, Thing, Agent),
      k(touchable, Agent, Recipient),
      k(touchable, Recipient, Agent)],
  [ ~ k(held_by, Thing, Agent),
      moves(held_by, Thing, Agent, give, held_by, Thing, Recipient),
      k(held_by, Thing, Recipient)] ):- \+ only_goto.

oper_db(Agent, tell(Agent, Player, [please, give, Recipient, the(Thing)]),
    [   Recipient \= Player, Agent \= Player,
        Thing \= Agent, Thing \= Recipient, Thing \= Player,
        exists(Thing), exists(Recipient), exists(Player),
        k(held_by, Thing, Player),
        k(touchable, Player, Recipient),
        k(touchable, Recipient, Player)],
    [ ~ k(held_by, Thing, Player),
        moves(held_by, Thing, Player, give, held_by, Thing, Recipient),
        k(held_by, Thing, Recipient)] ):- \+ only_goto.

