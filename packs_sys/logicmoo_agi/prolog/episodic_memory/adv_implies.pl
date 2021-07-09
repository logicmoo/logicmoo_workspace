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
:- '$set_source_module'(mu).
:- op(500, fx, ~).



:- discontiguous(implications/4).

implications(Knower, attempts(_Agent, Action), Preconds, Effects):- nonvar(Action), !, implications(Knower, Action, Preconds, Effects).
implications(Knower, did(_Agent, Action), Preconds, Effects):- nonvar(Action), !, implications(Knower, Action, Preconds, Effects).
implications(Knower, doing(_Agent, Action), Preconds, Effects):- nonvar(Action), !, implications(Knower, Action, Preconds, Effects).



only_goto:- fail, true.

:- discontiguous(oper_db/4).
% oper_db(_Knower, Action, Preconds, Effects)

% used by oper_splitk/4
oper_db(Knower, attempts(Agent, Action), Preconds, Effects):- nonvar(Action), !, ignore(Knower=Agent), oper_db(Agent, Action, Preconds, Effects).
oper_db(Knower, did(Agent, Action), Preconds, Effects):- nonvar(Action), !, ignore(Knower=Agent), oper_db(Agent, Action, Preconds, Effects).
oper_db(Knower, doing(Agent, Action), Preconds, Effects):- nonvar(Action), !, ignore(Knower=Agent), oper_db(Agent, Action, Preconds, Effects).

oper_db(Knower, act3(Verb, Agent, Args),  Preconds, Effects):- Knower \== Agent, (var(Knower) ; var(Agent)),  
  must_or_rtrace((Knower = Agent, Knower \== Agent)), !,
  oper_db(Knower, act3(Verb, Agent, Args),  Preconds, Effects).

oper_db(Knower, Action, Preconds, Effects):- fail, % Hooks to KR above
  sequenced(Knower, Whole),
 append(Preconds, [did(Action)|Effects], Whole).

oper_db(_Knower, ( act3('wait', _Agent,[])), [], []).




oper_db(_Knower, ( act3('put__via', Doer, [Manner, Mover, IntoDest, Dest])),

  %dshow_failure(prep_to_rel(Thing2, Prep, At)),
  % dshow_failure((At \= in ; \+ is_closed(At, Thing2))),
  %dshow_failure(has_rel(At, Thing2)),
  %dshow_failure(will_need_touch(Agent, Thing2)), % what if "under" an "untouchable" thing?

   [ FromLoc \= Mover, Dest \= Mover, % FromLoc \= Dest,
    h(spatial, FromPrep, Mover, FromLoc) % must be hhh/3 ?
    %h(spatial, fn(exit, ExitName), FromLoc, Dest), h(spatial, fn(exit, ReverseExit), Dest, FromLoc),
    %b(fn(exit, ExitName), FromLoc, _),    
    %ReverseExit \= ExitName,
    ],
   [ event( event3('depart', [ FromPrep, Mover, FromLoc], [Manner, Doer])),
     event( event3('arrive', [ IntoDest, Mover, Dest], [Manner, Doer]))]).
     % ~h(spatial, FromPrep, Mover, FromLoc)).


oper_db(_Knower, event3('depart', [ IntoDest, Mover, FromLoc], Details),
 [],
 [ ~h(spatial, IntoDest, Mover, FromLoc),
   percept_local(FromLoc, event3('depart', [ IntoDest, Mover, FromLoc], Details))]).

oper_db(_Knower, event3('arrive', [ IntoDest, Mover, Dest], Details), 
 [],
 [ 
  ~h(spatial, _, Mover, _),
   h(spatial, IntoDest, Mover, Dest),
   percept_local(Dest, event3('arrive', [ IntoDest, Mover, Dest], Details))]).

oper_db(_Knower, ( act3('go__dir',Mover,[ Manner, ExitName])),
   [ FromLoc \= Mover, Dest \= Mover, % FromLoc \= Dest,
    h(spatial, IntoDest, Mover, FromLoc), % must be hhh/3 ?
    h(spatial, fn(exit, ExitName), FromLoc, Dest),
    h(spatial, fn(exit, ReverseExit), Dest, FromLoc),
    b(fn(exit, ExitName), FromLoc, _),    
    ReverseExit \= ExitName,
    []],
   [
     % implies believe(Mover, ~h(spatial, in, Mover, FromLoc)),
  event( event3('depart', [ IntoDest, Mover, FromLoc], [Manner, ExitName])),
 %~h(spatial, IntoDest, Mover, FromLoc),
   b(fn(exit, ExitName), FromLoc, Dest),
   b(fn(exit, ReverseExit), Dest, FromLoc),
   % implies, believe(Mover, h(spatial, in, Mover, Dest)),
  %h(spatial, IntoDest, Mover, Dest),
   event( event3('arrive', [ IntoDest, Mover, Dest], [Manner, ExitName]))
   %  ~b(IntoDest, Mover, FromLoc),
   %   b(IntoDest, Mover, Dest),
     % Dest \= FromLoc
     ]):- dif(ExitName, escape).

% Return an operator after substituting Agent for Agent.
oper_db(_Knower, ( act3('go__dir',Agent,[ _Manner, ExitName])),
     [ b(in, Agent, FromLoc),
       b(fn(exit, ExitName), FromLoc, Dest),
       FromLoc \= Agent, Dest \= Agent, FromLoc \= Dest
       ], % path(FromLoc, Dest)
     [ ~b(in, Agent, FromLoc),
        b(in, Agent, Dest)
     ]):- fail.


oper_db(_Knower, ( act3('go__dir',Agent,[ Manner, Escape])),
     [ Object \= Agent, FromLoc \= Agent,
       k(FromPrep, Agent, Object),
       h(spatial, IntoDest, Object, FromLoc),
       Object \= FromLoc
     ],
     [
 percept_local(Object, event3('depart', [ FromPrep, Agent, Object],[ Manner, Escape])),
        % implies believe(Agent, ~h(spatial, in, Agent, Object)),
 percept_local(FromLoc, event3('arrive', [ IntoDest, Agent, FromLoc], [Manner, EscapedObject]))
        % implies, believe(Agent, h(spatial, in, Agent, FromLoc))
     ]) :- escape_rel(Escape), EscapedObject = escaped, \+ only_goto.


% Looking causes Percepts
oper_db(_Knower, looky(Agent),
     [ FromLoc \= Agent,
       % believe(Agent, h(spatial, _, Agent, _)),
       h(spatial, Sub, Agent, FromLoc)
       ],
     [ foreach(
         (h(spatial, Sub, Child, FromLoc), must_mw1(h(spatial, At, Child, Where))),
             percept(Agent, h(spatial, At, Child, Where))) ] ) :- \+ only_goto.



% the World agent has a *goal that no events go unhandled
oper_db(world, invoke_events(FromLoc),
     [ percept_local(FromLoc, Event)],
     [ ~percept_local(FromLoc, Event),
       foreach((h(spatial, in, Agent, FromLoc), 
          prop(Agent, inherited(perceptq))),
       percept(Agent, Event))]):- \+ only_goto.


% deducer Agents who preceive leavers from some exit believe the did_depart point is an exit
oper_db(_Knower, percept(Agent, event3('depart',[ IntoDest, Mover, FromLoc], [Manner, ExitName])),
 [ did(Mover, ( act3('go__dir', Mover, [ Manner, ExitName]))),
       prop(Agent, inherited(deducer)),
       h(spatial, IntoDest, Agent, FromLoc) ],
     [ believe(Agent, h(spatial, fn(exit, ExitName), FromLoc, _)),
       believe(Agent, prop(Mover, inherited(actor)))]):- \+ only_goto.

% deducer Agents who preceive arivers from some entrance believe the entry location is an exit
oper_db(_Knower, percept(Agent, event3('arrive',[ IntoDest, Mover, FromLoc], [Manner, ExitName])),
 [ did(Mover, ( act3('go__dir', Mover, [ Manner, ExitName]))),
       prop(Agent, inherited(deducer)),
       believe(Agent, h(spatial, IntoDest, Agent, FromLoc)) ],

     [ believe(Agent, h(spatial, fn(exit, ExitName), FromLoc, _)),
 believe(Agent, did(Mover, ( act3('go__dir', Mover, [ Manner, ExitName])))), % also belive someone did soething to make it happen
       believe(Agent, h(spatial, IntoDest, Mover, FromLoc)),
       believe(Agent, prop(Mover, inherited(actor)))]):- \+ only_goto.

% deducer Agents who preceive arivers from some entrance believe the entry location is an exit
oper_db(_Knower, percept(Agent, event3('arrive', [ IntoDest, Mover, FromLoc],[ Manner, ExitName])),
 [ did(Mover, ( act3('go__dir', Mover, [ Manner, ExitName]))),
       isa(Agent, deducer),
       b(Agent,
 [ percept_local(Dest, event3('depart', [IntoDest, Mover, Dest],[Manner, EnterName])),
                in(Agent, FromLoc)]) ],
     [ b(Agent,
                [fn(exit, ExitName, FromLoc, Dest),
      did(Mover, ( act3('go__dir', Mover, [ Manner, EnterName]))),
                in(Mover, FromLoc),
                isa(Mover, actor)])]):- \+ only_goto.

% ~h(spatial, Prep, '<mystery>'(closed, Prep, Object), Object)

% hhh = is really true
% b = is belived
% k = is belived and really true
oper_db(_Knower, ( act3('take',Agent,[ Thing ])), % from same room
  [ Thing \= Agent, exists(Thing),
    Dest \= Agent,
   % TODO k(takeable, Agent, Thing),
   h(spatial, At, Thing, Dest)
  ],
  [ ~ k(At, Thing, Dest),
      moves( At, Thing, Dest, take, held_by, Thing, Agent),
      k(held_by, Thing, Agent)]):- \+ only_goto.

oper_db(_Knower, ( act3('drop',Agent,[ Thing])),
  [ Thing \= Agent, exists(Thing),
      k(held_by, Thing, Agent),
      k(At, Agent, Where)],
  [ ~ h(spatial, held_by, Thing, Agent),
      moves(held_by, Thing, Agent, drop, At, Thing, Where),
      k(At, Thing, Where)] ):- \+ only_goto.

oper_db(_Knower, ( act3('put',Agent,[ Thing, Relation, Where])), % in somewhere
  [ Thing \= Agent, exists(Thing), exists(Where),
      k(held_by, Thing, Agent),
      h(touchable, Agent, Where),
      has_rel(Relation, Where),
    ~ is_closed(Relation, Where)],
  [ ~ k(held_by, Thing, Agent),
      moves(held_by, Thing, Agent, put, Relation, Thing, Where),
      k(Relation, Thing, Where)] ):- \+ only_goto.


oper_db(_Knower, ( act3('give',Agent,[ Thing, Recipient])), % in somewhere
  [ Thing \= Agent, Recipient \= Agent,
      exists(Thing), exists(Recipient),
      k(held_by, Thing, Agent),
      h(touchable, Agent, Recipient),
      h(touchable, Recipient, Agent)],
  [ ~ k(held_by, Thing, Agent),
      moves(held_by, Thing, Agent, give, held_by, Thing, Recipient),
      k(held_by, Thing, Recipient)] ):- \+ only_goto.

oper_db(_Knower, ( act3('tell',Agent,[ Player, [ please, give, Recipient, the(Thing)]])),
    [   Recipient \= Player, Agent \= Player,
        Thing \= Agent, Thing \= Recipient, Thing \= Player,
        exists(Thing), exists(Recipient), exists(Player),
        k(held_by, Thing, Player),
        h(touchable, Player, Recipient),
        h(touchable, Recipient, Player)],
    [ ~ k(held_by, Thing, Player),
        moves(held_by, Thing, Player, give, held_by, Thing, Recipient),
        k(held_by, Thing, Recipient)] ):- \+ only_goto.

