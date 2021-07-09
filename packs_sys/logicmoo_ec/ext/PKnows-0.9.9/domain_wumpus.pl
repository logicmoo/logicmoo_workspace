%
%  Domain-specific definitions.
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This axiomatisation is for the "multi-agent hunt the wumpus"" domain from
%  the paper "Asynchronous Knowledge with Hidden Actions in the Situation
%  Calculus" by Ryan F Kelly and Adrian R Pearce.
%

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).

:- multifile(adp_fluent/3).

% Enumerate the values of the various object types in the domain

agent(ann).
agent(bob).

room(room1).
room(room2).
room(room3).
room(room4).
room(room5).
room(room6).
room(room7).
room(room8).
room(room9).

adjacent(room1, room2).
adjacent(room1, room4).
adjacent(room2, room1).
adjacent(room2, room3).
adjacent(room2, room5).
adjacent(room3, room2).
adjacent(room3, room6).
adjacent(room4, room1).
adjacent(room4, room5).
adjacent(room4, room7).
adjacent(room5, room2).
adjacent(room5, room4).
adjacent(room5, room6).
adjacent(room5, room8).
adjacent(room6, room3).
adjacent(room6, room5).
adjacent(room6, room9).
adjacent(room7, room4).
adjacent(room7, room8).
adjacent(room8, room5).
adjacent(room8, room7).
adjacent(room8, room9).
adjacent(room9, room6).
adjacent(room9, room8).

% Enumerates primitive actions, and the types of their arguments.

prim_action(move(agent, room)).
prim_action(shoot(agent, room)).
prim_action(alert(agent)).

% Enumerates primitive observation terms, and the types of their arguments.

prim_observation(move(agent, room)).
prim_observation(steps).
prim_observation(shoot(agent, room)).
prim_observation(alert).
prim_observation(stench).
prim_observation(scream).

% Enumerates primitive fluents, and types of arguments

prim_fluent(in(agent, room)).
prim_fluent(wumpus(room)).
prim_fluent(stench(room)).
prim_fluent(adjacent(room, room)).
prim_fluent(killed).

% Definitions for action description predicate fluents

% Possibility.

adp_fluent(poss, move(Agt, Room),
  ext([R], in(Agt, R) & adjacent(R, Room))
).

adp_fluent(poss, shoot(Agt, Room),
  ext([R], in(Agt, R) & adjacent(R, Room))
).

adp_fluent(poss, alert(Agt),
  ext([R], in(Agt, R) & stench(R))
).

% Observations.

adp_fluent(obs(Agt, move(Agt1, Room1)), move(Agt2, Room2),
  % Full move action is observable if...
  (Room1=Room2) & (Agt1=Agt2) & (
      % the observer is in the destination room
      in(Agt, Room1)
    |
      % or the observer is in the source room.
      ext([Room3], in(Agt, Room3) & in(Agt1, Room3))
  )
).

adp_fluent(obs(Agt, steps), move(Agt1, Room1),
  % Footsteps can be heard when someone moves if...
  ext([Room2], (in(Agt, Room2) & (
      % the observer is adjacent to the destination room
      adjacent(Room2, Room1)
    |
      % or the observer is adjacent to the source room.
      ext([Room3], in(Agt1, Room3) & adjacent(Room2, Room3))
  )))
).

adp_fluent(obs(Agt, shoot(Agt1, Room1)), shoot(Agt2, Room2),
  % Full shoot action is observable if...
  (Room1=Room2) & (Agt1=Agt2) & (
      % the observer is in the target room
      in(Agt, Room1)
    |
      % or the observer is in the source room.
      ext([Room3], in(Agt, Room3) & in(Agt1, Room3))
  )
).

adp_fluent(obs(Agt, alert), alert(Agt1),
  % An alert can be heard if...
  ext([Room, Room1], (
    in(Agt, Room) & in(Agt1, Room1) & (
        % the observer is in the same room as the announcer
        (Room = Room1)
      |
        % or the observer is in a room adjacent to the announcer
        adjacent(Room, Room1)
    )
  ))
).

adp_fluent(obs(Agt, stench), move(Agt1, Room1),
  % A stench is observed by anyone entering a stench-filled room.
  (Agt=Agt1) & stench(Room1)
).

adp_fluent(obs(_, scream), shoot(_, Room1),
  % Everyone hears the scream if the wumpus is shot.
  wumpus(Room1)
).


% Causal rules for each fluent/action combo

causes_true(in(Agt, Room), move(Agt2, Room2),
  (Agt=Agt2) & (Room=Room2)
).

causes_false(in(Agt, Room), move(Agt2, Room2),
  (Agt=Agt2) & ~(Room=Room2)
).

causes_true(killed, shoot(_, Room),
  wumpus(Room)
).


%  Specify what holds in the initial situation.

% The wumpus is in room five, but this is not known to the agents.
initially(wumpus(room5)).

% The agents are known to start in the entry room.
initially_known(in(Agt, room1)) :-
  agent(Agt).

% The wumpus is known to start out alive.
initially_known(~killed).

%  Specify the constraints.
%  These are true in all situations and hence are common knowledge.
%  They form the background theory of the domain.

% There's nothing suspicious about the entry room.
constraint(~wumpus(room1)).
constraint(~stench(room1)).

% The cave layout is fixed, according to adjacency definitions.
constraint(adjacent(R1, R2)) :-
  room(R1), room(R2), adjacent(R1, R2).
constraint(~adjacent(R1, R2)) :-
  room(R1), room(R2), \+ adjacent(R1, R2).

% Exactly one room contains the wumpus.
constraint(all([R1,R2], (wumpus(R1) & wumpus(R2)) => (R1=R2))).
constraint(ext([R], wumpus(R))).

% Each agent is in exactly one room.
% XXX TODO: including these tips the scales into too much exponential
% branching during proof search.
%constraint(all([R1,R2], (in(Agt, R1) & in(Agt, R2)) => (R1=R2))) :-
%  agent(Agt).
%constraint(ext([R], in(Agt, R))) :-
%  agent(Agt).

% A room has a stench iff the wumpus is in that room or in an adjacent one.
constraint(all([R1], (
  stench(R1) => (wumpus(R1) | ext([R2], (wumpus(R2) & adjacent(R1, R2))))
))).
constraint(all([R1], (
  (wumpus(R1) | ext([R2], (wumpus(R2) & adjacent(R1, R2)))) => stench(R1)
))).


%
%  And now for the unit tests...
%

:- begin_tests(domain_wumpus,[sto(rational_trees)]).

test(sanity1) :-
  domain_prove(~wumpus(room1)),
  domain_prove(all([R], adjacent(room1, R) => ~wumpus(R))),
  %domain_prove(in(bob, room1) => ~in(bob, room2)),
  true.

test(example0) :-
  holds(in(ann, room1), s0),
  holds(in(bob, room1), s0),
  %holds(knows(ann, in(bob, room1)), s0),
  %holds(~ext([X], knows(ann, wumpus(X))), s0),
  %holds(knows(ann, ~wumpus(room1)), s0),
  %holds(knows(ann, ~wumpus(room2)), s0),
  %holds(knows(ann, ~wumpus(room4)), s0),
  true. %\+ holds(knows(ann, ~wumpus(room5)), s0).

:- end_tests(domain).
