%
%  Domain-specific definitions.
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This axiomatisation is for the "party invitation"" domain from the
%  PhD thesis "Asynchronous Multi-Agent Reasoning in the Situation Calculus"
%  by Ryan F Kelly.
%

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).

:- multifile(adp_fluent/3).

% Enumerate the values of the various object types in the domain

agent(ann).
agent(bob).

location(cathys_house).
location(dannys_house).

% Enumerates primitive actions, and the types of their arguments.

prim_action(read(agent)).
prim_action(leave(agent)).
prim_action(enter(agent)).

% Enumerates primitive observation terms, and the types of their arguments.

prim_observation(read(agent)).
prim_observation(leave(agent)).
prim_observation(enter(agent)).
prim_observation(party_at(location)).

% Enumerates primitive fluents, and types of arguments

prim_fluent(inroom(agent)).
prim_fluent(party_at(location)).

% Definitions for action description predicate fluents

% Possibility.

adp_fluent(poss, read(Agt), inroom(Agt)).
adp_fluent(poss, enter(Agt), ~inroom(Agt)).
adp_fluent(poss, leave(Agt), inroom(Agt)).

% Observations.

adp_fluent(obs(Agt, read(Agt1)), read(Agt2), (Agt1=Agt2) & (Agt1=Agt | inroom(Agt))).
adp_fluent(obs(_, enter(Agt1)), enter(Agt2), Agt1=Agt2).
adp_fluent(obs(_, leave(Agt1)), leave(Agt2), Agt1=Agt2).
adp_fluent(obs(Agt, party_at(Loc)), read(Agt1), (Agt=Agt1) & party_at(Loc)).

% Causal rules for each fluent/action combos

causes_true(inroom(Agt1), enter(Agt2), Agt1=Agt2).
causes_false(inroom(Agt1), leave(Agt2), Agt1=Agt2).

%  Specify what holds in the initial situation.

initially(party_at(cathys_house)).
initially(~party_at(dannys_house)).
initially(~knows(Agt, party_at(Loc))) :-
    agent(Agt), location(Loc).
initially(knows(Agt1, ~knows(Agt2, party_at(Loc)))) :-
    agent(Agt1), agent(Agt2), location(Loc).

initially_known(inroom(ann)).
initially_known(inroom(bob)).
initially_known(party_at(cathys_house) | party_at(davids_house)).

%
%  And now for the unit tests...
%

:- begin_tests(domain_party, [sto(rational_trees)]).

test(reg1) :-
    regression(inroom(ann), read(bob), inroom(ann)).
test(reg2) :-
    regression(inroom(ann), enter(bob), inroom(ann)).
test(reg3) :-
    regression(inroom(ann), leave(bob), inroom(ann)).
test(reg4) :-
    regression(inroom(ann), enter(ann), true).
test(reg5) :-
    regression(inroom(ann), leave(ann), false).

test(adp1) :-
    adp_fluent(obs(ann, read(ann)), O, O=read(ann)).
test(adp2) :-
    adp_fluent(obs(ann, read(bob)), O, inroom(ann) & (O=read(bob))).
test(adp3) :-
    adp_fluent(pbu(ann), read(bob), inroom(bob)& ~inroom(ann)).

test(holds1) :-
    holds(inroom(ann), s0).
test(holds2, fail) :-
    holds(~inroom(ann), s0).
test(holds3) :-
    holds(~inroom(ann), do(leave(ann), s0)), !.
test(holds4) :-
    holds(inroom(ann), do(leave(bob), s0)), !.
test(holds5) :-
    holds(ext([X], inroom(X)), do(leave(bob), s0)), !.
test(holds6) :-
    holds(all([X], inroom(X)), s0).
test(holds7) :-
    \+ holds(all([X], inroom(X)), do(leave(bob), s0)).
test(holds8) :-
    holds(party_at(cathys_house), s0).

test(knows1) :-
    holds(knows(ann, inroom(ann)), s0), !.
test(knows2) :-
    holds(knows(ann, inroom(bob)), s0), !.
test(knows3) :-
    holds(knows(bob, ~inroom(ann)), do(leave(ann), s0)), !.
test(knows4) :-
    holds(~knows(bob, inroom(ann)), do(leave(ann), s0)), !.
test(knows6) :-
    \+ holds(knows(bob, party_at(cathys_house)), s0).

test(example1) :-
    % Initially, ann does not know where the party is.
    holds(~ ext([L], knows(ann, party_at(L))), s0), !.
test(example2) :-
    % Bob knows the true location of the party after reading the invite.
    holds(knows(bob, party_at(cathys_house)), do(read(bob), s0)), !.
test(example3) :-
    % Bob knows that ann does not know where the party is.
    holds(knows(bob, ~knows(ann, party_at(cathys_house))), s0).
test(example4) :-
    % After leaving the room, bob no longer knows that
    % ann does not know where the party is.
    holds(~knows(bob, ~knows(ann, party_at(cathys_house))),
          do(leave(bob), s0)), !.

:- end_tests(domain_party).
