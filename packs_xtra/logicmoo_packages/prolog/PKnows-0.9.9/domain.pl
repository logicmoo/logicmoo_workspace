%
%  Domain-specific definitions.
%
%  Copyright 2008, Ryan Kelly
%
%  This axiomatisation is for the "party invitation" domain from the
%  paper "Common Knowledge, Hidden Actions, and the Frame Problem", as
%  well as Ryan's thesis "Asynchronous Multi-Agent Reasoning in the
%  Situation Calculus".
%

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).

% Enumerate the values of the various types in the domain
agent(ann).
agent(bob).

location(c).
location(d).

result(R) :-
  location(R).

action(nil).
action(A) :-
    action_with_vars(A,Vs),
    enumerate_vars(Vs).

observation(nil).
observation(A) :-
    action_with_vars(A,Vs),
    enumerate_vars(Vs).
observation(pair(A,R)) :-
    A = read(Agt), agent(Agt), location(R).

object(O) :-
    agent(O) ; location(O) ; result(O) ; observation(O).

% Enumerates primitive actions, and the types of their arguments.
prim_action(read(agent)).
prim_action(leave(agent)).
prim_action(enter(agent)).

% Enumerates primitive fluents, and types of arguments
prim_fluent(loc(location)).
prim_fluent(inroom(agent)).

% Definitions for action description predicate fluents
adp_fluent(poss,read(Agt),inroom(Agt)).
adp_fluent(poss,enter(Agt),~inroom(Agt)).
adp_fluent(poss,leave(Agt),inroom(Agt)).

adp_fluent(canObs(Agt),read(_),inroom(Agt)).
adp_fluent(canObs(_),leave(_),true).
adp_fluent(canObs(_),enter(_),true).

adp_fluent(canSense(Agt),read(Agt2),Agt=Agt2).
adp_fluent(canSense(_),leave(_),false).
adp_fluent(canSense(_),enter(_),false).

adp_fluent(sr(R),read(_),loc(R)).
adp_fluent(sr(ok),leave(_),true).
adp_fluent(sr(ok),enter(_),true).

% Causal rules for each fluent/action combo
causes_true(inroom(Agt),enter(Agt2),(Agt=Agt2)).
causes_false(inroom(Agt),leave(Agt2),(Agt=Agt2)).

%  Specify what holds in the initial situation.
initially(loc(c)).
initially(~loc(d)).
initially(inroom(ann)).
initially(inroom(bob)).

% Specify what is common knowledge in the initial situation
initially(pknows0((ann | bob)*,P)) :-
    P = inroom(ann) ; P = inroom(bob)
    ; P = (loc(c) <=> ~loc(d))
    ; P = ~knows(ann,loc(c)) ; P = ~knows(bob,loc(c))
    ; P = ~knows(ann,loc(d)) ; P = ~knows(bob,loc(d)).


%
%  And now for the unit tests...
%

:- begin_tests(domain,[sto(rational_trees)]).

test(reg1) :-
    regression(inroom(ann),read(bob),inroom(ann)).
test(reg2) :-
    regression(inroom(ann),enter(bob),inroom(ann)).
test(reg3) :-
    regression(inroom(ann),leave(bob),inroom(ann)).
test(reg4) :-
    regression(inroom(ann),enter(ann),true).
test(reg5) :-
    regression(inroom(ann),leave(ann),false).


test(adp1) :-
    adp_fluent(canObs(ann),read(ann),inroom(ann)).
test(adp2) :-
    adp_fluent(canObs(ann),read(bob),inroom(ann)).

test(holds1) :-
    holds(inroom(ann),s0), !.
test(holds2,fail) :-
    holds(~inroom(ann),s0).
test(holds3) :-
    holds(~inroom(ann),do(leave(ann),s0)), !.
test(holds4) :-
    holds(inroom(ann),do(leave(bob),s0)), !.
test(holds5) :-
    holds(?([X:agent] : inroom(X)),do(leave(bob),s0)), !.
test(holds6) :-
    holds(!([X:agent] : inroom(X)),s0), !.
test(holds7) :-
    \+ holds(!([X:agent] : inroom(X)),do(leave(bob),s0)).
test(holds8) :-
    holds(loc(c),s0), !.


test(knows1) :-
    holds(knows(ann,inroom(ann)),s0), !.
test(knows2) :-
    holds(knows(ann,inroom(bob)),s0), !.
test(knows3) :-
    holds(knows(bob,~inroom(ann)),do(leave(ann),s0)), !.
test(knows4) :-
    holds(~knows(bob,inroom(ann)),do(leave(ann),s0)), !.
test(knows5) :-
    holds(~knows(bob,loc(c)),s0), !.
test(knows6) :-
    \+ holds(knows(bob,loc(c)),s0).

test(path1) :-
    domain_tautology(pknows0(ann | bob,loc(c)) <=> (knows(ann,loc(c)) & knows(bob,loc(c)))).
test(path2) :-
    domain_tautology(pknows0(!(X:agent) ; ?(inroom(X)),loc(c)) <=> ((inroom(ann) | inroom(bob)) => loc(c))).
test(path3) :-
    P = pknows0(ann,loc(c)),
    regression(P,nil,R),
    domain_tautology(R => P), !.
%test(path4) :-
%    P = pknows0(ann*,loc(c)),
%    regression(P,nil,R),
%    domain_tautology(R => P), !.

test(pknows1) :-
    holds(pknows(ann,inroom(ann)),s0), !.
test(pknows2) :-
    holds(pknows(bob,~inroom(ann)),do(leave(ann),s0)), !.
test(pknows3) :-
    holds(pknows((ann | bob)*,inroom(ann)),s0), !.

%
%  Examples from the thesis
%
test(example1) :-
    holds(~ ?([L:location]:knows(ann,loc(L))),s0), !.
test(example2) :-
    holds(knows(bob,loc(c)),do(read(bob),s0)), !.
test(example3) :-
    holds(knows(bob,~knows(ann,loc(c))),s0).
test(example4) :-
    holds(~ knows(bob,~ knows(ann,loc(c))),do(leave(bob),s0)), !.

%test(example5) :-
%    holds(pknows((ann | bob)*,~knows(ann,loc(c))),s0), !.
%test(example6) :-
%    holds(~pknows((ann | bob)*,loc(c)),do(read(bob),s0)), !.
%test(example7) :-
%    holds(pknows((ann | bob)*,?([X:location] : knows(bob,loc(X)))),do(read(bob),s0)), !.
%test(example8) :-
%    holds(pknows((ann | bob)*,loc(c)),do(read(ann),do(read(bob),s0))), !.

:- end_tests(domain).

