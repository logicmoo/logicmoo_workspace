

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_packs)).

:- include(library(ec_planner/ec_test_incl)).

/*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(got(robot,p1)),T)],R).

*/

do_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(got(robot,p1)),T)],R).


/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(shift_pack(robot,P,R),T1,T4),
     [happens(retrieve_pack(robot,P),T1,T2), before(T2,T3),
     happens(deliver_pack(robot,P,R),T3,T4),
     not(clipped(T2,got(robot,P),T3))]).

axiom(initiates(shift_pack(robot,P,R),inRoom(P,R),T),[]).


axiom(happens(retrieve_pack(robot,P),T1,T2),
     [holds_at(inRoom(P,R),T1), happens(go_to_room(robot,R),T1),
     happens(pick_up(robot,P),T2), before(T1,T2),
     not(clipped(T1,inRoom(robot,R),T2))]).

axiom(initiates(retrieve_pack(robot,P),got(robot,P),T),[]).


axiom(happens(deliver_pack(robot,P,R),T1,T2),
     [happens(go_to_room(robot,R),T1),
     happens(put_down(robot,P),T2), before(T1,T2),
     not(clipped(T1,inRoom(robot,R),T2))]).

axiom(initiates(deliver_pack(robot,P,R),inRoom(P,R),T),[holds_at(got(robot,P),T)]).



/* Primitive actions */


axiom(initiates(pick_up(robot,P),got(robot,P),T),
     [diff(P,robot), holds_at(inRoom(P,R),T), holds_at(inRoom(robot,R),T)]).

axiom(releases(pick_up(robot,P),inRoom(P,R),T),
     [diff(P,robot), holds_at(inRoom(P,R),T), holds_at(inRoom(robot,R),T)]).


axiom(initiates(put_down(robot,P),inRoom(P,R),T),
     [diff(P,robot), holds_at(got(robot,P),T), holds_at(inRoom(robot,R),T)]).

axiom(terminates(put_down(robot,P),got(robot,P),T),[]).


axiom(initiates(go_to_room(robot,R),inRoom(robot,R),T),[]).

axiom(terminates(go_to_room(robot,R1),inRoom(robot,R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,robot), holds_at(got(robot,P),T), holds_at(inRoom(robot,R),T)]).



/* Narrative */


axiom(initially(inRoom(robot,r1)),[]).

axiom(initially(neg(inRoom(robot,r2))),[]).

axiom(initially(neg(inRoom(robot,r3))),[]).

axiom(initially(inRoom(p1,r2)),[]).

axiom(initially(neg(inRoom(p1,r1))),[]).

axiom(initially(neg(inRoom(p1,r3))),[]).



/* Abduction policy */


abducible(dummy).

executable(pick_up(robot,P)).

executable(put_down(robot,P)).

executable(go_to_room(robot,R)).


%:- run_tests.

%:- halt.
