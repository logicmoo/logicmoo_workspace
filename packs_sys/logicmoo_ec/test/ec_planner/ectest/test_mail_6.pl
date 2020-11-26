:- include('../ec_test_incl').

        /*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(agent(N),p1)),T)],R).

*/

do_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(agent(N),p1)),T)],R).


/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(do(agent(N),shift_pack,P,R),T1,T4),
     [happens(do(agent(N),retrieve_pack,P),T1,T2), before(T2,T3),
      happens(do(agent(N),deliver_pack,P,R),T3,T4),
     not(clipped(T2,hasGot(agent(N),P),T3))]).

axiom(initiates(do(agent(N),shift_pack,P,R),inRoom(P,R),T),[]).


axiom(happens(do(agent(N),retrieve_pack,P),T1,T2),
     [holds_at(inRoom(P,R),T1), happens(do(agent(N),go_to_room,R),T1),
      happens(do(agent(N),pickup,P),T2), before(T1,T2),
      not(clipped(T1,inRoom(agent(N),R),T2))]).

axiom(initiates(do(agent(N),retrieve_pack,P),hasGot(agent(N),P),T),[]).


axiom(happens(do(agent(N),deliver_pack,P,R),T1,T2),
     [happens(do(agent(N),go_to_room,R),T1),
      happens(do(agent(N),putdown,P),T2), before(T1,T2),
      not(clipped(T1,inRoom(agent(N),R),T2))]).

axiom(initiates(do(agent(N),deliver_pack,P,R),inRoom(P,R),T),[holds_at(hasGot(agent(N),P),T)]).



/* Primitive actions */


axiom(initiates(do(agent(N),pickup,P),hasGot(agent(N),P),T),
     [diff(P,agent(N)), holds_at(inRoom(P,R),T), holds_at(inRoom(agent(N),R),T)]).

axiom(releases(do(agent(N),pickup,P),inRoom(P,R),T),
     [diff(P,agent(N)), holds_at(inRoom(P,R),T), holds_at(inRoom(agent(N),R),T)]).


axiom(initiates(do(agent(N),putdown,P),inRoom(P,R),T),
     [diff(P,agent(N)), holds_at(hasGot(agent(N),P),T), holds_at(inRoom(agent(N),R),T)]).

axiom(terminates(do(agent(N),putdown,P),hasGot(agent(N),P),T),[]).

axiom(initiates(do(agent(N),go_to_room,R),inRoom(agent(N),R),T),[]).

axiom(terminates(do(agent(N),go_to_room,R1),inRoom(agent(N),R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,agent(N)), holds_at(hasGot(agent(N),P),T), holds_at(inRoom(agent(N),R),T)]).



/* Narrative */


axiom(initially(inRoom(agent(N),r1)),[]).

axiom(initially(neg(inRoom(agent(N),r2))),[]).

axiom(initially(neg(inRoom(agent(N),r3))),[]).

axiom(initially(inRoom(p1,r2)),[]).

axiom(initially(neg(inRoom(p1,r1))),[]).

axiom(initially(neg(inRoom(p1,r3))),[]).



/* Abduction policy */


abducible(dummy).

/*
executable(do(agent(N),pickup,P)).

executable(do(agent(N),putdown,P)).

executable(do(agent(N),go_to_room,R)).
*/
executable(do(agent(N),Act,R)).

:- run_tests.

:- halt.
