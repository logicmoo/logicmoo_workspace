:- include('../ec_test_incl').

        /*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(do(Agnt,got,p1)),T)],R).

*/

do_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(do(Agnt,got,p1)),T)],R).


/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(do(Agnt,shift_pack,P,R),T1,T4),
     [happens(do(Agnt,retrieve_pack,P),T1,T2), before(T2,T3),
      happens(do(Agnt,deliver_pack,P,R),T3,T4),
     not(clipped(T2,do(Agnt,got,P),T3))]).

axiom(initiates(do(Agnt,shift_pack,P,R),inRoom(P,R),T),[]).


axiom(happens(do(Agnt,retrieve_pack,P),T1,T2),
     [holds_at(inRoom(P,R),T1), happens(do(Agnt,go_to_room,R),T1),
      happens(do(Agnt,pickup,P),T2), before(T1,T2),
      not(clipped(T1,do(Agnt,atRoom,R),T2))]).

axiom(initiates(do(Agnt,retrieve_pack,P),do(Agnt,got,P),T),[]).


axiom(happens(do(Agnt,deliver_pack,P,R),T1,T2),
     [happens(do(Agnt,go_to_room,R),T1),
      happens(do(Agnt,putdown,P),T2), before(T1,T2),
      not(clipped(T1,do(Agnt,atRoom,R),T2))]).

axiom(initiates(do(Agnt,deliver_pack,P,R),inRoom(P,R),T),[holds_at(do(Agnt,got,P),T)]).



/* Primitive actions */


axiom(initiates(do(Agnt,pickup,P),do(Agnt,got,P),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(do(Agnt,atRoom,R),T)]).

axiom(releases(do(Agnt,pickup,P),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(do(Agnt,atRoom,R),T)]).


axiom(initiates(do(Agnt,putdown,P),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(do(Agnt,got,P),T), holds_at(do(Agnt,atRoom,R),T)]).

axiom(terminates(do(Agnt,putdown,P),do(Agnt,got,P),T),[]).


axiom(initiates(do(Agnt,go_to_room,R),do(Agnt,atRoom,R),T),[]).

axiom(terminates(do(Agnt,go_to_room,R1),do(Agnt,atRoom,R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,Agnt), holds_at(do(Agnt,got,P),T), holds_at(do(Agnt,atRoom,R),T)]).



/* Narrative */


axiom(initially(do(Agnt,atRoom,r1)),[]).

axiom(initially(neg(do(Agnt,atRoom,r2))),[]).

axiom(initially(neg(do(Agnt,atRoom,r3))),[]).

axiom(initially(inRoom(p1,r2)),[]).

axiom(initially(neg(inRoom(p1,r1))),[]).

axiom(initially(neg(inRoom(p1,r3))),[]).



/* Abduction policy */


abducible(dummy).

/*
executable(do(Agnt,pickup,P)).

executable(do(Agnt,putdown,P)).

executable(do(Agnt,go_to_room,R)).
*/
executable(do(Agnt,Act,R)).

:- run_tests.

:- halt.
