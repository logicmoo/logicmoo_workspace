:- include('../ec_test_incl').


/*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(got(Agnt,p1)),T)],R).

*/

do_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(got(Agnt,p1)),T)],R).


/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(shift_pack(Agnt,P,R),T1,T4),
     [happens(retrieve_pack(Agnt,P),T1,T2), before(T2,T3),
     happens(deliver_pack(Agnt,P,R),T3,T4),
     not(clipped(T2,got(Agnt,P),T3))]).

axiom(initiates(shift_pack(Agnt,P,R),inRoom(P,R),T),[]).


axiom(happens(retrieve_pack(Agnt,P),T1,T2),
     [holds_at(inRoom(P,R),T1), happens(go_to_room(Agnt,R),T1),
     happens(pick_up(Agnt,P),T2), before(T1,T2),
     not(clipped(T1,inRoom(Agnt,R),T2))]).

axiom(initiates(retrieve_pack(Agnt,P),got(Agnt,P),T),[]).


axiom(happens(deliver_pack(Agnt,P,R),T1,T2),
     [happens(go_to_room(Agnt,R),T1),
     happens(put_down(Agnt,P),T2), before(T1,T2),
     not(clipped(T1,inRoom(Agnt,R),T2))]).

axiom(initiates(deliver_pack(Agnt,P,R),inRoom(P,R),T),[holds_at(got(Agnt,P),T)]).



/* Primitive actions */


axiom(initiates(pick_up(Agnt,P),got(Agnt,P),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).

axiom(releases(pick_up(Agnt,P),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).


axiom(initiates(put_down(Agnt,P),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(got(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).

axiom(terminates(put_down(Agnt,P),got(Agnt,P),T),[]).


axiom(initiates(go_to_room(Agnt,R),inRoom(Agnt,R),T),[]).

axiom(terminates(go_to_room(Agnt,R1),inRoom(Agnt,R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,Agnt), holds_at(got(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).



/* Narrative */


axiom(initially(inRoom(Agnt,r1)),[]).

axiom(initially(neg(inRoom(Agnt,r2))),[]).

axiom(initially(neg(inRoom(Agnt,r3))),[]).

axiom(initially(inRoom(p1,r2)),[]).

axiom(initially(neg(inRoom(p1,r1))),[]).

axiom(initially(neg(inRoom(p1,r3))),[]).



/* Abduction policy */


abducible(dummy).

executable(pick_up(Agnt,P)).

executable(put_down(Agnt,P)).

executable(go_to_room(Agnt,R)).


:- run_tests.

% :- halt.
