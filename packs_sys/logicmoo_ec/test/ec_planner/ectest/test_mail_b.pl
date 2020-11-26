:- include('../ec_test_incl').

        /*

   Formulae for the mail delivery domain.

   Example queries:

   :- abdemo([holds_at(inRoom(p1,r3),T)],R).

   :- abdemo([holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(Agnt,p1)),T)],R).

*/

do_test(mail1)   :- abdemo_special(loops,[holds_at(inRoom(p1,r2),t)],R).
do_test(mail2)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),t)],R).
do_test(mail2T)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T)],R).
do_test(mail3)   :- abdemo_special(loops,[holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(agent(1),p1)),T)],R).
do_test(mail4)   :- abdemo_special(loops,[holds_at(inRoom(p2,r3),T),holds_at(inRoom(p1,r3),T),holds_at(neg(hasGot(agent(1),p1)),T)],R).
do_test(mail5)   :- abdemo_special(loops,[holds_at(inRoom(p2,r3),T),
                                          holds_at(inRoom(p1,r3),T),
                                          holds_at(neg(hasGot(agent(1),p1)),T),holds_at(hasGot(agent(1),p2),T)],R).

/*
<<<<< ENDOF OUTPUT of mail5
'T'=t71.
'R' :-

    [
      [
        [ happens(do(agent(1), putdown(p1)), t79, t79),
          happens(do(A, retrieve_pack(p1)), t78, t78),
          happens(do(A, deliver_pack(p1, r3)), t77, t77),
          happens(do(agent(1), retrieve_pack(p2)), t72, t72),
          happens(do(agent(1), deliver_pack(p2, r3)), t69, t69)
        ],

        [ b(t79, t78),
          b(t79, t71),
          b(t78, t77),
          b(t77, t71),
          b(t72, t69),
          b(t69, t71),
          b(t, end),
          b(start, t)
        ]
      ],
      [[], -]
    ].

!!!PASSED!!! mail5 time=0.038

*/

do_test(mail6)   :- abdemo_special(loops,[holds_at(inRoom(p2,r3),T),
                                          holds_at(inRoom(p1,r3),T),
                                          happens(do(A, deliver_pack(p1, r1)), T),
                                          holds_at(neg(hasGot(agent(1),p1)),T),holds_at(hasGot(agent(1),p2),T)],R).



/*
<<<<< ENDOF OUTPUT of mail6
'T'=t90.
'A'=_.
'R' :-

    [
      [
        [ happens(do(agent(1), putdown(p1)), t98, t98),
          happens(do(_, deliver_pack(p1, r1)), t90, t90),
          happens(do(A, retrieve_pack(p1)), t97, t97),
          happens(do(A, deliver_pack(p1, r3)), t96, t96),
          happens(do(agent(1), retrieve_pack(p2)), t91, t91),
          happens(do(agent(1), deliver_pack(p2, r3)), t88, t88)
        ],

        [ b(t98, t97),
          b(t98, t90),
          b(t97, t96),
          b(t96, t90),
          b(t91, t88),
          b(t88, t90),
          b(t, end),
          b(start, t)
        ]
      ],
      [[], -]
    ].

!!!PASSED!!! mail6 time=0.02
*/
/* There should probably be some releases clauses for compound actions */

/* Compound actions */


axiom(happens(do(Agnt,shift_pack(P,R)),T,T+3),
     [happens(do(Agnt,retrieve_pack(P)),T,T+1),      
      happens(do(Agnt,deliver_pack(P,R)),T+2,T+3),
     not(clipped(T+1,hasGot(Agnt,P),T+2))]).

axiom(initiates(do(Agnt,shift_pack(P,R)),inRoom(P,R),T),
    [inRoom(Agnt,Somewhere), Somewhere \= R]).


axiom(happens(do(Agnt,retrieve_pack(P)),T,T+1),
     [holds_at(inRoom(P,R),T), happens(do(Agnt,go_to_room(R)),T),
      happens(do(Agnt,pickup(P)),T+1), 
      not(clipped(T,inRoom(Agnt,R),T+1))]).

axiom(initiates(do(Agnt,retrieve_pack(P)),hasGot(Agnt,P),T),[]).


axiom(happens(do(Agnt,deliver_pack(P,R)),T,T+1),
     [happens(do(Agnt,go_to_room(R)),T),
      happens(do(Agnt,putdown(P)),T+1),
      not(clipped(T,inRoom(Agnt,R),T+1))]).

axiom(initiates(do(Agnt,deliver_pack(P,R)),inRoom(P,R),T),
     [holds_at(hasGot(Agnt,P),T)]).



/* Primitive actions */


axiom(initiates(do(Agnt,pickup(P)),hasGot(Agnt,P),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).

axiom(releases(do(Agnt,pickup(P)),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(inRoom(P,R),T), holds_at(inRoom(Agnt,R),T)]).


axiom(initiates(do(Agnt,putdown(P)),inRoom(P,R),T),
     [diff(P,Agnt), holds_at(hasGot(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).

axiom(terminates(do(Agnt,putdown(P)),hasGot(Agnt,P),T),[]).

axiom(initiates(do(Agnt,go_to_room(R)),inRoom(Agnt,R),T),[]).

axiom(terminates(do(Agnt,go_to_room(R1)),inRoom(Agnt,R2),T),[diff(R1,R2)]).



/* Domain constraints */


axiom(holds_at(inRoom(P,R),T),
     [diff(P,Agnt), holds_at(hasGot(Agnt,P),T), holds_at(inRoom(Agnt,R),T)]).



/* Narrative */


axiom(neg(inRoom(Obj,R2)),[diff(R1,R2),inRoom(Obj,R1)]).


initially(inRoom(agent(1),r1)).
initially(inRoom(p1,r2)).
initially(inRoom(p2,r2)).
initially(inRoom(p3,r2)).




/* Abduction policy */


abducible(dummy).

/*
executable(do(Agnt,pickup(P))).

executable(do(Agnt,putdown(P))).

executable(do(Agnt,go_to_room(R))).
*/
executable(do(Agn,Act)).

:- listing(ec_current_domain_db).

:- run_tests.

:- halt.
