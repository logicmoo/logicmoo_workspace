:- include('../ec_test_incl').
/*

   Test A

*/

do_test(stdtest+3) :-
     testing_msg('Test 3'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t)], R).

do_test(stdtest+4) :-
     testing_msg('Test 4'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t)], R).

do_test(stdtest+5) :-
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t)], R).

do_test(stdtest+6) :-
     testing_msg('Test 6'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t)], R).

do_test(stdtest+7) :-
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t)], R).

do_test(stdtest+8) :-
     testing_msg('Test 8 - 111 sicstus'),
     abdemo_special(easy,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t)], R).

do_test(benchtest+9) :-
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t)], R).

do_test(benchtest+10+long) :-
     testing_msg('Test 10'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t)], R).

do_test(benchtest+12+long) :-
     testing_msg('Test 12'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t)], R).

do_test(benchtest+14+long) :-
     testing_msg('Test 14'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t),
          holds_at(have(o13),t), holds_at(have(o14),t)], R).

do_test(benchtest+16+long) :-
     testing_msg('Test 16'),
     abdemo_special(long,[holds_at(have(o1),t), holds_at(have(o2),t),
          holds_at(have(o3),t), holds_at(have(o4),t),
          holds_at(have(o5),t), holds_at(have(o6),t),
          holds_at(have(o7),t), holds_at(have(o8),t),
          holds_at(have(o9),t), holds_at(have(o10),t),
          holds_at(have(o11),t), holds_at(have(o12),t),
          holds_at(have(o13),t), holds_at(have(o14),t),
          holds_at(have(o15),t), holds_at(have(o16),t)], R).








axiom(initiates(go(X),at(X),T),[]).

axiom(terminates(go(X),at(Y),T),[diff(X,Y)]).

axiom(initiates(buy(X),have(X),T),[sells(Y,X), holds_at(at(Y),T)]).

axiom(sells(s1,o1),[]).

axiom(sells(s2,o2),[]).

axiom(sells(s3,o3),[]).

axiom(sells(s4,o4),[]).

axiom(sells(s5,o5),[]).

axiom(sells(s6,o6),[]).

axiom(sells(s7,o7),[]).

axiom(sells(s8,o8),[]).

axiom(sells(s9,o9),[]).

axiom(sells(s10,o10),[]).

axiom(sells(s11,o11),[]).

axiom(sells(s12,o12),[]).

axiom(sells(s13,o13),[]).

axiom(sells(s14,o14),[]).

axiom(sells(s15,o15),[]).

axiom(sells(s16,o16),[]).






/* Abduction policy */

abducible(dummy).

executable(go(X)).

executable(buy(X)).



