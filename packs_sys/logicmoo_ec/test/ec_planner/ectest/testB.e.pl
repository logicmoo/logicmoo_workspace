:- include('../ec_test_incl').
/*
   Test B
*/


do_test(test2) :-
     testing_msg('Test 2'),
     abdemo_special(easy,[holds_at(f2,t)], R).

do_test(test4) :-
     testing_msg('Test 4'),
     abdemo_special(easy,[holds_at(f4,t)], R).

do_test(test6) :-
     testing_msg('Test 6'),
     abdemo_special(easy,[holds_at(f6,t)], R).

do_test(test8) :-
     testing_msg('Test 8'),
     abdemo_special(easy,[holds_at(f8,t)], R).

do_test(test10) :-
     testing_msg('Test 10'),
     abdemo_special(easy,[holds_at(f10,t)], R).

do_test(test12) :-
     testing_msg('Test 12'),
     abdemo_special(easy,[holds_at(f12,t)], R).

do_test(test14) :-
     testing_msg('Test 14'),
     abdemo_special(easy,[holds_at(f14,t)], R).

do_test(test16) :-
     testing_msg('Test 16'),
     abdemo_special(easy,[holds_at(f16,t)], R).






axiom(initiates(a1,f1,T),[]).

axiom(initiates(a2,f2,T),[holds_at(f1,T)]).

axiom(initiates(a3,f3,T),[holds_at(f2,T)]).

axiom(initiates(a4,f4,T),[holds_at(f3,T)]).

axiom(initiates(a5,f5,T),[holds_at(f4,T)]).

axiom(initiates(a6,f6,T),[holds_at(f5,T)]).

axiom(initiates(a7,f7,T),[holds_at(f6,T)]).

axiom(initiates(a8,f8,T),[holds_at(f7,T)]).

axiom(initiates(a9,f9,T),[holds_at(f8,T)]).

axiom(initiates(a10,f10,T),[holds_at(f9,T)]).

axiom(initiates(a11,f11,T),[holds_at(f10,T)]).

axiom(initiates(a12,f12,T),[holds_at(f11,T)]).

axiom(initiates(a13,f13,T),[holds_at(f12,T)]).

axiom(initiates(a14,f14,T),[holds_at(f13,T)]).

axiom(initiates(a15,f15,T),[holds_at(f14,T)]).

axiom(initiates(a16,f16,T),[holds_at(f15,T)]).





/* Abduction policy */

abducible(dummy).

executable(a1).

executable(a2).

executable(a3).

executable(a4).

executable(a5).

executable(a6).

executable(a7).

executable(a8).

executable(a9).

executable(a10).

executable(a11).

executable(a12).

executable(a13).

executable(a14).

executable(a15).

executable(a16).

