/*

   ABDUCTIVE EVENT CALCULUS

   MURRAY SHANAHAN

   Version 4.2
   Stripped down, cut-free version, without comments

   Written for LPA MacProlog 32

*/

abdemo(Gs,R) :- init_gensym(t), abdemo(Gs,[[],[]],R,[],N).


abdemo([],R,R,N,N).


abdemo([holds_at(F1,T)|Gs1],R1,R3,N1,N4) :-
     F1 \= neg(F2), abresolve(initially(F1),R1,Gs2,R1),
     append(Gs2,Gs1,Gs3), add_neg([clipped(0,F1,T)],N1,N2),
     abdemo_naf([clipped(0,F1,T)],R1,R2,N2,N3),
     abdemo(Gs3,R2,R3,N3,N4).

abdemo([holds_at(F1,T3)|Gs1],R1,R5,N1,N4) :-
     F1 \= neg(F2), abresolve(initiates(A,F1,T1),R1,Gs2,R1),
     abresolve(happens(A,T1,T2),R1,[],R2),
     abresolve(before(T2,T3),R2,[],R3),
     append(Gs2,Gs1,Gs3),
     add_neg([clipped(T1,F1,T3)],N1,N2),
     abdemo_nafs(N2,R3,R4,N2,N3),
     abdemo(Gs3,R4,R5,N3,N4).

abdemo([holds_at(neg(F),T)|Gs1],R1,R3,N1,N4) :-
     abresolve(initially(neg(F)),R1,Gs2,R1),
     append(Gs2,Gs1,Gs3), add_neg([declipped(0,F,T)],N1,N2),
     abdemo_naf([declipped(0,F,T)],R1,R2,N2,N3),
     abdemo(Gs3,R2,R3,N3,N4).

abdemo([holds_at(neg(F),T3)|Gs1],R1,R5,N1,N4) :-
     abresolve(terminates(A,F,T1),R1,Gs2,R1),
     abresolve(happens(A,T1,T2),R1,[],R2),
     abresolve(before(T2,T3),R2,[],R3),
     append(Gs2,Gs1,Gs3),
     add_neg([declipped(T1,F,T3)],N1,N2),
     abdemo_nafs(N2,R3,R4,N2,N3),
     abdemo(Gs3,R4,R5,N3,N4).

abdemo([G|Gs1],R1,R3,N1,N2) :-
     abresolve(G,R1,Gs2,R2), append(Gs2,Gs1,Gs3),
     abdemo(Gs3,R2,R3,N1,N2).


abresolve(terms_or_rels(A,F,T),R,Gs,R) :- axiom(releases(A,F,T),Gs).

abresolve(terms_or_rels(A,F,T),R,Gs,R) :- axiom(terminates(A,F,T),Gs).

abresolve(inits_or_rels(A,F,T),R,Gs,R) :- axiom(releases(A,F,T),Gs).

abresolve(inits_or_rels(A,F,T),R,Gs,R) :- axiom(initiates(A,F,T),Gs).

abresolve(happens(A,T),R1,Gs,R2) :- abresolve(happens(A,T,T),R1,Gs,R2).

abresolve(happens(A,T1,T2),[HA,BA],[],[HA,BA]) :- member(happens(A,T1,T2),HA).

abresolve(happens(A,T,T),[HA,BA],[],[[happens(A,T,T)|HA],BA]) :-
     executable(A), skolemise(T).

abresolve(before(X,Y),R,[],R) :- demo_before(X,Y,R).

abresolve(before(X,Y),R1,[],R2) :-
     \+ demo_before(X,Y,R1), \+ demo_beq(Y,X,R1), add_before(X,Y,R1,R2).

abresolve(diff(X,Y),R,[],R) :- X \= Y.

abresolve(G,R,Gs,R) :- axiom(G,Gs).


abdemo_nafs([],R,R,N,N).

abdemo_nafs([N|Ns],R1,R3,N1,N3) :-
     abdemo_naf(N,R1,R2,N1,N2), abdemo_nafs(Ns,R2,R3,N2,N3).

abdemo_naf([clipped(T1,F,T4)|Gs1],R1,R2,N1,N2) :-
     findall(Gs3,
          (abresolve(terms_or_rels(A,F,T2),R1,Gs2,R1),
          abresolve(happens(A,T2,T3),R1,[],R1),
          append([before(T1,T3),before(T2,T4)|Gs2],Gs1,Gs3)),Gss),
     abdemo_nafs(Gss,R1,R2,N1,N2).

abdemo_naf([declipped(T1,F,T4)|Gs1],R1,R2,N1,N2) :-
     findall(Gs3,
          (abresolve(inits_or_rels(A,F,T2),R1,Gs2,R1),
          abresolve(happens(A,T2,T3),R1,[],R1),
          append([before(T1,T3),before(T2,T4)|Gs2],Gs1,Gs3)),Gss),
     abdemo_nafs(Gss,R1,R2,N1,N2).

abdemo_naf([holds_at(F1,T)|Gs],R1,R2,N1,N2) :-
     opposite(F1,F2), abdemo([holds_at(F2,T)],R1,R2,N1,N2).

abdemo_naf([holds_at(F,T)|Gs],R1,R2,N1,N2) :-
     abdemo_naf(Gs,R1,R2,N1,N2).

abdemo_naf([before(X,Y)|Gs],R,R,N,N) :- X = Y.

abdemo_naf([before(X,Y)|Gs],R,R,N,N) :- X \= Y, demo_before(Y,X,R).

abdemo_naf([before(X,Y)|Gs],R1,R2,N1,N2) :-
     X \= Y, \+ demo_before(Y,X,R1),
     abdemo_naf(Gs,R1,R2,N1,N2).

abdemo_naf([before(X,Y)|Gs],R1,R2,N,N) :-
     X \= Y, \+ demo_before(Y,X,R1),
     \+ demo_beq(X,Y,R1), add_before(Y,X,R1,R2).

abdemo_naf([G|Gs1],R,R,N,N) :-
     G \= clipped(T1,F,T2), G \= declipped(T1,F,T2), G \= holds_at(F,T),
     G \= before(X,Y), \+ abresolve(G,R,Gs2,R).

abdemo_naf([G1|Gs1],R1,R2,N1,N2) :-
     G1 \= clipped(T1,F,T2), G1 \= declipped(T1,F,T2),
     G1 \= holds_at(F,T), G1 \= before(X,Y),
     findall(Gs3,(abresolve(G1,R1,Gs2,R1),append(Gs2,Gs1,Gs3)),Gss),
     Gss \= [], abdemo_nafs(Gss,R1,R2,N1,N2).


demo_before(X,Y,[HA,BA]) :- demo_before(X,Y,BA,[]).

demo_before(0,Y,R,L) :- Y \= 0.

demo_before(X,Y,R,L) :- X \= 0, member(before(X,Y),R).

demo_before(X,Y,R,L) :- X \= 0, \+ member(before(X,Y),R), member(X,L).

demo_before(X,Y,R,L) :-
     X \= 0, \+ member(before(X,Y),R), \+ member(X,L),
     member(before(X,Z),R), demo_before(Z,Y,R,[X|L]).


demo_beq(X,X,R).

demo_beq(X,Y,R) :- X \= Y, demo_before(X,Y,R).


add_before(X,Y,[HA,BA]) :- member(before(X,Y),BA).

add_before(X,Y,[HA,BA],[HA,[before(X,Y)|BA]]) :-
     \+ member(before(X,Y),BA), \+ demo_beq(Y,X,[HA,BA]).


add_neg(N,Ns,Ns) :- member(N,Ns).

add_neg(N,Ns,[N|Ns]) :- \+ member(N,Ns).


skolemise(T) :- gensym(t,T).


opposite(neg(F),F).

opposite(F1,neg(F1)) :- F1 \= neg(F2).




/*

   Formulae for the shopping example from Russell and Norvig

*/

test3 :-
     nl, writeln('Test 3'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T)], [[],[]], R).

test4 :-
     nl, writeln('Test 4'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T)], [[],[]], R).

test5 :-
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T)], [[],[]], R).

test6 :-
     nl, writeln('Test 6'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T)], [[],[]], R).

test7 :-
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T)], [[],[]], R).

test8 :-
     nl, writeln('Test 8'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T)], [[],[]], R).

test9 :-
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T),
          holds_at(have(o9),T)], [[],[]], R).

test10 :-
     nl, writeln('Test 10'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T),
          holds_at(have(o9),T), holds_at(have(o10),T)], [[],[]], R).

test12 :-
     nl, writeln('Test 12'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T),
          holds_at(have(o9),T), holds_at(have(o10),T),
          holds_at(have(o11),T), holds_at(have(o12),T)], [[],[]], R).

test14 :-
     nl, writeln('Test 14'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T),
          holds_at(have(o9),T), holds_at(have(o10),T),
          holds_at(have(o11),T), holds_at(have(o12),T),
          holds_at(have(o13),T), holds_at(have(o14),T)], [[],[]], R).

test16 :-
     nl, writeln('Test 16'),
     abdemo([holds_at(have(o1),T), holds_at(have(o2),T),
          holds_at(have(o3),T), holds_at(have(o4),T),
          holds_at(have(o5),T), holds_at(have(o6),T),
          holds_at(have(o7),T), holds_at(have(o8),T),
          holds_at(have(o9),T), holds_at(have(o10),T),
          holds_at(have(o11),T), holds_at(have(o12),T),
          holds_at(have(o13),T), holds_at(have(o14),T),
          holds_at(have(o15),T), holds_at(have(o16),T)], [[],[]], R).







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




test2_2 :-
     nl, writeln('test2_ 2'),
     abdemo([holds_at(f2,T)], [[],[]], R).

test2_4 :-
     nl, writeln('test2_ 4'),
     abdemo([holds_at(f4,T)], [[],[]], R).

test2_6 :-
     nl, writeln('test2_ 6'),
     abdemo([holds_at(f6,T)], [[],[]], R).

test2_8 :-
     nl, writeln('test2_ 8'),
     abdemo([holds_at(f8,T)], [[],[]], R).

test2_10 :-
     nl, writeln('test2_ 10'),
     abdemo([holds_at(f10,T)], [[],[]], R).

test2_12 :-
     nl, writeln('test2_ 12'),
     abdemo([holds_at(f12,T)], [[],[]], R).

test2_14 :-
     nl, writeln('test2_ 14'),
     abdemo([holds_at(f14,T)], [[],[]], R).

test2_16 :-
     nl, writeln('test2_ 16'),
     abdemo([holds_at(f16,T)], [[],[]], R).






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

init_gensym(_).

ticks(Z1):-  statistics(runtime,[Z1,_]).
abdemo(A,_B,C):- abdemo(A,C),writeq(C).
abdemo(Gs,R) :-
     ticks(Z1), abdemo(Gs,[[],[]],R,[],N), ticks(Z2),
     Z is (Z2-Z1)/60, write('Total time taken '), writeln(Z), nl.

/*
abdemo(Gs, [HA, BA]) :-
     init_gensym(t), ticks(Z1),
     abdemo_top(Gs, [[[], []], [[], []]], [[HA, HC], [BA, BC]], [], N, 0),
     ticks(Z2), Z is (Z2-Z1)/60, write('Total time taken '), writeln(Z), nl.
abdemo_top(Gs, R1, R3, N1, N3, D) :-
     abdemo(Gs, R1, R2, N1, N2).
     */
