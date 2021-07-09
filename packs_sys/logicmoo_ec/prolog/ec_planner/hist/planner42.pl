/*

   ABDUCTIVE EVENT CALCULUS

   MURRAY SHANAHAN

   Version 4.2
   Stripped down, cut-free version, without comments

   Written for LPA MacProlog 32

*/

:- include(ec_common).


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


