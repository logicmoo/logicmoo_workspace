/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may Be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/




/*
:- op(1001,xfy,...).
:- op(1101,xfx,'--->').
:- op(500,fx,+).
:- op(500,fx,-).
*/
terminal(T,S,S,x(_,terminal,T,X),X).
%   writef("           terminal -- SUCCEEDED1 for T= {0} S= {1}", [T,S])
%.
terminal(T,[T|S],S,X,X) :-
%   writef("           terminal -- SUCCEEDED2 for T= {0} S= {1}", [T,S]),
   gap(X).
%terminal(A,B,C,D,E) :-
%   writef("           terminal -- FAILED for T= {0} S= {1}", [A,B]), !, fail.

gap(x(gap,_,_,_)).
gap([]).

virtual(NT,x(_,nonterminal,NT,X),X).


xg_and(G1,G2, B, C, D, E) :- phraseXG(G1, B, C, D, E), phraseXG(G2, B, C, D, E).

phraseXG0((G1 ; G2), B, C, D, E) :- !, (phraseXG(G1, B, C, D, E); phraseXG(G2, B, C, D, E)).
phraseXG0('&'(G1 , G2), B, C, D, E) :- !, xg_and(G1,G2, B, C, D, E).
phraseXG0((A , B), C, D, E, F) :- !, phraseXG(A, C, G, E, H), phraseXG(B, G, D, H, F).

phraseXGF((_ , _)).
phraseXGF((_ ; _)).
phraseXGF('&'(_ , _)).

phraseXG(P,A1,A2,A3,A4):- var(P),!, freeze(P,phraseXG(P,A1,A2,A3,A4)).
phraseXG(P,A1,A2,A3,A4):- phraseXGF(P),!,phraseXG0(P,A1,A2,A3,A4).
phraseXG([A | B], C, D, E, F) :- !, phraseXGL([A | B], C, D, E, F).
phraseXG([], X, X, Y, Y) :- !.
phraseXG(R, S0,S,H0,H):- expandrhs(R,S0,S,H0,H,Q),!,call(Q).
phraseXG(P,A1,A2,A3,A4):- !, apply(P,[A1,A2,A3,A4]).

phraseXGL(Var,S0,S,H0,H):- var(Var),!, freeze(Var,phraseXGL(Var,S0,S,H0,H)).
phraseXGL([], X, X, Y, Y).
phraseXGL([A | B], C, D, E, F) :- !, terminal(A, C, G, E, H), phraseXGL(B, G, D, H, F).

phraseXG(P,A1,A2):- (var(P)->gen_xg_heads(P);true),
  swich_on_var(A1,(sync_w2_l(A1,W1),sync_w2_l(A2,W2)),phraseXG(P,W1,W2,Y,Y)).

swich_on_var(A1,G1,G2):- nonvar(A1),!,call(G1),call(G2).
swich_on_var(_, G1,G2):- call(G2),call(G1).

%sync_w2_l(A,A):- !.
sync_w2_l(A,B):- (A==[];B==[]),!,A=B.
sync_w2_l(A,B):- var(A),var(B),!,freeze(A,sync_w2_l(A,B)),freeze(B,sync_w2_l(A,B)).
sync_w2_l([A|AA],[W2|BB]):- sync_w2(A,W2),!, sync_w2_l(AA,BB).

sync_w2(A,W2):- A==W2,!.
sync_w2(A,W2):- var(A),var(W2),!,freeze(A,sync_w2(A,W2)),freeze(W2,sync_w2(A,W2)).
sync_w2(A,W2):- atom(W2),!,W2=A.
sync_w2(A,W2):- compound(A),!,W2=A.
sync_w2(A,W2):- compound(W2),!,arg(1,W2,A).
sync_w2(A,W2):- atom(A),!,W2=w(A,_).



%sentence80(Tree,Question,[],[],[])
/*
phraseXG(P,A1,A2,A3,A4):-
   safe_univ(P,[F|Args0]),
  % dtrace,
   conc_gx(Args0,[A1,A2,A3,A4],Args),
   Q=..[F|Args], 
   call(Q).
*/
