% Selectionne les 2 faces cachees
% haut,bas - dev,der - gau,droi
 select([F1,F2,F3,F4,F5,F6],[F3,F4,F5,F6]).
 select([F1,F2,F3,F4,F5,F6],[F1,F2,F5,F6]).
 select([F1,F2,F3,F4,F5,F6],[F1,F2,F3,F4]).
% Retourner le cube 2 memes faces cachees
 turn(X,X).
 turn([Dev,Der,Gau,Droi],[Der,Dev,Gau,Droi]).

% Rotations possibles vers la droite 0,90,180,270
 rotate(X,X).
 rotate([Dev,Der,Gau,Droi], [Gau,Droi,Der,Dev]).
 rotate([Dev,Der,Gau,Droi], [Der,Dev,Droi,Gau]).
 rotate([Dev,Der,Gau,Droi], [Droi,Gau,Dev,Der]).

% Pour un cube, 24 pos. differentes
 sol1(Cube,Pos) :- select(Cube,F4), turn(F4,Pos).
 sol(Cube,Pos) :- sol1(Cube,Pos1), rotate(Pos1,Pos).

 insanity(C1,C2,C3,C4) :- 
      sol1(C1,L1),
      sol(C2,L2), are_dif(L1,L2),
      sol(C3,L3), are_dif(L1,L3), are_dif(L2,L3),
      sol(C4,L4), are_dif(L1,L4), are_dif(L2,L4), are_dif(L3,L4),
      write(L1),nl,
      write(L2),nl,
      write(L3),nl,
      write(L4).

 are_dif([],[]).
 are_dif([T1|Q1],[T2|Q2]) :- T1\==T2, are_dif(Q1,Q2).

 p1 :- insanity([a,a,a,a,a,a],[b,b,b,b,b,b],[c,c,c,c,c,c],[d,d,d,d,d,d]).

 p2 :- insanity([j,b,j,r,v,j],[b,j,j,b,r,v],[r,j,r,b,v,b],[b,v,r,v,r,v]).

 p3 :- insanity([a,a,a,a,a,a],[a,a,a,a,a,a],[a,a,a,a,a,a],[a,a,a,a,a,a]).

 p4 :- insanity([v,j,v,r,j,b],[j,b,r,b,v,v],[r,v,b,j,r,r],[b,r,j,v,r,j]).
