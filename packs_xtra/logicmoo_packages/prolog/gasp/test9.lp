%%%%%% encoding: 6 squares (0..5)

master_size(45).
square(0..5).
position(0..44).
size(0,Y):-master_size(X), Y = X / 3 * 2.
size(1,Y):-master_size(X), Y = X / 3.
size(2,Y):-master_size(X), Y = X / 3.
size(3,Y):-master_size(X), Y = X / 3.
size(4,Y):-master_size(X), Y = X / 3.
size(5,Y):-master_size(X), Y = X / 3.


%% lparse version
pos(X,Y):-position(X),position(Y).
1{place(Square,X,Y):pos(X,Y)}1:-square(Square).

:-place(Sq1,X1,Y1),place(Sq2,X2,Y2),
  square(Sq1),square(Sq2),
  position(X1),position(Y1),
  position(X2),position(Y2),
  Sq1 != Sq2, size(Sq1,Size1),size(Sq2,Size2),
  X2+Size2>X1, X2<X1+Size1,
  Y2+Size2>Y1, Y2<Y1+Size1.
%% lparse version end

%%%% GASP version
%%domain(X):-square(X).
%%range1(X):-position(X).
%%range2(X):-position(X).
%%assignment(X,Y,Z).
%%
%%%non overlap
%%:-place(Sq1,X1,Y1),place(Sq2,X2,Y2),
%%  square(Sq1),square(Sq2),
%%  position(X1),position(Y1),
%%  position(X2),position(Y2),
%%  Sq1 neq Sq2, size(Sq1,Size1),size(Sq2,Size2),
%%  X2+Size2>X1, X2<X1+Size1,
%%  Y2+Size2>Y1, Y2<Y1+Size1.
  
%contained in the main square
:-place(S,X,Y),square(S),
  position(X),position(Y),
  size(S,Size),
  master_size(MS),
  X+Size>MS.

:-place(S,X,Y),square(S),
  position(X),position(Y),
  size(S,Size),
  master_size(MS),
  Y+Size>MS.
  
