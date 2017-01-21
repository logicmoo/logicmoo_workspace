% Conway's game of life, cf. https://en.wikipedia.org/wiki/Conway's_Game_of_Life
% cells assumed dead by default
% x-y : cell at column x and line y is alive
initially 2-3,3-3,4-3. % blinker
maxTime(10).
cell(X,Y) :- 
    Range = [1,2,3,4,5,6,7,8,9,10], member(X,Range), member(Y,Range).

adjacent(X-Y,L) :- findall(Ax-Ay,(
	member(Dx,[-1,0,1]), member(Dy,[-1,0,1]), not (Dx=0,Dy=0),
    Ax is X+Dx, Ay is Y+Dy
    ), L).

fluents _X - _Y.
actions die(_cell), live(_cell).

aliveNeighbors(X-Y,N) at T if
	adjacent(X-Y,L),
	countLivingNeighbors(L,N) at T.

countLivingNeighbors([],0) at _.
countLivingNeighbors([X-Y|Cells],N) at T if
	X-Y at T, countLivingNeighbors(Cells,N1) at T, N is N1+1.
countLivingNeighbors([X-Y|Cells],N) at T if
	not(X-Y) at T, countLivingNeighbors(Cells,N) at T.

if X-Y at T, aliveNeighbors(X-Y,N) at T, N<2 then die(X-Y).
if X-Y at T, aliveNeighbors(X-Y,N) at T, N>3 then die(X-Y).
if cell(X,Y), aliveNeighbors(X-Y,3) at T then live(X-Y).

die(X-Y) terminates X-Y.
live(X-Y) initiates X-Y.
/** <examples>
?- go(Timeline).
*/