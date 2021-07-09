
:- expects_dialect(lps).

% Conway's game of life.
% cells assumed dead by default
% x-y represents that the cell at column x and row y is alive
% Variant to allow clicking in empty cells to make them alive

minCycleTime(1).
fluents X - Y.
actions die(_), live(_).

initially 2-3,3-3,4-3. % blinker
%initially 6-3,7-3,8-3. % another
%initially 6-6,7-6,8-6. % yet another

%Notice that times can sometimes be omitted, as in these rules:
%
if X-Y at T, aliveNeighbors(X-Y,N) at T, N<2 then die(X-Y).
if X-Y at T, aliveNeighbors(X-Y,N) at T, N>3 then die(X-Y).
if cell(X,Y), aliveNeighbors(X-Y,3) at T then live(X-Y).

% The following two clauses, which are time-independent, are in Prolog.
%
cell(X,Y) :- 
    Range = [1,2,3,4,5,6,7,8,9,10], member(X,Range), member(Y,Range).

adjacent(X-Y,L) :- findall(Ax-Ay,(
	member(Dx,[-1,0,1]), member(Dy,[-1,0,1]), not (Dx=0,Dy=0),
    Ax is X+Dx, Ay is Y+Dy
    ), L).

% The following four clauses define predicates that depend on the state at time T.
%
aliveNeighbors(X-Y,N) at T 
if	adjacent(X-Y,L),
	countLivingNeighbors(L,N) at T.

countLivingNeighbors([],0) at _.

countLivingNeighbors([X-Y|Cells],N) at T 
if	X-Y at T, countLivingNeighbors(Cells,N1) at T, N is N1+1.

countLivingNeighbors([X-Y|Cells],N) at T 
if	not(X-Y) at T, countLivingNeighbors(Cells,N) at T.

live(X-Y) 	initiates X-Y.	
die(X-Y) 	terminates X-Y.

events lps_mousedown(_,_,_).
% rather than detect a click on a fluent, detect a click anywhere:
lps_mousedown(_,XX,YY) initiates X-Y if X is round(XX/10), Y is round(YY/10).

display(X-Y,[center:[XX,YY], radius:5, type:circle, fillColor:green]) :- 
    XX is X*10, YY is Y*10.
display(live(X-Y),[type:star, center:[XX,YY], points:7, radius1:4, radius2:7, fillColor:red]) :- 
    XX is X*10, YY is Y*10.
display(die(X-Y),[type:star, center:[XX,YY], points:7, radius1:4, radius2:7, fillColor:black]) :- 
    XX is X*10, YY is Y*10.

display(timeless,[[type:raster,position:[50,120], scale:0.08, % more complicated: transform:[-0.1,0,0,0.1,0,0],
             source:'https://upload.wikimedia.org/wikipedia/commons/0/04/John_H_Conway_2005_%28cropped%29.jpg'],
     [type:text, point:[0,5], content:'Conway\'s Game of Life']]).


/** <examples>
?- serve(Timeline).
*/
