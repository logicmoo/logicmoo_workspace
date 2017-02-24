% Conway's game of life.
% cells assumed dead by default
% x-y represents that the cell at column x and row y is alive
% XSB requires: :- use_module(basics, [member/2]).

maxTime(10).
fluents X - Y.
actions die(_), live(_).

initially 2-3,3-3,4-3. % blinker

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


/** <examples>
?- go(Timeline).
*/