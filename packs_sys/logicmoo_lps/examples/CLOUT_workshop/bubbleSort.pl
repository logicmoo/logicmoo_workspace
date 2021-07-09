
:- expects_dialect(lps).

% bubble sort with relational data structure .
maxTime(5).
fluents	location(_, _).
actions	swap(_,_,_,_).

initially	location(d, 1), location(c, 2), location(b, 3),  location(a,4).

if	location(X, N1) at T1, N2 is N1 +1,  location(Y, N2) at T1,  Y@<X
then	swapped(X, N1, Y, N2) from T2 to T3.

% swapped does not work if the order of the two clauses below is
% reversed. Perhaps for good reasons, 
% namely in the hope that positions will become swapped in the future 
% without the need to swap them explicitly.

swapped(X, N1, Y, N2) from T1 to T2 
if 	location(X, N1) at T1, location(Y, N2) at T1,  
	Y@<X, swap(X, N1, Y, N2) from T1 to T2.

swapped(X, N1, Y, N2) from T to T 
if	location(X, N1) at T, location(Y, N2) at T, X@<Y.

swap(X, N1, Y, N2)  	initiates 	location(X, N2).
swap(X, N1, Y, N2)  	initiates 	location(Y, N1).

swap(X, N1, Y, N2)  	terminates 	location(X, N1).
swap(X, N1, Y, N2)  	terminates 	location(Y, N2).

false 	swap(X, N1, Y, N2), swap(Y, N2, Z, N3).

/* Uncomment this to get an experimental 2d display:
display(location(Value,Pos),[label:Value, fontSize:16, type:circle, center:[X,20], radius:15, strokeColor:blue]) :- X is Pos * 30.
display(swap(_,Pos1,_,Pos2),[from:[FX,40],to:[TX,40],type:arrow, label:'swap!', biDirectional]) :- FX is Pos1*30, TX is Pos2*30.
display(timeless,[type:rectangle, label:'Bubble Sort', fillColor:white, from:[0,0], to:[150,140]]).
*/
/** <examples>
?- go(Timeline).
*/