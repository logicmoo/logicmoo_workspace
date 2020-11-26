:- expects_dialect(lps).

% This won't work on current LPS 

actions row(_Location1,_Location2), transport(_Obj,_Location1,_Location2).
fluents loc(_, _).

initially loc(wolf,south),loc(goat, south),loc(cabbage, south),loc(farmer, south).

transport(Ob,L1,L2) updates L1 to L2 in loc(Ob,L1).
row(L1,L2) updates L1 to L2 in loc(farmer,L1).

false loc(wolf,L) at T, loc(goat,L) at T, not loc(farmer,L) at T, happens(_,_,T). 
false loc(cabbage,L) at T, loc(goat,L) at T, not loc(farmer,L) at T, happens(_,_,T).
false row(L1,L2), row(L2,L1).
false transport(Obj1, Loc1, Loc2), transport(Obj2, Loc1, Loc2) , Obj1 \= Obj2.

opposite(north,south). 
opposite(south,north).

move from T1 to T2 if 
	loc(Ob,L1) at T1, Ob\=farmer, opposite(L1,L2), transport(Ob,L1,L2) from T1 to T2, 
	loc(farmer,L1) at T1, row(L1,L2) from T1 to T2.
move  if  
	loc(farmer,L1) at T, opposite(L1,L2), row(L1,L2) from T.

moves from T to T.
moves if move  to T, moves from T.

if not loc(_,north) at Start then 
	moves from Start to Finish, not loc(_,south) at Finish.



/** <examples> 
?- go(Timeline).
*/


