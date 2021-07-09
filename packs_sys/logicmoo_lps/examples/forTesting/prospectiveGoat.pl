
:- expects_dialect(lps).

maxTime(10).
actions row(_,_), transport(_,_,__).
fluents loc(_, _).

initially
loc(wolf,south),
loc(goat, south),
loc(cabbage, south),
loc(farmer, south).

% wolf, goat, cabbage and farmer are things and are located at the south margin
% locations are north and south margins

% all things on the south margin, except the farmer, must move to the north margin
if   loc(Object, south),	Object \= farmer
then   makeLoc(Object, north) from T2 to T3.

% to move an object to a location, except for the farmer, first move the farmer to the other location, then ...

makeLoc(Object, Location1) from T1 to T3  if
	Object \= farmer, loc(Object, Location2) at T1, Location1 \= Location2,
	makeLoc(farmer, Location2) from T1 to T2,
	row(Location2, Location1) from T2 to T3, 
	transport(Object, Location2, Location1) from T2 to T3.

% farmer crosses empty-handed
makeLoc(farmer, Location1)   from T1 to T2 if	
	loc(farmer, Location2) at T1, Location1 \= Location2,
	row(Location2, Location1) from T1 to T2.

% farmer crosses with load
makeLoc(farmer, Location1)   from T1 to T2 if
	loc(farmer, Location2) at T1, Location1 \= Location2,
	loc(Object,Location2) at T1,
	transport(Object, Location2, Location1) from T1 to T2,
	row(Location2, Location1) from T1 to T2.

makeLoc(Object, Location) from T to T if
	loc(Object, Location) at T.

transport(Object, Location1, Location2) updates Location1 to Location2 in loc(Object, Location1).
row(Location1, Location2) updates Location1 to Location2 in loc(farmer, Location1).

false transport(Object1, Location1, Location2), transport(Object2, Location1, Location2) , Object1 \= Object2.
false row(south, north), row(north, south).

false loc(goat,L) at T, loc(wolf,L) at T, not loc(farmer,L) at T, row(_,_) to T.  
false loc(goat,L) at T, loc(cabbage,L) at T, not loc(farmer,L) at T, row(_,_) to T.

