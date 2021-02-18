
:- expects_dialect(lps).

% Basd on prospectiveGoat.pl, but with RK fixes
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


makeLoc(Object, north) from T1 to T3  if
	Object \= farmer, 
	makeLoc(farmer, south) from T1 to T2,
	loc(Object, south) at T2,
	transport(Object, south, north) from T2 to T3.

% to move an object to a location, except for the farmer, first move the farmer to the other location, then ...

makeLoc(farmer, Location) from T to T if
	loc(farmer, Location) at T.

% farmer crosses with load
makeLoc(farmer, Location1)   from T1 to T2 if
	loc(farmer, Location2) at T1, Location1 \= Location2,
	loc(Object,Location2) at T1,
	Object \= farmer,
	transport(Object, Location2, Location1) from T1 to T2.

% farmer crosses empty-handed
makeLoc(farmer, Location1)   from T1 to T2 if	
	loc(farmer, Location2) at T1, Location1 \= Location2,
	transport(farmer, Location2, Location1) from T1 to T2.
%	row(Location2, Location1) from T1 to T2.

transport(Object, Location1, Location2) updates Location1 to Location2 in loc(Object, Location1).
% row(Location1, Location2) updates Location1 to Location2 in loc(farmer, Location1).

transport(Object, Location1, Location2) updates Location1 to Location2 in loc(farmer, Location1).


false transport(Object1, Location1, Location2), 
transport(Object2, Location1, Location2) , Object1 \= Object2.
% false row(south, north), row(north, south).

false transport(_, L, _) from T1 to T2, loc(goat,L) at T2, loc(wolf,L) at T2.  
false transport(_, L, _) from T1 to T2, loc(goat,L) at T2, loc(cabbage,L) at T2.

/** <examples>
?- godfa(Graph).
?- go.
*/

