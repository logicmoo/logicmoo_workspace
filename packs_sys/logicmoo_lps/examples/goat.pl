
:- expects_dialect(lps).

maxTime(8).
actions row(_,_), transport(_,_,__).
fluents loc(_, _).

initially
loc(wolf,south),
loc(goat, south),
loc(cabbage, south),
loc(farmer, south).

if        loc(Object, south) at T1,	Object \= farmer
then   makeLoc(Object, north) from T2 to T3.


makeLoc(Object, Location1)   from T1 to T3
if	 Object \= farmer, loc(Object, Location2) at T1, Location1 \= Location2,
	makeLoc(farmer, Location2) from T1 to T2,
	row(Location2, Location1) from T2 to T3, transport(Object, Location2, Location1) from T2 to T3,
	dealWithGoat(Location2, Location1) from T2 to T3.


makeLoc(farmer, Location1)   from T1 to T3
if	 loc(farmer, Location2) at T1, Location1 \= Location2,
	row(Location2, Location1) from T1, dealWithGoat(Location2, Location1) from T1 to T3.

dealWithGoat(Location2, Location1) from T1 to T2
if	 not (loc(goat, Location2)) at T1.


dealWithGoat(Location2, Location1) from T1 to T2
if	 not (loc(wolf, Location2)) at T1, not(loc(cabbage, Location2)) at T1.


dealWithGoat(Location2, Location1) from T1 to T2
if	 loc(goat, Location2) at T1, loc(wolf, Location2) at T1,
	transport(goat, Location2, Location1) from T1 to T2.

dealWithGoat(Location2, Location1) from T1 to T2
if	 loc(goat, Location2) at T1, loc(wolf, Location2) at T1,
not loc(cabbage, Location2) at T1,
	transport(wolf, Location2, Location1) from T1 to T2.

dealWithGoat(Location2, Location1) from T1 to T2
if	 loc(goat, Location2) at T1, loc(cabbage, Location2) at T1,
	transport(goat, Location2, Location1) from T1 to T2.

dealWithGoat(Location2, Location1) from T1 to T2
if	 loc(goat, Location2) at T1, loc(cabbage, Location2) at T1,
not loc(wolf, Location2) at T1,
	transport(cabbage, Location2, Location1) from T1 to T2.


makeLoc(Object, Location) from T to T
if	loc(Object, Location) at T.

transport(Object, Location1, Location2) initiates loc(Object, Location2).
row(Location1, Location2) initiates loc(farmer, Location2).
transport(Object, Location1, Location2) terminates loc(Object, Location1).
row(Location1, Location2) terminates loc(farmer, Location1).

false transport(Object1, Location1, Location2), transport(Object2, Location1, Location2) , Object1 \= Object2.
false row(south, north), row(north, south).
