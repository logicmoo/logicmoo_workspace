:- expects_dialect(lps).

% x-y represents the location at x-coordinate x and y-coordinate y.


maxTime(20).

fluents location(Vehicle, Place, Heading).
events destination(Vehicle, Place).
actions step(Vehicle), turn(Vehicle, Direction).

step(Vehicle) updates OldPlace to NewPlace in location(Vehicle, OldPlace, Heading) if
	next(OldPlace, Heading, NewPlace).
	
next(X-Y1, northward, X-Y2) :- Y2 is Y1 + 1.
next(X-Y1, southward, X-Y2) :- Y2 is Y1 - 1.
next(X1-Y, eastward, X2-Y) :- X2 is X1 +1.
next(X1-Y, westward, X2-Y) :- X2 is X1 - 1.

turn(Vehicle, NewHeading) updates OldHeading to NewHeading in location(Vehicle, Place, OldHeading).

initially location(mycar, 2-1, northward), location(yourcar, 9-9, westward).

/*
initially place(X-Y) :-  member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    		member(Y, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
  	not on(X-Y, Street).
*/

observe destination(mycar, 9-9) from 2 to 3.
observe destination(yourcar, 2-1) from 3 to 4.

on(X-5, mainStreet) :- 3 =< X, X =< 10.
on(X-9, northStreet) :- 2 =< X, X =< 9.
on(6-Y, highStreet) :- 1 =< Y, Y =< 9.
on(2-Y, westStreet) :- 1 =< Y, Y =< 9.
on(8-Y, eastStreet) :- 1 =< Y, Y =< 9.

directions(2-1, [northward - westStreet, eastward - northStreet], 9-9).
directions(9-9, [westward - northStreet, southward - westStreet], 2-1).



if destination(Vehicle, NewPlace) to T, 
location(Vehicle, PresentPlace, Heading) at T, 
directions(PresentPlace, Route, NewPlace)
then drive(Vehicle, Route, NewPlace) from T to T2.

drive(Vehicle, _, NewPlace) from T to T if
	location(Vehicle, NewPlace, Heading) at T.
	

drive(Vehicle, [Heading - Street| Rest], NewPlace) from T1 to T3 if
	location(Vehicle, OldPlace, Heading), on(OldPlace, Street),
	next(OldPlace, Heading, NextPlace), on(NextPlace, Street), 
	step(Vehicle) from T1 to T2,
	drive(Vehicle, [Heading - Street| Rest], NewPlace) from T2 to T3.
	

drive(Vehicle, [OldHeading - OldStreet, NewHeading - NewStreet | Rest], NewPlace) from T1 to T3 if
	location(Vehicle, OldPlace, OldHeading), on(OldPlace, OldStreet),
	on(OldPlace, NewStreet), 
	turn(Vehicle, NewHeading) from T1 to T2,
	drive(Vehicle, [NewHeading - NewStreet| Rest], NewPlace) from T2 to T3.

d(location(mycar,X-Y, Heading),[type:circle, center:[XX, YY], radius:5, fillColor:blue]) :- 
    XX is X*10+5, YY is Y*10+5.
d(location(yourcar,X-Y, Heading),[type:circle, center:[XX, YY], radius:5, fillColor:red]) :- 
    XX is X*10+5, YY is Y*10+5.

d(timeless,Background) :- findall(
   	[type:rectangle,  from:[XX,YY], to:[XX2, YY2], fillColor:yellow ],
  	( place(X-Y), not on(X-Y, _), XX is X*10, YY is Y*10,
  XX2 is XX+10, YY2 is YY+10), 
   	Background).

place(X-Y) :- member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    		member(Y, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

/** <examples>
?- go(Timeline).
*/
