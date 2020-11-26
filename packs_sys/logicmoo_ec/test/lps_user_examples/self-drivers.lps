/*
This simplified example is inspired by 
Zhao, Lihua, Ryutaro Ichise, Zheng Liu, Seiichi Mita, and Yutaka Sasaki. 
"Ontology-Based Driving Decision Making: A Feasibility Study at Uncontrolled Intersections." 
IEICE TRANSACTIONS on Information and Systems 100, no. 7 (2017): 1425-1439.
At https://www.jstage.jst.go.jp/article/transinf/E100.D/7/E100.D_2016EDP7337/_pdf

For simplicity, it assumes all streets have two lanes,
and all intersections are uncontrolled without traffic lights.
All streets run North-South or East-West.
The term x-y represents the location with x-coordinate x and y-coordinate y.

Cars drive on the left, as in Japan and in the UK.
At cross road intersections, the car on the right has priority.

All cars have an initial location. 
The destination of a car journey is observed as an external input.
These observations trigger the car to drive, by consulting a database of known routes.
*/

maxTime(30).

% LPS uses a simple event-calculus-like ontology of fluents, and events, which include actions:
%
fluents location(Vehicle, Place, Heading), 
	collisionWarning(Vehicle1, Vehicle2, Place), 
	rightOfWay(Vehicle1, Vehicle2, Place), 
	collisionPossible(Vehicle1, Vehicle2).

events destination(Vehicle, Place), remove(Vehicle).

actions step(Vehicle, OldPlace, NextPlace), turn(Vehicle, NewHeading).

initially location(mycar, 2-1, northward), 
	location(yourcar, 6-1, northward),
	location(othercar, 10-5, westward),
	location(troubleMaker, 6-2, northward),
	location(brokencar, 2-7, noward).


observe destination(mycar, 9-9) from 2 to 3.
% observe destination(mycar, 6-5) from 7 to 8.
observe destination(troubleMaker, 9-9) from 2 to 3.
observe destination(yourcar, 9-9) from 2 to 3.
observe destination(othercar, 6-1) from 1 to 2.
observe remove(brokencar) from 15 to 16.

% Just for fun.
remove(Vehicle) terminates location(Vehicle,_,_).

% This is the main rule, which controls the behaviour of the system.
% Here T is a "time" variable, which is better understood as a state variable.
% 
if destination(Vehicle, NewPlace) to T, 
location(Vehicle, PresentPlace, Heading) at T 
then directions(PresentPlace, Route, NewPlace),
drive(Vehicle, Route, NewPlace) from T.

% Finding directions is easiest when the start and finish locations are on the same street:
% 
directions(Start, [Heading - Street], Finish):-
	on(Start, Street), on(Finish, Street), 
	orientation(Start, Finish, Heading).

orientation(X-Y1, X-Y2, northward) :- Y1 < Y2.
orientation(X-Y1, X-Y2, southward) :- Y1 > Y2.
orientation(X1-Y, X2-Y, eastward) :- X1 < X2.
orientation(X1-Y, X2-Y, westward) :- X1 > X2.

% Otherwise, directions can be found by consulting a database of long distance routes 
% and extracting segments of those routes.
%
directions(Start, NewRoute, Finish):-
	on(Finish, Street2),
	route(_, Route, _), 
	append(FirstPart, [Heading2 - Street2 | _], Route),
	on(Start, Street1),
	append(_, [Heading1 - Street1 | Link], FirstPart),
	append([Heading1 - Street1 | Link], [Heading2 - Street2], NewRoute).

% Here are some long distance routes and some street locations:
%
route(2-9, [eastward - northStreet], 9-9).
route(2-1, [northward - westStreet, eastward - northStreet], 9-9).
route(2-1, [northward - westStreet, eastward - mainStreet], 9-6).
route(6-1, [northward - highStreet, eastward - northStreet], 9-9).
route(9-9, [westward - northStreet, southward - westStreet], 2-1).
route(10-5, [westward - mainStreet, southward - highStreet], 6-1).

on(X-5, mainStreet) :- 2 =< X, X =< 10.
on(X-9, northStreet) :- 2 =< X, X =< 9.
on(6-Y, highStreet) :- 1 =< Y, Y =< 9.
on(2-Y, westStreet) :- 1 =< Y, Y =< 9.
on(8-Y, eastStreet) :- 1 =< Y, Y =< 9.

% There are three cases for driving. It might be desirable to refactor them.
% Notice that in all three cases, driving starts at "time" (really state) T, but stepping forward starts at T1. 
% This takes into account that it might be necessary to wait from T to T1 for the step to be possible.
% 
drive(Vehicle, [Heading - Street], NewPlace) from T to T3 if
	location(Vehicle, OldPlace, Heading) at T, OldPlace \= NewPlace,
	next(OldPlace, Heading, NextPlace), 
	on(NextPlace, Street), 
	step(Vehicle, OldPlace, NextPlace) from T1 to T2,
	drive(Vehicle, [Heading - Street], NewPlace) from T2 to T3.

drive(Vehicle, [OldHeading - OldStreet, NewHeading - NewStreet | Rest], NewPlace) from T to T3 if
	location(Vehicle, OldPlace, OldHeading), 
	next(OldPlace, OldHeading, NextPlace),
	not on(NextPlace, NewStreet), 
	step(Vehicle, OldPlace, NextPlace) from T1 to T2,
 	drive(Vehicle, [OldHeading - OldStreet, NewHeading - NewStreet | Rest], NewPlace) from T2 to T3.

% Here step and turn are executed concurrently.
%
drive(Vehicle, [OldHeading - OldStreet, NewHeading - NewStreet | Rest], NewPlace) from T to T3 if
	location(Vehicle, OldPlace, OldHeading), 
	next(OldPlace, OldHeading, NextPlace),
	on(NextPlace, NewStreet), 
	step(Vehicle, OldPlace, NextPlace) from T1 to T2,
	turn(Vehicle, NewHeading) from T1 to T2,
	drive(Vehicle, [NewHeading - NewStreet| Rest], NewPlace) from T2 to T3.

% Stepping and turning update the current state:
%
step(Vehicle, OldPlace, NextPlace) updates OldPlace to NextPlace in location(Vehicle, OldPlace, Heading).
turn(Vehicle, NewHeading) updates OldHeading to NewHeading in location(Vehicle, Place, OldHeading).

% Some geography:
%
next(X-Y1, northward, X-Y2) :- Y2 is Y1 + 1.
next(X-Y1, southward, X-Y2) :- Y2 is Y1 - 1.
next(X1-Y, eastward, X2-Y) :- X2 is X1 +1.
next(X1-Y, westward, X2-Y) :- X2 is X1 - 1.

% Some self-preservation:
%
false step(Vehicle1, OldPlace, NextPlace),  
	collisionPossible(Vehicle1, Vehicle2).

collisionPossible(Vehicle1, Vehicle2) at T if
	location(Vehicle1, Place1, Heading1) at T, 
	next(Place1, Heading1, Place2), 
	location(Vehicle2, Place2, Heading2) at T,
	not opposite(Heading1, Heading2).

opposite(northward, southward).
opposite(southward, northward).
opposite(eastward, westward).
opposite(westward, eastward).

% Some rules of the road, which are conducive to a well-ordered society:
%
false step(Vehicle1, OldPlace, NextPlace),  
	collisionWarning(Vehicle1, Vehicle2, NextPlace), 
	not rightOfWay(Vehicle1, Vehicle2, NextPlace).

collisionWarning(Vehicle1, Vehicle2, NextPlace) at T if
	location(Vehicle1, Place1, Heading1) at T, 
	location(Vehicle2, Place2, Heading2) at T,
	next(Place1, Heading1, NextPlace), 
	next(Place2, Heading2, NextPlace),
	clash(Heading1, Heading2).

clash(Heading1, Heading2) :- 
    horizontal(Heading1), vertical(Heading2).
clash(Heading1, Heading2) :- 
    vertical(Heading1), horizontal(Heading2).

horizontal(H) :- H=eastward ; H=westward.
vertical(H) :- H=northward ; H=southward.

rightOfWay(Vehicle1, Vehicle2, NextPlace) if
	priorityTjunction(NextPlace, Street),
	location(Vehicle1, Place, _),
	on(Place, Street).
	
rightOfWay(Vehicle1, Vehicle2, NextPlace) if
	crossRoads(NextPlace),
	location(Vehicle1, _, Heading1) at T, 
	location(Vehicle2, _, Heading2) at T,
	rightOf(Heading1, Heading2).

rightOf(westward, northward).
rightOf(southward, westward).
rightOf(eastward, southward).
rightOf(northward, eastward).

% Cross roads and T junctions can be derived from street locations,
% but for this simple example, it is easier just to list them:
%
crossRoads(6-5).
crossRoads(8-5).

priorityTjunction(2-5, westStreet).
priorityTjunction(6-9, northStreet).
priorityTjunction(8-9, northStreet).

% Here is a basic visualisation for an animation:
%

d(location(mycar,X-Y, Heading),[type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:blue]) :- 
    	position(X,Y,Heading,XX,YY, Xcar, Ycar).

d(location(yourcar,X-Y, Heading),[type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:red]) :- 
    	position(X,Y,Heading,XX,YY, Xcar, Ycar).

d(location(othercar,X-Y, Heading),[type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:green]) :- 
    	position(X,Y,Heading,XX,YY, Xcar, Ycar).

d(location(troubleMaker,X-Y, Heading),[type:ellipse, point:[XX, YY], size:[Xcar, Ycar], fillColor:maroon]) :- 
    	position(X,Y,Heading,XX,YY, Xcar, Ycar).

d(location(brokencar,X-Y, Heading),[type:circle, center:[XX, YY], 
        radius: 10, fillColor:red]) :- XX is X*20+10, YY is Y*20+10.

position(X,Y,northward,XX,YY, 3,7) :- XX is X*20, YY is Y*20.
position(X,Y,southward,XX,YY, 3, 7) :- XX is X*20+15, YY is Y*20.
position(X,Y,westward,XX,YY, 7, 3) :- XX is X*20+15, YY is Y*20.
position(X,Y,eastward,XX,YY, 7, 3) :- XX is X*20, YY is Y*20+15.

d(timeless,Background) :- findall(
   	[type:rectangle,  from:[XX,YY], to:[XX2, YY2], fillColor:yellow ],
  	( place(X-Y), not on(X-Y, _), XX is X*20, YY is Y*20,
  	XX2 is XX+20, YY2 is YY+20), 
   	Background).

place(X-Y) :- member(X, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    		member(Y, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

/** <examples>
?- go(Timeline).
?- go.
*/
