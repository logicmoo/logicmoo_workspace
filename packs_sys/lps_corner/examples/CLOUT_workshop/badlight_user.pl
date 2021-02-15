
:- expects_dialect(lps).

maxRealTime(30).
minCycleTime(1).
fluents light(_,_), location(_,_).
actions switch(_,_, _), goto(_,_).
events lps_mousedown(_ID,_X,_Y).

initially 	light(livingroom, off), light(kitchen, on), 
			light(bedroom, on), light(bathroom, on),
			location(bob, livingroom), location(dad, kitchen).
			
						
if 		location(bob, Place) at T1, light(Place, off) at T1
then 	switch(bob, Place, on) from T1 to T2.


if light(Place, on) at T1, location(dad, Place) at T1
then switch(dad, Place, off) from T1 to T2.


if light(Place, on) at T1, not location(dad, Place) at T1
then goto(dad, Place) from T2 to T3.


switch(Person, Place, New) 	initiates light(Place, New).

switch(Person, Place, New) 	terminates light(Place, Old).

goto(Person, Place) 	initiates 	location(Person, Place).
goto(Person, _) 		terminates 	location(Person, _).

% A click in a room switches its light on. Rooms have atom names
lps_mousedown(Where,_X,_Y) updates Old to 'on' in light(Where,Old) if atomic(Where).

false goto(dad, Place1), goto(dad, Place2), Place1 \= Place2.
false goto(Person,_), switch(Person, _, _).

display(light(Place,on),[
  	type:raster, scale:0.2,
    source:'http://www.clker.com/cliparts/O/Y/B/C/t/1/light-bulb-on-md.png',
    position:[X,Y]
  ]) :- locationXY(Place,RX,RY), X is RX+75, Y is RY+120.

display(location(P,L),[type:ellipse,label:P,point:[PX,PY],size:[20,40],fillColor:green]) :-
    locationXY(L,X,Y), PY is Y+10,
    (P=dad ->  PX is X + 25 ; PX is X+50).

locationXY(livingroom,0,0).
locationXY(kitchen,150,0).
locationXY(bedroom,150,150).
locationXY(bathroom,0,150).

display(timeless,Divisions) :- findall(
   	[type:rectangle, label:D, id:D, from:[X,Y], to:[TX,TY], strokeColor:black ],
  	( locationXY(D,X,Y), TX is X+150, TY is Y+150), 
   	Divisions).
/** <examples>
?- serve(S).
*/