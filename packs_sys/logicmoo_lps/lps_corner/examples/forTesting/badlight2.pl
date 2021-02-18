
:- expects_dialect(lps).

maxTime(12).
fluents light(_,_), location(_,_).
actions switch(_,_, _), goto(_,_).

initially 	light(livingroom, off), light(kitchen, on), 
			light(bedroom, on), light(bathroom, on),
			location(bob, livingroom), location(dad, kitchen).
			
						
if 		location(bob, Place) at T1, light(Place, off) at T1
then 	switch(bob, Place, on) from T1 to T2.


if light(Place, on) at T1, location(dad, Place) at T1
then switch(dad, Place, off) from T1 to T2.


if light(Place, on) at T1, not location(dad, Place) at T1
then check(dad, Place) from T2 to T3.

check(dad, Place) from T to T if light(Place, off) at T.
check(dad, Place) from T1 to T2 if light(Place, on) at T1, goto(dad, Place) from T1 to T2.


switch(Person, Place, New) 	initiates light(Place, New).

switch(Person, Place, New) 	terminates light(Place, Old).

goto(Person, Place) 	initiates 	location(Person, Place).
goto(Person, _) 		terminates 	location(Person, _).


false goto(dad, Place1), goto(dad, Place2), Place1 \= Place2.
false goto(Person,_), switch(Person, _, _).
