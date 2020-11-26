#domain time(Time).

time(0..2).
offset(1..1).



fluent(carParked).

event(park).
event(steal).



initiates(park,carParked,Time).

terminates(steal,carParked,Time).

:- releasedAt(carParked,0).

:- holdsAt(carParked,0).

happens(park,0).

:- holdsAt(carParked,2).

0 {happens(E, T)} 1 :- T<maxstep.



hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
