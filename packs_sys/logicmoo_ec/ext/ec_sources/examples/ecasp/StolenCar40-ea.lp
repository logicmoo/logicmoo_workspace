#domain time(Time;Time1;Time2).

time(0..40).
offset(1..1).



fluent(carParked).

event(park).
event(steal).



initiates(park,carParked,Time).

terminates(steal,carParked,Time).

:- releasedAt(carParked,0).

:- holdsAt(carParked,0).

happens(park,20).

:- holdsAt(carParked,40).

holdsAt(carParked,Time) :- happens(steal,Time).

:- happens(steal,Time1), Time1<Time2, happens(park,Time2).

0 {happens(E, T)} 1 :- T<maxstep.



hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
