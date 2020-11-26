#domain time(Time).

time(0..2).
offset(1..1).



fluent(blocked1).
fluent(blocked2).
fluent(stuffy).

event(close1).
event(close2).
event(start).



initiates(close1,blocked1,Time).

initiates(close2,blocked2,Time).

holdsAt(blocked2,Time) :- holdsAt(stuffy,Time).

holdsAt(blocked1,Time) :- holdsAt(stuffy,Time).

holdsAt(stuffy,Time) :- holdsAt(blocked1,Time), holdsAt(blocked2,Time).

initiates(start,blocked1,Time).

terminates(start,blocked2,Time).

releasedAt(stuffy,Time).

:- releasedAt(blocked1,0).

:- releasedAt(blocked2,0).

:- holdsAt(blocked1,0).

:- holdsAt(blocked2,0).

happens(start,0).

happens(close2,1).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
