#domain time(Time).

time(0..3).
offset(1..1).



fluent(loaded).
fluent(alive).
fluent(dead).

event(load).
event(shoot).
event(sneeze).



initiates(load,loaded,Time).

terminates(shoot,alive,Time) :- holdsAt(loaded,Time).

terminates(shoot,loaded,Time).

:- holdsAt(dead,Time), holdsAt(alive,Time).

holdsAt(dead,Time) :- not holdsAt(alive,Time).

releasedAt(dead,Time).

:- releasedAt(loaded,0).

:- releasedAt(alive,0).

holdsAt(alive,0).

:- holdsAt(loaded,0).

happens(load,0).

happens(sneeze,1).

happens(shoot,2).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
