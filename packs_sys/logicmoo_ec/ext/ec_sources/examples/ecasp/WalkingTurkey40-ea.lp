#domain time(Time).

time(0..40).
offset(1..1).



fluent(loaded).
fluent(alive).
fluent(walking).

event(load).
event(shoot).



initiates(load,loaded,Time).

terminates(shoot,alive,Time) :- holdsAt(loaded,Time).

terminates(shoot,walking,Time) :- terminates(shoot,alive,Time).

:- releasedAt(loaded,0).

:- releasedAt(alive,0).

:- releasedAt(walking,0).

holdsAt(alive,0).

holdsAt(loaded,0).

holdsAt(walking,0).

happens(shoot,25).

:- holdsAt(walking,37).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
