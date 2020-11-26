#domain time(Time).

time(0..40).
offset(1..1).



fluent(loaded).
fluent(alive).

event(load).
event(shoot).
event(spin).



initiates(load,loaded,Time).

terminates(shoot,alive,Time) :- holdsAt(loaded,Time).

releases(spin,loaded,Time).

terminates(shoot,loaded,Time).

:- releasedAt(loaded,0).

:- releasedAt(alive,0).

holdsAt(alive,0).

:- holdsAt(loaded,0).

happens(load,10).

happens(spin,20).

happens(shoot,30).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
