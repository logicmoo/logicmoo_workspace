#domain time(Time).

time(0..40).
offset(1..1).



fluent(itsHeads).
fluent(heads).

event(toss).



initiates(toss,heads,Time) :- holdsAt(itsHeads,Time).

terminates(toss,heads,Time) :- not holdsAt(itsHeads,Time).

releasedAt(itsHeads,Time).

:- releasedAt(heads,0).

holdsAt(heads,0).

happens(toss,10).

happens(toss,20).

happens(toss,30).

holdsAt(itsHeads,0).

holdsAt(itsHeads,40).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
