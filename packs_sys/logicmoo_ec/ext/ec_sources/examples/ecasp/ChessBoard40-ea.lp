#domain time(Time).

time(0..40).
offset(1..1).



fluent(itsBlack).
fluent(itsWhite).
fluent(onBlack).
fluent(onWhite).

event(throw).



initiates(throw,onWhite,Time) :- holdsAt(itsWhite,Time).

initiates(throw,onBlack,Time) :- holdsAt(itsBlack,Time).

holdsAt(itsWhite,Time)|holdsAt(itsBlack,Time).

releasedAt(itsBlack,Time).

releasedAt(itsWhite,Time).

:- releasedAt(onBlack,0).

:- releasedAt(onWhite,0).

:- holdsAt(onWhite,0).

:- holdsAt(onBlack,0).

happens(throw,35).

holdsAt(itsWhite,0).

holdsAt(itsBlack,0).

holdsAt(itsWhite,40).

holdsAt(itsBlack,40).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
