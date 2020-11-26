#domain time(Time).

time(0..40).
offset(1..1).



fluent(forwards).
fluent(backwards).
fluent(spinning).

event(push).
event(pull).



initiates(push,forwards,Time) :- not happens(pull,Time).

terminates(push,backwards,Time) :- not happens(pull,Time).

initiates(pull,backwards,Time) :- not happens(push,Time).

terminates(pull,forwards,Time) :- not happens(push,Time).

initiates(pull,spinning,Time) :- happens(push,Time).

terminates(pull,forwards,Time) :- happens(push,Time).

terminates(pull,backwards,Time) :- happens(push,Time).

terminates(push,spinning,Time) :- not happens(pull,Time).

terminates(pull,spinning,Time) :- not happens(push,Time).

:- releasedAt(forwards,0).

:- releasedAt(backwards,0).

:- releasedAt(spinning,0).

:- holdsAt(forwards,0).

:- holdsAt(backwards,0).

:- holdsAt(spinning,0).

happens(push,15).

happens(pull,15).

happens(pull,35).

happens(push,35).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
