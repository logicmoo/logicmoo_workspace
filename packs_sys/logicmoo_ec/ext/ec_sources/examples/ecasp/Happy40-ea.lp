#domain time(Time), person(Person).

time(0..40).
offset(1..1).

person(fred).


fluent(happy(Person)).
fluent(hungry(Person)).
fluent(cold(Person)).

event(feed(Person)).
event(clothe(Person)).



:- holdsAt(happy(Person),Time), holdsAt(cold(Person),Time).

:- holdsAt(happy(Person),Time), holdsAt(hungry(Person),Time).

holdsAt(happy(Person),Time) :- not holdsAt(hungry(Person),Time), not holdsAt(cold(Person),Time).

terminates(feed(Person),hungry(Person),Time).

terminates(clothe(Person),cold(Person),Time).

releasedAt(happy(Person),Time).

:- releasedAt(hungry(Person),0).

:- releasedAt(cold(Person),0).

holdsAt(hungry(fred),0).

:- holdsAt(cold(fred),0).

happens(feed(fred),25).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
