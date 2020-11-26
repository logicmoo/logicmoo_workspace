#domain time(Time;Time1;Time2;Time3;Time4), place(Place;Place1;Place2), station(Station;Station1;Station2).

time(0..15).
offset(1..1).

place(work).
place(home).
station(herneHill).
station(victoria).
station(southKen).

place(V) :- station(V).
fluent(at(Place)).
fluent(train(Station,Station1)).

event(walkTo(Place)).
event(trainTo(Station)).
event(goToWork).



initiates(walkTo(Place),at(Place),Time).

terminates(walkTo(Place2),at(Place1),Time) :- Place1!=Place2, holdsAt(at(Place1),Time).

initiates(trainTo(Station2),at(Station2),Time) :- holdsAt(train(Station1,Station2),Time), holdsAt(at(Station1),Time).

terminates(trainTo(Station2),at(Station1),Time) :- holdsAt(train(Station1,Station2),Time), holdsAt(at(Station1),Time).

initiates(goToWork,at(work),Time).

terminates(goToWork,at(Place1),Time) :- holdsAt(at(Place1),Time), Place1!=work.

happens3(goToWork,Time1,Time4) :- happens3(walkTo(herneHill),Time1,Time1), happens3(trainTo(victoria),Time2,Time2), happens3(trainTo(southKen),Time3,Time3), happens3(walkTo(work),Time4,Time4), Time1<Time2, Time2<Time3, Time3<Time4, not clipped(Time1,at(herneHill),Time2), not clipped(Time2,at(victoria),Time3), not clipped(Time3,at(southKen),Time4).

holdsAt(train(herneHill,victoria),Time).

holdsAt(train(victoria,southKen),Time).

:- releasedAt(at(Place),0).

:- releasedAt(train(Station1,Station2),0).

:- holdsAt(at(Place1),Time), holdsAt(at(Place2),Time), Place1!=Place2.

:- holdsAt(train(Station1,Station2),Time), Station1==Station2.

:- Station1!=herneHill, Station1!=victoria, holdsAt(train(Station1,Station2),Time).

:- Station2!=victoria, Station2!=southKen, holdsAt(train(Station1,Station2),Time).

:- holdsAt(train(Station1,Station2),Time), Station1==herneHill, Station2==southKen.

holdsAt(at(home),0).

happens3(walkTo(herneHill),1,1).

happens3(trainTo(victoria),6,6).

happens3(trainTo(southKen),10,10).

happens3(walkTo(work),12,12).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
