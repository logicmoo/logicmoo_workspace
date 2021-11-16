person(john).
person(mary).
person(suzy).

drivesIn(X, countryside) :- person(X).

drivesIn(X, redCar) :- drivesIn(X, countryside), not -drivesIn(X, redCar).

-drivesIn(X, redCar) :- person(X), person(Y), person(Z), X != Y, X != Z, Y != Z, drivesIn(Y, redCar), drivesIn(Z, redCar).
