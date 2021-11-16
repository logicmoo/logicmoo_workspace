c(john).
c(mary).
c(suzy).

head(d(X), drivesIn(X, countryside)) :- c(X).

def(dc(X)) :- c(X).
head(dc(X), drivesIn(X, redCar)) :- c(X).
body(dc(X), drivesIn(X, countryside)) :- c(X).

head(nc(X), neg(drivesIn(X, redCar))) :- c(X).
body(nc(X), drivesIn(Y, redCar)) :- c(X), c(Y), X != Y.
