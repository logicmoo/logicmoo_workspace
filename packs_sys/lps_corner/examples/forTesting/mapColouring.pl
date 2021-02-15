
:- expects_dialect(lps).


maxTime(5).

if country(X)
then
    colour(C),
    paint(X,C) from 1 to 2. 
    % relaxing times requires adding: false paint(X,C), painted(X,_).

false
    paint(X,C),
    adjacent(X,Y),
    paint(Y,C).

false
    paint(X,C) from /* from causes weird bug by psyntax*/ T1,
    adjacent(X,Y),
    painted(Y,C) at T1.

false
	paint(Country,C1), paint(Country,C2), C1\=C2.
	
country(iz).
country(oz).
country(az).
country(uz).

colour(red).
colour(yellow).
colour(blue).

adjacent(az,iz).
adjacent(az,oz).
adjacent(iz,oz).
adjacent(iz,uz).
adjacent(oz,uz).

actions paint(_,_).
fluents painted(_G2000,_G2001).

paint(X, C) initiates painted(X,C).

