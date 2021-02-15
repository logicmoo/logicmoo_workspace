
:- expects_dialect(lps).


maxTime(5).

actions paint(_,_).

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

if country(X)
then
    colour(C),
    paint(X,C) from T1 to T2.

false
    paint(X,C),
    adjacent(X,Y),
    paint(Y,C).



/* What happens if we add the following commented out constraint?.
 
false
    paint(X,_), paint(Y,_), X\=Y.
    
*/

/** <examples>
?- go(Timeline).
*/