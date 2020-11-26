:- expects_dialect(lps).

maxTime(10). 

events g/2, gx/1, o.
actions s/1. 

observe o from 1 to 2. 

g(X,Y) if X \== Y, c(X,Z), g(X,Z), g(Z,Y).
g(X,X) if s(X).

gx(Y) from T1 to T2 if c(X,Y), gx(X) from T1 to T2, s(Y). 
gx(Y) from T1 to T2 if s(Y) from T1 to T2.

c(1,2).
c(2,3).
c(3,4).
c(4,5).

if o from T1 to T2 then gx(5) from T2 to T3. 

% if o from T1 to T2 then g(1,5) from T2 to T3.


/** <examples>
?- go.
*/