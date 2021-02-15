
:- expects_dialect(lps).

fluents on(_,_).
m1 from T1 if initiate on(a,b) from T1.
m2 from T1 if initiate on(a,c) from T1.

if true at 1 then m1,m2.

false initiate(on(O,X)), initiate(on(O,Y)), X\=Y.