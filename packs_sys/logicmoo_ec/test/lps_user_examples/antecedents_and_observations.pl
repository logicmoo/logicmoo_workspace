:- expects_dialect(lps).

maxTime(10).
actions a1, r1, a2, a3.
events e2.
fluents f1.

observe     a1 from 1 to 2.
observe     a2 from 2 to 3.
observe		e2 from 3 to 6.
observe		a3 from 8 to 9.


if    a1 from T1 to T2, e1 from T2 to T3
then r1 from T3 to T4.

e1 from T1 to T4 if a2 from T1 to T2, e2 from T3 to T4.
e2 from T1 to T2 if a3 from T3 to T4.

e1 initiates f1.