
:- expects_dialect(lps).

maxTime(10).
events e1,e2.
actions a.

observe([e1,e2],T) :- between(4,7,T).

if not e1 from T1 to T2, not e2 from T3 to T4, T3>T2 then
	a from T4+1.


/** <examples> 
?- go(Timeline).
*/

