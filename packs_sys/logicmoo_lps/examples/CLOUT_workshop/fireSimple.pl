
:- expects_dialect(lps).

maxTime(5).

fluents 	fire.
actions 	eliminate, escape.
events 		deal_with_fire.

initially 	fire.

if 		fire at T1 
then 	deal_with_fire from T1 to T2.

deal_with_fire from T1 to T2
if 		eliminate from T1 to T2.

deal_with_fire from T1 to T2 
if		 escape from T1 to T2.

eliminate  terminates fire.


/** <examples>
?- go(Timeline).
*/