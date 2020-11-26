:- expects_dialect(lps).

maxTime(5).

fluents 	emergencia.
actions 	presione_el_botón.
events 		alerte_al_conductor.

observe emergencia from 1 to 2.

if 		emergencia at T1 
then 	alerte_al_conductor from T1 to T2.

alerte_al_conductor from T1 to T2
if 		presione_el_botón from T1 to T2.


/** <examples>
?- go(Timeline).
*/