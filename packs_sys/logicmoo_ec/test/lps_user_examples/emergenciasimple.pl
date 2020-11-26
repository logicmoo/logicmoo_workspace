:- expects_dialect(lps).

maxTime(5).

% fluents 	emergencia.
actions 	presione_el_botón.
events 		emergencia, alerte_al_conductor.

observe emergencia from 1 to 2.

if 		emergencia from T1 to T2
then 	alerte_al_conductor from T2 to T3.

alerte_al_conductor from T1 to T2
if 		presione_el_botón from T1 to T2. 

/** <examples>
?- go(Timeline).
*/