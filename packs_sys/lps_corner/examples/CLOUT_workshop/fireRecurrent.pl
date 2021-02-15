
:- expects_dialect(lps).

% fire2


maxTime(10).
fluents 	fire, water.
actions	eliminate, ignite(_), escape, refill.

observe 	ignite(sofa) from 1 to 2.
observe 	ignite(bed) from 4 to 5.
observe		refill from 7 to 8.

initially	water.

flammable(sofa).
flammable(bed).


if 	  fire at T1 
then deal_with_fire from T2 to T3. 

deal_with_fire 	from T1 to T2
if 	eliminate from T1 to T2.

deal_with_fire 	from T1 to T2
if 	escape from T1 to T2.

ignite(Object) 	initiates 	fire  if 	flammable(Object).

eliminate terminates fire.
eliminate terminates water. 
refill initiates water.

false	eliminate, fire, not water.

/** <examples>
?- go(Timeline).
*/
