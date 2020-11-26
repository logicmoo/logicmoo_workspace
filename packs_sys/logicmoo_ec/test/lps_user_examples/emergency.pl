:- expects_dialect(lps).

maxTime(5).

fluents 	emergency.
actions 	press(_).
events 		alert/1.

initially emergency. 

if 		emergency at T1
then 	alert(driver) from T1 to T2.

alert(driver) from T1 to T2
if 		press(buttom) from T1 to T2.

alert(driver) terminates emergency. 

/** <examples>
?- go(Timeline).
*/