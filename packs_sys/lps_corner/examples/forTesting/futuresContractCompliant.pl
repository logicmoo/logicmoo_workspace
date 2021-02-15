
:- expects_dialect(lps).

maxTime(10).

events to(_Agent,_Right), future(_,_).

future(RightA,RightB) from _Begin to _End if
	to(holder,RightA) from T,  to(counterParty,RightB) from T.

if true then 
	future("1 round lot pork bellies", usd(1500)) from 3 to 7.

observe to(holder,"1 round lot pork bellies") from 5.
observe to(counterParty,usd(1500)) from 5.
