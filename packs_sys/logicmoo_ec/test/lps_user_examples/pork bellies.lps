:- expects_dialect(lps).

simulatedRealTimeBeginning('2002-11-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(200).  
maxRealTime(M) :- M is 24*3600*90. % 90 days   NOT WORKING???
minCycleTime(Min) :- Min is 3600*12. % just 2 LPS cycles per calendar day
:- include(system('date_utils.pl')). 

fluents  future(_RightA,_RightB, _Period), fulfilled(_RightA,_RightB).
events  to(_Agent,_Right).
actions confirmCompliance(_RightA,_RightB, _Period), legalAction(_RightA,_RightB, _Period).

if 	future(RightA,RightB,First-Last) at T, 
 	real_date(Today) at T, Today @>= First, Last @>= Today, 
	fulfilled(RightA,RightB) at T
then 	confirmCompliance(RightA,RightB,First-Last) from T, 
	terminate future(RightA,RightB,First-Last) from T.

if 	future(RightA,RightB,First-Last) at T, 
	real_date(Last) at T,
	not fulfilled(RightA,RightB) at T
then 	legalAction(RightA,RightB,First-Last) from T,
	terminate future(RightA,RightB,First-Last) from T.

if	to(holder,RightA) to T,
	to(counterParty,RightB) to T
then 	initiate fulfilled(RightA,RightB) from T.


initially future("1 round lot pork bellies", usd(1500), 2002/12/1 - 2002/12/31).
initially future("christmas turkey", usd(15), 2002/12/21 - 2002/12/24).
observe to(holder,"1 round lot pork bellies") at '2002-12-15T11:00'.
observe to(counterParty,usd(1500)) at '2002-12-15T11:00'.
observe to(holder,"christmas turkey") at '2002-12-23T11:00'.
% observe to(counterParty,usd(15)) at '2002-12-23T11:00'.

/** <examples>
?- go.
?- go(Timeline).
*/