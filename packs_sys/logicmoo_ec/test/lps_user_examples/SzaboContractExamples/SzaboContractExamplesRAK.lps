:- expects_dialect(lps).

% Examples from https://nakamotoinstitute.org/contract-language/

% Use for compliance: we'll be checking these events occur properly
events to(_Agent,_Right).

simulatedRealTimeBeginning('2002-06-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(200).  
maxRealTime(M) :- M is 24*3600*90. % 90 days   NOT WORKING???
minCycleTime(Min) :- Min is 3600*12. % just 2 LPS cycles per calendar day
:- include(system('date_utils.pl')). 

% withinPeriod(Period): Period is StartDate-EndDate, 
% where each of these dates is Year/Month/Day, with integers
withinPeriod(First-Last) at T if
	real_date(Today) at T, Today @>= First, Last @>= Today.

/*
fluents future/3.

initially future("1 round lot pork bellies", usd(1500), 2002/7/1 - 2002/7/31).

if 	future(RightA, RightB, Period) at T1
then 	withinPeriod(TPeriod) at T, 
	to(holder, RightA) from T, to(counterparty, RightB) from T.

*/
future("1 round lot pork bellies", usd(1500), 2002/7/1 - 2002/7/31).

if 	future(RightA, RightB, Period) 
then 	withinPeriod(Period) at T, 
	to(holder, RightA) from T, to(counterparty, RightB) from T.

/*

future(RightA,RightB,Period) from _Begin to End if
	withinPeriod(Period) at T,
	to(holder,RightA) from T to End,
	to(counterParty,RightB) from T to End.

if true then future("1 round lot pork bellies", usd(1500), 2002/7/1 - 2002/7/31).




if true then withinPeriod(2002/7/1 - 2002/7/31) at T,
	to(holder,"1 round lot pork bellies") from T to End,
	to(counterParty,usd(1500)) from T to End.
*/

observe to(holder,"1 round lot pork bellies") at '2002-07-15T11:00'.
observe to(counterParty,usd(1500)) at '2002-07-15T11:00'.


/** <examples> 
?- go(Timeline).
*/
