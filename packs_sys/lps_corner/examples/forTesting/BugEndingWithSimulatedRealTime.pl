
:- expects_dialect(lps).

events to(_Agent,_Right).

simulatedRealTimeBeginning('2002-06-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
%maxTime(60).  no need for this...
maxRealTime(M) :- M is 24*3600*30. % 30 days   
minCycleTime(Min) :- Min is 3600*12. % just 2 LPS cycles per calendar day

% System primitives used next
:- include(system('date_utils.pl')). 
% withinPeriod(Period): Period is StartDate-EndDate, where each of these dates is Year/Month/Day, with integers
% TODO: try to "cut" alternatives after Today>@>Last ...??
withinPeriod(First-Last) at T if
	real_date(Today) at T, Today @>= First, Last @>= Today.


future(RightA,RightB,Period) from _ to _ if
	withinPeriod(Period) at T,
	to(holder,RightA) from T,  to(counterParty,RightB) from T.

if true then 
	future("1 round lot pork bellies", usd(1500), 2002/7/1 - 2002/7/31).

observe to(holder,"1 round lot pork bellies") at '2002-06-05T11:00'.
%observe to(holder,"1 round lot pork bellies") at '2002-07-15T11:00'.
%observe to(counterParty,usd(1500)) at '2002-07-15T11:00'.
