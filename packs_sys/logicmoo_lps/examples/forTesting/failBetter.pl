
:- expects_dialect(lps).

maxTime(10).

events to(_Agent,_Right).

withinPeriod(First-Last) at T if
	T >= First, Last >= T.
    
future(RightA,RightB,Period) from _ if
	withinPeriod(Period) at T, 
	to(holder,RightA) from T,  to(counterParty,RightB) from T.

% we need to discover that the consequent is doomed to fail, given the following observations
% time limit 7 is detected, so the engine stops then reporting failure, rather then continuing
if true then 
	future("1 round lot pork bellies", usd(1500), 3 - 7) from _ to _.

observe to(holder,"1 round lot pork bellies") at 4.
observe to(counterParty,usd(1500)) at 5. % too late!
