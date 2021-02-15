
:- expects_dialect(lps).

% First example from https://nakamotoinstitute.org/contract-language/

simulatedRealTimeBeginning('2002-07-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*90. % 90 days  
:- include(system('date_utils.pl')). 

events to(_Agent,_Right).

future(RightA,RightB) from _Begin to _End if
	to(holder,RightA) from T,  to(counterParty,RightB) from T.

% The contract must be enforced:
if true then 
	future("1 round lot pork bellies", usd(1500)) from 2002/7/2 to 2002/7/4.

% These must cause the program to (finitely) fail, as contract time runs out:
observe to(holder,"1 round lot pork bellies") at 2002/7/3/16.
% observe to(counterParty,usd(1500)) at 2002/7/3/14.
