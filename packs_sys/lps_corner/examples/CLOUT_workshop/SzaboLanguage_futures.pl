
:- expects_dialect(lps).

/*
Original first example from https://nakamotoinstitute.org/contract-language/ :
future(rightA="1 round lot pork bellies",
       rightB="$1,500.00",
       p = "for delivery in July 2002") =

    when withinPeriod(p)
        to Holder rightA   with   to Counterparty rightB
    then terminate 

Fully working logical contract:
*/
:- include(system('date_utils.pl')). 

% Rather then run live, we'll simulate real time by mapping its time points to simulation cycles:
simulatedRealTimeBeginning('2002-07-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*90. % 90 days max lifetime of the contract

events to(_Agent,_Right).

future(RightA,RightB) from _Begin to _End if
	to(holder,RightA) from T,  to(counterParty,RightB) from T.
% Note that the time interval of a composite event head implicitly includes all times in its body

% The contract must be enforced:
if true then 
	future("1 round lot pork bellies", usd(1500)) from 2002/7/2 to 2002/7/4.

% If these timely observations come in, the world complies to the contract and the program succeeds
% Otherwise, the program fails, because the contract is violated
observe to(holder,"1 round lot pork bellies") at '2002-07-02T11:00'.
observe to(counterParty,usd(1500)) at '2002-07-02T11:00'.

% alternate form, denoting the first instant of the following moments:
% observe to(counterParty,usd(1500)) at 2002/7/2/11.

/** <examples>
?- go(Timeline).
*/