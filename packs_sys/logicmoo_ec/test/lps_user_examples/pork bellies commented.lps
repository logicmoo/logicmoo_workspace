:- expects_dialect(lps).

/** We take Nick Szabo's specification of a pork bellies futures smart contract:

  future(rightA="1 round lot pork bellies",
         rightB="$1,500.00",
         p = "for delivery in July 2002") =
      when withinPeriod(p)
          to Holder rightA   with   to Counterparty rightB
      then terminate

and implement it as a Logical Contract (LPS compilable to Ethereum) below.

Click on the "Run!" button at the lower right of these windows to execute.
*/

/** BEGIN set up of test environment */
simulatedRealTimeBeginning('2002-11-01'). /** Initialize 'T'. */
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(200).  
maxRealTime(M) :- M is 24*3600*90. % 90 days   NOT WORKING???
minCycleTime(Min) :- Min is 3600*12. % just 2 LPS cycles per calendar day
:- include(system('date_utils.pl')). 
/** END set up of test environment */

/** BEGIN declaration of fluents, events and actions
	in terms of which goals (rights and obligations) and beliefs will be asserted below. */
fluents  future(_RightA,_RightB, _Period), fulfilled(_RightA,_RightB).
events  to(_Agent,_Right).
actions confirmCompliance(_RightA,_RightB, _Period), legalAction(_RightA,_RightB, _Period).
/**END declaration of fluents, events and actions
	in terms of which goals (rights and obligations) and beliefs will be asserted below. */

/** Assert rights and obligations (goals) of the contract. */
/** Assert that fulfillment within the term completes the contract. */
if 	future(RightA,RightB,First-Last) at T, 
 	real_date(Today) at T, Today @>= First, Last @>= Today, 
	fulfilled(RightA,RightB) at T
then 	confirmCompliance(RightA,RightB,First-Last) from T, 
	terminate future(RightA,RightB,First-Last) from T.
/** Asserted that fulfillment within the term completes the contract. */

/** Assert that failure to fulfill within the term defaults to legal action on the contract. */
if 	future(RightA,RightB,First-Last) at T, 
	real_date(Last) at T,
	not fulfilled(RightA,RightB) at T
then 	legalAction(RightA,RightB,First-Last) from T,
	terminate future(RightA,RightB,First-Last) from T.
/** Asserted that failure to fulfill within the term defaults to legal action on the contract. */

/** Assert that when both parties have fulfilled their obligations the order is fulfilled. */
if	to(holder,RightA) to T,
	to(counterParty,RightB) to T
then 	initiate fulfilled(RightA,RightB) from T.
/** Asserted that when both parties have fulfilled their obligations the order is fulfilled. */
/** Asserted rights and obligations (goals) of the contract. */

/** No beliefs are asserted. 
 */

/** Test the contract. */
initially future("1 round lot pork bellies", usd(1500), 2002/12/1 - 2002/12/31).
initially future("christmas turkey", usd(15), 2002/12/21 - 2002/12/24).
observe to(holder,"1 round lot pork bellies") at '2002-12-15T11:00'.
observe to(counterParty,usd(1500)) at '2002-12-15T11:00'.
observe to(holder,"christmas turkey") at '2002-12-23T11:00'.
% observe to(counterParty,usd(15)) at '2002-12-23T11:00'.
/** Tested the contract. */

/** <examples>
?- go.
?- go(Timeline).
*/