/* Original:
(The insured is the Holder, the insurer is the Counterparty)
insureGoods(goodsPremium, principal, penalty, t1, t2, goodsInsured) =
    counterpartySecurity =  pledge(allGoods(Counterparty))
    with to Counterparty getTitle(goodsPremium)
    insurancePayment(goodsInsured, principal, t1, t2)
    with when breachedPerformance(insurancePayment)
        to Holder foreclose(counterpartySecurity, penalty)
insurancePayment(goodsInsured, principal, t1, t2) =
    when safeArrival(goodsInsured) terminate % buggy, IMHO: should check dates
    when withinPeriod(t1,t2)
        when choiceOf(Holder)
            to Holder principal
*/
:- include(system('date_utils.pl')). 

% Rather then run live, we'll simulate real time by mapping its time points to simulation cycles:
simulatedRealTimeBeginning('2018-05-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*120. % 120 days max lifetime of the contract

events to(_Agent,_Right), pledge(_Goods,_Security), safeArrival(_GoodsInsured), choiceOf(_Party), foreclose(_Security,_Amount).

insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) from _ to _ if
	pledge(allGoods(counterparty),CounterpartySecurity) to M1, 
	to(counterParty,getTitle(GoodsPremium)) to M1,
	T1 @>= M1,
	( if insurancePayment(GoodsInsured,Principal) from T1 to T2_ then T2_ @=< T2
		else to(holder,foreclose(CounterpartySecurity, Penalty)) to T2_, T2_ @>= T2 ). % this can only happens after T2...
	% add true at Finish...?

insurancePayment(GoodsInsured, _Principal) from _ to _ if
    safeArrival(GoodsInsured). 
insurancePayment(_GoodsInsured, Principal) from _ to _ if
    choiceOf(holder) from T, to(holder,Principal) from T.

false safeArrival(_), choiceOf(_). % can't have both at the same time

if true then 
	insureGoods(usd(600), usd(120000), usd(5000), 2018/5/27 - 2018/7/31, "10 John D. tractors") to T,
	T @=< 2018/8/10, % establish a future limit for foreclosing
	writeln('Compliant!') from T.
	
% simulate shipment from US to Europe
% these must precede the insurance period per se:
observe pledge(allGoods(counterparty),"Some key to all Counterparty goods") at 2018/5/20.
observe to(counterParty,getTitle(usd(600))) at "2018-05-20".

% Now events pertaining to the contracted period:
% Instead of this file use the ones that include it and add events.


