:- expects_dialect(lps).

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

end_of_file.
% @TODO unbreak

:- include(system('date_utils.pl')). 

% Rather then run live, we'll simulate real time by mapping its time points to simulation cycles:
simulatedRealTimeBeginning('2018-05-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*120. % 120 days max lifetime of the contract

events to(_Agent,_Right), pledge(_Goods,_Security), safeArrival(_GoodsInsured), choiceOf(_Party), foreclose(_Security,_Amount).
% action to(holder, foreclose(_,_)). doesn't work.
          
insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) from Start to Finish if
	pledge(allGoods(counterparty),CounterpartySecurity) from Start, 
	to(counterParty,getTitle(GoodsPremium)) from Start, writeln('insured') from T,
		( if insurancePayment(GoodsInsured,Principal) to Finish,   withinPeriod(T1-T2) at Finish 
        then  true
		else to(holder,foreclose(CounterpartySecurity, Penalty)) to Finish, Finish @>= T2 ). % this can only happens after T2...
	
insurancePayment(GoodsInsured, _Principal) to Time  if
    safeArrival(GoodsInsured) to Time, writeln('arrived') from T.
insurancePayment(_GoodsInsured, Principal) to Time if
    choiceOf(holder) to Time, to(holder,Principal) to Time.

false safeArrival(_), choiceOf(_). % can't have both at the same time
% Also need constraint for no foreclosure if safe arrival or if principal paid.

if true then 
	insureGoods(usd(600), usd(120000), usd(5000), 2018/5/27 - 2018/7/31, "10 John D. tractors") to T,
%	T @=< 2018/8/10, % establish a future limit for foreclosing
	writeln('Compliant!') from T.

	
% simulate shipment from US to Europe
% these must precede the insurance period per se:
observe pledge(allGoods(counterparty),"Some key to all Counterparty goods") at 2018/5/20.
observe to(counterParty,getTitle(usd(600))) at "2018-05-20".
% observe safeArrival("10 John D. tractors") at 2018/6/1.
observe to(holder,foreclose("Some key to all Counterparty goods", usd(5000) )) at 2018/8/1.

% Now events pertaining to the contracted period:
% Instead of this file use the ones that include it and add events.

/** <examples>
?- godc(Timeline).
?- go.
?- holds(withinPeriod( 2018/5/27 - 2018/7/31), 2018/6/1).
*/
