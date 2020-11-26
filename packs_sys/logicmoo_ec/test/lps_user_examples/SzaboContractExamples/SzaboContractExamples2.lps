:- expects_dialect(lps).

% Examples from https://nakamotoinstitute.org/contract-language/

% Use for compliance: we'll be checking these events occur properly
events to(_Agent,_Right).

simulatedRealTimeBeginning('2002-06-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(200).  
maxRealTime(M) :- M is 24*3600*90. % 90 days   NOT WORKING???
minCycleTime(Min) :- Min is 3600*12. % just 2 LPS cycles per calendar day

% System primitives used next
:- include(system('date_utils.pl')). 
% withinPeriod(Period): Period is StartDate-EndDate, where each of these dates is Year/Month/Day, with integers
% TODO: try to "cut" alternatives after Today>@>Last ...??
withinPeriod(First-Last) at T if
	real_date(Today) at T, Today @>= First, Last @>= Today.

% also needed: LPS engine must be cleverer detecting failures due to time never going back 

/* First Example - Futures Contract
Original:
future(rightA="1 round lot pork bellies",
       rightB="$1,500.00",
       p = "for delivery in July 2002") =

    when withinPeriod(p)
        to Holder rightA   with   to Counterparty rightB
    then terminate */
    
future(RightA,RightB,Period) if
	withinPeriod(Period) at T,
	to(holder,RightA) from T,  to(counterParty,RightB) from T.

if true then 
	future("1 round lot pork bellies", usd(1500), 2002/7/1 - 2002/7/31).

observe to(holder,"1 round lot pork bellies") at '2002-07-15T11:00'.
%observe to(counterParty,usd(1500)) at '2002-07-15T11:00'.

% System primitives used next
beforeTime(TimeExpression) if 
	convert(TimeExpression,Instant), real_time(Now) at T, Now @> Instant.
afterTime(TimeExpression) if 
	convert(TimeExpression,Instant), real_time(Now) at T, Now @< Instant.
	
/* Second Example - Option Contract
Original:
callOptionAmerican (rightA="1 round lot XYZ Corp.",
                    rightB="$2,000/lot",
                    time="end of trading on last trading day of August") =
    when beforeTime(time)
        when choiceOf(Holder)
            to Holder rightA with to Counterparty rightB
    when afterTime(time)
        terminate */

callOptionAmerican(RightA,RightB,Time) if
	beforeTime(Time) at T,
	choiceOf(holder) from T to End,
	to(holder,RightA) from T to End, to(counterParty,RightB) from T to End.
callOptionAmerican(RightA,RightB,Time) if
	afterTime(Time) at T,
	lps_terminate from T.

if true then
	callOptionAmerican("1 round lot XYZ Corp.","$2,000/lot","end of trading on last trading day of August").

observe to(holder,"1 round lot XYZ Corp.") at '2019-08-30T16:00'.
observe to(counterParty,usd(2000)) at '2019-08-30T16:00'.

% System primitives used next
schedule(StringExpression,Intervals) if ...
	% Generate list of Moment1-Moment2 intervals
	
% Introducing schedule in a bond

bond(Coupon, Principal, Schedule) if
	schedule(Expression,Intervals),
	bond(Coupon, Principal, Intervals).
	
bond(Coupon, Principal, [Next|More])  if
	More \== [],
	withinPeriod(Next) at T1,
	to(holder,Coupon) from T1 to T2,
	bond(Coupon, Principal, More) from T2.
bond(Coupon, Principal, [LastInterval])  if
	withinPeriod(LastInterval) at T,
	to(holder,Principal) from T.

% System primitives used next
..., Event until FluentCondition otherwise Action, ...
	means:
...initiate must_123, Event to T, terminate must_123 from T, ...
	plus:
if FluentCondition at T, must_123 at T then
   MacroAction from T, terminate must_123 from T.

% or perhaps better...:

... , if Condition then Then else Else,...
	means:
... , initiate if_123, if_then_else(Condition,Then,Else),...
	plus:
if_then_else(Condition,Then,Else) if Condition, terminate if_123, Then.
if_then_else(Condition,Then,Else) if if_123, Else.

% Insurance

/* Original:
loan(goods, principal, penalty, t1, t2) =
    counterpartySecurity =  pledge(allGoods(Counterparty))
    with to Counterparty getTitle(goods)
    loanPayment(principal, t1, t2)
    with when breachedPerformance(loanPayment)
        to Holder foreclose(counterpartySecurity, penalty)
loanPayment(principal, t1, t2) =
    when withinPeriod(t1,t2)
        when choiceOf(Holder)
            to Holder principal */

/* loan(Goods, Principal, Penalty, T1-T2) if
	pledge(allGoods(counterparty),CounterpartySecurity) from M1 to M2, 
	to(counterParty,getTitle(Goods)) from M1 to M2,
	loanPayment(Principal, T1-T2) until real_date(T2) 
		otherwise to(holder,foreclose(counterpartySecurity, Penalty)).
using if then else instead:*/

loan(Goods, Principal, Penalty, T1-T2) if
	pledge(allGoods(counterparty),CounterpartySecurity) from M1 to M2, 
	to(counterParty,getTitle(Goods)) from M1 to M2,
	if ( loanPayment(Principal, T1-T2) to T, real_date(T3) at T )
		then T3 @=< T2 
    	else to(holder,foreclose(counterpartySecurity, Penalty)).

loanPayment(Principal, T1-T2) if
	withinPeriod(T1-T2) at T,
	choiceOf(holder) from T to End, 
	to(holder,Principal) from T to End.

if true then
	loan("some house",usd(300000), usd(10000), 2018/10/1-2018/12/31).


/* Original:
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
/* insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) if
	pledge(allGoods(counterparty),CounterpartySecurity) from M1 to M2, 
	to(counterParty,getTitle(GoodsPremium)) from M1 to M2,
	insurancePayment(GoodsInsured,Principal,T1-T2) until real_date(T2)
		otherwise to(holder,foreclose(counterpartySecurity, Penalty)).
using if then else instead: */

insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) if
	pledge(allGoods(counterparty),CounterpartySecurity) from M1 to M2, 
	to(counterParty,getTitle(GoodsPremium)) from M1 to M2,
	if (insurancePayment(GoodsInsured,Principal,T1-T2) to T, real_date(T3) at T)
		then T3@=<T2
		else to(holder,foreclose(counterpartySecurity, Penalty)).

insurancePayment(GoodsInsured, Principal, T1-T2) if
    safeArrival(goodsInsured) to T. 
insurancePayment(GoodsInsured, Principal, T1, T2) if
	not safeArrival(goodsInsured) at T,
    withinPeriod(T1-T2) at T,
    choiceOf(Holder) from T, to(Holder,Principal) from T.

/** <examples> 
?- go(Timeline).
*/


