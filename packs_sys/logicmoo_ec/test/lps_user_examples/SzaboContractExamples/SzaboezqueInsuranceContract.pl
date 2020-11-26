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

events to(_Agent,_Right), pledge(_Goods,_Security), safeArrival(_GoodsInsured), choiceOf(_Party), foreclose(_Security,_Amount).

insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) from ContractSigned if
	pledge(allGoods(counterparty),CounterpartySecurity) from ContractSigned, 
	to(counterParty,getTitle(GoodsPremium)) from ContractSigned,
	T1 @> ContractSigned,
	( if insurancePayment(GoodsInsured,Principal) from T, T@>=T1, T@<T2 then true
		else to(holder,foreclose(CounterpartySecurity, Penalty)) ). 

insurancePayment(GoodsInsured, _Principal) from T if
    safeArrival(GoodsInsured) from T. 
insurancePayment(_GoodsInsured, Principal) from T if
    choiceOf(holder) from T, to(holder,Principal) from T.

false safeArrival(_), choiceOf(_). % can't have both at the same time

if insuranceContract(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured, ContractLimit) then
	insureGoods(GoodsPremium, Principal, Penalty, T1-T2, GoodsInsured) to T,
	T @=< ContractLimit,
	writeln('Compliant!') from T.