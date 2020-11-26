
end_of_file.
% @TODO unbreak
:- expects_dialect(lps).

/*
A simple loan formulation where Bob has 5 euros and needs for 50 euros "now" and Alice has 150 euros and needs 165 euros "later". Alice uses the external market rate of risk free bonds of 10% as an expectation for her money value over time. So Bob's need is met by Alice "now" and Alice's need for cash "later" is met by Bob over time. This convergent arrangement of now and later constitutes the core functional construct of the contract. 

Bob and Alice have rights to resources which is modelled as Claim to resources. for e.g., Bob having cash of 5 euros is modelled as Bob having a Claim to 5 Euros cash. Additional (notional) resources are modelled for tracking exchanges.

A necessary notion for Bob's ability to pay is his solvency over time - sort of a condition that Bob's assets will always exceed his liabilities. Towards this, a notional resource HumanCapital is modelled. The Human Capital is a proxy for Bob converting labour over time to cash. Bob earns (income) by converting his labour to cash. 

*/

:- include(system('date_utils.pl')). 
simulatedRealTimeBeginning('2018-07-31'). 
simulatedRealTimePerCycle(8640). % Each cycle = 8640 seconds = 1/10th of a day.
maxTime(200). % 200 cycles = 20 days.

/*
Fluents represent states of interest. The sense in which they are used here represents states of financial resources and financial information of interest. 
The state of financial resources is represented in a fluent called claimAtTime, with parameters as Agents, Resources and Values.
Financial information is modelled as external information relevant for the model. The sense in which they are used here represents key external financial information, which includes income, rates, repayments and publicly available risk information
*/ 


fluents	claimAtTime(_Agent,_Resource,_Unit,_Value),
		needAtTime(_Agent,_Resource,_Unit,_Value,_Time),
		incomeAtTime(_Agent,_Unit,_Value),
		paymentAtTime(_Agent,_Unit,_Value),
		settled(_AgentFrom,_AgentTo,_Resource,_Unit,_Value,_Time),
		rateAtTime(_Resource,_Basis,_Value).

events	claim(_Agent,_Resource,_Unit,_Value),
		need(_Agent,_Resource,_Unit,_Value,_Time),
		earning(_Agent,_Unit,_Value),
		repayment(_Agent,_Unit,_Value),
		pricingInfo(_Resource,_Basis,_Value).

actions settle(_AgentFrom,_AgentTo,_Resource,_Unit,_Value),
		transfer(_Agent,_ResourceFrom,_ResourceTo,_Unit,_Value),
		needMet(_Agent,_Resource,_Unit,_Value,_Time),
		registerIncome(_Agent,_Unit,_Value),
		markSettled(_AgentFrom,_AgentTo,_Resource,_Unit,_Value,_Time),
		registerPayment(_Agent,_Unit,_Value).
	
initially	claimAtTime(bob,needResource,euros,0),
			claimAtTime(alice,needResource,euros,0),
			claimAtTime(bob,cash,euros,5),
			claimAtTime(alice,cash,euros,150),
			settled(alice,bob,needResource,euros,0,0),	
			claimAtTime(bob,humanCapital,euros,100),
			rateAtTime(defFree,pct,0).
		

observe	need(bob,cash,euros,50,0) at  '2018-08-01T09:00'.
observe	need(alice,cash,euros,180,20) at  '2018-08-01T09:00'. % This part is tricky - how do we register a future need now?
observe	pricingInfo(defFree,pct,0.1) at  '2018-08-01T09:00'.

observe	earning(bob,euros,35) at  '2018-08-02T18:00'.
observe	earning(bob,euros,40) at  '2018-08-05T18:00'.

observe	repayment(bob,euros,27) at  '2018-08-03T18:00'.
observe	repayment(bob,euros,30) at  '2018-08-05T18:00'.
observe	repayment(bob,euros,20) at  '2018-08-06T18:00'.



claim(Agent,Resource,Unit,Value)	initiates	claimAtTime(Agent,Resource,Unit,Value).

need(Agent,Resource,Unit,Value,Time) 	initiates	needAtTime(Agent,Resource,Unit,Value,Time).

needMet(Agent,Resource,Unit,Value,Time)	initiates	needAtTime(Agent,Resource,Unit,Value1,Time)
if needAtTime(Agent,Resource,Unit,Value2,Time), Value1 is Value2-Value.

needMet(Agent,Resource,Unit,Value,Time) terminates	needAtTime(Agent,Resource,_,_,_).

earning(Agent,Unit,Value)			initiates	incomeAtTime(Agent,Unit,Value).

registerIncome(Agent,Unit,Value)	updates	OldValue to 0 in incomeAtTime(Agent,Unit,OldValue)
											if	OldValue > 0.

repayment(Agent,Unit,Value)			initiates	paymentAtTime(Agent,Unit,Value).

registerPayment(Agent,Unit,Value)	updates	OldValue to 0 in 
							paymentAtTime(Agent,Unit,OldValue).

markSettled(AgentFrom,AgentTo,Resource,Unit,_,Time) updates OldTime to Time
									in settled(AgentFrom,AgentTo,Resource,Unit,_,OldTime).
									

pricingInfo(Resource,Basis,Value)	updates	OldValue to Value in rateAtTime(Resource,Basis,OldValue).

/*
Goals of the Agent, as represented by Reactive Rules

In this case, registering an income event as an external input has the goal of triggering a conversion from human capital resource to labour income
With this, the context of resources at hand, in terms of claims as well as a (sort of) deterministic profile of additional claims over time, completes the context needed
*/
/*
(Labour) Income event is ingested as a use of Human Capital to earn Cash
*/


if 		incomeAtTime(Agent,Unit,Value) at T1, Value > 0
then	
		registerIncome(Agent,Unit,Value) from T1 to T2,
		transfer(Agent,humanCapital,cash,Unit,Value) from T2 to T3.

/*
if 		paymentAtTime(Agent,Unit,Value) at T1, Value > 0
then	
		registerPayment(Agent,Unit,Value) from T1 to T2.
*/

/*
Bob's need now is met by Alice providing cash
*/


if 		needAtTime(bob, cash,euros,Amount,Time) at T1,	Amount > 0,
		claimAtTime(alice,cash,euros,LendableAmt) at T1,
		Amount < LendableAmt, Amount > 0
then 	settle(alice,bob,cash,euros,Amount) from T1 to T2,
		needMet(bob,cash,euros,Amount,T2) from T1 to T2,
		transfer(bob,cash,needResource,euros,Amount) from T2 to T3,
		settle(bob,alice,needResource,euros,Amount) from T3 to T4. 


/*
Once Bob's need is met, the contract has to work at meeting Alice's need by processing repayments from Bob
*/

if 		settle(bob,alice,needResource,euros,Amount1) from T1 to T2, Amount1 > 0, 
		paymentAtTime(bob,euros,PaymentAmt) , PaymentAmt > 0,
		claimAtTime(bob,cash,euros,Balance) at T3, Balance >= PaymentAmt,
		settled(alice,bob,needResource,euros,_,LastTime), LastTime < T2,
   		rateAtTime(defFree,pct,Rate) at T3,
%		real_date_add(T2,UsePeriod,T3),   % Really stuck at this - how to get Use Period in terms of days?
		UsePeriod is T3 - T2,
		Amount2 is Amount1 + (UsePeriod/10) * Rate * Amount1,
		Amount2 > PaymentAmt
then	
		registerPayment(bob,euros,PaymentAmt) from T3 to T4,
		settle(bob,alice,cash,euros,PaymentAmt) from T3 to T4, T4 > T3,
		settle(alice,bob,needResource,euros,Amount1) from T4 to T5, T5 > T4,
		Amount3 is Amount2 - PaymentAmt,
		markSettled(alice,bob,needResource,euros,Amount1,T5) from T4 to T5,
		settle(bob,alice,needResource,euros,Amount3) from T5 to T6, T6 > T5.
		


if 		settle(bob,alice,needResource,euros,Amount1) from T1 to T2, Amount1 > 0, 
		paymentAtTime(bob,euros,PaymentAmt) , PaymentAmt > 0,
		claimAtTime(bob,cash,euros,Balance) at T3, Balance >= PaymentAmt,
		settled(alice,bob,needResource,euros,_,LastTime), LastTime < T2,
		rateAtTime(defFree,pct,Rate) at T3,
		UsePeriod is T3 - T2,
		Amount2 is Amount1 + (UsePeriod/10) * Rate * Amount1,
		Amount2 < PaymentAmt
then	
		registerPayment(bob,euros,Amount2) from T3 to T4,
		settle(bob,alice,cash,euros,Amount2) from T3 to T4, T4 > T3,
		settle(alice,bob,needResource,euros,Amount1) from T4 to T5, T5 > T4,
		claimAtTime(alice,cash,euros,TotalAmt) at T5,
		needMet(alice,cash,euros,TotalAmt,T4) from T5 to T6, T6 > T5,
		markSettled(alice,bob,needResource,euros,Amount1,T5) from T5 to T6.


/* Somhow, the updates version of settle is not working properly - not sure why
settle(Giver, Receiver,Resource,Unit,Amount) updates Amount1 to Amount2 in claimAtTime(Receiver,Resource,Unit, Amount1)
			if Amount2 is Amount1 + Amount.
       
settle(Giver, Receiver,Resource,Unit,Amount) updates Amount1 to Amount2 in claimAtTime(Giver,Resource,Unit, Amount2)
			if Amount2 is Amount1 - Amount.
*/

settle(Giver,Receiver,Resource,Unit,Amount) initiates claimAtTime(Receiver,Resource,Unit, Amount2)
if 	claimAtTime(Receiver,Resource,Unit, Amount1), Amount2 is Amount1 + Amount.

settle(Giver,Receiver,Resource,Unit, Amount) initiates claimAtTime(Giver, Resource,Unit,Amount2)
if	claimAtTime(Giver,Resource,Unit, Amount1), Amount2 is Amount1 - Amount.

settle(Giver, Receiver,Resource,Unit,Amount) terminates claimAtTime(Receiver,Resource,_,_).
settle(Giver, Receiver,Resource,Unit,Amount) terminates claimAtTime(Giver,Resource,_,_).


transfer(Agent, FromResource,ToResource,Unit,Amount) initiates claimAtTime(Agent,ToResource,Unit, Amount2)
if 	claimAtTime(Agent,ToResource,Unit, Amount1), Amount2 is Amount1 + Amount.

transfer(Agent,FromResource,ToResource,Unit, Amount) initiates claimAtTime(Agent, FromResource,Unit,Amount2)
if	claimAtTime(Agent,FromResource,Unit, Amount1), Amount2 is Amount1 - Amount.

transfer(Agent,FromResource,ToResource,Unit,Amount) terminates claimAtTime(Agent,FromResource,_,_).

transfer(Agent,FromResource,ToResource,Unit,Amount) terminates claimAtTime(Agent,ToResource,_,_).

/* 2D Animated Display */ 

/* Bob's World */
d(claimAtTime(Person,humanCapital,Unit,V), 
	[from:[X,Y], to:[RightX,TopY], label:(hC:Value), type:rectangle,  fontSize:13, fillColor:[1,0.746,0] ]
	) :- 
    (Person=bob,X=20,Y=200),
    RightX is X+70, TopY is V+Y, Value is round(V). 

d(claimAtTime(Person,cash,Unit,V), 
	[from:[X,Y], to:[RightX,TopY], label:(cash:Value), type:rectangle,  fontSize:13, fillColor:'#85bb65' ]
	) :- 
    (Person=bob,X=140,Y=200),
    RightX is X+70, TopY is V+Y, Value is round(V).

d(claimAtTime(Person,needResource,Unit,V), 
	[from:[X,Y], to:[RightX,TopY], label:(nR:Value), type:rectangle,  fontSize:13, fillColor:[1,0.646,1] ]
	) :- 
    (Person=bob,X=380,Y=200),
    RightX is X+70, TopY is V+Y, Value is round(V).


/* Alice's World */ 
d(claimAtTime(Person,cash,Unit,V), 
	[from:[X,Y], to:[RightX,TopY], label:(cash:Value), type:rectangle,  fontSize:13, fillColor:'#85bb65' ]
	) :- 
    (Person=alice,X=140,Y=5), Value is round(V),
    RightX is X+70, TopY is V+Y.

d(claimAtTime(Person,needResource,Unit,V), 
	[from:[X,Y], to:[RightX,TopY], label:(nR:Value), type:rectangle,  fontSize:13, fillColor:[1,0.746,1] ]
	) :- 
    (Person=alice,X=380,Y=5),
    RightX is X+70, TopY is V+Y, Value is round(V).

/* Transfers */

d(transfer(Agent,From,To,U,Amount),[type:arrow, label:(transfer:Value), from:[FX,FY], to:[TX,TY]]) :- 
    Agent=bob,From=cash,To=needResource,FX=200,FY=200,TX=400,TY=200, Value is round(Amount)
;
    Agent=bob,From=humanCapital,To=cash,FX=80,FY=200,TX=160,TY=200, Value is round(Amount)
.

/* Settlements */

d(settle(From,To,Resource,Unit,Amount),[type:arrow, label:(settle:Value), from:[FX,FY], to:[TX,TY]] ):-

% cash settles
	From=bob,To=alice,Resource=cash, FX=200,FY=200, TX=200,TY=75, Value is round(Amount)
;
	From=alice,To=bob,Resource=cash, FX=200,FY=75, TX=200,TY=200, Value is round(Amount)
;

% needResource settles

	From=bob,To=alice,Resource=needResource, FX=400,FY=200, TX=400,TY=75, Value is round(Amount)
;
	From=alice,To=bob,Resource=needResource, FX=400,FY=75, TX=400,TY=200, Value is round(Amount)
.

d(claim(From,Resource,Unit,Amount),
	[type:star, label:(claim:From:Resource:Amount),center:[650,300], points:4, radius1:4, radius2:15, fillColor:Color ]):-
% Bob claim events
	From=bob,Resource=cash,Color=white
.

d(need(Agent,Resource,Unit,Value,Time),
	[type:star, label:(need:Agent:Resource:Value),center:[700,275], points:4, radius1:4, radius2:15, fillColor:Color, justification:right ]):-
% Bob claim events
	Agent=bob,Color=blue
.

d(earning(Agent,Unit,Value),
	[type:star, label:(earning:Agent:Value),center:[700,250], points:4, radius1:4, radius2:15, fillColor:Color]):-
% Bob claim events
	Agent=bob,Color=green
.

d(pricingInfo(Resource,Basis,Value),
	[type:star, label:(pricingInfo:Resource:Basis:Value),center:[700,200], points:8, radius1:15, radius2:25, fillColor:Color ]):-
% Bob claim events
	From=bob,Color=darkBlue
.


d(repayment(Agent,Unit,Value),
	[type:star, label:(repayment:Agent:Unit:Value),center:[700,175], points:4, radius1:15, radius2:25, fillColor:Color ]):-
	From=bob,Color=orange
.

d(timeless,[ 
    % a display spec can be a list of properties (for one object) or a list of lists (4 objects here:)
   
%    [type:star, center:[450,250], points:9, radius1:20, radius2:25, fillColor:yellow, sendToBack],
% Bob's World    
    [type:rectangle, from:[0,200], to:[600,400], sendToBack, fillColor:[0,0.800,1]], % R,G,B
    [type:pointtext, point:[0,375],fillColor:black, fontSize:20,content:("Bob's World")],   
    [type:star, center:[560,350], points:9, radius1:20, radius2:25, fillColor:yellow],       

%Alice's World
    [type:pointtext, point:[0,130],fillColor:white, fontSize:20,content:("Alice's World")],
    [type:rectangle, from:[0,0], to:[600,175], sendToBack, fillColor:[0,0.450,1]], % R,G,B
    [type:circle, center:[560,120], radius:20, fillColor:white],       
    [type:ellipse, shadowOffset:5, shadowColor:darkGray , point:[480,80], size:[110, 40],fillColor: white],
    [type:ellipse, point:[450,90], size:[90, 30],fillColor: white ],

% External World    
    [type:rectangle, from:[625,0], to:[800,400], sendToBack, fillColor:[0,0.800,1]], % R,G,B
    [type:pointtext, point:[625,385],fillColor:black, fontSize:20,content:("External World")],   
    [type:star, center:[750,350], points:9, radius1:20, radius2:25, fillColor:white]       
  
]).


