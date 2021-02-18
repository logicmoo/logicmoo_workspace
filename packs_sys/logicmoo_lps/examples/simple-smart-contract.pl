
:- expects_dialect(lps).

maxTime(16).
fluents account(_,_).
actions transfers(_,_,_).

initially account(bob, 10), account(fariba, 100).

day(2, friday).
day(T2, friday) :- day(T1, friday), T2 is T1 + 7.

if 		needs(bob, Amount) at T1,	Amount < 100
then 	transfers(fariba, Amount, bob) from T1 to T2.

if 		transfers(fariba, Amount1, bob) from T1 to T2,
      Amount2 is Amount1 + 0.10 * Amount1, T3 is T2+2
then 	transfers(bob, Amount2, fariba) from T3 to T4.

needs(bob, 10) at T
if	account(bob, Balance) at T,	Balance < 20 ,	day(T, friday).

transfers(Giver, Amount, Receiver) initiates account(Receiver, Amount2)
if 	account(Receiver, Amount1), Amount2 is Amount1 + Amount.

transfers(Giver, Amount, Receiver) initiates account(Giver, Amount2)
if	account(Giver, Amount1), Amount2 is Amount1 - Amount.

transfers(Giver, Amount, Receiver) terminates account(Receiver, _).
transfers(Giver, Amount, Receiver) terminates account(Giver, _).

display(account(Person,V),
	[from:[X,0], to:[RightX,V], label:(Person:V), type:rectangle,  fontSize:13, fillColor:'#85bb65'/* USD:-)*/ ]
	) :-
    (Person=bob,X=50;Person=fariba,X=200),
    RightX is X+70.

display(timeless,[
    % a display spec can be a list of properties (for one object) or a list of lists (4 objects here:)
    [type:star, center:[250,150], points:9, radius1:20, radius2:25, fillColor:yellow, sendToBack],
    [type:rectangle, from:[0,0], to:[320,200], sendToBack, fillColor:[0,0.746,1]], % R,G,B
    [type:ellipse, shadowOffset:5, shadowColor:darkGray , point:[50,150], size:[110, 40],fillColor: white],
    [type:ellipse,  point:[20,130], size:[90, 30],fillColor: white ]
]).
