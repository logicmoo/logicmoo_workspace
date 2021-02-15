
:- expects_dialect(lps).

maxTime(10).
actions		transfer(From, To, Amount).
fluents		balance(Person, Amount).

initially	balance(bob, 0), balance(fariba, 100).
observe		transfer(fariba, bob, 10) 	from 1 to 2.

if		transfer(fariba, bob, X) 	from  T1 to T2,  
		balance(bob, A) at T2, A >= 10
then	transfer(bob, fariba, 10) 	from T2 to T3.

if		transfer(bob, fariba, X) 	from  T1 to T2,
		balance(fariba, A) at T2, A >= 20
then  	transfer(fariba, bob, 20) 	from  T2 to T3.

transfer(F,T,A) updates Old to New in balance(T, Old) if New is Old + A.
transfer(F,T,A) updates Old to New in balance(F, Old) if New is Old - A.

false	transfer(From, To, Amount), balance(From, Old),  Old < Amount.
false	transfer(From, To1, Amount1), transfer(From, To2, Amount2),  To1 \=To2.
false	transfer(From1, To, Amount1), transfer(From2, To, Amount2),  From1 \= From2.

fluents happened(_Event,_Time).

happens(E,_,T) initiates happened(E,T) if not happened(E,_).

/* Uncomment this to get an experimental 2d display:
display(balance(Person,V), 
	[from:[X,0], to:[RightX,V], label:(Person:V), type:rectangle,  fontSize:13, fillColor:'#85bb65'/* USD:-)*/ ]
	) :- 
    (Person=bob,X=50;Person=fariba,X=200),
    RightX is X+70.
    
display(transfer(From,To,Amount),[type:arrow, label:Amount, from:[FX,20], to:[TX,20]]) :- 
    (From=bob,FX=120,TX=200 ; From=fariba,FX=200,TX=120).

display(timeless,[ 
    % a display spec can be a list of properties (for one object) or a list of lists (4 objects here:)
    [type:star, center:[250,150], points:9, radius1:20, radius2:25, fillColor:yellow, sendToBack],
    [type:rectangle, from:[0,0], to:[320,200], sendToBack, fillColor:[0,0.746,1]], % R,G,B
    [type:ellipse, shadowOffset:5, shadowColor:darkGray , point:[50,150], size:[110, 40],fillColor: white],
    [type:ellipse,  point:[20,130], size:[90, 30],fillColor: white ]
]).
*/
/** <examples>
?- go(Timeline).
*/