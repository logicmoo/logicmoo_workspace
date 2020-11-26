:- expects_dialect(lps).

maxTime(10).
actions		transfer(From, To, Amount).
fluents		balance(Person, Amount).

initially	balance(bob, 0), balance(fariba, 100).
observe		transfer(fariba, bob, 10) 	from 1 to 2.

if		transfer(fariba, bob, Any),  
		balance(bob, Amount), Amount >= 10
then	transfer(bob, fariba, 10).

if		transfer(bob, fariba, Any),
		balance(fariba, Amount), Amount >= 20
then  	transfer(fariba, bob, 20).

transfer(From,To,Amount) updates Old to New in balance(To, Old) 
if 		New is Old + Amount.
transfer(From,To,Amount) updates Old to New in balance(From, Old) 
if 		New is Old - Amount.

false	transfer(From, To, Amount), balance(From, Old),
		Old - Amount < 0.





/* Uncomment this to get an experimental 2d display:
d(balance(Person,V), 
	[from:[X,0], to:[RightX,V], label:(Person:V), type:rectangle,  fontSize:13, fillColor:'#85bb65'/* USD:-)*/ ]
	) :- 
    (Person=bob,X=50;Person=fariba,X=200),
    RightX is X+70.
    
d(transfer(From,To,Amount),[type:arrow, label:Amount, from:[FX,20], to:[TX,20]]) :- 
    (From=bob,FX=120,TX=200 ; From=fariba,FX=200,TX=120).

d(timeless,[ 
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