
:- expects_dialect(lps).

maxTime(9).

initially balance(bob,0),balance(fariba,100).

fluents balance(_Person,_Value).

action(transfer(From,To,Amount)). % Why is this still needed?

transfer(From,To,Amount) initiates balance(To,New) if
    balance(To,Old), New is Old+Amount.
transfer(From,To,Amount) initiates balance(From,New) if
    balance(From,Old),
    New is Old-Amount.

transfer(From,To,Amount) terminates balance(To,Old).
transfer(From,To,Amount) terminates balance(From,Old).

observe([transfer(fariba,bob,10)],2).

if transfer(fariba,bob,_X)
then 
    transfer(bob,fariba,10).
    
if transfer(bob,fariba,_X)
then 
    transfer(fariba,bob,20).

false 
    transfer(From,_Any,Amount),
    balance(From,Old),
    Old<Amount.
false 
    transfer(From,To1,_Amount1),
    transfer(From,To2,_Amount2),
    To1\==To2.
false 
    transfer(From1,_To,_Amount1),
    transfer(From2,_To2,_Amount2),
    From1\==From2.


/** <examples>
?- go(Timeline).
*/
