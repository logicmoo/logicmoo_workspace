
:- expects_dialect(lps).

% bankTransfer

maxTime(30).
actions		transfer(From, To, Amount).
fluents		balance(Person, Amount).

initially	balance(bob, 0), balance(fariba, 100).
observe		transfer(fariba, bob, 10) 	from 1 to 2.

if		transfer(fariba, bob, X) 	from  T1 to T2  
		, balance(bob, A) at T2, A >= 10
then		transfer(bob, fariba, 10) 	from T2 to T3.

if		transfer(bob, fariba, X) 	from  T1 to T2
		, balance(fariba, A) at T2, A >= 20
then  		transfer(fariba, bob, 20) 	from  T2 to T3.

	transfer(From, To, Amount) 	initiates 	balance(To, New) 
if    	balance(To, Old),  New is Old + Amount.

	transfer(From, To, Amount) 	terminates	balance(To, Old).

	transfer(From, To, Amount) 	initiates 	balance(From, New) 
if    	balance(From, Old),  New is Old - Amount.

	transfer(From, To, Amount) 	terminates	balance(From, Old).

false	transfer(From, To, Amount), balance(From, Old),  Old < Amount.

false	transfer(From, To1, Amount1), 
	transfer(From, To2, Amount2),  To1 \=To2.

false	transfer(From1, To, Amount1), 
	transfer(From2, To, Amount2),  From1 \= From2.