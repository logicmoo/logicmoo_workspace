
:- expects_dialect(lps).

maxTime(4).

events default_occurs(_).
actions legal_action_against(_).

valid_contract at T if maxTime(Max), between(1,Max,T).

%fluents total_due(_). initially total_due(100).

fluents paid(_,_,_).

total_due(1075) at T if not paid(borrower, lender, 550) at T.
total_due(525) at T if paid(borrower, lender, 550) at T, not paid(borrower, lender, 525) at T. 


observe default_occurs(boo) from 1 to 2.

due_payable(Sum) from T1 to T2 if 	
	total_due(Sum) at T1, default_occurs(_) from T1 to T2.  % this works
	% default_occurs(_) from T1 to T2, total_due(Sum) at T1.   % this fails, event consumed at T2...

if  due_payable(Sum) to T1, valid_contract at T1
then legal_action_against(borrower) from T2.

/** <examples> 
?- go(Timeline).
*/
