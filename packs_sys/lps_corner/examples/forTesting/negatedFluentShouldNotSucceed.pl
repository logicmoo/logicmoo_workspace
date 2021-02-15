
:- expects_dialect(lps).

maxTime(5).
if request(borrower, 1000, 1/6/2014) to 2, not advanced(lender, 1000, 2/6/2014) at 3
then legal_action_against(lender) from T3.

fluents advanced/3.
events request/3, advance/3.
actions legal_action_against/1.

advance(Person, Amount, Date) initiates advanced(Person, Amount, Date).

observe request(borrower, 1000, 1/6/2014) from 1 to 2.
observe advance(lender, 1000, 2/6/2014) from 2 to 3.