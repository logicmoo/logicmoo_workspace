
:- expects_dialect(lps).

/* Adapted by RAK from the simplified loan agreement in Flood and Goodenough, 2015. 
Contract as automaton: the computational representation of financial agreements.
https://pdfs.semanticscholar.org/b7ad/4e46b58094de72d9e82510ba38cb7e0521e4.pdf

Bug in timeline display.
advanced, defaulted and cured fluents displayed above  events in some of these scenaria. 

*/

fluents 	valid_contract, requested/2, advanced/2, paid/3, tax_due/1, true/1,
potential_defaulted/1, defaulted/1, remedy/1, cured/1.

events 	request/2, advance/2, pay/3, bankruptcy_insolvency/1, notice/2.

actions 	legal_action_against/1, potential_default_occurs/1, 
cure/1, default_occurs/1, terminate_correctly, cancel.

% Note event initiates fluent is a convenient way to remember that an event has happened,
% The time can be added as a parameter.

request(Person, Amount) 					initiates requested(Person, Amount).
advance(Person, Amount) 					initiates advanced(Person, Amount).
pay(Borrower, Lender, Amount) 				initiates paid(Borrower, Lender, Amount).
potential_default_occurs(Requirement) 		initiates potential_defaulted(Requirement).
default_occurs(Requirement) 				initiates defaulted(Requirement).
cure(Default) 								initiates cured(Default).
cure(Default) 								terminates potential_defaulted(Default).
cure(Default) 								terminates defaulted(Default).

terminate_correctly 	terminates valid_contract.
cancel 					terminates valid_contract.

initially 		valid_contract, true(exceed( assests, liabilities)).

% Comment and uncomment to explore alternative scenaria. % Also change times.
% Notice that not every scenario makes sense.
% For example, repaying a loan that has not been advanced.
% These can be rejected by adding preconditions/constraints. 
% Or can be compensated by adding reactive rules, e.g. returning payments.

/* All goes according to plan:
observe request(borrower, 1000) from 1 to 2.
observe advance(lender, 1000) from 2 to 3.
observe pay(borrower, lender, 550) from 3 to 4. % pay on time.
observe pay(borrower, lender, 525) from 10 to 11. % pay on time.
*/
/*
% Alternative scenario: borrower does not pay on time, but cures potential default: 
observe request(borrower, 1000) from 1 to 2.
observe advance(lender, 1000) from 2 to 3.
observe notice(lender, default(pay(borrower, lender, 550))) from 4 to 5.
observe pay(borrower, lender, 550) from 5 to 6.
observe notice(borrower, remedy(pay(borrower, lender, 550))) from 6 to 7.
observe pay(borrower, lender, 550) from 3 to 4. % pay on time.
observe pay(borrower, lender, 525) from 10 to 11. % pay on time.
*/

% Alternative scenario: borrower does not pay on time and does not cure the default.
observe request(borrower, 1000) from 1 to 2.
observe advance(lender, 1000) from 2 to 3.
observe notice(lender, default(pay(borrower, lender, 550))) from 4 to 5.

/*
% Alternative scenario: borrower does not pay on time two times. 
% but cures first potential default, but not second one.
observe request(borrower, 1000) from 1 to 2.
observe advance(lender, 1000) from 2 to 3.
observe notice(lender, default(pay(borrower, lender, 550))) from 4 to 5.
observe pay(borrower, lender, 550) from 5 to 6.
observe notice(borrower, remedy(pay(borrower, lender, 550))) from 6 to 7.
observe notice(lender, default(pay(borrower, lender, 525))) from 13 to 14.
*/

% observe request(borrower, 1000) from 2 to 3. % too late.
% observe notice(lender, default(pay(borrower, lender, 525))) from 14 to 15.
% observe pay(borrower, lender, 525) from 16 to 17.
% observe notice(borrower, remedy(pay(borrower, lender, 525))) from 16 to 17.

due(550, 1/6/2015). % $550 should be paid by the end of the day. 
due(525, 1/6/2016). 

% simulation of time for testing.
%
real_date(1/6/2014, 2). % 2 is the end of the day. borrower needs to request $1000.
real_date(2/6/2014, 3). % lender needs to advance $1000.
real_date(1/6/2015, 4). % borrower needs to pay $550.
real_date(2/6/2015, 5). % lender might give notice of failure to pay.
real_date(4/6/2015, 8). % borrower needs to pay $550 and give notice, if potential default.
real_date(5/6/2015, 9). % if borrower defaulted, all payments due.
real_date(1/6/2016, 11). % borrower needs to pay $525.
real_date(5/6/2016, 14). % lender gives notice of failure to pay.
real_date(7/6/2016, 16). % borrower needs to pay $525 and give notice, if potential default.
real_date(8/6/2016, 17). % if borrower defaulted, all payments due.
real_date(1/6/2020, 19). % stature of limitations.

% temporary for testing.
add(Day1/Month/Year, N, Day2/Month/Year) :- Day2 is Day1 + N. 

% a negated request event is not currently allowed, 
% and would not be correct here.
if 	real_date(1/6/2014,T), 
	not requested(borrower, 1000) at T 	
then terminate_correctly from T.

if  real_date(1/6/2014,T1), 
	requested(borrower, 1000) at T1, 
	real_date(2/6/2014, T2), not advanced(lender, 1000) at T2
then legal_action_against(lender) from T2.

% Here is an initial attempt to disentangle the meaning of warrant, represent and convenant. 
% These would be unanalyzed in a propositional approach.

% A bit of a fudge. Should be represent_warrant(( exceed( assests, liabilities) if … ) at T ). 
% using a meta-interpreter such as rep_war(P at T) if rep_war((P if q) at T), rep_war(q at T).
%
represent_warrant( exceed( assests, liabilities) ) at T 
if  requested(borrower, 1000) at T. 

represent_warrant( exceed( assests, liabilities) ) at T 
if due(Payment, Date), real_date(Date,T).

covenant(pay_authority(Amount)) at T 
if  tax_due(Amount) at T.

if  potential_default_occurs( Requirement) to T1,
	notice(lender, default(Requirement)) to T2, T1=<T2,
	real_date(Date1,T2), add(Date1, 2, Date2), real_date(Date2, T3),
	not defaulted(_) at T3, % from end of section 5. 
	not cured(Requirement) at T3 
then default_occurs(Requirement) from T3.

if  valid_contract at T,
	due(Amount, Date), real_date(Date,T), 
	not paid(borrower, lender, Amount) at T
then potential_default_occurs( pay(borrower, lender, Amount)) from T.

if  valid_contract at T,
	represent_warrant( Requirement ) at T, not true(Requirement) at T
then potential_default_occurs( Requirement) from T.

if valid_contract at T,
	covenant( Requirement ) at T, not true(Requirement) at T
then potential_default_occurs( Requirement) from T.

if  valid_contract at T,
	bankruptcy_insolvency(borrower) to T
then potential_default_occurs( solvent(borrower)) from T.

% borrower gives notice that the potential default has been remedied.
if  potential_defaulted(Requirement) at T,
	notice(borrower, remedy(Requirement)) to T, remedy(Requirement) at T
then  cure(Requirement) from T. 

% Example of one case of remedy for the moment. 
% remedy applies only in the context of potential default.
remedy(pay(borrower, lender, Amount)) at T
if paid(borrower, lender, Amount) at T. 

due_payable(Sum) from T1 to T2 
if 	 default_occurs(_) from T1 to T2, total_due(Sum) at T2.

total_due(1075) at T if not paid(borrower, lender, 550) at T.
total_due(525) at T if paid(borrower, lender, 550) at T, not paid(borrower, lender, 525) at T. 

if  due_payable(Sum) to T1, valid_contract at T1,
	real_date(Date1, T1),
	add(Date1, 1, Date2), real_date(Date2, T2),
	paid(borrower, lender, Sum) at T2
then terminate_correctly from T2. 

if  due_payable(Sum) to T1,
	valid_contract at T1,
	real_date(Date1, T1),
	add(Date1, 1, Date2),
	real_date(Date2, T2),
	real_date(1/6/2020,TooLate), T2 < TooLate,
	not paid(borrower, lender, Sum) at T2
then legal_action_against(owes(borrower, Sum)) from T2.

if  due_payable(Sum) to T1, valid_contract at T1,
	real_date(Date1, T1),
	add(Date1, 1, Date2), real_date(Date2, T2),
	real_date(1/6/2020,TooLate), TooLate =< T2,
	not paid(borrower, lender, Sum) at T2
then cancel from T2.

if  pay(borrower, lender, 525) to T2, valid_contract at T1, 
	paid(borrower, lender, 550) at T1
then terminate_correctly from T2.


/** <examples>
?- go(Timeline).
*/
