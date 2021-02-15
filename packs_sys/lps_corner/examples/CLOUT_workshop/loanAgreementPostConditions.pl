
:- expects_dialect(lps).

/* Adapted by RAK from the simplified loan agreement in Flood and Goodenough, 2015. 
Contract as automaton: the computational representation of financial agreements.
https://www.financialresearch.gov/working-papers/files/OFRwp-2015-04_Contract-as-Automaton-The-Computational-Representation-of-Financial-Agreements.pdf

This representation is similar in spirit to the Flood_Goodenough formalisation as a
collection of current state -> event -> new state transitions, represented in LPS by clauses
of the form: event initiates new fluent if current current fluents.

Please note, occasionally some fluents might be displayed in the timeline above events.
*/

maxTime(22).

fluents 	valid_contract, requested/2, advanced/2,
covenant/1, represent_warrant/1,
paid/3, tax_due/1, true/1,
notified/3, potential_defaulted/1, cured/1, defaulted/2, remedied/1, due_payable/2,
total_due/1.

events 	end_of_day/1, request/2, advance/2, pay/3, 
violate/1, bankruptcy_insolvency/1, notify/3.

actions 	legal_action_against/1, terminate_correctly, cancel.

% Note event initiates fluent is a convenient way to remember that an event has happened,
% It may be possible to add the time of the event as a parameter of the fluent.

request(Person, Amount) 					initiates requested(Person, Amount).
advance(Person, Amount) 					initiates advanced(Person, Amount).
pay(Borrower, Lender, Amount) 				initiates paid(Borrower, Lender, Amount).
notify(Person, Requirement, Date) 			initiates notified(Person, Requirement, Date).

terminate_correctly 	terminates valid_contract.
cancel 					terminates valid_contract.
legal_action_against(_) terminates valid_contract.

initially 		valid_contract.

% true(exceed( assets, liabilities)).

% simulation of time for testing.
%
% temporary for testing.
add(Day1/Month/Year, N, Day2/Month/Year) :- Day2 is Day1 + N. 

% For simplicity, one cycle represents one day.
% end_of_day(D) from T to T+1 means day D coincides with T, T+1 is the next day.
% An event (such as a payment) happening on day D is represented as happening from T-1 to T. 
% So any fluent (such as the payment being paid) initiated by the event holds on day D.
% 
% To explore alternative scenaria, comment and uncomment observations.
%  Notice that not every scenario makes sense.
% For example, repaying a loan that has not been advanced makes no sense.
% These can be rejected by adding preconditions/constraints. 
% Or can be compensated by adding reactive rules, e.g. returning payments.
% 
% Try, for example, borrower doesn't pay anything back, but lender does not notify default.
observe end_of_day(1/6/2014) from 2 to 3. %  borrower needs to have requested $1000 at 2.
observe end_of_day(2/6/2014) from 3 to 4. % lender needs to have advanced $1000 at 3.
observe end_of_day(1/6/2015) from 4 to 5. % borrower needs to have paid $550 at 4.
observe end_of_day(2/6/2015) from 5 to 6. % lender might notify potential default to 5.
observe end_of_day(4/6/2015) from 8 to 9. % borrower might cure potential default to 8.
observe end_of_day(5/6/2015) from 9 to 10. % if borrower defaults, all payments due to 9.
observe end_of_day(1/6/2016) from 11 to 12. % borrower needs to have paid $525 at 11.
observe end_of_day(5/6/2016) from 14 to 15. % lender might notify potential default to 14.
observe end_of_day(7/6/2016) from 16 to 17. % borrower might cure potential default to 16.
observe end_of_day(8/6/2016) from 18 to 19. % if borrower defaults, all payments due to 18.
observe end_of_day(1/6/2020) from 20 to 21.  % stature of limitations.

observe request(borrower, 1000) from 1 to 2. % request on time.
observe advance(lender, 1000) from 2 to 3. % advance on time
% observe pay(borrower, lender, 550) from 3 to 4. % pay on time.
observe notify(lender, default(pay(borrower, lender, 550)), 2/6/2015) from 4 to 5.
observe pay(borrower, lender, 550) from 6 to 7. % must pay before giving notice.
observe notify(borrower, remedy(pay(borrower, lender, 550)), 4/6/2015) from 7 to 8.
% observe pay(borrower, lender, 525) from 10 to 11. % pay on time.
observe notify(lender, default(pay(borrower, lender, 525)), 5/6/2016) from 13 to 14.
observe pay(borrower, lender, 525) from 17 to 18. % borrower pays one day after default.
% Notice that if borrower defaults, by paying but not notifying the payment,
% the payment does not become due, and the contract terminates "correctly".

% The next two rules could be initiates-postcondition clauses.
if 	end_of_day(1/6/2014) to T, 
	not requested(borrower, 1000) at T 	
then terminate_correctly from T.

if 	end_of_day(2/6/2014) to T,
	requested(borrower, 1000) at T, not advanced(lender, 1000) at T
then legal_action_against(lender) from T.

due(550, 1/6/2015). % $550 should be paid by the end of the day. 
due(525, 1/6/2016). 

end_of_day(Date) 
initiates potential_defaulted(pay(borrower, lender, Amount))
if valid_contract, due(Amount, Date), not paid(borrower, lender, Amount).

% Note that in a more refined representation, 
% violate could be a macro-event defined by a metainterpreter:
% For example, violate(Requirement) from T1 to T2 
% if true(Requirement) at T1, not true(Requirement) at T2.
% true(P if Q) at T if true(P) at T, not true(Q) at T.
% true(P at T1) at T if T = T1, P at T.
% etc.

/*
initially represent_warrant( exceed( assets, liabilities) at T
					if due(Payment, Date), end_of_day(Date) at T).

initially represent_warrant( exceed( assets, liabilities) at T
					if  requested(borrower, 1000) at T).
                    
initially covenant(pay_authority(Amount) at T 
					if  tax_due(Amount) at T).
*/

represent_warrant('assets exceed liabilities 
                   if borrower requests funds or a payment is due').

covenant('pay tax when due 
          if borrower requests funds or a payment is due').

violate(Requirement) 
initiates potential_defaulted(Requirement)
if valid_contract, represent_warrant( Requirement ).

violate(Requirement) 
initiates potential_defaulted( Requirement)
if  valid_contract, covenant( Requirement ).

bankruptcy_insolvency(borrower)
initiates potential_defaulted( solvent_not_bankrupt(borrower))
if  valid_contract.

end_of_day(Date2)
initiates defaulted(Requirement, Date2) 
if potential_defaulted( Requirement),
	notified(lender, default(Requirement), Date1), add(Date1, 2, Date2),
	not defaulted(_, _),	not cured(Requirement). 

cured(Requirement) at T
if remedied(Requirement) at T, notified(borrower, remedy(Requirement), _).

/* Example of one case of remedy for the moment. 
More generally we could have:

remedy(Requirement) from T1 to T2
if potential_defaulted(Requirement) at T1,
true(Requirement) at T2.

remedy(Requirement) initiates remedied(Requirement).
*/

pay(borrower, lender, Amount)
initiates remedied(pay(borrower, lender, Amount))
if potential_defaulted(pay(borrower, lender, Amount)).

end_of_day(Date2)
initiates due_payable(Sum, Date2)
if defaulted(pay(borrower, lender, Payment), Date1),
add(Date1, 1, Date2), total_due(Sum).

total_due(1075) at T if not paid(borrower, lender, 550) at T.
total_due(525) at T if paid(borrower, lender, 550) at T, not paid(borrower, lender, 525) at T. 

% Alternatively, replace the following four rules by initiates postcondition fluent clauese.
if  end_of_day(Date) to T, due_payable(Sum, Date) at T,
	paid(borrower, lender, Sum) at T 
then terminate_correctly from T. 

if  due_payable(Sum, Date) at T, valid_contract at T,
	Date \= 1/6/2020, % better to compare Date < 1/6/2020.
	not paid(borrower, lender, Sum) at T
then legal_action_against(owes(borrower, Sum)) from T.

if   valid_contract at T,
	end_of_day(1/6/2020) to  T
then cancel from T.

if  pay(borrower, lender, 525) to T, 
 	valid_contract at T, 
	paid(borrower, lender, 550) at T
then terminate_correctly from T.

/** <examples>
?- go(Timeline).
*/
