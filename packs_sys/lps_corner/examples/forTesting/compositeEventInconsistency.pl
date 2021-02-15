
:- expects_dialect(lps).

/* Adapted by RAK from the simplified loan agreement in Flood and Goodenough, 2015. 
Contract as automaton: the computational representation of financial agreements.
https://pdfs.semanticscholar.org/b7ad/4e46b58094de72d9e82510ba38cb7e0521e4.pdf

This representation is similar in spirit to the Flood_Goodenough formalisation as a
collection of current state -> event -> new state transitions, represented in LPS by clauses
of the form: event initiates new fluent if current fluents.

Further modified by MC to use LPS real time features. 
Dates are represented textually or with Y/M/D terms, assuming time local to the server.
PRELIMINARY, missing:
- more testing
- an Ethereum variant for the loaner, including use of emails (clause 11)
*/

%%% LPS "system library":
real_date(Y/M/D) at T if 
	real_time(Now) at T, stamp_date_time(Now,date(Y,M,D,_,_,_,_,_,_),local).

real_date_begin(Date) from Last to First if  % last cycle of eve, first cycle of date
	real_date(Previous) at Last,
	First is Last+1,
	real_date(Date) at First,
	Date \= Previous.

real_date_end(Date) from Last to First if  % last cycle of Date, first cycle of next
	real_date(Date) at Last,
	First is Last+1,
	real_date(Next) at First,
	Date \= Next.

% We don't need both end_of_day and real_date_end.OK for now.
end_of_day(Date) from T1 to T2 if real_date_end(Date) from T1 to T2.

real_date_add(Y/M/D,Days,NY/NM/ND) :- 
    date_time_stamp(date(Y,M,D),T), NewT is T + (24*3600*Days), 
    stamp_date_time(NewT,date(NY,NM,ND,_,_,_,_,_,_),local).
%%%

% maxRealTime(10).
maxTime(3200). % Not enough time to reach statute of limitations. 
%  2020/6/1 replaced by 2016/07/1) below.
simulatedRealTimeBeginning('2014-05-31'). 
simulatedRealTimePerCycle(21600). % 4 cycles per calendar day

fluents 	requested/2, advanced/2, tax_due/2, exceed_assets_liabilities,
covenant/1, represent_warrant/1, paid/3,
notified/3, potential_defaulted/1, cured/1, defaulted/2, remedied/1, due_payable/2,
total_due/1.

events 	request/2, advance/2, pay/3, file/2, notify/2.

actions 	legal_action_against/1, terminate_correctly, cancel, potential_default/1.

% Note event initiates fluent is a convenient way to remember that an event has happened.
% Note that the Date of the event notify is remembered in the fluent notified.

request(Person, Amount) 		initiates requested(Person, Amount).
advance(Person, Amount) 		initiates advanced(Person, Amount).
pay(Borrower, Lender, Amount) 	initiates paid(Borrower, Lender, Amount).
notify(Person, Requirement) 	initiates notified(Person, Requirement, Date) 
								if 	real_date(Date).

terminate_correctly from T	if lps_terminate(terminate_correctly) from T. 
cancel from T				if lps_terminate(cancel) from T.
legal_action_against(X) from T if lps_terminate(legal_action_against(X)) from T.

% To explore alternative scenaria, comment and uncomment observations.
%  Notice that not every scenario makes sense.
% For example, repaying a loan that has not been advanced makes no sense.
% These can be rejected by adding preconditions/constraints. 
% Or can be compensated by adding reactive rules, e.g. returning payments.
% 
% Try, for example, borrower doesn't pay anything back, but lender does not notify default.

initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time
% observe pay(borrower, lender, 550) at '2015-05-31'. % pay on time.why not 2015-06-01?
observe notify(lender, default(pay(borrower, lender, 550))) at '2015-06-02'.
% observe pay(borrower, lender, 550) at '2015-06-03'. % must pay and notify.
% observe notify(borrower, remedy(pay(borrower, lender, 550))) at '2015-06-03T15:00'.
% observe pay(borrower, lender, 525) at '2016-06-01'. % pay on time.
% observe notify(lender, default(pay(borrower, lender, 525))) at '2016-06-04T10:00'.
% observe pay(borrower, lender, 525) at '2016-06-05'.% borrower avoids default by paying before default.
% observe notify(borrower, remedy(pay(borrower, lender, 525))) at '2016-06-06'.
% 
% Notice that if borrower defaults, by paying but not notifying the payment,
% the payment of 0 does not become due, and the contract terminates "correctly".
% observe pay(borrower, lender, 525) at '2016-06-07'.% borrower pays one day after default.

% The next two rules could be initiates-postcondition clauses.
if 	end_of_day(2014/6/1) to T, 
	not requested(borrower, 1000) at T 	
then terminate_correctly from T.

if 	end_of_day(2014/6/2) to T,
	requested(borrower, 1000) at T, not advanced(lender, 1000) at T
then legal_action_against(lender) from T.

due(550, 2015/6/1). % $550 should be paid by the end of the day. 
due(525, 2016/6/1). 

potential_default(pay(borrower, lender, Amount))
if end_of_day(Date), due(Amount, Date), not paid(borrower, lender, Amount).

potential_default(represent_warrant( exceed_assets_liabilities))
if end_of_day(Date), due(_, Date), 
not exceed_assets_liabilities.

potential_default(represent_warrant( exceed_assets_liabilities))
if request(borrower, 1000),
 not exceed_assets_liabilities.

potential_default(covenant(pay(borrower, Authority, Amount)))
if end_of_day(Date), due(_, Date), 
tax_due(Authority, Amount), not paid(borrower, Authority, Amount).

potential_default(covenant(pay(borrower, Authority, Amount)))
if request(borrower, 1000),
tax_due(Authority, Amount), not paid(borrower, Authority, Amount).

potential_default(bankruptcy_insolvency(borrower))
if file(borrower, bankruptcy).

potential_default(bankruptcy_insolvency(borrower))
if file(borrower, insolvency).
                    
potential_default(Requirement) 
initiates potential_defaulted( Requirement).

end_of_day(Date2)
initiates defaulted(Requirement, Date2) 
if potential_defaulted( Requirement),
	notified(lender, default(Requirement), Date1), real_date_add(Date1, 2, Date2),
	not defaulted(_, _),	not cured(Requirement). 

cured(Requirement) at T
if remedied(Requirement) at T, notified(borrower, remedy(Requirement), _) at T.

% Proposed remedies for all potential defaults, 
% except for filling for bankruptcy or insolvency.

pay(borrower, lender, Amount)
initiates remedied(pay(borrower, lender, Amount))
if potential_defaulted(pay(borrower, lender, Amount)).

events start_exceed_assets_liabilities.

start_exceed_assets_liabilities
initiates remedied(represent_warrant( exceed_assets_liabilities))
if potential_default(represent_warrant( exceed_assets_liabilities)).

pay(borrower, Authority, Amount)
initiates remedied(covenant(pay(borrower, Authority, Amount)))
if potential_default(covenant(pay(borrower, Authority, Amount))).

end_of_day(Date2)
initiates due_payable(Sum, Date2)
if defaulted(pay(borrower, lender, Payment), Date1), 
real_date_add(Date1, 1, Date2), total_due(Sum).

total_due(1075) at T if not paid(borrower, lender, 550) at T.
total_due(525) at T if paid(borrower, lender, 550) at T, not paid(borrower, lender, 525) at T. 
% The following clause is not be helpful, 
% because pay(0) is not the same as not paying.
% Legal action can be taken if the borrower does not pay 0.
% total_due(0) at T if paid(borrower, lender, 550) at T, paid(borrower, lender, 525) at T. 

% Alternatively, replace the following four rules by initiates postcondition fluent clause.
if  end_of_day(Date) to T, due_payable(Sum, Date) at T,	
paid(borrower, lender, Sum) at T 
then terminate_correctly from T. 

if  due_payable(Sum, Date) at T, 
	Date @< 2016/07/1, % within NY's Statute of Limitation. should be 2020/6/1.
	not paid(borrower, lender, Sum) at T
then legal_action_against(owes(borrower, Sum)) from T.

if   end_of_day(2016/07/1) to  T  %should be 2020/6/1.
then  lps_terminate(statute_of_limitations) from T. % more informative than:
% cancel from T.

% The following clause is not explicit in the contract. 
% 
if  pay(borrower, lender, 525) to T, 
	paid(borrower, lender, 550) at T
then terminate_correctly from T.

/** <examples>
?- go.
*/


