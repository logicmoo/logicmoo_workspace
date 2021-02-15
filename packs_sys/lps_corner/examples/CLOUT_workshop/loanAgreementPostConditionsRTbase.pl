
:- expects_dialect(lps).

/* Adapted by RAK from the simplified loan agreement in Flood and Goodenough, March 2017.
Contract as automaton: the computational representation of financial agreements.
https://www.financialresearch.gov/working-papers/files/OFRwp-2015-04_Contract-as-Automaton-The-Computational-Representation-of-Financial-Agreements.pdf

This representation is similar in spirit to the Flood_Goodenough formalisation as a
collection of transitions of the form current-state -> event -> new-state, 
represented in LPS in the form: event initiates new-fluent if current-conditions.

Further modified by MC to use LPS real time features. 
Dates are represented textually or with Y/M/D terms, assuming time local to the server.
PRELIMINARY, missing:
- more testing
- an Ethereum variant for the loaner, including use of emails (clause 11)
*/

:- include(system('date_utils.pl')).  % get us the end_of_day(Date) event

% These 3 lines are for development only; to be commented on deployment, if this happens at contract start:
simulatedRealTimeBeginning('2014-05-31'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(3200). % Not enough time to reach statute of limitations. 
%  Therefore 2020/6/1 replaced by 2016/07/1) below. ???

maxRealTime(189216000). % 6 years to reach statute of limitations
minCycleTime(21600). % 4 cycles per calendar day

fluents requested/3, advanced/3, tax_due/2, exceed_assets_liabilities,
		covenant/1, represent_warrant/1, paid/4,notified/3, potential_defaulted/1, 
		cured/1, defaulted/2, remedied/1, due_payable/2,total_due/1.

events 	request/2, advance/2, pay/3, file/2, notify/2, start_exceed_assets_liabilities.

actions legal_action_against/1, terminate_correctly, cancel, potential_default/1. 
		
% Note event initiates fluent is a convenient way to remember that an event has happened.
% Note that the Date of the event can be remembered in the fluent initiated by the event.

request(Person, Amount) 		initiates requested(Person, Amount, Date)
								if 	real_date(Date).
advance(Person, Amount) 		initiates advanced(Person, Amount, Date)
								if 	real_date(Date).
pay(Borrower, Lender, Amount) 	initiates paid(Borrower, Lender, Amount, Date)
								if 	real_date(Date).
notify(Person, Requirement) 	initiates notified(Person, Requirement, Date) 
								if 	real_date(Date).
% The above could be replaced by
%	happens(E,T,_) initiates happened(E,Date) if real_date(Date).
% ...and revising references below to requested(...) etc. to happened(request(Person,Amount),Date) etc.

pay(borrower, uncle_sam, Amount) terminates tax_due(uncle_sam, Amount).
start_exceed_assets_liabilities initiates remedied(represent_warrant( exceed_assets_liabilities))
if	 potential_defaulted(represent_warrant( exceed_assets_liabilities)).

% Syntax error if "from T" is eliminated from the following three clauses.
terminate_correctly	from T		if lps_terminate(terminate_correctly) from T. 
cancel from T					if lps_terminate(cancel) from T.
legal_action_against(X) from T 	if lps_terminate(legal_action_against(X)) from T.

% To explore alternative scenarios, add observations, or include this file from your file
%  Notice that not every scenario makes sense.
% For example, repaying a loan that has not been advanced makes no sense.
% These could be rejected by adding preconditions/constraints. 
% Or could be compensated by adding reactive rules, e.g. returning payments.
% 
% Try, for example, borrower doesn't pay anything back, 
% but lender does not notify default.i.e.
/*
initially exceed_assets_liabilities.
observe request(borrower, 1000) at '2014-06-01T10:00'. % request on time at 10:00.
observe advance(lender, 1000) at '2014-06-01T15:00'. % advance on time at 15:00.
*/

%%%%%%%%%%%%%%%%   This is where the contract begins.
%
% The following rules and clauses are written approximately in the same order 
% in which they occur in the English contract.
% The next two rules could be written as event-initiates-fluent clauses.
% 
if 	end_of_day(2014/6/1), 
	not requested(borrower, 1000, 2014/6/1) 	
then terminate_correctly.

if 	end_of_day(2014/6/2),
	requested(borrower, 1000, 2014/6/1), 
	not advanced(lender, 1000, _)
then legal_action_against(lender). % not in contract, but in DFA.

due(550, 2015/6/1). % $550 should be paid by the end of the day. 
due(525, 2016/6/1). 

potential_default(pay(borrower, lender, Amount))
if 	end_of_day(Date), due(Amount, Date), 
	% pre-payments made on earlier dates shall not be accepted.
	not paid(borrower, lender, Amount, Date).


potential_default(represent_warrant( exceed_assets_liabilities))
if 	end_of_day(Date), due(_, Date), % One interpretation of 
	% "any repayment amount shall be outstanding"
	not exceed_assets_liabilities.

% The English formulation of the contract additionally requires 
% assets exceed liabilities  on the execution of the contract,
% which is the same day as the borrower requests the loan.
% 
potential_default(represent_warrant( exceed_assets_liabilities))
if 	request(borrower, 1000),
 	not exceed_assets_liabilities.

potential_default(covenant(pay(borrower, Authority, Amount)))
if 	end_of_day(Date), due(_, Date), % One interpretation of 
	% "any repayment amount shall be outstanding"
	tax_due(Authority, Amount), 
	not paid(borrower, Authority, Amount, Date).

% The English formulation of the contract additionally requires 
% the borrower to pay any due tax  on the execution of the contract,
% which is the same day as the borrower requests the loan.
% 
potential_default(covenant(pay(borrower, Authority, Amount)))
if 	request(borrower, 1000),
	tax_due(Authority, Amount), 
	not paid(borrower, Authority, Amount, Date).

potential_default(bankruptcy_insolvency(borrower))
if 	file(borrower, bankruptcy).

potential_default(bankruptcy_insolvency(borrower))
if 	file(borrower, insolvency).
                    
potential_default(Requirement) 
initiates potential_defaulted( Requirement).

end_of_day(Date2)
initiates defaulted(Requirement, Date2) 
if 	potential_defaulted( Requirement),
	notified(lender, default(Requirement), Date1), 
	real_date_add(Date1, 2, Date2),
	not defaulted(_, _), % In the event of multiple events of default,
	% the first to occur shall take precedence 
	% for the purposes of specifying outcomes under this agreement.
	not cured(Requirement). 

cured(Requirement)
if 	remedied(Requirement), 
	notified(borrower, remedy(Requirement), _).

% Remedies for all potential defaults, 
% except for filing for bankruptcy or insolvency.

pay(borrower, lender, Amount)
initiates 	remedied(pay(borrower, lender, Amount))
if 	potential_defaulted(pay(borrower, lender, Amount)).

pay(borrower, Authority, Amount)
initiates remedied(covenant(pay(borrower, Authority, Amount)))
if 	potential_defaulted(covenant(pay(borrower, Authority, Amount))).

end_of_day(Date2)
initiates due_payable(Sum, Date2)
if 	defaulted(_, Date1), 
	real_date_add(Date1, 1, Date2), total_due(Sum).

total_due(1075) if not paid(borrower, lender, 550,_).
total_due(525) 	if paid(borrower, lender, 550,_), not paid(borrower, lender, 525,_). 
% The following clause is not be helpful, 
% because pay(0) is not the same as not paying.
% Legal action can be taken if the borrower does not pay 0.
% total_due(0) if paid(borrower, lender, 550), paid(borrower, lender, 525). 

% Alternatively, replace the following four rules by event-initiates-fluent clause.
if  end_of_day(Date), due_payable(Sum, Date),	
	paid(borrower, lender, Sum, Date)
then terminate_correctly. 

if  due_payable(Sum, Date), 
	Date @< 2016/07/1, % within NY's Statute of Limitation. should be 2020/6/1.
	not paid(borrower, lender, Sum, Date)
then legal_action_against(owes(borrower, Sum)).

if   end_of_day(2016/07/1)  %should be 2020/6/1.
then  lps_terminate(statute_of_limitations). % more informative than cancel.

% The following clause is not explicit in the contract. 
% It is not obvious whether there should be extra conditions.
% not potentially_defaulted(Requirement)?
% not defaulted(Requirement)?
if  pay(borrower, lender, 525), 
	paid(borrower, lender, 550,_)
then terminate_correctly.

/** <examples>
?- go.
?- go(Timeline).
*/
