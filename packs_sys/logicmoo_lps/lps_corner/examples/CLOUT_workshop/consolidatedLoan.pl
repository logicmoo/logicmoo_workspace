
:- expects_dialect(lps).

% consolidated loan example.

:- include(system('date_utils.pl')).  

simulatedRealTimeBeginning('2014-05-31'). % The date of the initial state at cycle 1.
simulatedRealTimePerCycle(21600). % Each cycle = 21600 seconds = 1/4th of a day.
maxTime(3200). % 3200 cycles = 800 days.

events 		request/2, advance/2.
fluents 	requested/3, advanced/2, legal_action_against/1, terminated.

request(Borrower, Amount) initiates requested(Borrower, Amount, Date)
if real_date(Date).

advance(Lender, Amount) initiates advanced(Lender, Amount).

end_of_day(2014/6/1)
initiates terminated
if not requested(borrower, 1000, 2014/6/1).

end_of_day(2014/6/2)
initiates legal_action_against(lender)
if requested(borrower, 1000, 2014/6/1), 
not advanced(lender, 1000).

events 		pay/3.
fluents 	paid/4, potential_defaulted/1.

due(550, 2015/6/1). 
due(525, 2016/6/1).

end_of_day(Date) initiates
potential_defaulted(pay(borrower, lender, Amount))
if due(Amount, Date),
not paid(borrower, lender, Amount, Date).

false 	pay(borrower, lender, Amount), due(Amount, Date2), 
		real_date(Date1), Date1 @< Date2.

pay(Borrower, Lender, Amount) 	initiates paid(Borrower, Lender, Amount, Date)
								if 	real_date(Date).

events notify/2.
fluents defaulted/2, notified/3, remedied/1,
cured/1. % shouldn't be necessary.

notify(Person, Message) initiates notified(Person, Message, Date)
						if 	real_date(Date).

end_of_day(Date2)
initiates defaulted(Requirement, Date2) 
if 	potential_defaulted( Requirement),
	notified(lender, default(Requirement), Date1),
	real_date_add(Date1, 2, Date2),
	not defaulted(_, _), % In the event of multiple events of default,
	% the first to occur shall take precedence 
	% for the purposes of specifying outcomes under this agreement.
	not cured(Requirement). 

cured(Requirement) at T
if 	remedied(Requirement) at T, 
	notified(borrower, remedy(Requirement), _) at T.

pay(borrower, lender, Amount)
initiates 	remedied(pay(borrower, lender, Amount))
if 	potential_defaulted(pay(borrower, lender, Amount)).

events file/2.

file(borrower, bankruptcy) initiates 
potential_defaulted(bankruptcy_insolvency(borrower)).

file(borrower, insolvency) initiates 
potential_defaulted(bankruptcy_insolvency(borrower)).

events remedy/1.

remedy(Violation) initiates
remedied(Violation).

events prove_untrue/1.

prove_untrue(Requirement) initiates
potential_defaulted(Requirement) if represents_warrants(borrower, Requirement).

represents_warrants(borrower, 
                   `borrower's assets exceed liabilities at time T 
                    if borrower executes contract at time T
                    or borrower requests funds at time T
                    or due(Amount, Date) and T is on or before Date`).

events fails_to_perform/2.

fails_to_perform(borrower, Requirement) initiates
potential_defaulted(Requirement) if covenant(borrower, Requirement).

covenant(borrower,
         `pay borrower tax at time T 
          if tax due at time T
          and borrower executes contract at time T
              or borrower requests funds at time T
              or due(Amount, Date) and T is on or before Date`).

fluents due_payable/2.

total_due(1075) at T if not paid(borrower, lender, 550,_) at T.
total_due(525) at T if paid(borrower, lender, 550,_), not paid(borrower, lender, 525,_) at T. 

end_of_day(Date2)
initiates due_payable(Sum, Date2)
if 	defaulted(_, Date1), 
	real_date_add(Date1, 1, Date2), total_due(Sum).

% fluents due_payable/2, paid/4. % These declarations should not be necessary.
% conditions commented out. still not working.
% 
end_of_day(Date) initiates legal_action_against(borrower)
if due_payable(Sum, Date).
% Date @< 2020/06/01, % within NY State Statute of Limitation. 
% not paid(borrower, lender, Sum, Date).

pay(borrower, lender, Sum) initiates terminated
if due_payable(Sum, Date).

pay(borrower, lender, 525) initiates terminated
if paid(borrower, lender, 550,_).

% Alternative scenaria:
% 
% Everything according to plan.

observe request(borrower, 1000) at '2014-06-01T15:00'. % at 15:00.
observe advance(lender, 1000) at '2014-06-02T18:00'.
observe pay(borrower,lender, 550) at '2015-06-01T12:00'.
% observe pay(borrower,lender, 525) at '2016-06-01T06:00'.
observe notify(lender, default(pay(borrower, lender, 525))) at '2016-06-02'.

