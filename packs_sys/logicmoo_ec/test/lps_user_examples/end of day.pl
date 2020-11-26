:- expects_dialect(lps).

end_of_file.
% @TODO unbreak
:- include(system('date_utils.pl')).  

simulatedRealTimeBeginning('2014-05-31'). % The date of the initial state at cycle 1.
simulatedRealTimePerCycle(21600). % Each cycle = 21600 seconds = 1/4th of a day.
maxTime(3200). % 3200 cycles = 800 days.

fluents legal_action_against/1, due_payable/2, paid/4.

initially due_payable(1000, 2015/06/10).

end_of_day(Date)
initiates legal_action_against(borrower)
if due_payable(Sum, Date) ,
Date @< 2020/06/01, % within NY State Statute of Limitation. 
not paid(borrower, lender, Sum, Date).

/*
actions legal_action/1.

% This doesn't work as intended without explicit times.
% Works with explicit time.
if end_of_day(Date) to T,
due_payable(Sum, Date) at T,
Date @< 2020/06/01, % within NY State Statute of Limitation. 
not paid(borrower, lender, Sum, Date) at T
then legal_action(lender) from T.
*/