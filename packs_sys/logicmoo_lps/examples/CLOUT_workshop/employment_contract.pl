
:- expects_dialect(lps).

% Originally at http://demo.logicalcontracts.com/example/employment_contract.pl
% Employment contract, for one employee
% It owns a (possibly) blockchain Ethereum account exclusive for this employee

%%% Employee parameters; may be included from employee specific file:
manager('manager@logicalcontracts.com'). % who at Employer to ask for more money for this particular contract
employer_email('WorkingBee@logicalcontracts.com'). % for notifications
employee_account('0x07388d8a07b5b9886c56ba673345fc62943c2321'). % for salaries and reimbursements
commencement('2017-12-01'). % this timeless fact should probably be initialized with the contract start
pay_day(28). % day in each month
probation(90). % better days then months
initially remuneration(20). % Ether
social_security_account(Address). % for employer payments

:- include(system('date_utils.pl')).

%%% Generic contract

fluents commencement(_date).
minCycleTime(43200). % 2 cycles per calendar day

% These two lines are for development only; to be commented on deployment:
simulatedRealTimeBeginning(Date) :- commencement(Date).
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC).

% TBD: clauses for termination in probation period, or later

fluents remuneration/1, social_security_rate/1.

initially social_security_rate(0.17). % For employer part

ss_contribution(C) at T if   % contribution by employer only
	remuneration(Salary), social_security_rate(Rate), C is Salary*Rate.

actions pay(_Recipient,_Ammount). % This initiates paid/3; reactive rules or human initiates confirmed_payment.

pay(E,Ammount) initiates paid(E,Ammount,Date) if real_date(Date), not paid(E,_,Date).

fluents bad_employee(_Account). % has somehow violated the contract

false pay(R,A), A>0, bad_employee(R).

fluents paid(_Recipient,_Ammount,_Date). % Address, Eth, Y/M/D
fluents confirmed_payment(_Recipient,_Ammount,_TransactionReceipt,_Date).

% TBD: clauses  for automated salary payments

% too important for smart contract? :
events company_is_satisfied, extend_probation, employer_terminates, employee_terminates, taxes_evaded.

events leave_request(_Type,_Begin,_Days). % _Type is anual/medical/compassionate
actions authorize_leave(_Type,_Begin,_Days).

% TBD: clauses for leave management, cf. pseudo English draft

events request_reimbursement(_ID,_Ammount,_Evidence), approve_reimbursement(_ID,_ApprovedAmmount).
fluents reimbursement(_ID,_ApprovedAmmount).

% if an employee requests the benefit of annual leave for a date at a time
% and the date is two week or more from the time
% then inform his/her manager with all the details that approval is required.

if requests_annual_leave(EmployeeID, Starting_Date) at Time,
   n_weeks_from(Time, 2, TwoWeeksLater),
   Starting_Date > TwoWeeksLater
then inform_manager_proposal(Employee_ID, Starting_Date) from Time.

inform_manager_proposal(Employee_ID, Starting_Date) from T1 to T2 if
   compute_actual_possible_leave_for(Employee_ID, Starting_Date, Finishing_Date, Total_Days) at T1,
   atomic_list_concat(['Hey Chief! ','This guy ',Employee_ID,' wants a vacation.'],Salutation),
	 atomic_list_concat(['It could start, as requested, at ', Starting_Date, '.'],FirstLine),
	 atomic_list_concat(['It could finish, the latest, at ', Finishing_Date, '.'],SecondLine),
	 atomic_list_concat(['For a total of ', Total_Days, ' days off.'],ThirdLine),
	 atomic_list_concat(['PS: Calculations made on ', T1],PS),
   atomic_list_concat([Salutation, FirstLine, SecondLine, ThirdLine, 'Please, confirm', PS],'\n', Message),
   lps_send_email('jd@logicalcontracts.com','Annual Leave for Employee',Message) from T to T2.

% One computes the actual possible leave for an employee at a time if
%   the employee has some balance carried from the previous employment year at the time
% and Remaining Annual Leave for an Employee in the current year of employment
%     at the time is a given number
% and the actual possible leave for the employee is the given number plus the balance.

compute_actual_possible_leave_for(Employee_ID, Starting_Date, Finishing_Date, Total_Days) at T if
   annual_leave_balance_for(Employee_ID, Days_Left) at T,
	 annual_leave_carried_from_previous_employment_year(Employee_ID, Previous) at T,
   Total_Days is Days_Left + Previous,
	 n_days_from(Starting_Date, Total_Days, Finishing_Date ).

% if an employee requests the benefit of annual leave at a date
% and his/her manager approval has been submitted at a second date
% then compute and inform that actual possible leave for this employee immediately.

if requests_annual_leave(EmployeeID, Starting_Date) at Time,
   n_weeks_from(Time, 2, TwoWeeksLater),
   Starting_Date > TwoWeeksLater,
	 approved_possible_leave_for(Employee_ID, Starting_Date, Finishing_Date, Total_Days) at T2
then inform_employee_leave_approved(Employee_ID, Starting_Date, Finishing_Date, Total_Days) from T2.

inform_employee_leave_approved(Employee_ID, Starting_Date, Finishing_Date, Total_Days) from T1 to T2 if
   % it may be done as a recheck
   % compute_actual_possible_leave_for(Employee_ID, Starting_Date, Finishing_Date, Total_Days) at T1,
   atomic_list_concat(['Dear ',Employee_ID,'. I am pleased to inform you that your annual leave has been approved as follows:'],Salutation),
	 atomic_list_concat(['It could start, as requested, at ', Starting_Date, '.'],FirstLine),
	 atomic_list_concat(['It could finish, the latest, at ', Finishing_Date, '.'],SecondLine),
	 atomic_list_concat(['For a total of ', Total_Days, ' days off.'],ThirdLine),
	 % atomic_list_concat(['PS: Calculations made on ', T1],PS),
   atomic_list_concat([Salutation, FirstLine, SecondLine, ThirdLine, 'Please, confirm your are indeed leaving'],'\n', Message),
   lps_send_email('jd@logicalcontracts.com','Approval for Annual Leave',Message) from T to T2.

% An employee returning from annual leave updates the Remaining Annual Leave for an Employee
%    in the current year if
%    the employee current annual leave started at a first date and finish at a second date
% and the new Remaining Annual Leave for an Employee in the current year of employment is
%   the old Remaining Annual Leave for an Employee in the current year of employment
%   plus any balance carried from the previous employment year
%   minus the difference between the first date and the second date.

returning_from_annual_leave(Employee_ID, Started_at) from Today updates Old to New in
   annual_leave_balance_for(Employee_ID, Old) if
	 difference_in_days_between(Started_at, Today, Difference),
	 annual_leave_carried_from_previous_employment_year(Employee_ID, Previous) at Started_at,
	 New is Old + Previous - Difference.

% if an employee returns from annual leave at a date
% and the employee current annual leave started at a first date and finish at the date
% and the difference between the first date and the date is greater than
%         the actual possible leave for the employee at the date
% then submit a misconduct record for the employee due to excessive annual leave time immediately after.

if returning_from_annual_leave(Employee_ID, Started_at) from Today to T2,
   compute_actual_possible_leave_for(Employee_ID, Started_at, _Finishing_Date, Total_Days) at Started_at,
	 difference_in_days_between(Started_at, Today, Difference),
	 Difference > Total_Days
then submit_misconduct_report_on(Employee_ID, 'Excessive Annual Leave', Started_at, Today, Total_Days, Difference) from T2 to T3.

submit_misconduct_report_on(Employee_ID, Reason, Starting_Date, Finishing_Date, Total_Days, Discrepancy) from T1 to T2 if
   atomic_list_concat(['Dear Chief of ',Employee_ID,'. You should know that this employee is being reported for: ', Reason],Salutation),
	 atomic_list_concat(['It started at ', Starting_Date, '.'],FirstLine),
	 atomic_list_concat(['It finished at ', Finishing_Date, '.'],SecondLine),
	 atomic_list_concat(['It must have been:  ', Total_Days, ' days off.'],ThirdLine),
	 atomic_list_concat(['But it has been: ', Discrepancy],Fourthline),
   atomic_list_concat([Salutation, FirstLine, SecondLine, ThirdLine, Fourthline],'\n', Message),
   lps_send_email('jd@logicalcontracts.com', Reason,Message) from T to T2.

% if it is the end of an employee s year of employment at date
% and  the Remaining Annual Leave for an Employee in the current year of employment
%    at the date is a number of days greater than or equal to 0
% and the number is lower than the Annual Leave for an Employee at the date, whatever its value,
% then initiate the balance carried from the previous employment year to that number inmediately
% and initiate the Remaining Annual Leave for the Employee in the current year of employment to
%     the Annual Leave for an Employee to be effective inmediately.

if end_of_employment_year(Employee_ID) at T,
   annual_leave_balance_for(Employee_ID, Days_Left) at T,
	 Days_Left >= 0,
	 annual_leave_for_an(Employee_ID, Reference) at T,
	 Days_Left < Reference
then initiate annual_leave_carried_from_previous_employment_year(Employee_ID, Days_Left) from T to T2,
    initiate annual_leave_balance_for(Employee_ID, Reference) from T2 to T3.

% if it is the end of an employee s year of employment at date
% and the Remaining Annual Leave for an Employee in the current year of employment
%        at the date is a number of days greater than or equal to
%        the Annual Leave for an Employee at the date
%        (which means it has been accumulated from the previous employment year)
% and the new value is equal to the Remaining Annual Leave for an Employee
%      in the current year of employment minus the  Annual Leave for an Employee
% then initiate the balance carried from the previous employment year to the new value immediately
% and initiate the Remaining Annual Leave for the Employee in the current year of employment to
%         the Annual Leave for an Employee to be effective immediately.

if end_of_employment_year(Employee_ID) at T,
   annual_leave_balance_for(Employee_ID, Days_Left) at T,
	 annual_leave_for_an(Employee_ID, Reference) at T,
	 Days_Left >= Reference,
	 Diff is Days_Left - Reference
then initiate annual_leave_carried_from_previous_employment_year(Employee_ID, Diff) from T to T1,
    initiate annual_leave_balance_for(Employee_ID, Reference) from T1 to T2.

% TBD: clauses for business expense management

% In case we talk the employee into partially returning salaries:
funds_to_return(RetroSalary,Date,Ammount) at T if
	findall(NetP, (confirmed_payment(_,P,_,PD) at T, PD @=< Date, NetP is P-RetroSalary), Payments),
	sum_list(Payments,Ammount).

salary_in_lieu(Days,SalaryIL) at T if
	remuneration(R) at T, SalaryIL is R*Days/30. % assuming 30 days per month

n_days_from(Starting_Date, Total_Days, Finishing_Date ) :-
  nonvar(Starting_Date), nonvar(Total_Days),
  Starting_Date = Year-Month-Day,
  NewDay is Day+Total_Days,
  date_time_stamp(date(Year,Month,NewDay,0,0,0,0,-,-), Stamp),
  stamp_date_time(Stamp, D, 0),
  date_time_value(date, D,date(FYear,FMonth,FDay)),
  Finishing_Date = FYear-FMonth-FDay.

n_days_from(Starting_Date, Total_Days, Finishing_Date ) :-
  nonvar(Starting_Date), var(Total_Days), nonvar(Finishing_Date),
  Starting_Date = Year-Month-Day,
  date_time_stamp(date(Year,Month,Day,0,0,0,0,-,-), Stamp1),
  Finishing_Date = FYear-FMonth-FDay,
  date_time_stamp(date(FYear,FMonth,FDay,0,0,0,0,-,-), Stamp2),
  Diff is Stamp2 - Stamp1,
  Diff > 0,
  Total_Days is truncate(Diff/86400).
