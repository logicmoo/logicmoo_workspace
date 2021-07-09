
:- expects_dialect(lps).

/* Late Delivery and Penalty. In case of delayed delivery except for
 Force Majeure cases, the Seller shall pay to the Buyer for every 2 weeks of
 delay penalty amounting to 10.5% of total value of the Equipment whose
 delivery has been delayed. Any fractional part of a week is to be
 considered a full week. The total amount of penalty shall not,
 however, exceed 55% of the total value of the Equipment involved in late
 delivery.
 If the delay is more than 10 weeks, the Buyer is entitled to terminate
 this Contract.
*/


maxTime(10).
actions		buys(Buyer, Seller, Equipment), pay_now/3.
fluents		case_for_delivery(Buyer, Seller, Equipment, Date, ToBeDeliveredWithin),
            delayed_delivery_case(CaseID, DelayInWeeks),
			penalty(CaseID, Amount).
events      end_of_day/1.

total_value_of_equipment(equipment1, 100).

promised_delivery_time(equipment1, 1).

force_majeure(none).

observe buys(bob, alex, equipment1) from  1 to 2.
observe end_of_day(2018/3/20) from 3 to 4.
observe end_of_day(2018/4/20) from 5 to 6.
observe end_of_day(2018/5/12) from 6 to 7.
observe end_of_day(2018/6/1) from 7 to 8.

buys(Buyer, Seller, Equipment) initiates
case_for_delivery(Buyer, Seller, Equipment, (Y/M/D), ToBeDeliveredWithin) if
   promised_delivery_time(Equipment, Promise),
   date(date(Y, M, D)),
   ToBeDeliveredWithin is Promise*7. % The promise is in weeks

buys(Buyer, Seller, Equipment) initiates
   penalty(case_for_delivery(Buyer, Seller, Equipment, (Y/M/D), ToBeDeliveredWithin), 0) if
   promised_delivery_time(Equipment, Promise),
   date(date(Y, M, D)),
   ToBeDeliveredWithin is Promise*7. % The promise is in weeks

end_of_day(Date) initiates
   delayed_delivery_case(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), Delay) if
   case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin),
   n_days_from(ThatDate,Diff,Date),
   Diff > ToBeDeliveredWithin,
   Delay is (Diff - ToBeDeliveredWithin)/7. % Delay in weeks

end_of_day(Date) terminates
   delayed_delivery_case(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), PreviousDelay).
%if
%   case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin),
%   n_days_from(ThatDate,Diff,Date),
%   Diff > ToBeDeliveredWithin,
%   Delay is (Diff - ToBeDeliveredWithin)/7,
%   not(Delay = PreviousDelay). % Delay in weeks

% Compute a penalty for a case if it is a case of delayed delivery
% and the delay is a rounded up number of weeks
% and the penalty is the delay divided by 2 times
%     10.5% of the total value of the Equipment whose delivery has been delayed

end_of_day(Date) updates Old to New in
   penalty(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), Old) if
   delayed_delivery_case(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), Delay),
   total_value_of_equipment(Equipment, Value),
   New is round(Delay/2)*10.5*Value.

% Late Delivery and Penalty.

% If it is a case of delayed delivery and not a for Force Majeure case
% then the Seller shall pay to the Buyer a penalty for the case.

if delayed_delivery_case(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), _Delay) at T,
   not(force_majeure(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin))),
   penalty(case_for_delivery(Buyer, Seller, Equipment, ThatDate, ToBeDeliveredWithin), Penalty) at T
then pay_now(Seller, Buyer, Penalty) from T to _.


% The total amount of penalty shall not, however, exceed 55% of the total
% value of the Equipment involved in late delivery.

% If the delay is more than 10 weeks then
% the Buyer is entitled to terminate this Contract.


/** <examples>
?- go(Timeline).
*/
