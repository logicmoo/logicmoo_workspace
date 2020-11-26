:- expects_dialect(lps).

/* Accord Project example:

 * "Late Delivery and Penalty. In case of delayed delivery
 * [{" except for Force Majeure cases,":? forceMajeure}] 
 * the Seller shall pay to the Buyer for every 
 * [{penaltyDuration}] of delay penalty amounting to 
 * [{penaltyPercentage}]% of the total value of the Equipment whose delivery has been delayed.
 *  Any fractional part of a [{fractionalPart}] is to be considered a full [{fractionalPart}]. 
 * The total amount of penalty shall not however, 
 * exceed [{capPercentage}]% of the total value of the Equipment involved in late delivery. 
 * If the delay is more than [{termination}], the Buyer is entitled to terminate this Contract."
*/

fluents		day/1, penalty/2, delivered/1, force_majeure/1, terminated/1.
events      end_of_day/1, deliver/1.

deliver(Order) initiates
delivered(Order).

% For simplicity, the penaltyDuration is assumed to be one day,
% and fractionalPart is assumed to be a fractional part of one day.

end_of_day(_) updates Old to New in penalty(Order, Old) if 
  latest_delivery(Order, Date1),
	not delivered(Order),
	day(Date2), Date2 @> Date1,
	days_difference(Date1,Delay,Date2),
	not force_majeure(_),
	not terminated(Order),
   total_value(Order, Value),
   penalty_percentage(Order, PenaltyPercent),
   percentage_cap(Order, CapPercent),
   New is PenaltyPercent*Value*Delay,
   Cap is CapPercent*Value,
   New =< Cap.

% If the delay is more than 10 weeks then
% the Buyer is entitled to terminate this Contract.
% Additional rules/clauses are needed to terminate the order if the buyer is entitled to do so
% and the buyer decides to exercise the option.
%
entitled(terminate(Person, Order)) at T if 
    buyer(Order, Person),
    latest_delivery(Order, Date1),
	not(delivered(Order)) at T,
	day(Date2) at T,
    Date2 @> Date1.
         
% Example late delivery.
%
seller(mydelivery, bob).
buyer(mydelivery, alex).
equipment(mydelivery,logicforproblemsolving).

latest_delivery(mydelivery, 2018/4/1).
total_value(mydelivery, 100). 
penalty_percentage(mydelivery, 0.20).
percentage_cap(mydelivery, 0.50).

initially penalty(mydelivery, 0).

observe deliver(mydelivery) from 11 to 12.


% Real time simulation 
% Each day takes two cycles.
% 

initially day(2018/3/31).

observe end_of_day(2018/3/31) from 2 to 3.
observe end_of_day(2018/4/1) from 4 to 5.
observe end_of_day(2018/4/2) from 6 to 7.
observe end_of_day(2018/4/3) from 8 to 9.
observe end_of_day(2018/4/4) from 10 to 11.
observe end_of_day(2018/4/5) from 12 to 13.
observe end_of_day(2018/4/6) from 14 to 15.

end_of_day(_) updates Date to NextDate in day(Date) if 
next_day(Date, NextDate).

next_day(2018/3/31, 2018/4/1).
next_day(Year/4/Day1, Year/4/Day2) :- Day1 < 31, Day2 is Day1 +1.

days_difference(Year/Month/Day1,Difference,Year/Month/Day2) :-
    	Difference is Day2 - Day1.



/** <examples>
?- go(Timeline).
?- go.
*/