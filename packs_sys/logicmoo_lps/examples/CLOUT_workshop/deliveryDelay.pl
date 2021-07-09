
:- expects_dialect(lps).

/* Example from Accord Project:
   https://docs.google.com/document/d/1UacA_r2KGcBA2D4voDgGE8jqid-Uh4Dt09AE-shBKR0/edit#
   https://www.accordproject.org/

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

% System predicates defining simulation of real time:
:- include(system('date_utils.pl')). 


simulatedRealTimeBeginning('2018-04-01'). 
simulatedRealTimePerCycle(21600). % Each cycle = 21600 seconds = 1/4th of a day.
maxTime(32). % 32 cycles = 8 days.

fluents		day/1, penalty/2, delivered/1, force_majeure/1, terminated/1.
events   	deliver/1.

deliver(Order) initiates delivered(Order).

% For simplicity, the penaltyDuration is assumed to be one day,
% and fractionalPart is assumed to be a fractional part of one day.
% 
initially penalty(mydelivery, 0.0).

% Example late delivery.
%
seller(mydelivery, bob).
buyer(mydelivery, alex).
equipment(mydelivery,logicforproblemsolving).

latest_delivery(mydelivery, 2018/4/1). 
total_value(mydelivery, 100). 
penalty_percentage(mydelivery, 0.20).
percentage_cap(mydelivery, 0.50).

observe deliver(mydelivery) at'2018-04-04T15:00'. % at 15:00.
% you can change the date, and see what happens.

end_of_day(Date2) updates Old to New in penalty(Order, Old) if 
  latest_delivery(Order, Date1),
	not delivered(Order),
	real_date_add(Date1,Delay,Date2),
	not force_majeure(_),
	not terminated(Order),
   total_value(Order, Value),
   penalty_percentage(Order, PenaltyPercent),
   percentage_cap(Order, CapPercent),
   New is PenaltyPercent*Value*(Delay+1),
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
	Date1 @=< Date2.
         

/** <examples>
?- go(Timeline).
*/
