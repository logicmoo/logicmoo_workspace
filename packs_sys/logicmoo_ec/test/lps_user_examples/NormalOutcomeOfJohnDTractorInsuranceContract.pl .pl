:- expects_dialect(lps).

end_of_file.
% @TODO unbreak

% Rather then run live, we'll simulate real time by mapping its time points to simulation cycles:
:- include(system('date_utils.pl')).
simulatedRealTimeBeginning('2018-05-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*120. % 120 days max lifetime of the contract

:- include('SzaboezqueInsuranceContract.pl'). % We'll now instantiate this kind of contract.
% succeeds correctly, claim being paid
% observe safeArrival("10 John D. tractors") at "2018-06-21T15:00".
observe choiceOf(holder) at 2018/6/25.
observe to(holder,usd(120000)) at 2018/6/25.
% observe to(holder,foreclose("Some key to all Counterparty goods", usd(5000))) at 2018/8/2.

/** <examples>
?- go(Timeline).
*/