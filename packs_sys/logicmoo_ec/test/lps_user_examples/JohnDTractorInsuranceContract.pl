:- expects_dialect(lps).

end_of_file.
% @TODO unbreak
ct(lps).

% Rather then run live, we'll simulate real time by mapping its time points to simulation cycles:
:- include(system('date_utils.pl')).
simulatedRealTimeBeginning('2018-05-01'). 
simulatedRealTimePerCycle(RTPC) :- RTPC is  3600*12. % just 2 LPS cycles per calendar day
maxRealTime(M) :- M is 24*3600*120. % 120 days max lifetime of the contract

:- include('SzaboezqueInsuranceContract.pl'). % We'll now instantiate this kind of contract.

insuranceContract( usd(600), usd(120000), usd(5000), 2018/5/27 - 2018/7/31, "10 John D. tractors", 2018/8/10 ).
