:- expects_dialect(lps).

end_of_file.
% @TODO unbreak

:- include(system('date_utils.pl')). 
simulatedRealTimeBeginning('2020-03-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
minCycleTime(43200). % 2 cycles per calendar day



observe meets(gertrude, alice) at '2020-03-02'.
observe test(alice, positive) at '2020-03-05'.



/** <examples> 
?- go(Timeline).
*/
