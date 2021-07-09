
:- expects_dialect(lps).

maxTime(5).

simulatedRealTimeBeginning('2014-06-01'). 
simulatedRealTimePerCycle(86400). % 1 day in seconds

actions another_day(_).

if real_time(RT) at T 
then format_time(string(S),"%F, a fine %A",RT), another_day(S at T) from T. 


/** <examples> 
?- go(Timeline).
*/
