:- expects_dialect(lps).

simulatedRealTimeBeginning('2019-06-15'). % The date of the initial state at cycle 1.
simulatedRealTimePerCycle(T) :- T is 86400/3. 
maxTime(30). % 30 cycles = 30 days.

observe remove(me,items) at 2019/06/17.
/** <examples> 
?- go(Timeline).
*/
