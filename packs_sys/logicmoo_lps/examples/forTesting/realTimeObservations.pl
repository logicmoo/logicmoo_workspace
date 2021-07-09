
:- expects_dialect(lps).

maxRealTime(5).
% See datetime formats in  http://www.swi-prolog.org/pldoc/doc_for?object=parse_time/3
simulatedRealTimeBeginning('2014-05-31'). 
simulatedRealTimePerCycle(28800). % 8 hours

observe agreement_date_begin at '2014-06-01'. % 0:00 hours

observe advancement_limit1 at '2014-06-02T23:59:00'.
observe advancement_limit2 at '2014-06-03'. % 0:00 hours
observe payment1 at '2015-06-02'. % 0:00
observe payment2 at '2016-06-02'.

observe termination at '2016-06-30'.

events termination.

if termination to T then writeln(last_cycle-T), lps_terminate from T.
