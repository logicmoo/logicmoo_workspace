:- expects_dialect(lps).

maxTime(20).

fluents     alert1,alert2,alert3.
actions     letter.
events      termMessage1, termMessage2,termMessage3,termMessage4.

observe alert1 from 1 to 2.

if       alert1  at T1
then     termMessage1 from T1 to T2.

observe alert2 from 3 to 4.

if       alert2 at T3
then     termMessage2 from T3 to T4.

observe alert2 from 5 to 6.

if       alert3 at T5
then     termMessage3 from T5 to T6.

observe letter from 3 to 5.

termMessage4 from T3 to T5
if 		letter from T3 to T5.

