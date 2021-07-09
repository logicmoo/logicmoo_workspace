
:- expects_dialect(lps).

% This illustrates the use of assynchronous Ethereum calls
maxRealTime(5).

if true
then 
	e(coinbase(_),MID) to 2, e_available(MID) to T1, e_result(MID,A) from T1 to T2, 
	writeln(MID/T1/A), lps_terminate from T2.

/** <examples> 
?- go.
*/
