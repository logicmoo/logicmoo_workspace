
:- expects_dialect(lps).

% "Never-ending composite event" hack. 
% However we can NOT use this in preconditions... as these bind time to the current cycle transition
maxTime(10).
 
events play(_Player, _Number), played(_,_).
actions boo(_,_).

observe play(miguel,13) from 1.
observe play(miguel,14) from 2.

played(P,N) from T1 to T2 if play(P,N) from T1 to T, true at T2.

if played(P,N) from T1 to T2 then boo(T1,T2) from T2.

% can't do this...played will always fail:
% false play(_,_) from T1 to T, played(_,_) from T2 to T, T1\==T2. 

/** <examples> 
?- go(Timeline).
*/
