:- expects_dialect(lps).

maxTime(25).
fluents lightOn, lightOff.
events switch.
actions switch.

lightOff if not lightOn.

/*
observe switch  from 2 to 3.
observe switch  from 4 to 5.
observe switch  from 5 to 6.
observe switch  from 7 to 8.
observe switch  from 8 to 9.
observe switch  from 10 to 11.
*/

sos  from T1 to  T4  if lightOff at T1, makeOn from T1 to T2, 
makeOn from T2 to T3, makeOn from T3 to T4.

% makeOn from T1 to T4 if switch from T1  to T2, T3 is T2+1, switch from T3 to T4.

% This version doesn't generate sos with second rule.
% But works OK with first rule.
makeOn from T1 to T3 if switch from T1  to T2, switch from T2+1 to T3.

if true then sos.

% if sos to T then sos from T+3.

switch initiates 	lightOn 	if lightOff.
switch terminates 	lightOn 	if lightOn.

/** <examples>
?- go(T).
*/
