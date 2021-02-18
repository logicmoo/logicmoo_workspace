
:- expects_dialect(lps).

% RK's SOS recognizer/generator
maxTime(40).
events switch, sos, flash.
actions switch, shout.
fluents lightOn, lightOff.

observe switch  from 2 to 3.
observe switch  from 4 to 5.
observe switch  from 5 to 6.
observe switch  from 7 to 8.
observe switch  from 8 to 9.
observe switch  from 10 to 11.

lightOff  if not lightOn.

switch initiates lightOn if 
	lightOff.
switch terminates lightOn if 
	lightOn.

if sos to T then 
	sos from T+3.

sos  from T1 to  T4 if 
	lightOff at T1, 
	flash from T1 to T2, 
	flash from T2 to T3, 
	flash from T3 to T4.

flash from T1 to T2 if 
	switch from T1  to T, 
	switch from T+1 to T2.

/*
if sos to T then sos from T+3.

sos  if lightOff, flash, flash, flash.
flash if switch to T, switch from T+1.
*/


/** <examples>
?- go(Timeline).
?- go.
?-  go(Timeline, [sample([lightOff])]). 
*/


