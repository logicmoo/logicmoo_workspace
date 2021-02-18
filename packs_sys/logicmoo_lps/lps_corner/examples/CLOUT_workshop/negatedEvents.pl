
:- expects_dialect(lps).

% Masochistic agent
maxTime(10).
events shot.
actions ouch, i_bet_you_cant_shoot_me.

observe shot from 2.

if shot then 
	ouch.
if not shot then 
	i_bet_you_cant_shoot_me.

if true then
	not shot from 5,
	writeln(inGoal) from _.


/** <examples> 
?- go(Timeline).
*/


