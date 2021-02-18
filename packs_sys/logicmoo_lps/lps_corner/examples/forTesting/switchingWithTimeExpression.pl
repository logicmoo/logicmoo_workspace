
:- expects_dialect(lps).

events switch.
actions switch.
fluents lightOn.

% if  lightOff then switch.

lightOff at _ if not lightOn.

if lightOff then switch from T2 to T2+1.



/** <examples> 
?- go(Timeline).
*/

