
:- expects_dialect(lps).

% Technique to simulate an "outside world" event new_lustrum(_) every 5 cycles


new_lustrum(N) :- 
    current_time(T), 0 is T mod 5, N is T/5.

% Wire the above Prolog code, so it is "polled" at each LPS engine cycle:
prolog_events new_lustrum(_).

actions some_action.

if new_lustrum(D) then 
	some_action.

/** <examples> 
?- go(Timeline).
*/

