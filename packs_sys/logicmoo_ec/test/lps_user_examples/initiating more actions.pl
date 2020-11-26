:- expects_dialect(lps).

maxTime(5).

actions initiating(_).
fluents gotit(_).

initiating(X) initiates gotit(X).

if true then initiating(1), initiating(2), update Old to 13 in gotit(Old) from 3.


% probably not to be used in user programs, just shaking things a bit...
if initiating(2) then terminate gotit(1) from _.

% false initiating(Num1), gotit(Num2).

/** <examples>
?- go(T, [more_actions]).
?- go(T).
*/
