
:- expects_dialect(lps).

maxTime(5).
fluents gotit(_).
if true then initiate gotit(1), initiate gotit(2), update Old to 13 in gotit(Old) from 3.
events initiate(_).

% probably not to be used in user programs, just shaking things a bit...
if initiate gotit(2) then terminate gotit(1) from _.