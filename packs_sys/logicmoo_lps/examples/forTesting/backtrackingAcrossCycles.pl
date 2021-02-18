
:- expects_dialect(lps).

% From Sam Yong's MSc thesis report, page 78
% Backtracking across cycles... works although the implementation does not do that, instead 
% relies on "ethernally available fluent goals"...
maxTime(10).
actions a(_), b(_). 
events e.
fluents f(_).
initially f(1).

observe e from 2 to 3.

if true then f(X) at T1, a(X) from T1 to T2, T3 is T2+1, b(X) from T3 to T4.
false b(1) from _ to _. 
initiates(e, f(2)).

/** <examples> 
?- go(Timeline).
*/
