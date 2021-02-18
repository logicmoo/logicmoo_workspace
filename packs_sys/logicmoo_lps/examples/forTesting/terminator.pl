
:- expects_dialect(lps).

% Demo lps_terminate action, and Prolog system "output predicate" called as an action

if true then writeln('Goodbye!'(T)) from 5 to T, exit from T.

exit from T if lps_terminate(leaving) from T.

/** <examples> 
?- go(Timeline).
*/
