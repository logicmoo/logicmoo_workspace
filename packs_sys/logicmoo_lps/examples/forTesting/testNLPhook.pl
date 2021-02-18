
:- expects_dialect(lps).

maxTime(4).

events test.

observe test from 1 to 2.

en("
    I'm a bit of English!
").
/** <examples> 
?- godc(Timeline).
?- dumplps.
*/
