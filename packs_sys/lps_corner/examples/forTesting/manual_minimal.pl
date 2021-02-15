
:- expects_dialect(lps).

% Use with option 'manual', and inject event 'a'.
maxTime(4).
%observe a from 1 to 2.
%observe a from 3 to 4.
actions a, b.
if a then b.

