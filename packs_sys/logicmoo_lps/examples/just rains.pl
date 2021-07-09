
:- expects_dialect(lps).

% just rains.lps

actions rain(_).
% observe rain(0) from 0 to 1.
if true then rain(1) from 1 to 2.
if rain(T1) from T1 to T2  then rain(T2) from T2 to T3.
