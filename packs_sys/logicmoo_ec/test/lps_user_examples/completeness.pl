:- expects_dialect(lps).

events e1, e2, e3, a1.
actions a1.
fluents p.

if e1 from T1 then a1 from T2, T2>T1.
if e2 from T then p at T+3.
a1 initiates p.
e3 terminates p.

observe e1 from 1.
observe e3  from 3.
observe e2 from 4.
