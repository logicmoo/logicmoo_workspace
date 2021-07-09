
:- expects_dialect(lps).

maxTime(4).
played(a).
played(b).
fluents gameOn.
initially gameOn.

if gameOn at T1, played(P1) at T1, played(P2) at T1, P2\==P1
then terminate gameOn from T1, true from _.
