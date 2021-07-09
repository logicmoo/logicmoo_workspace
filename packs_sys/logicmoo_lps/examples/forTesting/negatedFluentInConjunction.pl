
:- expects_dialect(lps).

maxTime(4).
played(a).
played(b).
fluents gameOver.

% Moving not gameOver to the end of the antecedent causes different behaviour!
if not gameOver at T1, played(P1) at T1, played(P2) at T1, P2\==P1
then initiate gameOver from T1, writeln(gameOver) from _.