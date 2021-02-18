
:- expects_dialect(lps).

maxTime(10).

actions send(_).
fluents receipt_available(_).

events receipt(_).

receipt(X) initiates receipt_available(X).

observe receipt(a) from 4 to 5.
observe receipt(b) from 7 to 8.

if true then pay(a) from T1 to T2, pay(b) from T1 to T3, writeln(finito-T4/T5) from T4 to T5.

pay(X) from T1 to T3 if
    send(X) from T1 to T2, 
	receipt_available(X) at T3.
