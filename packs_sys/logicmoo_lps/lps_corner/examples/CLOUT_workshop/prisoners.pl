
:- expects_dialect(lps).

maxTime(5).
fluents		total_years_in_jail(_,_).
actions		refuses(_), bears_witness(_), gets(_,_).

initially	total_years_in_jail(me,0),  total_years_in_jail(you,0).
observe		refuses(you) from 1 to 2.
observe	    bears_witness(me) from 1 to 2.

other(me,you).
other(you,me).

if		bears_witness(P) from T1 to T2, refuses(Q) from T1 to T2
then	gets(P,0) from T2 to T3,    gets(Q,3) from T2 to T3.

if		bears_witness(P) from T1 to T2, bears_witness(Q) from T1 to T2, other(P,Q)
then	gets(P,2) from T2 to T3.

if		refuses(P) from T1 to T2,  refuses(Q) from T1 to T2, other(P,Q)
then	gets(P,1) from T2 to T3.

if		refuses(O) from T1 to T2, other(I,O)
then	refuses(I) from T2 to T3.

if		bears_witness(O) from T1 to T2, other(I,O)
then	bears_witness(I) from T2 to T3.


gets(Prisoner,Years)	initiates total_years_in_jail(Prisoner,NewYears) 
if		total_years_in_jail(Prisoner,OldYears),    NewYears is OldYears+Years.

gets(Prisoner,Years)	terminates total_years_in_jail(Prisoner, OldYears).

/** <examples>
?- go(Timeline).
*/