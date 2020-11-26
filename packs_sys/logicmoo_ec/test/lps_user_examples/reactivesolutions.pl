:- expects_dialect(lps).

events b/1.

if true then b(X). 

b(X) if c(X).

c(1) :- writeln(1).
c(2) :- writeln(2).
c(3) :- writeln(3).