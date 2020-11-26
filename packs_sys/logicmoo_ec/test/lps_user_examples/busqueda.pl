:- expects_dialect(lps).

events a, a1, a3.
actions a11, a31. 

if true then a. 

% a if a1.
a if a2.
a if a3.

a1 if a11.
a3 if a31. 

a2 :- a22, writeln('hello').
a22.
