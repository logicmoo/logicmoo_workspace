

:- expects_dialect(lps).

maxTime(10).
fluents locked(_), trash(_), bin(_).
actions dispose(_,_), keep(_).

initially bin(bucket).

if true then  
	(if bin(Container) at 3 then 
		dispose(garbage, Container) else keep(garbage) ).

if true then
	keep(uhuh) from 5,
	terminate bin(bucket),
	(if bin(Container)  then 
		dispose(garbage, Container) ).
