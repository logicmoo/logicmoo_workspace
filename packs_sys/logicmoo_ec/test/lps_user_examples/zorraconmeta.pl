:- expects_dialect(lps).

maxTime(5).

fluents 	tiene(X,Y), cerca(X,Y), hambre(X). 
actions 	alaba/2, toma/2. 
events 		canta/1, obtiene/2.

initially tiene(cuervo, queso), hambre(yo). 

%observe alaba(yo, cuervo) from 1 to 2.
%observe toma(yo, queso) from 2 to 3. 

toma(X,Y) initiates tiene(X,Y) if cerca(X,Y). 

toma(Z,Y) terminates tiene(X,Y) if X \== Z. 

toma(X,queso) terminates hambre(X). 

canta(cuervo) initiates cerca(yo, queso) if tiene(cuervo, queso). 

canta(cuervo) if alaba(yo, cuervo).  

obtiene(X,Y) if tiene(X,Y), toma(X,Y). 

cerca(yo,queso) if tiene(cuervo, queso).   

if hambre(yo) at T1 then obtiene(yo, queso) from T1 to T2. 

/** <examples>
?- go(Timeline).
*/