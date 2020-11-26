:- expects_dialect(lps).

maxTime(6).

fluents 	tiene(X,Y), cerca(X,Y), hambre(X). 
actions 	alaba/2, toma/2, come/2, obtiene/2. 
events 		canta/1, consigue/2.

initially tiene(cuervo, queso), hambre(yo). 

%observe alaba(yo, cuervo) from 1 to 2.
%observe toma(yo, queso) from 2 to 3. 

toma(X,Y) initiates tiene(X,Y) if cerca(X,Y). 

toma(Z,Y) terminates tiene(X,Y) if X \== Z. 

toma(X,queso) terminates hambre(X). 

canta(cuervo) initiates cerca(yo, queso) if tiene(cuervo, queso). 

canta(cuervo) if alaba(yo, cuervo).  

obtiene(yo, queso) if tiene(cuervo, queso), canta(cuervo). 

consigue(X,Y) if obtiene(X,Y), toma(X,Y), come(X,Y). 

if hambre(yo) at T1 then consigue(yo, queso) from T1 to T3.  

/** <examples>
?- go(Timeline).
?- go. 
*/