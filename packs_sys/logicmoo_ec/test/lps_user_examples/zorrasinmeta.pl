:- expects_dialect(lps).

maxTime(5).

fluents 	tiene(X,Y), cerca(X,Y). 
actions 	alaba/2, toma/2. 
events 		canta/1.

initially tiene(cuervo, queso). 

observe alaba(yo, cuervo) from 1 to 2.
observe toma(yo, queso) from 2 to 3. 

toma(X,Y) initiates tiene(X,Y) if cerca(X,Y). 

canta(cuervo) initiates cerca(yo, queso) if tiene(cuervo, queso). 

canta(cuervo) if alaba(yo, cuervo). 

/** <examples>
?- go(Timeline).
*/