:- expects_dialect(lps).

maxTime(6).

fluents 	has(X,Y), near(X,Y).
actions 	pray/2, takes/2, eats/2, gets/2.
events 		hunger/1, sings/1, finds/2.

initially has(crow, cheese).

observe hunger(me) from 1 to 2. 

takes(X,Y) initiates has(X,Y) if near(X,Y).

takes(Z,Y) terminates has(X,Y) if X \== Z.

sings(crow) initiates near(me, cheese) if has(crow, cheese).

sings(crow) if pray(me, crow).

gets(me, cheese) if has(crow, cheese), sings(crow).

finds(X,Y) if gets(X,Y), takes(X,Y), eats(X,Y).

if hunger(me) from T1 to T2 then finds(me, cheese) from T2 to T3.

/** <examples>
?- go(Timeline).
?- go. 
*/