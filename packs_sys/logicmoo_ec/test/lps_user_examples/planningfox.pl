:- expects_dialect(lps).

maxTime(6).

fluents  has(X,Y), near(X,Y).
actions  praise/2, take/2, eat/2.
events   hunger/1, sing/1, gets/2, reach/1.

initially has(crow, cheese). 

observe hunger(me) from 1 to 2.

take(X,Y) initiates has(X,Y) if near(X,Y). 

take(Z,Y) terminates has(X,Y) if X \== Z.

sing(crow) initiates near(me,cheese). 

sing(crow) if praise(me, crow).

%gets(me, cheese) from T1 to T3 if
%	has(crow, cheese) at T1, sing(crow) from T1 to T2, 
	% near(me, crow) at T2,
%	take(me, cheese) from T2 to T3.

reach(has(me,cheese)) if reach(near(me,cheese)), take(me,cheese). 

reach(near(me, cheese)) if sing(crow). 

if hunger(me) from T1 to T2 
then reach(has(me, cheese)) from T2 to T3, eat(me, cheese) from T3 to T4.

/** <examples>
?- go(T).
*/

