:- expects_dialect(lps).

maxTime(5). 

fluents at_pos/2, free/2, visited/2, obstacle/2, life/2, lookingtowards/2.
actions step(X,Y), turn_right, report. 

initially at_pos(0,0), life(2,1), free(1,0), free(2,0), obstacle(3,0), obstacle(2, -1), obstacle(2,1), lookingtowards(1,0).  

step(X,Y) terminates lookingtowards(X,Y).
step(X,Y) initiates lookingtowards(X,Z) if 
   at_pos(X,Y0), lookingtowards(X,Y), 
   Diff is Y-Y0, abs(Diff) >0, Z is Y + Diff.  
   
step(X,Y) initiates lookingtowards(Z,Y) if 
   at_pos(X0,Y), lookingtowards(X,Y), 
   Diff is X-X0, abs(Diff) >0, Z is X + Diff.   

step(X,Y) initiates at_pos(X,Y). 

step(X2,Y2) terminates at_pos(X,Y) if at_pos(X,Y). %, (X2\==X;Y\==Y2).   

step(X,Y) terminates free(X,Y). 
step(X2,Y2) initiates visited(X,Y) if at_pos(X,Y). 


if lookingtowards(X,Y), free(X,Y), 
not(visited(X,Y))
then step(X,Y). 


/** <examples>
?- go(Timeline).
?- go. 
*/
