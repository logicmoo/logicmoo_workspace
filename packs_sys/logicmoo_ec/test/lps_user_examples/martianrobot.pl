:- expects_dialect(lps).

maxTime(15). 

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

step(X2,Y2) initiates free(X,Y) if at_pos(X,Y). 

turn_right terminates lookingtowards(X,Y).
turn_right initiates  lookingtowards(X,Yminus) if 
   at_pos(X,Y), 
   lookingtowards(Xplus,Y), 
   Xplus is X+1, Yminus is Y-1. 
turn_right initiates  lookingtowards(Xminus,Y) if 
   at_pos(X,Y), 
   lookingtowards(X,Yminus), 
   Xminus is X-1, Yminus is Y-1. 
turn_right initiates  lookingtowards(X,Yplus) if 
   at_pos(X,Y), 
   lookingtowards(Xminus,Y), 
   Xminus is X-1, Yplus is Y+1.
turn_right initiates lookingtowards(Xplus,Y) if 
   at_pos(X,Y), 
   lookingtowards(X,Yplus), 
   Xplus is X+1, Yplus is Y+1. 

% false free(X,Y), obstacle(X,Y). 

false at_pos(X,Y), step(X,Y).
%false visited(X,Y), step(X,Y).
%false life(X,Y), lookingtowards(X,Y), turn_right. 

if lookingtowards(X,Y) at T1, free(X,Y) at T1, 
not(visited(X,Y)) at T1
then step(X,Y) from T1 to T2. 

if lookingtowards(X,Y) at T1, free(X,Y) at T1,
visited(X,Y) at T1 
then turn_right from T1 to T2. 

if lookingtowards(X,Y) at T1, obstacle(X,Y) at T1 %, 
% not(life(X,Y)) at T1 
then turn_right. 

if lookingtowards(X,Y) at T1, obstacle(X,Y) at T1, 
life(X,Y) at T1 
then report. %, turn_right. 

/** <examples>
?- go(Timeline).
?- go. 
*/
