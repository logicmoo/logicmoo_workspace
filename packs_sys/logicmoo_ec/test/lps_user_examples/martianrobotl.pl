:- expects_dialect(lps).

maxTime(10). 

fluents en/2, libre/2, estuve/2, obstaculo/2, vida/2, mirada/2.
actions ir(X,Y), derecha, reporte. 

initially 
   en(0,0), vida(2,1), libre(1,0), libre(2,0), obstaculo(3,0), 
   obstaculo(2, -1), obstaculo(2,1), mirada(1,0).  

ir(X,Y) terminates mirada(X,Y). 
ir(X,Y) initiates mirada(X,Z) if 
   en(X,Y0), mirada(X,Y), 
   Diff is Y-Y0, abs(Diff) >0, Z is Y + Diff.  
   
ir(X,Y) initiates mirada(Z,Y) if
   en(X0,Y), mirada(X,Y), 
   Diff is X-X0, abs(Diff) >0, Z is X + Diff.   

ir(X,Y) initiates en(X,Y). 

ir(X2,Y2) terminates en(X,Y). % if en(X,Y), (X2\==X;Y\==Y2).   

ir(X,Y) terminates libre(X,Y). 
ir(X2,Y2) initiates estuve(X,Y) if en(X,Y).  

ir(X2,Y2) initiates libre(X,Y) if en(X,Y). 

derecha terminates mirada(X,Y).
derecha initiates  mirada(X,Yminus) if 
   en(X,Y), 
   mirada(Xplus,Y), 
   Xplus is X+1, Yminus is Y-1. 
derecha initiates  mirada(Xminus,Y) if 
   en(X,Y), 
   mirada(X,Yminus), 
   Xminus is X-1, Yminus is Y-1. 
derecha initiates  mirada(X,Yplus) if 
   en(X,Y), 
   mirada(Xminus,Y), 
   Xminus is X-1, Yplus is Y+1.
derecha initiates  mirada(Xplus,Y) if 
   en(X,Y), 
   mirada(X,Yplus), 
   Xplus is X+1, Yplus is Y+1. 

% false libre(X,Y), obstaculo(X,Y). 

% Si el espacio adelante está libre
if mirada(X,Y), libre(X,Y),
% y yo no he estado en ese espacio antes,
not(estuve(X,Y)) 
% entonces vaya a ese espacio.
then ir(X,Y). 

%Si el espacio adelante está libre
if mirada(X,Y), libre(X,Y),
%y he estado en ese espacio antes,
estuve(X,Y) 
%entonces volteo a la derecha.
then derecha. 

%Si hay un obstáculo adelante
if mirada(X,Y), obstaculo(X,Y), 
%y no muestra signos de vida,
not(vida(X,Y)) 
%entonces volteo a la derecha.
then derecha. 

%Si hay un obstáculo adelante
if mirada(X,Y), obstaculo(X,Y), 
%y muestra signos de vida,
vida(X,Y) 
%entonces lo reporto al centro de control
then reporte, 
%y volteo a la derecha.
derecha. 

/** <examples>
?- go(Timeline).
?- go. 
*/
