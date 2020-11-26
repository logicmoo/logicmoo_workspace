:- expects_dialect(lps).

maxTime(10). 

fluents en/2, libre/2, estuve/2, obstaculo/2, vida/2, mirada/2.
actions ir(X,Y), derecha, izquierda, reporte. 

initially 
   en(0,0), vida(2,1), libre(1,0), libre(2,0), obstaculo(3,0), 
   obstaculo(2, -1), obstaculo(2,1), mirada(1,0). 

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
