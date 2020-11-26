:- expects_dialect(lps).

% Situacion de Venezuela Enero 2019
% Modelo A
% 

maxTime(20). 
actions accion_de(Agente, Accion).

if accion_de(maduro, insistir) to T
then accion_de(trump, sancionar) from T.

if accion_de(maduro, insistir) from T to T2, 
   accion_de(guaido, insistir) from T2 to T3
then accion_de(trump, bombardear) from T3. 

if accion_de(guaido, insistir) from T to T2,
   accion_de(trump, bombardear) from T2 to T3, 
   accion_de(maduro, insistir) from T2 to T3 
then accion_de(trump, invadir) from T3. 

if accion_de(guaido, desistir) to T 
then accion_de(trump, desistir) from T. 
 
if accion_de(trump, desistir) to T 
then accion_de(guaido, desistir) from T. 

if accion_de(trump, invadir) from T to T1, 
   accion_de(maduro, insistir) from T1 to T2
then accion_de(trump, desistir) from T2. 

if accion_de(trump, invadir) from T to T1, 
   accion_de(duque, invadir) from T to T1,
   accion_de(bolsonaro, invadir) from T to T1
then accion_de(maduro, desistir) from T1. 

if accion_de(maduro, desistir) to T
then accion_de(guaido, insistir) from T. 

% false accion_de(Ag, Act) to T, 
%      accion_de(Ag, desistir) to T, Act\=desistir.

false accion_de(Ag, insistir) to T, 
      accion_de(Ag, desistir) to T. 

if  accion_de(guaido, insistir) to T
then accion_de(maduro, insistir) from T. 

if  accion_de(maduro, insistir) to T
then accion_de(guaido, insistir) from T. 

observe accion_de(maduro, insistir) from 1 to 2.

