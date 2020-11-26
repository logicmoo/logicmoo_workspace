:- expects_dialect(lps).

% Versión historia del Rey Salomón y las dos mujeres (Reyes 3:3-28)
% Vanessa Marquez Logica B2018

maxtime(10). 

% Evento disputa entre a y b, poner a prueba a a y b, tomar decisión

events disputa_entre(A,B), poner_a_prueba(A,B),  
       propone_salida_drastica(Agente), tomar_decision.

% Acción 
% Salomón propone una salida drástica
% Salomón propone dividir al niño en dos
% Entregar el niño vivo a la verdadera madre
actions propone_dividir_nino(Agente),
		dice(Agente, Mensaje),
		declara(Agente, Veredicto),
		dicta(Agente, Sentencia). 

% Observo 

observe disputa_entre(a,b) from 1 to 2. 

% Si se plantea una disputa entre a y b en T1 y T2
% Entonces pongo a prueba a a y b en T2 y T3
if disputa_entre(A,B) from T1 to T2
then poner_a_prueba(A,B) from T2 to T3. 

% Pongo a prueba a a y b en T2 y T3
% Si propongo salida drástica en T2 y T3
% Propongo salida drástica entre T2 y T3
% Si divido al niño en dos en T2 y T3
poner_a_prueba(A,B) from T1 to T2 if
   propone_salida_drastica(salomon) from T1 to T2. 

propone_salida_drastica(A) from T1 to T2 if
  propone_dividir_nino(A) from T1 to T2. 

% Si el Rey Salomón propone dividir al niño en dos en T2 y T3
% Entonces digo no lo divida entreguélo a ella en T2 y 
if propone_dividir_nino(salomon) from T1 to T2,
   mujer(X),
   soy_su_madre(X)
then dice(X, 'No lo mate! Déselo a Ella') from T2 to T3. 

% Si el Rey Salomón propone dividir al niño en dos en T2 y T3
% Entonces digo si divídalo en T2 y T3
if propone_dividir_nino(salomon) from T1 to T2,
   mujer(X),
   not(soy_su_madre(X))
then dice(X, 'Sí, mátelo') from T2 to T3.

% Si Pongo a prueba a a y b en T2 y T3 y
%  a dice que si lo divida y
%      b dice que no lo divida
% Entonces se que la verdadera madre es b en T2 y T3
if propone_dividir_nino(Juez) from T1 to T2,
   dice(X, 'No lo mate! Déselo a Ella') from T3 to T4,
   dice(Y, 'Sí, mátelo') from T5 to T6
then declara(Juez, la_verdadera_madre_es(X)) from T7 to T8,
	 dicta(Juez, entreguen_nino_a(X)) from T8 to T9. 

mujer(a).
mujer(b).
soy_su_madre(b).
