:- expects_dialect(lps).

maxTime(20).

fluents     alerta1,alerta2,alerta3.
actions     carta.
events      giroMensaje1, giroMensaje2,giroMensaje3,giroMensaje4.

observe alerta1 from 1 to 2.

if       alerta1  at T1
then     giroMensaje1 from T1 to T2.

observe alerta2 from 3 to 4.

if       alerta2 at T3
then     giroMensaje2 from T3 to T4.

observe alerta2 from 5 to 6.

if       alerta3 at T5
then     giroMensaje3 from T5 to T6.

observe carta from 3 to 5.

giroMensaje4 from T3 to T5
if 		carta from T3 to T5.


