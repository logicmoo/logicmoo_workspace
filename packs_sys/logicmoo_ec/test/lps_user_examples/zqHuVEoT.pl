:- expects_dialect(lps).

maxTime(5).

fluents     trabajo.
actions     trabajo_por_pago.
events      vender_billetes, ir_al_cine.

initially trabajo. 

if         trabajo at T1
then     vender_billetes from T1 to T2.

vender_billetes from T1 to T2
if         trabajo_por_paga from T1 to T2.

ir_al_cine from T2 to T3
if 		vender_billetes from T2 to T3.


vender_billetes terminates trabajo. 

/** 
?- go(Timeline).
*/

