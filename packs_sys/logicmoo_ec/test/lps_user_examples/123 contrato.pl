:- expects_dialect(lps).

maxTime(10).

actions pagar(comprador,cantidad,dueño). %Pago por parte del primer agente de la cantidad (x) al segundo agente
		

events causa(personal). %Situacion personal del dueño que impida la venta
		
fluents validar(comprador), %Validacion del comprador 
		por_pagar(cantidad). %El monto que queda debiendo el comprador si se cierra la Opcion a compra

cantidad(_). %Cantidad inicial de la Opcion a compra
total(_) .%Costo total de la propiedad
acordado(_). %Cantidad a devolver en caso de no concretar la compra

opcompra(comprador,cantidad,dueño) at T1.

if causa(personal),validar(comprador) to T 
   then cantidad is acordado. 
if causa(personal),validar(comprador)to T 
   then pagar(dueño,cantidad,comprador) from T to T2. 

if opcompra(comprador,cantidad,dueño) at T 
   then updates cantidad to nCantidad in por_pargar(cantidad). 

if nCuenta is total-cantidad.

