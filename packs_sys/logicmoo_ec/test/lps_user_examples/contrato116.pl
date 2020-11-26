:- expects_dialect(lps).

% LogicaB2018 Contrato 116
% Jacinto Dávila
% partially based on this Syntax https://bitbucket.org/lpsmasters/lps_corner/wiki/Syntax

maxTime(10). 

actions dragar(Camion, Cantidad, Origen), % Dragar Cantidad de Tierra desde Origen para cargar Camions
	   botar(Camion, Cantidad, Destino), % vaciar el Camion en Destino
       pagar(Cantidad), % pagar por Cantidad de tierra 
       paralizar(Quien),  % Quien paraliza la obra
       modificar(Especificacion). % Modificar la especificación de la obra

events
       validar(Camion, Cantidad, Destino).  % validar que Camion se deposito en Destino.

fluents obra_paralizada,
        especificacion(E),
        cuenta_camion(C), % van C camiones en la obra
        por_pagar(T), % falta pagar por T metros cúbicos de tierra
		validado(C, Cant, L). 

excavar(Camion, Cantidad, Lugar) from T1 to T2 if
   especificacion(E) at T1, 
   cuenta_camion(Camion) at T1,
   conforme_a(E, Cantidad, Lugar),
   capacidad(Camion, Cantidad), 
   dragar(Camion, Cantidad, Lugar) from T1 to T2. 

conforme_a(_,_,_). % Sin especificación real
capacidad(_, 10).  % Todos los camiones tienen capacidad de 10 metros cúbicos

fecha_tope(8).

if not(obra_paralizada) at T, not(fecha_tope(T)) at T
then excavar(Camion, Cantidad, Lugar) from T to T2. 

if dragar(Camion, Cantidad, aqui) from T1 to T2 then botar(Camion, Cantidad, alli) from T2 to T3. 

dragar(Camion, Cantidad, Lugar) updates Camion to Siguiente in cuenta_camion(Camion) if
  Siguiente is Camion + 1. 

botar(Camion, Cantidad, Lugar) updates Cuenta to NuevaCuenta in por_pagar(Cuenta) if
  validado(Camion, Cantidad, Lugar), NuevaCuenta is Cuenta + Cantidad. 

validar(Camion, Cantidad, Lugar) initiates validado(Camion, Cantidad, Lugar) if
  cuenta_camion(Camion). 

initially especificacion(vacia), cuenta_camion(1), por_pagar(0). 

observe validar(_, 10, alli) from 1 to 2. 
observe validar(_, 10, alli) from 2 to 3. 
observe validar(_, 10, alli) from 3 to 4. 
observe validar(_, 10, alli) from 4 to 5. 
observe validar(_, 10, alli) from 6 to 7. 
observe validar(_, 10, alli) from 7 to 8. 

