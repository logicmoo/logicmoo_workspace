:- expects_dialect(lps).

%maxtime: Numero maximo de estados
maxTime(10).

%actions: expresion atomica que se realiza instantaneamente y son generadas por
%         el sistema. Se pueden declarar como eventos externos. 
actions  atacar, robar, llamar_al(Number), repita_mensaje_esposa(Mensaje).

%fluents: oraciones atomicas dependientes del tiempo. 
%         Son extensionales si estan representados por hechos y actualizados por eventos.
%         Son intensionales si estan representados por clausulas que reflejan los cambios de los fluidos extencionales.                 
fluents es_un_arma, es_un_regalo.

%events:  Pueden ser externos, dado por observaciones o generados por el sistema.
%         Pueden ser atomicos, tienen lugar inmediatamente de un estados al siguiente.
%         Pueden ser compuestos, tienen lugar en una serie de 0 o mas estados.
events prepare, persuade, muestra_del_general(Number),
       llamar_general(Number), general_recuerda_esposa(Mensaje).

%ENTRADA. initially: especifica los fluidos que son verdaderos en el estado inicial t = 0.
initially es_un_arma.

%observaciones: entradas del sistema que ocurren en trnacisiones de estados
%               t1 y t2 deben ser enteros positivos sucesivos.
observe muestra_del_general('+86-555000001') from 2 to 3.
observe general_recuerda_esposa('##########') from 3 to 4.

%RESTRICCION. false: restringe las acciones del literal candidato
%                    Los fluidos en las restricciones pueden ser negativo, pero las acciones y los eventos no se pueden negar.
false es_un_regalo, atacar. 

%reglas reactivas: formados por literales donde cada una es una formula atomica temporizada.
%                  Todos los tiempos despues de "then" son posteriores o iguales al tiempo anterior a "then".
if es_un_arma then prepare, atacar. 
if es_un_arma then persuade.

%clausulas para predicados dependientes del tiempo
%acciones o eventos compuestos que pueden tener lugar en 0 o mas trancisiones de estados.
prepare from T1 to T2 if 
            true at T1, 
			T2 is T1 + 6. 

persuade if muestra_del_general(Number), 
			general_recuerda_esposa(Mensaje),
			llamar_general(Number),
			repita_mensaje_esposa(Mensaje).

llamar_general(Number) if robar, llamar_al(Number).

%leyes causales. Aplicado a ventos externos sin tiempo explicito.
%initiates: Si el evento tiene lugar de T a T+1 el fluido es verdadero en T+1 si todos los literales son verdaderos en T.
%terminates: Si el evento tiene lugar de T a T+1 entonces el fluido del momento termina.                
persuade terminates es_un_arma.
persuade initiates es_un_regalo. 

/** <examples>
?- go(T).
?- go.
*/
