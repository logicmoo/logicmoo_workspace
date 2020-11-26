:- expects_dialect(lps).

% Logic Production Systems

% declarations, initial state, observations, reactive rules, logic programs, causal laws

% Your program goes here

maxTime(15).

fluents    peligro_fisico(_), peligro_sancion_social(_).
events    peligro_de_muerte(Pasajeros), alejarse_del_peligro(Pasajeros), 
          causar_muerte(Pasajeros,_otros), posible_sancion_social(_sociedad,Pasajeros), 
          evitar_sancion(_sociedad,Pasajeros), gana(X).
actions    usar_detonador(Pasajeros), guardar_detonador(Pasajeros).

% Si descomento la siguiente restricción el peligro de muerte continúa. Pero no se 
% ejecuta tampoco el guardar_detonador/1, así que no es
%false    usar_detonador(_), peligro_posible_sancion_social(_).

% Rules for detonation:

if peligro_de_muerte(Pasajeros) from T1 to T2 then alejarse_del_peligro(Pasajeros) from T9 to T10.
alejarse_del_peligro(Pasajeros) from T9 to T10 if usar_detonador(Pasajeros) from T4 to T5.

% Rules for social sanction:

causar_muerte(Pasajeros, _otros) from T10 to T11 if usar_detonador(Pasajeros) from T4 to T5.
posible_sancion_social(_sociedad,Pasajeros) if causar_muerte(Pasajeros,_otros).
if posible_sancion_social(_sociedad,Pasajeros) from T9 to T10 then evitar_sancion(_sociedad,Pasajeros) from T11 to T12.
evitar_sancion(_sociedad,Pasajeros) if guardar_detonador(Pasajeros) from T3 to T4.

% Rules for moral battle between protagonist and antagonist:

gana(batman) if evitar_sancion(_sociedad,ciudadanos), evitar_sancion(_sociedad,delincuentes).
gana(joker) if alejarse_del_peligro(ciudadanos).
gana(joker) if alejarse_del_peligro(delincuentes).

initially    peligro_fisico(pasajeros), peligro_sancion_social(pasajeros).
observe      peligro_de_muerte(ciudadanos) from 1 to 2, peligro_de_muerte(delincuentes) from 1 to 2.

guardar_detonador(_)    terminates peligro_sancion_social(_).
usar_detonador(_)       terminates peligro_fisico(_).

/** <examples> 
?- go(Timeline).
*/
