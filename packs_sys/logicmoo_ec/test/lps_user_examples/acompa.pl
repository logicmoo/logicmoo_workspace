:- expects_dialect(lps).

/* Estudio de relaciones */

ama(romeo, julieta).
ama(julieta, romeo).
ama(bolivar, colombia).
ama(manuela, bolivar). 

acompaña(X,Y) :- ama(X,Y). 
acompaña(X,Y) :- combina(X,Y). 
acompaña(X,Y) :- rcombina(X,Y). 
acompaña(X,Y) :- viaja_junto(X,Y, []).
% acompaña(X,Y) :- rviaja_junto(X,Y).

combina(cafe, chocolate). 
combina(salsa, pasta).

rcombina(X,Y) :- combina(Y,X). 

viaja_junto(fulano, sutano). 

viaja_junto(X,Y,_) :- viaja_junto(X,Y). 

viaja_junto(X,Y, C) :- not(member((X,Y),C)), viaja_junto(Y,X, [(X,Y)|C]). 


/** <examples>
?- acompaña(X,Y). 
?- viaja_junto(X,Y,[]).
*/
