:- expects_dialect(lps).

/* Bienvenidos a la Programación Lógica */

ama(romeo, julieta).
ama(julieta, romeo).
ama(bolivar, colombia).
ama(manuela, bolivar). 

mortal(X) :- humano(X). 
humano(socrates).


acompaña(rosa, amanda).
acompaña(amanda, gretta).
acompaña(gretta, cesar).
acompaña(cesar, rosa).

acompañante(X) :- persona(X).
persona(rosa).


/*
?- ama(romeo,X).
*/

