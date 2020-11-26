:- expects_dialect(lps).

/* Bienvenidos a la Programación Lógica */

acompaña(Hijo, Padre).
acompaña(Padre, Hijo).
acompaña(Madre, Hijo).
acompaña(Hijo, Madre).
acompaña(Hombre, Mujer).
acompaña(Mujer, Hombre).
acompaña(Respecto, Rezar).
acompaña(Rezar, Respecto).
acompaña(Arepa, Desayuno).
acompaña(Desayuno, arepa).
acompaña(Almuerzo, Arroz).
acompaña(Arroz, Almuerzo).
acompaña(Persona, mascota).
acompaña(mascota, Persona).
acompaña(amigo, feliz).
acompaña(Feliz, amigo).


acompaña(X):- plato(X).
plato(Arroz).

