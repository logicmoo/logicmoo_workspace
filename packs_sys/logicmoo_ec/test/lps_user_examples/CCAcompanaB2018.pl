:- expects_dialect(lps).

%Definicion de Personas
persona(claudio).
persona(vanessa).
persona(jorge).
persona(jacinto).
persona(aquiles).

%Definicion de las relaciones ACOMPAÑA

acompaña(jorge,claudio).
acompaña(claudio,jorge).
acompaña(vanessa,jorge).
acompaña(jorge,vanessa).

%Algunas reglas
acompaña(X,Y) :- acompaña(Z,X),acompaña(Z,Y),not(X=Y), !.
