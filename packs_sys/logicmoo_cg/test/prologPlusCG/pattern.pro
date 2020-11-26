mortel(x) :- femme(x).
mortel(x) :- not(mortel(x)).

femme(khadija).
homme(khadija).

Personne(Elomari, 45, adresse(56, "Rue Guerraoui", Rabat)).
Personne(Zerktouni, 78, adresse(786, "Rue Chems", Casa)).
Personne(Cherkaoui, 56, adresse(10, "Rue Guerraoui", Rabat)).