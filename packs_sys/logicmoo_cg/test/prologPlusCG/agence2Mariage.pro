femme(nom, etat_civil).

homme(nom, etat_civil).

physique(nom, taille, poids, couleur_des_cheveux, maturite).

situation(nom, profession, salaire, situation_financiere).

gouts(nom, type2Musique, type2Livre, type2Sport).

biens(nom, liste2Biens).

profile(nom,divorcee, taille, couleur_des_cheveux, maturite, profession,
        salaire, situation_financiere, type2Sport, liste2Biens).

pere(pere, enfant).

mere(mere, enfant).

parent(x, y) :- 

frere_ou_soeur(x, y) :- 


tante_ou_oncle(x, y) :-


proche(x, y) :-


homme_convient(h, f) :-


