// Manipulation des arbres (p. 166) 
// Représentation imbriquée: [nr, [nc1, nc11, [nc12, nc121, nc122], nc13], [nc2, nc21, nc22], nc3]

// Représentation simple Pere/Fils
arbre(nr, [nc1, nc2, nc3]).
arbre(nc1, [nc11, nc12, nc13]).
arbre(nc12, [nc121, nc122]).
arbre(nc2, [nc21, nc22]).
arbre(nc22, [nc221, nc222, nc223]).
arbre(nc221, [nc2211, nc2212, nc2213]).
arbre(nc2211, [nc22111, nc22112]).
arbre(nc223, [nc2231, nc2232, nc2233]).
arbre(nc2233, [nc22331, nc22332]).

// fils(noeud, listeDesFils)
fils(n, l) :- arbre(n, l).

// pere(n1, n2: n1 est le pere de n2
pere(n1, n2) :-
  fils(n1, l),
  membre(n2, l).

// n1 est un descendant de n2: n2 -------> n1 ou si n2 -> n3 ------> n1 
descendant(n1, n2) :- pere(n2, n1).
descendant(n1, n2) :-
  pere(n2, n3),
  descendant(n1, n3).

antecedent(n1, n2) :- pere(n1, n2).
antecedent(n1, n2) :-
  pere(n3, n2),
  antecedent(n1, n3).

antecedent_commun(n1, n2, n3) :-
  antecedent(n3, n1),
  antecedent(n3, n2).

descendant_en_largeur(n1, n2) :-
  fils(n2, l),
  parcours_en_largeur(n1, l).

parcours_en_largeur(n1, []) :- !, fail.
parcours_en_largeur(n1, l) :- membre(n1, l).
parcours_en_largeur(n1, l) :-
  fils_des_fils(l, l1),
  parcours_en_largeur(n1, l1).

fils_des_fils([], []) :- !.
fils_des_fils([f|l], l_fils_des_fils) :-
  fils_des_fils(l, l2),
  trouve_fils(f, l_fils),
  concatene(l_fils, l2, l_fils_des_fils), !.

trouve_fils(f, l) :- fils(f, l), !.
trouve_fils(f, []).