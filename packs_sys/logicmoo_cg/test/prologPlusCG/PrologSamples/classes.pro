prop(mamifere,		lien(sorte_de), vertebre).
prop(proboscidien,	lien(sorte_de), mamifere).
prop(elephant,		lien(sorte_de), proboscidien).
prop(mamouth,		lien(sorte_de), proboscidien).
prop(mastodonte,	lien(sorte_de), proboscidien).

prop(bimbo,	lien(est_un), elephant).
prop(furie,	lien(est_un), mastodonte).
prop(nez_rouge,	lien(est_un), mamouth).

prop(mamifere, attr(nourriture, lait)).
prop(mamifere, attr(peau, poils)).

prop(proboscidien, attr(taille, grande)).
prop(proboscidien, attr(forme_nez, trompe)).

prop(elephant, attr(son, barir)).
prop(elephant, attr(habitat, brousse)).
prop(elephant, attr(defense, deux)).

prop(mamouth, attr(pelage, fourni)).

prop(mastodonte, attr(molaire, mamelones)).
prop(mastodonte, attr(defense, sup_deux)).

classe(_Instance, _Classe) :-
	prop(_Instance, lien(est_un), _Classe).

classe(_Animal, _Classe) :-
	prop(_Animal, lien(sorte_de), _Classe).


possede(_Objet, _Attribut, _Valeur) :-
	prop(_Objet, attr(_Attribut, _Valeur)).

sousclasse(_Classe, _Objet) :- classe(_Objet, _Classe).