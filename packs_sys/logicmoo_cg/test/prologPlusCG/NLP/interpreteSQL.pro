schema(voiture, voiture(num(x1), nom(x2), prop(x3), couleur(x4))).
schema(personne, personne(nom(x1), prenom(x2), age(x3), salaire(x4), paysNat(x5))).
schema(navire, navire(nom(x1), longueur(x2), nation(x3), loc(x4), cout(x5))).

voiture(ma123, fiat, hassan, rouge).
voiture(ma3462, renault, moha, noire).
voiture(ma324, simca, samira, rouge).

personne(maho, samira, 25, 4500, Alger).
personne(moh, moha, 50, 3000, Tunis).
personne(benani, hassan, 36, 5000, Maroc).
personne(bourich, soumia, 60, 4600, Alger).

navire(Koutoubia, 150, Maroc, casa, 4500).
navire(Safir, 200, Tunis, safi, 15000).
navire(Agadir, 100, Maroc, casa, 2500).
navire(Azhar, 200, Alger, agadir, 1000).


select(v_rel, l_attr_dem, l_cond, l_couple_dem) :-
  schema(v_rel, v_descr),
  term2List(v_descr, [v_rel|l_attrParam]),
  extrait(l_attrParam, l_attr_dem, l_cond, l_tous_param, l1_couple_dem, l_cond_maj),
  if(eq(l_attr_dem, "*"), eq(l_couple_dem, l_attrParam), eq(l_couple_dem, l1_couple_dem)),
  list2Term([v_rel|l_tous_param], t),
  t,
  verifieCondition(l_cond_maj).

extrait([v_attr(v_param)|l_attrParam], l_attr_dem, l_cond, [v_param|l_tous_param], l_couple_dem, l_cond_maj) :-
  if(membre(v_attr, l_attr_dem), eq(l_couple_dem, [v_attr(v_param)|l1_couple_dem]),
                                 eq(l_couple_dem, l1_couple_dem)),
  if(membre(v_oper(v_attr, v_val), l_cond), eq(l_cond_maj, [v_oper(v_param, v_val)|l1_cond_maj]),
                                            eq(l_cond_maj, l1_cond_maj)),
  extrait(l_attrParam, l_attr_dem, l_cond, l_tous_param, l1_couple_dem, l1_cond_maj), !.
extrait([], l_attr_dem, l_cond, [], [], []).

verifieCondition(l_cond) :-
  membre(v_cond, l_cond),
  not(v_cond),
  !, fail.
verifieCondition(_).

inferieur(x, y) :- x < y.
superieur(x, y) :- x > y.
egal(x, y) :- eq(x, y).
inf_egal(x, y) :- x < y, !.
inf_egal(x, y) :- eq(x, y).
sup_egal(x, y) :- x > y, !.
sup_egal(x, y) :- eq(x, y).




