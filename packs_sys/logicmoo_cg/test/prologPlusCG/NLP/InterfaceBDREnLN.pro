dict_requete(select, ["Imprimer", "Afficher", "Lister", "Selectionner", "Donner", "Montrer", "Quel", "Quels"]).

dict_req_option(["moi", "est", "sont"]).

dict_attribut(nom, ["nom", "noms"]).
dict_attribut(longueur, ["longueur", "longueurs"]).
dict_attribut(nation, ["nation", "nationalite", "pays"]).
dict_attribut(loc, ["localite", "position", "positions"]).
dict_attribut(age, ["age"]).
dict_attribut(salaire, ["salaire"]).
dict_attribut(cout, ["cout", "prix"]).

dict_relation(navire, nom, [Koutoubia, Marrakech, Agadir, Safir, Azhar], ["navire", "navires", "bateau", "bateaux"]).
dict_relation(personne, nom, [maho, benani, moh, bourich], ["personne", "femme", "homme"]).
dict_relation(voiture, nom, [fiat, renault, simca], ["voiture", "auto", "automobile", "char", "bagnole"]).

interfaceBDR :-
  writeln("Posez votre requete en Langage naturel: " ),
  readSentence(p),
  requeteNormale(p, v_modele_sem),
  if(free(v_modele_sem), writeln("Erreur dans la requete."), activeModele(v_modele_sem)), 
  writeln(""), writeln("Autre question ? (oui/non): "),
  read(r), writeln(""),
  if(eq(r, oui), interfaceBDR, writeln("Au revoir ...")),  !.

requeteNormale(p, req([q_frag, v_ques], [a_frag, l_attr], d_frag, [c_frag, v_rel, l_cond])) :-
  typ_quest(p, x1, v_ques),
  concatene(q_frag, x1, p),
  attributs(x1, x2, l_attr),
  concatene(a_frag, x2, x1),
  or(det(x2, x3), article(x2, x3)),
  concatene(d_frag, x3, x2),
  partieCond(x3, [v_ponct], l_cond, v_rel),
  concatene(c_frag, [v_ponct], x3), !.

typ_quest([m|x], y, v_ques) :-
  dict_requete(v_ques, l_exp_syn),
  membre(m, l_exp_syn),
  option_req(x, y), !.

option_req([m|x], x) :-
  dict_req_option(l_opt),
  membre(m, l_opt), !.
option_req(x, x).

attributs(x, y, [v_attr|l_attr]) :-
  article(x, x1),
  attribut(x1, x2, v_attr),
  conjonction(x2, x3),
  attributs(x3, y, l_attr), !.
attributs([v_det|x], [v_det|x], []) :-
  membre(v_det, ["de", "du", "des", ".", "?", "!"]), !.
attributs(p, p, "*") :-
  article(p, p1).

attribut([m|x], x, v_attr) :-
  dict_attribut(v_attr, l_exp_syn),
  membre(m, l_exp_syn), !.

conjonction([v_conj|x], x) :-
  membre(v_conj, ["et", ",", ";", "puis", "ensuite"]), !.
conjonction(x, x).
  
article([a|x], x) :-
  membre(a, ["le", "la", "les", "un", "une"]), !.
article(["l","'"|x], x).
article(["l"|x], x).

det(["de", "la"|x], x) :- !.
det(["de", "l", "'"|x], x) :- !.
det(["de", "l"|x], x) :- !.
det([d|x], x) :-
  membre(d, ["de", "du", "des"]), !. 

partieCond([m|x], y, l_cond, v_rel) :-
  dict_relation(v_rel, v_cle, l_val_cle, l_exp_syn),
  membre(m, l_exp_syn),
  conditions(x, y, l_cond), !.
partieCond([v_val1|x], x, [egal(v_cle, v_val)], v_rel) :-
  string2Ident(v_val1, v_val),
  dict_relation(v_rel, v_cle, l_val_cle, l_exp_syn),
  membre(v_val, l_val_cle), !.

conditions(x, y, [v_oper(v_attr, v_val)|l_cond]) :-
  pre_modifier(x, x1),
  attribut(x1, x2, v_attr),
  post_modifier(x2, x3, v_oper, v_val),
  conjonction(x3, x4),
  conditions(x4, y, l_cond), !.
conditions([v_ponct], [v_ponct], []).

pre_modifier(["qui", "a"|x], y) :-
  pre_mod(x, y), !.
pre_modifier(x, y) :-
  or(eq(x, ["dont"|x1]), eq(x, x1)),
  article(x1, y), !.
pre_modifier(["de"|x],x).

pre_mod([m|x], x) :- 
  or(eq(m, "comme"), eq(m, "pour")), !.
pre_mod(x, y) :- article(x, y).

post_modifier(x, y, v_oper, v_val) :-
  consomme(x, x1, "est"),
  consomme(x1, x2, "strictement"),
  typ_oper(x2, [v_val1|y], v_oper),
  conversion_arg(v_val1, v_val), !.

conversion_arg(v_arg, v_val) :-
  car is v_arg:charAt(0),
  v is "java.lang.Character":isDigit(car),
  v = true,
  string2Integer(v_arg, v_val), !.
conversion_arg(v_arg, v_val) :-
  string2Ident(v_arg, v_val), !.

consomme([m|x], x, m) :- !.
consomme(x, x, m).

typ_oper(["inferieur", "ou", "egal", "a"|x], x, inf_egal) :- !.
typ_oper(["superieur", "ou", "egal", "a"|x], x, sup_egal) :- !.
typ_oper([v1_oper, "a"|x], x, v_oper) :-
  membre(v1_oper, ["egal", "inferieur", "superieur"]),
  string2Ident(v1_oper, v_oper), !.
typ_oper(x, x, egal).

string2Ident(x, y) :-
  y is "aminePlatform.util.Identifier":new(x), !.

string2Integer(x, y) :- 
  y is "java.lang.Integer":decode(x), !.

activeModele(req([q_frag, v_ques], [a_frag, l_attr], d_frag, [c_frag, v_rel, l_cond])) :-
  v_ques(v_rel, l_attr, l_cond, l_coupD),
  afficheNom(l_coupD),
  fail.
activeModele(_).

afficheNom(l_cple_attr_val) :-
  membre(v_attr(v_val), l_cple_attr_val),
  write(" La valeur de "),
  dict_attribut(v_attr, [v1_attr|_]),
  write(v1_attr), write(" est: "), writeln(v_val),
  fail.
afficheNom(_) :- writeln(" ").


