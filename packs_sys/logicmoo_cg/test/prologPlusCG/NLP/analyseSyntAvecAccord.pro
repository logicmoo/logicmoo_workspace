analyse :-
  readSentence(s),
  phrase(s).

phrase(s) :-
  gn(s, p1, g_genre, n_nombre, p_personne, p_gn_cor),
  gv(p1, ["."], g_genre, n_nombre, p_personne, p_gv_cor),
  imprime_phrase(p_gn_cor),
  write(" "),
  imprime_phrase(p_gv_cor), !.

gn(p, p1, g_genre, n_nombre, p_personne, p_gn_cor) :- 
  pr_pers_sj(p, p1, g_genre, n_nombre, p_personne, p_gn_cor), !.
gn(p, p1, g_genre, n_nombre, 3, p_gn_cor) :- 
  sn(p, p1, g_genre, n_nombre, p_gn_cor), !.

gv(p, p1, g_genre, n_nombre, p_personne, [v_verb_corr, c_dir_corr, c_ind_corr, c_circs_corr]) :-
  verbe(p, p2, n_nombre, p_personne, v_verb_corr),
  compDirect(p2, p3, c_dir_corr),
  compIndirect(p3, p4, c_ind_corr),
  compCircs(p4, p1, c_circs_corr), !.
gv(p, p1, g_genre, n_nombre, p_personne, [v_etat_corr, v_attr_corr, c_circs_corr]) :-
  verbeEtat(p, p2, n_nombre, p_personne, v_etat_corr),
  attribut(p2, p3,  g_genre, n_nombre, v_attr_corr),
  compCircs(p3, p1, c_circs), !.

// On retarde la vérification et la correction de l'accord de l'article; une fois le genre et le nombre du nom
// déterminés. Quant aux adjectifs qui précèderaient le nom, il faut les lire en imposant seulement une 
// contrainte de cohérence (tous les adjectifs se rapportant à un nom doivent avoir le même genre et nombre), 
// ceci est réalisé avec l'utilisation des variables g1_genre et n1_nombre. Cette faible contrainte sur la adj.
// sera renforcée par la suite, une fois le genre et le nombre du nom déterminés. Une fois le nom lu et analysé,
// on revient à l'article et aux adjs. (car le genre et le nombre du nom déterminent le genre et le nombre de 
// l'article et des adjs.
sn([a|p], p1, g_genre, n_nombre, [a_art_corr, l_adjs1_corr, n_nom_corr, l_adjs2_corr, c2Nom_corr]) :-
  adjs(p, p2, g1_genre, n1_nombre, l1_adjs1_corr),
  nom(p2, p3, g_genre, n_nombre, n_nom_corr),
  art(a, g_genre, n_nombre, a_art_corr),
  adjs(p, p21, g_genre, n_nombre, l_adjs1_corr),
  adjs(p3, p4, g_genre, n_nombre, l_adjs2_corr),
  comp2Nom(p4, p1, c2Nom_corr), !.

// l'accord dans un complément y est local
comp2Nom(["de"|p], p1, ["de"|s_sn_corr]) :-
  sn(p, p1, _, _, s_sn_corr), !.
comp2Nom(p, p, []).

compDirect(p, p1, s_sn_corr) :-
  sn(p, p1, _, _, s_sn_corr), !.
compDirect(p, p, []).

// l'accord entre le sujet et l'attribut n'a lieu que dans le cas où l'attribut est un adjectif.
attribut(p, p1, g_genre, n_nombre, v_adj_corr) :- 
  adj(p, p1, g_genre, n_nombre, v_adj_corr), !.
attribut(p, p1, g_genre, n_nombre, s_sn_corr) :- 
  sn(p, p1, _, _, s_sn_corr), !.
attribut(p, p, _, _, []).

compIndirect(["à"|p], p1, ["à"|s_sn_corr]) :-
  sn(p, p1, _, _, s_sn_corr), !.
compIndirect(p, p, []).

compCircs(p, p1, [c_circ_corr|r]) :-
  compCirc(p, p2, c_circ_corr),
  compCircs(p2, p1, r), !.
compCircs(p, p, []).

compCirc(p, p1, [p_prep|s_sn_corr]) :-
  prep(p, p2, p_prep),
  sn(p2, p1, _, _, s_sn_corr), !.

// si g_genre et n_nombre sont libres au temps d'activation du but adjs, on obtient l'implantation de la 
// contrainte faible et la liste des adjs. corrigés sera ignorée. Si les deux arg. sont instanciés au temps 
// d'appel, on obtient alors l'implantation de la vérification et correction de la phrase.
adjs(p, p1, g_genre, n_nombre, [a|r]) :-
  adj(p, p2, g_genre, n_nombre, a),
  adjs(p2, p1, g_genre, n_nombre, r), !.
adjs(p, p, _, _, []).

art(a, g_genre, n_nombre, a_art_corr) :-
  art(g1_genre, n1_nombre, a),
  or(art(g_genre, n_nombre, a),
     art(g_genre, n_nombre, a1_art_corr)),
  corrige(a, a1_art_corr, a_art_corr), !.

nom([v_nom|p], p, g_genre, sing, v_nom) :-
  nom_racine(v_nom, g_genre, v_term_plur), !.
nom([v_nom|p], p, g_genre, plur, v_nom_cor) :-
  nom_racine(v1_nom_racine, g_genre, v_term_plur),
  findPattern(v_nom, v1_nom_racine, 1),
  conc_string(v1_nom_racine, v_term_plur, v1_nom),
  corrige(v_nom, v1_nom, v_nom_cor), !.

verbeEtat([v_verbEtat|p], p, n_nombre, p_personne, v_etat_cor) :-
  verbEtre(v_verbEtat, n, p1),
  verbEtre(v_verbEtat_cor, n_nombre, p_personne),
  corrige(v_verbEtat, v_verbEtat_cor, v_etat_cor), !. 

verbe(p, p2, n_nombre, p_personne, v_verb_cor) :-
  verb_adj(p, p2, n_nombre, p_personne, v_verb_cor, verb, term_verb).

adj(p, p2, g_genre, n_nombre, a_cor) :-
  verb_adj(p, p2, n_nombre, g_genre, a_cor, adj, term_adj).

verb_adj([m|p], p, n_nombre, g_pers_genre, v_mot_cor, v_fait_racine, v_fait_term) :-
  analyseMot(m, v_racine, v_grp_typ, v_fait_racine, v_fait_term),
  motCorrect(v_racine, v_grp_typ, n_nombre, g_pers_genre, v1_mot_cor, v_fait_term),
  corrige(m, v1_mot_cor, v_mot_cor), !.

pr_pers_sj([m|p], p, g_genre, n_nombre, p_personne, m) :-
  prPerSj(g_genre, n_nombre, p_personne, m).


analyseMot(m, v_racine, v_grp_typ, v_fait_racine, v_fait_term) :-
  list2Term([v_fait_racine, v_grp_typ, v_racine], t),
  t,
  conc_string1(v_racine, v_term, m),
  list2Term([v_fait_term, v_grp_typ, n_nombr, p, v_term], t1),
  t1, !.
analyseMot(m, v_racine, v_grp_typ, v_fait_racine, v_fait_term) :-
  list2Term([v_fait_racine, v_grp_typ, v_racine], t),
  t,
  conc_string1(v_racine, v_term, m), !.

motCorrect(v_racine, v_grp_typ, n_nombre, g_pers_genre, v_mot_cor, v_fait_term) :-
  list2Term([v_fait_term, v_grp_typ, n_nombre, g_pers_genre, v_term], t),
  t,
  conc_string(v_racine, v_term, v_mot_cor), !.

corrige(v_mot, v_mot, v_mot) :- !.
corrige(v_mot, v1_mot, corr(v1_mot)). 
  // string_ident(v1_mot, v2_mot), !.


imprime_phrase([[]|p]) :- imprime_phrase(p), !.
imprime_phrase([corr(m)|p]) :-
  // isIdent(m),
  write("*"), write(m), write("* "),
  imprime_phrase(p), !.
imprime_phrase([[a|r]|p]) :-
  imprime_phrase([a|r]),
  imprime_phrase(p), !.
imprime_phrase([m|p]) :-
  //isString(m),
  write(m), write(" "),
  imprime_phrase(p), !.
imprime_phrase([]) :- !.
imprime_phrase(m) :- write(m), write(" ").

// les terminaisons des verbes et des adjectifs
// term_verb(Groupe, Nombre, Personne, Terminaison)
term_verb(1, sing, 1, "e").
term_verb(1, sing, 2, "es").
term_verb(1, sing, 3, "e").
term_verb(1, plur, 1, "ons").
term_verb(1, plur, 2, "ez").
term_verb(1, plur, 3, "ent").
term_verb(2, sing, 1, "is").
term_verb(2, sing, 2, "is").
term_verb(2, sing, 3, "it").
term_verb(2, plur, 1, "issons").
term_verb(2, plur, 2, "issez").
term_verb(2, plur, 3, "issent").
term_verb(3, sing, 1, "").
term_verb(3, sing, 2, "s").
term_verb(3, sing, 3, "").
term_verb(3, plur, 1, "ons").
term_verb(3, plur, 2, "ez").
term_verb(3, plur, 3, "ent").

//term_adj(Type, Nombre, Genre, Terminaison)
term_adj(1, sing, masc, "").
term_adj(1, sing, fem, "e").
term_adj(1, plur, masc, "s").
term_adj(1, plur, fem, "es").
term_adj(2, sing, x, "").
term_adj(2, plur, x, "s").
term_adj(3, x, masc, "x").
term_adj(3, sing, fem, "se").
term_adj(3, plur, fem, "ses").

art(masc, sing, "le").
art(fem, sing, "la").
art(masc, sing, "un").
art(x, plur, "les").
art(fem, sing, "une").
art(x, plur, "des").


verbEtre("suis", sing, 1).
verbEtre("es", sing, 2).
verbEtre("est", sing, 3).
verbEtre("sommes", plur, 1).
verbEtre("etes", plur, 2).
verbEtre("sont", plur, 3).


verb(1, "march").
verb(1, "emport").
verb(1, "mang").
verb(1, "consol").
verb(1, "port").
verb(2, "grand").
verb(2, "chois").
verb(3, "tend").
verb(3, "perd").
verb(3, "apprend").


adj(1, "grand").
adj(1, "petit").
adj(1, "joli").
adj(1, "vert").
adj(1, "noir").
adj(1, "blond").
adj(1, "fier").
adj(2, "pauvre").
adj(2, "fragile").
adj(3, "heureu").
adj(3, "courageu").

nom_racine("champion", masc, "s").
nom_racine("exploit", masc, "s").
nom_racine("pied", masc, "s").
nom_racine("plage", fem, "s").
nom_racine("sable", masc, "s").
nom_racine("sourire", masc, "s").
nom_racine("femme", fem, "s").
nom_racine("homme", masc, "s").
nom_racine("chat", masc, "s").
nom_racine("patience", fem, "s").
nom_racine("natation", fem, "s").
nom_racine("cheveu", masc, "x").
nom_racine("chapeau", masc, "x").
nom_racine("cadeau", masc, "x").
nom_racine("mer", fem, "s").

prep([p|r], r, p) :-
  membre(p, ["dans", "par", "avec", "sur", "sous", "devant", "derriere", "avant", "apres", "depuis"]).
 
prPerSj(x, sing, 1, "je").
prPerSj(x, sing, 2, "tu").
prPerSj(masc, sing, 3, "il").
prPerSj(fem, sing, 3, "elle").
prPerSj(x, plur, 1, "nous").
prPerSj(x, plur, 2, "vous").
prPerSj(masc, plur, 3, "ils").
prPerSj(fem, plur, 3, "elles").

findPattern(v_nom, v1_nom_racine, 1) :-
  v is v_nom:startsWith(v1_nom_racine),
  checkTrue(v).

checkTrue(true).

conc_string(v1_nom_racine, v_term_plur, v1_nom) :-
  v1_nom is v1_nom_racine:concat(v_term_plur).

conc_string1(v1_nom_racine, v_term_plur, v1_nom) :-
  v is v1_nom:startsWith(v1_nom_racine),
  checkTrue(v),
  v_size is v1_nom_racine:length(),
  v_term_plur is v1_nom:substring(v_size), !.
  //v1_nom is v1_nom_racine:concat(v_term_plur).

// string_ident(v1_mot, v2_mot) :-

// isString(m) :-

// isIdent(m) :-

dessineArbre(p) :-
  "javaMethods.CMethods":dessineArbre(p), !.