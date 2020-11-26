analyse :-
  readSentence(s),
  phrase(s, p),
  writeln(p),
  dessineArbre(p).

phrase(s, ph(v_gn, v_gv)) :-
  gn(s, p1, v_gn),
  gv(p1, ["."], v_gv), !.

gn(p, p1, gn(s)) :- sn(p, p1, s), !.
gn(p, p1, gn(s)) :- pr_pers_sj(p, p1, s).

gv(p, p1, t) :-
  verbe(p, p2, v),
  compDirect(p2, p3, c_dir),
  compIndirect(p3, p4, c_ind),
  compCircs(p4, p1, c_circs),
  elimine_listesVide([v, c_dir, c_ind, c_circs], l),
  list2Term([gv|l], t), !.
gv(p, p1, t) :-
  verbeEtat(p, p2, v_etat),
  attribut(p2, p3, v_attr),
  compCircs(p3, p1, c_circs),
  elimine_listesVide([v_etat, v_attr, c_circs], l),
  list2Term([gv|l], t), !.

sn(p, p1, t) :-
  art(p, p2, a),
  adjs(p2, p3, l_adjs1),
  nom(p3, p4, n),
  adjs(p4, p5, l_adjs2),
  comp2Nom(p5, p1, c2Nom),
  elimine_listesVide([a, l_adjs1, n, l_adjs2, c2Nom], l), list2Term([sn|l], t1),
  t = t1, !.

comp2Nom(["de"|p], p1, comp2Nom("de", s)) :-
  sn(p, p1, s), !.
comp2Nom(p, p, []).

compDirect(p, p1, compDir(s)) :-
  sn(p, p1, s), !.
compDirect(p, p, []).

attribut(p, p1, attr(a)) :- adj(p, p1, a), !.
attribut(p, p1, attr(s)) :- sn(p, p1, s), !.
attribut(p, p, []).

compIndirect(["à"|p], p1, compInd("à",s)) :-
  sn(p, p1, s), !.
compIndirect(p, p, []).

compCircs(p, p1, t) :-
  compCircs1(p, p1, l_ccircs),
  dif(l_ccircs, []),
  list2Term([compCircs|l_ccircs], t1),
  t1 = t, !.
compCircs(p, p, []).

compCircs1(p, p1, [c_circ|r]) :-
  compCirc(p, p2, c_circ),
  compCircs1(p2, p1, r), !.
compCircs1(p, p, []).

compCirc(p, p1, compCirc(p_prep, s)) :-
  prep(p, p2, p_prep),
  sn(p2, p1, s), !.

adjs(p, p1, t) :-
  adjs1(p, p1, l_adjs),
  dif(l_adjs, []),
  list2Term([adjs|l_adjs], t), !.
adjs(p, p, []).

adjs1(p, p1, [a|r]) :-
  adj(p, p2, a),
  adjs1(p2, p1, r), !.
adjs1(p, p, []).


elimine_listesVide([[]|r], s) :-
  elimine_listesVide(r, s), !.
elimine_listesVide([x|r], [x|s]) :-
  elimine_listesVide(r, s), !.
elimine_listesVide([], []).

// dictionnaire
adj([a|p], p, adj(a)) :-
  membre(a, ["grand", "grande", "brun", "brune", "petit", "petite", "vite", "agreable", "docile", "facile",
	    "rouge", "charmante"]).

verbe([v|p], p, verb(v)) :-
  membre(v, ["aime", "court", "mange", "regarde"]).

verbeEtat(["est"|p], p, verbEtat("est")).

pr_pers_sj([p|r], r, prSj(p)) :-
  membre(p, ["je", "tu", "il", "elle", "nous", "vous", "ils", "elles"]).

prep([p|r], r, prep(p)) :-
  membre(p, ["dans", "par", "avec", "sur", "sous", "devant", "derriere", "avant", "apres", "depuis"]).

art([a|r], r, art(a)) :-
  membre(a, ["le", "la", "les", "un", "une", "des"]).

nom([n|r], r, nom(n)) :-
  membre(n, ["femme", "homme", "fille", "maison", "chien", "oiseau", "souris", "mer", "fourchette", "pomme"]).

dessineArbre(p) :-
  "javaMethods.CMethods":dessineArbre(p), !.