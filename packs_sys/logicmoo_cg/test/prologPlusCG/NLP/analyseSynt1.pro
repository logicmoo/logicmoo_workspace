phrase(p) :-
  gn(p, p1),
  gv(p1, ["."]).

gn(p, p1) :-
  art(p, p0),
  adj(p0, [n|p1]),
  nom(n).

gv([v|r], p1) :-
  verbe(v),
  complement(r, p1).

complement(p, p1) :-
  preposition(p, r),
  gn(r, p1).
complement(p, p).

art([a|p], p) :-
  art(a).
art(p, p).

adj([a|p], p) :-
  adj(a).
adj(p, p).

preposition([a|p], p) :-
  prep(a).
preposition(p, p).

art("le").
art("la").

nom("moh").
nom("maison").

adj("belle").
adj("grand").

prep("a").
prep("dans").

verbe("habite").
verbe("mange").
