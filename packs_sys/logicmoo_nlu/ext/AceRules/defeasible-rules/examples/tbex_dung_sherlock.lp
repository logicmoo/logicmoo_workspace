person(p1).
person(p2).
person(s).

head(i(P), neg(inno(P))) :- person(P).
body(i(P), inno(Q)) :- person(P), person(Q), P != Q.

def(di(P)) :- person(P).
head(di(P), inno(P)) :- person(P).

head(r, inno(s)).
