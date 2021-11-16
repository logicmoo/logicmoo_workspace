c(tweety).
c(tux).

def(bf(C)) :- c(C).
body(bf(C), bird(C)) :- c(C).
head(bf(C), flies(C)) :- c(C).

body(pn(C), penguin(C)) :- c(C).
head(pn(C), neg(flies(C))) :- c(C).

head(twb, bird(tweety)).
head(twp, penguin(tweety)).

head(tup, penguin(tux)).
