c(nixon).

head(nr, republican(nixon)).

head(nq, quaker(nixon)).

def(rnp).
head(rnp, neg(pacifist(nixon))).
body(rnp, republican(nixon)).

def(qp).
head(qp, pacifist(nixon)).
body(qp, quaker(nixon)).
