c(named("Nixon")).
c("Nixon").
c(named).
c(na).
c(eq).
c(1).
c(quaker).
c(republican).
c(countable).


head(nn,object(named("Nixon"),"Nixon",named,na,eq,1)).

head(nq,object(named("Nixon"),quaker,countable,na,eq,1)).

head(nr,object(named("Nixon"),republican,countable,na,eq,1)).


def(rp(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).

head(rp(A,B,C,D,E,F),neg(group(xxx_list(object(A,pacifist,countable,na,eq,1))))):-c(A),c(B),c(C),c(D),c(E),c(F).

body(rp(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).

body(rp(A,B,C,D,E,F),object(A,republican,countable,na,eq,1)):-c(A),c(B),c(C),c(D),c(E),c(F).


def(qp(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).

head(qp(A,B,C,D,E,F),group(xxx_list(object(A,pacifist,countable,na,eq,1)))):-c(A),c(B),c(C),c(D),c(E),c(F).

body(qp(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).

body(qp(A,B,C,D,E,F),object(A,quaker,countable,na,eq,1)):-c(A),c(B),c(C),c(D),c(E),c(F).
