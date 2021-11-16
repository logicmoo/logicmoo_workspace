c(C):-object(C,_,_,_,_,_).
c(C):-object(_,C,_,_,_,_).
c(C):-object(_,_,C,_,_,_).
c(C):-object(_,_,_,C,_,_).
c(C):-object(_,_,_,_,C,_).
c(C):-object(_,_,_,_,_,C).

object(named("Tweety"),"Tweety",named,na,eq,1).
object(named("Tweety"),bird,countable,na,eq,1).



head(tt(A,B,C,D,E,F),object(named("Tweety"),"Tweety",named,na,eq,1)):-c(A),c(B),c(C),c(D),c(E),c(F).
head(tb(A,B,C,D,E,F),object(named("Tweety"),bird,countable,na,eq,1)):-c(A),c(B),c(C),c(D),c(E),c(F).


def(bf(A,B,C,D,E,F)) :- c(A),c(B),c(C),c(D),c(E),c(F).
body(bf(A,B,C,D,E,F), object(A,bird,countable,na,eq,1)) :- c(A),c(B),c(C),c(D),c(E),c(F).
head(bf(A,bird,C,D,E,F), group(xxx_list(pred_mod(fly,A,xxx_emptylist)))) :- c(A),c(B),c(C),c(D),c(E),c(F).

