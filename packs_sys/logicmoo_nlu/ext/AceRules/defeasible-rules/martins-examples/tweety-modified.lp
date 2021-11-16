c(fly).
c(penguin).
c(countable).
c(na).
c(eq).
c(1).
c(bird).
c(named("Tweety")).
c(bird).
c(named("Tux")).
c(penguin).
c("Tux").
c("Tweety").
c(named).



head(pnf(A),neg(group(xxx_list(pred_mod(fly,A,xxx_emptylist))))):-c(A).
%body(pnf(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).
body(pnf(A),object(A,penguin,countable,na,eq,1)):-c(A).


head(pb(A),group(xxx_list(object(A,bird,countable,na,eq,1)))):-c(A).
%body(pb(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).
body(pb(A),object(A,penguin,countable,na,eq,1)):-c(A).


head(tb,group(xxx_list(object(named("Tweety"),bird,countable,na,eq,1)))).



head(xn,object(named("Tux"),"Tux",named,na,eq,1)).


head(xp,object(named("Tux"),penguin,countable,na,eq,1)).

head(tn,object(named("Tweety"),"Tweety",named,na,eq,1)).



def(bf(A)):-c(A).
head(bf(A),group(xxx_list(pred_mod(fly,A,xxx_emptylist)))):-c(A).
body(bf(A),group(xxx_list(object(A,bird,countable,na,eq,1)))):-c(A).
%body(bf(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).







