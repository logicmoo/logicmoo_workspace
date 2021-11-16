c(named("Jacob")).
c("Jacob").
c(named("Jones")).
c("Jones").
c(named("Paul")).
c("Paul").
c(named).
c(na).
c(eq).
c(1).
c(person).
c(countable).
c(pos).
c(reliable).
c(gunman).
c(have).
c(moustache).


head(jn,object(named("Jacob"),"Jacob",named,na,eq,1)).
head(jp,object(named("Jacob"),person,countable,na,eq,1)).
head(on,object(named("Jones"),"Jones",named,na,eq,1)).
head(op,object(named("Jones"),person,countable,na,eq,1)).
head(pn,object(named("Paul"),"Paul",named,na,eq,1)).
head(pp,object(named("Paul"),person,countable,na,eq,1)).


head(pjr,neg(group(xxx_list(property(named("Jones"),reliable,pos))))).
body(pjr,group(xxx_list(property(named("Paul"),reliable,pos)))).


def(pr(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).
head(pr(A,B,C,D,E,F),group(xxx_list(property(A,reliable,pos)))):-c(A),c(B),c(C),c(D),c(E),c(F).
body(pr(A,B,C,D,E,F),object(A,B,C,D,E,F)):-c(A),c(B),c(C),c(D),c(E),c(F).
body(pr(A,B,C,D,E,F),object(A,person,countable,na,eq,1)):-c(A),c(B),c(C),c(D),c(E),c(F).



head(jjr,group(xxx_list(property(named("Jones"),reliable,pos)))).
body(jjr,group(xxx_list(property(named("Jacob"),reliable,pos)))).


head(mou,group(xxx_list(pred_mod(have,gv(0),gv(1),xxx_emptylist),object(gv(0),gunman,countable,na,eq,1),object(g(1),moustache,countable,na,eq,1)))).
body(mou,xxx_list(group(xxx_list(property(named("Jones"),reliable,pos))))).















