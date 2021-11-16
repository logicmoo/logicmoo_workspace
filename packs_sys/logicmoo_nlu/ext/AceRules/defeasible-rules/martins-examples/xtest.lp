c("Tweety").
c("Tux").
c(group(xxx_list(object(named("Tweety"),bird,countable,na,eq,1)))).
c(eq).
c(object(named("Tux"),"Tux",named,na,eq,1)).
c(xxx_list()).
c(xxx_list(object(named("Tweety"),bird,countable,na,eq,1))).
c(named).
c(bird).
c(fly).
c(object(named("Tweety"),"Tweety",named,na,eq,1)).
c(named("Tweety")).
c(object(named("Tux"),penguin,countable,na,eq,1)).
c(named("Tux")).
c(countable).
c(na).
c(object(named("Tweety"),bird,countable,na,eq,1)).
c(penguin).

head(xxx_sr1(A),group(xxx_list(object(A,bird,countable,na,eq,1)))):-c(A).
body(xxx_sr1(A),object(A,penguin,countable,na,eq,1)):-c(A).

head(xxx_sr2,object(named("Tux"),penguin,countable,na,eq,1)).

head(xxx_sr3(A),neg(group(xxx_list(pred_mod(fly,A,xxx_list()))))):-c(A).
body(xxx_sr3(A),object(A,penguin,countable,na,eq,1)):-c(A).

head(xxx_sr4,object(named("Tux"),"Tux",named,na,eq,1)).

head(xxx_sr5,object(named("Tweety"),"Tweety",named,na,eq,1)).

head(xxx_sr6,group(xxx_list(object(named("Tweety"),bird,countable,na,eq,1)))).

def(xxx_dr1(A)):-c(A).
head(xxx_dr1(A),group(xxx_list(pred_mod(fly,A,xxx_list())))):-c(A).
body(xxx_dr1(A),group(xxx_list(object(A,bird,countable,na,eq,1)))):-c(A).


