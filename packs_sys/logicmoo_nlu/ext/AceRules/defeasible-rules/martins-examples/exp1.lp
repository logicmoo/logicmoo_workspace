c(object(named("Tux"),"Tux",named,na,eq,1)).
c(object(named("Tweety"),"Tweety",named,na,eq,1)).

group(xxx_list(object(named("Tweety"),bird,countable,na,eq,1))).


object(named("Tux"),penguin,countable,na,eq,1).

body(bf(object(A,B,C,D,E,F)),c(object(A,B,C,D,E,F))):-c(object(A,B,C,D,E,F)).
body(bf(object(A,B,C,D,E,F)),c(object(A,penguin,countable,na,eq,1))):-c(object(A,B,C,D,E,F)).
head(bf(X),-group(xxx_list(pred_mod(fly,A,xxx_list())))):-c(X). 
