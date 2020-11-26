


pred(ClNum,Prd):- clauzes(P,ClNum),functor(P,Prd,_).
arg1(ClNum,Arg):- clauzes(P,ClNum),arg(1,P,Arg).
arg2(ClNum,Arg):- clauzes(P,ClNum),arg(2,P,Arg).
arg3(ClNum,Arg):- clauzes(P,ClNum),arg(3,P,Arg).

clauzes(P,ClNum):- arity_was(F,A),functor(P,F,A),clause(P,true,ClNum).


