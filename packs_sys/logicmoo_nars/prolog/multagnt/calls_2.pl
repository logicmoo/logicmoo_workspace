?-['teacher'].
?-['learner2'].

?-cannot_do(Tl,Tt,parent(warren,catherine),C).
?-cannot_do(Tl,Tt,parent(warren,david),C).
?-cannot_do(Tl,Tt,parent(warren,variable(child)),C).
?-can_do(Tl,Tt,parent(variable(parent),variable(child)),C).

?-cannot_do(Tl,Tt,entropy_increases(variable(a),variable(b)),C).

?-cannot_do(Tl,Tt,t_member(f,[g,f,d,s,a]),C).
?-cannot_do(Tl,Tt,t_member(e,[q,w,e,r,t,z,t,r,e,w,q]),C).
?-cannot_do(Tl,Tt,t_member(variable(a),[g,f,d,s,a]),C).
?-can_do(Tl,Tt,t_member(variable(a),variable(list)),C).

?-cannot_do(Tl,Tt,reverse([c,b,a],variable(c)),C).
?-cannot_do(Tl,Tt,reverse([a,b,c],[c,b,a]),C).

?-what_cannot_do(Ls,Ts,Q <- Ans,[],F).
?-what_cannot_do(Ls,Ts,qsort([1,2],variable(a)) <- Ans,[],F).
?-what_cannot_do(Ls,Ts,qsort([2,1],variable(a)) <- Ans,[],F).
?-what_cannot_do(Ls,Ts,qsort([2,3,1],variable(a)) <- Ans,[],F).
?-what_cannot_do(Ls,Ts,qsort([3,2,1],variable(a)) <- Ans,[],F).
