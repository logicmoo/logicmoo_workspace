
 conc([],X,X).
 conc([T|Q],L,[T|R]) :- conc(Q,L,R).

 nrev([],[]).
 nrev([T|Q],R) :- nrev(Q,R1), conc(R1,[T],R).
 bench :- 
   X is cputime,
   nrev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c,d,e,f,g,h,i,j,k,l,m,o,p,q,r],R),
   Y is cputime,
   Z is Y - X,
   write('1 Klips en '), write(Z), nl.

