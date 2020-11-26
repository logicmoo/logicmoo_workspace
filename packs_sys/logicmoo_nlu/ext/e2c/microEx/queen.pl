 queen(N,R) :- range(1,N,Ns), queens(Ns,[],R).

 range(N,N,[N]) :- !.
 range(M,N,[M|Ns]) :- M1 is M+1, range(M1,N,Ns).

 queens([],R,R).
 queens(Unplaced,Safe,R) :- del(Q,Unplaced,U1),
     not(attack(Q,1,Safe)),
     queens(U1,[Q|Safe],R).

del(X,[X|R],R).
del(X,[Y|R],[Y|R1]) :- del(X,R,R1).

 attack(X,N,[Y|_]) :- X is Y+N.
 attack(X,N,[Y|_]) :- Y is X+N.
 attack(X,N,[_|Ys]) :- N1 is N+1, attack(X,N1,Ys).

 test :- queen(8,R), write(R), nl, fail.
 test.

 go :- X is cputime, test, R is cputime-X, write(R).
