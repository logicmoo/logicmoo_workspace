f(0) <- 1.

f(N) <- N*f(N-1).


join( [], L ) <- L.

join( [H|T], L ) <- [ H | join(T,L) ].


sum( [] ) <- 0.

sum( [H|T] ) <- H + sum(T).


sum1( L ) <- 0 if L = [].

sum1( [H|T] ) <- H + sum1(T).


twist( A,B,C) <- A+B-C.


small( P ) if f(P) < 24.
