factorial(N) <- 1 if N =< 0.
factorial(N) <- N * factorial(N-1) if N > 0.

count( [] ) <- 0.
count( [_|T] ) <- 1 + count(T).
