:- expects_dialect(lps).

insere_ordenado(N, [], [N]).
      insere_ordenado(N, [P | R], [N, P | R]) :-
          N < P.
      insere_ordenado(N, [P | R], [P | Temp]) :-
          N >= P,
          insere_ordenado(N, R, Temp).