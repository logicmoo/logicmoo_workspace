:- expects_dialect(lps).

maxTime(10).

actions say(_, _), say_1(_, _), valid(_), show(_).

input([l, l, l, l], [r, r, r, r]).

crossing([l, X, Y, Z], [r, X, Y, Z], farmer_cross).
crossing([r, X, Y, Z], [l, X, Y, Z], farmer_back).

crossing([l, l, Y, Z], [r, r, Y, Z], fox_cross).
crossing([r, r, Y, Z], [l, l, Y, Z], fox_back).

crossing([l, Y, l, Z], [r, Y, r, Z], goose_cross).
crossing([r, Y, r, Z], [l, Y, l, Z], goose_back).

crossing([l, Y, Z, l], [r, Y, Z, r], beans_cross).
crossing([r, Y, Z, r], [l, Y, Z, l], beans_back).

if input(Start, End) then
    river(Start, End, [Start], P) from T1 to T2,
    show(P).

river(A,A,_,[]) from T to T.

river(A, B, V, P) from T1 to T3 if
    crossing(A, C, Action),
    \+ member(C, V),
    valid(C) from T1 to T2,
    say(Action, C) from T1 to T2,
    river(C, B, [C|V], Plan) from T2 to T3,
    P = [Action | Plan].

false valid([A, B, B, C]), A \= B.
false valid([A, C, B, B]), A \= B.
false valid(X), valid(Y), X \= Y.