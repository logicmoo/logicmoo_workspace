:- module(normalize_head, [normalize_head/2]).

:- use_module(library(implementation_module)).

current_predicate_ext(M:F/A) :-
    ( nonvar(M) ->
      findall(M:F/A, current_predicate(M:F/A), PIL0)
    ; findall(M:F/A, (current_predicate(CM:F/A),
		      functor(H, F, A),
		      implementation_module(CM:H, M)), PIL0)
    ),
    sort(PIL0, PIL),
    member(M:F/A, PIL),
    current_predicate(M:F/A).

:- meta_predicate normalize_head(?, ?).
normalize_head(P,     P)   :- var(P), !.
normalize_head(M:P,   M:P) :- var(P), !.
normalize_head(M:F/A, M:H) :- !, normalize_head_from_pi(M, F, A, H).
normalize_head(P,     MH) :-
    ( P = F/A *->
      MH = M:H,
      normalize_head_from_pi(M, F, A, H)
    ; MH = P
    ).

normalize_head_from_pi(M, F, A, H) :-
    ( atom(F), integer(A) -> true
    ; current_predicate_ext(M:F/A)
    ),
    functor(H, F, A).
