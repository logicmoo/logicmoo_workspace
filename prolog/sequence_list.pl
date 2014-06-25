:- module(sequence_list, [sequence_list/3]).

sequence_list(V) --> {var(V)}, !, [V].
sequence_list([]) --> !, [].
sequence_list([A|B]) --> !,
    sequence_list(A),
    sequence_list(B).
sequence_list((A, B)) --> !,
    sequence_list(A),
    sequence_list(B).
sequence_list(A) --> [A].
