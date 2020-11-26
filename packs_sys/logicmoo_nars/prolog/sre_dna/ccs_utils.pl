trace_append(R, S, T) :- tappend(R, S, T).
trace_append(R, S, T) :- tappend(S, R, T).

tappend([], X, X).
tappend([A|R], [B|S], T) :-
	opposite(A, B),
	!,
	trace_append(R, S, T).
tappend([A|R], [B|S], [A|T]) :-
	trace_append(R, [B|S], T).

opposite(X, -X).
opposite(-X, X).


stripped(-(A), A) :- !.
stripped(A, A).
