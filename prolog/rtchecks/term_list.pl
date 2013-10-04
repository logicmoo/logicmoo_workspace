:- module(term_list, [eq/1, push_term/3, push_meta/3, collapse_terms/3],
	  [assertions, regtypes]).

:- use_module(library(lists)).

:- regtype eq/1.

eq(_=_).

:- pred push_term/3 :: term * list(eq) * term.

push_term(E, L, V) :-
	member(T=V, L),
	(T==E ; var(T), T = E),
	!.

:- if(current_prolog_flag(dialect, ciao)).
push_meta(E, L, V) :-
	member('$meta$rtc'(T, V), L),
	(T==E ; var(T), T = E),
	!.
:- else.
push_meta(E, L, V) :-
    '$set_source_module'(M, M),
    ( predicate_property(M:E, imported_from(IM)) ->
      true
    ; IM = M
    ),
    push_term(IM:E, L, V).
:- endif.

collapse_terms(G, L0, L) :-
	count_vars(G, [], C),
	collapse_term(L0, C, L, []).

collapse_term([],       _,  R,  R).
collapse_term([E|L0], C0, R0, R) :-
	!,
	(
	    E = (T=V),
	    (
		(
		    member(V0=N, C0),
		    V0==V ->
		    (
			(N==1 ; atomic(T)) ->
			 T=V,
			 R0 = R1
		    ;
			R0 = [T=V|R1]
		    ),
		    count_vars(T, C0, C)
		;
		    R0 = R1,
		    C = C0
		)
	    ) -> true
	;
	    R0 = [E|R1],
	    C = C0
	),
	collapse_term(L0, C, R1, R).

count_vars(Term, C0, C) :-
	var(Term),
	!,
	(
	    select(T=N, C0, C1),
	    T==Term ->
	    N1 is N + 1,
	    C=[Term=N1|C1]
	;
	    C=[Term=1|C0]
	).
count_vars(Arg, C0, C) :-
	compound(Arg),
	!,
	count_var(1, Arg, C0, C).
count_vars(_, C, C).

count_var(N, Term, C0, C) :-
	arg(N, Term, Arg),
	!,
	count_vars(Arg, C0, C1),
	N1 is N + 1,
	count_var(N1, Term, C1, C).
count_var(_, _, C, C).
