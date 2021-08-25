
or_holds(A, B, C) :-
	D=suspension(F, active, _, 0, user:or_holds___3__0(A, B, C, D), or_holds, A, B, C),
	term_variables(A, H, E),
	term_variables(C, E),
	'chr gen_id'(F),
	nb_getval('$chr_store_global_list_user____or_holds___3', G),
	b_setval('$chr_store_global_list_user____or_holds___3',
		 [D|G]),
	attach_or_holds___3(H, D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(or_holds(A, B, C)#D)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(D)),
		or_holds___3__0(A, B, C, D)
	    ;   'chr debug_event'(fail(D)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(D))
	    ;   'chr debug_event'(redo(D)),
		fail
	    )
	;   or_holds___3__0(A, B, C, D)
	).

or_holds___2__0(G, A, M) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, H, I, F),
	F==A,
	member(K, G),
	copy_fluent(H, I, J, L),
	J=K,
	\+ call(#\+L),
	'chr debug_event'(try([M],
			      [E],
			      (member(O, G), copy_fluent(H, I, N, P), N=O, \+call(#\+P)),
			      true)), !,
	'chr debug_event'(apply([M],
				[E],
				(member(O, G), copy_fluent(H, I, N, P), N=O, \+call(#\+P)),
				true)),
	'chr debug_event'(remove(M)),
	M=suspension(_, _, _, _, _, or_holds, Q, R),
	setarg(2, M, removed),
	term_variables(term(Q, R), U),
	nb_getval('$chr_store_global_list_user____or_holds___2', S),
	'chr sbag_del_element'(S, M, T),
	b_setval('$chr_store_global_list_user____or_holds___2', T),
	detach_or_holds___2(U, M).
or_holds___2__0(G, A, M) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, H, I, F),
	F==A,
	member(K, G, W),
	copy_fluent(H, I, J, L),
	J=K,
	\+ call(#\+L),
	'chr debug_event'(try([M],
			      [E],
			      (member(O, G, Q), copy_fluent(H, I, N, P), N=O, \+call(#\+P)),
			      or_holds(Q, A))), !,
	'chr debug_event'(apply([M],
				[E],
				(member(O, G, Q), copy_fluent(H, I, N, P), N=O, \+call(#\+P)),
				or_holds(Q, A))),
	'chr debug_event'(remove(M)),
	M=suspension(_, _, _, _, _, or_holds, R, S),
	setarg(2, M, removed),
	term_variables(term(R, S), V),
	nb_getval('$chr_store_global_list_user____or_holds___2', T),
	'chr sbag_del_element'(T, M, U),
	b_setval('$chr_store_global_list_user____or_holds___2', U),
	detach_or_holds___2(V, M),
	or_holds(W, A).
or_holds___2__0(A, E, D) :-
	nonvar(A),
	A=[C|B],
	B==[],
	C\=eq(_, _),
	C\=neq(_, _),
	'chr debug_event'(try([D],
			      [],
			      (C\=eq(F, G), C\=neq(H, I)),
			      holds(C, E))), !,
	'chr debug_event'(apply([D],
				[],
				(C\=eq(F, G), C\=neq(H, I)),
				holds(C, E))),
	'chr debug_event'(remove(D)),
	D=suspension(_, _, _, _, _, or_holds, J, K),
	setarg(2, D, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____or_holds___2', L),
	'chr sbag_del_element'(L, D, M),
	b_setval('$chr_store_global_list_user____or_holds___2', M),
	detach_or_holds___2(N, D),
	holds(C, E).
or_holds___2__0(A, _, C) :-
	\+ ( member(B, A),
	     B\=eq(_, _),
	     B\=neq(_, _)
	   ),
	'chr debug_event'(try([C],
			      [],
			      \+ (member(D, A), D\=eq(F, G), D\=neq(H, I)),
			      (or_and_eq(A, E), call(E)))), !,
	'chr debug_event'(apply([C],
				[],
				\+ (member(D, A), D\=eq(F, G), D\=neq(H, I)),
				(or_and_eq(A, E), call(E)))),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, or_holds, J, K),
	setarg(2, C, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____or_holds___2', L),
	'chr sbag_del_element'(L, C, M),
	b_setval('$chr_store_global_list_user____or_holds___2', M),
	detach_or_holds___2(N, C),
	or_and_eq(A, O),
	call(O).
or_holds___2__0(B, A, D) :-
	A==[],
	member(C, B, P),
	C\=eq(_, _),
	C\=neq(_, _),
	'chr debug_event'(try([D],
			      [],
			      (member(E, B, F), E\=eq(G, H), E\=neq(I, J)),
			      or_holds(F, []))), !,
	'chr debug_event'(apply([D],
				[],
				(member(E, B, F), E\=eq(G, H), E\=neq(I, J)),
				or_holds(F, []))),
	'chr debug_event'(remove(D)),
	D=suspension(_, _, _, _, _, or_holds, K, L),
	setarg(2, D, removed),
	term_variables(term(K, L), O),
	nb_getval('$chr_store_global_list_user____or_holds___2', M),
	'chr sbag_del_element'(M, D, N),
	b_setval('$chr_store_global_list_user____or_holds___2', N),
	detach_or_holds___2(O, D),
	or_holds(P, []).
or_holds___2__0(A, _, E) :-
	member(eq(B, C), A),
	or_neq(exists, B, C, D),
	\+ call(D),
	'chr debug_event'(try([E],
			      [],
			      (member(eq(F, G), A), or_neq(exists, F, G, H), \+call(H)),
			      true)), !,
	'chr debug_event'(apply([E],
				[],
				(member(eq(F, G), A), or_neq(exists, F, G, H), \+call(H)),
				true)),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, or_holds, I, J),
	setarg(2, E, removed),
	term_variables(term(I, J), M),
	nb_getval('$chr_store_global_list_user____or_holds___2', K),
	'chr sbag_del_element'(K, E, L),
	b_setval('$chr_store_global_list_user____or_holds___2', L),
	detach_or_holds___2(M, E).
or_holds___2__0(A, _, E) :-
	member(neq(B, C), A),
	and_eq(B, C, D),
	\+ call(D),
	'chr debug_event'(try([E],
			      [],
			      (member(neq(F, G), A), and_eq(F, G, H), \+call(H)),
			      true)), !,
	'chr debug_event'(apply([E],
				[],
				(member(neq(F, G), A), and_eq(F, G, H), \+call(H)),
				true)),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, or_holds, I, J),
	setarg(2, E, removed),
	term_variables(term(I, J), M),
	nb_getval('$chr_store_global_list_user____or_holds___2', K),
	'chr sbag_del_element'(K, E, L),
	b_setval('$chr_store_global_list_user____or_holds___2', L),
	detach_or_holds___2(M, E).
or_holds___2__0(A, J, E) :-
	member(eq(B, C), A, P),
	\+ ( and_eq(B, C, D),
	     call(D)
	   ),
	'chr debug_event'(try([E],
			      [],
			      (member(eq(F, G), A, I), \+ (and_eq(F, G, H), call(H))),
			      or_holds(I, J))), !,
	'chr debug_event'(apply([E],
				[],
				(member(eq(F, G), A, I), \+ (and_eq(F, G, H), call(H))),
				or_holds(I, J))),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, or_holds, K, L),
	setarg(2, E, removed),
	term_variables(term(K, L), O),
	nb_getval('$chr_store_global_list_user____or_holds___2', M),
	'chr sbag_del_element'(M, E, N),
	b_setval('$chr_store_global_list_user____or_holds___2', N),
	detach_or_holds___2(O, E),
	or_holds(P, J).
or_holds___2__0(A, J, E) :-
	member(neq(B, C), A, P),
	\+ ( or_neq(exists, B, C, D),
	     call(D)
	   ),
	'chr debug_event'(try([E],
			      [],
			      (member(neq(F, G), A, I), \+ (or_neq(exists, F, G, H), call(H))),
			      or_holds(I, J))), !,
	'chr debug_event'(apply([E],
				[],
				(member(neq(F, G), A, I), \+ (or_neq(exists, F, G, H), call(H))),
				or_holds(I, J))),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, or_holds, K, L),
	setarg(2, E, removed),
	term_variables(term(K, L), O),
	nb_getval('$chr_store_global_list_user____or_holds___2', M),
	'chr sbag_del_element'(M, E, N),
	b_setval('$chr_store_global_list_user____or_holds___2', N),
	detach_or_holds___2(O, E),
	or_holds(P, J).
or_holds___2__0(C, A, B) :-
	nonvar(A),
	A=[D|E],
	'chr debug_event'(try([B],
			      [],
			      true,
			      or_holds(C, [], [D|E]))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				or_holds(C, [], [D|E]))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, or_holds, F, G),
	setarg(2, B, removed),
	term_variables(term(F, G), J),
	nb_getval('$chr_store_global_list_user____or_holds___2', H),
	'chr sbag_del_element'(H, B, I),
	b_setval('$chr_store_global_list_user____or_holds___2', I),
	detach_or_holds___2(J, B),
	or_holds(C, [], [D|E]).
or_holds___2__0(G, A, J) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, H, F),
	F==A,
	member(I, G, R),
	H==I,
	'chr debug_event'(try([J],
			      [E],
			      (member(K, G, L), H==K),
			      or_holds(L, A))), !,
	'chr debug_event'(apply([J],
				[E],
				(member(K, G, L), H==K),
				or_holds(L, A))),
	'chr debug_event'(remove(J)),
	J=suspension(_, _, _, _, _, or_holds, M, N),
	setarg(2, J, removed),
	term_variables(term(M, N), Q),
	nb_getval('$chr_store_global_list_user____or_holds___2', O),
	'chr sbag_del_element'(O, J, P),
	b_setval('$chr_store_global_list_user____or_holds___2', P),
	detach_or_holds___2(Q, J),
	or_holds(R, A).
or_holds___2__0(G, A, J) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, _, _, D, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, H, F),
	F==A,
	member(I, G),
	\+ H\=I,
	'chr debug_event'(try([J],
			      [E],
			      (member(K, G), \+H\=K),
			      true)), !,
	'chr debug_event'(apply([J],
				[E],
				(member(K, G), \+H\=K),
				true)),
	'chr debug_event'(remove(J)),
	J=suspension(_, _, _, _, _, or_holds, L, M),
	setarg(2, J, removed),
	term_variables(term(L, M), P),
	nb_getval('$chr_store_global_list_user____or_holds___2', N),
	'chr sbag_del_element'(N, J, O),
	b_setval('$chr_store_global_list_user____or_holds___2', O),
	detach_or_holds___2(P, J).
or_holds___2__0(_, _, A) :-
	setarg(2, A, active).

detach_not_holds_all___2([], _).
detach_not_holds_all___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, D, I, J, K, L, M, N, O, P, Q, R, S),
	    (   C/\2=:=2
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -3,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   [],
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       F,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_not_holds_all___2(T, E).

all_not_holds___3__0(I, J, A, O) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, H, F),
	F==A,
	copy_fluent(G, H, K, M),
	copy_fluent(I, J, L, N),
	K=L,
	call(M#/\N),
	'chr debug_event'(try([E, O],
			      [],
			      (copy_fluent(G, H, P, R), copy_fluent(I, J, Q, S), P=Q, call(R#/\S)),
			      false)), !,
	'chr debug_event'(apply([E, O],
				[],
				(copy_fluent(G, H, P, R), copy_fluent(I, J, Q, S), P=Q, call(R#/\S)),
				false)),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, all_holds, T, U, V),
	setarg(2, E, removed),
	term_variables(term(T, U, V), Y),
	nb_getval('$chr_store_global_list_user____all_holds___3', W),
	'chr sbag_del_element'(W, E, X),
	b_setval('$chr_store_global_list_user____all_holds___3', X),
	detach_all_holds___3(Y, E),
	'chr debug_event'(remove(O)),
	O=suspension(_, _, _, _, _, all_not_holds, Z, A1, B1),
	setarg(2, O, removed),
	term_variables(term(Z, A1, B1), E1),
	nb_getval('$chr_store_global_list_user____all_not_holds___3', C1),
	'chr sbag_del_element'(C1, O, D1),
	b_setval('$chr_store_global_list_user____all_not_holds___3', D1),
	detach_all_not_holds___3(E1, O),
	false.
all_not_holds___3__0(_, _, A, B) :-
	A==[],
	'chr debug_event'(try([B], [], true, true)), !,
	'chr debug_event'(apply([B], [], true, true)),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, all_not_holds, C, D, E),
	setarg(2, B, removed),
	term_variables(term(C, D, E), H),
	nb_getval('$chr_store_global_list_user____all_not_holds___3', F),
	'chr sbag_del_element'(F, B, G),
	b_setval('$chr_store_global_list_user____all_not_holds___3', G),
	detach_all_not_holds___3(H, B).
all_not_holds___3__0(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	), !,
	all_not_holds___3__0__0__3(D, E, F, A, G).
all_not_holds___3__0(A, B, C, D) :-
	all_not_holds___3__1(A, B, C, D).

all_not_holds___3__1__0__4([], A, B, C, D) :-
	all_not_holds___3__2(A, B, C, D).
all_not_holds___3__1__0__4([A|V], E, F, C, J) :-
	(   A=suspension(_, active, _, _, _, _, D, B),
	    B==C,
	    member(H, D, U),
	    copy_fluent(E, F, G, I),
	    G=H,
	    \+ call(#\+I),
	    'chr debug_event'(try([A],
				  [J],
				  (member(L, O, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				  or_holds(N, C)))
	->  'chr debug_event'(apply([A],
				    [J],
				    (member(L, O, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				    or_holds(N, C))),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, or_holds, P, Q),
	    setarg(2, A, removed),
	    term_variables(term(P, Q), T),
	    nb_getval('$chr_store_global_list_user____or_holds___2', R),
	    'chr sbag_del_element'(R, A, S),
	    b_setval('$chr_store_global_list_user____or_holds___2', S),
	    detach_or_holds___2(T, A),
	    setarg(2, J, active),
	    or_holds(U, C),
	    (   J=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, J, inactive),
		all_not_holds___3__1__0__4(V,
					   E,
					   F,
					   C,
					   J)
	    ;   true
	    )
	;   all_not_holds___3__1__0__4(V,
				       E,
				       F,
				       C,
				       J)
	).

:- dynamic prolog_file_type/2.
:- multifile prolog_file_type/2.

prolog_file_type(pl, prolog).
prolog_file_type(prolog, prolog).
prolog_file_type(qlf, prolog).
prolog_file_type(qlf, qlf).
prolog_file_type(A, executable) :-
	system:current_prolog_flag(shared_object_extension, A).

or_neq(_, [], [], 0#\=0).
or_neq(A, [D|B], [F|C], E) :-
	or_neq(A, B, C, H),
	(   A=forall,
	    var(D),
	    \+ is_domain(D)
	->  (   binding(D, B, C, G)
	    ->  E= (F#\=G#\/H)
	    ;   E=H
	    )
	;   E= (D#\=F#\/H)
	).

if_then_holds(A, B, C) :-
	D=suspension(E, active, _, 0, user:if_then_holds___3__0(A, B, C, D), if_then_holds, A, B, C),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_user____if_then_holds___3', F),
	b_setval('$chr_store_global_list_user____if_then_holds___3',
		 [D|F]),
	attach_if_then_holds___3([], D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(if_then_holds(A, B, C)#D)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(D)),
		if_then_holds___3__0(A, B, C, D)
	    ;   'chr debug_event'(fail(D)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(D))
	    ;   'chr debug_event'(redo(D)),
		fail
	    )
	;   if_then_holds___3__0(A, B, C, D)
	).

attach_not_holds_all___2([], _).
attach_not_holds_all___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, F, G, H, I, J, K, L, M, N, O, P, Q),
	    (   C/\2=:=2
	    ->  R=v(C, D, [E|F], G, H, I, J, K, L, M, N, O, P, Q)
	    ;   S is C\/2,
		R=v(S, D, [E], G, H, I, J, K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(2, [], [E], [], [], [], [], [], [], [], [], [], [], []))
	),
	attach_not_holds_all___2(T, E).

:- dynamic prolog_event_hook/1.
:- multifile prolog_event_hook/1.


all_holds___3__4(_, _, _, A) :-
	setarg(2, A, active).

:- multifile message_property/2.


attribute_goals(_, A, A).

getval(A, B) :-
	nb_getval(A, B).

attach_duplicate_free___1([], _).
attach_duplicate_free___1([A|T], F) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, G, H, I, J, K, L, M, N, O, P, Q),
	    (   C/\4=:=4
	    ->  R=v(C, D, E, [F|G], H, I, J, K, L, M, N, O, P, Q)
	    ;   S is C\/4,
		R=v(S, D, E, [F], H, I, J, K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(4, [], [], [F], [], [], [], [], [], [], [], [], [], []))
	),
	attach_duplicate_free___1(T, F).

or_and_eq([], 0#\=0).
or_and_eq([A|E], D#\/F) :-
	(   A=eq(B, C)
	->  and_eq(B, C, D)
	;   A=neq(B, C),
	    or_neq(exists, B, C, D)
	),
	or_and_eq(E, F).

not_holds___2__2__0__6([], A, B, C) :-
	not_holds___2__3(A, B, C).
not_holds___2__2__0__6([A|N], D, C, F) :-
	(   A=suspension(_, active, _, _, _, _, E, _, B),
	    B==C,
	    D==E,
	    'chr debug_event'(try([A], [F], D==G, true))
	->  'chr debug_event'(apply([A], [F], D==G, true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, H, I, J),
	    setarg(2, A, removed),
	    term_variables(term(H, I, J), M),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      K),
	    'chr sbag_del_element'(K, A, L),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     L),
	    detach_if_then_or_holds___3(M, A),
	    setarg(2, F, active),
	    (   F=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, F, inactive),
		not_holds___2__2__0__6(N, D, C, F)
	    ;   true
	    )
	;   not_holds___2__2__0__6(N, D, C, F)
	).

all_not_holds___3__2__0__5([], A, B, C, D) :-
	all_not_holds___3__3(A, B, C, D).
all_not_holds___3__2__0__5([A|S], D, E, C, I) :-
	(   A=suspension(_, active, _, _, _, _, G, _, B),
	    B==C,
	    copy_fluent(D, E, F, H),
	    F=G,
	    \+ call(#\+H),
	    'chr debug_event'(try([A],
				  [I],
				  (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				  true))
	->  'chr debug_event'(apply([A],
				    [I],
				    (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, M, N, O),
	    setarg(2, A, removed),
	    term_variables(term(M, N, O), R),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      P),
	    'chr sbag_del_element'(P, A, Q),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     Q),
	    detach_if_then_or_holds___3(R, A),
	    setarg(2, I, active),
	    (   I=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, I, inactive),
		all_not_holds___3__2__0__5(S,
					   D,
					   E,
					   C,
					   I)
	    ;   true
	    )
	;   all_not_holds___3__2__0__5(S,
				       D,
				       E,
				       C,
				       I)
	).

setval(A, B) :-
	nb_setval(A, B).

duplicate_free___1__0(A, B) :-
	nonvar(A),
	A=[C|D],
	'chr debug_event'(try([B],
			      [],
			      true,
			      (not_holds(C, D), duplicate_free(D)))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				(not_holds(C, D), duplicate_free(D)))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, duplicate_free, E),
	setarg(2, B, removed),
	term_variables(E, H),
	nb_getval('$chr_store_global_list_user____duplicate_free___1', F),
	'chr sbag_del_element'(F, B, G),
	b_setval('$chr_store_global_list_user____duplicate_free___1', G),
	detach_duplicate_free___1(H, B),
	not_holds(C, D),
	duplicate_free(D).
duplicate_free___1__0(A, B) :-
	A==[],
	'chr debug_event'(try([B], [], true, true)), !,
	'chr debug_event'(apply([B], [], true, true)),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, duplicate_free, C),
	setarg(2, B, removed),
	term_variables(C, F),
	nb_getval('$chr_store_global_list_user____duplicate_free___1', D),
	'chr sbag_del_element'(D, B, E),
	b_setval('$chr_store_global_list_user____duplicate_free___1', E),
	detach_duplicate_free___1(F, B).
duplicate_free___1__0(_, A) :-
	setarg(2, A, active).

:- dynamic file_search_path/2.
:- multifile file_search_path/2.

file_search_path(library, '/usr/local/lib/swipl-7.3.16/library/dialect/hprolog') :-
	hprolog:prolog_load_context(dialect, hprolog).
file_search_path(library, A) :-
	library_directory(A).
file_search_path(swi, A) :-
	system:current_prolog_flag(home, A).
file_search_path(foreign, swi(B)) :-
    system:
    (   current_prolog_flag(arch, A),
	atom_concat('lib/', A, B)
    ).
file_search_path(foreign, swi(A)) :-
    system:
    (   (   current_prolog_flag(windows, true)
	->  A=bin
	;   A=lib
	)
    ).
file_search_path(path, C) :-
    system:
    (   getenv('PATH', A),
	(   current_prolog_flag(windows, true)
	->  atomic_list_concat(B, ;, A)
	;   atomic_list_concat(B, :, A)
	),
	'$member'(C, B),
	'$no-null-bytes'(C)
    ).
file_search_path(user_profile, app_preferences('.')).
file_search_path(app_data, A) :-
	'$toplevel':catch(expand_file_name('~/lib/swipl', [A]), _, fail).
file_search_path(app_preferences, A) :-
	'$toplevel':catch(expand_file_name(~, [A]), _, fail).
file_search_path(autoload, library('.')).
file_search_path(pack, app_data(pack)).
file_search_path(pack, swi(pack)).
file_search_path(library, A) :-
	'$pack':pack_dir(_, prolog, A).
file_search_path(foreign, A) :-
	'$pack':pack_dir(_, foreign, A).
file_search_path(pce, A) :-
	link_xpce:pcehome_(A).
file_search_path(library, pce('prolog/lib')).
file_search_path(foreign, pce(B)) :-
    link_xpce:
    (   current_prolog_flag(arch, A),
	atom_concat('lib/', A, B)
    ).
file_search_path(chr, library(chr)).

copy_term_vars(C, A, B) :-
	copy_term(A, B),
	term_variables(B, C).

variable(A, B) :-
	nb_setval(A, B).

:- dynamic exception/3.
:- multifile exception/3.

exception(undefined_global_variable, A, retry) :-
    clpfd:
    (   make_clpfd_var(A), !
    ).
exception(undefined_global_variable, A, retry) :-
    chr_runtime:
    (   chr_runtime_global_variable(A),
	chr_init
    ).
exception(undefined_global_variable, A, retry) :-
    chr_runtime:
    (   chr_runtime_debug_global_variable(A),
	chr_debug_init
    ).
exception(undefined_global_variable, A, retry) :-
    guard_entailment:
    (   '$chr_prolog_global_variable'(A),
	'$chr_initialization'
    ).
exception(undefined_global_variable, A, retry) :-
    chr_translate:
    (   '$chr_prolog_global_variable'(A),
	'$chr_initialization'
    ).
exception(undefined_global_variable, A, retry) :-
    user:
    (   '$chr_prolog_global_variable'(A),
	'$chr_initialization'
    ).

attach_if_then_or_holds___3([], _).
attach_if_then_or_holds___3([A|T], M) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, K, L, N, O, P, Q),
	    (   C/\512=:=512
	    ->  R=v(C, D, E, F, G, H, I, J, K, L, [M|N], O, P, Q)
	    ;   S is C\/512,
		R=v(S, D, E, F, G, H, I, J, K, L, [M], O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(512, [], [], [], [], [], [], [], [], [], [M], [], [], []))
	),
	attach_if_then_or_holds___3(T, M).

detach_duplicate_free___1([], _).
detach_duplicate_free___1([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, D, J, K, L, M, N, O, P, Q, R, S),
	    (   C/\4=:=4
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -5,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   [],
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       F,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_duplicate_free___1(T, E).

not_holds___2__3__0__7([], A, B, C) :-
	not_holds___2__4(A, B, C).
not_holds___2__3__0__7([A|T], E, C, G) :-
	(   A=suspension(_, active, _, _, _, _, R, D, B),
	    B==C,
	    member(F, D, S),
	    E==F,
	    'chr debug_event'(try([A],
				  [G],
				  (member(H, J, I), E==H),
				  if_then_or_holds(K, I, C)))
	->  'chr debug_event'(apply([A],
				    [G],
				    (member(H, J, I), E==H),
				    if_then_or_holds(K, I, C))),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, L, M, N),
	    setarg(2, A, removed),
	    term_variables(term(L, M, N), Q),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      O),
	    'chr sbag_del_element'(O, A, P),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     P),
	    detach_if_then_or_holds___3(Q, A),
	    setarg(2, G, active),
	    if_then_or_holds(R, S, C),
	    (   G=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, G, inactive),
		not_holds___2__3__0__7(T, E, C, G)
	    ;   true
	    )
	;   not_holds___2__3__0__7(T, E, C, G)
	).

all_not_holds___3__1(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	all_not_holds___3__1__0__4(D, E, F, A, G).
all_not_holds___3__1(A, B, C, D) :-
	all_not_holds___3__2(A, B, C, D).

:- dynamic expand_answer/2.
:- multifile expand_answer/2.


all_not_holds___3__0__0__3([], A, B, C, D) :-
	all_not_holds___3__1(A, B, C, D).
all_not_holds___3__0__0__3([A|R], D, E, C, I) :-
	(   A=suspension(_, active, _, _, _, _, G, B),
	    B==C,
	    copy_fluent(D, E, F, H),
	    F=G,
	    \+ call(#\+H),
	    'chr debug_event'(try([A],
				  [I],
				  (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				  true))
	->  'chr debug_event'(apply([A],
				    [I],
				    (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, not_holds, M, N),
	    setarg(2, A, removed),
	    term_variables(term(M, N), Q),
	    nb_getval('$chr_store_global_list_user____not_holds___2', O),
	    'chr sbag_del_element'(O, A, P),
	    b_setval('$chr_store_global_list_user____not_holds___2', P),
	    detach_not_holds___2(Q, A),
	    setarg(2, I, active),
	    (   I=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, I, inactive),
		all_not_holds___3__0__0__3(R,
					   D,
					   E,
					   C,
					   I)
	    ;   true
	    )
	;   all_not_holds___3__0__0__3(R,
				       D,
				       E,
				       C,
				       I)
	).

cancel(A, B) :-
	C=suspension(D, active, _, 0, user:cancel___2__0(A, B, C), cancel, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____cancel___2', E),
	b_setval('$chr_store_global_list_user____cancel___2', [C|E]),
	attach_cancel___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(cancel(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		cancel___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   cancel___2__0(A, B, C)
	).

attach_or_holds___2([], _).
attach_or_holds___2([A|T], G) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, H, I, J, K, L, M, N, O, P, Q),
	    (   C/\8=:=8
	    ->  R=v(C, D, E, F, [G|H], I, J, K, L, M, N, O, P, Q)
	    ;   S is C\/8,
		R=v(S, D, E, F, [G], I, J, K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(8, [], [], [], [G], [], [], [], [], [], [], [], [], []))
	),
	attach_or_holds___2(T, G).

attach_if_then_holds___3([], _).
attach_if_then_holds___3([A|T], L) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, K, M, N, O, P, Q),
	    (   C/\256=:=256
	    ->  R=v(C, D, E, F, G, H, I, J, K, [L|M], N, O, P, Q)
	    ;   S is C\/256,
		R=v(S, D, E, F, G, H, I, J, K, [L], N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(256, [], [], [], [], [], [], [], [], [L], [], [], [], []))
	),
	attach_if_then_holds___3(T, L).

and_eq([], [], 0#=0).
and_eq([D|A], [E|B], C) :-
	and_eq(A, B, F),
	C= (D#=E#/\F).

all_not_holds___3__2(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	all_not_holds___3__2__0__5(D, E, F, A, G).
all_not_holds___3__2(A, B, C, D) :-
	all_not_holds___3__3(A, B, C, D).

cancel___2__1(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, D, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds_all___2',
		      D)
	), !,
	cancel___2__1__0__2(D, E, A, F).
cancel___2__1(A, B, C) :-
	cancel___2__2(A, B, C).

detach_if_then_or_holds___4([], _).
detach_if_then_or_holds___4([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, O, P, Q, D, R, S),
	    (   C/\1024=:=1024
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -1025,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   [],
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       F,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_if_then_or_holds___4(T, E).

attach_or_holds___3([], _).
attach_or_holds___3([A|T], H) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, I, J, K, L, M, N, O, P, Q),
	    (   C/\16=:=16
	    ->  R=v(C, D, E, F, G, [H|I], J, K, L, M, N, O, P, Q)
	    ;   S is C\/16,
		R=v(S, D, E, F, G, [H], J, K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(16, [], [], [], [], [H], [], [], [], [], [], [], [], []))
	),
	attach_or_holds___3(T, H).

cancelled(A, B) :-
	C=suspension(D, active, _, 0, user:cancelled___2__0(A, B, C), cancelled, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____cancelled___2', E),
	b_setval('$chr_store_global_list_user____cancelled___2',
		 [C|E]),
	attach_cancelled___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(cancelled(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		cancelled___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   cancelled___2__0(A, B, C)
	).

:- dynamic resource/3.
:- multifile resource/3.


all_holds___3__1(G, H, A, O) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, I, J, F),
	F==A,
	copy_fluent(G, H, K, M),
	copy_fluent(I, J, L, N),
	K=L,
	call(M#/\N),
	'chr debug_event'(try([O, E],
			      [],
			      (copy_fluent(G, H, P, R), copy_fluent(I, J, Q, S), P=Q, call(R#/\S)),
			      false)), !,
	'chr debug_event'(apply([O, E],
				[],
				(copy_fluent(G, H, P, R), copy_fluent(I, J, Q, S), P=Q, call(R#/\S)),
				false)),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, all_not_holds, T, U, V),
	setarg(2, E, removed),
	term_variables(term(T, U, V), Y),
	nb_getval('$chr_store_global_list_user____all_not_holds___3', W),
	'chr sbag_del_element'(W, E, X),
	b_setval('$chr_store_global_list_user____all_not_holds___3', X),
	detach_all_not_holds___3(Y, E),
	'chr debug_event'(remove(O)),
	O=suspension(_, _, _, _, _, all_holds, Z, A1, B1),
	setarg(2, O, removed),
	term_variables(term(Z, A1, B1), E1),
	nb_getval('$chr_store_global_list_user____all_holds___3', C1),
	'chr sbag_del_element'(C1, O, D1),
	b_setval('$chr_store_global_list_user____all_holds___3', D1),
	detach_all_holds___3(E1, O),
	false.
all_holds___3__1(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	all_holds___3__1__0__4(D, E, F, A, G).
all_holds___3__1(A, B, C, D) :-
	all_holds___3__2(A, B, C, D).

all_holds___3__2__0__5([], A, B, C, D) :-
	all_holds___3__3(A, B, C, D).
all_holds___3__2__0__5([A|U], D, E, C, I) :-
	(   A=suspension(_, active, _, _, _, _, G, T, B),
	    B==C,
	    copy_fluent(D, E, F, H),
	    F=G,
	    \+ call(#\+H),
	    'chr debug_event'(try([A],
				  [I],
				  (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				  or_holds(M, C)))
	->  'chr debug_event'(apply([A],
				    [I],
				    (copy_fluent(D, E, J, K), J=L, \+call(#\+K)),
				    or_holds(M, C))),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, N, O, P),
	    setarg(2, A, removed),
	    term_variables(term(N, O, P), S),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      Q),
	    'chr sbag_del_element'(Q, A, R),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     R),
	    detach_if_then_or_holds___3(S, A),
	    setarg(2, I, active),
	    or_holds(T, C),
	    (   I=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, I, inactive),
		all_holds___3__2__0__5(U,
				       D,
				       E,
				       C,
				       I)
	    ;   true
	    )
	;   all_holds___3__2__0__5(U, D, E, C, I)
	).

neq(A, B) :-
	or_neq(exists, A, B).

variable(A) :-
	nb_setval(A, []).

cancel___2__3__0__4([], A, B, C) :-
	cancel___2__4(A, B, C).
cancel___2__3__0__4([A|N], D, C, F) :-
	(   A=suspension(_, active, _, _, _, _, E, _, B),
	    B==C,
	    \+ D\=E,
	    'chr debug_event'(try([A],
				  [F],
				  \+D\=G,
				  true))
	->  'chr debug_event'(apply([A],
				    [F],
				    \+D\=G,
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, H, I, J),
	    setarg(2, A, removed),
	    term_variables(term(H, I, J), M),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      K),
	    'chr sbag_del_element'(K, A, L),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     L),
	    detach_if_then_or_holds___3(M, A),
	    setarg(2, F, active),
	    (   F=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, F, inactive),
		cancel___2__3__0__4(N, D, C, F)
	    ;   true
	    )
	;   cancel___2__3__0__4(N, D, C, F)
	).

:- meta_predicate local (:).

local _:A :-
	call(A).

all_holds___3__3__0__6([], A, B, C, D) :-
	all_holds___3__4(A, B, C, D).
all_holds___3__3__0__6([A|U], E, F, C, J) :-
	(   A=suspension(_, active, _, _, _, _, _, D, B),
	    B==C,
	    member(H, D),
	    copy_fluent(E, F, G, I),
	    G=H,
	    \+ call(#\+I),
	    'chr debug_event'(try([A],
				  [J],
				  (member(L, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				  true))
	->  'chr debug_event'(apply([A],
				    [J],
				    (member(L, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, O, P, Q),
	    setarg(2, A, removed),
	    term_variables(term(O, P, Q), T),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      R),
	    'chr sbag_del_element'(R, A, S),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     S),
	    detach_if_then_or_holds___3(T, A),
	    setarg(2, J, active),
	    (   J=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, J, inactive),
		all_holds___3__3__0__6(U,
				       E,
				       F,
				       C,
				       J)
	    ;   true
	    )
	;   all_holds___3__3__0__6(U, E, F, C, J)
	).

type_prolog(swi).

detach_or_holds___3([], _).
detach_or_holds___3([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, D, L, M, N, O, P, Q, R, S),
	    (   C/\16=:=16
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -17,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   [],
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       F,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_or_holds___3(T, E).

attach_if_then_or_holds___4([], _).
attach_if_then_or_holds___4([A|T], N) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, K, L, M, O, P, Q),
	    (   C/\1024=:=1024
	    ->  R=v(C, D, E, F, G, H, I, J, K, L, M, [N|O], P, Q)
	    ;   S is C\/1024,
		R=v(S, D, E, F, G, H, I, J, K, L, M, [N], P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(1024, [], [], [], [], [], [], [], [], [], [], [N], [], []))
	),
	attach_if_then_or_holds___4(T, N).

cancel___2__2__0__3([], A, B, C) :-
	cancel___2__3(A, B, C).
cancel___2__2__0__3([A|O], E, C, G) :-
	(   A=suspension(_, active, _, _, _, _, D, B),
	    B==C,
	    member(F, D),
	    \+ E\=F,
	    'chr debug_event'(try([A],
				  [G],
				  (member(H, I), \+E\=H),
				  true))
	->  'chr debug_event'(apply([A],
				    [G],
				    (member(H, I), \+E\=H),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, or_holds, J, K),
	    setarg(2, A, removed),
	    term_variables(term(J, K), N),
	    nb_getval('$chr_store_global_list_user____or_holds___2', L),
	    'chr sbag_del_element'(L, A, M),
	    b_setval('$chr_store_global_list_user____or_holds___2', M),
	    detach_or_holds___2(N, A),
	    setarg(2, G, active),
	    (   G=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, G, inactive),
		cancel___2__2__0__3(O, E, C, G)
	    ;   true
	    )
	;   cancel___2__2__0__3(O, E, C, G)
	).

neq_all(A, B) :-
	or_neq(forall, A, B).

setval(A) :-
	trace,
	nb_setval(A, []).

attach_all_holds___2([], _).
attach_all_holds___2([A|T], I) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, J, K, L, M, N, O, P, Q),
	    (   C/\32=:=32
	    ->  R=v(C, D, E, F, G, H, [I|J], K, L, M, N, O, P, Q)
	    ;   S is C\/32,
		R=v(S, D, E, F, G, H, [I], K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(32, [], [], [], [], [], [I], [], [], [], [], [], [], []))
	),
	attach_all_holds___2(T, I).

cancel___2__0(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	), !,
	cancel___2__0__0__1(D, E, A, F).
cancel___2__0(A, B, C) :-
	cancel___2__1(A, B, C).

or_neq(G, A, B) :-
	functor(A, C, E),
	functor(B, D, F),
	(   C=D,
	    E=F
	->  A=..[_|H],
	    B=..[_|I],
	    or_neq(G, H, I, J),
	    call(J)
	;   true
	).

all_holds___3__0__0__2([], A, B, C, D) :-
	all_holds___3__1(A, B, C, D).
all_holds___3__0__0__2([A|M], F, G, C, D) :-
	(   A=suspension(_, active, _, _, _, _, I, B),
	    B==C,
	    E=t(7, D, A),
	    '$novel_production'(D, E),
	    '$novel_production'(A, E),
	    copy_fluent(F, G, K, L),
	    'chr debug_event'(try([],
				  [D, A],
				  copy_fluent(F, G, H, J),
				  (H=I, call(#\+J))))
	->  'chr debug_event'(apply([],
				    [D, A],
				    copy_fluent(F,
						G,
						H,
						J),
				    (H=I, call(#\+J)))),
	    '$extend_history'(D, E),
	    setarg(2, D, active),
	    K=I,
	    call(#\+L),
	    (   D=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, D, inactive),
		all_holds___3__0__0__2(M,
				       F,
				       G,
				       C,
				       D)
	    ;   true
	    )
	;   all_holds___3__0__0__2(M, F, G, C, D)
	).

not_holds___2__0__0__3([], A, B, C) :-
	not_holds___2__1(A, B, C).
not_holds___2__0__0__3([A|M], I, C, D) :-
	(   A=suspension(_, active, _, _, _, _, F, G, B),
	    B==C,
	    E=t(7, A, D),
	    '$novel_production'(A, E),
	    '$novel_production'(D, E),
	    copy_fluent(F, G, K, L),
	    'chr debug_event'(try([],
				  [A, D],
				  copy_fluent(F, G, H, J),
				  (H=I, call(#\+J))))
	->  'chr debug_event'(apply([],
				    [A, D],
				    copy_fluent(F,
						G,
						H,
						J),
				    (H=I, call(#\+J)))),
	    '$extend_history'(D, E),
	    setarg(2, D, active),
	    K=I,
	    call(#\+L),
	    (   D=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, D, inactive),
		not_holds___2__0__0__3(M, I, C, D)
	    ;   true
	    )
	;   not_holds___2__0__0__3(M, I, C, D)
	).

all_holds___3__2(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	all_holds___3__2__0__5(D, E, F, A, G).
all_holds___3__2(A, B, C, D) :-
	all_holds___3__3(A, B, C, D).

detach_all_holds___3([], _).
detach_all_holds___3([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, D, N, O, P, Q, R, S),
	    (   C/\64=:=64
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -65,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   [],
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       F,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_all_holds___3(T, E).

inst(A, B) :-
	\+ ( term_variables(A, D),
	     term_variables(B, C),
	     bound_free(C, D, G, E),
	     copy_term_vars(E, B, F),
	     \+ no_global_bindings(A=F, G)
	   ).

attach_cancel___2([], _).
attach_cancel___2([A|T], O) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, K, L, M, N, P, Q),
	    (   C/\2048=:=2048
	    ->  R=v(C, D, E, F, G, H, I, J, K, L, M, N, [O|P], Q)
	    ;   S is C\/2048,
		R=v(S, D, E, F, G, H, I, J, K, L, M, N, [O], Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(2048, [], [], [], [], [], [], [], [], [], [], [], [O], []))
	),
	attach_cancel___2(T, O).

neq(A, B, C) :-
	or_neq_c(exists, A, B, C).

all_holds___2__0(B, C, A) :-
	'chr debug_event'(try([A],
			      [],
			      true,
			      all_holds(B, 0#=0, C))), !,
	'chr debug_event'(apply([A],
				[],
				true,
				all_holds(B, 0#=0, C))),
	'chr debug_event'(remove(A)),
	A=suspension(_, _, _, _, _, all_holds, _, _),
	setarg(2, A, removed),
	nb_getval('$chr_store_global_list_user____all_holds___2', D),
	'chr sbag_del_element'(D, A, E),
	b_setval('$chr_store_global_list_user____all_holds___2', E),
	all_holds(B, 0#=0, C).
all_holds___2__0(_, _, A) :-
	setarg(2, A, active).

all_holds___3__1__0__4([], A, B, C, D) :-
	all_holds___3__2(A, B, C, D).
all_holds___3__1__0__4([A|T], E, F, C, J) :-
	(   A=suspension(_, active, _, _, _, _, D, B),
	    B==C,
	    member(H, D),
	    copy_fluent(E, F, G, I),
	    G=H,
	    \+ call(#\+I),
	    'chr debug_event'(try([A],
				  [J],
				  (member(L, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				  true))
	->  'chr debug_event'(apply([A],
				    [J],
				    (member(L, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, or_holds, O, P),
	    setarg(2, A, removed),
	    term_variables(term(O, P), S),
	    nb_getval('$chr_store_global_list_user____or_holds___2', Q),
	    'chr sbag_del_element'(Q, A, R),
	    b_setval('$chr_store_global_list_user____or_holds___2', R),
	    detach_or_holds___2(S, A),
	    setarg(2, J, active),
	    (   J=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, J, inactive),
		all_holds___3__1__0__4(T,
				       E,
				       F,
				       C,
				       J)
	    ;   true
	    )
	;   all_holds___3__1__0__4(T, E, F, C, J)
	).

:- dynamic library_directory/1.
:- multifile library_directory/1.

library_directory(B) :-
    '$parms':
    (   cached_library_directory(local, A=lib, A),
	B=A
    ).
library_directory(B) :-
    '$parms':
    (   cached_library_directory(user,
				 expand_file_name('~/lib/prolog', [A]),
				 A),
	B=A
    ).
library_directory(B) :-
    '$parms':
    (   cached_library_directory(system,
				 absolute_file_name(swi(library), A),
				 A),
	B=A
    ).
library_directory(B) :-
    '$parms':
    (   cached_library_directory(clp,
				 absolute_file_name(swi('library/clp'), A),
				 A),
	B=A
    ).

attach_all_holds___3([], _).
attach_all_holds___3([A|T], J) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, K, L, M, N, O, P, Q),
	    (   C/\64=:=64
	    ->  R=v(C, D, E, F, G, H, I, [J|K], L, M, N, O, P, Q)
	    ;   S is C\/64,
		R=v(S, D, E, F, G, H, I, [J], L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(64, [], [], [], [], [], [], [J], [], [], [], [], [], []))
	),
	attach_all_holds___3(T, J).

copy_fluent(A, D, E, F) :-
	term_variables(A, B),
	bound_free(B, [], _, C),
	copy_term_vars(C, [A, D], [E, F]).

cancel___2__4__0__5([], A, B, C) :-
	cancel___2__5(A, B, C).
cancel___2__4__0__5([A|P], E, C, G) :-
	(   A=suspension(_, active, _, _, _, _, _, D, B),
	    B==C,
	    member(F, D),
	    \+ E\=F,
	    'chr debug_event'(try([A],
				  [G],
				  (member(H, I), \+E\=H),
				  true))
	->  'chr debug_event'(apply([A],
				    [G],
				    (member(H, I), \+E\=H),
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, J, K, L),
	    setarg(2, A, removed),
	    term_variables(term(J, K, L), O),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      M),
	    'chr sbag_del_element'(M, A, N),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     N),
	    detach_if_then_or_holds___3(O, A),
	    setarg(2, G, active),
	    (   G=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, G, inactive),
		cancel___2__4__0__5(P, E, C, G)
	    ;   true
	    )
	;   cancel___2__4__0__5(P, E, C, G)
	).

not_holds___2__1__0__5([], A, B, C) :-
	not_holds___2__2(A, B, C).
not_holds___2__1__0__5([A|Q], E, C, G) :-
	(   A=suspension(_, active, _, _, _, _, D, B),
	    B==C,
	    member(F, D, P),
	    E==F,
	    'chr debug_event'(try([A],
				  [G],
				  (member(H, J, I), E==H),
				  or_holds(I, C)))
	->  'chr debug_event'(apply([A],
				    [G],
				    (member(H, J, I), E==H),
				    or_holds(I, C))),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, or_holds, K, L),
	    setarg(2, A, removed),
	    term_variables(term(K, L), O),
	    nb_getval('$chr_store_global_list_user____or_holds___2', M),
	    'chr sbag_del_element'(M, A, N),
	    b_setval('$chr_store_global_list_user____or_holds___2', N),
	    detach_or_holds___2(O, A),
	    setarg(2, G, active),
	    or_holds(P, C),
	    (   G=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, G, inactive),
		not_holds___2__1__0__5(Q, E, C, G)
	    ;   true
	    )
	;   not_holds___2__1__0__5(Q, E, C, G)
	).

all_holds___3__3(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	all_holds___3__3__0__6(D, E, F, A, G).
all_holds___3__3(A, B, C, D) :-
	all_holds___3__4(A, B, C, D).

:- thread_local thread_message_hook/3.
:- dynamic thread_message_hook/3.
:- volatile thread_message_hook/3.


attach_increment([], _).
attach_increment([A|G2], B) :-
	(   get_attr(A, user, C)
	->  B=v(Q1, E, H, K, N, Q, T, W, Z, C1, F1, I1, L1, O1),
	    C=v(R1, D, G, J, M, P, S, V, Y, B1, E1, H1, K1, N1),
	    sort(D, F),
	    'chr merge_attributes'(E, F, T1),
	    sort(G, I),
	    'chr merge_attributes'(H, I, U1),
	    sort(J, L),
	    'chr merge_attributes'(K, L, V1),
	    sort(M, O),
	    'chr merge_attributes'(N, O, W1),
	    sort(P, R),
	    'chr merge_attributes'(Q, R, X1),
	    sort(S, U),
	    'chr merge_attributes'(T, U, Y1),
	    sort(V, X),
	    'chr merge_attributes'(W, X, Z1),
	    sort(Y, A1),
	    'chr merge_attributes'(Z, A1, A2),
	    sort(B1, D1),
	    'chr merge_attributes'(C1, D1, B2),
	    sort(E1, G1),
	    'chr merge_attributes'(F1, G1, C2),
	    sort(H1, J1),
	    'chr merge_attributes'(I1, J1, D2),
	    sort(K1, M1),
	    'chr merge_attributes'(L1, M1, E2),
	    sort(N1, P1),
	    'chr merge_attributes'(O1, P1, F2),
	    S1 is Q1\/R1,
	    put_attr(A,
		     user,
		     v(S1,
		       T1,
		       U1,
		       V1,
		       W1,
		       X1,
		       Y1,
		       Z1,
		       A2,
		       B2,
		       C2,
		       D2,
		       E2,
		       F2))
	;   put_attr(A, user, B)
	),
	attach_increment(G2, B).

if_then_or_holds(A, B, C) :-
	D=suspension(E, active, _, 0, user:if_then_or_holds___3__0(A, B, C, D), if_then_or_holds, A, B, C),
	term_variables(term(A, B, C), G),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3', F),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		 [D|F]),
	attach_if_then_or_holds___3(G, D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(if_then_or_holds(A, B, C)#D)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(D)),
		if_then_or_holds___3__0(A, B, C, D)
	    ;   'chr debug_event'(fail(D)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(D))
	    ;   'chr debug_event'(redo(D)),
		fail
	    )
	;   if_then_or_holds___3__0(A, B, C, D)
	).

cancel___2__5(A, B, I) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, user, D),
	    D=v(_, _, _, _, _, _, _, _, _, _, _, _, _, E)
	;   nb_getval('$chr_store_global_list_user____cancelled___2', E)
	),
	member(F, E),
	F=suspension(_, active, _, _, _, _, G, H),
	G==A,
	H==B,
	'chr debug_event'(try([I, F], [], true, true)), !,
	'chr debug_event'(apply([I, F], [], true, true)),
	'chr debug_event'(remove(F)),
	F=suspension(_, _, _, _, _, cancelled, J, K),
	setarg(2, F, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____cancelled___2', L),
	'chr sbag_del_element'(L, F, M),
	b_setval('$chr_store_global_list_user____cancelled___2', M),
	detach_cancelled___2(N, F),
	'chr debug_event'(remove(I)),
	I=suspension(_, _, _, _, _, cancel, O, P),
	setarg(2, I, removed),
	term_variables(term(O, P), S),
	nb_getval('$chr_store_global_list_user____cancel___2', Q),
	'chr sbag_del_element'(Q, I, R),
	b_setval('$chr_store_global_list_user____cancel___2', R),
	detach_cancel___2(S, I).
cancel___2__5(_, _, A) :-
	setarg(2, A, active).

:- dynamic message_hook/3.
:- multifile message_hook/3.

message_hook(trace_mode(A), _, _) :-
    chr:
    (   (   A==on
	->  chr_trace
	;   chr_notrace
	),
	fail
    ).

or_holds___3__0(A, G, B, C) :-
	nonvar(A),
	A=[D|F],
	nonvar(B),
	B=[E|H],
	'chr debug_event'(try([C],
			      [],
			      true,
			      (D==E->true;D\=E->or_holds(F, [D|G], [E|H]);D=..[K|I], E=..[L|J], or_holds(F, [eq(I, J), D|G], [E|H])))), !,
	'chr debug_event'(apply([C],
				[],
				true,
				(D==E->true;D\=E->or_holds(F, [D|G], [E|H]);D=..[K|I], E=..[L|J], or_holds(F, [eq(I, J), D|G], [E|H])))),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, or_holds, M, _, N),
	setarg(2, C, removed),
	term_variables(M, R, O),
	term_variables(N, O),
	nb_getval('$chr_store_global_list_user____or_holds___3', P),
	'chr sbag_del_element'(P, C, Q),
	b_setval('$chr_store_global_list_user____or_holds___3', Q),
	detach_or_holds___3(R, C),
	(   D==E
	->  true
	;   D\=E
	->  or_holds(F, [D|G], [E|H])
	;   D=..[_|S],
	    E=..[_|T],
	    or_holds(F,
		     [eq(S, T), D|G],
		     [E|H])
	).
or_holds___3__0(A, D, B, C) :-
	A==[],
	nonvar(B),
	B=[_|E],
	'chr debug_event'(try([C], [], true, or_holds(D, E))), !,
	'chr debug_event'(apply([C], [], true, or_holds(D, E))),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, or_holds, F, _, G),
	setarg(2, C, removed),
	term_variables(F, K, H),
	term_variables(G, H),
	nb_getval('$chr_store_global_list_user____or_holds___3', I),
	'chr sbag_del_element'(I, C, J),
	b_setval('$chr_store_global_list_user____or_holds___3', J),
	detach_or_holds___3(K, C),
	or_holds(D, E).
or_holds___3__0(_, _, _, A) :-
	setarg(2, A, active).

flux_version(3.1).

all_not_holds(A, B, C) :-
	D=suspension(E, active, _, 0, user:all_not_holds___3__0(A, B, C, D), all_not_holds, A, B, C),
	term_variables(term(A, B, C), G),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_user____all_not_holds___3', F),
	b_setval('$chr_store_global_list_user____all_not_holds___3',
		 [D|F]),
	attach_all_not_holds___3(G, D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(all_not_holds(A, B, C)#D)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(D)),
		all_not_holds___3__0(A, B, C, D)
	    ;   'chr debug_event'(fail(D)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(D))
	    ;   'chr debug_event'(redo(D)),
		fail
	    )
	;   all_not_holds___3__0(A, B, C, D)
	).

not_holds___2__1(J, A, L) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, H, F),
	F==A,
	copy_fluent(G, H, I, K),
	I=J,
	\+ call(#\+K),
	'chr debug_event'(try([L],
			      [E],
			      (copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
			      true)), !,
	'chr debug_event'(apply([L],
				[E],
				(copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
				true)),
	'chr debug_event'(remove(L)),
	L=suspension(_, _, _, _, _, not_holds, O, P),
	setarg(2, L, removed),
	term_variables(term(O, P), S),
	nb_getval('$chr_store_global_list_user____not_holds___2', Q),
	'chr sbag_del_element'(Q, L, R),
	b_setval('$chr_store_global_list_user____not_holds___2', R),
	detach_not_holds___2(S, L).
not_holds___2__1(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	not_holds___2__1__0__5(D, E, A, F).
not_holds___2__1(A, B, C) :-
	not_holds___2__2(A, B, C).

cancel___2__4(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	cancel___2__4__0__5(D, E, A, F).
cancel___2__4(A, B, C) :-
	cancel___2__5(A, B, C).

is_predicate(A/B) :-
	current_predicate(A/B),
	functor(C, A, B),
	predicate_property(C, visible).

:- dynamic expand_query/4.
:- multifile expand_query/4.


attach_not_holds___2([], _).
attach_not_holds___2([A|T], D) :-
	(   get_attr(A, user, B)
	->  B=v(C, E, F, G, H, I, J, K, L, M, N, O, P, Q),
	    (   C/\1=:=1
	    ->  R=v(C, [D|E], F, G, H, I, J, K, L, M, N, O, P, Q)
	    ;   S is C\/1,
		R=v(S, [D], F, G, H, I, J, K, L, M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(1, [D], [], [], [], [], [], [], [], [], [], [], [], []))
	),
	attach_not_holds___2(T, D).

or_neq_c(G, A, B, J) :-
	functor(A, C, E),
	functor(B, D, F),
	(   C=D,
	    E=F
	->  A=..[_|H],
	    B=..[_|I],
	    or_neq(G, H, I, J)
	;   J= (0#=0)
	).

not_holds_all(A, B) :-
	C=suspension(D, active, _, 0, user:not_holds_all___2__0(A, B, C), not_holds_all, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____not_holds_all___2', E),
	b_setval('$chr_store_global_list_user____not_holds_all___2',
		 [C|E]),
	attach_not_holds_all___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(not_holds_all(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		not_holds_all___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   not_holds_all___2__0(A, B, C)
	).

#\+A :-
	#\A.

detach_not_holds___2([], _).
detach_not_holds___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, H, I, J, K, L, M, N, O, P, Q, R, S),
	    (   C/\1=:=1
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -2,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   [],
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       F,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_not_holds___2(T, E).

detach_all_not_holds___3([], _).
detach_all_not_holds___3([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, D, O, P, Q, R, S),
	    (   C/\128=:=128
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -129,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   [],
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       F,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_all_not_holds___3(T, E).

all_holds___3__0(C, E, A, B) :-
	nonvar(A),
	A=[D|F],
	'chr debug_event'(try([B],
			      [],
			      true,
			      (\+ (C=D, call(E))->all_holds(C, E, F);C=..[J|G], D=..[K|H], or_neq(exists, G, H, I), all_holds(C, E#/\I, F)))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				(\+ (C=D, call(E))->all_holds(C, E, F);C=..[J|G], D=..[K|H], or_neq(exists, G, H, I), all_holds(C, E#/\I, F)))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, all_holds, L, M, N),
	setarg(2, B, removed),
	term_variables(term(L, M, N), Q),
	nb_getval('$chr_store_global_list_user____all_holds___3', O),
	'chr sbag_del_element'(O, B, P),
	b_setval('$chr_store_global_list_user____all_holds___3', P),
	detach_all_holds___3(Q, B),
	(   \+ ( C=D,
		 call(E)
	       )
	->  all_holds(C, E, F)
	;   C=..[_|R],
	    D=..[_|S],
	    or_neq(exists, R, S, T),
	    all_holds(C, E#/\T, F)
	).
all_holds___3__0(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	), !,
	all_holds___3__0__0__2(D, E, F, A, G).
all_holds___3__0(A, B, C, D) :-
	all_holds___3__1(A, B, C, D).

neq_all(A, B, C) :-
	or_neq_c(forall, A, B, C).

cancel___2__3(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	cancel___2__3__0__4(D, E, A, F).
cancel___2__3(A, B, C) :-
	cancel___2__4(A, B, C).

binding(A, [B|E], [D|F], C) :-
	(   A==B
	->  C=D
	;   binding(A, E, F, C)
	).

cancel___2__1__0__2([], A, B, C) :-
	cancel___2__2(A, B, C).
cancel___2__1__0__2([A|M], D, C, F) :-
	(   A=suspension(_, active, _, _, _, _, E, B),
	    B==C,
	    \+ D\=E,
	    'chr debug_event'(try([A],
				  [F],
				  \+D\=G,
				  true))
	->  'chr debug_event'(apply([A],
				    [F],
				    \+D\=G,
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, not_holds_all, H, I),
	    setarg(2, A, removed),
	    term_variables(term(H, I), L),
	    nb_getval('$chr_store_global_list_user____not_holds_all___2',
		      J),
	    'chr sbag_del_element'(J, A, K),
	    b_setval('$chr_store_global_list_user____not_holds_all___2',
		     K),
	    detach_not_holds_all___2(L, A),
	    setarg(2, F, active),
	    (   F=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, F, inactive),
		cancel___2__1__0__2(M, D, C, F)
	    ;   true
	    )
	;   cancel___2__1__0__2(M, D, C, F)
	).

eq(A, B, I) :-
	functor(A, C, E),
	functor(B, D, F),
	(   C=D,
	    E=F
	->  A=..[_|G],
	    B=..[_|H],
	    and_eq(G, H, I)
	;   I= (0#\=0)
	).

or_holds(A, B) :-
	C=suspension(D, active, _, 0, user:or_holds___2__0(A, B, C), or_holds, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____or_holds___2', E),
	b_setval('$chr_store_global_list_user____or_holds___2',
		 [C|E]),
	attach_or_holds___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(or_holds(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		or_holds___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   or_holds___2__0(A, B, C)
	).

:- dynamic portray/1.
:- multifile portray/1.


detach_cancelled___2([], _).
detach_cancelled___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, O, P, Q, R, S, D),
	    (   C/\4096=:=4096
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -4097,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S,
				   []))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S,
			       F))
		)
	    ;   true
	    )
	;   true
	),
	detach_cancelled___2(T, E).

not_holds___2__0(C, A, B) :-
	nonvar(A),
	A=[D|E],
	'chr debug_event'(try([B],
			      [],
			      true,
			      (neq(C, D), not_holds(C, E)))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				(neq(C, D), not_holds(C, E)))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, not_holds, F, G),
	setarg(2, B, removed),
	term_variables(term(F, G), J),
	nb_getval('$chr_store_global_list_user____not_holds___2', H),
	'chr sbag_del_element'(H, B, I),
	b_setval('$chr_store_global_list_user____not_holds___2', I),
	detach_not_holds___2(J, B),
	neq(C, D),
	not_holds(C, E).
not_holds___2__0(_, A, B) :-
	A==[],
	'chr debug_event'(try([B], [], true, true)), !,
	'chr debug_event'(apply([B], [], true, true)),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, not_holds, C, D),
	setarg(2, B, removed),
	term_variables(term(C, D), G),
	nb_getval('$chr_store_global_list_user____not_holds___2', E),
	'chr sbag_del_element'(E, B, F),
	b_setval('$chr_store_global_list_user____not_holds___2', F),
	detach_not_holds___2(G, B).
not_holds___2__0(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	), !,
	not_holds___2__0__0__3(D, E, A, F).
not_holds___2__0(A, B, C) :-
	not_holds___2__1(A, B, C).

if_then_or_holds___4__0(F, A, H, B, C) :-
	nonvar(A),
	A=[D|G],
	nonvar(B),
	B=[E|I],
	'chr debug_event'(try([C],
			      [],
			      true,
			      (D==E->true;D\=E->if_then_or_holds(F, G, [D|H], [E|I]);D=..[L|J], E=..[M|K], if_then_or_holds(F, G, [eq(J, K), D|H], [E|I])))), !,
	'chr debug_event'(apply([C],
				[],
				true,
				(D==E->true;D\=E->if_then_or_holds(F, G, [D|H], [E|I]);D=..[L|J], E=..[M|K], if_then_or_holds(F, G, [eq(J, K), D|H], [E|I])))),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, if_then_or_holds, _, N, _, O),
	setarg(2, C, removed),
	term_variables(N, S, P),
	term_variables(O, P),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___4',
		  Q),
	'chr sbag_del_element'(Q, C, R),
	b_setval('$chr_store_global_list_user____if_then_or_holds___4', R),
	detach_if_then_or_holds___4(S, C),
	(   D==E
	->  true
	;   D\=E
	->  if_then_or_holds(F,
			     G,
			     [D|H],
			     [E|I])
	;   D=..[_|T],
	    E=..[_|U],
	    if_then_or_holds(F,
			     G,
			     [eq(T, U), D|H],
			     [E|I])
	).
if_then_or_holds___4__0(D, A, E, B, C) :-
	A==[],
	nonvar(B),
	B=[_|F],
	'chr debug_event'(try([C],
			      [],
			      true,
			      if_then_or_holds(D, E, F))), !,
	'chr debug_event'(apply([C],
				[],
				true,
				if_then_or_holds(D, E, F))),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, if_then_or_holds, _, G, _, H),
	setarg(2, C, removed),
	term_variables(G, L, I),
	term_variables(H, I),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___4', J),
	'chr sbag_del_element'(J, C, K),
	b_setval('$chr_store_global_list_user____if_then_or_holds___4', K),
	detach_if_then_or_holds___4(L, C),
	if_then_or_holds(D, E, F).
if_then_or_holds___4__0(_, _, _, _, A) :-
	setarg(2, A, active).

member(A, [A|B], B).
member(B, [A|C], [A|D]) :-
	member(B, C, D).

cancel___2__2(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	cancel___2__2__0__3(D, E, A, F).
cancel___2__2(A, B, C) :-
	cancel___2__3(A, B, C).

cancel___2__0__0__1([], A, B, C) :-
	cancel___2__1(A, B, C).
cancel___2__0__0__1([A|M], D, C, F) :-
	(   A=suspension(_, active, _, _, _, _, E, B),
	    B==C,
	    \+ D\=E,
	    'chr debug_event'(try([A],
				  [F],
				  \+D\=G,
				  true))
	->  'chr debug_event'(apply([A],
				    [F],
				    \+D\=G,
				    true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, not_holds, H, I),
	    setarg(2, A, removed),
	    term_variables(term(H, I), L),
	    nb_getval('$chr_store_global_list_user____not_holds___2', J),
	    'chr sbag_del_element'(J, A, K),
	    b_setval('$chr_store_global_list_user____not_holds___2', K),
	    detach_not_holds___2(L, A),
	    setarg(2, F, active),
	    (   F=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, F, inactive),
		cancel___2__0__0__1(M, D, C, F)
	    ;   true
	    )
	;   cancel___2__0__0__1(M, D, C, F)
	).

bound_free([], A, A, []).
bound_free([C|A], B, D, F) :-
	bound_free(A, B, E, G),
	(   is_domain(C)
	->  D=[C|E],
	    F=G
	;   D=E,
	    F=[C|G]
	).

duplicate_free(A) :-
	B=suspension(C, active, _, 0, user:duplicate_free___1__0(A, B), duplicate_free, A),
	term_variables(A, E),
	'chr gen_id'(C),
	nb_getval('$chr_store_global_list_user____duplicate_free___1', D),
	b_setval('$chr_store_global_list_user____duplicate_free___1',
		 [B|D]),
	attach_duplicate_free___1(E, B),
	setarg(2, B, inactive),
	'chr debug_event'(insert(duplicate_free(A)#B)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(B)),
		duplicate_free___1__0(A, B)
	    ;   'chr debug_event'(fail(B)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(B))
	    ;   'chr debug_event'(redo(B)),
		fail
	    )
	;   duplicate_free___1__0(A, B)
	).

attach_cancelled___2([], _).
attach_cancelled___2([A|T], P) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, K, L, M, N, O, Q),
	    (   C/\4096=:=4096
	    ->  R=v(C, D, E, F, G, H, I, J, K, L, M, N, O, [P|Q])
	    ;   S is C\/4096,
		R=v(S, D, E, F, G, H, I, J, K, L, M, N, O, [P])
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(4096, [], [], [], [], [], [], [], [], [], [], [], [], [P]))
	),
	attach_cancelled___2(T, P).

all_not_holds___3__4(C, E, A, B) :-
	nonvar(A),
	A=[D|K],
	'chr debug_event'(try([B],
			      [],
			      true,
			      ((\+ (C=D, call(E))->true;copy_fluent(C=D, E, F=G, I), F=G, eq(D, G, H), neq_all(C, D, J), call(H#/\ #\+I#\/J)), all_not_holds(C, E, K)))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				((\+ (C=D, call(E))->true;copy_fluent(C=D, E, F=G, I), F=G, eq(D, G, H), neq_all(C, D, J), call(H#/\ #\+I#\/J)), all_not_holds(C, E, K)))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, all_not_holds, L, M, N),
	setarg(2, B, removed),
	term_variables(term(L, M, N), Q),
	nb_getval('$chr_store_global_list_user____all_not_holds___3', O),
	'chr sbag_del_element'(O, B, P),
	b_setval('$chr_store_global_list_user____all_not_holds___3', P),
	detach_all_not_holds___3(Q, B),
	(   \+ ( C=D,
		 call(E)
	       )
	->  true
	;   copy_fluent(C=D, E, R=S, U),
	    R=S,
	    eq(D, S, T),
	    neq_all(C, D, V),
	    call(T#/\ #\+U#\/V)
	),
	all_not_holds(C, E, K).
all_not_holds___3__4(_, _, _, A) :-
	setarg(2, A, active).

detach_all_holds___2([], _).
detach_all_holds___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, D, M, N, O, P, Q, R, S),
	    (   C/\32=:=32
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -33,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   [],
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       F,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_all_holds___2(T, E).

detach_or_holds___2([], _).
detach_or_holds___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, D, K, L, M, N, O, P, Q, R, S),
	    (   C/\8=:=8
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -9,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   [],
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       F,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_or_holds___2(T, E).

all_not_holds___3__3(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	all_not_holds___3__3__0__6(D, E, F, A, G).
all_not_holds___3__3(A, B, C, D) :-
	all_not_holds___3__4(A, B, C, D).

not_holds___2__4(H, A, I) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, _, _, D, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, F),
	F==A,
	\+ G\=H,
	'chr debug_event'(try([I], [E], \+G\=H, true)), !,
	'chr debug_event'(apply([I], [E], \+G\=H, true)),
	'chr debug_event'(remove(I)),
	I=suspension(_, _, _, _, _, not_holds, J, K),
	setarg(2, I, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____not_holds___2', L),
	'chr sbag_del_element'(L, I, M),
	b_setval('$chr_store_global_list_user____not_holds___2', M),
	detach_not_holds___2(N, I).
not_holds___2__4(_, _, A) :-
	setarg(2, A, active).

detach_cancel___2([], _).
detach_cancel___2([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, O, P, Q, R, D, S),
	    (   C/\2048=:=2048
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -2049,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   Q,
				   R,
				   [],
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       Q,
			       R,
			       F,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_cancel___2(T, E).

all_holds(A, B) :-
	C=suspension(D, active, _, 0, user:all_holds___2__0(A, B, C), all_holds, A, B),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____all_holds___2', E),
	b_setval('$chr_store_global_list_user____all_holds___2',
		 [C|E]),
	attach_all_holds___2([], C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(all_holds(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		all_holds___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   all_holds___2__0(A, B, C)
	).

:- dynamic prolog_load_file/2.
:- multifile prolog_load_file/2.


not_holds_all___2__0(B, C, A) :-
	'chr debug_event'(try([A],
			      [],
			      true,
			      all_not_holds(B, 0#=0, C))), !,
	'chr debug_event'(apply([A],
				[],
				true,
				all_not_holds(B, 0#=0, C))),
	'chr debug_event'(remove(A)),
	A=suspension(_, _, _, _, _, not_holds_all, D, E),
	setarg(2, A, removed),
	term_variables(term(D, E), H),
	nb_getval('$chr_store_global_list_user____not_holds_all___2', F),
	'chr sbag_del_element'(F, A, G),
	b_setval('$chr_store_global_list_user____not_holds_all___2', G),
	detach_not_holds_all___2(H, A),
	all_not_holds(B, 0#=0, C).
not_holds_all___2__0(H, A, I) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, _, _, D, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, F),
	F==A,
	\+ G\=H,
	'chr debug_event'(try([I], [E], \+G\=H, true)), !,
	'chr debug_event'(apply([I], [E], \+G\=H, true)),
	'chr debug_event'(remove(I)),
	I=suspension(_, _, _, _, _, not_holds_all, J, K),
	setarg(2, I, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____not_holds_all___2', L),
	'chr sbag_del_element'(L, I, M),
	b_setval('$chr_store_global_list_user____not_holds_all___2', M),
	detach_not_holds_all___2(N, I).
not_holds_all___2__0(_, _, A) :-
	setarg(2, A, active).

attach_all_not_holds___3([], _).
attach_all_not_holds___3([A|T], K) :-
	(   get_attr(A, user, B)
	->  B=v(C, D, E, F, G, H, I, J, L, M, N, O, P, Q),
	    (   C/\128=:=128
	    ->  R=v(C, D, E, F, G, H, I, J, [K|L], M, N, O, P, Q)
	    ;   S is C\/128,
		R=v(S, D, E, F, G, H, I, J, [K], M, N, O, P, Q)
	    ),
	    put_attr(A, user, R)
	;   put_attr(A,
		     user,
		     v(128, [], [], [], [], [], [], [], [K], [], [], [], [], []))
	),
	attach_all_not_holds___3(T, K).

is_domain(A) :-
	clpfd:fd_get(A, B, _), !,
	clpfd:clpfd_in(A, B).

attr_unify_hook(v(C2, A, B, C, D, E, F, G, H, I, J, K, L, M), N) :-
	sort(A, Q),
	sort(B, T),
	sort(C, W),
	sort(D, Z),
	sort(E, C1),
	sort(F, F1),
	sort(G, I1),
	sort(H, L1),
	sort(I, O1),
	sort(J, R1),
	sort(K, U1),
	sort(L, X1),
	sort(M, A2),
	(   var(N)
	->  (   get_attr(N, user, O)
	    ->  O=v(D2, P, S, V, Y, B1, E1, H1, K1, N1, Q1, T1, W1, Z1),
		sort(P, R),
		'chr merge_attributes'(Q, R, F2),
		sort(S, U),
		'chr merge_attributes'(T, U, G2),
		sort(V, X),
		'chr merge_attributes'(W, X, H2),
		sort(Y, A1),
		'chr merge_attributes'(Z, A1, I2),
		sort(B1, D1),
		'chr merge_attributes'(C1, D1, J2),
		sort(E1, G1),
		'chr merge_attributes'(F1, G1, K2),
		sort(H1, J1),
		'chr merge_attributes'(I1, J1, L2),
		sort(K1, M1),
		'chr merge_attributes'(L1, M1, M2),
		sort(N1, P1),
		'chr merge_attributes'(O1, P1, N2),
		sort(Q1, S1),
		'chr merge_attributes'(R1, S1, O2),
		sort(T1, V1),
		'chr merge_attributes'(U1, V1, P2),
		sort(W1, Y1),
		'chr merge_attributes'(X1, Y1, Q2),
		sort(Z1, B2),
		'chr merge_attributes'(A2, B2, R2),
		E2 is C2\/D2,
		put_attr(N,
			 user,
			 v(E2,
			   F2,
			   G2,
			   H2,
			   I2,
			   J2,
			   K2,
			   L2,
			   M2,
			   N2,
			   O2,
			   P2,
			   Q2,
			   R2)),
		'$run_suspensions_not_holds___2'(F2),
		'$run_suspensions_not_holds_all___2'(G2),
		'$run_suspensions_duplicate_free___1'(H2),
		'$run_suspensions_or_holds___2'(I2),
		'$run_suspensions_or_holds___3'(J2),
		'$run_suspensions_all_holds___2'(F1),
		'$run_suspensions_all_holds___3'(L2),
		'$run_suspensions_all_not_holds___3'(M2),
		'$run_suspensions_if_then_holds___3'(O1),
		'$run_suspensions_if_then_or_holds___3'(O2),
		'$run_suspensions_if_then_or_holds___4'(P2),
		'$run_suspensions_cancel___2'(Q2),
		'$run_suspensions_cancelled___2'(A2)
	    ;   put_attr(N,
			 user,
			 v(C2,
			   Q,
			   T,
			   W,
			   Z,
			   C1,
			   F1,
			   I1,
			   L1,
			   O1,
			   R1,
			   U1,
			   X1,
			   A2)),
		'$run_suspensions_not_holds___2'(Q),
		'$run_suspensions_not_holds_all___2'(T),
		'$run_suspensions_duplicate_free___1'(W),
		'$run_suspensions_or_holds___2'(Z),
		'$run_suspensions_or_holds___3'(C1),
		'$run_suspensions_all_holds___2'(F1),
		'$run_suspensions_all_holds___3'(I1),
		'$run_suspensions_all_not_holds___3'(L1),
		'$run_suspensions_if_then_holds___3'(O1),
		'$run_suspensions_if_then_or_holds___3'(R1),
		'$run_suspensions_if_then_or_holds___4'(U1),
		'$run_suspensions_cancel___2'(X1),
		'$run_suspensions_cancelled___2'(A2)
	    )
	;   (   compound(N)
	    ->  term_variables(N, S2),
		attach_increment(S2,
				 v(C2,
				   Q,
				   T,
				   W,
				   Z,
				   C1,
				   F1,
				   I1,
				   L1,
				   O1,
				   R1,
				   U1,
				   X1,
				   A2))
	    ;   true
	    ),
	    '$run_suspensions_not_holds___2'(Q),
	    '$run_suspensions_not_holds_all___2'(T),
	    '$run_suspensions_duplicate_free___1'(W),
	    '$run_suspensions_or_holds___2'(Z),
	    '$run_suspensions_or_holds___3'(C1),
	    '$run_suspensions_all_holds___2'(F1),
	    '$run_suspensions_all_holds___3'(I1),
	    '$run_suspensions_all_not_holds___3'(L1),
	    '$run_suspensions_if_then_holds___3'(O1),
	    '$run_suspensions_if_then_or_holds___3'(R1),
	    '$run_suspensions_if_then_or_holds___4'(U1),
	    '$run_suspensions_cancel___2'(X1),
	    '$run_suspensions_cancelled___2'(A2)
	).

all_not_holds___3__3__0__6([], A, B, C, D) :-
	all_not_holds___3__4(A, B, C, D).
all_not_holds___3__3__0__6([A|Y], E, F, C, J) :-
	(   A=suspension(_, active, _, _, _, _, W, D, B),
	    B==C,
	    member(H, D, X),
	    copy_fluent(E, F, G, I),
	    G=H,
	    \+ call(#\+I),
	    'chr debug_event'(try([A],
				  [J],
				  (member(L, O, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				  if_then_or_holds(P, N, C)))
	->  'chr debug_event'(apply([A],
				    [J],
				    (member(L, O, N), copy_fluent(E, F, K, M), K=L, \+call(#\+M)),
				    if_then_or_holds(P,
						     N,
						     C))),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, if_then_or_holds, Q, R, S),
	    setarg(2, A, removed),
	    term_variables(term(Q, R, S), V),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      T),
	    'chr sbag_del_element'(T, A, U),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     U),
	    detach_if_then_or_holds___3(V, A),
	    setarg(2, J, active),
	    if_then_or_holds(W, X, C),
	    (   J=suspension(_, active, _, _, _, _, _, _, _)
	    ->  setarg(2, J, inactive),
		all_not_holds___3__3__0__6(Y,
					   E,
					   F,
					   C,
					   J)
	    ;   true
	    )
	;   all_not_holds___3__3__0__6(Y,
				       E,
				       F,
				       C,
				       J)
	).

if_then_or_holds___3__0(J, O, A, L) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, H, F),
	F==A,
	(   copy_fluent(G, H, I, K),
	    I=J,
	    \+ call(#\+K),
	    'chr debug_event'(try([L],
				  [E],
				  (copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
				  or_holds(O, A))), !,
	    'chr debug_event'(apply([L],
				    [E],
				    (copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
				    or_holds(O, A))),
	    'chr debug_event'(remove(L)),
	    L=suspension(_, _, _, _, _, if_then_or_holds, P, Q, R),
	    setarg(2, L, removed),
	    term_variables(term(P, Q, R), U),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      S),
	    'chr sbag_del_element'(S, L, T),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     T),
	    detach_if_then_or_holds___3(U, L),
	    or_holds(O, A)
	;   member(W, O),
	    copy_fluent(G, H, V, X),
	    V=W,
	    \+ call(#\+X),
	    'chr debug_event'(try([L],
				  [E],
				  (member(Z, O), copy_fluent(G, H, Y, A1), Y=Z, \+call(#\+A1)),
				  true)), !,
	    'chr debug_event'(apply([L],
				    [E],
				    (member(Z, O), copy_fluent(G, H, Y, A1), Y=Z, \+call(#\+A1)),
				    true)),
	    'chr debug_event'(remove(L)),
	    L=suspension(_, _, _, _, _, if_then_or_holds, B1, C1, D1),
	    setarg(2, L, removed),
	    term_variables(term(B1, C1, D1), G1),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      E1),
	    'chr sbag_del_element'(E1, L, F1),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     F1),
	    detach_if_then_or_holds___3(G1, L)
	).
if_then_or_holds___3__0(J, U, A, L) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, H, F),
	F==A,
	(   copy_fluent(G, H, I, K),
	    I=J,
	    \+ call(#\+K),
	    'chr debug_event'(try([L],
				  [E],
				  (copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
				  true)), !,
	    'chr debug_event'(apply([L],
				    [E],
				    (copy_fluent(G, H, M, N), M=J, \+call(#\+N)),
				    true)),
	    'chr debug_event'(remove(L)),
	    L=suspension(_, _, _, _, _, if_then_or_holds, O, P, Q),
	    setarg(2, L, removed),
	    term_variables(term(O, P, Q), T),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      R),
	    'chr sbag_del_element'(R, L, S),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     S),
	    detach_if_then_or_holds___3(T, L)
	;   member(W, U, I1),
	    copy_fluent(G, H, V, X),
	    V=W,
	    \+ call(#\+X),
	    'chr debug_event'(try([L],
				  [E],
				  (member(Z, U, B1), copy_fluent(G, H, Y, A1), Y=Z, \+call(#\+A1)),
				  if_then_or_holds(J, B1, A))), !,
	    'chr debug_event'(apply([L],
				    [E],
				    (member(Z, U, B1), copy_fluent(G, H, Y, A1), Y=Z, \+call(#\+A1)),
				    if_then_or_holds(J, B1, A))),
	    'chr debug_event'(remove(L)),
	    L=suspension(_, _, _, _, _, if_then_or_holds, C1, D1, E1),
	    setarg(2, L, removed),
	    term_variables(term(C1, D1, E1), H1),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      F1),
	    'chr sbag_del_element'(F1, L, G1),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     G1),
	    detach_if_then_or_holds___3(H1, L),
	    if_then_or_holds(J, I1, A)
	).
if_then_or_holds___3__0(C, A, D, B) :-
	A==[],
	'chr debug_event'(try([B], [], true, not_holds(C, D))), !,
	'chr debug_event'(apply([B], [], true, not_holds(C, D))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, if_then_or_holds, E, F, G),
	setarg(2, B, removed),
	term_variables(term(E, F, G), J),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3', H),
	'chr sbag_del_element'(H, B, I),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3', I),
	detach_if_then_or_holds___3(J, B),
	not_holds(C, D).
if_then_or_holds___3__0(_, _, A, B) :-
	A==[],
	'chr debug_event'(try([B], [], true, true)), !,
	'chr debug_event'(apply([B], [], true, true)),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, if_then_or_holds, C, D, E),
	setarg(2, B, removed),
	term_variables(term(C, D, E), H),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3', F),
	'chr sbag_del_element'(F, B, G),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3', G),
	detach_if_then_or_holds___3(H, B).
if_then_or_holds___3__0(_, A, _, E) :-
	member(eq(B, C), A),
	or_neq(exists, B, C, D),
	\+ call(D),
	'chr debug_event'(try([E],
			      [],
			      (member(eq(F, G), A), or_neq(exists, F, G, H), \+call(H)),
			      true)), !,
	'chr debug_event'(apply([E],
				[],
				(member(eq(F, G), A), or_neq(exists, F, G, H), \+call(H)),
				true)),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, if_then_or_holds, I, J, K),
	setarg(2, E, removed),
	term_variables(term(I, J, K), N),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		  L),
	'chr sbag_del_element'(L, E, M),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3', M),
	detach_if_then_or_holds___3(N, E).
if_then_or_holds___3__0(I, A, K, E) :-
	member(eq(B, C), A, R),
	\+ ( and_eq(B, C, D),
	     call(D)
	   ),
	'chr debug_event'(try([E],
			      [],
			      (member(eq(F, G), A, J), \+ (and_eq(F, G, H), call(H))),
			      if_then_or_holds(I, J, K))), !,
	'chr debug_event'(apply([E],
				[],
				(member(eq(F, G), A, J), \+ (and_eq(F, G, H), call(H))),
				if_then_or_holds(I, J, K))),
	'chr debug_event'(remove(E)),
	E=suspension(_, _, _, _, _, if_then_or_holds, L, M, N),
	setarg(2, E, removed),
	term_variables(term(L, M, N), Q),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		  O),
	'chr sbag_del_element'(O, E, P),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3', P),
	detach_if_then_or_holds___3(Q, E),
	if_then_or_holds(I, R, K).
if_then_or_holds___3__0(C, E, A, B) :-
	nonvar(A),
	A=[D|F],
	'chr debug_event'(try([B],
			      [],
			      true,
			      (C==D->or_holds(E, [D|F]);C\=D->if_then_or_holds(C, E, [], [D|F]);C=..[I|G], D=..[J|H], or_holds([neq(G, H)|E], [D|F]), if_then_or_holds(C, E, [], [D|F])))), !,
	'chr debug_event'(apply([B],
				[],
				true,
				(C==D->or_holds(E, [D|F]);C\=D->if_then_or_holds(C, E, [], [D|F]);C=..[I|G], D=..[J|H], or_holds([neq(G, H)|E], [D|F]), if_then_or_holds(C, E, [], [D|F])))),
	'chr debug_event'(remove(B)),
	B=suspension(_, _, _, _, _, if_then_or_holds, K, L, M),
	setarg(2, B, removed),
	term_variables(term(K, L, M), P),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		  N),
	'chr sbag_del_element'(N, B, O),
	b_setval('$chr_store_global_list_user____if_then_or_holds___3', O),
	detach_if_then_or_holds___3(P, B),
	(   C==D
	->  or_holds(E, [D|F])
	;   C\=D
	->  if_then_or_holds(C, E, [], [D|F])
	;   C=..[_|Q],
	    D=..[_|R],
	    or_holds([neq(Q, R)|E], [D|F]),
	    if_then_or_holds(C, E, [], [D|F])
	).
if_then_or_holds___3__0(H, P, A, I) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, F),
	F==A,
	(   G==H,
	    'chr debug_event'(try([I], [E], G==H, true)), !,
	    'chr debug_event'(apply([I], [E], G==H, true)),
	    'chr debug_event'(remove(I)),
	    I=suspension(_, _, _, _, _, if_then_or_holds, J, K, L),
	    setarg(2, I, removed),
	    term_variables(term(J, K, L), O),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      M),
	    'chr sbag_del_element'(M, I, N),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     N),
	    detach_if_then_or_holds___3(O, I)
	;   member(Q, P, Z),
	    G==Q,
	    'chr debug_event'(try([I],
				  [E],
				  (member(R, P, S), G==R),
				  if_then_or_holds(H, S, A))), !,
	    'chr debug_event'(apply([I],
				    [E],
				    (member(R, P, S), G==R),
				    if_then_or_holds(H, S, A))),
	    'chr debug_event'(remove(I)),
	    I=suspension(_, _, _, _, _, if_then_or_holds, T, U, V),
	    setarg(2, I, removed),
	    term_variables(term(T, U, V), Y),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      W),
	    'chr sbag_del_element'(W, I, X),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     X),
	    detach_if_then_or_holds___3(Y, I),
	    if_then_or_holds(H, Z, A)
	).
if_then_or_holds___3__0(H, P, A, I) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, _, _, D, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, G, F),
	F==A,
	(   \+ G\=H,
	    'chr debug_event'(try([I],
				  [E],
				  \+G\=H,
				  true)), !,
	    'chr debug_event'(apply([I],
				    [E],
				    \+G\=H,
				    true)),
	    'chr debug_event'(remove(I)),
	    I=suspension(_, _, _, _, _, if_then_or_holds, J, K, L),
	    setarg(2, I, removed),
	    term_variables(term(J, K, L), O),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      M),
	    'chr sbag_del_element'(M, I, N),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     N),
	    detach_if_then_or_holds___3(O, I)
	;   member(Q, P),
	    \+ G\=Q,
	    'chr debug_event'(try([I],
				  [E],
				  (member(R, P), \+G\=R),
				  true)), !,
	    'chr debug_event'(apply([I],
				    [E],
				    (member(R, P), \+G\=R),
				    true)),
	    'chr debug_event'(remove(I)),
	    I=suspension(_, _, _, _, _, if_then_or_holds, S, T, U),
	    setarg(2, I, removed),
	    term_variables(term(S, T, U), X),
	    nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      V),
	    'chr sbag_del_element'(V, I, W),
	    b_setval('$chr_store_global_list_user____if_then_or_holds___3',
		     W),
	    detach_if_then_or_holds___3(X, I)
	).
if_then_or_holds___3__0(_, _, _, A) :-
	setarg(2, A, active).

detach_if_then_or_holds___3([], _).
detach_if_then_or_holds___3([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, O, P, D, Q, R, S),
	    (   C/\512=:=512
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -513,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   P,
				   [],
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       P,
			       F,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_if_then_or_holds___3(T, E).

not_holds___2__2(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	not_holds___2__2__0__6(D, E, A, F).
not_holds___2__2(A, B, C) :-
	not_holds___2__3(A, B, C).

not_holds(A, B) :-
	C=suspension(D, active, t, 0, user:not_holds___2__0(A, B, C), not_holds, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____not_holds___2', E),
	b_setval('$chr_store_global_list_user____not_holds___2',
		 [C|E]),
	attach_not_holds___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(not_holds(A, B)#C)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		not_holds___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   not_holds___2__0(A, B, C)
	).

all_holds(A, B, C) :-
	D=suspension(E, active, t, 0, user:all_holds___3__0(A, B, C, D), all_holds, A, B, C),
	term_variables(term(A, B, C), G),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_user____all_holds___3', F),
	b_setval('$chr_store_global_list_user____all_holds___3',
		 [D|F]),
	attach_all_holds___3(G, D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(all_holds(A, B, C)#D)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(D)),
		all_holds___3__0(A, B, C, D)
	    ;   'chr debug_event'(fail(D)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(D))
	    ;   'chr debug_event'(redo(D)),
		fail
	    )
	;   all_holds___3__0(A, B, C, D)
	).

if_then_holds___3__0(B, C, D, A) :-
	'chr debug_event'(try([A],
			      [],
			      true,
			      if_then_or_holds(B, [C], D))), !,
	'chr debug_event'(apply([A],
				[],
				true,
				if_then_or_holds(B, [C], D))),
	'chr debug_event'(remove(A)),
	A=suspension(_, _, _, _, _, if_then_holds, _, _, _),
	setarg(2, A, removed),
	nb_getval('$chr_store_global_list_user____if_then_holds___3', E),
	'chr sbag_del_element'(E, A, F),
	b_setval('$chr_store_global_list_user____if_then_holds___3', F),
	if_then_or_holds(B, [C], D).
if_then_holds___3__0(_, _, _, A) :-
	setarg(2, A, active).

if_then_or_holds(A, B, C, D) :-
	E=suspension(G, active, _, 0, user:if_then_or_holds___4__0(A, B, C, D, E), if_then_or_holds, A, B, C, D),
	term_variables(B, I, F),
	term_variables(D, F),
	'chr gen_id'(G),
	nb_getval('$chr_store_global_list_user____if_then_or_holds___4', H),
	b_setval('$chr_store_global_list_user____if_then_or_holds___4',
		 [E|H]),
	attach_if_then_or_holds___4(I, E),
	setarg(2, E, inactive),
	'chr debug_event'(insert(if_then_or_holds(A, B, C, D)#E)),
	(   'chr debugging'
	->  (   'chr debug_event'(call(E)),
		if_then_or_holds___4__0(A,
					B,
					C,
					D,
					E)
	    ;   'chr debug_event'(fail(E)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(E))
	    ;   'chr debug_event'(redo(E)),
		fail
	    )
	;   if_then_or_holds___4__0(A, B, C, D, E)
	).

:- multifile prolog_list_goal/1.


detach_if_then_holds___3([], _).
detach_if_then_holds___3([A|T], E) :-
	(   get_attr(A, user, B)
	->  B=v(C, H, I, J, K, L, M, N, O, D, P, Q, R, S),
	    (   C/\256=:=256
	    ->  'chr sbag_del_element'(D, E, F),
		(   F==[]
		->  G is C/\ -257,
		    (   G==0
		    ->  del_attr(A, user)
		    ;   put_attr(A,
				 user,
				 v(G,
				   H,
				   I,
				   J,
				   K,
				   L,
				   M,
				   N,
				   O,
				   [],
				   P,
				   Q,
				   R,
				   S))
		    )
		;   put_attr(A,
			     user,
			     v(C,
			       H,
			       I,
			       J,
			       K,
			       L,
			       M,
			       N,
			       O,
			       F,
			       P,
			       Q,
			       R,
			       S))
		)
	    ;   true
	    )
	;   true
	),
	detach_if_then_holds___3(T, E).

not_holds___2__3(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, D, _, _, _)
	;   nb_getval('$chr_store_global_list_user____if_then_or_holds___3',
		      D)
	), !,
	not_holds___2__3__0__7(D, E, A, F).
not_holds___2__3(A, B, C) :-
	not_holds___2__4(A, B, C).

cancelled___2__0(A, B, I) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, user, D),
	    D=v(_, _, _, _, _, _, _, _, _, _, _, _, E, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', E)
	),
	member(F, E),
	F=suspension(_, active, _, _, _, _, G, H),
	G==A,
	H==B,
	'chr debug_event'(try([F, I], [], true, true)), !,
	'chr debug_event'(apply([F, I], [], true, true)),
	'chr debug_event'(remove(F)),
	F=suspension(_, _, _, _, _, cancel, J, K),
	setarg(2, F, removed),
	term_variables(term(J, K), N),
	nb_getval('$chr_store_global_list_user____cancel___2', L),
	'chr sbag_del_element'(L, F, M),
	b_setval('$chr_store_global_list_user____cancel___2', M),
	detach_cancel___2(N, F),
	'chr debug_event'(remove(I)),
	I=suspension(_, _, _, _, _, cancelled, O, P),
	setarg(2, I, removed),
	term_variables(term(O, P), S),
	nb_getval('$chr_store_global_list_user____cancelled___2', Q),
	'chr sbag_del_element'(Q, I, R),
	b_setval('$chr_store_global_list_user____cancelled___2', R),
	detach_cancelled___2(S, I).
cancelled___2__0(_, _, A) :-
	setarg(2, A, active).
