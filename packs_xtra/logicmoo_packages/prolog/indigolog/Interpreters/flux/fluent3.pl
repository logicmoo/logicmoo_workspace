
:-[flux2].


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

:- dynamic portray/1.
:- multifile portray/1.


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
	(   'chr debug_event'(call(D)),
	    all_not_holds___3__0(A, B, C, D)
	;   'chr debug_event'(fail(D)), !,
	    fail
	),
	(   'chr debug_event'(exit(D))
	;   'chr debug_event'(redo(D)),
	    fail
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

turn_to(A, B, C) :-
	(   knows(facing(A), B)
	->  C=B
	;   execute(turn, B, D),
	    turn_to(A, D, C)
	).

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

all_holds(A, B) :-
	C=suspension(D, active, _, 0, user:all_holds___2__0(A, B, C), all_holds, A, B),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____all_holds___2', E),
	b_setval('$chr_store_global_list_user____all_holds___2',
		 [C|E]),
	attach_all_holds___2([], C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(all_holds(A, B)#C)),
	(   'chr debug_event'(call(C)),
	    all_holds___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
	).

main :-
	init_simulator,
	init(A),
	execute(enter, A, E),
	B=[1, 1, [1, 2]],
	C=[[1, 1]],
	D=[],
	main_loop(B, C, D, E).

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

binding(A, [B|E], [D|F], C) :-
	(   A==B
	->  C=D
	;   binding(A, E, F, C)
	).

:- dynamic resource/3.
:- multifile resource/3.


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

if_then_or_holds___3__0(J, O, A, L) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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

a_star_do(D, A, H, I) :-
	(   A=do(go_to(B, C), _)
	->  true
	;   knows_val([B, C], at(B, C), D)
	),
	(   E=4
	;   E=3
	;   E=2
	;   E=1
	),
	adjacent(B, C, E, F, G),
	\+ visited(F, G),
	knows_not(pit(F, G), D),
	(   \+ knows(dead, D)
	->  knows_not(wumpus(F, G), D)
	;   true
	),
	H=go_to(F, G),
	assertz(visited(F, G)),
	I is F+G-2.

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
	(   'chr debug_event'(call(C)),
	    not_holds___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
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

main_loop([B, C, A|M], E, N, F) :-
	(   A=[D|L]
	->  (   explore(B, C, D, E, F, I)
	    ->  knows_val([G, H], at(G, H), I),
		hunt_wumpus(G, H, I, J),
		(   knows(gold(G, H), J)
		->  execute(grab, J, K),
		    go_home(K)
		;   O=[G, H, [1, 2, 3, 4], B, C, L|M],
		    P=[[G, H]|E],
		    Q=[B, C|N],
		    main_loop(O, P, Q, J)
		)
	    ;   main_loop([B, C, L|M],
			  E,
			  N,
			  F)
	    )
	;   backtrack(M, E, N, F)
	).

inst(A, B) :-
	\+ ( term_variables(A, D),
	     term_variables(B, C),
	     bound_free(C, D, G, E),
	     copy_term_vars(E, B, F),
	     \+ no_global_bindings(A=F, G)
	   ).

neq(A, B) :-
	or_neq(exists, A, B).

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
	(   'chr debug_event'(call(E)),
	    if_then_or_holds___4__0(A, B, C, D, E)
	;   'chr debug_event'(fail(E)), !,
	    fail
	),
	(   'chr debug_event'(exit(E))
	;   'chr debug_event'(redo(E)),
	    fail
	).

cancel___2__0(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, D, _, _, _, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____not_holds___2', D)
	), !,
	cancel___2__0__0__1(D, E, A, F).
cancel___2__0(A, B, C) :-
	cancel___2__1(A, B, C).

neq(A, B, C) :-
	or_neq_c(exists, A, B, C).

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

insert_all([], _, _, A, A).
insert_all([[G, F]|A], B, C, D, J) :-
	insert_all(A, B, C, D, I),
	E is C+1,
	H is E+F,
	ins(do(G, B), E, H, I, J).

and_eq([], [], 0#=0).
and_eq([D|A], [E|B], C) :-
	and_eq(A, B, F),
	C= (D#=E#/\F).

all_not_holds___3__1(E, F, A, G) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	all_not_holds___3__1__0__4(D, E, F, A, G).
all_not_holds___3__1(A, B, C, D) :-
	all_not_holds___3__2(A, B, C, D).

init_scenario :-
	retractall(wumpus(_, _)),
	retractall(pit(_, _)),
	retractall(gold(_, _)),
	xdim(B),
	ydim(D),
	random(0, 4294967296, A),
	random(0, 4294967296, C),
	E is A mod B+1,
	F is C mod D+1,
	(   E=1,
	    F=1
	->  true
	;   assertz(wumpus(E, F)),
	    write(wumpus(E, F))
	),
	random(0, 4294967296, G),
	random(0, 4294967296, H),
	I is G mod B+1,
	J is H mod D+1,
	assertz(gold(I, J)),
	write(gold(I, J)),
	no_of_random_pits(K),
	create_pits(K).

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
	(   'chr debug_event'(call(D)),
	    if_then_or_holds___3__0(A, B, C, D)
	;   'chr debug_event'(fail(D)), !,
	    fail
	),
	(   'chr debug_event'(exit(D))
	;   'chr debug_event'(redo(D)),
	    fail
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

a_star_plan(C, D) :-
	retractall(visited(_, _)),
	knows_val([A, B], at(A, B), C),
	assertz(visited(A, B)),
	a_star(C, [[], 0, 100000], D).

cancelled___2__0(A, B, I) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, user, D),
	    D=v(_, _, _, _, _, _, _, _, _, _, _, _, E, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', E)
	),
	'chr sbag_member'(F, E),
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

is_domain(A) :-
	clpfd:fd_get(A, B, _), !,
	A in B.

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

go_home(A) :-
	write('Planning...'),
	a_star_plan(A, B),
	execute(B, A, C),
	execute(exit, C, _).

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

ydim(12).

cancel___2__2(E, A, F) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, D, _, _, _, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____or_holds___2', D)
	), !,
	cancel___2__2__0__3(D, E, A, F).
cancel___2__2(A, B, C) :-
	cancel___2__3(A, B, C).

adjacent(A, D, G, C, F) :-
	xdim(B),
	ydim(E),
	A in 1..B,
	C in 1..B,
	D in 1..E,
	F in 1..E,
	G in 1..4,
	G#=1#/\C#=A#/\F#=D+1#\/G#=3#/\C#=A#/\F#=D-1#\/G#=2#/\C#=A+1#/\F#=D#\/G#=4#/\C#=A-1#/\F#=D.

bound_free([], A, A, []).
bound_free([C|A], B, D, F) :-
	bound_free(A, B, E, G),
	(   is_domain(C)
	->  D=[C|E],
	    F=G
	;   D=E,
	    F=[C|G]
	).

:- dynamic expand_query/4.
:- multifile expand_query/4.

expand_query(A, B, C, D) :-
	toplevel_variables:expand_query(A, B, C, D).

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

hunt_wumpus(D, E, A, H) :-
	(   \+ knows(dead, A),
	    knows_val([B, C], wumpus(B, C), A),
	    in_direction(D, E, F, B, C)
	->  turn_to(F, A, G),
	    execute(shoot, G, H)
	;   H=A
	).

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

cancel(A, B) :-
	C=suspension(D, active, _, 0, user:cancel___2__0(A, B, C), cancel, A, B),
	term_variables(term(A, B), F),
	'chr gen_id'(D),
	nb_getval('$chr_store_global_list_user____cancel___2', E),
	b_setval('$chr_store_global_list_user____cancel___2', [C|E]),
	attach_cancel___2(F, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(cancel(A, B)#C)),
	(   'chr debug_event'(call(C)),
	    cancel___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
	).

breeze_perception(A, F, K, L) :-
	(   integer(B)
	->  (   var(A+1)
	    ->  A+1 is B
	    ;   integer(A)
	    ->  B=:=A+1
	    ;   C is B,
		clpfd:clpfd_equal(C, A+1)
	    )
	;   integer(A)
	->  (   var(B)
	    ->  B is A+1
	    ;   C is A+1,
		clpfd:clpfd_equal(B, C)
	    )
	;   clpfd:clpfd_equal(B, A+1)
	),
	(   integer(D)
	->  (   var(A-1)
	    ->  A-1 is D
	    ;   integer(A)
	    ->  D=:=A-1
	    ;   E is D,
		clpfd:clpfd_equal(E, A-1)
	    )
	;   integer(A)
	->  (   var(D)
	    ->  D is A-1
	    ;   E is A+ -1,
		clpfd:clpfd_equal(D, E)
	    )
	;   clpfd:clpfd_equal(D, A-1)
	),
	(   integer(G)
	->  (   var(F+1)
	    ->  F+1 is G
	    ;   integer(F)
	    ->  G=:=F+1
	    ;   H is G,
		clpfd:clpfd_equal(H, F+1)
	    )
	;   integer(F)
	->  (   var(G)
	    ->  G is F+1
	    ;   H is F+1,
		clpfd:clpfd_equal(G, H)
	    )
	;   clpfd:clpfd_equal(G, F+1)
	),
	(   integer(I)
	->  (   var(F-1)
	    ->  F-1 is I
	    ;   integer(F)
	    ->  I=:=F-1
	    ;   J is I,
		clpfd:clpfd_equal(J, F-1)
	    )
	;   integer(F)
	->  (   var(I)
	    ->  I is F-1
	    ;   J is F+ -1,
		clpfd:clpfd_equal(I, J)
	    )
	;   clpfd:clpfd_equal(I, F-1)
	),
	(   K=false,
	    not_holds(pit(B, F), L),
	    not_holds(pit(D, F), L),
	    not_holds(pit(A, G), L),
	    not_holds(pit(A, I), L)
	;   K=true,
	    or_holds(
		     [ pit(B, F),
		       pit(A, G),
		       pit(D, F),
		       pit(A, I)
		     ],
		     L)
	).

create_pits(0) :- !.
create_pits(G) :-
	xdim(B),
	ydim(D),
	random(0, 4294967296, A),
	random(0, 4294967296, C),
	E is A mod B+1,
	F is C mod D+1,
	(   E+F<4
	->  create_pits(G)
	;   assertz(pit(E, F)),
	    write(pit(E, F))
	),
	H is G+ -1,
	create_pits(H).

ins(C, D, A, [G, H, B|E], F) :-
	(   A>B
	->  ins(C, D, A, E, I),
	    F=[G, H, B|I]
	;   F=[C, D, A, G, H, B|E]
	).
ins(A, B, C, [], [A, B, C]).

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
	(   'chr debug_event'(call(D)),
	    or_holds___3__0(A, B, C, D)
	;   'chr debug_event'(fail(D)), !,
	    fail
	),
	(   'chr debug_event'(exit(D))
	;   'chr debug_event'(redo(D)),
	    fail
	).

init_simulator :-
	init_scenario,
	retractall(current_state(_)),
	assertz(current_state([])).

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

perform(turn, []) :-
	write(turn),
	nl,
	current_state([at(A, B), facing(C)]),
	retract(current_state([at(A, B), facing(C)])),
	(   C<4
	->  D is C+1
	;   D is 1
	),
	assertz(current_state([at(A, B), facing(D)])).
perform(enter, [D, C, B]) :-
	write(enter),
	nl,
	current_state(A),
	retract(current_state(A)),
	assertz(current_state([at(1, 1), facing(1)])),
	(   gold(1, 1)
	->  B=true
	;   B=false
	),
	(   (   wumpus(1, 2)
	    ;   wumpus(2, 1)
	    )
	->  C=true
	;   C=false
	),
	(   (   pit(2, 1)
	    ;   pit(1, 2)
	    )
	->  D=true
	;   D=false
	).
perform(exit, []) :-
	write(exit),
	nl,
	current_state([at(A, B), facing(C)]),
	retract(current_state([at(A, B), facing(C)])),
	assertz(current_state([])).
perform(grab, []) :-
	write(grab),
	nl.
perform(shoot, [F]) :-
	write(shoot),
	nl,
	current_state([at(A, B), facing(C)]),
	wumpus(D, E),
	(   in_direction(A, B, C, D, E),
	    F=true
	;   F=false
	).
perform(go, [L, K, F]) :-
	write(go),
	nl,
	current_state([at(A, B), facing(C)]),
	retract(current_state([at(A, B), facing(C)])),
	(   C=1
	->  D is A,
	    E is B+1
	;   C=3
	->  D is A,
	    E is B+ -1
	;   C=2
	->  D is A+1,
	    E is B
	;   C=4
	->  D is A+ -1,
	    E is B
	),
	assertz(current_state([at(D, E), facing(C)])),
	(   gold(D, E)
	->  F=true
	;   F=false
	),
	G is D+1,
	I is D+ -1,
	H is E+1,
	J is E+ -1,
	(   (   wumpus(G, E)
	    ;   wumpus(D, H)
	    ;   wumpus(I, E)
	    ;   wumpus(D, J)
	    )
	->  K=true
	;   K=false
	),
	(   (   pit(G, E)
	    ;   pit(D, H)
	    ;   pit(I, E)
	    ;   pit(D, J)
	    )
	->  L=true
	;   L=false
	).

:- dynamic visited/2.


copy_fluent(A, D, E, F) :-
	term_variables(A, B),
	bound_free(B, [], _, C),
	copy_term_vars(C, [A, D], [E, F]).

xdim(10).

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

variable(A) :-
	nb_setval(A, []).

:- dynamic expand_answer/2.
:- multifile expand_answer/2.

expand_answer(A, B) :-
	toplevel_variables:expand_answer(A, B).

explore(A, B, C, F, G, I) :-
	adjacent(A, B, C, D, E),
	\+ member([D, E], F),
	knows_not(pit(D, E), G),
	(   knows_not(wumpus(D, E), G)
	;   knows(dead, G)
	),
	turn_to(C, G, H),
	execute(go, H, I).

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

:- dynamic prolog_event_hook/1.
:- multifile prolog_event_hook/1.


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
	(   'chr debug_event'(call(C)),
	    cancelled___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
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

:- dynamic current_state/1.

current_state([]).

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

member(A, [A|B], B).
member(B, [A|C], [A|D]) :-
	member(B, C, D).

a_star(A, [B, H, _|I], F) :-
	findall([C, D],
		a_star_do(A, B, C, D),
		E),
	(   member([G, 0], E)
	->  F=do(G, B)
	;   insert_all(E, B, H, I, J),
	    a_star(A, J, F)
	).

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

:- thread_local thread_message_hook/3.
:- dynamic thread_message_hook/3.
:- volatile thread_message_hook/3.


glitter_perception(B, C, A, D) :-
	(   A=false,
	    not_holds(gold(B, C), D)
	;   A=true,
	    holds(gold(B, C), D)
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
	(   'chr debug_event'(call(B)),
	    duplicate_free___1__0(A, B)
	;   'chr debug_event'(fail(B)), !,
	    fail
	),
	(   'chr debug_event'(exit(B))
	;   'chr debug_event'(redo(B)),
	    fail
	).

attribute_goals(_, A, A).

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

all_holds___3__1(G, H, A, O) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	'chr sbag_member'(E, D),
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

getval(A, B) :-
	nb_getval(A, B).

:- multifile prolog_clause_name/2.


setval(A) :-
	trace,
	nb_setval(A, []).

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

:- meta_predicate local (:).

local _:A :-
	call(A).

:- dynamic pit/2.

pit(6, 4).
pit(4, 10).
pit(6, 8).
pit(10, 5).
pit(2, 12).
pit(6, 4).
pit(1, 6).
pit(6, 10).
pit(10, 5).
pit(6, 1).
pit(8, 8).
pit(3, 6).

if_then_holds(A, B, C) :-
	D=suspension(E, active, _, 0, user:if_then_holds___3__0(A, B, C, D), if_then_holds, A, B, C),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_user____if_then_holds___3', F),
	b_setval('$chr_store_global_list_user____if_then_holds___3',
		 [D|F]),
	attach_if_then_holds___3([], D),
	setarg(2, D, inactive),
	'chr debug_event'(insert(if_then_holds(A, B, C)#D)),
	(   'chr debug_event'(call(D)),
	    if_then_holds___3__0(A, B, C, D)
	;   'chr debug_event'(fail(D)), !,
	    fail
	),
	(   'chr debug_event'(exit(D))
	;   'chr debug_event'(redo(D)),
	    fail
	).

or_holds___2__0(G, A, M) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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
	'chr sbag_member'(E, D),
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

state_update(A, enter, B, [C, D, E]) :-
	update(A, [at(1, 1), facing(1)], [], B),
	breeze_perception(1, 1, C, B),
	stench_perception(1, 1, D, B),
	glitter_perception(1, 1, E, B).
state_update(A, exit, C, []) :-
	holds(facing(B), A),
	update(A, [], [at(1, 1), facing(B)], C).
state_update(A, turn, D, []) :-
	holds(facing(B), A),
	B#<4#/\C#=B+1#\/B#=4#/\C#=1,
	update(A, [facing(C)], [facing(B)], D).
state_update(A, grab, D, []) :-
	holds(at(B, C), A),
	update(A, [has(1)], [gold(B, C)], D).
state_update(B, shoot, C, [A]) :-
	(   A=true,
	    update(B, [dead], [has(2)], C)
	;   A=false,
	    update(B, [], [has(2)], C)
	).
state_update(A, go, G, [H, I, J]) :-
	holds(at(B, C), A),
	holds(facing(D), A),
	adjacent(B, C, D, E, F),
	update(A, [at(E, F)], [at(B, C)], G),
	breeze_perception(E, F, H, G),
	stench_perception(E, F, I, G),
	glitter_perception(E, F, J, G).

:- dynamic prolog_file_type/2.
:- multifile prolog_file_type/2.

prolog_file_type(pl, prolog).
prolog_file_type(prolog, prolog).
prolog_file_type(qlf, prolog).
prolog_file_type(qlf, qlf).
prolog_file_type(A, executable) :-
	system:current_prolog_flag(shared_object_extension, A).

or_and_eq([], 0#\=0).
or_and_eq([A|E], D#\/F) :-
	(   A=eq(B, C)
	->  and_eq(B, C, D)
	;   A=neq(B, C),
	    or_neq(exists, B, C, D)
	),
	or_and_eq(E, F).

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

:- dynamic file_search_path/2.
:- multifile file_search_path/2.

file_search_path(library, 'c:/pf/swipl/library/dialect/hprolog') :-
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
	'$member'(C, B)
    ).
file_search_path(user_profile, app_preferences('.')).
file_search_path(app_preferences, app_data('.')).
file_search_path(app_data, B) :-
    '$toplevel':
    (   current_prolog_flag(windows, true),
	catch(win_folder(appdata, A), _, fail),
	atom_concat(A, '/SWI-Prolog', B),
	(   exists_directory(B)
	->  true
	;   catch(make_directory(B), _, fail)
	)
    ).
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

:- multifile prolog_list_goal/1.


no_of_random_pits(12).

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

go_back(D, E, A, H) :-
	holds(at(B, C), A),
	adjacent(B, C, F, D, E),
	turn_to(F, A, G),
	execute(go, G, H).

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
	(   'chr debug_event'(call(C)),
	    or_holds___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
	).

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

cancel___2__5(A, B, I) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, user, D),
	    D=v(_, _, _, _, _, _, _, _, _, _, _, _, _, E)
	;   nb_getval('$chr_store_global_list_user____cancelled___2', E)
	),
	'chr sbag_member'(F, E),
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
	(   'chr debug_event'(call(D)),
	    all_holds___3__0(A, B, C, D)
	;   'chr debug_event'(fail(D)), !,
	    fail
	),
	(   'chr debug_event'(exit(D))
	;   'chr debug_event'(redo(D)),
	    fail
	).

stench_perception(A, F, K, L) :-
	(   integer(B)
	->  (   var(A+1)
	    ->  A+1 is B
	    ;   integer(A)
	    ->  B=:=A+1
	    ;   C is B,
		clpfd:clpfd_equal(C, A+1)
	    )
	;   integer(A)
	->  (   var(B)
	    ->  B is A+1
	    ;   C is A+1,
		clpfd:clpfd_equal(B, C)
	    )
	;   clpfd:clpfd_equal(B, A+1)
	),
	(   integer(D)
	->  (   var(A-1)
	    ->  A-1 is D
	    ;   integer(A)
	    ->  D=:=A-1
	    ;   E is D,
		clpfd:clpfd_equal(E, A-1)
	    )
	;   integer(A)
	->  (   var(D)
	    ->  D is A-1
	    ;   E is A+ -1,
		clpfd:clpfd_equal(D, E)
	    )
	;   clpfd:clpfd_equal(D, A-1)
	),
	(   integer(G)
	->  (   var(F+1)
	    ->  F+1 is G
	    ;   integer(F)
	    ->  G=:=F+1
	    ;   H is G,
		clpfd:clpfd_equal(H, F+1)
	    )
	;   integer(F)
	->  (   var(G)
	    ->  G is F+1
	    ;   H is F+1,
		clpfd:clpfd_equal(G, H)
	    )
	;   clpfd:clpfd_equal(G, F+1)
	),
	(   integer(I)
	->  (   var(F-1)
	    ->  F-1 is I
	    ;   integer(F)
	    ->  I=:=F-1
	    ;   J is I,
		clpfd:clpfd_equal(J, F-1)
	    )
	;   integer(F)
	->  (   var(I)
	    ->  I is F-1
	    ;   J is F+ -1,
		clpfd:clpfd_equal(I, J)
	    )
	;   clpfd:clpfd_equal(I, F-1)
	),
	(   K=false,
	    not_holds(wumpus(B, F), L),
	    not_holds(wumpus(D, F), L),
	    not_holds(wumpus(A, G), L),
	    not_holds(wumpus(A, I), L)
	;   K=true,
	    or_holds(
		     [ wumpus(B, F),
		       wumpus(A, G),
		       wumpus(D, F),
		       wumpus(A, I)
		     ],
		     L)
	).

setval(A, B) :-
	nb_setval(A, B).

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

:- multifile prolog_predicate_name/2.


type_prolog(swi).

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

:- dynamic wumpus/2.

wumpus(8, 3).

:- multifile message_property/2.


complex_action(do(C, A), B, E) :-
	execute(A, B, D),
	execute(C, D, E).
complex_action(go_to(D, E), A, H) :-
	holds(at(B, C), A),
	adjacent(B, C, F, D, E),
	turn_to(F, A, G),
	execute(go, G, H).

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

neq_all(A, B, C) :-
	or_neq_c(forall, A, B, C).

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
	'chr sbag_member'(E, D),
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

:- dynamic prolog_load_file/2.
:- multifile prolog_load_file/2.


all_holds___3__4(_, _, _, A) :-
	setarg(2, A, active).

:- dynamic exception/3.
:- multifile exception/3.

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
exception(undefined_global_variable, A, retry) :-
    clpfd:
    (   make_clpfd_var(A), !
    ).

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

is_predicate(A/B) :-
	current_predicate(A/B),
	functor(C, A, B),
	predicate_property(C, visible).

variable(A, B) :-
	nb_setval(A, B).

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

init(A) :-
	A=[has(2), wumpus(D, E)|F],
	xdim(B),
	ydim(C),
	H is B+1,
	G is C+1,
	D in 1..B,
	E in 1..C,
	not_holds(wumpus(1, 1), A),
	not_holds_all(wumpus(_, _), F),
	not_holds(dead, F),
	not_holds(pit(1, 1), F),
	not_holds_all(pit(_, 0), F),
	not_holds_all(pit(_, G), F),
	not_holds_all(pit(0, _), F),
	not_holds_all(pit(H, _), F),
	not_holds_all(at(_, _), F),
	not_holds_all(facing(_), F),
	duplicate_free(A).

not_holds___2__4(H, A, I) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, _, _, _, _, D, _)
	;   nb_getval('$chr_store_global_list_user____cancel___2', D)
	),
	'chr sbag_member'(E, D),
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

backtrack(_, _, [], A) :-
	execute(exit, A, _).
backtrack(D, E, [A, B|F], C) :-
	go_back(A, B, C, G),
	main_loop(D, E, F, G).

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
	(   'chr debug_event'(call(C)),
	    not_holds_all___2__0(A, B, C)
	;   'chr debug_event'(fail(C)), !,
	    fail
	),
	(   'chr debug_event'(exit(C))
	;   'chr debug_event'(redo(C)),
	    fail
	).

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

all_not_holds___3__0(I, J, A, O) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, D, _, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_holds___3', D)
	),
	'chr sbag_member'(E, D),
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

not_holds___2__1(J, A, L) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, user, C),
	    C=v(_, _, _, _, _, _, _, _, D, _, _, _, _, _)
	;   nb_getval('$chr_store_global_list_user____all_not_holds___3',
		      D)
	),
	'chr sbag_member'(E, D),
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

#\+A :-
	#\A.

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

copy_term_vars(C, A, B) :-
	copy_term(A, B),
	term_variables(B, C).

neq_all(A, B) :-
	or_neq(forall, A, B).

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

:- dynamic gold/2.

gold(9, 8).

in_direction(A, D, G, C, F) :-
	xdim(B),
	ydim(E),
	A in 1..B,
	C in 1..B,
	D in 1..E,
	F in 1..E,
	G in 1..4,
	G#=1#/\C#=A#/\F#>D#\/G#=3#/\C#=A#/\F#<D#\/G#=2#/\C#>A#/\F#=D#\/G#=4#/\C#<A#/\F#=D.
