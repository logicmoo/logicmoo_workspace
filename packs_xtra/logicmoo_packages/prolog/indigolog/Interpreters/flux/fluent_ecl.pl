
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- lib(fd).
neq(_6701, _6704) :- or_neq(exists, _6701, _6704).
neq_all(_6729, _6732) :- or_neq(forall, _6729, _6732).
or_neq(_6757, _6760, _6763) :- _6760 =.. [_6776|_6777], _6763 =.. [_6786|_6787], (_6776 = _6786 -> or_neq(_6757, _6777, _6787, _6805), call(_6805) ; true).
or_neq(_6976, [], [], 0 #\= 0).
or_neq(_7019, [_7024|_7025], [_7030|_7031], _7034) :- or_neq(_7019, _7025, _7031, _7051), (_7019 = forall, var(_7024) -> _7034 = _7051 ; _7034 = #\/(_7024 #\= _7030, _7051)).
and_eq([], [], 0 #= 0).
and_eq([_7294|_7295], [_7300|_7301], _7304) :- and_eq(_7295, _7301, _7318), _7304 = #/\(_7294 #= _7300, _7318).
or_and_eq([], 0 #\= 0).
or_and_eq([eq(_7478, _7481)|_7485], #\/(_7488, _7492)) :- or_and_eq(_7485, _7488), and_eq(_7478, _7481, _7492).
member(_7607, [_7607|_7612], _7612).
member(_7627, [_7632|_7633], [_7632|_7638]) :- member(_7627, _7633, _7638).
cancel(_7664, _7667, _7670) :- var(_7667) -> cancel(_7664, _7667), cancelled(_7664, _7667), _7670 = _7667 ; _7667 = [_7716|_7717], (_7664 \= _7716 -> cancel(_7664, _7717, _7733), _7670 = [_7716|_7733] ; cancel(_7664, _7717, _7670)).
not_holds(A, B) :-
	'CHRnot_holds_2'(not_holds(A, B), C, D, E).



%%% Rules handling for not_holds / 2

'CHRnot_holds_2'(not_holds(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnot_holds_2'(not_holds(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnot_holds_2'(not_holds(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	neq(A, B),
	not_holds(A, C).
'CHRnot_holds_2'(not_holds(A, []), B, C, D) ?-
	!,
	B = true.
'CHRnot_holds_2'(not_holds(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__23'(F, [B], [G], H),
	no_delayed_goals(instance(A, G)),
	!,
	C = true.
'CHRnot_holds_2'(not_holds(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__24'(F, [B], [G], H),
	no_delayed_goals(\+ G \= A),
	!,
	C = true,
	cancel(G, B).
'CHRnot_holds_2'(not_holds(A, B), C, D, E) :-
	'CHRnot_holds_2__22'(not_holds(A, B), C, D, E).
'CHRnot_holds_2__23'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_2__23'([A|B], C, D, E) :-
	'CHRnot_holds_2__23'(B, C, D, E).
:- set_flag('CHRnot_holds_2__23' / 4, leash, notrace).
'CHRnot_holds_2__24'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_2__24'([A|B], C, D, E) :-
	'CHRnot_holds_2__24'(B, C, D, E).
:- set_flag('CHRnot_holds_2__24' / 4, leash, notrace).
:- set_flag('CHRnot_holds_2' / 4, leash, notrace).
:- current_macro('CHRnot_holds_2' / 4, _9493, _9494, _9495) -> true ; define_macro('CHRnot_holds_2' / 4, tr_chr / 2, [write]).
'CHRnot_holds_2__22'(A, B, C, D) :-
	'CHRnot_holds_2__25'(A, B, C, D).
:- set_flag('CHRnot_holds_2__22' / 4, leash, notrace).
'CHRnot_holds_2__25'(not_holds(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__25__26'(F, C, not_holds(A, B), D, E).
'CHRnot_holds_2__25'(not_holds(A, B), C, D, E) :-
	'CHRnot_holds_2__25__27'(not_holds(A, B), C, D, E).
:- set_flag('CHRnot_holds_2__25' / 4, leash, notrace).
'CHRnot_holds_2__25__26'(['CHRor_2'(or(A, B), C, D, E)|F], G, not_holds(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A, L), H == K)),
	!,
	C = true,
	'CHRnot_holds_2__25__26'(F, G, not_holds(H, B), I, J),
	or(L, B).
'CHRnot_holds_2__25__26'([A|B], C, D, E, F) :-
	'CHRnot_holds_2__25__26'(B, C, D, E, F).
'CHRnot_holds_2__25__26'([], A, B, C, D) :-
	'CHRnot_holds_2__25__27'(B, A, C, D).
:- set_flag('CHRnot_holds_2__25__26' / 5, leash, notrace).
'CHRnot_holds_2__25__27'(not_holds(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, not_holds(A, B)], 'CHRnot_holds_2'(not_holds(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnot_holds_2__25__27' / 4, leash, notrace).
not_holds_all(A, B) :-
	'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E).



%%% Rules handling for not_holds_all / 2

'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnot_holds_all_2'(not_holds_all(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	neq_all(A, B),
	not_holds_all(A, C).
'CHRnot_holds_all_2'(not_holds_all(A, []), B, C, D) ?-
	!,
	B = true.
'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__29'(F, [B], [G], H),
	no_delayed_goals(instance(A, G)),
	!,
	C = true.
'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__28'(not_holds_all(A, B), C, D, E).
'CHRnot_holds_all_2__29'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_all_2__29'([A|B], C, D, E) :-
	'CHRnot_holds_all_2__29'(B, C, D, E).
:- set_flag('CHRnot_holds_all_2__29' / 4, leash, notrace).
:- set_flag('CHRnot_holds_all_2' / 4, leash, notrace).
:- current_macro('CHRnot_holds_all_2' / 4, _11159, _11160, _11161) -> true ; define_macro('CHRnot_holds_all_2' / 4, tr_chr / 2, [write]).
'CHRnot_holds_all_2__28'(A, B, C, D) :-
	'CHRnot_holds_all_2__30'(A, B, C, D).
:- set_flag('CHRnot_holds_all_2__28' / 4, leash, notrace).
'CHRnot_holds_all_2__30'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__31'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30' / 4, leash, notrace).
'CHRnot_holds_all_2__30__31'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals(instance(A, H)),
	!,
	C = true,
	'CHRnot_holds_all_2__30__31'(F, G, not_holds_all(H, B), I, J).
'CHRnot_holds_all_2__30__31'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__31'(B, C, D, E, F).
'CHRnot_holds_all_2__30__31'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__31' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__32__33'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30__32' / 4, leash, notrace).
'CHRnot_holds_all_2__30__32__33'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals(instance(A, H)),
	!,
	C = true,
	'CHRnot_holds_all_2__30__32__33'(F, G, not_holds_all(H, B), I, J).
'CHRnot_holds_all_2__30__32__33'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__32__33'(B, C, D, E, F).
'CHRnot_holds_all_2__30__32__33'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32__34'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__32__33' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__32__34__35'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32__34__36'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30__32__34' / 4, leash, notrace).
'CHRnot_holds_all_2__30__32__34__35'(['CHRor_2'(or(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A, L), instance(K, H))),
	!,
	C = true,
	'CHRnot_holds_all_2__30__32__34__35'(F, G, not_holds_all(H, B), I, J),
	or(L, B).
'CHRnot_holds_all_2__30__32__34__35'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__32__34__35'(B, C, D, E, F).
'CHRnot_holds_all_2__30__32__34__35'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32__34__36'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__32__34__35' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32__34__36'(not_holds_all(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, not_holds_all(A, B)], 'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnot_holds_all_2__30__32__34__36' / 4, leash, notrace).
duplicate_free(A) :-
	'CHRduplicate_free_1'(duplicate_free(A), B, C, D).



%%% Rules handling for duplicate_free / 1

'CHRduplicate_free_1'(duplicate_free(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRduplicate_free_1'(duplicate_free(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRduplicate_free_1'(duplicate_free([A|B]), C, D, E) ?-
	!,
	C = true,
	not_holds(A, B),
	duplicate_free(B).
'CHRduplicate_free_1'(duplicate_free([]), A, B, C) ?-
	!,
	A = true.
'CHRduplicate_free_1'(duplicate_free(A), B, C, D) :-
	'CHRduplicate_free_1__37'(duplicate_free(A), B, C, D).
:- set_flag('CHRduplicate_free_1' / 4, leash, notrace).
:- current_macro('CHRduplicate_free_1' / 4, _13394, _13395, _13396) -> true ; define_macro('CHRduplicate_free_1' / 4, tr_chr / 2, [write]).
'CHRduplicate_free_1__37'(A, B, C, D) :-
	'CHRduplicate_free_1__38'(A, B, C, D).
:- set_flag('CHRduplicate_free_1__37' / 4, leash, notrace).
'CHRduplicate_free_1__38'(duplicate_free(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, duplicate_free(A)], 'CHRduplicate_free_1'(duplicate_free(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRduplicate_free_1__38' / 4, leash, notrace).
or(A, B) :-
	'CHRor_2'(or(A, B), C, D, E).



%%% Rules handling for or / 2

'CHRor_2'(or(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRor_2'(or(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRor_2'(or([eq(A, B)], C), D, E, F) ?-
	!,
	D = true,
	and_eq(A, B, G),
	call(G).
'CHRor_2'(or([A], B), C, D, E) ?-
	!,
	C = true,
	holds(A, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals(\+ (member(F, A), F \= eq(G, H))),
	!,
	C = true,
	or_and_eq(A, I),
	call(I).
'CHRor_2'(or(A, []), B, C, D) ?-
	no_delayed_goals((member(E, A, F), E \= eq(G, H))),
	!,
	B = true,
	or(F, []).
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals((member(eq(F, G), A), or_neq(exists, F, G, H), \+ call(H))),
	!,
	C = true.
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals((member(eq(F, G), A, H), \+ (and_eq(F, G, I), call(I)))),
	!,
	C = true,
	or(H, B).
'CHRor_2'(or(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	or(A, [], [B|C]).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__40'(F, [B], [G], H),
	no_delayed_goals((member(I, A, J), G == I)),
	!,
	C = true,
	or(J, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__41'(F, [B], [G], H),
	no_delayed_goals((member(I, A, J), instance(I, G))),
	!,
	C = true,
	or(J, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__42'(F, [B], [G], H),
	no_delayed_goals((member(I, A), \+ G \= I)),
	!,
	C = true,
	cancel(G, B).
'CHRor_2'(or(A, B), C, D, E) :-
	'CHRor_2__39'(or(A, B), C, D, E).
'CHRor_2__40'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__40'([A|B], C, D, E) :-
	'CHRor_2__40'(B, C, D, E).
:- set_flag('CHRor_2__40' / 4, leash, notrace).
'CHRor_2__41'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__41'([A|B], C, D, E) :-
	'CHRor_2__41'(B, C, D, E).
:- set_flag('CHRor_2__41' / 4, leash, notrace).
'CHRor_2__42'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__42'([A|B], C, D, E) :-
	'CHRor_2__42'(B, C, D, E).
:- set_flag('CHRor_2__42' / 4, leash, notrace).
:- set_flag('CHRor_2' / 4, leash, notrace).
:- current_macro('CHRor_2' / 4, _16297, _16298, _16299) -> true ; define_macro('CHRor_2' / 4, tr_chr / 2, [write]).
'CHRor_2__39'(A, B, C, D) :-
	'CHRor_2__43'(A, B, C, D).
:- set_flag('CHRor_2__39' / 4, leash, notrace).
'CHRor_2__43'(or(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, or(A, B)], 'CHRor_2'(or(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRor_2__43' / 4, leash, notrace).
or(A, B, C) :-
	'CHRor_3'(or(A, B, C), D, E, F).



%%% Rules handling for or / 3

'CHRor_3'(or(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRor_3'(or(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRor_3'(or(A, B, [C|D]), E, F, G) ?-
	no_delayed_goals((member(H, A, I), \+ C \= H)),
	!,
	E = true,
	(
	    H == C
	->
	    true
	;
	    H =.. [J|K],
	    C =.. [L|M],
	    or(I, [eq(K, M), H|B], [C|D])
	).
'CHRor_3'(or(A, B, [C|D]), E, F, G) ?-
	!,
	E = true,
	append(A, B, H),
	or(H, D).
'CHRor_3'(or(A, B, C), D, E, F) :-
	'CHRor_3__44'(or(A, B, C), D, E, F).
:- set_flag('CHRor_3' / 4, leash, notrace).
:- current_macro('CHRor_3' / 4, _17155, _17156, _17157) -> true ; define_macro('CHRor_3' / 4, tr_chr / 2, [write]).
'CHRor_3__44'(A, B, C, D) :-
	'CHRor_3__45'(A, B, C, D).
:- set_flag('CHRor_3__44' / 4, leash, notrace).
'CHRor_3__45'(or(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, or(A, B, C)], 'CHRor_3'(or(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRor_3__45' / 4, leash, notrace).
cancel(A, B) :-
	'CHRcancel_2'(cancel(A, B), C, D, E).



%%% Rules handling for cancel / 2

'CHRcancel_2'(cancel(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRcancel_2'(cancel(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRcancel_2'(cancel(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__47'(F, [B, A], [], G),
	!,
	C = true.
'CHRcancel_2'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__46'(cancel(A, B), C, D, E).
'CHRcancel_2__47'(['CHRcancelled_2'(cancelled(A, B), C, D, E)|F], [B, A], [], G) ?-
	var(C),
	[C, E] = [true, G].
'CHRcancel_2__47'([A|B], C, D, E) :-
	'CHRcancel_2__47'(B, C, D, E).
:- set_flag('CHRcancel_2__47' / 4, leash, notrace).
:- set_flag('CHRcancel_2' / 4, leash, notrace).
:- current_macro('CHRcancel_2' / 4, _18017, _18018, _18019) -> true ; define_macro('CHRcancel_2' / 4, tr_chr / 2, [write]).
'CHRcancel_2__46'(A, B, C, D) :-
	'CHRcancel_2__48'(A, B, C, D).
:- set_flag('CHRcancel_2__46' / 4, leash, notrace).
'CHRcancel_2__48'(cancel(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__48__49'(F, C, cancel(A, B), D, E).
'CHRcancel_2__48'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__48__50'(cancel(A, B), C, D, E).
:- set_flag('CHRcancel_2__48' / 4, leash, notrace).
'CHRcancel_2__48__49'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], G, cancel(H, B), I, J) ?-
	var(C),
	no_delayed_goals(\+ H \= A),
	!,
	C = true,
	'CHRcancel_2__48__49'(F, G, cancel(H, B), I, J),
	cancel(H, B).
'CHRcancel_2__48__49'([A|B], C, D, E, F) :-
	'CHRcancel_2__48__49'(B, C, D, E, F).
'CHRcancel_2__48__49'([], A, B, C, D) :-
	'CHRcancel_2__48__50'(B, A, C, D).
:- set_flag('CHRcancel_2__48__49' / 5, leash, notrace).
'CHRcancel_2__48__50'(cancel(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__48__50__51'(F, C, cancel(A, B), D, E).
'CHRcancel_2__48__50'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__48__50__52'(cancel(A, B), C, D, E).
:- set_flag('CHRcancel_2__48__50' / 4, leash, notrace).
'CHRcancel_2__48__50__51'(['CHRor_2'(or(A, B), C, D, E)|F], G, cancel(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A), \+ H \= K)),
	!,
	C = true,
	'CHRcancel_2__48__50__51'(F, G, cancel(H, B), I, J),
	cancel(H, B).
'CHRcancel_2__48__50__51'([A|B], C, D, E, F) :-
	'CHRcancel_2__48__50__51'(B, C, D, E, F).
'CHRcancel_2__48__50__51'([], A, B, C, D) :-
	'CHRcancel_2__48__50__52'(B, A, C, D).
:- set_flag('CHRcancel_2__48__50__51' / 5, leash, notrace).
'CHRcancel_2__48__50__52'(cancel(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, cancel(A, B)], 'CHRcancel_2'(cancel(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRcancel_2__48__50__52' / 4, leash, notrace).
cancelled(A, B) :-
	'CHRcancelled_2'(cancelled(A, B), C, D, E).



%%% Rules handling for cancelled / 2

'CHRcancelled_2'(cancelled(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRcancelled_2'(cancelled(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRcancelled_2'(cancelled(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRcancelled_2__54'(F, [B, A], [], G),
	!,
	C = true.
'CHRcancelled_2'(cancelled(A, B), C, D, E) :-
	'CHRcancelled_2__53'(cancelled(A, B), C, D, E).
'CHRcancelled_2__54'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B, A], [], G) ?-
	var(C),
	[C, E] = [true, G].
'CHRcancelled_2__54'([A|B], C, D, E) :-
	'CHRcancelled_2__54'(B, C, D, E).
:- set_flag('CHRcancelled_2__54' / 4, leash, notrace).
:- set_flag('CHRcancelled_2' / 4, leash, notrace).
:- current_macro('CHRcancelled_2' / 4, _19903, _19904, _19905) -> true ; define_macro('CHRcancelled_2' / 4, tr_chr / 2, [write]).
'CHRcancelled_2__53'(A, B, C, D) :-
	'CHRcancelled_2__55'(A, B, C, D).
:- set_flag('CHRcancelled_2__53' / 4, leash, notrace).
'CHRcancelled_2__55'(cancelled(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, cancelled(A, B)], 'CHRcancelled_2'(cancelled(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRcancelled_2__55' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
