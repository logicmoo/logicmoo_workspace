

% Bob Moore's variation on Mark Johnson's left recursion
% elimination algorithm

% this operateson a version of the grammar stored in cfg_rule/2
% whose first arg is a nonterminal, and whose second arg
% is a rule body (a list of terminal/nonterminals symbols).
% a terminal symbol is embedded in an lt/1 wrapper.
mark_johnson_transform_proper :-
	clear_dynamic_predicates([new_cfg_rule/2]),
	compute_left_rec_table, 
	find_retained_nonterminals,
	mark_johnson_rule1,
	cfg_rule(NT, Body),
	apply_transform_list(Body, NT),
	fail.


% Rule 1 is spelled out and performed in the beginning
mark_johnson_rule1 :-
	left_corner(A, X),
	left_recursive(A),
	retained_nonterminal(A),
	(X = lt(_) -> true ; \+left_recursive(X)),
	new_mj_nt(A, X, AX),
	assert(new_cfg_rule(A, [X, AX])),
	fail.
mark_johnson_rule1.


apply_transform_list([], _NT).
apply_transform_list([Body|RestList], NT):-
	apply_transform(Body, NT),
	apply_transform_list(RestList, NT).

% rule 2
apply_transform([X|Beta], B):-
	left_recursive(B),
	left_corner(A, B),
	left_recursive(A),
	retained_nonterminal(A),
	new_mj_nt(A, X, AX),
	new_mj_nt(A, B, AB),
	append(Beta, [AB], NewBody),
	assert(new_cfg_rule(AX,NewBody)),
	fail.

% rule 3
apply_transform([X|Beta], A):-
	left_recursive(A),
	retained_nonterminal(A),
	left_corner(A, X),
	new_mj_nt(A, X, AX),
	assert(new_cfg_rule(AX,Beta)),
	fail.

% rule 4
apply_transform(Beta, A):-
	\+ left_recursive(A),
	assert(new_cfg_rule(A,Beta)),
	fail.

apply_transform(_, _).


new_mj_nt(lt(A), lt(X), AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(A, lt(X), AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(lt(A), X, AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(A, X, AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).



find_retained_nonterminals :-
	clear_dynamic_predicates([retained_nonterminal/1]),
	assert_once(retained_nonterminal(sigma)),
	cfg_rule(NT, Body),
	find_retained_nonterminals_list(Body, NT),
	fail.
find_retained_nonterminals.

find_retained_nonterminals_list([], _).
find_retained_nonterminals_list([First|Rest], NT):-
	( left_recursive(NT)  ->
	      First = [_LeftMost| NonInitial],
	      find_retained_nonterminals(NonInitial)
	;     find_retained_nonterminals(First)),
	find_retained_nonterminals_list(Rest, NT).

find_retained_nonterminals([]).
find_retained_nonterminals([NT|Rest]):-
	atomic(NT),
	!,
	assert_once(retained_nonterminal(NT)),
	find_retained_nonterminals(Rest).
find_retained_nonterminals([_NT|Rest]):-
	find_retained_nonterminals(Rest).


left_recursive(NT):-
	left_corner(NT, NT),
	!.
	
compute_left_rec_table:-
	clear_dynamic_predicates([direct_left_corner/2, left_corner/2]),
	compute_direct_left_corner,
	direct_left_corner(NT, Dau),
	compute_left_corner(NT, Dau),
	fail.
compute_left_rec_table:-
	num_clauses(direct_left_corner(_,_), D),
	num_clauses(left_corner(_,_), L),
	format(' found ~d direct left corners and ~d proper left corners.~n', [D, L]).


compute_left_corner(NT, Dau):-
	\+ left_corner(NT, Dau),
	assert_once(left_corner(NT, Dau)),
	direct_left_corner(Dau, Next),
	compute_left_corner(NT, Next),
	fail.

compute_direct_left_corner:-
	cfg_rule(NT, Body),
	member([First|_], Body),
	assert_once(direct_left_corner(NT, First)),
	fail.
compute_direct_left_corner.
