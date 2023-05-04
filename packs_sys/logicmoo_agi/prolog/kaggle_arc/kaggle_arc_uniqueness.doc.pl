:- dynamic(is_for_ilp/4). % Dynamic predicate to store is_for_ilp/4 facts
:- dynamic(is_accompany_changed_db/4). % Dynamic predicate to store is_accompany_changed_db/4 facts

% Clear scene rules for a given TestID
clear_scene_rules(TestID):-
forall(is_accompany_changed_db(TestID,IO,P,PSame),
ignore(retract(is_accompany_changed_db(TestID,IO,P,PSame)))),!,
clear_object_dependancy(TestID).

% Count occurrences of G and store the result in N
count_of(G,N):- findall(G,G,L),variant_list_to_set(L,S),length(S,N).

% Define predicates that shouldn't be noticed
dont_notice(oid()).
dont_notice(giz()).
dont_notice(global2G(,)).
dont_notice(link(contains,)).
dont_notice(occurs_in_links(contained_by,)).
dont_notice(iz(i_o())).
dont_notice(P):- compound(P),arg(,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(F):- + atom(F),!,fail.
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).

% Define predicates that should be noticed
do_notice(pg(,,rank1,)).
do_notice(pg(,,,_)).

% Predicate to check if P should be noticed
ok_notice(P):- + + do_notice(P),!.
ok_notice(P):- + dont_notice(P).

% Define predicates that shouldn't be deduced
dont_deduce(link(sees(),)).
dont_deduce(giz()).
dont_deduce(size2D()).
dont_deduce(global2G(,)).
dont_deduce(vis2D(,)).
dont_deduce(P):- + compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_gridoid(P).
dont_deduce(unique_colors_count()).
dont_deduce(P):- compound(P),compound_name_arguments(P,,[X]),number(X).

% Define predicates that should be deduced
do_deduce(link(sees(),)).
do_deduce(rot2D()).
do_deduce(pen()).
do_deduce(iz(sid())).
do_deduce(P):- compound(P),compound_name_arguments(P,,[X,Y]),number(X),number(Y).
do_deduce(rotSize2D(grav,,)).

% Predicate to check if P should be deduced
ok_deduce(P):- + + dont_deduce(P), !, fail.
ok_deduce(P):- + + do_deduce(P),!.

% Check if two values have the same property names
other_val(X1,X2):- X1=@=X2, same_prop_names(X1,X2),!.
same_prop_names(X1,X2):-
compound(X1),compound(X2), same_functor(X1,X2),!,
make_unifiable_u(X1,U1), make_unifiable_u(X2,U2), U1 =@= U2.

% Helper predicate to create a unifiable version of a term
make_unifiable_u(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]).
make_unifiable_u(X1,U1):- make_unifiable_cc(X1,U1),!.

% Check if a



% Check if a term should be deduced
should_deduce(Term):- ok_deduce(Term), + + (+ deduce(Term)).

% Deduce a term if it should be deduced
deduce(Term):- should_deduce(Term),!.
deduce(Term):- fail.

% Generate the is_for_ilp/4 facts
generate_is_for_ilp(TestID, IO, Obj, P):-
is_accompany_changed_db(TestID, IO, Obj, P),
(ok_notice(P) -> (is_for_ilp(TestID, IO, Obj, P); fail); fail).

% Generate the is_for_ilp/4 facts for all TestIDs and IOs
generate_all_is_for_ilp:-
forall(test(TestID, IO), forall(is_accompany_changed_db(TestID, IO, Obj, P),
generate_is_for_ilp(TestID, IO, Obj, P))).

% Clear all is_for_ilp/4 facts
clear_is_for_ilp:- retractall(is_for_ilp(_, _, _, _)).

% Store the is_for_ilp/4 facts for a given TestID and IO
store_is_for_ilp(TestID, IO):-
forall(is_accompany_changed_db(TestID, IO, Obj, P),
assertz(is_for_ilp(TestID, IO, Obj, P))).

% Update the is_for_ilp/4 facts based on the given TestID and IO
update_is_for_ilp(TestID, IO):-
clear_is_for_ilp,
store_is_for_ilp(TestID, IO).

% Predicate to find all unique instances of G
variant_list_to_set(L, S):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S).

% Predicate to find all unique instances of G in a list
variant_list_to_set_list(L, S):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S).

% Predicate to find all unique instances of G in a list, and count them
variant_list_to_set_list_and_count(L, S, N):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S), length(S, N).

% Helper predicate to create a unifiable version of a compound term
make_unifiable_cc(X1, U1):- compound_name_arguments(X1, F, L1), maplist(make_unifiable_u, L1, L2), compound_name_arguments(U1, F, L2).

% Check if two terms have the same functor
same_functor(X1, X2):- compound_name_arity(X1, F, N), compound_name_arity(X2, F, N).





Douglas Miles
continue
% Check if two terms have the same functor and arguments
same_functor_and_args(X1, X2):- compound_name_arguments(X1, F, L1), compound_name_arguments(X2, F, L2), maplist(same_functor_and_args_u, L1, L2).

% Check if two terms have the same functor and arguments (helper for unifiable terms)
same_functor_and_args_u(X1, X2):- (nonvar(X1) -> same_functor_and_args(X1, X2); true).

% Check if two terms are unifiable
are_unifiable(X1, X2):- make_unifiable_cc(X1, U1), make_unifiable_cc(X2, U2), same_functor(U1, U2), + + (U1 = U2).

% Check if two terms are unifiable (helper for unifiable terms)
are_unifiable_u(X1, X2):- (nonvar(X1) -> are_unifiable(X1, X2); true).

% Create a unifiable version of a term (helper for unifiable terms)
make_unifiable_u(X1, X2):- (nonvar(X1) -> make_unifiable_cc(X1, X2); X2 = X1).

% Check if two terms are variant
are_variant(X1, X2):- make_unifiable_cc(X1, U1), make_unifiable_cc(X2, U2), U1 =@= U2.

% Count the number of unifiable instances in a list
count_unifiable_instances([], , 0).
count_unifiable_instances([H|T], X, N):- are_unifiable(H, X), !, count_unifiable_instances(T, X, N1), N is N1 + 1.
count_unifiable_instances([|T], X, N):- count_unifiable_instances(T, X, N).

% Count the number of variant instances in a list
count_variant_instances([], , 0).
count_variant_instances([H|T], X, N):- are_variant(H, X), !, count_variant_instances(T, X, N1), N is N1 + 1.
count_variant_instances([|T], X, N):- count_variant_instances(T, X, N).

% Check if a term is a member of a list of terms
term_member(Term, List):- member(T, List), T =@= Term.

% Check if a term is a member of a list of terms, considering only unifiable terms
term_member_unifiable(Term, List):- member(T, List), are_unifiable(T, Term).

% Check if a term is a member of a list of terms, considering only variant terms
term_member_variant(Term, List):- member(T, List), are_variant(T, Term).





Douglas Miles
continue
% Find the first unifiable term in a list
find_first_unifiable(, [], false).
find_first_unifiable(X, [H|T], H):- are_unifiable(X, H), !.
find_first_unifiable(X, [|T], R):- find_first_unifiable(X, T, R).

% Find the first variant term in a list
find_first_variant(, [], false).
find_first_variant(X, [H|T], H):- are_variant(X, H), !.
find_first_variant(X, [|T], R):- find_first_variant(X, T, R).

% Remove all unifiable instances from a list
remove_unifiable_instances(, [], []).
remove_unifiable_instances(X, [H|T], [H|R]):- + are_unifiable(X, H), !, remove_unifiable_instances(X, T, R).
remove_unifiable_instances(X, [|T], R):- remove_unifiable_instances(X, T, R).

% Remove all variant instances from a list
remove_variant_instances(, [], []).
remove_variant_instances(X, [H|T], [H|R]):- + are_variant(X, H), !, remove_variant_instances(X, T, R).
remove_variant_instances(X, [|T], R):- remove_variant_instances(X, T, R).

% Replace all unifiable instances in a list
replace_unifiable_instances(_, _, [], []).
replace_unifiable_instances(X, Y, [H|T], [Y|R]):- are_unifiable(X, H), !, replace_unifiable_instances(X, Y, T, R).
replace_unifiable_instances(X, Y, [H|T], [H|R]):- replace_unifiable_instances(X, Y, T, R).

% Replace all variant instances in a list
replace_variant_instances(_, _, [], []).
replace_variant_instances(X, Y, [H|T], [Y|R]):- are_variant(X, H), !, replace_variant_instances(X, Y, T, R).
replace_variant_instances(X, Y, [H|T], [H|R]):- replace_variant_instances(X, Y, T, R).

% This set of predicates provides various operations for lists of terms, considering unifiable or variant terms. Some predicates include counting instances, finding the first occurrence, removing instances, and replacing instances. These predicates will help in handling lists of terms and making manipulations based on unifiability or variants.






% Check if a term should be deduced
should_deduce(Term):- ok_deduce(Term), + + (+ deduce(Term)).

% Deduce a term if it should be deduced
deduce(Term):- should_deduce(Term),!.
deduce(Term):- fail.

% Generate the is_for_ilp/4 facts
generate_is_for_ilp(TestID, IO, Obj, P):-
is_accompany_changed_db(TestID, IO, Obj, P),
(ok_notice(P) -> (is_for_ilp(TestID, IO, Obj, P); fail); fail).

% Generate the is_for_ilp/4 facts for all TestIDs and IOs
generate_all_is_for_ilp:-
forall(test(TestID, IO), forall(is_accompany_changed_db(TestID, IO, Obj, P),
generate_is_for_ilp(TestID, IO, Obj, P))).

% Clear all is_for_ilp/4 facts
clear_is_for_ilp:- retractall(is_for_ilp(_, _, _, _)).

% Store the is_for_ilp/4 facts for a given TestID and IO
store_is_for_ilp(TestID, IO):-
forall(is_accompany_changed_db(TestID, IO, Obj, P),
assertz(is_for_ilp(TestID, IO, Obj, P))).

% Update the is_for_ilp/4 facts based on the given TestID and IO
update_is_for_ilp(TestID, IO):-
clear_is_for_ilp,
store_is_for_ilp(TestID, IO).

% Predicate to find all unique instances of G
variant_list_to_set(L, S):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S).

% Predicate to find all unique instances of G in a list
variant_list_to_set_list(L, S):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S).

% Predicate to find all unique instances of G in a list, and count them
variant_list_to_set_list_and_count(L, S, N):- findall(E, (member(E, L), + (member(E2, L), E =@= E2, E =@= E2)), S), length(S, N).

% Helper predicate to create a unifiable version of a compound term
make_unifiable_cc(X1, U1):- compound_name_arguments(X1, F, L1), maplist(make_unifiable_u, L1, L2), compound_name_arguments(U1, F, L2).

% Check if two terms have the same functor
same_functor(X1, X2):- compound_name_arity(X1, F, N), compound_name_arity(X2, F, N).





Douglas Miles
continue
% Check if two terms have the same functor and arguments
same_functor_and_args(X1, X2):- compound_name_arguments(X1, F, L1), compound_name_arguments(X2, F, L2), maplist(same_functor_and_args_u, L1, L2).

% Check if two terms have the same functor and arguments (helper for unifiable terms)
same_functor_and_args_u(X1, X2):- (nonvar(X1) -> same_functor_and_args(X1, X2); true).

% Check if two terms are unifiable
are_unifiable(X1, X2):- make_unifiable_cc(X1, U1), make_unifiable_cc(X2, U2), same_functor(U1, U2), + + (U1 = U2).

% Check if two terms are unifiable (helper for unifiable terms)
are_unifiable_u(X1, X2):- (nonvar(X1) -> are_unifiable(X1, X2); true).

% Create a unifiable version of a term (helper for unifiable terms)
make_unifiable_u(X1, X2):- (nonvar(X1) -> make_unifiable_cc(X1, X2); X2 = X1).

% Check if two terms are variant
are_variant(X1, X2):- make_unifiable_cc(X1, U1), make_unifiable_cc(X2, U2), U1 =@= U2.

% Count the number of unifiable instances in a list
count_unifiable_instances([], , 0).
count_unifiable_instances([H|T], X, N):- are_unifiable(H, X), !, count_unifiable_instances(T, X, N1), N is N1 + 1.
count_unifiable_instances([|T], X, N):- count_unifiable_instances(T, X, N).

% Count the number of variant instances in a list
count_variant_instances([], , 0).
count_variant_instances([H|T], X, N):- are_variant(H, X), !, count_variant_instances(T, X, N1), N is N1 + 1.
count_variant_instances([|T], X, N):- count_variant_instances(T, X, N).

% Check if a term is a member of a list of terms
term_member(Term, List):- member(T, List), T =@= Term.

% Check if a term is a member of a list of terms, considering only unifiable terms
term_member_unifiable(Term, List):- member(T, List), are_unifiable(T, Term).

% Check if a term is a member of a list of terms, considering only variant terms
term_member_variant(Term, List):- member(T, List), are_variant(T, Term).
