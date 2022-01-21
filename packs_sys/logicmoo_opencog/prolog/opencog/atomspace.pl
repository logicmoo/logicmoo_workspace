
%%% individual inferencerules

% (1) "revision" merges its two premises into a conclusion;
% (2) "choice"opencog_selects one of its two premises as a conclusion;
% (3) "inference" generates a conclusion from one or two premises.

opencog_ctx(default).

%revision/3
opencog:revision([S, T1], [S, T2], [S, T]):- opencog_revision([S, T1], [S, T2], [S, T]).

opencog_revision([S, T1], [S, T2], [S, T]):-
 	opencog_z_f_rev(T1, T2, T).

%OPENCOG_AS choice/3
opencog:choice(X, Y, Z):- opencog_choice(X, Y, Z).

opencog_choice([S, [F1, C1]], [S, [_F2, C2]], [S, [F1, C1]]):-
 	C1 >= C2, !.
opencog_choice([S, [_F1, C1]], [S, [F2, C2]], [S, [F2, C2]]):-
 	C1 < C2, !.
opencog_choice([S1, T1], [S2, T2], [S1, T1]):-
 	S1 \= S2, opencog_z_f_exp(T1, E1), opencog_z_f_exp(T2, E2), E1 >= E2, !.
opencog_choice([S1, T1], [S2, T2], [S2, T2]):-
 	S1 \= S2, opencog_z_f_exp(T1, E1), opencog_z_f_exp(T2, E2), E1 < E2, !.


%OPENCOG_AS infer-ence/2 (simplified version)
opencog:infer(T1, T):- opencog_infer(T1, T).

opencog_infer(T1, T):-  opencog_ctx(Ctx), opencog_inference(Ctx, [T1, [1, 0.9]], T).

opencog_infer(
          inheritance(W1, ext_image(ext_image(represent, [nil, inheritance(product([X, T2]), R)]), [nil, W2, W3])), 
          inheritance(W1, ext_image(represent, [nil, X])), 
             [inheritance(ext_image(represent, [nil, Y]), 
                          ext_image(ext_image(represent, [nil, inheritance(product([Y, T2]), R)]), [nil, W2, W3])),
           V]):-
   opencog_z_f_ind([1, 0.9], [1, 0.9], V), !.

opencog_infer(inheritance(W3, ext_image(ext_image(represent, [nil, inheritance(product([T1, X]), R)]), [W1, W2, nil])), inheritance(W3, ext_image(represent, [nil, X])), [inheritance(ext_image(represent, [nil, Y]), ext_image(ext_image(represent, [nil, inheritance(product([T1, Y]), R)]), [W1, W2, nil])), V]):-
 opencog_z_f_ind([1, 0.9], [1, 0.9], V), !.

opencog_infer(T1, T2, T):-  opencog_ctx(Ctx), opencog_inference(Ctx, [T1, [1, 0.9]], [T2, [1, 0.9]], T).


%OPENCOG_AS inference/2
opencog:inference(T1, T):- opencog_ctx(Ctx), opencog_inference(Ctx, T1, T).

%% immediate inference

opencog_inference(_Ctx, [inheritance(S, P), T1], [inheritance(P, S), T]):-
 	opencog_z_f_cnv(T1, T).
opencog_inference(_Ctx, [implication(S, P), T1], [implication(P, S), T]):-
 	opencog_z_f_cnv(T1, T).
opencog_inference(_Ctx, [implication(negation(S), P), T1], [implication(negation(P), S), T]):-
 	opencog_z_f_cnt(T1, T).

opencog_inference(_Ctx, [negation(S), T1], [S, T]):-
 	opencog_z_f_neg(T1, T).
opencog_inference(_Ctx, [S, [F1, C1]], [negation(S), T]):-
 	F1 < 0.5, opencog_z_f_neg([F1, C1], T).

%% structural inference

opencog_inference(_Ctx, [S1, T], [S, T]):-
 	opencog_z_reduce(S1, S), S1 \== S, !.
opencog_inference(Ctx, [S1, T], [S, T]):-
 	opencog_equivalence(Ctx, S1, S);opencog_equivalence(Ctx, S, S1).

opencog_inference(Ctx, P, C):-
 	opencog_inference(Ctx, P, [S, [1, 1]], C), call(S).
opencog_inference(Ctx, P, C):-
 	opencog_inference(Ctx, [S, [1, 1]], P, C), call(S).


% inference/3
opencog:inference(X, Y, Z):- opencog_ctx(Ctx), opencog_inference(Ctx, X, Y, Z).


%% inheritance-based syllogism

opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, opencog_z_f_ded(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(S, P), T]):-
 	S \= P, opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(M, S), T2], [inheritance(S, P), T]):-
 	S \= P, opencog_z_f_exe(T1, T2, T).

%% similarity from inheritance

opencog_inference(_Ctx, [inheritance(S, P), T1], [inheritance(P, S), T2], [similarity(S, P), T]):-
 	opencog_z_f_int(T1, T2, T).

%% similarity-based syllogism

opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [similarity(S, P), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [similarity(S, P), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [similarity(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, opencog_z_f_ana(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [similarity(S, M), T2], [inheritance(P, S), T]):-
 	S \= P, opencog_z_f_ana(T1, T2, T).
opencog_inference(_Ctx, [similarity(M, P), T1], [similarity(S, M), T2], [similarity(S, P), T]):-
 	S \= P, opencog_z_f_res(T1, T2, T).

%% inheritance-based composition

opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, opencog_z_reduce(int_intersection([P, S]), N), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, opencog_z_reduce(ext_intersection([P, S]), N), opencog_z_f_uni(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, opencog_z_reduce(int_difference(P, S), N), opencog_z_f_dif(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, opencog_z_reduce(ext_intersection([P, S]), N), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, opencog_z_reduce(int_intersection([P, S]), N), opencog_z_f_uni(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, opencog_z_reduce(ext_difference(P, S), N), opencog_z_f_dif(T1, T2, T).

%% inheirance-based decomposition

opencog_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_intersection(L), M), T2], [inheritance(P, M), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(int_intersection(N), P), opencog_z_f_pnn(T1, T2, T).
opencog_inference(_Ctx, [inheritance(S, M), T1], [inheritance(ext_intersection(L), M), T2], [inheritance(P, M), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(ext_intersection(N), P), opencog_z_f_npp(T1, T2, T).
opencog_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_difference(S, P), M), T2], [inheritance(P, M), T]):-
 	atom(S), atom(P), opencog_z_f_pnp(T1, T2, T).
opencog_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_difference(P, S), M), T2], [inheritance(P, M), T]):-
 	atom(S), atom(P), opencog_z_f_nnn(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_intersection(L)), T2], [inheritance(M, P), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(ext_intersection(N), P), opencog_z_f_pnn(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, int_intersection(L)), T2], [inheritance(M, P), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(int_intersection(N), P), opencog_z_f_npp(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_difference(S, P)), T2], [inheritance(M, P), T]):-
 	atom(S), atom(P), opencog_z_f_pnp(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_difference(P, S)), T2], [inheritance(M, P), T]):-
 	atom(S), atom(P), opencog_z_f_nnn(T1, T2, T).

%% implication-based syllogism

opencog_inference(_Ctx, [implication(M, P), T1], [implication(S, M), T2], [implication(S, P), T]):-
 	S \= P, opencog_z_f_ded(T1, T2, T).
opencog_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(S, P), T]):-
 	S \= P, opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(S, P), T]):-
 	S \= P, opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [implication(P, M), T1], [implication(M, S), T2], [implication(S, P), T]):-
 	S \= P, opencog_z_f_exe(T1, T2, T).

%% implication to equivalence

opencog_inference(_Ctx, [implication(S, P), T1], [implication(P, S), T2], [equivalence(S, P), T]):-
 	opencog_z_f_int(T1, T2, T).

%% equivalence-based syllogism

opencog_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [equivalence(S, P), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [equivalence(S, P), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [implication(M, P), T1], [equivalence(S, M), T2], [implication(S, P), T]):-
 	S \= P, opencog_z_f_ana(T1, T2, T).
opencog_inference(_Ctx, [implication(P, M), T1], [equivalence(S, M), T2], [implication(P, S), T]):-
 	S \= P, opencog_z_f_ana(T1, T2, T).
opencog_inference(_Ctx, [equivalence(M, P), T1], [equivalence(S, M), T2], [equivalence(S, P), T]):-
 	S \= P, opencog_z_f_res(T1, T2, T).

%% implication-based composition

opencog_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]):-
 	S \= P, opencog_z_reduce(disjunction([P, S]), N), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]):-
 	S \= P, opencog_z_reduce(conjunction([P, S]), N), opencog_z_f_uni(T1, T2, T).
opencog_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]):-
 	S \= P, opencog_z_reduce(conjunction([P, S]), N), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]):-
 	S \= P, opencog_z_reduce(disjunction([P, S]), N), opencog_z_f_uni(T1, T2, T).

%% implication-based decomposition

opencog_inference(_Ctx, [implication(S, M), T1], [implication(disjunction(L), M), T2], [implication(P, M), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(disjunction(N), P), opencog_z_f_pnn(T1, T2, T).
opencog_inference(_Ctx, [implication(S, M), T1], [implication(conjunction(L), M), T2], [implication(P, M), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(conjunction(N), P), opencog_z_f_npp(T1, T2, T).
opencog_inference(_Ctx, [implication(M, S), T1], [implication(M, conjunction(L)), T2], [implication(M, P), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(conjunction(N), P), opencog_z_f_pnn(T1, T2, T).
opencog_inference(_Ctx, [implication(M, S), T1], [implication(M, disjunction(L)), T2], [implication(M, P), T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(disjunction(N), P), opencog_z_f_npp(T1, T2, T).

%% conditioatomspace syllogism

opencog_inference(_Ctx, [implication(M, P), T1], [M, T2], [P, T]):-
 	opencog_z_ground(P), opencog_z_f_ded(T1, T2, T).
opencog_inference(_Ctx, [implication(P, M), T1], [M, T2], [P, T]):-
 	opencog_z_ground(P), opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [M, T1], [equivalence(S, M), T2], [S, T]):-
 	opencog_z_ground(S), opencog_z_f_ana(T1, T2, T).

%% conditioatomspace composition

opencog_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	C == implication(S, P), opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	C == equivalence(S, P), opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	opencog_z_reduce(conjunction([P, S]), N), N == C, opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	opencog_z_reduce(disjunction([P, S]), N), N == C, opencog_z_f_uni(T1, T2, T).

%% propositioatomspace decomposition

opencog_inference(_Ctx, [S, T1], [conjunction(L), T2], [P, T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(conjunction(N), P), opencog_z_f_pnn(T1, T2, T).
opencog_inference(_Ctx, [S, T1], [disjunction(L), T2], [P, T]):-
 	opencog_z_ground(S), opencog_z_ground(L), member(S, L), delete(L, S, N), opencog_z_reduce(disjunction(N), P), opencog_z_f_npp(T1, T2, T).

%% multi-conditioatomspace syllogism

opencog_inference(_Ctx, [implication(conjunction(L), C), T1], [M, T2], [implication(P, C), T]):-
 	nonvar(L), member(M, L), opencog_subtract(L, [M], A), A \= [], opencog_z_reduce(conjunction(A), P), opencog_z_f_ded(T1, T2, T).
opencog_inference(_Ctx, [implication(conjunction(L), C), T1], [implication(P, C), T2], [M, T]):-
 	opencog_z_ground(L), member(M, L), opencog_subtract(L, [M], A), A \= [], opencog_z_reduce(conjunction(A), P), opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [implication(conjunction(L), C), T1], [M, T2], [S, T]):-
 	S == implication(conjunction([M|L]), C), opencog_z_f_ind(T1, T2, T).

opencog_inference(_Ctx, [implication(conjunction(Lm), C), T1], [implication(A, M), T2], [implication(P, C), T]):-
 	nonvar(Lm), opencog_z_replace(Lm, M, La, A), opencog_z_reduce(conjunction(La), P), opencog_z_f_ded(T1, T2, T).
opencog_inference(_Ctx, [implication(conjunction(Lm), C), T1], [implication(conjunction(La), C), T2], [implication(A, M), T]):-
 	nonvar(Lm), opencog_z_replace(Lm, M, La, A), opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [implication(conjunction(La), C), T1], [implication(A, M), T2], [implication(P, C), T]):-
 	nonvar(La), opencog_z_replace(Lm, M, La, A), opencog_z_reduce(conjunction(Lm), P), opencog_z_f_ind(T1, T2, T).

%% variable introduction

opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [implication(inheritance(X, S), inheritance(X, P)), T]):-
 	S \= P, opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [implication(inheritance(P, X), inheritance(S, X)), T]):-
 	S \= P, opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [equivalence(inheritance(X, S), inheritance(X, P)), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [equivalence(inheritance(P, X), inheritance(S, X)), T]):-
 	S \= P, opencog_z_f_com(T1, T2, T).
opencog_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)]), T]):-
 	S \= P, opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))]), T]):-
 	S \= P, opencog_z_f_int(T1, T2, T).

%% 2nd variable introduction

opencog_inference(_Ctx, [implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [implication(conjunction([A, inheritance(X, S)]), inheritance(X, P)), T]):-
 	S \= P, M1 == M2, A \= inheritance(M2, S), opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [conjunction([implication(A, inheritance(var(Y, []), P)), inheritance(var(Y, []), S)]), T]):-
 	S \= P, M1 == M2, A \= inheritance(M2, S), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [implication(inheritance(Y, S), conjunction([inheritance(Y, P2)|L3])), T]):-
 	opencog_subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, opencog_z_dependant(P, Y, P2), opencog_z_dependant(L2, Y, L3), opencog_z_f_ind(T1, T2, T).
opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)|L2]), T]):-
 	opencog_subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, opencog_z_f_int(T1, T2, T).

opencog_inference(_Ctx, [implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [implication(conjunction([A, inheritance(P, X)]), inheritance(S, X)), T]):-
 	S \= P, M1 == M2, A \= inheritance(S, M2), opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [conjunction([implication(A, inheritance(P, var(Y, []))), inheritance(S, var(Y, []))]), T]):-
 	S \= P, M1 == M2, A \= inheritance(S, M2), opencog_z_f_int(T1, T2, T).
opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [implication(inheritance(S, Y), conjunction([inheritance(P2, Y)|L3])), T]):-
 	opencog_subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, opencog_z_dependant(P, Y, P2), opencog_z_dependant(L2, Y, L3), opencog_z_f_abd(T1, T2, T).
opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))|L2]), T]):-
 	opencog_subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, opencog_z_f_int(T1, T2, T).

%% dependant variable elimination

opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [C, T]):-
 	opencog_subtract(L1, [inheritance(var(N, D), S)], L2), L1 \= L2,
 	replace_var(L2, var(N, D), L3, M), opencog_z_reduce(conjunction(L3), C), opencog_z_f_cnv(T2, T0), opencog_z_f_ana(T1, T0, T).
opencog_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [C, T]):-
 	opencog_subtract(L1, [inheritance(S, var(N, D))], L2), L1 \= L2,
 	replace_var(L2, var(N, D), L3, M), opencog_z_reduce(conjunction(L3), C), opencog_z_f_cnv(T2, T0), opencog_z_f_ana(T1, T0, T).

replace_var([], _, [], _).
replace_var([inheritance(S1, P)|T1], S1, [inheritance(S2, P)|T2], S2):-
 	replace_var(T1, S1, T2, S2).
replace_var([inheritance(S, P1)|T1], P1, [inheritance(S, P2)|T2], P2):-
 	replace_var(T1, P1, T2, P2).
replace_all([H|T1], H1, [H|T2], H2):-
 	replace_var(T1, H1, T2, H2).



%%% Theorems in IL:

%OPENCOG_AS inheritance/2
opencog:inheritance(X, Y):- opencog_ctx(Ctx), opencog_inheritance(Ctx, X, Y).

opencog_inheritance(_Ctx, ext_intersection(Ls), P):-
 	opencog_z_include([P], Ls).
opencog_inheritance(_Ctx, S, int_intersection(Lp)):-
 	opencog_z_include([S], Lp).
opencog_inheritance(_Ctx, ext_intersection(S), ext_intersection(P)):-
 	opencog_z_include(P, S), P \= [_].
opencog_inheritance(_Ctx, int_intersection(S), int_intersection(P)):-
 	opencog_z_include(S, P), S \= [_].
opencog_inheritance(_Ctx, ext_set(S), ext_set(P)):-
 	opencog_z_include(S, P).
opencog_inheritance(_Ctx, int_set(S), int_set(P)):-
 	opencog_z_include(P, S).

opencog_inheritance(_Ctx, ext_difference(S, P), S):-
 	opencog_z_ground(S), opencog_z_ground(P).
opencog_inheritance(_Ctx, S, int_difference(S, P)):-
 	opencog_z_ground(S), opencog_z_ground(P).

opencog_inheritance(_Ctx, product(L1), R):-
 	opencog_z_ground(L1), member(ext_image(R, L2), L1), opencog_z_replace(L1, ext_image(R, L2), L2).
opencog_inheritance(_Ctx, R, product(L1)):-
 	opencog_z_ground(L1), member(int_image(R, L2), L1), opencog_z_replace(L1, int_image(R, L2), L2).

%OPENCOG_AS similarity/2
opencog:similarity(X, Y):- opencog_ctx(Ctx), opencog_similarity(Ctx, X, Y).

opencog_similarity(_Ctx, X, Y):-
 	opencog_z_ground(X), opencog_z_reduce(X, Y), X \== Y, !.

opencog_similarity(_Ctx, ext_intersection(L1), ext_intersection(L2)):-
 	opencog_z_same_set(L1, L2).
opencog_similarity(_Ctx, int_intersection(L1), int_intersection(L2)):-
 	opencog_z_same_set(L1, L2).
opencog_similarity(_Ctx, ext_set(L1), ext_set(L2)):-
 	opencog_z_same_set(L1, L2).
opencog_similarity(_Ctx, int_set(L1), int_set(L2)):-
 	opencog_z_same_set(L1, L2).

%OPENCOG_AS implication/2
opencog:implication(X, Y):- opencog_ctx(Ctx), opencog_implication(Ctx, X, Y).

opencog_implication(_Ctx, similarity(S, P), inheritance(S, P)).
opencog_implication(_Ctx, equivalence(S, P), implication(S, P)).

opencog_implication(_Ctx, conjunction(L), M):-
 	opencog_z_ground(L), member(M, L).
opencog_implication(_Ctx, M, disjunction(L)):-
 	opencog_z_ground(L), member(M, L).

opencog_implication(_Ctx, conjunction(L1), conjunction(L2)):-
 	opencog_z_ground(L1), opencog_z_ground(L2), subset(L2, L1).
opencog_implication(_Ctx, disjunction(L1), disjunction(L2)):-
 	opencog_z_ground(L1), opencog_z_ground(L2), subset(L1, L2).

opencog_implication(_Ctx, inheritance(S, P), inheritance(ext_intersection(Ls), ext_intersection(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, inheritance(S, P), inheritance(int_intersection(Ls), int_intersection(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, similarity(S, P), similarity(ext_intersection(Ls), ext_intersection(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, similarity(S, P), similarity(int_intersection(Ls), int_intersection(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).

opencog_implication(_Ctx, inheritance(S, P), inheritance(ext_difference(S, M), ext_difference(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), inheritance(int_difference(S, M), int_difference(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, similarity(S, P), similarity(ext_difference(S, M), ext_difference(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, similarity(S, P), similarity(int_difference(S, M), int_difference(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), inheritance(ext_difference(M, P), ext_difference(M, S))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), inheritance(int_difference(M, P), int_difference(M, S))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, similarity(S, P), similarity(ext_difference(M, P), ext_difference(M, S))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, similarity(S, P), similarity(int_difference(M, P), int_difference(M, S))):-
 	opencog_z_ground(M).

opencog_implication(_Ctx, inheritance(S, P), negation(inheritance(S, ext_difference(M, P)))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, ext_difference(M, P)), negation(inheritance(S, P))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), negation(inheritance(int_difference(M, S), P))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(int_difference(M, S), P), negation(inheritance(S, P))):-
 	opencog_z_ground(M).

opencog_implication(_Ctx, inheritance(S, P), inheritance(ext_image(S, M), ext_image(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), inheritance(int_image(S, M), int_image(P, M))):-
 	opencog_z_ground(M).
opencog_implication(_Ctx, inheritance(S, P), inheritance(ext_image(M, Lp), ext_image(M, Ls))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).
opencog_implication(_Ctx, inheritance(S, P), inheritance(int_image(M, Lp), int_image(M, Ls))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).

opencog_implication(_Ctx, negation(M), negation(conjunction(L))):-
 	opencog_z_include([M], L).
opencog_implication(_Ctx, negation(disjunction(L)), negation(M)):-
 	opencog_z_include([M], L).

opencog_implication(_Ctx, implication(S, P), implication(conjunction(Ls), conjunction(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, implication(S, P), implication(disjunction(Ls), disjunction(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, equivalence(S, P), equivalence(conjunction(Ls), conjunction(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).
opencog_implication(_Ctx, equivalence(S, P), equivalence(disjunction(Ls), disjunction(Lp))):-
 	opencog_z_ground(Ls), opencog_z_ground(Lp), opencog_z_replace(Ls, S, L, P), opencog_z_same(L, Lp).


%OPENCOG_AS equivalence/2
equivalence(X, Y):- opencog_ctx(Ctx), opencog_equivalence(Ctx, X, Y).

opencog_equivalence(_Ctx, X, Y):-
 	opencog_z_ground(X), opencog_z_reduce(X, Y), X \== Y, !.

opencog_equivalence(_Ctx, similarity(S, P), similarity(P, S)).

opencog_equivalence(_Ctx, inheritance(S, ext_set([P])), similarity(S, ext_set([P]))).
opencog_equivalence(_Ctx, inheritance(int_set([S]), P), similarity(int_set([S]), P)).

opencog_equivalence(Ctx, inheritance(S, ext_intersection(Lp)), conjunction(L)):-
 	findall(opencog_inheritance(Ctx, S, P), member(P, Lp), L).
opencog_equivalence(Ctx, inheritance(int_intersection(Ls), P), conjunction(L)):-
 	findall(opencog_inheritance(Ctx, S, P), member(S, Ls), L).

opencog_equivalence(_Ctx, inheritance(S, ext_difference(P1, P2)),
 	    conjunction([inheritance(S, P1), negation(inheritance(S, P2))])).
opencog_equivalence(_Ctx, inheritance(int_difference(S1, S2), P),
 	    conjunction([inheritance(S1, P), negation(inheritance(S2, P))])).

opencog_equivalence(_Ctx, inheritance(product(Ls), product(Lp)), conjunction(L)):-
 	equ_product(Ls, Lp, L).

opencog_equivalence(_Ctx, inheritance(product([S|L]), product([P|L])), inheritance(S, P)):-
 	opencog_z_ground(L).
opencog_equivalence(Ctx, inheritance(S, P), inheritance(product([H|Ls]), product([H|Lp]))):-
 	opencog_z_ground(H), opencog_equivalence(Ctx, inheritance(product(Ls), product(Lp)), inheritance(S, P)).

opencog_equivalence(_Ctx, inheritance(product(L), R), inheritance(T, ext_image(R, L1))):-
 	opencog_z_replace(L, T, L1).
opencog_equivalence(_Ctx, inheritance(R, product(L)), inheritance(int_image(R, L1), T)):-
 	opencog_z_replace(L, T, L1).

opencog_equivalence(_Ctx, equivalence(S, P), equivalence(P, S)).

opencog_equivalence(_Ctx, equivalence(negation(S), P), equivalence(negation(P), S)).

opencog_equivalence(_Ctx, conjunction(L1), conjunction(L2)):-
 	opencog_z_same_set(L1, L2).
opencog_equivalence(_Ctx, disjunction(L1), disjunction(L2)):-
 	opencog_z_same_set(L1, L2).

opencog_equivalence(Ctx, implication(S, conjunction(Lp)), conjunction(L)):-
 	findall(opencog_implication(Ctx, S, P), member(P, Lp), L).
opencog_equivalence(Ctx, implication(disjunction(Ls), P), conjunction(L)):-
 	findall(opencog_implication(Ctx, S, P), member(S, Ls), L).

opencog_equivalence(Ctx, T1, T2):-
 	not(atom(T1)), not(atom(T2)), opencog_z_ground(T1), opencog_z_ground(T2),
 	T1 =.. L1, T2 =.. L2, opencog_equivalence_list(Ctx, L1, L2).

opencog_equivalence_list(_Ctx, L, L).
opencog_equivalence_list(Ctx, [H|L1], [H|L2]):-
 	opencog_equivalence_list(Ctx, L1, L2).
opencog_equivalence_list(Ctx, [H1|L1], [H2|L2]):-
 	opencog_similarity(Ctx, H1, H2), opencog_equivalence_list(Ctx, L1, L2).
opencog_equivalence_list(Ctx, [H1|L1], [H2|L2]):-
 	opencog_equivalence(Ctx, H1, H2), opencog_equivalence_list(Ctx, L1, L2).

% compound termopencog_structurereduction

opencog_z_reduce(similarity(ext_set([S]), ext_set([P])), similarity(S, P)):-
 	!.
opencog_z_reduce(similarity(int_set([S]), int_set([P])), similarity(S, P)):-
 	!.

opencog_z_reduce(instance(S, P), inheritance(ext_set([S]), P)):-
 	!.
opencog_z_reduce(property(S, P), inheritance(S, int_set([P]))):-
 	!.
opencog_z_reduce(inst_prop(S, P), inheritance(ext_set([S]), int_set([P]))):-
 	!.

opencog_z_reduce(ext_intersection([T]), T):-
 	!.
opencog_z_reduce(int_intersection([T]), T):-
 	!.

opencog_z_reduce(ext_intersection([ext_intersection(L1), ext_intersection(L2)]), ext_intersection(L)):-
 	opencog_union(L1, L2, L), !.
opencog_z_reduce(ext_intersection([ext_intersection(L1), L2]), ext_intersection(L)):-
 	opencog_union(L1, [L2], L), !.
opencog_z_reduce(ext_intersection([L1, ext_intersection(L2)]), ext_intersection(L)):-
 	opencog_union([L1], L2, L), !.
opencog_z_reduce(ext_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)):-
 	intersection(L1, L2, L), !.
opencog_z_reduce(ext_intersection([int_set(L1), int_set(L2)]), int_set(L)):-
 	opencog_union(L1, L2, L), !.

opencog_z_reduce(int_intersection([int_intersection(L1), int_intersection(L2)]), int_intersection(L)):-
 	opencog_union(L1, L2, L), !.
opencog_z_reduce(int_intersection([int_intersection(L1), L2]), int_intersection(L)):-
 	opencog_union(L1, [L2], L), !.
opencog_z_reduce(int_intersection([L1, int_intersection(L2)]), int_intersection(L)):-
 	opencog_union([L1], L2, L), !.
opencog_z_reduce(int_intersection([int_set(L1), int_set(L2)]), int_set(L)):-
 	intersection(L1, L2, L), !.
opencog_z_reduce(int_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)):-
 	opencog_union(L1, L2, L), !.

opencog_z_reduce(ext_difference(ext_set(L1), ext_set(L2)), ext_set(L)):-
 	opencog_subtract(L1, L2, L), !.
opencog_z_reduce(int_difference(int_set(L1), int_set(L2)), int_set(L)):-
 	opencog_subtract(L1, L2, L), !.

opencog_z_reduce(product(product(L), T), product(L1)):-
 	append(L, [T], L1), !.

opencog_z_reduce(ext_image(product(L1), L2), T1):-
 	member(T1, L1), opencog_z_replace(L1, T1, L2), !.
opencog_z_reduce(int_image(product(L1), L2), T1):-
 	member(T1, L1), opencog_z_replace(L1, T1, L2), !.

opencog_z_reduce(negation(negation(S)), S):-
 	!.

opencog_z_reduce(conjunction([T]), T):-
 	!.
opencog_z_reduce(disjunction([T]), T):-
 	!.

opencog_z_reduce(conjunction([conjunction(L1), conjunction(L2)]), conjunction(L)):-
 	opencog_union(L1, L2, L), !.
opencog_z_reduce(conjunction([conjunction(L1), L2]), conjunction(L)):-
 	opencog_union(L1, [L2], L), !.
opencog_z_reduce(conjunction([L1, conjunction(L2)]), conjunction(L)):-
 	opencog_union([L1], L2, L), !.

opencog_z_reduce(disjunction(disjunction(L1), disjunction(L2)), disjunction(L)):-
 	opencog_union(L1, L2, L), !.
opencog_z_reduce(disjunction(disjunction(L1), L2), disjunction(L)):-
 	opencog_union(L1, [L2], L), !.
opencog_z_reduce(disjunction(L1, disjunction(L2)), disjunction(L)):-
 	opencog_union([L1], L2, L), !.

opencog_z_reduce(X, X).


%opencog_union(X,Y,Z):- (nonvar(X);nonvar(Z)), catch(union(X,Y,Z),_,fail).
opencog_union(X,Y,Z):- catch(union(X,Y,Z),_,fail).
%opencog_subtract(X,Y,Z):- (nonvar(X);nonvar(Z)), catch(subtract(X,Y,Z),_,fail).
opencog_subtract(X,Y,Z):- catch(subtract(X,Y,Z),_,fail).

%%% Argument processing

equ_product([], [], []).
equ_product([T|Ls], [T|Lp], L):-
 	equ_product(Ls, Lp, L), !.
equ_product([S|Ls], [P|Lp], [inheritance(S, P)|L]):-
 	equ_product(Ls, Lp, L).

opencog_z_same_set(L1, L2):-
 	L1 \== [], L1 \== [_], opencog_z_same(L1, L2), L1 \== L2.

opencog_z_same([], []).
opencog_z_same(L, [H|T]):-
 	member(H, L), opencog_subtract(L, [H], L1), opencog_z_same(L1, T).

opencog_z_include(L1, L2):-
 	opencog_z_ground(L2), include1_2(L1, L2), L1 \== [], L1 \== L2.

 include1_2([], _).
 include1_2([H|T1], [H|T2]):-
 	include1_2(T1, T2).
 include1_2([H1|T1], [H2|T2]):-
 	H2 \== H1, include1_2([H1|T1], T2).

opencog_z_not_member(_, []).
opencog_z_not_member(C, [C|_]):-  !, fail.
opencog_z_not_member([S, T], [[S1, T]|_]):- opencog_equivalence(_Ctx, S, S1), !, fail.
opencog_z_not_member(C, [_|L]):- opencog_z_not_member(C, L).

opencog_z_replace([T|L], T, [nil|L]).
opencog_z_replace([H|L], T, [H|L1]):-
 	opencog_z_replace(L, T, L1).

opencog_z_replace([H1|T], H1, [H2|T], H2).
opencog_z_replace([H|T1], H1, [H|T2], H2):-
 	opencog_z_replace(T1, H1, T2, H2).

opencog_z_dependant(var(V, L), Y, var(V, [Y|L])):-
 	!.
opencog_z_dependant([H|T], Y, [H1|T1]):-
 	opencog_z_dependant(H, Y, H1), opencog_z_dependant(T, Y, T1), !.
opencog_z_dependant(inheritance(S, P), Y, inheritance(S1, P1)):-
 	opencog_z_dependant(S, Y, S1), opencog_z_dependant(P, Y, P1), !.
opencog_z_dependant(ext_image(R, A), Y, ext_image(R, A1)):-
 	opencog_z_dependant(A, Y, A1), !.
opencog_z_dependant(int_image(R, A), Y, int_image(R, A1)):-
 	opencog_z_dependant(A, Y, A1), !.
opencog_z_dependant(X, _, X).


%%% Truth-value functions

opencog_z_f_rev([F1, C1], [F2, C2], [F, C]):-
      C1 < 1,
      C2 < 1,
 	M1 is C1 / (1 - C1),
 	M2 is C2 / (1 - C2),
 	F is (M1 * F1 + M2 * F2) / (M1 + M2),
 	C is (M1 + M2) / (M1 + M2 + 1).

opencog_z_f_exp([F, C], E):-
 	E is C * (F - 0.5) + 0.5.

opencog_z_f_neg([F1, C1], [F, C1]):-
 	u_not(F1, F).

opencog_z_f_cnv([F1, C1], [1, C]):-
     u_and([F1, C1], W),
 	u_w2c(W, C).

opencog_z_f_cnt([F1, C1], [0, C]):-
 	u_not(F1, F0),
     u_and([F0, C1], W),
 	u_w2c(W, C).

opencog_z_f_ded([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2, F], C).

opencog_z_f_ana([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2, F2], C).

opencog_z_f_res([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_or([F1, F2], F0),
 	u_and([C1, C2, F0], C).

opencog_z_f_abd([F1, C1], [F2, C2], [F2, C]):-
 	u_and([F1, C1, C2], W),
 	u_w2c(W, C).

opencog_z_f_ind(T1, T2, T):-
 	opencog_z_f_abd(T2, T1, T).

opencog_z_f_exe([F1, C1], [F2, C2], [1, C]):-
 	u_and([F1, C1, F2, C2], W),
 	u_w2c(W, C).

opencog_z_f_com([0, _C1], [0, _C2], [0, 0]).
opencog_z_f_com([F1, C1], [F2, C2], [F, C]):-
 	u_or([F1, F2], F0),
 	F0 > 0,
 	F is F1 * F2 / F0,
 	u_and([F0, C1, C2], W),
 	u_w2c(W, C).

opencog_z_f_int([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2], C).

opencog_z_f_uni([F1, C1], [F2, C2], [F, C]):-
 	u_or([F1, F2], F),
 	u_and([C1, C2], C).

opencog_z_f_dif([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F0),
 	u_and([F1, F0], F),
 	u_and([C1, C2], C).

opencog_z_f_pnn([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F2n),
 	u_and([F1, F2n], Fn),
 	u_not(Fn, F),
 	u_and([Fn, C1, C2], C).

opencog_z_f_npp([F1, C1], [F2, C2], [F, C]):-
 	u_not(F1, F1n),
 	u_and([F1n, F2], F),
 	u_and([F, C1, C2], C).

opencog_z_f_pnp([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F2n),
 	u_and([F1, F2n], F),
 	u_and([F, C1, C2], C).

opencog_z_f_nnn([F1, C1], [F2, C2], [F, C]):-
 	u_not(F1, F1n),
 	u_not(F2, F2n),
 	u_and([F1n, F2n], Fn),
 	u_not(Fn, F),
 	u_and([Fn, C1, C2], C).

% Utility functions

u_not(N0, N):-
 	N is (1 - N0), !.

u_and([N], N).
u_and([N0 | Nt], N):-
 	u_and(Nt, N1), N is N0 * N1, !.

u_or([N], N).
u_or([N0 | Nt], N):-
 	u_or(Nt, N1), N is (N0 + N1 - N0 * N1), !.

u_w2c(W, C):-
 	K = 1, C is (W / (W + K)), !.





