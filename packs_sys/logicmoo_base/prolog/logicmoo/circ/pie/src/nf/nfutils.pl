
:- module(nfutils,
	  [c_is_range_restricted/1,
	   c_is_horn/1,
	   c_is_ground/1,
	   c_is_depth_increasing/1,
	   m_print_stats/1,
	   m_print_sig/1,
	   m_signature/3,
	   m_predicates/2,
	   m_switch_polarity/3,
	   m_add_equality/2,
	   m_add_equality_r_s_t/2,
	   m_add_equality_r_st/2,
	   m_add_equality_r_ts/2,
	   f_signature/3,
	   f_signature_pol/3,	   
	   substitutivity_axioms/3,
	   equality_rst_axioms_r_s_t/1,
	   equality_rst_axioms_r_st/1,
	   equality_rst_axioms_r_ts/1,
	   pfun_axioms/2,
	   c_split_pos_neg/3,
	   m_sort/2,
	   c_sort/2,
	   l_atom/2,
	   prolog_vars_to_atoms/2
	   ]).

:- use_module(nf).

m_print_stats(M) :-
	length(M, L),
	m_signature(M, Ps, Fs),	
	n_without_property(c_is_range_restricted, M, N_NR),
	n_without_property(c_is_horn, M, N_NH),
	n_without_property(c_is_ground, M, N_NG),
	n_with_property(c_is_depth_increasing, M, ND),	
	n_with_property(c_is_unit, M, NU),
	n_with_property(c_is_pos_nonunit, M, NP),
	n_with_property(c_is_neg, M, NN),
	n_with_property(c_is_non_horn_not_rr, M, NS),	
	percent(N_NR, L, P_NR),
	percent(N_NH, L, P_NH),
	percent(N_NG, L, P_NG),
	percent(ND, L, P_ND),
	percent(NU, L, P_NU),
	percent(NP, L, P_NP),
	percent(NS, L, P_NS),	
	percent(NN, L, P_NN),
	format('Clauses:~t ~w ~40|~n', [L]),
	format('Not Horn:~t ~w ~40|~t(~w%)~6+~n', [N_NH,P_NH]),
	format('Not ground:~t ~w ~40|~t(~w%)~6+~n', [N_NG,P_NG]),
	format('Not range restricted:~t ~w ~40|~t(~w%)~6+~n', [N_NR,P_NR]),
	format('Not range restricted non Horn:~t ~w ~40|~t(~w%)~6+~n',
	       [NS,P_NS]),
	format('Depth increasing:~t ~w ~40|~t(~w%)~6+~n', [ND,P_ND]),	
	format('Units:~t ~w ~40|~t(~w%)~6+~n', [NU,P_NU]),
	format('Positive Non-Units:~t ~w ~40|~t(~w%)~6+~n', [NP,P_NP]),
	format('Negative:~t ~w ~40|~t(~w%)~6+~n', [NN,P_NN]),
	print_signature_info(Ps, Fs).


print_signature_info(Ps, Fs) :-
	print_sig_info(Ps, 'Predicates'),
	split_skolems(Fs, Fs1, Fs1SK),
	print_sig_info(Fs1, 'Non-Skolem functions'),
	print_sig_info(Fs1SK, 'Skolem functions').

print_sig_info(Fs, Info) :-
	( Fs = [] ->
  	  format('~w:~t ~w ~40|~n', [Info, none])
	; setof(S-Fs1, setof(F, member(F/S, Fs), Fs1), Ss),
	  length(Fs, L),
  	  format('~w:~t ~w ~40|~n', [Info, L]),
	  ( member(A-S, Ss),
	    length(S, LS),
	    format('        with arity~t ~w: ~33|~t ~w ~7+~n', [A, LS]),
	    fail
	  ; true
	  )
	).

m_print_sig(M) :-
	m_signature(M, Ps, Fs),	
	print_sig(Ps, 'predicate'),
	split_skolems(Fs, Fs1, Fs2),
	print_sig(Fs1, 'function'),
	print_sig(Fs2, 'skolem function').

print_sig(Fs, Info) :-
	( Fs = [] ->
	  format('Empty signature for type: ~w~n', [Info])
	; setof(S-Fs1, setof(F, member(F/S, Fs), Fs1), Ss),
	  ( member(A-S, Ss),
	    ( member(Symbol, S),
              format('~q~t ~60|/ ~w ~w~n', [Symbol, A, Info]),
              fail
	    ; true
	    ),
	    fail
	  ; true
	  )
	).


split_skolems([FS|FSs], FSs1, [FS|FSs2]) :-
	is_skolem_function(FS),
	!,
	split_skolems(FSs, FSs1, FSs2).
split_skolems([FS|FSs], [FS|FSs1], FSs2) :-
	split_skolems(FSs, FSs1, FSs2).
split_skolems([], [], []).

is_skolem_function(F/_) :-
	sub_atom(F, 0, 2, _, sk),
	sub_atom(F, 2, _, 0, Rest),
	atom_chars(Rest, Chars),
	\+ ( member(C, Chars), \+ char_type(C, digit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percent(N, L, P) :-
	( L = 0 -> P = 100 ; P is round((N/L * 100))).

n_with_property(P, M, N) :-
	findall(k, ( member(C, M),
		     Call =.. [P, C],
		     call(Call) ),
		M1),
	length(M1, N).

n_without_property(P, M, N) :-
	findall(C, ( member(C, M),
		     Call =.. [P, C],
		     \+ call(Call) ),
		M1),
	length(M1, N).

c_is_range_restricted(C) :-
	split_pn(C, P, N),
	free_variables(P, HVs),
        free_variables(N, BVs),
        \+ ( member(V, HVs),
	       \+ ( member(V1, BVs), V == V1 ) ).

c_is_horn(C) :-
	split_pn(C, P, _),
	( P = []
	; P = [_]
	).

c_is_non_horn_not_rr(C) :-
	\+ c_is_horn(C),
	\+ c_is_range_restricted(C).

c_is_ground(C) :-
	ground(C).

split_pn([~(A)|Ls], P, [A|N]) :-
	!,
	split_pn(Ls, P, N).
split_pn([A|Ls], [A|P], N) :-
	split_pn(Ls, P, N).
split_pn([], [], []).


c_is_unit([_]).

c_is_neg(C) :-
	\+ (member(X, C), X \= ~(_)).

c_is_pos_nonunit([X,Y|Z]) :-
	X \= ~(_),
	Y \= ~(_),
	\+ (member(L, Z), L = ~(_)).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Matrix Signature
%%%% 
%%%% Result are lists of Symbol/Arity pairs.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_signature(M, Preds, Funs) :-
        mp(M, [], F1, [], P1),
        sort(F1, Funs),
        sort(P1, Preds).

mp([],F,F,P,P).
mp([C|Cs],F,F1,P,P1) :- cp(C,F,F2,P,P2), mp(Cs,F2,F1,P2,P1).
cp([],F,F,P,P).
cp([L|Ls],F,F1,P,P1) :- lp(L,F,F2,P,P2), cp(Ls,F2,F1,P2,P1).
lp(~(L), F, F1, P, P1) :- !, lp(L, F, F1, P, P1).
lp(L, F, F1, P, [Op/N|P]) :- functor(L, Op, N), map_f(N,L,F,F1).
map_f(0, _, F, F) :- !.
map_f(N, A, F, F1) :-
        arg(N, A, E), f(E, F, F2), N1 is N-1, map_f(N1, A, F2, F1).
f(E, F, F) :- var(E), !.
f(L, F, [Op/N|F1]) :- functor(L, Op, N), map_f(N,L,F,F1).

m_predicates(M, Preds) :-
        mp_1(M, [], P1),
        sort(P1, Preds).

mp_1([],P,P).
mp_1([C|Cs],P,P1) :- cp_1(C,P,P2), mp_1(Cs,P2,P1).
cp_1([],P,P).
cp_1([L|Ls],P,P1) :- lp_1(L,P,P2), cp_1(Ls,P2,P1).
lp_1(~(L), P, P1) :- !, lp_1(L, P, P1).
lp_1(L, P, [Op/N|P]) :- functor(L, Op, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Formula Signature
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f_signature(F, Preds, Funs) :-
        fp(F, [], [], F1, [], P1),
        sort(F1, Funs),
        sort(P1, Preds).

fp(true, _, F, F, P, P) :-
	!.
fp(false, _, F, F, P, P) :-
	!.
fp(all(Vs, G), V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fp(G, V1, F, F1, P, P1).
fp(ex(Vs, G), V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fp(G, V1, F, F1, P, P1).
fp(all2(Vs, G), V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fp(G, V1, F, F1, P, P1).
fp(ex2(Vs, G), V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fp(G, V1, F, F1, P, P1).
fp(~(G), V, F, F1, P, P1) :-
	!,
	fp(G, V, F, F1, P, P1).
fp((G1, G2),      V, F, F1, P, P1) :- !, fp_binop(G1, G2, V, F, F1, P, P1).
fp((G1; G2),      V, F, F1, P, P1) :- !, fp_binop(G1, G2, V, F, F1, P, P1).
fp((G1 -> G2),    V, F, F1, P, P1) :- !, fp_binop(G1, G2, V, F, F1, P, P1).
fp('<-'(G1, G2),  V, F, F1, P, P1) :- !, fp_binop(G1, G2, V, F, F1, P, P1).
fp('<->'(G1, G2), V, F, F1, P, P1) :- !, fp_binop(G1, G2, V, F, F1, P, P1).
fp(L, _, F, F, P, P) :- is_builtin_atom(L), !.
fp(L, V, F, F1, P, P1) :-
	functor(L, Op, N),
	( absmemberchk(Op, V) ->
	  P1 = P
	; P1 = [Op/N|P]
	),
	map_fv(N, V, L, F, F1).

fp_binop(G1, G2, V, F, F1, P, P1) :-
	fp(G1, V, F, F2, P, P2),
	fp(G2, V, F2, F1, P2, P1).

map_fv(0, _, _, F, F) :-
	!.
map_fv(N, V, A, F, F1) :-
        arg(N, A, E),
	fv(E, V, F, F2),
	N1 is N-1,
	map_fv(N1, V, A, F2, F1).
fv(E, V, F, F) :-
	absmemberchk(E, V),
	!.
fv(L, V, F, F1) :-
	functor(L, Op, N),
	( absmemberchk(Op, V) ->
	  F1 = F2
	; F1 = [Op/N|F2]
	),
	map_fv(N, V, L, F, F2).

add_vars(X, Vs, [X|Vs]) :-
	var(X), %% No legal Form syntax, but some people may use prolog vars.
	!.
add_vars([], Vs, Vs) :-
	!.
add_vars([X|Xs], Vs, Vs1) :-
	!,
	append([X|Xs], Vs, Vs1).
add_vars(X, Vs, [X|Vs]).

absmemberchk(X, [Y|_]) :-
	X == Y,
	!.
absmemberchk(X, [_|Ys]) :-
	absmemberchk(X, Ys).

is_builtin_atom('$prolog'(_)).
is_builtin_atom('$prolog'(_,_)).
is_builtin_atom('$prolog_post'(_)).
is_builtin_atom('$prolog_post'(_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Formula Signature -- With Polarity
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f_signature_pol(F, Preds, Funs) :-
        fpl(F, 1, [], [], F1, [], P1),
        sort(F1, Funs),
        sort(P1, Preds).

fpl(true, _, _, F, F, P, P) :-
	!.
fpl(false, _, _, F, F, P, P) :-
	!.
fpl(all(Vs, G), S, V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fpl(G, S, V1, F, F1, P, P1).
fpl(ex(Vs, G), S, V, F, F1, P, P1) :-
	!,
	add_vars(Vs, V, V1),
	fpl(G, S, V1, F, F1, P, P1).
fpl(~(G), S, V, F, F1, P, P1) :-
	!,
	S1 is S * -1,
	fpl(G, S1, V, F, F1, P, P1).
fpl((G1, G2),      S, V, F, F1, P, P1) :-
	!, fpl_binop(G1, G2, S, V, F, F1, P, P1).
fpl((G1; G2), S, V, F, F1, P, P1) :-
	!, fpl_binop(G1, G2, S, V, F, F1, P, P1).
fpl('->'(G1, G2),  S, V, F, F1, P, P1) :-
	!, fpl_binop(~(G1), G2, S, V, F, F1, P, P1).
fpl('<-'(G1, G2),  S, V, F, F1, P, P1) :-
	!, fpl_binop(G1, ~(G2), S, V, F, F1, P, P1).
fpl('<->'(G1, G2), _, V, F, F1, P, P1) :-
	!, fpl_binop(G1, G2, 0, V, F, F1, P, P1).
fpl(L, _, _, F, F, P, P) :- is_builtin_atom(L), !.
fpl(L, S, V, F, F1, P, P1) :-
	!,
	functor(L, Op, N),
	( S = 1 ->
	    P1 = [pos(Op/N)|P]
	; S = -1 ->
	    P1 = [neg(Op/N)|P]
	; S = 0 ->
	    P1 = [pos(Op/N), neg(Op/N)|P]
	),
	map_fv(N, V, L, F, F1).

fpl_binop(G1, G2, S, V, F, F1, P, P1) :-
	fpl(G1, S, V, F, F2, P, P2),
	fpl(G2, S, V, F2, F1, P2, P1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% MPTP (Mizar)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Preprocessing Stuff (from cm/pre.pl)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equality_rst_axioms(Axioms) :-
	equality_rst_axioms_r_st(Axioms).
	

%%%% 
%%%% Axioms for reflexivity, symmetry and transitivity. The result is 
%%%% a matrix.
%%%% 
equality_rst_axioms_r_s_t([E1, E2, E3]) :-
	E1 = [X = X],
	E2 = [~(X = Y), Y = X],
	E3 = [~(X = Y), ~(Y = Z), X = Z].

%%%% 
%%%% Axioms for reflexivity, symmetry and transitivity. The result is 
%%%% a matrix. Alternate version, may be more efficient with some provers.
%%%% 
equality_rst_axioms_r_st([E1, E2]) :-
	E1 = [X = X],
	E2 = [~(X = Y), ~(X = Z), Y = Z].

%%%% 
%%%% Axioms for reflexivity, symmetry and transitivity. The result is 
%%%% a matrix. Alternate version, may be more efficient with some provers.
%%%% 
equality_rst_axioms_r_ts([E1, E2]) :-
	E1 = [X = X],
	E2 = [~(X = Y), ~(Y = Z), Z = X].


%%%% 
%%%% Substitutivity axioms for the given signature. The result is
%%%% a matrix.
%%%% 
substitutivity_axioms(Preds, Funs, Axioms) :-
	findall(Ax, ( member(PA, Preds),
		      eqax_pred(PA, Ax)
                    ; member(FA, Funs),
                      eqax_fun(FA, Ax)
                    ),
		Axioms).

eqax_fun(F/A, Axiom) :-
	A > 0,
	A1 is A + 1,
	n_var_list(A1, Vs),
	[X2|Xs1] = Vs,
	n_var_list(A, Xs2),
	eqa_mem(Xs1, Xs2, X1, X2, _Number),
	F1 =.. [F|Xs1],
	F2 =.. [F|Xs2],
	Axiom = [~(X1 = X2), F1 = F2].

eqax_pred(P/A, Axiom) :-
	A > 0,
	A1 is A + 1,
	n_var_list(A1, Vs),
	[X2|Xs1] = Vs,
	n_var_list(A, Xs2),
	eqa_mem(Xs1, Xs2, X1, X2, _Number),
	P1 =.. [P|Xs1],
	P2 =.. [P|Xs2],
	Axiom = [~(P1), ~(X1 = X2),  P2].

eqa_mem([X|Xs],[Y|Xs],X,Y,1).
eqa_mem([X|Xs],[X|Ys],X1,Y1,N) :-
	eqa_mem(Xs,Ys,X1,Y1,N1),
	N is N1 + 1.
	
n_var_list(0, []) :- !. % blue
n_var_list(N, [_|Xs]) :-
	N > 0,
	!, % blue
	N1 is N - 1,
	n_var_list(N1, Xs).

%%%%
%%%% PNs is a signature of predicate specifiers, where each
%%%% predicate represents a function  (the last argument
%%%% is the function value). Axioms expressing the
%%%% uniqueness and existence conditions for the predicates
%%%% are returned. The result is a matrix.
%%%%
pfun_axioms(PNs, Axioms) :-
	pfun_axioms_v2(PNs, Axioms).

pfun_axioms_v1(PNs, Axioms) :-
	findall(Ax, (member(PN, PNs), pfun_axiom_v1(PN, Ax)), Axioms).

pfun_axioms_v2(PNs, Axioms) :-
	findall(Ax, (member(PN, PNs), pfun_axiom_v2(PN, Ax)), Axioms).

pfun_axiom_v1(P/N, Axiom) :-
	functor(T, P, N),
	T =.. [_|Args],
	reverse(Args, [Value|Params]),
	reverse(Params, OParams),
	gen_sk_symbol(SK),
	SKT =.. [SK|OParams],
	( Value = SKT,
	  Axiom = [T]
	; functor(T2, P, N),
	  T2 =.. [_|Args2],
	  reverse(Args2, [Value2|Params]),
	  Axiom = [Value = Value2, ~(T), ~(T2)]
	).

%%%% 
%%%% Alternate Version
%%%% 
pfun_axiom_v2(P/N, Axiom) :-
	functor(T, P, N),
	T =.. [_|Args],
	reverse(Args, [Value|Params]),
	reverse(Params, OParams),
	gen_sk_symbol(SK),
	SKT =.. [SK|OParams],
	( Value = SKT,
	  Axiom = [T]
	; Axiom = [Value = SKT, ~(T)]
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Switch polarity of specified predicates.
%%%% 

m_switch_polarity(PNs, M, M1) :-
	map_c_sps(M, PNs, M1).

map_c_sps([X|Xs], Y1, [X1|Xs1]) :-
	map_p_sps(X, Y1, X1),
	map_c_sps(Xs, Y1, Xs1).
map_c_sps([], _, []).

map_p_sps([X|Xs], Y1, [X1|Xs1]) :-
	p_sps(X, Y1, X1),
	map_p_sps(Xs, Y1, Xs1).
map_p_sps([], _, []).

p_sps(~(L), Ps, L1) :-
	!,
	functor(L, F, N),
	( memberchk(F/N, Ps) ->
	  L =.. [_|Args],
	  concat_atom(['not_', F], FN),
	  L1 =.. [FN|Args]
	; L1 = ~(L)
	).

p_sps(L, Ps, L1) :-
	!,
	functor(L, F, N),
	( memberchk(F/N, Ps) ->
	  L =.. [_|Args],
	  concat_atom(['not_', F], FN),
	  L2 =.. [FN|Args],
	  L1 = ~(L2)
	; L1 = L
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_add_equality(M, M1) :-
	m_add_equality_r_st(M, M1).

m_add_equality_r_s_t(M, M1) :-
	equality_rst_axioms_r_s_t(RstAxioms),
	m_add_equality(M, RstAxioms, M1).

m_add_equality_r_st(M, M1) :-
	equality_rst_axioms_r_st(RstAxioms),
	m_add_equality(M, RstAxioms, M1).

m_add_equality_r_ts(M, M1) :-
	equality_rst_axioms_r_ts(RstAxioms),
	m_add_equality(M, RstAxioms, M1).
	
m_add_equality(M, RstAxioms, M1) :-
	m_signature(M, Ps, Fs),
	( select((=)/2, Ps, Ps1) -> true ; Ps1 = Ps ),
	split_skolems(Fs, Fs1, _),
	substitutivity_axioms(Ps1, Fs1, SAxioms),
	append(RstAxioms, SAxioms, EAxioms),
	append(M, EAxioms, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c_split_pos_neg([~(X)|Xs], Ps, [X|Ns]) :-
	!,
	c_split_pos_neg(Xs, Ps, Ns).
c_split_pos_neg([X|Xs], [X|Ps], Ns) :-
	c_split_pos_neg(Xs, Ps, Ns).
c_split_pos_neg([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_sort(M,  M1) :-
	map_c_sort(M, M2),
	sort(M2, M1).

c_sort(C, C1) :-
	%% probably hard (impossible?) to make this unique when C
	%% contains variables
	c_split_pos_neg(C, CP, CN),
	sort(CP, CP1),
	sort(CN, CN1),
	map_add_neg(CN1, CN2),
	append(CN2, CP1, C1).

map_c_sort([X|Xs], [X1|Xs1]) :-
	c_sort(X, X1),
	map_c_sort(Xs, Xs1).
map_c_sort([], []).

map_add_neg([X|Xs], [~(X)|Xs1]) :-
	map_add_neg(Xs, Xs1).
map_add_neg([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l_atom(~(X), X) :-
	!.
l_atom(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c_is_depth_increasing(C) :-
	split_pn(C, P, N),
	free_variables(P, HVs),
        free_variables(N, BVs),
	abs_intersection(HVs, BVs, Vs),
	once( ( member(V, Vs),
	        maxdepth(V, P, DH),
	        maxdepth(V, N, DB),
		DH > DB )).

%%%% 
%%%% Maximal depth of an occurence of term V (modulo ==/2) in a term in
%%%% list of terms T.
%%%% 
maxdepth(V, Ts, D) :-
	maxdepth_1(Ts, V, 0, -1, D),
	D > -1.
	 
maxdepth_1([T|Ts], V, N, D, D1) :-
	maxdepth_2(T, V, N, D, D2),
	maxdepth_1(Ts, V, N, D2, D1).
maxdepth_1([], _, _, D, D).

maxdepth_2(T, V, N, D, D1) :-
	T == V,
	!,
	D1 is max(N, D).
maxdepth_2(T, V, N, D, D1) :-
	compound(T),
	!,
	T =.. [_|Args],
	N1 is N + 1,
	maxdepth_1(Args, V, N1, D, D1).
maxdepth_2(_, _, _, D, D).	


abs_intersection([X|Xs], Ys, [X|Zs]) :-
	member(Y, Ys),
	X == Y,
	!,
	abs_intersection(Xs, Ys, Zs).
abs_intersection([_|Xs], Ys, Zs) :-
	abs_intersection(Xs, Ys, Zs).
abs_intersection([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog_vars_to_atoms(F, F1) :-
	copy_term(F, F1),
	term_variables(F1, Vs),
	find_vars(1, F, Vs).

find_vars(N, L, [X|Xs]) :-
	( N = 0 ->
	  V = x
	; N > 0 ->
	  concat_atom([x, N], V)
	),
	!,
	( sub_var(V, L) ->
	  N1 is N+1,
	  find_vars(N1, L, [X|Xs])
	; X = V,
	  N1 is N+1,
	  find_vars(N1, L, Xs)
	).
find_vars(_, _, []).
