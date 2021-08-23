%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2019 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(unskclausal, [m_unskolemize/2,
			m_unskolemize/3]).

:- use_module(utils_fol, [prettysort_matrix/2]).

%%%% 
%%%% This should implement McCune's method from his paper "Un-Skolemizing
%%%% Clause Sets", 1988.
%%%%

%%%% 
%%%% m_unskolemize(+Matrix, -Formula)
%%%% m_unskolemize(+Matrix, +Options, -Formula)
%%%%
%%%%   Throws: clausal_unskolemization_failure
%%%%   
%%%%   Processed Options:
%%%%
%%%%     skolems = ListOfAtoms | sk
%%%%
%%%%          Specifies the functors to be considered as Skolem functions
%%%%          and constants:
%%%%
%%%%               ListOfAtoms - explicitly given list of functors
%%%%               sk (default) - for all functors of the form skNUMBER
%%%%
m_unskolemize(M, F) :-
	m_unskolemize(M, [], F).
m_unskolemize(M, Options, F) :-
	make_sk_representation(Options, SK),
	m_sk_partition(M, SK, SMs),
	map_p_unsk(SMs, SK, Options, Fs),
	list_to_conj(Fs, F).

tst_tptp(TP) :-
	tptpio:tptp_problem(TP, [], fof, _, F),
	cnf5(F, M),
	m_unskolemize(M, F1),
	ppl_valid(F1<->F, [split_iff=true]).

tst(F) :-
	cnf5(F, M),
	m_unskolemize(M, F1),
	ppl_valid(F<->F1, [split_iff=true]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_sk_representation(Options, SK) :-
	( memberchk(skolems=SK, Options) ->
	  true
	; SK = sk
	).

is_sk_functor_arity(SK, F/_) :- 
	is_sk_functor(SK, F).

is_sk_term(SK, T) :- 
	functor(T, F, _),
	is_sk_functor(SK, F).

is_sk_functor(sk, F) :-
	!,
	atom(F), %% [] might appear here
	sub_atom(F, 0, _, _, sk),
	sub_atom(F, 2, 1, _, N),
	char_type(N, digit).
is_sk_functor(List, F) :-
	memberchk(F, List).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_p_unsk([X|Xs], SK, O, [X1|Xs1]) :-
	p_unsk(X, SK, O, X1),
	map_p_unsk(Xs, SK, O, Xs1).
map_p_unsk([], _, _, []).

sks_has_only_constants([S/0|Ss], [S|Ss1]) :-
	!,
	sks_has_only_constants(Ss, Ss1).
sks_has_only_constants([], []).

sks_constants([S/0|Ss], [S|Ss1]) :-
	!,
	sks_constants(Ss, Ss1).
sks_constants([_|Ss], Ss1) :-
	!,
	sks_constants(Ss, Ss1).
sks_constants([], []).

p_unsk(S-M, _, _, F) :-
	sks_has_only_constants(S, Es),
	!,
	copy_term(M, M1),
	term_variables(M1, Vs),
	map_list_to_disj(M1, F2),
	list_to_conj(F2, F3),
	inst_vars(Vs),
	( Vs = [] ->
	  F4 = F3
	; Vs = [V] ->
	  F4 = all(V, F3)
	; F4 = all(Vs, F3)
	),
	( Es = [] ->
	  F = F4
	; Es = [E] ->
	  F = ex(E, F4)
	; F = ex(Es, F4)
	).
p_unsk(S-M, SK, _Options, F) :-
	step_1a(M, SK, M1),
	( step_1b_nd(M1, SK, M2),
	  step_2(M2, SK, Ss) ->
	  true
	; throw(clausal_unskolemization_failure)
	),
	( step_3(Ss) ->
	  true
	; %% Is it sure that backtracking to step_1b at this point is not
	  %% useful?
	  throw(clausal_unskolemization_failure)
	),
	sks_constants(S, E),
	sort(Ss, Ss1),
	append(E, Ss1, Ss2),
	prettysort_matrix(M2, M3),
	term_variables(M3, Vs),
	mk_prefix(Ss2, Vs, Prefix),
	map_list_to_disj(M3, F1),
	list_to_conj(F1, F2),
	apply_prefix(Prefix, F2, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_sk_partition(M, SK, SMs) :-
	map_add_skf(M, SK, M1),
	partition(M1, SMs).

map_add_skf([X|Xs], SK, [S-X|Xs1]) :-
	c_skolem_functions(X, SK, S),
	map_add_skf(Xs, SK, Xs1).
map_add_skf([], _, []).

c_skolem_functions(C, SK, F) :- 
	m_signature([C], _, F1),
	findall(S, ( member(S, F1), is_sk_functor_arity(SK, S) ), F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_1a(M, SK, M1) :-
	map_sk_pullout(M, SK, M1).

map_sk_pullout([C|Cs], SK, [C1|Cs1]) :-
	copy_term(C, C2),
	sk_termset(C2, SK, Ss),
	( check_sk_termset_property_1(Ss),
	  check_sk_termset_property_2(Ss) ->
	  true
	; throw(clausal_unskolemization_failure)
	),
	sk_termset_nonvar_args(Ss, As),
	c_pullout(As, C2, C1),
	map_sk_pullout(Cs, SK, Cs1).
map_sk_pullout([], _, []).

c_pullout([T|Ts], C, C1) :-
	subst(C, T, X, C2),
	C3 = [~(X=T)|C2],
	c_pullout(Ts, C3, C1).
c_pullout([], C, C).

sk_termset_nonvar_args(Ss, As) :-
	%% result is ordered: superterm before subterm
	sk_termset_nonvar_args_1(Ss, As1),
	sort(As1, As2),
	reverse(As2, As3),
	map_val(As3, As).
	
sk_termset_nonvar_args_1([S|Ss], As) :-
	atomic(S),
	!,
	sk_termset_nonvar_args_1(Ss, As).
sk_termset_nonvar_args_1([S|Ss], As) :-
	compound(S),
	S =.. [_|Ts],
	sk_nonvars(Ts, As, As1),
	sk_termset_nonvar_args_1(Ss, As1).
sk_termset_nonvar_args_1([], []).
	
sk_nonvars([X|Xs], As, As1) :-
	var(X),
	!,
	sk_nonvars(Xs, As, As1).
sk_nonvars([X|Xs], [TS-X|As], As1) :-
	term_size_1(X, TS),
	sk_nonvars(Xs, As, As1).
sk_nonvars([], As, As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_1b_nd(M, SK, M1) :-
	map_c_step_b_nd(M, SK, M1).

map_c_step_b_nd([X|Xs], SK, [X1|Xs1]) :-
	c_step_b_nd(X, SK, X1),
	map_c_step_b_nd(Xs, SK, Xs1).
map_c_step_b_nd([], _, []).

c_step_b_nd(C, SK, C1) :-
	sk_termset_nonatomics(C, SK, S),
	dup_vars(S, DV),
	dup_vars_alts(DV, VA),
	alts_nequations(VA, C1, C2),
	b_clause_nd(S, VA, C, C2).

%% ?- c_step_b([p(Z, sk1(Z), sk2(Z,Z), sk3(Z,Y,W,Z), sk4(Y,W,Z,Z))], R).

b_clause_nd([S|Ss], VA, C, C1) :-
	skt_variant_nd(S, VA, SV),
	subst(C, S, SV, C2),
	b_clause_nd(Ss, VA, C2, C1).
b_clause_nd([], _, C, C).

skt_variant_nd(T, VA, TV) :-
	functor(T, F, N),
	functor(TV, F, N),
	T =.. [_|Vs],
	TV =.. [F|Vs1],
	skt_gv(Vs1, Vs, VA).

skt_gv([X|Xs], [Y|Ys], VA) :-
	select(Z-V, VA, VA1),
	Z == Y,
	!,
	select(X, V, V1),
	skt_gv(Xs, Ys, [Z-V1|VA1]).
skt_gv([X|Xs], [X|Ys], VA) :-
	skt_gv(Xs, Ys, VA).
skt_gv([], [], _).
	
alts_nequations([X-[Y|Z]|VA], C, C1) :-
	!,
	( X == Y ->
	  C = C2
	; C = [~(X=Y)|C2]
	),
	alts_nequations([X-Z|VA], C2, C1).
alts_nequations([_-[]|VA], C, C1) :-
	alts_nequations(VA, C, C1).
alts_nequations([], C, C).

dup_vars_alts([X-N|DV], [X-[X|Xs]|DV1]) :-
	N1 is N-1,
	mk_n_vars(N1, Xs),
	dup_vars_alts(DV, DV1).
dup_vars_alts([], []).	

dup_vars(S, DV) :-
	dvs(S, [], DV).
	
dvs([S|Ss], DV, DV1) :-
	sk_unique_args(S),
	!,
	dvs(Ss, DV, DV1).
dvs([S|Ss], DV, DV1) :-
	S =.. [_|Vs],
	count_dv(Vs, DV2),
	merge_in_dvs(DV2, DV, DV3),
	dvs(Ss, DV3, DV1).
dvs([], DV, DV).

merge_in_dvs([X-N|DV], DV1, DV2) :-
	( select(Y-N1, DV1, DV3), X==Y ->
	  ( N > N1 ->
	    DV4 = DV3,
	    DV2 = [X-N|DV5]
	  ; DV4 = DV1,
	    DV5 = DV2
	  )
	; DV4 = DV1,
	  DV2 = [X-N|DV5]
	),
	merge_in_dvs(DV, DV4, DV5).
merge_in_dvs([], DV, DV).

count_dv([], []) :- !.
count_dv(Xs, DV) :-
	msort(Xs, [X|Y]),
	cdv(Y, X, 1, DV).

cdv([X|Y], Z, N, DV1) :-
	X == Z,
	!,
	N1 is N+1,
	cdv(Y, Z, N1, DV1).
cdv([X|Y], Z, N, DV1) :-
	( N > 1 ->
	  DV1 = [Z-N|DV2]
	; DV1 = DV2
	),
	cdv(Y, X, 1, DV2).
cdv([], Z, N, DV1) :-
	( N > 1 ->
	  DV1 = [Z-N]
	; DV1 = []
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_2(M, SK, Ss) :-
	unify_sks(M, SK, Ss),
	check_sk_termset_unique_args(Ss).

unify_sks(M, SK, Ss) :-
	sk_termset_nonatomics(M, SK, Ss1),
	( Ss1 = [S|Ss3] ->
	  sk_termset_unify(Ss3, S)
	; true
	),
	sort(Ss1, Ss).

sk_termset_unify([S|Ss], S) :-
	!,
	sk_termset_unify(Ss, S).
sk_termset_unify([S|Ss], _) :-
	sk_termset_unify(Ss, S).
sk_termset_unify([], _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step_3(Ss) :-
	map_to_arglist(Ss, As),
	sort(As, As1),
	reverse(As1, As2),
	map_val(As2, As3),
	( As3 = [A|As4] ->
	  ( unify_for_step_3_nd(As4, A, Ss) ->
	    true
	  ; fail
	  )
	; true
	).

unify_for_step_3_nd([A|NAs], A1, Ss) :-
	%% This is not literally as described in the paper, but hopefully
	%% correct. The subset relationship wrt functors with larger arity
	%% should be established with this implementation.
	vs_unify_into_nd(A, A1),
	check_sk_termset_unique_args(Ss),
	unify_for_step_3_nd(NAs, A, Ss).
unify_for_step_3_nd([], _, _).

vs_unify_into_nd([X|Xs], Y) :-
	select(X, Y, Y1),
	vs_unify_into_nd(Xs, Y1).
vs_unify_into_nd([], _).

map_to_arglist([X|Xs], [N-Ts|Xs1]) :-
	functor(X, _, N),
	X =.. [_|Ts],
	map_to_arglist(Xs, Xs1).
map_to_arglist([], []).

sk_arglist_unique_args(N-S) :-
	num_of_vars(S, N).

check_sk_arglists_unique_args([S|Ss]) :-
	sk_arglist_unique_args(S),
	!,
	check_sk_arglists_unique_args(Ss).
check_sk_arglists_unique_args([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_prefix(P, F, F1) :-
	apply_prefix_1(P, 0, F, F2),
	gather_prefix(F2, F1).

apply_prefix_1([ex(X)|P], N, F, F1) :-
	!,
	( atomic(X) ->
	  F1 = ex(X, F2),
	  F3 = F 
	; concat_atom(['$y',N], X1),
	  subst(F, X, X1, F3),
	  F1 = ex(X1, F2)
	),
	N1 is N+1,
	apply_prefix_1(P, N1, F3, F2).
apply_prefix_1([all(X)|P], N, F, all(Xs, F1)) :-
	!,
	term_variables(X, Xs),
	inst_vars(Xs, N, N1),
	apply_prefix_1(P, N1, F, F1).
apply_prefix_1([], _, F, F).

gather_prefix(ex(X, F), F1) :-
	!,
	gather_prefix(F, F2),
	( F2 = ex(Y, F3) ->
	  gp(X, Y, Z),
	  F1 = ex(Z, F3)
	; gp(X, Z),
	  F1 = ex(Z, F2)
	).
gather_prefix(all(X, F), F1) :-
	!,
	gather_prefix(F, F2),
	( F2 = all(Y, F3) ->
	  gp(X, Y, Z),
	  F1 = all(Z, F3)
	; gp(X, Z),
	  F1 = all(Z, F2)
	).
gather_prefix(F, F).

gp(X, Y, Z) :-
	( atomic(X) -> X1 = [X] ; X1 = X ),
	( atomic(Y) -> Y1 = [Y] ; Y1 = Y ),
	append(X1, Y1, Z).
gp(X, Z) :-
	( atomic(X) -> Z=X
	; X = [Z] -> true
	; X = Z
	).

mk_prefix(Ss, Vs, Prefix) :-
	map_add_arity(Ss, Ss1),
	sort(Ss1, Ss2),
	map_val(Ss2, Ss3),
	sort(Vs, Vs1),
	mkp(Ss3, 0, Vs1, Prefix).

mkp([S|Ss], N, Vs, P) :-
	functor(S, _, N1),
	( atomic(S) ->
	  P = [ex(S)|P1],
	  Vs1 = Vs
	; N1 = N ->
	  P = [ex(S)|P1],
	  Vs1 = Vs
	; S =.. [_|Xs],
	  mkp_sel_vars(Xs, Vs, Xs1, Vs1),
	  P = [all(Xs1), ex(S)|P1]
	),
	mkp(Ss, N1, Vs1, P1).
mkp([], _, [], []) :- !.
mkp([], _, Vs, [all(Vs)]).

mkp_sel_vars(Xs, Vs, Xs1, Vs1) :-
	sort(Xs, Xs2),
	ord_subtract(Vs, Xs2, Vs1),
	ord_intersect(Xs2, Vs, Xs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sk_unique_args(S) :-
	functor(S, _, N),
	num_of_vars(S, N).

check_sk_termset_unique_args([S|Ss]) :-
	sk_unique_args(S),
	!,
	check_sk_termset_unique_args(Ss).
check_sk_termset_unique_args([]).

check_sk_termset_property_2(Ss) :-
	map_add_arity(Ss, Ss1),
	sort(Ss1, Ss2),
	map_val(Ss2, Ss3),
	reverse(Ss3, Ss4),
	( Ss4 = [S|Ss5] ->
	  S =.. [_|Ts],
	  sort(Ts, Ts1),
	  chksk_2(Ss5, Ts1)
	; true
	).

chksk_2([S|_], _) :-
	atomic(S),
	!.
chksk_2([S|Ss], Ts) :-
	S =.. [_|Ts1],
	sort(Ts1, Ts2),
	( ord_subset(Ts2, Ts) ->
	  true
	; fail
	),
	chksk_2(Ss, Ts2).
chksk_2([], _).

check_sk_termset_property_1(Ss) :-
	( Ss = [S|Ss1] ->
	  functor(S, F, N),
	  chksk_1(Ss1, F, N)
	; true
	).

chksk_1([S|Ss], F, N) :-
	functor(S, F1, N1),
	( F1=F, N1=N ->
	  fail
	; true
	),
	chksk_1(Ss, F1, N1).
chksk_1([], _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sk_termset(T, SK, Ss) :-
	skt(T, [], SK, Ss1),
	sort(Ss1, Ss).

sk_termset_nonatomics(T, SK, Ss) :-
	sk_termset(T, SK, Ss1),
	skt_rm_atomics(Ss1, Ss).

skt_rm_atomics([S|Ss], Ss1) :-
	atomic(S),
	!,
	skt_rm_atomics(Ss, Ss1).
skt_rm_atomics(Ss, Ss).
	
skt(T, Ss, _, Ss) :-
	var(T),
	!.
skt(T, Ss, SK, Ss1) :-
	is_sk_term(SK, T),
	!,
	skt_sub(T, [T|Ss], SK, Ss1).
skt(T, Ss, SK, Ss1) :-
	skt_sub(T, Ss, SK, Ss1).

skt_sub(T, Ss, _, Ss) :-
	atomic(T),
	!.
skt_sub(T, Ss, SK, Ss1) :-
	compound(T),
	T =.. [_|Ts],
	map_skt(Ts, Ss, SK, Ss1).

map_skt([T|Ts], Ss, SK, Ss1) :-
	skt(T, Ss, SK, Ss2),
	map_skt(Ts, Ss2, SK, Ss1).
map_skt([], Ss, _, Ss).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% FEs is a list of Features-Element pairs.  Result is a list of
%%%% Features-Elements pairs that represents a partition according to shared
%%%% features. Input pairs where Features is empty are not gathered
%%%% into a single partition. Naive implementation.
%%%%
%%%% Example:
%%%% partition([[1,2]-a, [2,3]-b, [4]-c, [4,5]-d], X).
%%%% X = [[4, 5]-[d, c], [1, 2, 3]-[b, a]].
%%%% 
partition(FEs, Ps) :-
	reverse(FEs, FEs1), %% to let output order be more similar to input
	prt_1(FEs1, [], Ps).

prt_1([F-E|FEs], Ps, Ps1) :-
	prt_sel(Ps, F, Ps2, F, F1, Es1),
	sort(F1, F2),
	prt_1(FEs, [F2-[E|Es1]|Ps2], Ps1).
prt_1([], Ps, Ps).

prt_sel([F1-Es|Ps], F, Ps1, F2, F3, Es3) :-
	member(X, F),
	memberchk(X, F1),
	!,
	append(F1, F2, F4),
	append(Es, Es4, Es3),
	prt_sel(Ps, F, Ps1, F4, F3, Es4).
prt_sel([FE|Ps], F, [FE|Ps1], F2, F3, Es3) :-
	prt_sel(Ps, F, Ps1, F2, F3, Es3).
prt_sel([], _, [], F2, F2, []).

list_to_conj([F], F) :-
	!.
list_to_conj([F|G], (F, G1)) :-
	list_to_conj(G, G1).
list_to_conj([], true).

list_to_disj([F], F) :-
	!.
list_to_disj([F|G], (F; G1)) :-
	list_to_disj(G, G1).
list_to_disj([], false).

map_list_to_disj([X|Xs], [X1|Xs1]) :-
	list_to_disj(X, X1),
	map_list_to_disj(Xs, Xs1).
map_list_to_disj([], []).

map_list_to_conj([X|Xs], [X1|Xs1]) :-
	list_to_conj(X, X1),
	map_list_to_conj(Xs, Xs1).
map_list_to_conj([], []).

subst(T, X, Y, Y) :-
	T == X,
	!.
subst(T, _, _, T) :-
	var(T),
	!.
subst(T, _, _, T) :-
	atomic(T),
	!.
subst(T, X, Y, T1) :-
	compound(T),
	T =.. [F|Ts],
	map_subst(Ts, X, Y, Ts1),
	T1 =.. [F|Ts1].

map_subst([X|Xs], Y1, Y2, [X1|Xs1]) :-
	subst(X, Y1, Y2, X1),
	map_subst(Xs, Y1, Y2, Xs1).
map_subst([], _, _, []).

term_size_1(T, N) :-
	ts(T, 0, N).
	
ts(T, N, N1) :-
	var(T),
	!,
	N1 is N+1.
ts(T, N, N1) :-
	atomic(T),
	!,
	N1 is N+1.
ts(T, N, N1) :-
	compound(T),
	T =.. [_|Ts],
	N2 is N+1,
	map_ts(Ts, N2, N1).

map_ts([], N, N).
map_ts([T|Ts], N, N1) :-
	ts(T, N, N2),
	map_ts(Ts, N2, N1).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

map_add_arity([X|Xs], [A-X|Xs1]) :-
	functor(X, _, A),
	map_add_arity(Xs, Xs1).
map_add_arity([], []).

inst_vars(X) :-
	inst_vars_1(X, 1).
inst_vars_1([], _).
inst_vars_1([X|Xs], N) :-
	concat_atom(['$x',N],X),
	N1 is N+1,
	inst_vars_1(Xs, N1).

inst_vars([], N, N).
inst_vars([X|Xs], N, M) :-
	concat_atom(['$x',N],X),
	N1 is N+1,
	inst_vars(Xs, N1, M).

num_of_vars(T, N) :-
	term_variables(T, V),
	length(V, N).

mk_n_vars(0, []) :- !.
mk_n_vars(N, [_|Xs]) :-
	N1 is N-1,
	mk_n_vars(N1, Xs).

