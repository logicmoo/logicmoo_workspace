:- module(brand, [m_add_equality_steq/3]).

:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(swilib(pretty)).
:- use_module(swilib(info)).
:- use_module(swilib(err)).
:- use_module(toytools(lrpo)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% BRAND-GANZINGER STEQ EQUALITY
%%%% (Code from CM around 1997)
%%%%
%%%% 2018 Note: Apparently after:
%%%%
%%%% L. Bachmair, H. Ganzinger, A. Voronkov:
%%%% Elimination of Equality via Transformation with Ordering Constraints
%%%% Technical Report MPI I 97-2-012 1997
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


m_add_equality_steq(M, Options, [[X=X] | M1]) :-
	m_signature(M, Ps, Fs),
	memberchk(('='/2), Ps),
	!,
	steq_option(lex(LEX), Options, lex(default)),
	( atom(LEX) ->
	  default_lrpo_ordering(LEX, Fs, LEX1)
        ; LEX1 = LEX
        ),
	info(20, 'STEQ: Using lex for preprocessing: ~q', [LEX1]),
	install_lrpo_ordering(LEX1),
	only_neg_occuring_skolem_constants(Fs, M, NSKs),
	( LEX1 = [MIN/0|_] ->
	  Excepts = [MIN/0|NSKs]
        ; Excepts = NSKs
        ),
	matrix_to_imatrix(M, IM1),
	info(80, 'STEQ: Step 1'),
	imatrix_rm_equality_base(IM1, IM2),
	info(80, 'STEQ: Step 2'),
	imatrix_rm_substitutivity_axioms(Fs-Ps, IM2, IM2a),
	info(80, 'STEQ: Step 3'),
	steq_option( add_units(AddUnits), Options, add_units(pos_eq) ),
	( AddUnits = pos_eq ->
	  imatrix_unit_equations(IM2a, IMUnits),
	  length(IMUnits, LIMUnits),
	  info(50, 'STEQ: Adding units, ~w to add', [LIMUnits])
        ; IMUnits = []
        ),
	info(50, 'STEQ: Not pulling out ~q', [Excepts]),
	imatrix_pullout(IM2a, Excepts, Options, IM3),
	info(80, 'STEQ: Step 4'),
	imatrix_s_form(IM3, IM4),
	info(80, 'STEQ: Step 5'),	
	imatrix_t_form(IM4, Options, IM5),
	info(80, 'STEQ: Step 6'),	
	imatrix_simplify_steq_clauses(IM5, LEX1, IM5b),
	info(80, 'STEQ: Step 7'),		
	append(IMUnits, IM5b, IM5a),
	imatrix_to_matrix(IM5a, M1).
m_add_equality_steq(M, _, M).

check_steq_options(Options) :-
	member(Option, Options),
	\+ member(Option, [lex(_),
			   add_units(pos_eq),
			   add_units(off),
			   psimp(_),
			   tsimp(_)]),
	err('Bad option as input to steq: ~q', [Option]),
	!,
	fail.
check_steq_options(_).

steq_option(X,[X|_],_) :- !.
steq_option(X,[_|Xs],D) :- !, steq_option(X,Xs,D).
steq_option(X,[],X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_unit_equations(IM1, IM2) :-
	findall( IC, ( member(IC1, IM1),
	               IC1 = I-[A = B],
		       ( lrpo_greater(A, B) ->
			 IC = I-[A = B]
		       ; lrpo_greater(B, A) ->
			 IC = I-[B = A]
		       ; fail
		       )
		     ),
		 IM2
	       ).
		   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REMOVE EQUALITY AXIOMS
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_rm_equality_base(M, M1) :-
	eq_base_matrix(Eq),
	findall(C, ( member(C, M),
	             \+  is_subsumed_clause(C, Eq)
		   ),
		M1).

eq_base_matrix(
       [[X = X],
	[~(X = Y), Y = X],
	[~(X = Y), ~(Y = Z), X = Z],
	[~(X = Y), ~(X = Z), Y = Z],
	[~(X = Y), ~(Y = Z), Z = X]]).

imatrix_rm_substitutivity_axioms(Fs-Ps, M, M1) :-
	substitutivity_axioms(Fs-Ps, Subst),
	findall(C, ( member(C, M),
	             \+ is_subsumed_clause(C, Subst)
		   ),
		M1).

is_subsumed_clause(_-C, Cs) :-
	member(C1, Cs),
	clause_subsumes_chk(C1, C),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

only_neg_occuring_skolem_constants(Fs, M, NSKs) :-
	%% could be implemented more efficiently ...
	findall( Sk, ( member(Sk, Fs),
	               Sk = _/0,
		       is_skolem_fun(Sk),
		       \+ ( member(C, M),
		            member(L, C),
			    L \= ~(_),
			    %% todo: built-ins... (are usually negative anyway)
			    poslit_functions(L, [], Fs1),
			    memberchk(Sk, Fs1)
			  )
		      ),
		 NSKs).

poslit_functions(L, F, F1) :- functor(L, _, N), map_f(N, L, F, F1).

map_f(0, _, F, F) :- !.
map_f(N, A, F, F1) :-
	arg(N, A, E), f(E, F, F2), N1 is N-1, map_f(N1, A, F2, F1).
f(E, F, F) :- var(E), !.
f(L, F, [Op/N|F1]) :- functor(L, Op, N), map_f(N,L,F,F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PULLOUT (ELIMINATION OF MONOTONICITY)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_pullout(M, Excepts, Options, M1) :-
	map_pullout_clause(M, Excepts, Options, M1).

map_pullout_clause([C|Cs], Excepts, Options, [C1|Cs1]) :-
	pullout_clause(C, Excepts, Options, C1),
	map_pullout_clause(Cs, Excepts, Options, Cs1).
map_pullout_clause([], _, _, []).

pullout_clause(I-C, Excepts, Options, I-C1) :-
	pullout_lits(C, Excepts, C2),
	copy_term(C2, C3),
	p_simplify(C3, Options, C1).

pullout_lits([L|Ls], Excepts, C1) :-
	Options = [],                       %% Options is currently not used
	pullout_literal(L, Excepts, Options, Ls1),
	append(Ls1, C2, C1),
	pullout_lits(Ls, Excepts, C2).
pullout_lits([], _, []).

pullout_literal(Literal, Excepts, Options, Literals) :-
	( Literal = ~Atom ->
	  Literal1 = ~Atom1
        ; Atom = Literal,
	  Atom1 = Literal1
        ),
	functor(Atom, O, A),
	functor(Atom1, O, A),
	( Atom = (_ = _) ->
	  map_pout_subterms(A, Atom, Atom1, Excepts, Options, [], Equations)
        ; map_pout_a(A, Atom, Atom1, Excepts, Options, [], Equations)
        ),
	append(Equations, [Literal1], Literals).

map_pout_subterms(0, _, _, _, _, Eqs, Eqs) :- !.
map_pout_subterms(A, F, F1, Excepts, Opt, Eqs, Eqs1) :-
	arg(A, F, G),
	arg(A, F1, G1),
	pout_subterms(G, G1, Excepts, Opt, Eqs, Eqs2),
	A1 is A - 1,
	map_pout_subterms(A1, F, F1, Excepts, Opt, Eqs2, Eqs1).

pout_subterms(T, T, _, _, Eqs, Eqs) :-
	var(T),
	!.
pout_subterms(F, F1, Excepts, Opt, Eqs, Eqs1) :-
	functor(F, O, A),
	functor(F1, O, A),
	map_pout_a(A, F, F1, Excepts, Opt, Eqs, Eqs1).

map_pout_a(0, _, _, _, _, Eqs, Eqs) :- !.
map_pout_a(A, F, F1, Excepts, Opt, Eqs, Eqs1) :-
	arg(A, F, G),
	arg(A, F1, G1),
	pout_a(G, G1, Excepts, Opt, Eqs, Eqs2),
	A1 is A - 1,
	map_pout_a(A1, F, F1, Excepts, Opt, Eqs2, Eqs1).

pout_a(T, T, _Excepts, _, Eqs, Eqs) :-
	var(T),
	!.
pout_a(T, T, Excepts, _, Eqs, Eqs) :-
	functor(T, F, N),
	memberchk(F/N, Excepts),
	!.
pout_a(F, X, Excepts, Opt, Eqs, Eqs1) :-
	functor(F, O, A),
	functor(F1, O, A),
	map_pout_a(A, F, F1, Excepts, Opt, [~(F1 = X)|Eqs], Eqs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p_simplify(C, Options, C1) :-
	steq_option(psimp(PSimp), Options, psimp(1)),
	%% no copy term since copied by caller
	( PSimp = 0 -> C1 = C
	; PSimp = 1 -> p_simplify_idl(C, C1)
	; PSimp = 2 ->
	  p_simplify_ref(C, C2),
	  p_simplify_idl(C2, C1)
	; err('Bad psimp value: ~q', [PSimp])
	).

%%%%
%%%% Added 2018:
%%%% 
%%%% Negated equations with identical left side might be introduced
%%%% also after pullout.
%%%%
%%%% Does this preserve completeness?
%%%% Might lead to non-linear equations, e.g. ~((0+A) = A)
%%%%
t_simplify_idl(I-C, I-C1) :-
	%% no copy term since copied by caller (called in findall)
	p_simplify_idl(C, C1).

%%%%
%%%% Apply Reflexivity
%%%%
%%%% 2018 Note: It seems that this should be done by a constraint instead of
%%%% unification - but what would be the practical difference from a
%%%% constraint to unifying here?
%%%%
p_simplify_ref([~(U = V)|Eqs], Eqs1) :-
	var(U),
	var(V),
	!,
	U = V,
	p_simplify_ref(Eqs, Eqs1).
p_simplify_ref([Eq|Eqs], [Eq|Eqs1]) :-
	p_simplify_ref(Eqs, Eqs1).
p_simplify_ref([], []).

%%%%
%%%% Merge equations with identical left sides
%%%%
p_simplify_idl([~(T = V)|Eqs], [~(T = V)|Eqs1]) :-
	var(V),
	select(~(T1 = V1), Eqs, Eqs2),
	var(V1),
	T == T1,
	!,
	V = V1,
	p_simplify_idl(Eqs2, Eqs1).
p_simplify_idl([Eq|Eqs], [Eq|Eqs1]) :-
	p_simplify_idl(Eqs, Eqs1).
p_simplify_idl([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SYMMETRY
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_s_form(M, M1) :-
	findall(C, ( member(C1, M),
	             s_clause(C1, C)
		   ),
		M1).

s_clause(I-C, I-C1) :-
	s_lits(C, C1).

s_lits([S=T|Ls], Ls1) :-
	!,
	( Ls1 = [S=T|Ls2]
        ; Ls1 = [T=S|Ls2]
        ),
	s_lits(Ls, Ls2).
s_lits([~(S=T)|Ls], Ls1) :-
	var(S),
	\+ var(T),
	!,
	Ls1 = [~(T=S)|Ls2],
	s_lits(Ls, Ls2).
s_lits([L|Ls], [L|Ls1]) :-
	s_lits(Ls, Ls1).
s_lits([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TRANSITIVITY
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_t_form(M, Options, M1) :-
	steq_option(tsimp(TSimp), Options, tsimp(1)),
	findall(C, ( member(C1, M),
	             t_clause(C1, C2),
		     ( TSimp = 1 -> t_simplify_idl(C2, C) ; C=C2 )
		   ),
		M1).

t_clause(I-C, I-C1) :-
	t_lits(C, C1).

t_lits([S=T|Ls], Ls1) :-
	\+ var(T),
	!,
	Ls1 = [~(T=Z), S=Z, ConstrT, ConstrS | Ls2],
	ConstrT = ~'$hconstrain'(Z, lex_lessq, T),
	ConstrS = ~'$hconstrain'(Z, lex_less, S),
	t_lits(Ls, Ls2).
t_lits([~(S=T)|Ls], Ls1) :-
	\+ var(T),
	!,
	Ls1 = [~(T=Z), ~(S=Z), ConstrT, ConstrS | Ls2],
	ConstrT = ~'$hconstrain'(Z, lex_lessq, T),
	ConstrS = ~'$hconstrain'(Z, lex_lessq, S),
	t_lits(Ls, Ls2).
t_lits([S=X|Ls], Ls1) :-
	var(X),
	!,
        Ls1 = [S=X, ConstrS | Ls2],
	ConstrS = ~'$hconstrain'(X, lex_less, S),
	t_lits(Ls, Ls2).
t_lits([~(S=X)|Ls], Ls1) :-
	var(X),
	!,
        Ls1 = [~(S=X), ConstrS | Ls2],
	ConstrS = ~'$hconstrain'(X, lex_lessq, S),
	t_lits(Ls, Ls2).
t_lits([L|Ls], [L|Ls1]) :-
	t_lits(Ls, Ls1).
t_lits([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SIMPLIFICATIONS - "PARTIAL EVALUATION" OF HCONSTRAINTS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% imatrix_simplify_steq_clauses(IM1, Lex, IM1) :-
% 	!.

imatrix_simplify_steq_clauses(IM1, Lex, IM2) :-
	findall(C, ( member(C1, IM1),
	             simplify_steq_clause(C1, Lex, C)
		   ),
		IM2
	       ).

simplify_steq_clause(I-C, Lex, I1-C1) :-
	%%
	%% fails if the clause can be shown tautologic
	%%
	copy_term(I-C, I1-C2),
	select_hconstraints(C2, HCs, C3),
	( Lex = [Min/0|_] -> true ; Min = _ ),
	simplify_hconstraints(HCs, Min, HCs1),
	simplify_steq_literals(C3, C4),
	append(C4, HCs1, C1).

simplify_steq_literals([L|Ls], Ls1) :-
	( simplify_steq_literal(L) ->
	  simplify_steq_literals(Ls, Ls1)
        ; Ls1 = [L|Ls2],
          simplify_steq_literals(Ls, Ls2)
        ).
simplify_steq_literals([], []).

simplify_steq_literal(~(X = Y)) :-
	X == Y.

simplify_hconstraints(HCs, Min, HCs1) :-
	simplify_hconstraints_1(HCs, Min, HCs2),
	rm_duplicate_hconstraints(HCs2, HCs3),
	%% would it be useful also to detect 
	%% inconsistencies among hconstraints ???
	rm_subsumed_hconstraints(HCs3, HCs3, HCs1).

simplify_hconstraints_1(HCs, Min, HCs1) :-
	select(HC, HCs, HCs2),
	simplify_hconstraint(HC, Min, Fail),
	!,
	( Fail == fail ->
	  fail
        ; simplify_hconstraints_1(HCs2, Min, HCs1)
        ).
simplify_hconstraints_1(HCs, _, HCs).

rm_duplicate_hconstraints(HCs, HCs1) :-
	sort(HCs, HCs1).

rm_subsumed_hconstraints([HC|HCs], AllHCs, HCs1) :-
	( member(HC1, AllHCs),
	  hconstraint_subsumes(HC1, HC) ->
	  HCs1 = HCs2
        ; HCs1 = [HC|HCs2]
        ),
	rm_subsumed_hconstraints(HCs, AllHCs, HCs2).
rm_subsumed_hconstraints([], _, []).


hconstraint_subsumes(~'$hconstrain'(A, lex_less, B), 
	             ~'$hconstrain'(A1, lex_lessq, B1)) :-
	A == A1,
	B == B1.
hconstraint_subsumes(~'$hconstrain'(A, lex_less, B), 
	             ~'$hconstrain'(A1, lex_lessq, B1)) :-
	A == A1,
	lrpo_greater(B1, B).
hconstraint_subsumes(~'$hconstrain'(A, lex_lessq, B), 
	             ~'$hconstrain'(A1, lex_less, B1)) :-
	A == A1,
	lrpo_greater(B1, B).
hconstraint_subsumes(~'$hconstrain'(A, lex_lessq, B), 
	             ~'$hconstrain'(A1, lex_lessq, B1)) :-
	A == A1,
	lrpo_greater(B1, B).

simplify_hconstraint(~'$hconstrain'(A, lex_less, B), Min, Fail) :-
	( A == B -> Fail = fail
        ; lrpo_greater(A, B) -> Fail = fail
        ; B == Min -> Fail = fail
        ; lrpo_greater(B, A) -> true
        ).
simplify_hconstraint(~'$hconstrain'(A, lex_lessq, B), Min, Fail) :-
	( B == Min ->
	  ( unify_with_occurs_check(A, B) -> true
          ; Fail = fail
          )
	; A == Min -> true
        ; A == B -> true
        ; lrpo_greater(B, A) -> true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

substitutivity_axioms(Funs-Preds, Axioms) :-
	findall(Ax, ( member(PA, Preds),
	              \+ no_substitutivity(PA),
		      eqax_pred(PA, Ax)
                    ; member(FA, Funs),
	              \+ no_substitutivity(FA),
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
	Axiom = [~P1, ~(X1 = X2),  P2].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_to_imatrix(M, M1) :-
	map_clause_to_iclause(M, M1).

imatrix_to_matrix(M, M1) :-
	map_iclause_to_clause(M, M1).

clause_to_iclause(C, CInfo-C1) :-
	( select(~'$options'(Options), C, C2) ->
	  CI1 =  [options-Options]
        ; C2 = C,
          CI1 = []
        ),
	select_hconstraints(C2, HCs, C3),
	( HCs = [] ->
	  C1 = C2,
	  CInfo = CI1
        ; C1 = C3,
	  CInfo = [hconstraints-HCs|CI1]
        ).

iclause_to_clause(CInfo-C, C1) :-
	( memberchk(options-Options, CInfo) ->
	  C2 = [~'$options'(Options) | C]
        ; C2 = C
        ),
	( memberchk(hconstraints-HCs, CInfo) ->
	  append(C2, HCs, C1)
        ; C1 = C2
        ).
map_clause_to_iclause([X|Xs], [X1|Xs1]) :-
    clause_to_iclause(X, X1),
    map_clause_to_iclause(Xs, Xs1).
map_clause_to_iclause([], []).

map_iclause_to_clause([X|Xs], [X1|Xs1]) :-
    iclause_to_clause(X, X1),
    map_iclause_to_clause(Xs, Xs1).
map_iclause_to_clause([], []).

select_hconstraints([~'$hconstrain'(A,B,C)|Ls], 
	            [~'$hconstrain'(A,B,C)|HCs], Ls1) :-
	!,
	select_hconstraints(Ls, HCs, Ls1).
select_hconstraints([L|Ls], HCs, [L|Ls1]) :-
	select_hconstraints(Ls, HCs, Ls1).
select_hconstraints([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_skolem_fun(F/_) :-
	atom(F),
	name(F, Name),
	( 
          Name = [0's, 0'k, 0'_, 0'f|N ] -> true %% sk_f#
        ; Name = [0's, 0'k|N] -> true            %% sk#
        %% ; Name = [0'f|N] -> true              %% f# (used by komet nft?)
        ),
	name(I, N),
	number(I).

no_substitutivity('='/2).
% no_substitutivity('type'/2).
no_substitutivity(X) :-
	is_skolem_fun(X).

