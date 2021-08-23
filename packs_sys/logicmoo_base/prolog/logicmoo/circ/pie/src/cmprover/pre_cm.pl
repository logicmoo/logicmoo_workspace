%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 1992,1993,1996,1997,1998,2016 Christoph Wernhard
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PREPROCESSING Version: 1.2 Patchlevel: %I% Date: %G%
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Reductions: taut, mult, subs, pure, unit (naive implementations)
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(pre_cm, 
          [ imatrix_to_matrix/2,
	    matrix_to_imatrix/2,

	    matrix_red/2,
	    matrix_red/3,
	    matrix_red_with_pure/2,
	    matrix_red_with_unit/2,

	    imatrix_red/3,
	    imatrix_red_taut/2,
	    imatrix_red_mult/2,
	    imatrix_red_subs/2,
	    imatrix_red_pure/2,
	    imatrix_red_pure_step/3,
	    imatrix_red_unit_step/5,
	    imatrix_red_unit_step_full/5,
	    imatrix_red_unit/2,

	    is_horn_matrix/1,
	    sort_for_nofan/2,
	    choose_goal_clause/4,
	    matrix_add_equality/3 ]).

:- op(550, fy, user:(~)).

:- use_module(general_cm).
:- use_module(swilib(term_handling)).
:- use_module(swilib(pretty)).
:- use_module(prune).
:- use_module(toytools(lrpo)).

%  :- import pretty. % (for debugging)
%  verbose_info(X) :- nl, pp(X), nl. %% for debugging etc

verbose_info(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% STANDARD STUFF, ECLIPSE STUFF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(X) :- writeln(user_error, X).
dot :- write(user_error, '.'), flush_output(user_error).

local_lit_complem(~X,X) :- !.
local_lit_complem(X,~X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CONVERSION - MATRIX <-> IMATRIX
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% "imat" means matrix with clause information added.
%%%% syntax: imatrix := list of clauses
%%%%         clause := clause_info - list of literals
%%%%         clause_info := plist
%%%%

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
%%%%
%%%% IMATRIX_RED_TAUT
%%%%

imatrix_red_taut([_-Ls|Cs], Cs1) :-
	lits_taut_p(Ls),
	!,
	verbose_info(tautology(Ls)),
	imatrix_red_taut(Cs,Cs1).
imatrix_red_taut([C|Cs], [C|Cs1]) :-
	imatrix_red_taut(Cs, Cs1).
imatrix_red_taut([],[]).

lits_taut_p([L|Ls]) :-
	local_lit_complem(L, L1),
	absmember(L1, Ls),
	!.
lits_taut_p([_|Ls]) :-
	lits_taut_p(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% IMATRIX_RED_MULT
%%%%

imatrix_red_mult([I-X|Xs], [I-X1|Xs1]) :-
    lits_red_mult(X, X1),
    imatrix_red_mult(Xs, Xs1).
imatrix_red_mult([], []).

lits_red_mult([L|Ls], Ls1) :-
	absmember(L, Ls),
	!,
	lits_red_mult(Ls, Ls1).
lits_red_mult([L|Ls], [L|Ls1]) :-
	lits_red_mult(Ls, Ls1).
lits_red_mult([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% IMATRIX_RED_SUBS
%%%%

imatrix_red_subs(M,M1) :-
	imatrix_red_subs1(M,[],M1).

imatrix_red_subs1([],M,M).
imatrix_red_subs1([_-C|Cs],M1,M) :-
	(member(_-C1,Cs) ; member(_-C1,M1)),
	lits_subsumes_chk(C1,C),
	!,
	verbose_info(subsumed(C, by(C1))),
	imatrix_red_subs1(Cs,M1,M).
imatrix_red_subs1([C|Cs],M1,M) :-
	imatrix_red_subs1(Cs,[C|M1],M).

lits_subsumes_chk(C1,C2) :-
	copy_term(C1,C1c),
	copy_term(C2,C2c),
	std_term_variables(C2c,Vs),
	copy_term(Vs,Vpattern),
	lits_subsumes1_chk(C1c,C2c,Vs,Vpattern),
	!.

lits_subsumes1_chk([],_,_,_).
lits_subsumes1_chk([L|Ls],C2,Vs,Vpattern) :-
	oc_member(L,C2),
	variant_chk_noshare(Vs,Vpattern),
	lits_subsumes1_chk(Ls,C2,Vs,Vpattern).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% IMATRIX_RED_PURE
%%%%

imatrix_red_pure(M, M1) :-
	dot,
	imatrix_red_pure_step(M, Success, M2),
	( Success == yes ->
	  imatrix_red_pure(M2, M1)
        ; M1 = M
        ).

imatrix_red_pure_step(M, Suc, M1) :-
	imatrix_pruned_complem_literals(M, Lits),
	imatrix_red_pure1(M, Lits, Suc, M1).

imatrix_red_pure1([_-Ls|Cs], Lits, Suc, Cs1) :-
	lits_pure_p(Ls, Lits),
	!,
	Suc = yes,
	imatrix_red_pure1(Cs, Lits, _, Cs1).
imatrix_red_pure1([C|Cs], Lits, Suc, [C|Cs1]) :-
	imatrix_red_pure1(Cs, Lits, Suc, Cs1).
imatrix_red_pure1([], _, _, []).

lits_pure_p(Ls, Lits) :-
	member(L, Ls),
	\+ oc_member(L, Lits),
	!,
	verbose_info(pure(L, in(Ls))).

imatrix_pruned_complem_literals(M, Lits) :-
	coverof( LN, 
	         Ls^I^L^LN1^( member(I-Ls, M),
	                      member(L, Ls), 
			      local_lit_complem(L, LN1),
			      copy_term(LN1, LN)
                            ),
	         Lits
	       ),
	!.
imatrix_pruned_complem_literals(_, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% IMATRIX_RED_UNIT
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


imatrix_red_unit(M, M1) :-
	key_coverof( I-LN, 
	             L^LN1^( member(I-[L], M), 
		             local_lit_complem(L, LN1),
		             copy_term(LN1, LN)
		           ),
	             NUnits
	       ),
	!,
	imatrix_red_unit_loop(M, NUnits, M1).
imatrix_red_unit(M, M).	

imatrix_red_unit_loop(M, NUnits, M1) :-
% 	nl,
% 	pp(m(M)),
% 	nl, pp(units(NUnits)), nl,
	dot,
	imatrix_red_unit_step_full(M, NUnits, Success, NUnits1, M2),
	( Success == yes, NUnits1 \= [] ->
	  imatrix_red_unit_loop(M2, NUnits1, M1)
        ; Success == yes ->
	  M1 = M2
        ; M1 = M
        ).

imatrix_red_unit_step_full(M, NUnits, Success, NUnits1, M1) :-
	imatrix_red_unit_step(M, NUnits, Success, NUnits1, M2),
	( Success == yes ->
	  imatrix_red_subs(M2, M3),
	  imatrix_red_pure(M3, M1)
	  %% we could remove Units no longer in M3 here
        ; M1 = M
        ).

imatrix_red_unit_step(M, NUnits, Success, NUnits1, M1) :-
	%%
	%% NUnits - lits of negated literals in a unit clause, standardized
	%%          apart from M and from each other.
	%%
	%% NUnits1 - output: newly generated units
	%%
	%% Success - output: == yes if a reduction could be performed
	%%
	verbose_info(reducing_units(NUnits)),
	map_lits_red_unit(M, NUnits, Success, NUnits2, M1),
	key_prune_instances(NUnits2, NUnits1).
	
map_lits_red_unit([I-X|Xs], NUnits, Suc, NUnits1, [I1-X1|Xs1]) :-
    lits_red_unit(X, NUnits, Suc, I, I1, X1),
    ( X1 = [] ->                   % empty clause derived or in the matrix
      Xs1 = [],
      NUnits1 = []
    ; ( X \= [_], X1 = [Unit1] ->  % unit clause derived
        copy_term(Unit1, Unit2),
        local_lit_complem(Unit2, NUnit2),
        NUnits1 = [I1-NUnit2 | NUnits2]
      ; NUnits2 = NUnits1          % no unit clause derived
      ),
      map_lits_red_unit(Xs, NUnits, Suc, NUnits2, Xs1)
    ).
map_lits_red_unit([], _, _, [], []).

lits_red_unit(Ls, NUnits, Suc, I, I1, Ls1) :-
	lits_red_unit1(Ls, [], NUnits, Suc, I, I1, Ls1).
	
lits_red_unit1([L|Ls], Ls0, NUnits, yes, I, I1, Ls1) :-
	member(IU-NU, NUnits),
	( subsumes_chk_noshare(NU, L) ->
	  true
        ; \+ \+ unify_with_occurs_check(L, NU) ->
	  %% can also resolve with the unit if no other lits in the clause
	  %% get instantiated (precise result depends on literal ordering
	  %% within the clause if already removed other lits are no longer
	  %% considered):
	  ( ( std_term_variables(Ls-Ls0, OtherVars),
	      copy_term(OtherVars, VarPattern),
	      \+ \+ ( unify_with_occurs_check(L, NU),
         	      variant_chk_noshare(OtherVars, VarPattern)
		    )
	    ) ->
	    true
          ; fail
          )
        ; fail
        ),
	!,
	verbose_info( reduced_by_unit(L, in(Ls0,Ls), unit(NU)) ),
	add_proof_info(I, L, IU, I2),
	lits_red_unit1(Ls, Ls0, NUnits, _, I2, I1, Ls1).
lits_red_unit1([L|Ls], Ls0, NUnits, Suc, I, I1, [L|Ls1]) :-
	lits_red_unit1(Ls, [L|Ls0], NUnits, Suc, I, I1, Ls1).
lits_red_unit1([], _, _, _, I, I, []).


add_proof_info(ClauseInfo, ReducedLiteral, UnitInfo, NewClauseInfo) :-
	( select(preprocess_proof-ClauseProof, ClauseInfo, ClauseInfo1) ->
	  true
        ; ( memberchk(options-ClauseName, ClauseInfo) ->
	    true
	    %% take all the options as clause name here
	  ; ClauseName = []
          ),
          ClauseInfo1 = ClauseInfo,
	  ClauseProof = input(ClauseName)
        ),
	( memberchk(preprocess_proof-UnitProof, UnitInfo) ->
	  true
        ; ( memberchk(options-UnitName, UnitInfo) ->
	    true
	    %% take all the options as clause name here
	  ; UnitName = []
          ), 
	  UnitProof = input(UnitName)
        ),
	NewClauseInfo = [ preprocess_proof -
	                  unit_reduction(ReducedLiteral, ClauseProof, UnitProof)
		        | ClauseInfo1
		        ].
	
         


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Some often used combinations:
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_red(M, Commands, M1) :-
	matrix_to_imatrix(M, M2),
	imatrix_red(M2, Commands, M3),
	imatrix_to_matrix(M3, M1).

matrix_red(M, M1) :-
	matrix_to_imatrix(M, M2),
	info(taut),
	imatrix_red_taut(M2, M3),
	info(mult),
	imatrix_red_mult(M3, M4),
	info(subs),
	imatrix_red_subs(M4, M5),
	info(done),
	imatrix_to_matrix(M5, M1).

matrix_red_with_pure(M, M1) :-
	matrix_to_imatrix(M, M2),
	info(taut),
	imatrix_red_taut(M2, M3),
	info(mult),
	imatrix_red_mult(M3, M4),
	info(subs),
	imatrix_red_subs(M4, M5),
	info(pure),
	imatrix_red_pure(M5, M6),
	info(''),
	info(done),
	imatrix_to_matrix(M6, M1).

matrix_red_with_unit(M, M1) :-
	matrix_to_imatrix(M, M2),
	info(taut),
	imatrix_red_taut(M2, M3),
	info(mult),
	imatrix_red_mult(M3, M4),
	info(subs),
	imatrix_red_subs(M4, M5),
	info(pure),
	imatrix_red_pure(M5, M6),
	info(''),
	info(unit),
	imatrix_red_unit(M6, M7),
	info(''),
	info(done),
	imatrix_to_matrix(M7, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% matrix_red/3
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_red(M, [Command|Commands], M1) :-
	command_to_call(Command, Call),
	info(red(Command)),
	% nl, writeq(M),
	CallX =.. [Call, M, M2],
	call(CallX),
	imatrix_red(M2, Commands, M1).
imatrix_red(M, [], M).


command_to_call(taut, imatrix_red_taut) :- !.
command_to_call(pure, imatrix_red_pure) :- !.
command_to_call(subs, imatrix_red_subs) :- !.
command_to_call(unit, imatrix_red_unit) :- !.
command_to_call(mult, imatrix_red_mult) :- !.
command_to_call(X, _) :-
	info(no_such_reduction(X)),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_horn_matrix(M) :-
	\+ ( member(C,M), \+ horn_clause(C)).
horn_clause([]).
horn_clause([~_|Ls]) :-
	!,
	horn_clause(Ls).
horn_clause([_|Ls]) :-
	allneg_clause(Ls).
allneg_clause([]).
allneg_clause([~_|Ls]) :-
	allneg_clause(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_for_nofan(M,M1) :-
	%% also removes allnegative clauses
	m_sin(M,M1).
            
m_sin([],[]).
m_sin([C|Cs],Cs1) :-
	allneg_clause(C),
	!,
	m_sin(Cs,Cs1).
m_sin([C|Cs],[C1|Cs1]) :-
	c_sin(C,C1),
	m_sin(Cs,Cs1).

c_sin(Ls, [H|Ls1]) :-
	cs(Ls,H,Ls1).

cs([~L|Ls],H,[~L|Ls1]) :-
	!,
	cs(Ls,H,Ls1).
cs([H|Ls], H, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Equality Axioms
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_add_equality(steq(Options), M, M1) :-
	!,
	matrix_add_equality_steq(M, Options, M1).
matrix_add_equality(steq, M, M1) :-
	!,
	matrix_add_equality(steq([]), M, M1).
matrix_add_equality(Mode, M, M1) :-
	matrix_funs_preds(M, Fs-Ps),
	memberchk(('='/2), Ps),
	!,
	equality_base(Mode, E1),
	substitutivity_axioms(Fs-Ps, E2),
	append(E1, E2, Es),
	append(M, Es, M1).
matrix_add_equality(_, M, M).

no_substitutivity('='/2).
% no_substitutivity('type'/2).
no_substitutivity(X) :-
	is_skolem_fun(X).

is_skolem_fun(F/_) :- 
	name(F, Name),
	( 
          Name = [0's, 0'k, 0'_, 0'f|N ] -> true %% sk_f#
        ; Name = [0's, 0'k|N] -> true            %% sk#
        %% ; Name = [0'f|N] -> true              %% f# (used by komet nft?)
        ),
	name(I, N),
	number(I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_funs_preds(M,F-P) :-
	findall(C, ( member(C1, M),
	             ( select(~'$options'(_), C1, C) -> true ; C = C1 )
                   ),
                M1),
	mp(M1,[],F1,[],P1),
	sort(F1,F),
	sort(P1,P).

mp([],F,F,P,P).
mp([C|Cs],F,F1,P,P1) :-	cp(C,F,F2,P,P2), mp(Cs,F2,F1,P2,P1).
cp([],F,F,P,P).
cp([L|Ls],F,F1,P,P1) :-	fp(L,F,F2,P,P2), cp(Ls,F2,F1,P2,P1).
fp(~L, F, F1, P, P1) :- !, fp(L, F, F1, P, P1).
fp(L, F, F1, P, [Op/N|P]) :- functor(L, Op, N), map_f(N,L,F,F1).
map_f(0, _, F, F) :- !.
map_f(N, A, F, F1) :-
	arg(N, A, E), f(E, F, F2), N1 is N-1, map_f(N1, A, F2, F1).
f(E, F, F) :- var(E), !.
f(L, F, [Op/N|F1]) :- functor(L, Op, N), map_f(N,L,F,F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equality_base(subst, [E1, E2, E3]) :-
	E1 = [X = X],
	E2 = [~(X = Y), Y = X],
	E3 = [~(X = Y), ~(Y = Z), X = Z].

equality_base(subst1, [E1, E2]) :-
	E1 = [X = X],
	E2 = [~(X = Y), ~(X = Z), Y = Z].

equality_base(subst2, [E1, E2]) :-
	E1 = [X = X],
	E2 = [~(X = Y), ~(Y = Z), Z = X].

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% choose_goal_clause(Mode, Matrix, Matrix1)
%%%% 
%%%% Remove the literal '$query' from all except one clause satisfying
%%%% a heuristic condition.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% todo: '$query' shouldn't "unhorn" the matrix


choose_goal_clause(Mode, NthGoal, Matrix, Matrix1) :-
	split_matrix(Matrix, QMatrix, AMatrix),
	
	% length(QMatrix,N), writeln(choosing_goal_among(N)),

	choose_goal_clause1(Mode, NthGoal, QMatrix, AMatrix, QMatrix1),

	maplist(add_theorem_option, QMatrix1, QMatrix2),

	append(QMatrix2, AMatrix, Matrix2),
	%% red unit ?
	%% ... red subs ?
	Matrix1 = Matrix2,
	!.
choose_goal_clause(_, _, _, []). % unprovable in this case

% 	matrix_to_imatrix(Matrix2, M3),
% 	imatrix_red_subs(M3, M4),
% 	imatrix_to_matrix(M4, Matrix1).
% 

add_theorem_option(C,C1) :-
	( select(~'$options'(Options), C, C2) ->
          true 
        ; Options = [], C2 = C
        ),
	( \+ memberchk(role=negated_conjecture, Options) ->
	  Options1 = [role=negated_conjecture | Options]
        ; Options1 = Options
        ),
	C1 = [~'$options'(Options1) | C2].

split_matrix(Matrix, QMatrix, AMatrix) :-
	sm(Matrix, QMatrix, AMatrix).

sm([], [], []).
sm([C|Cs], [Q|Qs], As) :-
	query_clause(C,Q),
	!,
	sm(Cs, Qs, As).
sm([C|Cs], Qs, [C|As]) :-
	sm(Cs, Qs, As).

query_clause(['$query'|Q], Q) :- !.
query_clause([L|Ls], [L|Ls1]) :-
	query_clause(Ls, Ls1).


%%%% 
%%%% choose_goal_clause1(Mode, QMatrix, AMatrix, QMatrix1)
%%%% 

% Heuristically: lowest "connectivity" (product over number of
% connections (weakly unifiable) for each literal).
% 

choose_goal_clause1(Mode, NthGoal, QMatrix, AMatrix, QMatrix1) :-


	% connectivity against whole matrix
	%

	( Mode = conn_axiom ->
	  AMatrix = Matrix
	; append(QMatrix, AMatrix, Matrix)
        ),

	
	map_connectivity(QMatrix, Matrix, CMatrix),

	pp(QMatrix), nl,
	pp(Matrix), nl,
	pp(CMatrix), nl,

	keysort(CMatrix, CMatrix10),

	remove_zero_connecteds(CMatrix10, CMatrix1),

	% debug
	writeln(debug(CMatrix1)),

	map_remove_keys(CMatrix1, QMatrix2),
	
	select_nth_elem(NthGoal, QMatrix2, GC, Cs),
	%% fails if NthGoal is too large

	pp(goal(['$query'|GC])),

	QMatrix1 = [['$query'|GC]|Cs].


select_nth_elem(_, [], _, _) :- !, fail.
select_nth_elem(0, [E|Es], E, Es) :- !.
select_nth_elem(N, [E|Es], E1, [E|Es1]) :-
	N > 0,
	!,
	N1 is N - 1,
	select_nth_elem(N1, Es, E1, Es1).

remove_zero_connecteds([Connectivity-_|M],M1) :-
	is_zero_connectivity(Connectivity),
	!,
	remove_zero_connecteds(M,M1).
remove_zero_connecteds(M,M).

% we also put term weight into what is called "connectivity", i.e.
% the heuristic value.

map_remove_keys([_K-V|KVs], [V|Vs]) :-
	map_remove_keys(KVs,Vs).
map_remove_keys([],[]).

map_connectivity([C|Cs], M, [Connectivity-C|Cs1]) :-
	connectivity(C, M, N),
%	term_weight(C,W),
%	Connectivity=k(N,W),
	Connectivity=N,
	map_connectivity(Cs, M, Cs1).
map_connectivity([], _M, []).

is_zero_connectivity(0).
% is_zero_connectivity(k(0,_)).


% 
% % rationale: if connectivity is equal, prefer large literals as
% % goals. term_weight is applied here to the whole clauses. the size of
% % the clauses seems to be already "contained" in the connectivity.
% 
% term_weight(T, -1) :-
% 	atomic(T),
% 	!.
% term_weight(T, 0) :-
% 	var(T),
% 	!.
% term_weight(~(T), N) :-
% 	T =.. [_|Ts],
% 	map_term_weight(Ts, N).
% term_weight(T, N) :-
% 	T =.. [_|Ts],
% 	map_term_weight(Ts, N1),
% 	N is N1-1.
% 
% map_term_weight([X|Xs], N) :-
%     term_weight(X, N1),
%     map_term_weight(Xs, N2),
%     N is N1 + N2.
% map_term_weight([], 0).

connectivity(Clause, Matrix, N) :-
	copy_term(Clause, Clause1), %% to get weak unification
	c_conn(Clause1, Matrix, 1, N).

c_conn([L|Ls], M, N, N1) :-
	!,
	l_conn(L, M, N2),
	N3 is N * N2,
	c_conn(Ls, M, N3, N1).
c_conn([], _, N, N).


l_conn(~'$hconstrain'(_, _, _), _, 1) :- !.
l_conn(~'$options'(_), _, 1) :- !.
l_conn(L, M, N) :-
	negate_lit(L, L1),
	findall(k, (matrix_literal(M, L2), unify_with_occurs_check(L1, L2)), Ks),
	length(Ks, N).

negate_lit(~ L, L) :- !.
negate_lit(L, ~ L).

matrix_literal(M, L) :-
	member(C, M),
	member(L, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% EXPERIMENTAL STUFF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% BRAND-GANZINGER STEQ EQUALITY
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


matrix_add_equality_steq(M, Options, M1) :-
	matrix_funs_preds(M, Fs-Ps),
	memberchk(('='/2), Ps),
	!,
	steq_option(lex(LEX), Options, lex(default)),
	( LEX = default ->
	  default_lrpo_ordering(Fs, LEX1)
        ; LEX1 = LEX
        ),
	info('Using lex for preprocessing: '(LEX1)),
	install_lrpo_ordering(LEX1),
	only_neg_occuring_skolem_constants(Fs, M, NSKs),
	( LEX1 = [MIN/0|_] ->
	  Excepts = [MIN/0|NSKs]
        ; Excepts = NSKs
        ),
	matrix_to_imatrix(M, IM1),
	writeln(1),
	imatrix_rm_equality_base(IM1, IM2),
	writeln(2),
	imatrix_rm_substitutivity_axioms(Fs-Ps, IM2, IM2a),
	writeln(3),
	steq_option( add_units(AddUnits), Options, add_units(off) ),
	( AddUnits = pos_eq ->
	  imatrix_unit_equations(IM2a, IMUnits)
        ; IMUnits = []
        ),
	% imatrix_pullout(IM2a, [], IM3),
	imatrix_pullout(IM2a, Excepts, IM3), info('Not pulling out: '(Excepts)),
	writeln(4),
	imatrix_s_form(IM3, IM4),
	writeln(5),
	imatrix_t_form(IM4, IM5),
	writeln(6),
	imatrix_simplify_steq_clauses(IM5, LEX1, IM5b),
	writeln(7),
	append(IMUnits, IM5b, IM5a),
	imatrix_to_matrix(IM5a, M6),
	M1 = [[X = X]|M6].
matrix_add_equality_steq(M, _, M).


check_steq_options(Options) :-
	member(Option, Options),
	\+ member(Option, [lex(_), add_units(pos_eq), add_units(off)]),
	nl, writeln('ERROR: Bad option as input to steq: '(Option)),
	!, fail.
check_steq_options(_).

steq_option(X,[X|_],_) :- !.
steq_option(X,[_|Xs],D) :- steq_option(X,Xs,D).
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
	lits_subsumes_chk(C1, C),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PULLOUT (ELIMINATION OF MONOTONICITY)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imatrix_pullout(M, Excepts, M1) :-
	map_pullout_clause(M, Excepts, M1).

map_pullout_clause([C|Cs], Excepts, [C1|Cs1]) :-
	pullout_clause(C, Excepts, C1),
	map_pullout_clause(Cs, Excepts, Cs1).
map_pullout_clause([], _, []).

pullout_clause(I-C, Excepts, I-C1) :-
	pullout_lits(C, Excepts, C2),
	copy_term(C2, C3),
	pout_simplify(C3, C1).

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

pout_simplify([~(U = V)|Eqs], Eqs1) :-            %% Apply Reflexivity
	var(U),
	var(V),
	!,
	U = V,
	pout_simplify(Eqs, Eqs1).
pout_simplify([~(T = V)|Eqs], [~(T = V)|Eqs1]) :- %% Merge equations with
	                                          %% identical left sides
	var(V),
	select(~(T1 = V1), Eqs, Eqs2),
	var(V1),
	T == T1,
	!,
	V = V1,
	pout_simplify(Eqs2, Eqs1).
pout_simplify([Eq|Eqs], [Eq|Eqs1]) :-
	pout_simplify(Eqs, Eqs1).
pout_simplify([], []).

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


imatrix_t_form(M, M1) :-
	findall(C, ( member(C1, M),
	             t_clause(C1, C)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% EXPERIMENTAL STUFF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




