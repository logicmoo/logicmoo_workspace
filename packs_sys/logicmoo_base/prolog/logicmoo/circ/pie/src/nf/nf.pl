/***************************************************************************
NF - CLAUSAL FORM TRANSFORMER
***************************************************************************/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% NF TRANSFORMATION
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% cnf/2, dnf/2
%%%    as usual
%%%
%%% df(+F,-D,-L)
%%%    definitorial formula D for F and the top-level predicate L.
%%% 
%%% performs taut,subs,mult and unit reductions (some only with propositional 
%%% comparision via ==).
%%% 
%%% INPUT SYNTAX:
%%% 
%%% <->, ->, <-, ;, ,, ~,
%%% all(x,_),ex(x,_),
%%% all([x1,...xn],_)
%%% ex([x1,...xn],_)
%%% all2(x,_),ex2(x,_),
%%% all2([x1,...xn],_)
%%% ex2([x1,...xn],_)
%%%
%%% ex2,all2 are for use as second-order quantifiers, they are
%%% only supported by some of the exported predicates.
%%%
%%% rename/2 is also used as a logic operator, supported only
%%% by some of the exported predicates.
%%%
%%% true, false 
%%%
%%% X in quantor(X,p) is a single atomic or a non-empty list of those.
%%% 
%%% skrem might require that input form dosen't contain skolem terms 
%%% (i.e. functors skN, N a number) [???]
%%%
%%% constants are the globally free atomics.
%%% 
%%% OUTPUT:
%%% 
%%% matrix as list of lists of terms -
%%% a negated literal is a term ~T,
%%% variables are prolog variables.
%%% 
%%% NOTES
%%%
%%% see: protected_literal(L)
%%%
%%% BUGS: treatment of skolem funs could be stronger
%%%       protected_literal should perhaps protect also during taut
%%%       and subs
%%%
%%% While it seems that cnf/2 produces correct and acceptabe efficient
%%% matrices, this code is in parts rather old and not "professional" and
%%% contains some dead stuff originally intended for experimentation.
%%% 
% ? bug fol_red_subs doesn't consider protected_literal

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FURTHER PREPROCESSING
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% prep(+Goals,+Matrix,-Matrix1)
%%%
%%% (new, quick-written)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% OTHER SYNTAX SUPPORT
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% mott/1, mott/2
%%% mlisp/1
%%%
%%%     Print matrix as otter/elisp clauses.
%%%
%%% formott/1, formott/2
%%%
%%%     Print formula in otter/formed input style
%%%     mainly for use with formed (prints not very pretty)
%%%
%%% The 2nd arg is in all cases a filename. 
%%%
%%% % od comment:
%%% % old version:
%%% % completely negative clauses are translated as GOALS
%%% % (not AXIOMS with negated literals)
%%% 

:- module( nf, 
           [ cnf/2,
	     dnf/2,
	     df/3,
	     df1/3,
	     definitional_atom/1,
	     nnf/2,

	     cnf_miniscope/2,
	     prenex_nnf/2,
	     
	     inline_equality/2,
	     
	     set_sk_counter/1,
	     get_sk_counter/1,
	     gen_sk_symbol/1,

	     set_p_counter/1,
	     
	     matrix_to_form/2,
	     matrix_to_form_keep_skolems/2,
	     list_to_andseq/2,
	     list_to_orseq/2,

	     %% incomplete translations:
	     %% all quantified vars are prolog vars (like cnf)
	     aosk_for_cnf/2, 
	     aosk_to_cnf/2, 

	     nonhorn_clause/2,
	     sk_terms/2,

	     mott/1, mott/2,
	     mlisp/1,
	     formott/1, formott/2,

	     no_empty_quants/2,
	     skrem/2,
	     
	     maprev/2,

	     fol_red/2,
	     fol_red_subs/2,
	     fol_red_subs_by_unit/2,
	     fol_red_mult/2,
	     fol_red_taut/2,
	     fol_red_pure/2,
	     %% ? fol_red_pure/3,

	     clause_subsumes_chk/2,
	     clause_sort_lits_by_pred_occurrences/2,
	     
	     %% to debug
	     and_or_nf/2,
	     vars_to_prolog/2,
	     pnf/2,
	     pnf1/2,
	      
	     prep/3, 
	     relevant_matrix/4,
	     goals/3, ur/3,
	     % isol1/2,
	     pure1/2,

	     is_skolem_term/1,

	     cnf_fewsimp/2,
	     dnf_fewsimp/2,
	     fol_sort_lits_by_pred_occurrences/2,
	     pgnf/3
	   
	   
	   ]).
	      

:- op(1160,xfx, user:(<->)). % xfy means rassoc: (a->b->c) = (a->(b->c))
:- op(1050,xfy, user:(<-)).
:- op(550, fy, user:(~)).

:- use_module(swilib(term_handling)).
:- use_module(swilib(err)).
:- use_module(swilib(sysdep)).
:- use_module(library(occurs)).



/***************************************************************************/
/* NF TRANSFORMATION */
/***************************************************************************/


cnf(X,Y) :-
	cnf_1(X,X1),
	cnf_patch_true_false(X1,X2),
	clean_matrix(X2,Y).

dnf(X,Y) :-
	dnf_1(X,X1),
	dnf_patch_true_false(X1,X2),
	clean_matrix(X2,Y).

cnf_fewsimp(X, Y) :-
	cnf_1_fewsimp(X, X1),
	cnf_patch_true_false(X1, Y1),
	( memberchk([], Y1) ->
	  Y = [[]]
	; Y = Y1
	).

dnf_fewsimp(X, Y) :-
	dnf_1_fewsimp(X, X1),
	dnf_patch_true_false(X1, Y1),
	( memberchk([], Y1) ->
	  Y = [[]]
	; Y = Y1
	).

clean_matrix(X, Y) :-
	fol_sort_lits_by_pred_occurrences(X, X1),
	fol_red_subs(X1,Y).

%%%% 
%%%% Mon Jan 6 17:38:03 2014 Removed skrem from clean_matrix, since it
%%%% requires specific properties of the input clauses regarding skolem
%%%% functions.
%%%% 

maprev(X,Y) :-
	maplist(reverse,X,Y).

display_if_verbose(_).
% display_if_verbose(X) :- writeln(X).

cnf_1(X,Y) :- 
	display_if_verbose(1),
	and_or_nf(X,X1),
	display_if_verbose(2),
	vars_to_prolog(X1,X2),
	display_if_verbose(3),
	pnf(X2,X3),
	display_if_verbose(4),
	skolem_ex(X3,X31),
	display_if_verbose(5),
	nn_and_dl(X31,X4),
	display_if_verbose(6),
	mat_nf(X4,Y).

cnf_1_fewsimp(X,Y) :- 
	display_if_verbose(1),
	and_or_nf(X,X1),
	display_if_verbose(2),
	vars_to_prolog(X1,X2),
	display_if_verbose(3),
	pnf(X2,X3),
	display_if_verbose(4),
	skolem_ex(X3,X31),
	display_if_verbose(5),
	nn_and_dl(X31,X4),
	display_if_verbose(6),
	mat_nf_fewsimp(X4,Y).

dnf_1(X,Y) :- 
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	pnf(X2,X3), 
	display_if_verbose(4),
	skolem_all(X3,X31),
	display_if_verbose(5),
	nn_or_dl(X31,X4),
	display_if_verbose(6),
	mat_nf(X4,Y).

dnf_1_fewsimp(X,Y) :- 
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	pnf(X2,X3), 
	display_if_verbose(4),
	skolem_all(X3,X31),
	display_if_verbose(5),
	nn_or_dl(X31,X4),
	display_if_verbose(6),
	mat_nf_fewsimp(X4,Y).



/***************************************************************************/
/* Added: July 2003, chw */

%%%% 
%%%% Conjunctive negation ("nested") normal form.
%%%% 
%%%% As in CNF, the essentially existential quantors are skolemized.
%%%% 
%%%% NNF ::= Conjunction
%%%% Conjunction ::= Literal | List of Disjunctions
%%%% Disjunction ::= Literal | List of Conjunctions
%%%% 
%%%% -/2 and list constructors should not be used as predicates.
%%%% 

nnf(X,Y) :- 
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	pnf(X2,X3), 
	display_if_verbose(4),
	skolem_ex(X3,X31),
	display_if_verbose(5),
	nn_and_dl(X31,X4),
 	nnf_close_diffs(X4, Y).

nnf_close_diffs([L|Ls]-[], [L1|Ls1]) :-
	!,
	nnf_close_diffs(L, L1),
	nnf_close_diffs(Ls-[], Ls1).
nnf_close_diffs([]-[], []) :-
	!.
nnf_close_diffs(L, L).

/***************************************************************************/

and_or_nf(In,Out) :- implout(pos, In,X), negin(X,Out).

%% ? switch initial pos/neg for dnf ???

% old version:
%
% implout(POL, (P <-> Q), (((~P1 ; Q1) , (P1 ; ~Q1)))) :- !, 
% 	implout(POL, P,P1), implout(POL, Q,Q1).

implout(neg, (P <-> Q), (D1 ; D2)) :- !,
	implout(neg, (P, Q), D1),
	implout(neg, (~P, ~Q), D2).
implout(pos, (P <-> Q), (C1 , C2)) :- !,
	implout(pos, (~P ; Q), C1),
	implout(pos, (P ; ~Q), C2).
implout(pos, (P <- Q), (P1 ; ~Q1)) :- !,
	implout(pos, P,P1), implout(neg, Q,Q1).
implout(neg, (P <- Q), (P1 ; ~Q1)) :- !,
	implout(neg, P,P1), implout(pos, Q,Q1).
implout(pos, (P -> Q), (~P1 ; Q1)) :- !,
	implout(neg, P,P1), implout(pos, Q,Q1).
implout(neg, (P -> Q), (~P1 ; Q1)) :- !,
	implout(pos, P,P1), implout(neg, Q,Q1).
implout(POL, (P ; Q), (P1 ; Q1)) :- !, implout(POL, P,P1), implout(POL, Q,Q1).
implout(POL, (P , Q), (P1 , Q1)) :- !, implout(POL, P,P1), implout(POL, Q,Q1).
implout(pos, (~P), (~P1)) :- !, implout(neg, P,P1).
implout(neg, (~P), (~P1)) :- !, implout(pos, P,P1).
implout(POL, ex(X,P),ex(X,P1)) :- !, implout(POL, P,P1).
implout(POL, all(X,P),all(X,P1)) :- !, implout(POL, P,P1).
implout(POL, ex2(X,P),ex2(X,P1)) :- !, implout(POL, P,P1).
implout(POL, all2(X,P),all2(X,P1)) :- !, implout(POL, P,P1).
implout(POL, rename(X,P),rename(X,P1)) :- !, implout(POL, P,P1).
implout(_POL, P,P).

negin(~P,P1) :- !, neg(P,P1).
negin((P , Q), (P1 , Q1))  :- !, negin(P,P1), negin(Q,Q1).
negin((P ; Q), (P1 ; Q1))  :- !, negin(P,P1), negin(Q,Q1).
negin(all(X,P),all(X,P1)) :- !, negin(P,P1).
negin(ex(X,P),ex(X,P1)) :- !, negin(P,P1).
negin(all2(X,P),all2(X,P1)) :- !, negin(P,P1).
negin(ex2(X,P),ex2(X,P1)) :- !, negin(P,P1).
negin(rename(X,P),rename(X,P1)) :- !, negin(P,P1).
negin(P,P).

neg(~P,P1) :- !, negin(P,P1).
neg((P , Q), (P1 ; Q1))  :- !, neg(P,P1), neg(Q,Q1).
neg((P ; Q), (P1 , Q1))  :- !, neg(P,P1), neg(Q,Q1).
neg(all(X,P),ex(X,P1)) :- !, neg(P,P1).
neg(ex(X,P),all(X,P1)) :- !, neg(P,P1).
neg(all2(X,P),ex2(X,P1)) :- !, neg(P,P1).
neg(ex2(X,P),all2(X,P1)) :- !, neg(P,P1).
neg(rename(X,P),rename(X,P1)) :- !, neg(P,P1).
neg(P,~P).


/***************************************************************************/
/*** replace quantified vars by unique prolog vars */
/*
output proper for pnf input (if the input is an a/o formula)

Thu Sep 10 21:52:48 2009
Takes as input arbitrary formulas, not just a/o formulas
*/


vars_to_prolog(P,P1) :-
	var_to_prolog_1([],P,P1),
	!. %% To let SWI infer that cnf is deterministic
		    
var_to_prolog_1(Vs,(P,Q),(P1,Q1)) :- !,
	var_to_prolog_1(Vs,P,P1), var_to_prolog_1(Vs,Q,Q1).
var_to_prolog_1(Vs,(P;Q),(P1;Q1)) :- !,
	var_to_prolog_1(Vs,P,P1), var_to_prolog_1(Vs,Q,Q1).
var_to_prolog_1(Vs,(P->Q),(P1->Q1)) :- !,
	var_to_prolog_1(Vs,P,P1), var_to_prolog_1(Vs,Q,Q1).
var_to_prolog_1(Vs,(P<-Q),(P1<-Q1)) :- !,
	var_to_prolog_1(Vs,P,P1), var_to_prolog_1(Vs,Q,Q1).
var_to_prolog_1(Vs,(P<->Q),(P1<->Q1)) :- !,
	var_to_prolog_1(Vs,P,P1), var_to_prolog_1(Vs,Q,Q1).
var_to_prolog_1(Vs,all(X,P),all(X1,P1)) :- !, 
	gen_vars(Vs,X,Vs1,X1), var_to_prolog_1(Vs1,P,P1).
var_to_prolog_1(Vs,ex(X,P),ex(X1,P1)) :- !, 
	gen_vars(Vs,X,Vs1,X1), var_to_prolog_1(Vs1,P,P1).
var_to_prolog_1(Vs,rename(S,P),rename(S,P1)) :- !,
	var_to_prolog_1(Vs,P,P1).
var_to_prolog_1(Vs,all2(X,P),all2(X1,P1)) :- !, 
	gen_so_vars(Vs,X,P,Vs1,X1),
	var_to_prolog_1(Vs1,P,P1).
var_to_prolog_1(Vs,ex2(X,P),ex2(X1,P1)) :- !, 
	gen_so_vars(Vs,X,P,Vs1,X1),
	var_to_prolog_1(Vs1,P,P1).
var_to_prolog_1(Vs,~P,~P1) :- !, var_to_prolog_1(Vs,P,P1).
var_to_prolog_1(Vs,P,P1) :- var_to_prolog_2(Vs,P,P1).

%%%% 
%%%% Do not allow nesting of second-order variables to simplify handling of
%%%% polarity sensitive quantifiers.
%%%%
gen_so_vars(Vs, [X|Xs], F, Vs1, [Y|Ys]) :-
	!,
	so_varspec(X, V, F, W, Y),
	( memberchk(a(V,_), Vs)  ->
	  err('Invalid nesting of variable: ~w', [V])
	; true
	),
	gen_so_vars([a(V,W)|Vs], Xs, F, Vs1, Ys).	
gen_so_vars(Vs,[],_,Vs,[]) :-
	!.
gen_so_vars(Vs, X, F, Vs1, Ys) :-
	gen_so_vars(Vs, [X], F, Vs1, Ys).

so_varspec(+(X/N), X, _, Y, predpos(Y,X,N)) :- !.
so_varspec(-(X/N), X, _, Y, predneg(Y,X,N)) :- !.
so_varspec(X/N, X, _, Y, pred(Y,X,N)) :- atom(X).
so_varspec(+X, X, F, Y, predpos(Y,X,N)) :- !, lookup_arity(X, F, N).
so_varspec(-X, X, F, Y, predneg(Y,X,N)) :- !, lookup_arity(X, F, N).
so_varspec(X, X, F, Y, pred(Y,X,N)) :- atom(X), lookup_arity(X, F, N).

lookup_arity(Functor, F, N) :-
	sub_term(F, T),
	functor(T, Functor, N),
	!.
lookup_arity(_, _, 1).

%%%%
%%%% Wed Jul  8 11:08:37 2015
%%%% Some changes to allow arbitrary terms (not just atoms) as variables in
%%%% the input (to support inputs generated from tableau reasoner)
%%%%					   
gen_vars(Vs,[X|Xs],[a(X,Y)|Vs1],[Y|Ys]) :- gen_vars(Vs,Xs,Vs1,Ys). 
gen_vars(Vs,[],Vs,[]).
% gen_vars(Vs,X,[a(X,Y)|Vs],[Y]) :- atomic(X), \+(X = []).
gen_vars(Vs,X,[a(X,Y)|Vs],[Y]) :- X \== [].

% var_to_prolog_2(Vs,T,T1) :- atomic(T), assoc_var(Vs,T,T1).
var_to_prolog_2(Vs,T,T1) :-
	assoc_var(Vs,T,T1),
	!.
var_to_prolog_2(Vs,T,T1) :- 
	compound(T),
	!,
	T =.. [F|Xs],
	map_var_to_prolog_2(Vs,Xs,Xs1), T1 =.. [F|Xs1].	
var_to_prolog_2(_,T,T).

map_var_to_prolog_2(Vs,[T|Ts],[T1|Ts1]) :-
	var_to_prolog_2(Vs,T,T1),
	map_var_to_prolog_2(Vs,Ts,Ts1).
map_var_to_prolog_2(_,[],[]).

assoc_var([a(K,V)|_],K1,V) :- K == K1, !.
assoc_var([_|Ps],K,V) :- assoc_var(Ps,K,V).
% assoc_var([],K,K).


/***************************************************************************/
/* PNF (quants to front) */

% CNF: ex quants "as front" as possible
% DNF: all quants "as front" as possible

/* 
input: negations before atoms, X is always a list in quantor(X,P) 
output: X is always a single var 
*/ 

pnf((P,Q),R) :- !, pnf(P,P1), pnf(Q,Q1), pnf1((P1,Q1),R).
pnf((P;Q),R) :- !, pnf(P,P1), pnf(Q,Q1), pnf1((P1;Q1),R).

pnf(all([X|Xs],P),all(X,P1)) :- !, pnf(all(Xs,P),P1).

pnf(ex([X|Xs],P),ex(X,P1)) :- !, pnf(ex(Xs,P),P1).

pnf(all([],P),P1) :- !, pnf(P,P1).
pnf(ex([],P),P1) :- !, pnf(P,P1).
pnf(P,P).

% these two clauses should do a little
% preference of existence quants as front as possible 


pnf1((all(X,P) , all(X,Q)), all(X,R)) :- !, pnf1((P,Q),R).
pnf1((ex(X,P) ; ex(X,Q)), ex(X,R)) :- !, pnf1((P;Q),R).
%%
%% ex clauses first here such that existence quantors are preferred
%% in the front (good for cnf only)
%%
pnf1((ex(X,P),Q), ex(X,R)) :- !, pnf1((P,Q),R).
pnf1((P,ex(X,Q)), ex(X,R)) :- !, pnf1((P,Q),R).
pnf1((ex(X,P);Q), ex(X,R)) :- !, pnf1((P;Q),R).
pnf1((P;ex(X,Q)), ex(X,R)) :- !, pnf1((P;Q),R).
%%
pnf1((all(X,P),Q), all(X,R)) :- !, pnf1((P,Q),R).
pnf1((P,all(X,Q)), all(X,R)) :- !, pnf1((P,Q),R).
pnf1((all(X,P);Q), all(X,R)) :- !, pnf1((P;Q),R).
pnf1((P;all(X,Q)), all(X,R)) :- !, pnf1((P;Q),R).	
pnf1(P,P).


/***************************************************************************/
/* SKOLEMIZATION */
/* 
skolem_ex/_all - skolemize ex's/all's rsp. away
*/

skolem_ex(P,P1) :- skolem_ex(P,[],P1).
skolem_all(P,P1) :- skolem_all(P,[],P1).

skolem_ex(all(X,P),Vs,P1) :- !, skolem_ex(P,[X|Vs],P1).
skolem_ex(ex(X,P),Vs,P1) :- !, skterm(Vs,X), skolem_ex(P,Vs,P1).
skolem_ex(P,_,P).

skolem_all(ex(X,P),Vs,P1) :- !, skolem_all(P,[X|Vs],P1).
skolem_all(all(X,P),Vs,P1) :- !, skterm(Vs,X), skolem_all(P,Vs,P1).
skolem_all(P,_,P).

skterm(Vs,T) :-	gen_sk_symbol(SK), T =.. [SK|Vs].


/***************************************************************************/
/* to manipulate an unskolmized matrix */
/* manip_matrix(input-pnf,output-pnf,matrix-of-input-pnf,
   place-to-insert-a-matrix-into-output-pnf) */
/*
manip_matrix(all(X,P),P1,all(X,P2),P3) :- !, manip_matrix(P,P1,P2,P3).
manip_matrix(ex(X,P),P1,ex(X,P2),P3) :- !, manip_matrix(P,P1,P2,P3).
manip_matrix(P,P,P1,P1).
*/


/***************************************************************************/
/* nn_or, nn_and - to nested list/literal matrix */


nn_or_dl((P ; Q), R) :- 
	!, nn_or_dl(P,P1), nn_or_dl(Q,Q1), mat_union(P1,Q1,R).
nn_or_dl((P , Q), R) :- !, nn_and_dl((P , Q),R1), mat_embed(R1,R).
nn_or_dl(X,X).

nn_and_dl((P , Q), R) :- 
	!, nn_and_dl(P,P1), nn_and_dl(Q,Q1), mat_union(P1,Q1,R).
nn_and_dl((P ; Q), R) :- !, nn_or_dl((P ; Q),R1), mat_embed(R1,R).
nn_and_dl(X,X).

mat_union(A-B,B-W,A-W) :- !.
mat_union(A-[X|V],X,A-V) :- !.
mat_union(X,A-[X|V],A-V) :- !.
mat_union(X,Y,[X,Y|V]-V).

mat_embed(A-V,[A-V|W]-W) :- !.
mat_embed(X,X).

/*
nn_or(P,R) :- nn_or_dl(P,R1), nodiff(R1,R).
nn_and(P,R) :- nn_and_dl(P,R1), nodiff(R1,R).
*/


/***************************************************************************/
/* nn to clausal form */
/* 
input: nnf-matrix as nested difference lists 
output: a nf-matrix as list of lists

mult, taut, unit and subs reduction is built in, 
the following functions use difference lists of usual list as matrices
*/

mat_nf(M1,M2) :- mat_nf1(M1,M2-[]).

mat_nf_fewsimp(M1,M2) :- mat_nf1_fewsimp(M1,M2-[]).

mat_nf1_fewsimp(M1,M2) :-
	mat_nf2(M1,M2).

mat_nf1(M1,M2) :-
	mat_nf2(M1,M3),
	matred_unit(M3,M4),
	matred_subs(M4,M2).

mat_nf2(D-D1,D2-D2) :- D == D1, !.
mat_nf2([M|Ms]-D,M1-D3) :- 
	!,
	cla_nf(M,M1-D1),
	mat_nf2(Ms-D,D1-D3).
mat_nf2(L,[[L]|D]-D).

cla_nf(D-D1,D2-D2) :- D == D1, !.
cla_nf([M|Ms]-D,Cs-D1) :- 
	!,
 	cla_nf1([[]|D3]-D3,[M|Ms]-D,Cs-D1).
cla_nf(L,[[L]|D]-D).

cla_nf1(M,D-D1,M) :- D == D1, !.
cla_nf1(M1,[M2|Ms2]-D2,M3) :-
	mat_nf1(M2,M4), mat_multiply(M1,M4,M5), cla_nf1(M5,Ms2-D2,M3).

mat_multiply(M1,M2,M3) :-
	mat_mult1(M1,M2,M4), matred_subs(M4,M3).


mat_mult1(D-D1,_,D2-D2) :- D == D1, !.
mat_mult1([C|Cs]-D,CsD1,Cs2-D2) :-
	mat_mult2(C,CsD1,Cs2-D3),
	mat_mult1(Cs-D,CsD1,D3-D2).

mat_mult2(_,D-D1,D2-D2) :- D == D1, !.
mat_mult2(C,[C1|Cs1]-D1,[C2|Cs2]-D2) :-
	merge_clauses(C1,C,C2), !, mat_mult2(C,Cs1-D1,Cs2-D2).
mat_mult2(C,[_|Cs1]-D1,Cs2-D2) :-
	mat_mult2(C,Cs1-D1,Cs2-D2).

/*
assumes C1 and C2 are each mult and taut reduced, performs mult
reduction, fails if taut
*/
merge_clauses(C1,C2,_) :- 
	member(L1,C1), matlit_comp(L1,L2), absmember(L2,C2), !, fail.
merge_clauses([L|Ls],C2,C3) :-
	absmember(L,C2), !, merge_clauses(Ls,C2,C3).
merge_clauses([L|Ls],C2,[L|C3]) :-
	merge_clauses(Ls,C2,C3).
merge_clauses([],C2,C2). 

matlit_comp(~X,X) :- !.
matlit_comp(X,~X).


/************************/
/* unit */
/* the fixpt */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% New implem: Aug 2004
%%%% 

matred_unit(M-D,M1-D1) :-
	!,
	D = [],
	mr_unit(M, M2),
	append(M2, D1, M1).

select_units([[L]|Cs], [L|Ls]) :-
	ground(L),
	!,
	select_units(Cs, Ls).
select_units([_|Cs], Ls) :-
	select_units(Cs, Ls).
select_units([], []).

mr_unit(M, M1) :-
	select_units(M, Units),
	mr_unit_1(Units, M, [], M1).

mr_unit_1([], M, [], M) :-
	!.
mr_unit_1([], M, N, M1) :-
	!,
	mr_unit_1(N, M, [], M1).
mr_unit_1([L|Ls], M, N, M1) :-
	matlit_comp(L, L1),
	% copy_term(L1, L2),  (not needed for ground)
	L2 = L1,
	map_cr_unit(M, L2, N, N1, M2),
	mr_unit_1(Ls, M2, N1, M1).
	
map_cr_unit([C|Cs], L, N, N1, [C1|Cs1]) :-
	cr_unit(C, L, C1),
	( C \= [_], C1 = [L1], ground(L1) ->
	  N2 = [L1|N]
	; N2 = N
	),
	map_cr_unit(Cs, L, N2, N1, Cs1).
map_cr_unit([], _, N, N, []).

cr_unit([L1|C1], L, C1) :-
	%% assuming mult, there is no need to go further through the clause
	L == L1, 
	!.

% subsumes_chk would be WRONG here, since this unit reduction procedure
% is applied to "local" matrices during nf transformation, which might share
% variables with "outer" literals.

cr_unit([L1|C1],L,[L1|C2]) :- cr_unit(C1,L,C2).
cr_unit([],_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Original Implementation - Stack, Train Problems for
% conjunction of 820 ground disequations.
% 
% matred_unit_old(M-D,M1-D1) :- matred_unit1([],M-D,M1-D1). % ?
%  
% matred_unit1(Olds,M-[],M1-D) :-
% 	length(Olds, LLL),
% 	writeln(lll(LLL)),
% 	member([L],M),
% 	\+ protected_literal(L),
% 	\+(absmember(L,Olds)),
% 	!,
% 	matlit_comp(L,L1),
% 	copy_term(L1, L2),
% 	map_clared_unit(L2,M,M3-D3),
% 	matred_unit1([L|Olds],M3-D3,M1-D). 
% matred_unit1(_,M-D,M-D).
% 	
% map_clared_unit(L,[C|Cs],[C1|Cs1]-D) :-
% 	clared_unit1(L,C,C1),
% 	map_clared_unit(L,Cs,Cs1-D).
% map_clared_unit(_,[],D-D).
% 
% 
% clared_unit1(L,[L1|C1],C1) :-
% 	%% assuming mult, there is no need to go further through the clause
% 	L == L1, 
% 	!.
% 
% % subsumes_chk would be WRONG here, since this unit reduction procedure
% % is applied to "local" matrices during nf transformation, which might share
% % variables with "outer" literals.
% 
% clared_unit1(L,[L1|C1],[L1|C2]) :- clared_unit1(L,C1,C2).
% clared_unit1(_,[],[]).
% 

/************************/
/* subs */

matred_subs(X,X) :- !.

matred_subs(M-[],M1-D) :-
	filter_subs(M,[],M1-D).

% old version
% assumes a only a shorter clause may subsume a longer ie. taut, mult, unit
filter_subs([C|Cs],M,[C|Cs1]-D) :-
	cla_is_not_subsumed(0,C,M),
	cla_is_not_subsumed(0,C,Cs), !,
	filter_subs(Cs,[C|M],Cs1-D).
filter_subs([_|Cs],M,Cs1-D) :-
	% not necessary to put c into m since m contains a stronger clause
	filter_subs(Cs,M,Cs1-D).
filter_subs([],_,D-D).

cla_is_not_subsumed(N,C,[C1|Cs1]) :- 
	\+(cla_subsumesp(C1,C)), !,
	cla_is_not_subsumed(N,C,Cs1).
cla_is_not_subsumed(_,_,[]).	


cla_subsumesp(C1,C2) :- length(C1,N1), length(C2,N2), N2 < N1, !, fail.
cla_subsumesp([L|Ls],Ls2) :-
	absmember(L,Ls2), !, cla_subsumesp(Ls,Ls2).
cla_subsumesp([],_).


/*****************************************************************************/
/* DEFNF - DEFINITORIAL FORM TRANSFORMER */
/***************************************************************************/
/* 
definitorial form - 
see e.eder: relative complexity, section 2.2  
*/

% bug - requires and_or form first ?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag('$sk_counter', _, 1).
:- flag('$p_counter', _, 1).

%%%% 
%%%% Indices for Skolem functions start with the given Number+1
%%%%
set_sk_counter(N) :-
	N1 is N+1,
	flag('$sk_counter', _, N1).

get_sk_counter(N) :-
	flag('$sk_counter', N, N).

%%%% 
%%%% Return a freshly generated Skolem function name.
%%%% 
gen_sk_symbol(SK) :-
	flag_inc('$sk_counter', SK0),
	concat_atom(['sk', SK0], SK).

set_p_counter(N) :-
	N1 is N+1,
	flag('$p_counter', _, N1).

gen_definitional_predicate(P) :-
	flag_inc('$p_counter', P0),
	format(atom(P), 'def_~|~`0t~d~6+$', [P0]).

definitional_atom(A) :-
	functor(A, F, _),
	sub_atom(F, 0, _, _, 'def_'),
	sub_atom(F, _, _, 0, '$').

/***************************************************************************/

%% returns a definitorial form D for F and the top-level predicate L.

df(F,D,L) :-
	%% set_p_counter(0),
	df(F,[],L,[],D1),
	no_empty_quants(D1,D2),
	conj_to_list(D2, D3),
	list_to_conj(D3, D).

% bug: also definitions for atoms (???)
% bug: and_or_form is first required (???)

%%%% Fri Oct 29 16:32:09 2010
%%%% Revision: no definitions for atoms and negated atoms if they
%%%% do not contain variables

df1(F,D,L) :-
%	set_p_counter(0),
	and_or_nf(F,F1),
	df(F1,[],L,[],D1),
	no_empty_quants(D1,D).

% Vs1 should be [] on the top-level if this is correct
%% just 4 patterns: binary-ops, quantifiers, fol-atom, negation

df(all([X|Xs],G1),Vs,L,Vs1,(all(Vs1,(L <-> all([X|Xs],L1))),D1)) :- !,
	append([X|Xs],Vs,Vs2),
	df(G1,Vs2,L1,Vs01,D1),
	subtract_1(Vs01,[X|Xs],Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df(all(X,G1),Vs,L,Vs1,D1) :- !,
	df(all([X],G1),Vs,L,Vs1,D1).
df(ex([X|Xs],G1),Vs,L,Vs1,(all(Vs1,(L <-> ex([X|Xs],L1))),D1)) :- !,
	append([X|Xs],Vs,Vs2),
	df(G1,Vs2,L1,Vs01,D1),
	subtract_1(Vs01,[X|Xs],Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df(ex(X,G1),Vs,L,Vs1,D1) :- !,
	df(ex([X],G1),Vs,L,Vs1,D1).
df((G1 <- G2),Vs,L,Vs1,(all(Vs1,(L <-> (L1 <- L2))),(D1,D2))) :- !,
	df(G1,Vs,L1,Vs01,D1),
	df(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df((G1 -> G2),Vs,L,Vs1,(all(Vs1,(L <-> (L1 -> L2))),(D1,D2))) :- !,
	df(G1,Vs,L1,Vs01,D1),
	df(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df((G1 <-> G2),Vs,L,Vs1,(all(Vs1,(L <-> (L1 <-> L2))),(D1,D2))) :- !,
	df(G1,Vs,L1,Vs01,D1),
	df(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df((G1 , G2),Vs,L,Vs1,(all(Vs1,(L <-> (L1 , L2))),(D1,D2))) :- !,
	df(G1,Vs,L1,Vs01,D1),
	df(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df((G1 ; G2),Vs,L,Vs1,(all(Vs1,(L <-> (L1 ; L2))),(D1,D2))) :- !,
	df(G1,Vs,L1,Vs01,D1),
	df(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df(~G1,Vs,~G1,Vs1,true) :-
	%% No definition for negated atom without variables
	functor(G1, F, N),
	\+ is_df_logop(F, N),
 	filter_contains_var(Vs,G1,Vs1),
	Vs1 = [],
	!.
df(~G1,Vs,L,Vs1,(all(Vs1,(L <-> (~L1))),D1)) :- !,
	df(G1,Vs,L1,Vs1,D1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
df(G1,Vs,L,Vs1,D) :-
	%% Introduce a definition iff the atom contains variables
 	filter_contains_var(Vs,G1,Vs1),
	( Vs1 = [] ->
	  L = G1,
	  D = true
	; gen_definitional_predicate(P),
 	  L =.. [P|Vs1],
	  D = all(Vs1,(L <-> G1))
	).

is_df_logop(',',2).
is_df_logop(';',2).
is_df_logop('<-',2).
is_df_logop('->',2).
is_df_logop('<->',2).
is_df_logop('`',1).



   

% 				% contains_"var" means her comparision with ==.
% 				% not "complete": if a var is == to a
% 				% 0ary predicate it may be redundantly (but
% 				% harmless) retained.
				

filter_contains_var([V|Vs],T,[V|Vs1]) :-
	contains_var(V,T),
	!,
	filter_contains_var(Vs,T,Vs1).
filter_contains_var([_|Vs],T,Vs1) :-
	filter_contains_var(Vs,T,Vs1).
filter_contains_var([],_,[]).


%% this could of course be embedded into df
%% but is perhaps useful for other "generated" formulas too

no_empty_quants(all([],P),P1) :- !,
	no_empty_quants(P,P1).
no_empty_quants(ex([],P),P1) :- !,
	no_empty_quants(P,P1).
no_empty_quants(all(X,P),all(X,P1)) :- !, % covers list and singleton vars
	no_empty_quants(P,P1).
no_empty_quants(ex(X,P),ex(X,P1)) :- !,
	no_empty_quants(P,P1).
no_empty_quants((P <-> Q),(P1 <-> Q1)) :- !,
	no_empty_quants(P,P1),
	no_empty_quants(Q,Q1).
no_empty_quants((P -> Q),(P1 -> Q1)) :- !,
	no_empty_quants(P,P1),
	no_empty_quants(Q,Q1).
no_empty_quants((P <- Q),(P1 <- Q1)) :- !,
	no_empty_quants(P,P1),
	no_empty_quants(Q,Q1).
no_empty_quants((P , Q),(P1 , Q1)) :- !,
	no_empty_quants(P,P1),
	no_empty_quants(Q,Q1).
no_empty_quants((P ; Q),(P1 ; Q1)) :- !,
	no_empty_quants(P,P1),
	no_empty_quants(Q,Q1).
no_empty_quants(~P,~P1) :- !,
	no_empty_quants(P,P1).
no_empty_quants(P,P).


/***************************************************************************/
/* SKREM */
/***************************************************************************/
/* REMOVE SUPERFLUOUS ARGS FORM SKOLEM FUNS */

/* will probably be added to cnf/2, dnf/2 */

/* ... more might be done here */

/*
this might be considered as an instance of a reduction:
"remove superfluous args from predidiacates and terms"
(and perhaps instantiate terms if just one instance is sufficient)
*/

/* 
assumes the skolemterms are still "pure" ie contain only the
uninstantiated vars corresponding to the preceeding quantifiers.
also assumed that repeated occurences of the same skolemfun
within a clause are identically (with == variables) parametrized.

a skolemfun argument is not needed if (but maybe not iff):

it is not repeated within any clause except as argument of this
skolemfun
*/

% skred(+*M,-M1)
% remove unneccessary arguments from skolem functions

skrem(M,M1) :-
	sk_terms(M,SKs),
	map_new_skterm(SKs,M,SKs1),
	map_replace_skterm(SKs,SKs1,M,M1).

map_new_skterm([],_,[]).
map_new_skterm([SK|SKs],M,[SK1|SKs1]) :-
	new_skterm(SK,M,SK1),
	map_new_skterm(SKs,M,SKs1).

map_replace_skterm([],[],M,M).
map_replace_skterm([SK|SKs],[SK1|SKs1],M,M1) :-
	replace_skterm(SK,SK1,M,M2),
	map_replace_skterm(SKs,SKs1,M2,M1).

replace_skterm(SKold,SKnew,M,M) :-
	functor(SKold,_,N),
	functor(SKnew,_,N),
	!.
replace_skterm(SKold,SKnew,M,M1) :-
	%
	% relies on unification at replacing
	% 
	% might effect unification of variables in different clauses which
	% does not influence semantics ;
	%
	replace_term(M,SKold,SKnew,M1).

% replace_term/4, a general procedure,  uses unification (except for vars)

replace_term(X,Old,New,New) :- nonvar(X), X=Old, !.
replace_term(X,_,_,X) :- (atomic(X) ; var(X)), !.
replace_term(X,Old,New,X1) :- compound(X), !,
	functor(X,F,N),
	replace_term(N,X,Old,New,F,[],X1).
	
replace_term(0,_,_,_,F,Args,X1) :- !,
	% perhaps use a term-husk instead of lists
	X1 =.. [F|Args].
replace_term(N,X,Old,New,F,Args,X1) :-
	N > 0,
	arg(N,X,Arg),
	replace_term(Arg,Old,New,Arg1),
	M is N-1,
	replace_term(M,X,Old,New,F,[Arg1|Args],X1).

% for a given skolem term return one with removed superfluous vars 
% vars in the new are in arbitrary order but == to that in the old



/* 
%% quintus

:- use_module(library(occurs),[
        occurrences_of_var/3,
        sub_term/2
       ]).			  
*/


new_skterm(SK,M,SK1) :-
	vars_to_keep(SK,M,[],Vs1),
	sort(Vs1,Vs),
	functor(SK,F,_),
	SK1 =.. [F|Vs].

vars_to_keep(SK,[C|Cs],Vs,Vs1) :-
	vars_to_keep1(SK,C,Vs,Vs2),
	vars_to_keep(SK,Cs,Vs2,Vs1).
vars_to_keep(_,[],Vs,Vs).





vars_to_keep1(SK,C,Vs,Vs1) :-
	%
	% unify SK with the 1st occurence in C - the variables might
	% be "standardized apart" in the different clauses
	%
	% the doc in library(occurs) is pretty unclear about what
	% is instantiated by contains_term and occurrences_of_term, so
	% we avoid them here.
	%
	sub_term(Term,C),
	nonvar(Term),
	Term = SK,
	!,
	% how often occurs SK in N ?
	%
	occurrences_of_var(SK,C,NC),
	%
	functor(SK,_,N),
	vars_to_keep2(N,NC,SK,C,Vs,Vs1).
vars_to_keep1(_,_,Vs,Vs).


vars_to_keep2(0,_,_,_,Vs,Vs) :- !.
vars_to_keep2(N,NC,SK,C,Vs,Vs1) :-
	N > 0,
	arg(N,SK,V),
	occurrences_of_var(V,C,VC),
	VC > NC,
	!,
	M is N-1,
	vars_to_keep2(M,NC,SK,C,[V|Vs],Vs1).
vars_to_keep2(N,NC,SK,C,Vs,Vs1) :-
	N > 0,
	M is N-1,
	vars_to_keep2(M,NC,SK,C,Vs,Vs1).



% list of all skolem terms (one for each skolem function) in a matrix
%
% might effect unification of variables in different clauses which does
% not influence semantics


sk_terms(X,Y) :-
	sk_terms0(X,Y), !.

%% bug - not deterministic
%%
sk_terms0([C|Cs],SKs) :-
	sk_terms1(C,SKs),
	sk_terms0(Cs,SKs).
sk_terms0([],SKs) :-
	finish_list(SKs).

sk_terms1([L|Ls],SKs) :-
	sk_terms3(L,SKs),
	% literals (predicates) are also treated as terms here
	sk_terms1(Ls,SKs).
sk_terms1([],_).

is_skolem_functor(F) :-
	sub_atom(F, 0, _, _, 'sk').

is_skolem_term(Term) :-
	nonvar(Term),
	functor(Term,F,_),
	name(F, [115, 107|N]),
	name(I, N),
	number(I).

% *** ? shouldn't that enter a skolem term too
%
sk_terms3(Term,SKs) :-
	is_skolem_term(Term),
	!,
	% use a variant of SKs here if inter-clause unification
	% should be avoided 
	% ????
	copy_term(Term, Term1), % seems safer 
	memberchk(Term1, SKs).
sk_terms3(Term,SKs) :-
	compound(Term), 
	!,
	functor(Term,_,N),
	sk_terms3(N,Term,SKs).
sk_terms3(_,_).

sk_terms3(0,_,_) :- !.
sk_terms3(N,Term,SKs) :-
	N > 0,
	arg(N, Term, Arg),
	sk_terms3(Arg,SKs),
	M is N-1,
	sk_terms3(M,Term,SKs).
	
finish_list(X) :- var(X), !, X = [].
finish_list([_|Y]) :- finish_list(Y).



/***************************************************************************/




/******************************************************************************/
/*
versions of reductions for general use on matrices (list of lists of lits)
subsumption is "complete" here (contrary to the version used during 
transformation, which should be fixed sometimes)


list_to_difflist([H|T],[H|T1]-D) :-
	list_to_difflist(T,T1-D).
list_to_difflist([],D-D).


still not perfect (with the p&q v -p&-q definition of <->- eg:
ax(
 all([x,y], ((set(x) = set(y)) <-> (all(z, (elem(x,z) <-> elem(y,z))))))).


but fol_red_subs within nf trafo is also wrong (sharing of vars among clauses?)
*/

ax(
 all([x,y], ((set(x) = set(y)) <-> (all(z, (elem(x,z) <-> elem(y,z))))))).

/******************************************************************************/


fol_red(M,M1) :-
	display_if_verbose('taut-'),
	fol_red_taut(M,M2),
	display_if_verbose('mult-'),
	fol_red_mult(M2,M3),
	display_if_verbose('pure-'),
	fol_red_pure(M3,M4),
	display_if_verbose('subs'), 
	fol_red_subs(M4,M5),
	reverse(M5,M1),
	nl.

% FOL_RED_PURE
% naive implementation
% impelm as special case of expansion ?
fol_red_pure_v1(M,M1) :-
	fol_red_pure1(M,M,M2),
	(M == M2 -> M1 = M2 ; fol_red_pure(M2,M1)).
fol_red_pure1([],_,[]).
fol_red_pure1([C|Cs],M,Cs1) :-
	member(L,C),
	\+ protected_literal(L),
	copy_term(L,L2),
	lit_complem(L2,L3),
	\+ (member(C1,M), member(L1,C1), unify_with_occurs_check(L1,L3)),
	!,
	fol_red_pure1(Cs,M,Cs1).
fol_red_pure1([C|Cs],M,[C|Cs1]) :-
	fol_red_pure1(Cs,M,Cs1).

fol_red_pure(M,M1) :-
	fol_red_pure_v3(M, M1).

fol_red_pure_v3(M, M1) :-
	length(M, L),
	fol_red_pure_v3_1(M, M2),
	length(M2, L2),
	( L2 < L ->
	  fol_red_pure_v3(M2, M1)
	; M1 = M
	).

fol_red_pure_v3_1(M, M1) :-
	findall(L, (member(C, M), member(L, C)), Ls),
	sort(Ls, Ls1),
	map_align_vars(Ls1, _),
	sort(Ls1, Ls2),
	map_matlit_comp(Ls2, Ls4),
	fol_red_pure_v3_2(M, Ls4, M1).

fol_red_pure_v3_2([C|M], NLs, M1) :-
	member(L, C),
	\+ protected_literal(L),
	\+ ( member(L1, NLs), unify_with_occurs_check(L, L1) ),
	!,
	fol_red_pure_v3_2(M, NLs, M1).
fol_red_pure_v3_2([C|M], NLs, [C|M1]) :-
	fol_red_pure_v3_2(M, NLs, M1).
fol_red_pure_v3_2([], _, []).

map_matlit_comp([X|Xs], [X1|Xs1]) :-
	matlit_comp(X, X1),
	map_matlit_comp(Xs, Xs1).
map_matlit_comp([], []).


		  
%%%% 
%%%% Assume members of the list are at input time standardized apart, e.g. if
%%%% returned by findall.
%%%% 
map_align_vars([T|Ts], Vs) :-
	term_variables(T, Vs, _),
	map_align_vars(Ts, Vs).
map_align_vars([], _).

% FOL_RED_TAUT
%
% Note: no consideration of literals matching X=X and ~(X=X)
% here as these need to be handled differently in CNF and DNF
%
fol_red_taut([],[]).
fol_red_taut([C|Cs],Cs1) :-
	member(L,C),
	lit_complem(L,L1),
	absmember(L1,C),
	!,
	fol_red_taut(Cs,Cs1).
fol_red_taut([C|Cs],[C|Cs1]) :-
	fol_red_taut(Cs,Cs1).


% FOL_RED_MULT

% this should be improved to remove specializations
%
fol_red_mult([],[]).
fol_red_mult([C|Cs],[C1|Cs1]) :-
	fol_red_mult1(C,C1),
	fol_red_mult(Cs,Cs1).

fol_red_mult1([],[]).
fol_red_mult1([L|Ls],Ls1) :-
	absmember(L,Ls),
	!,
	fol_red_mult1(Ls,Ls1).
fol_red_mult1([L|Ls],[L|Ls1]) :-
	fol_red_mult1(Ls,Ls1).


% FOL_RED_SUBS

fol_red_subs(M,M1) :-
	fol_red_subs1(M,[],M1).

fol_red_subs1([],M,M).
fol_red_subs1([C|Cs],M1,M) :-
	(member(C1,Cs) ; member(C1,M1)),
	clause_subsumes_chk(C1,C),
	!,
	fol_red_subs1(Cs,M1,M).
fol_red_subs1([C|Cs],M1,M) :-
	fol_red_subs1(Cs,[C|M1],M).


fol_red_subs_by_unit(M,M1) :-
	fol_red_subs_by_unit1(M,[],M1).

fol_red_subs_by_unit1([],M,M).
fol_red_subs_by_unit1([C|Cs],M1,M) :-
	(member(C1,Cs) ; member(C1,M1)),
	C1 = [_],
	clause_subsumes_chk(C1,C),
	!,
	fol_red_subs_by_unit1(Cs,M1,M).
fol_red_subs_by_unit1([C|Cs],M1,M) :-
	fol_red_subs_by_unit1(Cs,[C|M1],M).

% no bindings are applied
% we can't use quintus standard order because it may sort variants differently

% clause_subsumes_chk(C1,C2) :-
% 	copy_term(C1,C1c),
% 	copy_term(C2,C2c),
% 	%% list_of_vars(C2c,Vs),
% 	term_variables(C2c, Vs),
% 	copy_term(Vs,Vpattern),
% 	clause_subsumes1_chk(C1c,C2c,Vs,Vpattern),
% 	!.

clause_subsumes_chk(C1,C2) :-
	\+ \+ ( copy_term(C2,C2c),
		term_variables(C2c, Vs),
		copy_term(Vs,Vpattern),
		clause_subsumes1_chk(C1,C2c,Vs,Vpattern)
	      ).
	
clause_subsumes1_chk([],_,_,_).
clause_subsumes1_chk([L|Ls],C2,Vs,Vpattern) :-
	%% oc_member(L,C2),
	member(L1, C2),
	unify_with_occurs_check(L, L1),
	subsumes_chk(Vs,Vpattern),
	clause_subsumes1_chk(Ls,C2,Vs,Vpattern).

lit_complem(~X,X) :- !.
lit_complem(X,~X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% support from cm:

list_of_vars(Term,Vars) :-
	list_of_vars1(Term,[],Vars).

list_of_vars1(Term,Vars,[Term|Vars]) :-
	var(Term), \+ absmember(Term,Vars), !.
list_of_vars1(Term,Vars1,Vars2) :-
	compound(Term),
	!,
	functor(Term,_,Arity),
	list_of_vars2(Arity,Term,Vars1,Vars2).
list_of_vars1(_,Vars,Vars).	

list_of_vars2(1,Term,Vars1,Vars2) :- !,
	arg(1,Term,Argument),
	list_of_vars1(Argument,Vars1,Vars2).
list_of_vars2(N,Term,Vars1,Vars2) :-
	arg(N,Term,Argument),
	list_of_vars1(Argument,Vars1,Vars3),
	M is N-1, !,                               % ! ?
	list_of_vars2(M,Term,Vars3,Vars2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% SOME OTTER SUPPORT
%%%%
%%%% from log93/.../mott.pl
%%%% seems rather quick written
%%%%

mott(MATRIX) :-
	% write('set(prolog_style_variables).'), 
	ppott1(MATRIX).

mott(MATRIX,FILE) :- tell(FILE), mott(MATRIX), told.

ppott1([]).
ppott1([C|Cs]) :- ppott2(C), ppott1(Cs).
ppott2(C) :- copy_term(C,C1), numbervars(C1,0,_), portray_ott(C1).
portray_ott([P]) :- !, portray_ott_lit(P),  write('.'), nl.
portray_ott([P|Ps]) :- portray_ott_lit(P), write(' | '), portray_ott(Ps).
portray_ott_lit(~P) :- !, 
	functor(P,N,_),
	( current_op(_,_,N) -> % if N is an operator
	  pol1(- P)
        ; write('-'), pol1(P)).
portray_ott_lit(P) :- pol1(P).
pol1(equal(X,Y)) :- !, pol1((X=Y)).
pol1((X = Y)) :-
	!, write('('), write_mott(X), write(' = '), write_mott(Y), write(')').
pol1(X) :- write_mott(X).

write_mott(X) :-
	write_term(X, [numbervars(true)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mlisp(MATRIX) :-
	%%
	%% print matrix for use by elisp/fed.el
	%%
	write('(\n'),
	pplisp1(MATRIX),
	write(')\n').

pplisp1([]).
pplisp1([C|Cs]) :- pplisp2(C), pplisp1(Cs).
pplisp2(C) :- copy_term(C,C1), numbervars(C1,0,_),
	      write(' ('), portray_lisp(C1), write(')\n').
portray_lisp([P]) :- !, portray_lisp_lit(P).
portray_lisp([P|Ps]) :- portray_lisp_lit(P), write(' '), portray_lisp(Ps).

portray_lisp_lit(~P) :- !, write('(- '), P =.. L, map_polisp(L), write(')').
portray_lisp_lit(P) :- write('(+ '), P =.. L, map_polisp(L), write(')').

polisp(T) :- atomic(T), write(T).
polisp(T) :- compound(T),
	functor(T,'$VAR',1),
	!,
	% write('?'),
	write(T).
polisp(T) :- compound(T),
	T =.. L,
	write('('),
	map_polisp(L),
	write(')').

map_polisp([T]) :- !, polisp(T).
map_polisp([T|Ts]) :- polisp(T), write(' '), map_polisp(Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formott(FORMULA) :-
	vars_to_prolog(FORMULA, FORMULA1),
	numbervars(FORMULA1, 0, _),
	formott1(FORMULA1,0),
	write('.'), nl.

formott(FORMULA,FILE) :- tell(FILE), formott(FORMULA), told.

tab_to2(_). % tab_to seems ignored since the whole formula is
            % printed in a single line.

formott1(all([X|Y],Z),T) :- !, tab_to2(T), T1 is 1 + T, write('(all '), 
	write(X), write(' '), formott1(all(Y,Z),T1), write(')').
formott1(all([],Z),T) :- !, formott1(Z,T).
formott1(all(X,Z),T) :- !, tab_to2(T), T1 is 1 + T, write('(all '),
	write(X), write(' '), formott1(Z,T1), write(')').
formott1(ex([X|Y],Z),T) :- !, tab_to2(T), T1 is 1 + T, write('(exists '),
	write(X), write(' '), formott1(ex(Y,Z),T1), write(')').
formott1(ex([],Z),T) :- !, formott1(Z,T).
formott1(ex(X,Z),T) :- !, tab_to2(T), T1 is 1 + T, write('(exists '),
	write(X), write(' '), formott1(Z,T1), write(')').
formott1((A -> B),T) :- !, tab_to2(T), T1 is 1 + T, write('('),
	formott1(A,T1), write(' -> '), formott1(B,T1), write(')').
formott1((A <- B),T) :- !, tab_to2(T), T1 is 1 + T, write('('),
	formott1(B,T1), write(' -> '), formott1(A,T1), write(')').
formott1((A <-> B),T) :- !, tab_to2(T), T1 is 1 + T, write('('),
	formott1(A,T1), write(' <-> '), formott1(B,T1), write(')').
formott1((A , B),T) :- !, tab_to2(T), T1 is 1 + T, write('('),
	formott1(A,T1), write(' & '), formott1(B,T1), write(')').
formott1((A ; B),T) :- !, tab_to2(T), T1 is 1 + T, write('('),
	formott1(A,T1), write(' | '), formott1(B,T1), write(')').
formott1((A = B),T) :- !, tab_to2(T), write('('), writeq_formott(A),
	write(' = '), writeq_formott(B), write(')').
formott1(~(A = B),T) :- !, tab_to2(T), write('('), writeq_formott(A),
	write(' != '), writeq_formott(B), write(')').
formott1(equal(A,B),T) :- !, formott1((A = B),T).
formott1(~A,T) :- !, tab_to2(T), T1 is 1 + T, write('-'), formott1(A,T1).
formott1((F<->false),T) :- !, formott1(~F, T).
formott1(false,T) :- !,	formott1((foo,~foo), T).
formott1(true,T) :- !,	formott1((foo;~foo), T).
formott1(X,T) :- tab_to2(T), writeq_formott(X).

writeq_formott([]) :-
	!,
	write('$nil').
writeq_formott(X) :-
	atomic(X),
	!,
	writeq_formott_atomic(X).
writeq_formott('$VAR'(X)) :-
	!,
	write('$VAR'(X)).
writeq_formott([X|Xs]) :-
	!,
	writeq_formott('$cons'(X, Xs)).
writeq_formott(X) :-
	X =.. [F,A|As],
	writeq_formott_atomic(F),
	write('('),
	writeq_formott(A),
	( member(A1, As),
	  write(','),
	  writeq_formott(A1),
	  fail
	; true
	),
	write(')').

writeq_formott_atomic(A) :-
	atom(A),
	atom_chars(A, Cs),
	member(C, Cs),
	C \= '$',
	C \= '_',
	\+ char_type(C, alnum),
	!,
	format('"~w"', [A]).
writeq_formott_atomic(A) :-
	write(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

protected_literal(L) :-
	%%
	%% don't remove protected literals while reductions
	%% such as PURE, ISOL, UNIT
	%% (note: eg. taut, subs? might remove them ...)
	%%
	once((L = ~LP ; LP = L)),
	LP =.. [F|_],
	( F = '$query' -> true
        ; F = '$hconstrain' -> true  %% used?
        ; fail
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FURTHER PREPROCESSING
%%%
% protect the respective goal ???

% options: o_goals

prep(Gs,M,M1) :- prep(Gs,M,[],M1).

prep(Gs,M,Options,M1) :-
	write('relevant'), nl, flush_output,
	relevant_matrix(Gs,M,M2,Options),
	( memberchk(o_goals, Options) -> 
	  o_goals(Gs,M2,Goals) % ?
        ; goals(Gs,M2,Goals)
        ),
	write('cycs'), nl, flush_output,
	cyclic_goals(Goals,M2,Cycs),
	nl, write(cycs(Cycs)), nl,
	write('ur'), nl, flush_output,
	ur(M2, M3, Cycs),
	% nl, write(ur(M3)), nl,
	fol_red_subs(M3, M4),
	prep_loop(M4, M1, Cycs).

prep_loop(M,M1,Cycs) :-
	write('prep_loop'), nl,
	% perhaps just once,
	% perhaps integrate pure into isol ?
	fol_red_isol(M,M2,Cycs),
	fol_red_pure(M2,M3),
	M \== M3, % fol_red_subs might change orderings and copy terms
	!,
	fol_red_subs(M3,M4),
	prep_loop(M4,M1,Cycs).
prep_loop(M,M,_).

% prep_once(M,M1) :-
% 	write('prep_once'), nl,
% 	fol_red_isol(M,M1).


    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% goals(+InitialGoals, +Matrix, -Goals)
%%
%% List of possible Goals if Matrix is queried with
%% InitialGoals. InitialGoals and Goals contain the already
%% negated literals (eg. InitialGoals [p(X)] to get Goals 
%% [p(X), q(X)] from the matrix [[p(X), ~q(X)]]). The result
%% Goals is standardized apart from the Matrix.
%%

goals(InitialGoals, Matrix, Goals) :-
	write('goals'), nl, flush_output,
	copy_term(InitialGoals, Gs), % ensure no sharing with Matrix
                                     % for weak unification
	goals1(Gs, Matrix, Goals).

goals1(Gs, M, Gs1) :-
	%%
	%% very naive implementation
	%%
	member(G, Gs), % can be before or after selection of L
	member(C,M),
	split(C,L,C1),
	\+ \+ unify_with_occurs_check(G, L),
	member(G1, C1),
	lit_complem(G1, G2),
	\+ ( member(G3, Gs), variant_chk(G2, G3)),
                   % ??? or take the most general of both
	!, 
	copy_term(G2, G4), % for weak unification
	goals1([G4|Gs], M, Gs1).
goals1(Gs, _, Gs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% same as goals but:
%% take only the 1st literal of a clause a entry for a query
%% (intended use: definitions like ral)
%% o_ for "ordered"
%%

o_goals(InitialGoals, Matrix, Goals) :-
	write('o_goals'), nl, flush_output,
	copy_term(InitialGoals, Gs), % ensure no sharing with Matrix
                                     % for weak unification
	o_goals1(Gs, Matrix, Goals).

o_goals1(Gs, M, Gs1) :-
	member(G, Gs), % can be before or after selection of L
	member(C,M),
	C = [L|C1],
	\+ \+ unify_with_occurs_check(G, L),
	member(G1, C1),
	lit_complem(G1, G2),
	\+ ( member(G3, Gs), variant_chk(G2, G3)),
                   % ??? or take the most general of both
	!, 
	copy_term(G2, G4), % for weak unification
	nl, write(G4), nl,
	o_goals1([G4|Gs], M, Gs1).
o_goals1(Gs, _, Gs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


goal_is_noncyclic(G, CYCs) :-
	\+ ( member(X, CYCs), unify_with_occurs_check(X, G)). % subsumes_chk?

cyclic_goals(Goals,M,Cyclics) :-
	findall(X, (member(X,Goals), is_cyclic(X,M)), Cyclics).

is_cyclic(L,M) :-
	copy_term(L,L1),
	cyc(L1,M,[]),
	!.

cyc(L,_,A) :-
	member(X,A),
	variant_chk(L,X),
	!.
cyc(L,M,A) :-
	member(C,M),
	split(C,L1,C1),
	\+ \+ unify_with_occurs_check(L,L1),
	map_cyc(C1,M,[L|A]).

map_cyc([],_,_) :-
	fail.
map_cyc([L|_],M,A) :-
	copy_term(L,L1),
	lit_complem(L1,L2),
	cyc(L2,M,A),
	!.
map_cyc([_|Ls],M,A) :-
	map_cyc(Ls,M,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RELEVANT MATRIX
%%
%% relevant_matrix(+Goals, +Matrix, Matrix1)
%%
%% Matrix1 is Matrix with clauses removed that are known to be not 
%% reachable if Matrix is queried with a goal in Goals. 
%%
%% example: > relevant_matrix([p],[[p, ~q], [~r]],X).
%%          X = [[p, ~ q]]
%%

relevant_matrix(Goals, Matrix, Matrix1,Options) :-
	( memberchk(o_goals, Options) ->
	  o_goals(Goals, Matrix, AllGoals)
	; goals(Goals, Matrix, AllGoals)
        ),
	nl, write('relm'), nl, flush_output,
	relm(Matrix, AllGoals, Matrix1).

relm([], _, []).
relm([C|M], Gs, [C|M1]) :-
	member(L, C),
	member(G, Gs),
	\+ \+ subsumes_chk(G, L),
	% \+ \+ unify_with_occurs_check(L, G),
	!,
	relm(M, Gs, M1).
relm([_|M], Gs, M1) :-
	relm(M, Gs, M1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% UNIT RESOLUTION
%%
%% ur(Matrix, Matrix1) 
%%
%% adds unit resolvents up to fixpoint
%%

ur(M, M1, CYCs) :-
	% nl, write(M), nl,
	%% naive straight4wd
	member([L], M),
	goal_is_noncyclic(L, CYCs),
	\+ protected_literal(L),
	lit_complem(L, L1),
	member(C, M),
	copy_term(C, C1),
	copy_term(L1, L2),
	split(C1, L3, C2),
	unify_with_occurs_check(L2, L3),
	\+ ( member(C4, M), variant_chk(C2, C4)),
	!,
	ur([C2|M], M1, CYCs).
ur(M, M, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ISOL, PURE
%%
%%


fol_red_pure_v2(M, M1) :-
	pure1(M, M2),
	M2 \== M,
	fol_red_pure_v2(M2, M1).
fol_red_pure_v2(M, M).

%% pure1 - one PURE scan over the matrix

pure1(M,M1) :- pure1(M,M,M1).

pure1([], _, []).
pure1([C|Cs], M, Cs1) :-
	member(L, C),
	\+ protected_literal(L), 
	has_n_connections(L, M, 0),
	!,
	pure1(Cs, M, Cs1).
pure1([C|Cs], M, [C|Cs1]) :-
	pure1(Cs, M, Cs1).




%% fol_red_isol - ISOL (to fixpt)
%% notice that isol is not symmetric (with weak unifiability connections)

fol_red_isol(M, M1, CYCs) :-
	% nl, nl, write(isol(M)), nl, nl,
	split(M, C, M2),
	copy_term(C, C1),
	split(C1, L, C2),
	\+ protected_literal(L),
	% goal_is_noncyclic(L, CYCs), 
	% difference of both noncyclic positions? this one makes
	% an iteration step??? does not always terminate
	has_n_connections(L, M, 1),
	lit_complem(L,L1),
	% find the connection [again]
	member(D, M),
	% might be an optimization:
	% D \== C, %% don't isol with the same clause (might loop)
	%         %% relationship to taut, taut circles etc
        copy_term(D, D1),		  
	split(D1, L2, D2),
	unify_with_occurs_check(L1,L2),
	once(( goal_is_noncyclic(L1, CYCs) ;
	       has_n_connections(L1, M, 1))), % or symmetric isol case
	!,
	append(C2,D2,R),

	% fol_red_subs([R|M2], M3),
	% fol_red_pure(M3, M4),

	M4 = [R|M2],
	fol_red_isol(M4, M1, CYCs).
fol_red_isol(M, M, _).

has_n_connections(L, M, N) :-
	%% test if L has N connections in the matrix M.
	%% uses weak unifiability with complement as criterion
	%% for connection.
	copy_term(L, L1),
	lit_complem(L1, L2),
	hnoc(M, L2, N, N1),
	%% hnoc fails if it contains more than N connections,
	%% otherwise it returns N-number_of_connnections
	N1 = 0.

hnoc([], _, N, N).
hnoc([C|M], G, N, N1) :-
	hnoc1(C, G, N, N2),
	hnoc(M, G, N2, N1).
hnoc1([], _, N, N).
hnoc1([L|C], G, N, N1) :-
	\+ \+ unify_with_occurs_check(G, L),
	!,
	N2 is N - 1,
	N2 >= 0,
	hnoc1(C, G, N2, N1).
hnoc1([_|C], G, N, N1) :-
	hnoc1(C, G, N, N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this seems quintus select/3 with other arg ordering

split([X|Xs],X,Xs).
split([X|Xs],X1,[X|Xs1]) :-
	split(Xs,X1,Xs1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nonhorn_clause(M,C) :-
	member(C,M),
	nh1(C,C1),
	nh1(C1,_).

nh1([~_|Ls],Ls1) :- !, nh1(Ls,Ls1).
nh1([_|Ls],Ls).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% used with the lambda stuff
%%%% 

aosk_for_cnf(X,Y) :-
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	pnf(X2,X3), 
	display_if_verbose(4),
	skolem_ex(X3,Y).

aosk_to_cnf(X,Y) :-
	asserta(fff(X)),
	nn_and_dl(X,X4),
	display_if_verbose(6),
	noconnect_mat_nf(X4,X5),
	display_if_verbose(7),
	clean_matrix(X5,Y).  % clean neccessary - subs
	

noconnect_mat_nf(M1,M2) :- noconnect_mat_nf1(M1,M2-[]).

noconnect_mat_nf1(M1,M2) :- 
	mat_nf2(M1,M3), 
	%% matred_unit(M3,M4),  %% *** this reduction would perform inferences
	M4 = M3,
	matred_subs(M4,M2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% matrix_to_form(+Matrix, -Form)
%%%%   Throws: nf_unskolemization_failure
%%%%
%%%%   Converts the given Matrix (in cnf) to a formula Form.
%%%% 
%%%%   Names variables with '$x1','$x2', ... - assuming constants don't
%%%%   start with '$x'
%%%% 
%%%%   Performs unskolemization for Skolem constants and (for now) of Skolem
%%%%   functions used only in a single clause (in elim_fol.pl there might be a
%%%%   more complete implementation of unskolemization)
%%%%
%%%%   If unskolemization fails because it is not possible or not sufficiently
%%%%   implemented the exception nf_unskolemization_failure is thrown.
%%%%
%%%%   2019: NOW SUPERSEDED BY unskclausal:m_unskolemize/2
%%%%
%%%% matrix_to_form_keep_skolems(+Matrix, -Form)
%%%%
%%%%   Like matrix_to_form(+Matrix, -Form) but handles Skolem constants
%%%%   and functions like ordinary constants and functions.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_to_form_keep_skolems(M, F) :-
	matrix_to_form_1(M, F).

matrix_to_form(M, F) :-
	m_signature(M, _, Fs),
	( member(F/N, Fs),
	  N > 0,
	  is_skolem_functor(F),
	  \+ m_has_only_clause_local_skolem_proper_functions(M) ->
	  throw(nf_unskolemization_failure)
	; findall(SK, ( member(SK/0, Fs), is_skolem_functor(SK) ), SKs),
	  ( member(SK1/N, Fs), N>0, is_skolem_functor(SK1) ->
	    matrix_to_form_1_unskolemize_clause_local(M, F1)
	  ; matrix_to_form_1(M, F1)
	  ),
	  ( SKs = [] ->
	    F = F1
	  ; F = ex(SKs, F1)
	  )
	).

m_has_only_clause_local_skolem_proper_functions(M) :-
	map_c_skolem_proper_functions(M, S),
	findall(X, (member(Y, S), member(X, Y)), S1),
	sort(S1, S2),
	length(S1, L1),
	length(S2, L2),
	L1 = L2.

map_c_skolem_proper_functions([X|Xs], [X1|Xs1]) :-
	c_skolem_proper_functions(X, X1),
	map_c_skolem_proper_functions(Xs, Xs1).
map_c_skolem_proper_functions([], []).

c_skolem_proper_functions(C, Fs) :-
	m_signature([C], _, Fs1),
	findall(F/N,
		(member(F/N, Fs1), N > 0, is_skolem_functor(F)),
		Fs2),
	sort(Fs2, Fs).

matrix_to_form_1_unskolemize_clause_local([], true) :-
	!.
matrix_to_form_1_unskolemize_clause_local([C], C1) :-
	!,
	clause_to_form_ucl(C,C1).
matrix_to_form_1_unskolemize_clause_local([C|Cs], (C1 , Cs1)) :-
	clause_to_form_ucl(C,C1),
	matrix_to_form_1_unskolemize_clause_local(Cs, Cs1).

clause_to_form_ucl(C, C1) :-
	copy_term(C, C2),
	std_term_variables(C2, Us),
	unsk_pf(C2, [], SKs, C2a),
	sort(Us, Us1),
	unsk_prefix(SKs, Us1, Prefix),
	list_to_orseq(C2a, C3),
	add_prefix(Prefix, C3, C1),
	std_term_variables(C1, Vs),
	cm_vars(Vs, 1).

add_prefix([all(X), all(Y)|P], F, F1) :-
	!,
	append(X, Y, Z),
	add_prefix([all(Z)|P], F, F1).
add_prefix([ex(X), ex(Y)|P], F, F1) :-
	!,
	append(X, Y, Z),
	add_prefix([ex(Z)|P], F, F1).
add_prefix([all([])|P], F, F1) :-
	!,
	add_prefix(P, F, F1).
add_prefix([ex([])|P], F, F1) :-
	!,
	add_prefix(P, F, F1).
add_prefix([all(X)|P], F, all(X, F1)) :-
	!,
	add_prefix(P, F, F1).
add_prefix([ex(X)|P], F, ex(X, F1)) :-
	!,
	add_prefix(P, F, F1).
add_prefix([], F, F).

unsk_prefix([], [], []) :-
	!.
unsk_prefix([], U, [all(U)]) :-
	!.
unsk_prefix(SKs, U, Prefix) :-
	srt_unsk_sks(SKs, SKs1),
	unsk_prefix_1(SKs1, 0, [], U, Prefix).

unsk_prefix_1([N-(_F-W-V)|SKs], N1, W1, U, Prefix) :-
	( N = N1 ->
	  ( W == W1 -> 
	    Prefix = [ex([V]) | Prefix1],
	    U1 = U
	  ; throw(nf_unskolemization_failure)
	  )
	; ord_subset(W1, W) ->
	  ord_subtract(W, W1, W2),
	  ord_subtract(U, W2, U1),
	  Prefix = [all(W2), ex([V])|Prefix1]
	; throw(nf_unskolemization_failure)
	),
	unsk_prefix_1(SKs, N, W, U1, Prefix1).
unsk_prefix_1([], _, _, U, [all(U)]).

srt_unsk_sks(SKs, SKs1) :-
	map_add_srt_sk_key(SKs, SKs2),
	keysort(SKs2, SKs1).

add_srt_sk_key(F-(Args-V), NVs-(F-Vars-V)) :-
	std_term_variables(Args, Vars1),
	sort(Vars1, Vars),
	length(Vars, NVs).

map_add_srt_sk_key([X|Xs], [X1|Xs1]) :-
	add_srt_sk_key(X, X1),
	map_add_srt_sk_key(Xs, Xs1).
map_add_srt_sk_key([], []).


unsk_pf(T, SKs, SKs1, V) :-
	compound(T),
	T =.. [F|Args],
	is_skolem_functor(F),
	!,
	( memberchk(F-(Args1-V), SKs) ->
	  ( Args == Args1 ->
	    SKs1 = SKs
	  ; throw(nf_unskolemization_failure)
	  )
	; SKs1 = [F-(Args-V)|SKs]
	).
unsk_pf(T, SKs, SKs1, T1) :-
	compound(T),
	!,
	T =.. [F|Args],
	map_unsk_pf(Args, SKs, SKs1, Args1),
	T1 =.. [F|Args1].
unsk_pf(T, SKs, SKs, T).
	
map_unsk_pf([X|Xs], Y1, Y2, [X1|Xs1]) :-
	unsk_pf(X, Y1, Y3, X1),
	map_unsk_pf(Xs, Y3, Y2, Xs1).
map_unsk_pf([], S, S, []).




matrix_to_form_1([], true) :-
	!.
matrix_to_form_1([C], C1) :-
	!,
	clause_to_form(C,C1).
matrix_to_form_1([C|Cs], (C1 , Cs1)) :-
	clause_to_form(C,C1),
	matrix_to_form_1(Cs, Cs1).
clause_to_form(C, C1) :-
	copy_term(C, C2),
	std_term_variables(C2, Vs),
	cm_vars(Vs, 1),
	list_to_orseq(C2, C3),
	( Vs = [] ->
	  C1 = C3
        ; C1 = all(Vs, C3)
        ).

list_to_andseq([],true).
list_to_andseq([X],X) :-
	!.
list_to_andseq([X,Y|Z],(X,YZ1)) :- 
	list_to_andseq([Y|Z],YZ1).

list_to_orseq([],false).
list_to_orseq([X],X) :-
	!.
list_to_orseq([X,Y|Z],(X;YZ1)) :- 
	list_to_orseq([Y|Z],YZ1).

cm_vars([], _).
cm_vars([X|Xs], N) :-
	concat_atom(['$x',N],X),
	N1 is N+1,
	cm_vars(Xs, N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Handle true and false in formulas (originally not supported by nf.pl).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf_patch_true_false(M, M1) :-
	map_ptf(M, true, false, M1).

dnf_patch_true_false(M, M1) :-
	map_ptf(M, false, true, M1).

map_ptf([X|Xs], Y1, Y2, Xs1) :-
	( memberchk(Y1, X)
	; memberchk(~Y2, X)
	),
	!,
	map_ptf(Xs, Y1, Y2, Xs1).
map_ptf([X|Xs], Y1, Y2, [X1|Xs1]) :-
	map_ptf_c(X, Y2, Y1, X1),
	map_ptf(Xs, Y1, Y2, Xs1).
map_ptf([], _, _, []).

map_ptf_c([Y1|Xs], Y1, Y2, Xs1) :-
	!,
	map_ptf_c(Xs, Y1, Y2, Xs1).
map_ptf_c([~Y2|Xs], Y1, Y2, Xs1) :-
	!,
	map_ptf_c(Xs, Y1, Y2, Xs1).
map_ptf_c([X|Xs], Y1, Y2, [X|Xs1]) :-
	map_ptf_c(Xs, Y1, Y2, Xs1).
map_ptf_c([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% EXPERIMENTAL
%%%% Inline Equality (i.e. desctructively apply reflexivity if one argument
%%%% is a variable) should be complete (CHECK).
%%%%
%%%% Note 2018: see newer similar stuff in utils_fol.pl
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inline_equality(M, M1) :-
	map_inline_eq(M, M2),
	fol_red_taut(M2, M3),
	rm_clauses_with_true_equalities(M3, M4),
	map_c_order_equations(M4, M5),
	M5 = M6,
	clean_matrix(M6, M1).

map_c_order_equations([X|Xs], [X1|Xs1]) :-
	map_l_order_equation(X, X1),
	map_c_order_equations(Xs, Xs1).
map_c_order_equations([], []).

map_l_order_equation([X|Xs], [X1|Xs1]) :-
	l_order_equation(X, X1),
	map_l_order_equation(Xs, Xs1).
map_l_order_equation([], []).

l_order_equation(X=Y, Y=X) :- Y @< X, !.
l_order_equation(~(X=Y), ~(Y=X)) :- Y @< X, !.
l_order_equation(X, X).

rm_clauses_with_true_equalities([C|M], M1) :-
	c_contains_true_equality(C),
	!,
	rm_clauses_with_true_equalities(M, M1).
rm_clauses_with_true_equalities([C|M], [C|M1]) :-
	rm_clauses_with_true_equalities(M, M1).
rm_clauses_with_true_equalities([], []).

c_contains_true_equality(C) :-
	member(X=Y, C),
	X == Y.

map_inline_eq([X|Xs], [X1|Xs1]) :-
	copy_term(X, X2),
	map_c_inline_eq(X2, X1),
	map_inline_eq(Xs, Xs1).
map_inline_eq([], []).

map_c_inline_eq([~(X = Y)|Xs], Xs1) :-
	once((var(X) ; var(Y))),
	unify_with_occurs_check(X, Y),
	!,
	map_c_inline_eq(Xs, Xs1).
map_c_inline_eq([X|Xs], [X|Xs1]) :-
	map_c_inline_eq(Xs, Xs1).
map_c_inline_eq([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% prenex_nnf(+Formula, -PForm)
%%%% 
%%%% PForm := all(V, PForm) |
%%%%          ex(V, PForm) |
%%%%          QuantifierFreePForm
%%%% 
%%%% - Variables are represented by Prolog Variables	
%%%% - The quantifier free form is in negation normal form
%%%% 
prenex_nnf(X, Y) :-
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	pnf(X2,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Miniscope (not extensively tested)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf_miniscope(X, Y) :-
	display_if_verbose(1),
	and_or_nf(X,X1), 
	display_if_verbose(2),
	vars_to_prolog(X1,X2), 
	display_if_verbose(3),
	canonical_quants(X2, X3),
	miniscope_1(X3, X4),
	ms_skolem(X4, [], X5),
	nn_and_dl(X5,X6),
	display_if_verbose(6),
	mat_nf(X6,X7),
	cnf_patch_true_false(X7,X8),
	clean_matrix(X8,Y).

ms_skolem(all(X, F), Vs, F1) :-
	!,
	ms_skolem(F, [X|Vs], F1).
ms_skolem(ex(X, F), Vs, F1) :-
	!,
	skterm(Vs, X),
	ms_skolem(F, Vs, F1).
ms_skolem((F , G), Vs, (F1 , G1)) :-
	!,
	ms_skolem(F, Vs, F1),
	ms_skolem(G, Vs, G1).
ms_skolem((F ; G), Vs, (F1 ; G1)) :-
	!,
	ms_skolem(F, Vs, F1),
	ms_skolem(G, Vs, G1).
ms_skolem(F, _, F).

canonical_quants(all([X|Xs], F), all(X, F1)) :-
	!,
	canonical_quants(all(Xs, F), F1).
canonical_quants(ex([X|Xs], F), ex(X, F1)) :-
	!,
	canonical_quants(ex(Xs, F), F1).		 
canonical_quants(ex([], F), F1) :-
	!,
	canonical_quants(F, F1).
canonical_quants(all([], F), F1) :-
	!,
	canonical_quants(F, F1).
canonical_quants((F , G), (F1 , G1)) :-
	!,
	canonical_quants(F, F1),
	canonical_quants(G, G1).
canonical_quants((F ; G), (F1 ; G1)) :-
	!,
	canonical_quants(F, F1),
	canonical_quants(G, G1).
canonical_quants(F, F).

copy_var(V, T, V1, T1) :-
	free_variables(T, Vs),
	copy_term(t(V,Vs,T), t(V1,Vs1,T1)),
	equate_vars_except(Vs, Vs1, V).

equate_vars_except([], _, _).
equate_vars_except([X|Xs], [_|Ys], V) :-
	X == V,
	!,
	equate_vars_except(Xs, Ys, V).
equate_vars_except([X|Xs], [X|Ys], V) :-
	equate_vars_except(Xs, Ys, V).

miniscope_1((F , G), (F1 , G1)) :-
	!,
	miniscope_1(F, F1),
	miniscope_1(G, G1).
miniscope_1((F ; G), (F1 ; G1)) :-
	!,
	miniscope_1(F, F1),
	miniscope_1(G, G1).
miniscope_1(all(X, F), F1) :-
	!,
	miniscope_1(F, F2),
	( F2 = (F3 , F4) ->
	  ( contains_var(X, F3) ->
	    ( contains_var(X, F4) ->
	      copy_var(X, F4, Y, F4Y),
	      F5 = (all(X, F3), all(Y, F4Y))
	    ; F5 = (all(X, F3), F4)
	    )
	  ; F5 = (F3, all(X, F4))
	  ),
	  miniscope_1(F5, F1)
	; F2 = (F3 ; F4) ->
	  ( contains_var(X, F3) ->
	    ( contains_var(X, F4) ->
              F1 = all(X, F2)
	    ; miniscope_1((all(X, F3) ; F4), F1)
	    )
	  ; miniscope_1((F3 ; all(X, F4)), F1)
	  )
	; F1 = all(X, F2)
	).
miniscope_1(ex(X, F), F1) :-
	!,
	miniscope_1(F, F2),
	( F2 = (F3 ; F4) ->
	  ( contains_var(X, F3) ->
	    ( contains_var(X, F4) ->
	      copy_var(X, F4, Y, F4Y),
	      F5 = (ex(X, F3) ; ex(Y, F4Y))
	    ; F5 = (ex(X, F3) ; F4)
	    )
	  ; F5 = (F3 ; ex(X, F4))
	  ),
	  miniscope_1(F5, F1)
	; F2 = (F3 , F4) ->
	  ( contains_var(X, F3) ->
	    ( contains_var(X, F4) ->
              F1 = ex(X, F2)
	    ; miniscope_1((ex(X, F3) , F4), F1)
	    )
	  ; miniscope_1((F3 , ex(X, F4)), F1)
	  )
	; F1 = ex(X, F2)
	).
miniscope_1(F, F).
  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conj_to_list((F, G), FG) :-
	!,
	conj_to_list(F, FL),
	conj_to_list(G, GL),
	append(FL, GL, FG).
conj_to_list(true, []) :-
	!.
conj_to_list(F, [F]).

list_to_conj([F], F) :-
	!.
list_to_conj([F|G], (F, G1)) :-
	list_to_conj(G, G1).
list_to_conj([], true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Useful before subsumption
%%%%
fol_sort_lits_by_pred_occurrences(M, M1) :-
	map_c_sort_by_pred_occurrences(M, M1).

map_c_sort_by_pred_occurrences([X|Xs], [X1|Xs1]) :-
	clause_sort_lits_by_pred_occurrences(X, X1),
	map_c_sort_by_pred_occurrences(Xs, Xs1).
map_c_sort_by_pred_occurrences([], []).

clause_sort_lits_by_pred_occurrences(C, C1) :-
	count_poccs(C, [], [], OP, ON),
	map_add_poccs(C, OP, ON, C2),
	sort(C2, C3),
	map_val(C3, C1).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

count_poccs([~A|Ls], OP, ON, OP1, ON1) :-
	!,
	functor(A, F, N),
	poccs_addto(ON, F/N, ON2),
	count_poccs(Ls, OP, ON2, OP1, ON1).
count_poccs([A|Ls], OP, ON, OP1, ON1) :-
	!,
	functor(A, F, N),
	poccs_addto(OP, F/N, OP2),
	count_poccs(Ls, OP2, ON, OP1, ON1).
count_poccs([], OP, ON, OP, ON).

poccs_addto(O, Key, O1) :-
	( select(Key-Val, O, O2) ->
	  Val1 is Val+1,
	  O1 = [Key-Val1|O2]
	; O1 = [Key-1|O]
	).
	
map_add_poccs([~A|Ls], OP, ON, [Val-(~A)|Ls1]) :-
	!,
	functor(A, F, N),
	( memberchk(F/N-Val, ON) -> true ; Val = 1 ),
	map_add_poccs(Ls, OP, ON, Ls1).
map_add_poccs([A|Ls], OP, ON, [Val-A|Ls1]) :-
	!,
	functor(A, F, N),
	( memberchk(F/N-Val, OP) -> true ; Val = 1 ),
	map_add_poccs(Ls, OP, ON, Ls1).
map_add_poccs([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pgnf(F,D,L) :-
	and_or_nf(F,F1),
	pg(F1,[],L,[],D1),
	no_empty_quants(D1,D).

pg(all([X|Xs],G1),Vs,L,Vs1,(all(Vs1,(L -> all([X|Xs],L1))),D1)) :- !,
	append([X|Xs],Vs,Vs2),
	pg(G1,Vs2,L1,Vs01,D1),
	subtract_1(Vs01,[X|Xs],Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
pg(all(X,G1),Vs,L,Vs1,D1) :- !,
	pg(all([X],G1),Vs,L,Vs1,D1).
pg(ex([X|Xs],G1),Vs,L,Vs1,(all(Vs1,(L -> ex([X|Xs],L1))),D1)) :- !,
	append([X|Xs],Vs,Vs2),
	pg(G1,Vs2,L1,Vs01,D1),
	subtract_1(Vs01,[X|Xs],Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
pg(ex(X,G1),Vs,L,Vs1,D1) :- !,
	pg(ex([X],G1),Vs,L,Vs1,D1).
pg((G1 , G2),Vs,L,Vs1,(all(Vs1,(L -> (L1 , L2))),(D1,D2))) :- !,
	pg(G1,Vs,L1,Vs01,D1),
	pg(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
pg((G1 ; G2),Vs,L,Vs1,(all(Vs1,(L -> (L1 ; L2))),(D1,D2))) :- !,
	pg(G1,Vs,L1,Vs01,D1),
	pg(G2,Vs,L2,Vs02,D2),
	append(Vs01,Vs02,Vs2),
	sort(Vs2,Vs1),
	gen_definitional_predicate(P),
	L =.. [P|Vs1].
pg(~G1,Vs,~G1,Vs1,true) :-
	functor(G1, F, N),
	\+ is_df_logop(F, N),
	!,
 	filter_contains_var(Vs,G1,Vs1).
pg(~_,_,_,_,_) :- !,
	err('Non atomic negation in PG normalization').
pg(G1,Vs,G1,Vs1,true) :-
	filter_contains_var(Vs,G1,Vs1).

% pg(~G1,Vs,L,Vs1,(all(Vs1,(L -> (~L1))),D1)) :- !,
% 	( functor(G1, F, N), is_df_logop(F, N) ->
% 	  err('Non atomic negation in PG normalization')
% 	; true
% 	),
% 	pg(G1,Vs,L1,Vs1,D1),
% 	gen_definitional_predicate(P),
% 	L =.. [P|Vs1].
% pg(G1,Vs,L,Vs1,D) :-
% 	%% Introduce a definition iff the atom contains variables
%  	filter_contains_var(Vs,G1,Vs1),
% 	( Vs1 = [] ->
% 	  L = G1,
% 	  D = true
% 	; gen_definitional_predicate(P),
%  	  L =.. [P|Vs1],
% 	  D = all(Vs1,(L -> G1))
% 	).

subtract_1([], _, []) :- !.
subtract_1([E|T], D, R) :-
	memberchk(E, D), !,
	subtract_1(T, D, R).
subtract_1([H|T], D, [H|R]) :-
	subtract_1(T, D, R).
