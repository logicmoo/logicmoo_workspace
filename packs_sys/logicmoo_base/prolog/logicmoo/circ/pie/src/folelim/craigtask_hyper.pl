%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
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

%%%% 
%%%% EXPERIMENTAL, NEEDS WORK
%%%% 

:- module(craigtask_hyper,
	  [hyper_craigproof/2,
	   hyper_prove/3,
	   hyper_prove/4
	  ]).

:- use_module(swilib(dotgraph)).
:- use_module(swilib(err)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(graphs)).
:- use_module(swilib(hash)).
:- use_module(swilib(pretty)).
:- use_module(swilib(options)).
:- use_module(swilib(info)).
:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(external_hyper).
:- use_module(external_prover9).
:- use_module(tabx).
:- use_module(tabx_dotgraph).
:- use_module(hyper_proof_conversion).
:- use_module(ipol_support).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Usage:
%%%% 
%%%% See documentation for hyper_prove/3 below. Here is a small example:
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

example_fitting((a, ((b,d);c)),
		~((a;e) -> ~(~b -> c))).

test_craig(Interpolant) :-
	example_fitting(A, NB),
	test_craig(A, NB, Interpolant).

test_craig(A, NB, Interpolant) :-
	hyper_prove(A, ~NB, [verify_ctabx,
			     dotgraph='/tmp/foo.gif',
			     interpolant=Interpolant], Tabx),
	asserta(last_tableaux(Tabx)).



hyper_craigproof((F -> G), Options) :-
	from_options(debug=Debug, Options, 0),
	( Debug > 2 ->
	  info(10, 'Debug: Verifying precondition for interpolant'),
	  ( fol_status_prover9(~(F -> G), [], unsatisfiable) -> true
	  ; asserta(errinfo(failed_precondition, (F -> G))),
	    err('Precondition for interpolant failed', [])
	  )
	; true
	),
	reset_gensyms,
	quantify_unshared_constants((F -> G), (F1 -> G1)),
	info(20, 'Normalizing side a'),
	bodyform_to_bodylist(F1, F2),
	normalize_formulas(F2, Options, MA1),
	info(20, 'Normalizing side b'),
	headform_to_bodylist(G1, G2),
	normalize_formulas(G2, Options, MB1),
	info(20, 'Preparing side a'),
	ip_prep_singleside(MA1, a, MB1, Options, MA2),
	info(20, 'Preparing side b'),
	ip_prep_singleside(MB1, b, MA1, Options, MB2),
	m_predicates(MA2, Psa),
	m_predicates(MB2, Psb),
	ord_intersection(Psa, Psb, Psab),
	info(20, 'Simplifying side a'),
	ip_simp_singleside(MA2, Psab, Options, MA3),
	info(20, 'Simplifying side b'),
	ip_simp_singleside(MB2, Psab, Options, MB3),	
	MAFinal = MA3,
	MBFinal = MB3,
	m_info(30, MAFinal, 'Left matrix'),
	m_info(30, MBFinal, 'Right matrix'),
	( ( is_specified_option(ip_prop, Options)
	  ; is_specified_option(ip, Options)
	  ; is_specified_option(ip_dotgraph, Options) ) ->
	  default_options(Options, [ip_tabx=_], Options1)
	; Options1 = Options
	),
	from_options(hyper=HOptions1, Options1, [] ),
	( is_specified_option(ip_pref_side, Options1) ->
	  from_options(ip_pref_side=AB, Options1, a),
	  HOptions2 = [preferred_side=AB|HOptions1]
	; HOptions2 = HOptions1
	),
	from_options(hyper_proof_version=Version, Options1, new_hyper),
	HOptions3 = [version=Version|HOptions2],
	hyper_prove(MAFinal, MBFinal, HOptions3, TabX),
	( from_options(ip_tabx=TabX, Options1, NoTabX), TabX \== NoTabX ->
	  true
	; true
	),
	( from_options(ip=IP, Options1, NoIP), IP \== NoIP ->
	  tabx_to_interpolant(TabX, MAFinal, MBFinal, Options1, IP),
	  ( Debug > 3 ->
	    info(10, 'Debug: Verifying interpolant'),
	    verify_interpolant(F,G,IP)
	  ; true
	  )
	; from_options(ip_prop=IPProp, Options1, NoIPProp),
	  IPProp \== NoIPProp ->
	  tabx_to_prop_interpolant(TabX, Options1, IPProp)
	; true
	),
	( from_options(ip_dotgraph=DotGraph, Options1, NoDotGraph),
	  DotGraph \== NoDotGraph ->
	  tabx_to_dotgraph(TabX, DotGraph)
	; true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% hyper_prove(+AForm, +Options, -Tabx)
%%%% hyper_prove(+AForm, +BForm, +Options, -Tabx)
%%%%
%%%% Return a term representation of a hypertableau proof of the
%%%% unsatisfiability of the union of AForm and BForm. Fails if
%%%% satisfiable. Invokes Hyper as external sub-system.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Arguments
%%%% 
%%%%   AForm: Input formula or matrix with side a
%%%%   BForm: Input formula or matrix with side b (default: true)
%%%% 
%%%% Options
%%%% 
%%%%   verify_ctabx
%%%%
%%%%     Verify that the result tableaux is indeed a closed (hyper-)tableau
%%%%     for the input clauses
%%%% 
%%%%  before_add=ListOfTerms
%%%%
%%%%     ListOfTerms is passed as input to Hyper, immediately before the
%%%%     formula clauses. Useful to alter flag values from their default
%%%%     setting.
%%%% 
%%%%  before=ListOfTerms
%%%%
%%%%     ListOfTerms is passed as first part of the input to Hyper.
%%%%     Defaulted by the value of hyper_prove_default_before/2.
%%%% 
%%%%  after=ListOfTerms
%%%%
%%%%     ListOfTerms is passed as last part of the input to Hyper.
%%%%     Defaulted by the value of hyper_prove_default_after/2.
%%%% 
%%%%  preferred_side=Side
%%%% 
%%%%     If the input clauses would allow assignment of a tableau node
%%%%     with side a as well as with b, then use the specified side.
%%%%     Side could also be a third value (which then needs to be handled
%%%%     appropriately by the tableau processing methods).
%%%%
%%%%  dotgraph=ImageFileName
%%%%
%%%%      Create an image file with a graph of the result tableaux
%%%%      (requires unix and graphviz installation)
%%%%
%%%%  dotgraph=FreeVariable
%%%%      
%%%%      Bind FreeVariable to dot graph representing the result tableaux.
%%%%      For further processing, see dotgraph.pl
%%%%     
%%%%  proof=FreeVariable
%%%%
%%%%      Bind FreeVariable to the proof term returned by Hyper
%%%%
%%%%  interpolant=FreeVariable
%%%%
%%%%      Bind FreeVariable to an interpolant of (AForm -> ~BForm)
%%%%      Current Limitations:
%%%%
%%%%      - no handling of constant and function signature
%%%%      - no handling of Skolem constants and functions
%%%%      - proper first-order features not fully tested
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Output Format
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% - Intermediate format CTabx: like Tabx but formulas are clauses or
%%%%   literals with free variables.
%%%%
%%%% - Note: Variables between nodes are not shared. This should be fine for
%%%%   positive literals (ensured by the Hypertableau calculus), but might
%%%%   be wrong for negative literals.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Remarks
%%%%
%%%% Hyper's equality handling  is not supported. Instead equality axioms
%%%% are added if =/2 is present in the input. The predicate equal/2 is
%%%% then used instead of =/2 in the input given to Hyper.
%%%%
%%%% Negative literals (i.e. #(set_flag(negative_units, true)) are
%%%% considered in the proof-to-tableau implementation, but so far
%%%% complement splitting is not properly simulated in the translation,
%%%% such that it is advisable to keep #(set_flag(negative_units, false)).
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
hyper_prove(Form, Options, Tabx) :-
	hyper_prove(Form, true, Options, Tabx).
hyper_prove(AForm, BForm, Options, Tabx) :-
	from_options(version=Version, Options, new_hyper),
	set_sk_counter(0),
	ensure_cnf(AForm, Options, AClauses),
	get_sk_counter(_LeastSkFromB),
	ensure_cnf(BForm, Options, BClauses),
	nf_to_tme_with_equality_predicate(AClauses, ATME1),
	nf_to_tme_with_equality_predicate(BClauses, BTME1),
	( memberchk(before=Before, Options) -> true
	; hyper_prove_default_before(Version, Before)
	),
	( memberchk(before_add=BeforeAdd, Options) -> true
	; BeforeAdd = []
	),
	( memberchk(after=After, Options) -> true
	; hyper_prove_default_after(Version, After)
	),
	apply_append([Before, BeforeAdd, ATME1, BTME1, After], TME),
	%% asserta(debug_tme(TME)),
	run_hyper(TME, [result=Result|Options], Proofs),
	( Result \= 0 ->
	  err('Hyper error')
	; true
	),
	( Proofs = [Proof] ->
	  true
	; Proofs = [] ->
	  fail
	; err('Multiple proofs obtained from Hyper')
	),
	set_hyper_proof_version(Version),
	( memberchk(proof=Proof, Options) -> true ; true ),
	%%	asserta(debug_lp(Proof)),
	%% if a clause is available in both sides a and b, then
	%% it gets the preferred side in the tableau
	( memberchk(preferred_side=PreferredSide, Options) -> true
	; PreferredSide=a
	),
	mk_hyper_sidespec(ATME1, BTME1, PreferredSide, SideSpec),
	append(ATME1, BTME1, PTME),
	tme_to_nf(PTME, PCLAUSES1),
	hyper_proof_to_ctabx(Proof, SideSpec, Tabx),
	( memberchk(verify_ctabx, Options) ->
	  verify_ctabx(Tabx, PCLAUSES1)
	; true
	),
	( memberchk(dotgraph=Dotgraph, Options) ->
	  ctabx_to_tabx(Tabx, Tabx1),
	  tabx_to_dotgraph(Tabx1, Dotgraph)
	; true
	),
	%% asserta(debug_tx(Tabx)),
	( memberchk(interpolant=Interpolant, Options) ->
	  ctabx_to_tabx(Tabx, Tabx1),
	  tabx_to_prop_interpolant(Tabx1, Interpolant)
	; true
	).

ensure_cnf([], _, []) :- !.
ensure_cnf(X, _, X) :- X = [_|_], !.
ensure_cnf(X, Options, Y) :-
	%% experimental
	memberchk(def_nf, Options),
	!,
	df(X, D, L),
	cnf((L, D), Y).
ensure_cnf(X, _, Y) :- cnf(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_append([L], L) :-
	!.
apply_append([L|Ls], L1) :-
	append(L, Ls1, L1),
	apply_append(Ls, Ls1).
apply_append([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hyper_prove_default_before(new_hyper,
			   [#(clear_builtins),
			    #(set_flag(proof_terms, true)),
			    #(set_flag(negative_units, false)),
			    #(set_flag(keep_proof_substitutions_flag, false)),
			    #(set_flag(simplify_flag, false))]).
hyper_prove_default_before(old_krh,
			   [#(clear_builtins),
			    #(set_flag(proof_terms, true)),
			    #(set_flag(negative_units, false))]).

hyper_prove_default_after(_, [#(run),
			      #(print_proof)]).
