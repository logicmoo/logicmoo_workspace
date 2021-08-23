%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016, 2019 Christoph Wernhard
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

:- module(craigtask_cm,
	  [ cm_craigproof/2,
	    cm_craig_symmetric/3 ]).

:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(options)).
:- use_module(nf(nf)).
:- use_module(ipol_support).
:- use_module(prooftask_cm).
:- use_module(proofs_cm).
:- use_module(tabx).
:- use_module(tabx_dotgraph).
:- use_module(external_prover9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cm_craig_symmetric(As, Options, Bs) :-
	default_options(Options,
			[goal_clauses=all,
			 goal_clause_sign=minority],
			Options1),
	cm_ci(As, [], Options1, Bs1),
	reverse(Bs1, Bs).

cm_ci([_], Bs, Options, Bs) :-
	from_options(ip_symmetric_butlast=true, Options, false),
	!.
cm_ci([A|As], Bs, Options, Bs1) :-
	append(As, Bs, RightSide),
	bodylist_to_headform(RightSide, RightSide1),
	( from_options(ip_symmetric_orient=ai_right, Options, ai_left) ->
	  Form = (~RightSide1 -> ~A), NegateIP = true
	; Form = (A -> RightSide1)
	),
	copy_term(Options, Options1),
	once(cm_craigproof(Form, [ip=B1|Options1])),
	( NegateIP == true -> negate(B1, B) ; B = B1 ),
	cm_ci(As, [B|Bs], Options, Bs1).
cm_ci([], Bs, _, Bs).

%%%% 
%%%% Query selection is performed by m_set_goal_clauses. Completeness
%%%% in particular wrt unsatisfiable F and valid G depends on options
%%%% processed there
%%%%
cm_craigproof((F -> G), Options) :-
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
	( memberchk([], MA3) ->
	  MA4 = [['$dummy1'],['$query',~'$dummy1']],
	  MA5 = [['$dummy1'],[~'$dummy1']],
	  MB4 = MB3,
	  MB5 = MB3
	; MA4 = MA3,
	  MA5 = MA3,
	  ( memberchk([], MB3) ->
	    MB4 = [['$dummy2'],['$query',~'$dummy2']],
	    MB5 = [['$dummy2'],[~'$dummy2']]
	  ; MB4 = MB3,
	    MB5 = MB3
	  )
	),

	info(10, 'Simplifying combined matrix'),
	ip_simp_combined(MA4, MB4, Options, MA6, MB6),

	( from_options(goal_from_shared_signature=true, Options, false) ->
	  append(MB6, MA6, MCombined1),
	  m_select_clauses_with_given_predicates(MCombined1, Psab, M1Th, M1Ax),
	  m_set_goal_clauses(M1Th, M1Ax, Options, M1)
	; m_set_goal_clauses(MB6, MA6, Options, M1)
	),

	%% 
	
	MAFinal = MA5, %% without $query
	MBFinal = MB5, %% without $query
	MFinal = M1,
	
	m_info(30, MAFinal, 'Left matrix'),
	m_info(30, MBFinal, 'Right matrix'),
	m_info(30, MFinal, 'Combined matrix'),
	m_features(MFinal, Features),
	m_info_features(30, Features, 'Combined matrix'),

	%% asserta(m(MAFinal, MBFinal, MFinal)),
	
	( is_specified_option(matrices, Options) ->
	  to_options(matrices=m(MAFinal,MBFinal,MFinal), Options)
	; true
	),

	( ( is_specified_option(ip_prop, Options)
	  ; is_specified_option(ip, Options)
	  ; is_specified_option(ip_dotgraph, Options) ) ->
	  default_options(Options, [ip_tabx=_], Options1)
	; Options1 = Options
	),
	
	( is_specified_option(ip_tabx, Options1) ->
	  default_options(Options1, [proof=_], Options2)
	; Options2 = Options1
	),

	default_options(Options2, [result=_], Options3a),
	to_options(result=Result, Options3a),

	from_options(enum_ips=EnumIps, Options3a, false),
	( EnumIps = false ->
	  Options3 = Options3a
	; Options3 = [add_cm_options=[acn]|Options3a]
	),
	cmpr3(MFinal, Features, Options3),
	( Result \== proved ->
	  info(10, 'Failed with result: ~w', [Result]),
	  fail
	; true
	),
	( is_specified_option(proof, Options3) ->
	  to_options(proof=P, Options3),
	  ( from_options(ip_tabx=TabX, Options3, NoTabX), TabX \== NoTabX ->
	    from_options(ip_pref_side=AB, Options3, a),
	    p_remove_query(P, P1),
	    m_remove_hconstraints(MAFinal, MAFinal1),
	    m_remove_hconstraints(MBFinal, MBFinal1),
	    cm_proof_to_tabx(P1, MAFinal1, MBFinal1, AB, Options, TabX),
	    ( from_options(ip=IP, Options3, NoIP), IP \== NoIP ->
	      tabx_to_interpolant(TabX, MAFinal, MBFinal, Options3, IP),
	      ( Debug > 3 ->
		info(10, 'Debug: Verifying interpolant'),
		verify_interpolant(F,G,IP)
	      ; true
	      )
	    ; from_options(ip_prop=IPProp, Options3, NoIPProp), IPProp \== NoIPProp ->
	      tabx_to_prop_interpolant(TabX, Options3, IPProp)
	    ; true
	    ),
	    ( from_options(ip_dotgraph=DotGraph, Options3, NoDotGraph), DotGraph \== NoDotGraph ->
	      tabx_to_dotgraph(TabX, DotGraph)
	    ; true
	    )
	  ; true
	  )
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_remove_query([[~'$query']|M], M1) :-
	!,
	m_remove_query(M, M1).
m_remove_query([C|M], [C1|M1]) :-
	c_remove_query(C, C1),
	m_remove_query(M, M1).
m_remove_query([], []).

c_remove_query(['$query'|C], C1) :-
	!,
	c_remove_query(C, C1).
c_remove_query([L|C], [L|C1]) :-
	c_remove_query(C, C1).
c_remove_query([], []).

p_remove_query(q([e('$query', Proofs)]), q(Proofs1)) :-
	!,
	map_p_remove_query_1(Proofs, Proofs1).
p_remove_query(P, _) :-
	err('Proof form mismatch (at top) for remove_query: ~q', [P]).

map_p_remove_query_1([r(~'$query')|Proofs], Proofs1) :-
	!,
	map_p_remove_query_1(Proofs, Proofs1).
map_p_remove_query_1([r(L)|Proofs], [r(L)|Proofs1]) :-
	!,
	map_p_remove_query_1(Proofs, Proofs1).
map_p_remove_query_1([e(L, Proofs)|Proofs1], [e(L, Proofs2)|Proofs3]) :-
	!,
	map_p_remove_query_1(Proofs, Proofs2),
	map_p_remove_query_1(Proofs1, Proofs3).
map_p_remove_query_1([], []) :-
	!.
map_p_remove_query_1(Proofs, _) :-
	err('Proof form mismatch (inside) for remove_query: ~q', [Proofs]).

m_remove_hconstraints(M, M1) :-
	findall(C1, (member(C, M), c_remove_hconstraints(C, C1)), M1).

c_remove_hconstraints([~'$hconstrain'(_,_,_)|C], C1) :-
	!,
	c_remove_hconstraints(C, C1).
c_remove_hconstraints([L|C], [L|C1]) :-
	c_remove_hconstraints(C, C1).
c_remove_hconstraints([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_functor(~A, F, N) :- !, functor(A, F, N).
lit_functor(A, F, N) :- functor(A, F, N).
	
m_select_clauses_with_given_predicates([C|M], Ps, [C|M1], M2) :-
	member(L, C),
	lit_functor(L, P, N),
	memberchk(P/N, Ps),
	!,
	m_select_clauses_with_given_predicates(M, Ps, M1, M2).
m_select_clauses_with_given_predicates([C|M], Ps, M1, [C|M2]) :-
	m_select_clauses_with_given_predicates(M, Ps, M1, M2).
m_select_clauses_with_given_predicates([], _, [], []).

negate(true, false) :-
	!.
negate(false, true) :-
	!.
negate(~F, F) :-
	!.
negate(F, ~F).
