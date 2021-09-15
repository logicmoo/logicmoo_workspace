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

:- module(hyper_proof_conversion,
	  [verify_ctabx/2,
	   verify_ctabx_clauses/2,
	   verify_ctabx_instantiation/1,
	   verify_ctabx_links/1,
	   verify_ctabx_sides/1,
	   ctabx_clause/2,
	   ctabx_to_tabx/2,
	   hyper_proof_to_ctabx/2,
	   hyper_proof_to_ctabx/3,
	   mk_hyper_sidespec/4,
	   prtg_test/1,
	   set_hyper_proof_version/1]).

:- use_module(swilib(graphs)).
:- use_module(swilib(err)).
:- use_module(swilib(hash)).
:- use_module(swilib(dotgraph)).
:- use_module(swilib(sysdep)).

:- dynamic hyper_proof_version/1.

%%%% 
%%%% Version =  new_hyper | old_krh
%%%% 
set_hyper_proof_version(X) :-
	retractall( hyper_version(_) ),
	assert( hyper_version(X) ).

:- set_hyper_proof_version(new_hyper).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hyper_proof_to_ctabx(Proof, Tabx) :-
	hyper_proof_to_ctabx(Proof, all_same_side, Tabx).

hyper_proof_to_ctabx(Proof, SideSpec, Tabx) :-
	prtg(Proof, SideSpec, T),
	prtg_to_ctabx(T, Tabx).

ctabx_to_tabx(CTabx, Tabx) :-
	map_ctabx_to_tabx_1(CTabx, Tabx).

ctabx_to_tabx_1(closed(L,C1,C2), closed(F,C1,C2)) :-
	literal_to_form(L, F).
ctabx_to_tabx_1(tab(L,C,CTabx), tab(F,C,Tabx)) :-
	literal_to_form(L, F),
	map_ctabx_to_tabx_1(CTabx, Tabx).

literal_to_form(L, F) :-
	term_variables(L, Vs),
	( Vs = [] ->
	  F = L
	; Vs = [X] ->
	  copy_term(X-L, X1-L1),
	  F = all(X1, L1),
	  find_vars(0, L, [X1])
	; copy_term(Vs-L, Vs1-L1),
	  F = all(Vs1, L1),
	  find_vars(1, L, Vs1)
	).

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
		
map_ctabx_to_tabx_1([X|Xs], [X1|Xs1]) :-
	ctabx_to_tabx_1(X, X1),
	map_ctabx_to_tabx_1(Xs, Xs1).
map_ctabx_to_tabx_1([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% To visualize the internal graph structure for debugging
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prtg_test(ProofTerm) :-
	File = '/tmp/prdot.gif',
	prtg(ProofTerm, T),
	prtg_to_dot(T, G),
	write_gif_dotgraph(G, File),
	format('Written ~w~n', [File]).

prtg_to_dot(T, G) :-
	t_graph(T, Graph),
	findall(edge(Node1, Node2, [dir=forward]),
		( htm_key(Graph, Node1),
		  htm_get(Graph, Node1, Node2) ),
		Edges ),
	findall(N, ( member(edge(N,_,_), Edges)
		   ; member(edge(_,N,_), Edges)
		   ),
		Ns),
	sort(Ns, AllNodes),
	findall(node(Node, [label=Label]),
		( member(Node, AllNodes),
		  dot_pretty_goal(Node, T, Goal),
		  format(atom(Label), '~w: ~w', [Node, Goal])
		),
		Nodes),
	append(Nodes, Edges, DGraph),
	G = dotgraph(d1, DGraph).

dot_pretty_goal(Node, T, Goal) :-
	t_goals(T, Goals),
	node_prid(Node, Id),
	ht_get(Goals, Id, Goal1),
	dot_pretty_goal_1(Goal1, Goal).

dot_pretty_goal_1(c(L,_,_), A) :-
	!,
	format(atom(A), 'CLOSED: ~q', [L]).
dot_pretty_goal_1(g(L,_), A) :-
	!,
	format(atom(A), 'ON_BRANCH ~q', [L]).
dot_pretty_goal_1(h(L, _), A) :-
	!,
	format(atom(A), 'LINKED ~q', [L]).
dot_pretty_goal_1(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Tabx Verification
%%%%   
%%%%   For "clausal" tabx (formula labels with free variables)
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_ctabx(Ts, Clauses) :-
	verify_ctabx_instantiation(Ts),
	verify_ctabx_links(Ts),
	verify_ctabx_clauses(Ts, Clauses),
	verify_ctabx_sides(Ts).

verify_ctabx_clauses(Ts, Clauses) :-
	( ctabx_clause(Ts, C),
	  ( member(C1, Clauses),
	    synt_clause_subsumes_chk(C1, C) ->
	    true
	  ; err('Tableau clause ~q is no instance of source ~q', [C, Clauses])
	  ),
	  fail
	; true
	).

verify_ctabx_sides(Ts) :-
	( ctabx_clause_sides(Ts, Cs),
	  ( Cs = [] -> true
	  ; sort(Cs, [_]) -> true
	  ; err('Tableau clause with multiple sides ~q', [Cs])
	  ),
	  fail
	; true
	).
	    
synt_clause_subsumes_chk(C1,C2) :-
	copy_term(C1,C1c),
	copy_term(C2,C2c),
	term_variables(C2c,Vs),
	copy_term(Vs,Vpattern),
	synt_clause_subsumes1_chk(C1c,C2c,Vs,Vpattern),
	!.

synt_clause_subsumes1_chk([],[],_,_).
synt_clause_subsumes1_chk([L|Ls],C2,Vs,Vpattern) :-
	select(L1,C2,C3),
	unify_with_occurs_check(L, L1),
	subsumes_chk(Vs,Vpattern),
	synt_clause_subsumes1_chk(Ls,C3,Vs,Vpattern).

verify_ctabx_instantiation(Ts) :-
	( ctabx_clause(Ts, C),
	  ( C = [] ->
	    true
	  ; C = [_] ->
	    true
	  ; member(~(A), C),
	    \+ ground(A),
	    err('Negative non-ground literal in non-unit tableau clause ~q', [C])
	  ),
	  fail
	; true
	).

ctabx_clause(Ts, C) :-
	map_tab_literal(Ts, C).
ctabx_clause(Ts, C) :-
	member(tab(_, _, Ts1), Ts),
	ctabx_clause(Ts1, C).

ctabx_clause_sides(Ts, C) :-
	map_tab_side(Ts, C).
ctabx_clause_sides(Ts, C) :-
	member(tab(_, _, Ts1), Ts),
	ctabx_clause_sides(Ts1, C).

tab_literal(tab(L, _, _), L).
tab_side(tab(_, C, _), C).

map_tab_literal([X|Xs], [X1|Xs1]) :-
	tab_literal(X, X1),
	map_tab_literal(Xs, Xs1).
map_tab_literal([], []).

map_tab_side([X|Xs], [X1|Xs1]) :-
	tab_side(X, X1),
	map_tab_side(Xs, Xs1).
map_tab_side([], []).

verify_ctabx_links(Ts) :-
	verify_ctabx_links(Ts, []).
verify_ctabx_links(Ts, Options) :-
	verify_ctabx_links([], Ts, Options).

verify_ctabx_links(Ancestors, Ts, Options) :-
	( member(T, Ts),
	  verify_tableau_links(Ancestors, T, Options),
	  fail
	; true
	).

verify_tableau_links(Ancestors, tab(G, _, []), _) :-
	!,
	err('Non closed branch above ~q: ~q', [G, Ancestors]).
verify_tableau_links(Ancestors, tab(G, _, Ts), Options) :-
	!,
	verify_ctabx_links([G|Ancestors], Ts, Options).
verify_tableau_links(Ancestors, closed(Lit, _, _), _Options) :-
	tab_lit_complement(Lit, NLit),
	( member(Lit1, Ancestors),
	  variant(Lit1, Lit),
	  member(NLit1, Ancestors),
	  subsumes_chk(NLit1, NLit) ->
	  true
	; err('Failed to find link for ~q in ~q', [Lit, Ancestors])
	).

tab_lit_complement(~(A), A) :- !.
tab_lit_complement(A, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Translation Part II: PRTG-Graph to Tabx
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prtg_to_ctabx(T, Tabx) :-
	prtg_to_terms(T, Goals, Graph, Root),
	p_to_s_graph(Graph, Graph2),
	memberchk(Root-Ns, Graph2),
	map_tgt(Ns, Graph2, Goals, Tabx).

prtg_to_terms(T, Goals1, Graph1, Root) :-
	t_graph(T, Graph),
	t_root(T, Root),
	t_goals(T, Goals),
	findall(M1-M2, (ht_key(Goals, M1), ht_get(Goals, M1, M2)), Goals1),
	findall(N1-N2, (htm_key(Graph, N1), htm_get(Graph, N1, N2)), Graph1).

tgt(N, Graph, Goals, Tab) :-
	node_prid(N, PrId),
	memberchk(N-Ns, Graph),
	( memberchk(PrId-Goal, Goals) -> true ; Goal = not_found ),
	( Goal = c(Lit, SideLit, SideOtherLit) ->
	  Tab = closed(Lit, SideLit, SideOtherLit)
	; (Goal = g(Lit, Side) ; Goal = h(Lit, Side)) ->
	  Tab =  tab(Lit, Side, Children),
	  map_tgt(Ns, Graph, Goals, Children)
	; err('Bad goal: ~q for node ~q', [Goal, N])
	).

map_tgt([X|Xs], Y1, Y2, [X1|Xs1]) :-
	tgt(X, Y1, Y2, X1),
	map_tgt(Xs, Y1, Y2, Xs1).
map_tgt([], _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Translation Part I: Hyper Proof Term to "PRTG" Graph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(graphnode, _, 0).

mk_graphnode_id(Id) :-
	flag_inc(graphnode, N),
	concat_atom(['n', N], Id).

reset_graphnode_id :-
	flag(graphnode, _, 0).

mk_e(P, SideSpec, e(P, SideSpec)).

e_top_proof(e(X, _), X).
e_side_spec(e(_, X), X).

mk_t(Graph, Root, t(Graph, Root, Goals)) :-
	htm_capacity(Graph, Capacity),
	mk_ht(Capacity, Goals).

t_graph(t(X, _, _), X).
t_root(t(_, X, _), X).		   
t_goals(t(_, _, X), X).

is_false_goal(c(L,_,_)) :- !, L = false.
is_false_goal(g(L,_)) :- !, L = false.
is_false_goal(h(L,_)) :- !, L = false.
is_false_goal(X) :- err('Bad goal: ~q', [X]).

mk_hyper_sidespec(A, B, Pref, sidespec(A, B, Pref)).

e_lookup_side(E, Clause, Side) :-
	e_side_spec(E, SideStruct),
	lookup_side(SideStruct, Clause, Side).

lookup_side(all_same_side, _, a) :-
	!.
lookup_side(sidespec(A, B, PreferredSide), Clause, Side) :-
	!,
	( member(C1, A),
	  variant_clause(Clause, C1) ->
	  ( member(C2, B),
	    variant_clause(Clause, C2) ->
	    Side = PreferredSide
	  ; Side = a
	  )
	; member(C1, B),
	  variant_clause(Clause, C1) ->
	  Side = b
	; err('No side specified for: ~q in ~q, ~q', [Clause, A, B])
	).

%%%% 
%%%% Hyper sometimes returns clauses with re-ordered heads in proofs,
%%%% this has to be considered here.
%%%% 
variant_clause(C, C1) :-
	variant(C, C1),
	!.
variant_clause((H :- B), (H1 :- B1)) :-
        !,
        variant(B, B1),
	head_variant(H, H1).
variant_clause(H, H1) :-
	head_variant(H, H1).

head_variant(H, H1) :-
	syntactic_litdisj_to_list(H, L),
	syntactic_litdisj_to_list(H1, L1),
	synt_clause_subsumes_chk(L, L1),
	synt_clause_subsumes_chk(L1, L).

set_goal(ThisId, Goal, Side, T) :-
	t_goals(T, GoalTable),
	( var(Goal) ->
	  %% might result for special nodes
	  ht_put(GoalTable, ThisId, g(Side))
	; stdize_fact(Goal, Goal1) ->
	  ht_put(GoalTable, ThisId, g(Goal1, Side))
	; %% non-facts as goals are not needed
	  true
	).

t_pr_get_goal(T, P, Goal, Side) :-
	t_goals(T, GoalTable),
	pr_id(P, Id),
	ht_get(GoalTable, Id, G),
	G = g(Goal, Side),
	!.
t_pr_get_goal(_, P, _, _) :-
	pr_id(P, Id),
	err('No matching goal entry for ~q', [Id]).

prid_get_side(Id, T, Side) :-
	t_goals(T, GoalTable),
	ht_get(GoalTable, Id, G),
	( G = g(_, Side) -> true
	; G = g(Side) -> true
	),
	!.
prid_get_side(Id, _, _) :-
	err('No side entry for ~q', [Id]).	     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prtg(P, T) :-
	prtg(P, all_same_side, T).
prtg(P, SideSpec, T) :-
	pr_mk_graph(P, Graph, Root),
	mk_e(P, SideSpec, E),
	mk_t(Graph, Root, T),
	proc_nodes(Root, E, T),
	gr_elim_splits(Root, Graph),
	t_goals(T, Goals),
	gr_elim_false(Root, Graph, Goals).

proc_nodes(Node, E, T) :-
	t_graph(T, G),
	findall(N1, htm_get(G, Node, N1), SubNodes),
	( member(N2, SubNodes),
	  proc_node(N2, Node, E, T),
	  fail
	; true
	),
	( member(N2, SubNodes),
	  proc_nodes(N2, E, T),
	  fail
	; true
	).

proc_node(Node, ParentNode, E, T) :-
	node_prid(Node, ThisId),
	e_top_proof(E, P0),
	pr_get_subproof(P0, ThisId, P),
	( proc_node_1(P, ParentNode, E, T) ->
	  true
	; err('Processing of node failed: ~q', [Node])
	).

proc_node_1(P, ParentNode, E, T) :-
	node_or_split_args(P, ThisId, Goal, Rule, Sub, SubN),
	!,
	( (Rule = input_fact ; Rule = input_clause), nonvar(Goal) ->
	  e_lookup_side(E, Goal, ClauseSide),
	  set_goal(ThisId, Goal, ClauseSide, T)
	; Rule = hyper(Clause) ->
	  e_lookup_side(E, Clause, ClauseSide),
	  set_goal(ThisId, Goal, ClauseSide, T),
	  copy_term(Clause, Clause1),
	  ( Clause1 = (Head1 :- Body) -> true
	  ; Clause1 = Head1,
	    Body = true
	  ),
	  syntactic_litconj_to_list(Body, PBody),
	  length(PBody, LenB),
	  r_subs(hyper, ThisId, LenB, Sub, E, Sub1),
	  r_process_body(PBody, Sub1, T, ParentNode, ClauseSide),	  
	  syntactic_litdisj_to_list(Head1, Head2),
	  r_process_head(Head2, SubN, T, ParentNode, ClauseSide)
	; Rule = composed ->
	  true
	; err('No tableau conversion support for rule: ~q', [Rule])
	).
proc_node_1(P, _ParentNode,  _, T) :-
 	P = branch(ThisId, Goal, SplitOrRef),
	!,
	branch_clause_id(SplitOrRef, ClauseId),
	prid_get_side(ClauseId, T, ClauseSide),
	set_goal(ThisId, Goal, ClauseSide, T).

branch_clause_id(ref(Id), Id) :-
	!.
branch_clause_id(P, Id) :-
	split_args(P, Id, _, _, _),
	!.
branch_clause_id(X, _) :-
	err('Failed to find branch clause id for ~q', [X]).

stdize_fact(~(A), ~(A)) :- !.
stdize_fact((false :- A), ~(A)) :- !.
stdize_fact((A :- true), A) :- !.
stdize_fact((H :- B), (H :- B)) :- !, fail.
stdize_fact(A, A).

r_process_body([A|As], [P|Ps], T, ParentNode, ClauseSide) :-
	t_pr_get_goal(T, P, Goal, OtherSide),
	copy_term(Goal, G1),
	( unify_with_occurs_check(A, G1) ->
	  true
	; err('Failed to reconstruct rule instance for literals ~q and ~q',
	      [A, G1])
	),
	attach_closed_node(T, ParentNode, ~(A), ClauseSide, OtherSide),
	r_process_body(As, Ps, T, ParentNode, ClauseSide).
r_process_body([], [], _, _, _).

attach_closed_node(T, ParentNode, L, ClauseSide, OtherSide) :-
	mk_graphnode_id(LitNodeId),
	mk_graphnode_id(ClosureNodeId),
	t_graph(T, GraphTable),
	htm_add(GraphTable, ParentNode, LitNodeId),
	htm_add(GraphTable, LitNodeId, ClosureNodeId),
	t_goals(T, GoalTable),
	ht_put(GoalTable, LitNodeId, h(L, ClauseSide)),
	ht_put(GoalTable, ClosureNodeId, c(L, ClauseSide, OtherSide)).

r_process_head(H, SN, T, ParentNode, ClauseSide) :-
	findall(H2-NB2, uhsc(H, SN, T, H2, NB2), Results),
	( Results = [_-NB] ->
	  r_process_closed_head(NB, T, ParentNode, ClauseSide)
	; err('Ambiguity for head and negative literals: ~q',
	     [Results])
	).
r_process_closed_head([A-OtherSide|ACs], T, ParentNode, ClauseSide) :-
	attach_closed_node(T, ParentNode, A, ClauseSide, OtherSide),
	r_process_closed_head(ACs, T, ParentNode, ClauseSide).
r_process_closed_head([], _, _, _).

uhsc(H, [P|SN1], T, H1, [A-OtherSide|NB]) :-
	t_pr_get_goal(T, P, ~(G), OtherSide),
	copy_term(G, G1),
	select(A, H, H2),
	unify_with_occurs_check(A, G1),
	uhsc(H2, SN1, T, H1, NB).
uhsc(H, [], _, H, []).

r_subs(input, Id, LenB, Sub, _, Sub) :-
	!,
	( length(Sub, LenB) ->
	  true
	; err('Mismatch of body and subproofs: ~q', [Id])
	).
r_subs(hyper, Id, LenB, Sub, _E, Sub1) :-
	hyper_version(new_hyper),
	!,
	length(Sub, LenSub),
	( LenSub is 1 + LenB ->
	  Sub = [_|Sub1]
	; err('Mismatch of body and subproofs: ~q (proof from old krh?)', [Id])
	).
r_subs(hyper, Id, LenB, Sub, _E, Sub) :-
	hyper_version(old_krh),
	!,
	( length(Sub, LenB) ->
	  true
	; err('Mismatch of body and subproofs: ~q', [Id])
	).

pr_get_subproof(ref(_), _, _) :-
	!,
	fail.
pr_get_subproof(P, Id, P1) :-
	pr_id(P, Id),
	!,
	P1 = P.
pr_get_subproof(P, Id, P1) :-
	pr_sub_proof(P, P2),
	pr_get_subproof(P2, Id, P1),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pr_mk_graph(P, T, Root) :-
	reset_graphnode_id,
	pr_deps(P, Deps),
	pr_ordered_nodes(P, Nodes, Splits),
	reverse(Nodes, Nodes1),
	%% writeq(ordering(Nodes1)), nl,
	length(Nodes1, L),
	HSize is round(L/4),
	mk_htm(HSize, T),
	mk_ht(HSize, IT),
	mk_node(root, Root),
	pr_mk_graph_1(Nodes1, [Root], Deps, Splits, T, IT).

gr_elim_false(N, T, Goals) :-
	findall(N1, htm_get(T, N, N1), N1s),
	( member(N2, N1s),
	  gr_elim_false(N2, T, Goals),
	  fail
	; true
	),
	findall(N3, htm_get(T, N, N3), N3s),
	( member(NF, N3s),
	  node_prid(NF, PrId),
	  ht_get(Goals, PrId, Goal),
	  is_false_goal(Goal) ->
	  htm_del(T, N, NF),
	  false
	; true
	).

gr_elim_splits(N, T) :-
	findall(N1, htm_get(T, N, N1), N1s),
	( member(N2, N1s),
	  gr_elim_splits(N2, T),
	  fail
	; true
	),
	findall(N3, htm_get(T, N, N3), N3s),
	( member(NS, N3s),
	  node_prid(NS, PrId),
	  is_split_node_id(PrId) ->
	  htm_del(T, N, NS),
	  findall(N5, htm_get(T, NS, N5), N5s),
	  ( member(N6, N5s),
	    htm_del(T, NS, N6),
	    htm_add(T, N, N6),
	    fail
	  ; true
	  )
	; true
	).

pr_mk_graph_1([This|Ns], Leafs, Deps, Splits, T, IT) :-
	memberchk(This-DepsForThis, Deps),
	pr_mk_graph_2(Leafs, This, DepsForThis, Splits, T, IT, Leafs1),
	pr_mk_graph_1(Ns, Leafs1, Deps, Splits, T, IT).
pr_mk_graph_1([], _, _, _, _, _).

pr_mk_graph_2([Leaf|Leafs], This, DepsForThis, Splits, T, IT, Leafs1) :-
	%% 
	%% It seems that this test could introduce redundant items, is a
	%% stronger test possible?
	%%
	( all_are_in_branch(DepsForThis, Leaf, IT) ->
	  true
	; fail
	),
	!,
	mk_node(This, ThisNode),
	htm_add(T, Leaf, ThisNode),
	ht_put(IT, ThisNode, Leaf),
	( node_prid(Leaf, LeafId),
	  is_split_node_id(LeafId) ->
	  findall(Child, htm_get(T, Leaf, Child), Children),
	  length(Children, Len1),
	  memberchk(LeafId-SplitSuccessors, Splits),
	  length(SplitSuccessors, Len2),
	  ( Len1 = Len2 ->
	    append(Children, Leafs2, Leafs1)
	  ; Len1 < Len2 ->
	    Leafs1 = [Leaf|Leafs2]
	  ; err('Graph error: ~q ~q ~q', [Leaf, This, Children])
	  )
	; Leafs1 = [ThisNode|Leafs2]
	),
	pr_mk_graph_2(Leafs, This, DepsForThis, Splits, T, IT, Leafs2).
pr_mk_graph_2([Leaf|Leafs], This, DepsForThis, Splits, T, IT, [Leaf|Leafs1]) :-
	pr_mk_graph_2(Leafs, This, DepsForThis, Splits, T, IT, Leafs1).
pr_mk_graph_2([], _, _, _, _, _, []).

node_prid(n(_, PrId), Id) :-
	!,
	Id = PrId.
node_prid(X, X).
	  
mk_node(PrId, n(NodeId, PrId)) :-
	mk_graphnode_id(NodeId).

all_are_in_branch([N|Ns], N1, IT) :-
	is_in_branch(N, N1, IT),
	all_are_in_branch(Ns, N1, IT).
all_are_in_branch([], _, _).

is_in_branch(N, Node, _) :-
        node_prid(Node, N),		   
	!.
is_in_branch(N, N1, IT) :-
	ht_get(IT, N1, N2),
	is_in_branch(N, N2, IT).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pr_deps(P, G) :-
	findall(E, pr_dep_edge(P, E), G1),
	p_to_s_graph(G1, G).

pr_dep_edge(P, Id1-Id2) :-
	pr_id(P, Id1),
	pr_sub_proof(P, P1),
	pr_id(P1, Id2).
pr_dep_edge(P, Edge) :-
	pr_sub_proof(P, P1),
	pr_dep_edge(P1, Edge).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pr_direct_goal(P, G) :-
	( functor(P, node, _) ->
	  arg(2, P, G)
	; functor(P, branch, _) ->
	  arg(2, P, G)
	),
	G \= '---'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pr_ordered_nodes(P, Nodes, SplitDependencies) :-
	findall(E, pr_simp_edge(P, E), Es),
	p_to_s_graph(Es, G0),
	%% add additional edges to the effect that branching nodes are put
	%% downward in the tableau, so far just by comparing nodes with
	%% identical precondition sets:
	findall(ND-NP, ( member(ND-Pres, G0),
			 is_split_node_id(ND),
			 member(NP-Pres, G0),
			 NP \= ND,
			 \+ is_split_node_id(NP)
		       ),
		UnitPreferences),
	append(Es, UnitPreferences, Es1),
	p_to_s_graph(Es1, G1),
	%% pp(G1), nl,
	%% Topological sorting of all nodes
	top_sort(G1, Nodes1),
	rm_composition_nodes(Nodes1, Nodes2),
	findall(DE, pr_disj_edge(P, DE), DisjEs),
	findall(BN, member(_-BN, DisjEs), BNs),
	sort(BNs, BNs1),
	rm_nodes(Nodes2, BNs1, Nodes3),
	p_to_s_graph(DisjEs, DG1),
	findall(D1-Ns1, (member(D1-Ns1, DG1), is_split_node_id(D1)),
	       SplitDependencies),
	%% Insert or-branches in the total ordering immediately below their
	%% disj parents.
	nodelist_insert_disj_children(Nodes3, SplitDependencies, Nodes).

pr_disj_edge(branch(Id, _, SplitOrRef), Id1-Id) :-
	pr_id(SplitOrRef, Id1).
pr_disj_edge(P, Edge) :-
	pr_sub_proof(P, P1),
	pr_disj_edge(P1, Edge).
	
pr_simp_edge(P, Id1-Id2) :-
	pr_id(P, Id1),
	pr_sub_proof(P, P1),
	pr_id(P1, Id2).
pr_simp_edge(P, Edge) :-
	pr_sub_proof(P, P1),
	pr_simp_edge(P1, Edge).

pr_id(P, Id) :-
	arg(1, P, Id).

rm_composition_nodes([N|Ns], N1) :-
	is_composition_node_id(N),
	!,
	rm_composition_nodes(Ns, N1).
rm_composition_nodes([N|Ns], [N|N1]) :-
	rm_composition_nodes(Ns, N1).
rm_composition_nodes([], []).

rm_nodes([N|Ns], Ns1, Ns2) :-
	memberchk(N, Ns1),
	!,
	rm_nodes(Ns, Ns1, Ns2).
rm_nodes([N|Ns], Ns1, [N|Ns2]) :-
	rm_nodes(Ns, Ns1, Ns2).
rm_nodes([], _, []).

nodelist_insert_disj_children([N|Ns], DG, Ns1) :-
	is_split_node_id(N),
	memberchk(N-Ns2, DG),
	!,
	append(Ns2, [N|Ns3], Ns1),
	nodelist_insert_disj_children(Ns, DG, Ns3).
nodelist_insert_disj_children([N|Ns], DG, [N|Ns1]) :-
	nodelist_insert_disj_children(Ns, DG, Ns1).
nodelist_insert_disj_children([], _, []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
is_split_node_id(Item) :-
	sub_atom(Item, 0, _, _, disj).

is_composition_node_id(Item) :-
	sub_atom(Item, 0, _, _, composed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

syntactic_litconj_to_list(true, []) :-
	!.
syntactic_litconj_to_list((L,F), [L|Ls]) :-
	!,
	syntactic_litconj_to_list(F, Ls).
syntactic_litconj_to_list(L, [L]).

syntactic_litdisj_to_list(false, []) :-
	!.
syntactic_litdisj_to_list((L;F), [L|Ls]) :-
	!,
	syntactic_litdisj_to_list(F, Ls).
syntactic_litdisj_to_list(L, [L]).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
pr_sub_proof(node(_, _, hyper(_), _, [_|S1], S2), P) :-
	hyper_version(new_hyper),
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(node(_, _, hyper(_), [_|S1], S2), P) :-
	hyper_version(new_hyper),
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(node(_, _, _, _, S1, S2), P) :-
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(node(_, _, _, S1, S2), P) :-
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(split(_, _, _, [_|S1], S2), P) :-
	hyper_version(new_hyper),
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(split(_, _, [_|S1], S2), P) :-
	hyper_version(new_hyper),
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(split(_, _, _, S1, S2), P) :-
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(split(_, _, S1, S2), P) :-
	!,
	( member(P, S1)
	; member(P, S2)
	).
pr_sub_proof(branch(_, _, P), P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node_or_split_args(N, Id, Goal, Rule, Sub, SubN) :-
	node_args(N, Id, Goal, Rule, Sub, SubN),
	!.
node_or_split_args(N, Id, _, Rule, Sub, SubN) :-
	split_args(N, Id, Rule, Sub, SubN).

node_args(N, Id, Goal, Rule, Sub, SubN) :-
	N = node(Id, Goal1, Rule1, Sub, SubN),
	!,
	fix_rule(Rule1, Rule),
	fix_clause(Goal1, Goal).
node_args(N, Id, Goal, Rule, Sub, SubN) :-
	N = node(Id, Goal1, Rule1, _InstRule, Sub, SubN),
	fix_rule(Rule1, Rule),
	fix_clause(Goal1, Goal).

split_args(N, Id, Rule, Sub, SubN) :-
	N = split(Id, Rule1, Sub, SubN),
	!,
	fix_rule(Rule1, Rule).
split_args(N, Id, Rule, Sub, SubN) :-
	N = split(Id, Rule1, _InstRule, Sub, SubN),
	fix_rule(Rule1, Rule).


pr_is_composition_node(node(_, _, composed, _, _, _)) :- !.
pr_is_composition_node(node(_, _, composed, _, _)) :- !.
	
fix_rule(hyper(C), hyper(C1)) :- !, fix_clause(C, C1).
fix_rule(R, R).

fix_clause((H :- []), H) :- !.
fix_clause(~(A), (false :- A)) :- !.
fix_clause(C, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Hyper's Proof Format
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% from fact.mli, edited
%%%%
%%%% Note: InstRule - see keep_proof_substitutions_flag (new Hyper)
%%%% we do not use this here, but re-compute the instances from
%%%% the goals of the subproofs
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% A proof term is structured as follows:
%%%%       node(Id, Goal, Rule, InstRule, Subproofs, NegativeSubproofs) |
%%%%       node(Id, Goal, Rule, Subproofs, NegativeSubproofs) |
%%%%       split(Id, Rule, Subproofs, NegativeSubproofs) |
%%%%       split(Id, Rule, InstRule, Subproofs, NegativeSubproofs) |
%%%%       branch(Id, Goal, Subproof) |
%%%%       ref(Id)
%%%% 
%%%%    - Id     is an atom identifying the node.
%%%%    - Goal   is the subgoal proven by the node.
%%%%    - Rule   is an atom or compound representing the applied deduction rule.
%%%%            It is one of
%%%%            - builtin       - not further specified
%%%%            - input_fact    - an input fact
%%%%            - hyper(Clause) - hypertableau/resolution inference
%%%%                               step with the specified
%%%%                               input clause.
%%%%                               Clause is given as term.
%%%%    
%%%%    - Subproofs is a list of proof terms. It represents the
%%%%       subproofs used to prove Goal with Rule.
%%%%    - NegativeSubproofs are subproofs that resolve head literals.
%%%%       They could result from negative input literals and complement
%%%%       splitting.
%%%%    - A proof term with node/5 structure represents a proof of Goal
%%%%       by applying Rule to Subproofs and NegativeSubproofs
%%%%    - A proof term with split/4 structure represents a disjunction, that
%%%%       has been derived by applying Rule to Subproofs and
%%%%       NegativeSubproofs.
%%%%    - A proof term with branch/3 structure represents a disjunct
%%%%       contained in the disjunction of the split/4 structure Subproof.
%%%%     - A proof term with ref/1 structure is a reference to
%%%%       the node with the given identifier.
%%%%


