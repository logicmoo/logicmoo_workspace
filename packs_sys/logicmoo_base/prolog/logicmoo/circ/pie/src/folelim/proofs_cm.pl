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

:- module(proofs_cm,
	  [cm_proof_to_tabx/2,
	   cm_proof_to_tabx/3,
	   cm_proof_to_tabx/4,
	   cm_proof_to_tabx/6,
	   cm_proof_to_dotgraph/2]).

:- use_module(swilib(options)).
:- use_module(swilib(err)).

:- use_module(tabx).
:- use_module(tabx_dotgraph).

cm_proof_to_tabx(CMProof, Tabx) :-
	  cm_proof_to_tabx(CMProof, [], Tabx).
	  
cm_proof_to_tabx(CMProof, Options, Tabx) :-
	cm_proof_to_tabx(CMProof, all_same_color, Options, Tabx).

cm_proof_to_tabx(CMProof, MA, MB, PC, Options, Tabx) :-
	mk_colorspec(MA, MB, PC, ColorSpec),
	cm_proof_to_tabx(CMProof, ColorSpec, Options, Tabx).

cm_proof_to_tabx(q(SP), ColorSpec, Options, Tabx) :-
	copy_term(SP, SP1),
	term_variables(SP1, Vars),
	( from_options(ip_grounding_vars=different, Options, same) ->
	  instantiate_vars_to_different(Vars, '$k', 1)
	; instantiate_vars(Vars, '$k')
	),
	p_to_tabx_1(q(SP1), ColorSpec, Tabx).

p_to_tabx_1(q(SP), ColorSpec, Tabx) :-
	q_clause(q(SP), Clause),
	lookup_color(ColorSpec, Clause, Color),
	map_p_subproof_to_tab(SP, [], Color, ColorSpec, Tabx).

p_to_tab_1(e(L, SP), ParentColor, Anc, ColorSpec,
	   [tab(L, Color, [closed(L, Color, ParentColor)])|Ts]) :-
	e_clause(e(L, SP), Clause),
	lookup_color(ColorSpec, Clause, Color),
	map_p_subproof_to_tab(SP, [L-ParentColor|Anc], Color, ColorSpec, Ts).
p_to_tab_1(b(L), ParentColor, _, ColorSpec,
	   [tab(L, Color, [closed(L, Color, ParentColor)])]) :-
	lookup_builtin_color(ColorSpec, L, Color).

q_clause(q(SP), Ls) :-
	map_p_get_lit_complem(SP, Ls).
e_clause(e(L, SP), [L|Ls]) :-
	map_p_get_lit_complem(SP, Ls).

p_subproof_to_tab(e(L, SP), Anc, Color, ColorSpec, tab(NL, Color, Ts)) :-
	!,
	l_complem(L, NL),
	p_to_tab_1(e(L, SP), Color, Anc, ColorSpec, Ts).
p_subproof_to_tab(b(L), Anc, Color, ColorSpec, tab(NL, Color, Ts)) :-
	!,
	l_complem(L, NL),
	p_to_tab_1(b(L), Color, Anc, ColorSpec, Ts).
p_subproof_to_tab(r(L), Anc, Color, _,
		  tab(NL, Color, [closed(NL, Color, Color1)])) :-
	l_complem(L, NL),
	member(NL1-Color1, Anc),
	NL1 == NL,
	% pp(member(NL1-Color1, Anc)), nl,
	!.

map_p_subproof_to_tab([X|Xs], Y1, Y2, Y3, [X1|Xs1]) :-
	p_subproof_to_tab(X, Y1, Y2, Y3, X1),
	map_p_subproof_to_tab(Xs, Y1, Y2, Y3, Xs1).
map_p_subproof_to_tab([], _, _, _, []).

p_get_lit_complem(e(L, _), L1) :-
	!,
	l_complem(L, L1).
p_get_lit_complem(b(L), L1) :-
	!,
	l_complem(L, L1).
p_get_lit_complem(r(L), L1) :-
	l_complem(L, L1).

map_p_get_lit_complem([X|Xs], [X1|Xs1]) :-
	p_get_lit_complem(X, X1),
	map_p_get_lit_complem(Xs, Xs1).
map_p_get_lit_complem([], []).

l_complem(~A, A) :-
	!.
l_complem(A, ~A).

instantiate_vars([X|Xs], X) :-
	instantiate_vars(Xs, X).
instantiate_vars([], _).

instantiate_vars_to_different([V|Vs], X, N) :-
	concat_atom([X,N], V),
	N1 is N+1,
	instantiate_vars_to_different(Vs, X, N1).
instantiate_vars_to_different([], _, _).

clause_instance(C1, C2) :-
	%% assuming C1 is ground
	copy_term(C2, C3),
	perm_instance(C1, C3),
	!.

perm_instance([E|Es], G) :-
	select(E, G, G1),
	perm_instance(Es, G1).
perm_instance([], []).

mk_colorspec(MA, MB, PreferredColor, ColorSpec) :-
	ColorSpec = colorspec(MA, MB, PreferredColor).

lookup_color(colorspec(_, _, PreferredColor), [~'$query'], PreferredColor) :-
	!.
lookup_color(colorspec(A, B, PreferredColor), Clause, Color) :-
	!,
	( member(C1, A),
	  clause_instance(Clause, C1) ->
	  ( member(C2, B),
	    clause_instance(Clause, C2) ->
	    Color = PreferredColor
	  ; Color = a
	  )
	; member(C1, B),
	  clause_instance(Clause, C1) ->
	  Color = b
	; err('No color specified for: ~q in ~q, ~q', [Clause, A, B])
	).
lookup_color(_, _, a).

lookup_builtin_color(colorspec(_, _, PreferredColor), _, PreferredColor) :-
	!.
lookup_builtin_color(_, _, a).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cm_proof_to_dotgraph(P, DotGraph) :-
	cm_proof_to_tabx(P, T),
	tabx_to_dotgraph(T, DotGraph).