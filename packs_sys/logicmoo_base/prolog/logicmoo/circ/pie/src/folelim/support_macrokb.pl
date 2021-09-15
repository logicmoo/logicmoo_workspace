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
%%%% For use in macro definitions:
%%%%

:- module(support_macrokb,
	  [ ppl_pl/2,
	    register_ppl_pl_method/3,
	    mac_intersection/3,
	    mac_union/3,
	    mac_subtract/3,
	    mac_make_args/2,
	    mac_make_fresh_arg/1,
	    mac_make_fresh_args/2,
	    mac_make_atom/3,
	    mac_free_predicates/2,
	    mac_free_predicates_as_scse/2,
	    mac_free_predicates_as_scsae/2,
	    mac_sc_rename_free_predicates/4,
	    mac_concat/3,
	    mac_rename_free_predicate/5,
	    mac_rename_free_functions/4,
	    mac_get_arity/3,
	    mac_transfer_clauses/4,
	    mac_like/3]).

:- use_module(logop_fol).
:- use_module(scopespec).
:- use_module(prettysymbols_fol, [n_pretty_vars/2]).
:- use_module(macrokb).
:- use_module(swilib(err)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Support Predicates for Use in Macro Definitions
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mac_concat(X, E, Z) :-
	compound(E),
	current_arithmetic_function(E),
	!,
	Y is E,
	concat_atom([X, Y], Z).
mac_concat(X, Y, Z) :-
	concat_atom([X, Y], Z).

mac_intersection(S1, S2, S3) :-
	sort(S1, S4),
	sort(S2, S5),
	ord_intersection(S4, S5, S3).

mac_union(S1, S2, S3) :-
	sort(S1, S4),
	sort(S2, S5),
	ord_union(S4, S5, S3).

mac_subtract(S1, S2, S3) :-
	sort(S1, S4),
	sort(S2, S5),
	ord_subtract(S4, S5, S3).

mac_make_args(N, Xs) :-
	n_pretty_vars(N, Xs).
% 	mnx_1(N, Xs1),
% 	reverse(Xs1, Xs).

mac_make_fresh_arg(X) :-
	logform_gen_function(X).
	
mac_make_fresh_args(N, Xs) :-
	mnx_1_fresh(N, Xs).

mnx_1(0, []) :- !.	
mnx_1(N, [X|Xs]) :-
	concat_atom([x,N], X),
	N1 is N - 1,
	mnx_1(N1, Xs).

mnx_1_fresh(0, []) :- !.	
mnx_1_fresh(N, [X|Xs]) :-
	N1 is N-1,
	logform_gen_function(X),
	mnx_1_fresh(N1, Xs).

mac_make_atom(Predicate, Args, Atom) :-
	Atom =.. [Predicate|Args].

mac_free_predicates(F, S) :-
	mac_expand(F, F1),
	findall(P, tfp1(F1, P), S1),
	sort(S1, S).

mac_free_predicates_as_scse(F, S) :-
	mac_expand(F, F1),
	findall(P, tfp1_pol(F1, P), S1),
	sort(S1, S).

mac_free_predicates_as_scsae(F, S) :-
	mac_expand(F, F1),
	findall(P, tfp1_arity_pol(F1, P), S1),
	sort(S1, S).

tfp1(F, P) :-
	logform_enum_atoms(F, A, _, B),
	functor(A, P, _),
	\+ memberchk(P, B).

tfp1_pol(F, P-Pol) :-
	logform_enum_atoms(F, A, Pol, B),
	functor(A, P, _),
	\+ memberchk(P, B).

tfp1_arity_pol(F, P/Arity-Pol) :-
	logform_enum_atoms(F, A, Pol, B),
	functor(A, P, Arity),
	\+ memberchk(P, B).

mac_get_arity(P, F, N) :-
	mac_expand(F, F2),
	logform_enum_atoms(F2, A, _, B),
	functor(A, P, N),
	\+ memberchk(P, B),
	!.
mac_get_arity(_, _, 0).

mac_rename_free_predicate(F, Pred, Pol, F1, Pred1) :-
	mac_expand(F, F2),
	( var(Pred1) ->
	  logform_gen_predicate(Pred1)
	; true
	),
	logform_rename_free_predicates(F2, [Pred-Pred1], Pol, F1).

%%%% 
%%%% Here F, X, X1 are input arguments
%%%% 
mac_rename_free_functions(F, Xs, Xs1, F1) :-
	mac_expand(F, F2),
	( (Xs = []; Xs = [_|_]) ->
	  mk_rename_map(Xs, Xs1, Map)
	; Map = [Xs-Xs1]
	),
	logform_rename_free_functions(F2, Map, F1).

mk_rename_map([X|Xs], [Y|Ys], [X-Y|XYs]) :-
	mk_rename_map(Xs, Ys, XYs).
mk_rename_map([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% If the scsap format is used, no transfer clauses for pn are created.
%%%% 
complement_pol(p, n).
complement_pol(n, p).

mac_transfer_clauses([P/N-Pol|Ps], Dir, [Q|Qs], Cs) :-
	( Pol = Dir ->
	  T = (P_X -> Q_X)
	; complement_pol(Dir, Pol) ->
	  T = (Q_X -> P_X)
	; T = true
	),
	( T \= true ->
	  ( N = 0 -> C = T ; C = all(X, T) ),
	  mac_make_args(N, X),
	  mac_make_atom(Q, X, Q_X),
	  mac_make_atom(P, X, P_X),
	  ( Ps = [] ->
	    Cs = C
	  ; Cs = (C,Cs1),
	    mac_transfer_clauses(Ps, Dir, Qs, Cs1)
	  )
	; ( Ps = [] ->
	    Cs = true
	  ; mac_transfer_clauses(Ps, Dir, Qs, Cs)
	  )
	).

	  
mac_transfer_clauses([], _, [], true).

% mac_scsap_rename_free_predicates([PSpec-Pol|Ps], F, [Q|Qs], F1) :-
% 	( PSpec = P/_ -> true ; P=PSpec ),
% 	mac_expand(F, F2),
% 	logform_gen_predicate(Q),
% 	logform_rename_free_predicates(F2, [P-Q], Pol, F3),
% 	mac_scsae_rename_free_predicates(Ps, F3, Qs, F1).
% mac_scsap_rename_free_predicates([], F, [], F).

mac_sc_rename_free_predicates(Ps, F, Qs, F1) :-
	findall(P, ( member(PSpec, Ps), scp_functor(PSpec, P) ), Ps1),
	sort(Ps1, Ps2),
	findall(P1-Q1, ( member(P1, Ps2), logform_gen_predicate(Q1) ), PQs),
	findall(Q2, member(_-Q2, PQs), Qs ),
	mac_expand(F, F2),
	logform_rename_free_predicates(F2, PQs, pn, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mac_like(F, Modifiers, F1) :-
	mac_expand_toplevel(F, F2),
	apply_like_modifiers(Modifiers, F2, F1).

apply_like_modifiers([], F, F).
apply_like_modifiers([M|Ms], F, F1) :-
	apply_like_modifier(M, F, F2),
	apply_like_modifiers(Ms, F2, F1).

apply_like_modifier(conjoin(G), F, F1) :-
	!,
	logform_conjoin(F, G, F1).
apply_like_modifier(add_precondition(G), F, (G -> F)) :-
	!.
apply_like_modifier(replace(From, To), F, F1) :-
	!,
	subst_matches(F, From, To, F1).
apply_like_modifier(M, _, _) :-
	err('Unsupported like-modifier: ~q', [M]).

subst_matches(T, From, To, T1) :-
	subsumes_term(From, T),
	!,
	copy_term(From-To, T-T1).
subst_matches(T, _, _, T) :-
	var(T),
	!.
subst_matches(T, _, _, T) :-
	atomic(T),
	!.
subst_matches(T, X, Y, T1) :-
	compound(T),
	T =.. [F|Ts],
	map_subst_matches(Ts, X, Y, Ts1),
	T1 =.. [F|Ts1].

map_subst_matches([X|Xs], Y1, Y2, [X1|Xs1]) :-
	subst_matches(X, Y1, Y2, X1),
	map_subst_matches(Xs, Y1, Y2, Xs1).
map_subst_matches([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic ppl_pl_method/2.
 
register_ppl_pl_method(Pattern, Options, Body) :-
 	retractall(ppl_pl_method(Pattern, _)),
 	( Body = fail ->
 	  true
 	; assert(ppl_pl_method(Pattern, Options) :- Body)
 	).

ppl_pl(Pattern, Options) :-
 	ppl_pl_method(Pattern, Options),
 	!.
ppl_pl(Pattern, Options) :-
	ppl_pl_1(Pattern, Options).


ppl_pl_1(mac_get_arity(P,F,N), Options) :-
	format('~@ \\assign \\mathrm{arity\\ of }\\; ~@\\; \\mathrm{ in }\\; ~@',
	       [write_form(N, Options),
		write_form(P, Options),
		write_form(F, Options)]).
ppl_pl_1(A is B, Options) :-
	format('~@ \\assign ~@',
 	       [write_form(A, Options), write_form(B, Options)]).
ppl_pl_1(mac_concat(A,B,C), Options) :-
	format('~@ \\assign ~@\\concat ~@',
	       [write_form(C, Options), write_form(A, Options),
		write_form(B, Options)]).

ppl_pl_1(sc_to_scse(A,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{(in\\ different\\ representation)}',
	       [write_form(B, Options), write_form(A, Options)]).

ppl_pl_1(sc_to_scsae(A,F,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{with\\ arities\\ from}\\; ~@',
	       [write_form(B, Options), write_form(A, Options), write_form(F, Options)]).

ppl_pl_1(sc_to_scsap(A,F,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{with\\ arities\\ from}\\; ~@',
	       [write_form(B, Options), write_form(A, Options), write_form(F, Options)]).

ppl_pl_1(scsae_to_scsp(A,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{(in\\ different\\ representation)}',
	       [write_form(B, Options), write_form(A, Options)]).

ppl_pl_1(scse_add_duals(A,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{closed\\ under\\ duals}',
	       [write_form(B, Options), write_form(A, Options)]).

ppl_pl_1(sc_duals(A,B), Options) :-
	format('~@ \\assign \\mathrm{duals\\ of}\\; ~@',
	       [write_form(B, Options), write_form(A, Options)]).

ppl_pl_1(scsae_add_duals(A,B), Options) :-
	format('~@ \\assign ~@\\; \\mathrm{closed\\ under\\ duals}',
	       [write_form(B, Options), write_form(A, Options)]).

ppl_pl_1(mac_union(A,B,C), Options) :-
	format('~@ \\assign ~@ \\cup ~@',
	       [write_form(C, Options), write_form(A, Options),
		write_form(B, Options)]).
ppl_pl_1(mac_subtract(A,B,C), Options) :-
	format('~@ \\assign ~@ \\setminus ~@',
	       [write_form(C, Options), write_form(A, Options),
		write_form(B, Options)]).
ppl_pl_1(mac_intersection(A,B,C), Options) :-
	format('~@ \\assign ~@ \\cap ~@',
	       [write_form(C, Options), write_form(A, Options),
		write_form(B, Options)]).

ppl_pl_1(mac_make_args(N, X), Options) :-
	format('~@ \\assign x_1,\\ldots,x_{~@}',
	       [write_form(X, Options), write_form(N, Options)]).

ppl_pl_1(mac_make_fresh_args(N, X), Options) :-
	format('~@ \\assign \\mathrm{a\\ sequence\\ of\\ ~@\\ fresh\\ symbols}',
	      [write_form(X, Options), write_form(N, Options)]).

ppl_pl_1(mac_make_fresh_arg(X), Options) :-
	format('~@ \\assign \\mathrm{a\\ fresh\\ symbol}',
	      [write_form(X, Options)]).

ppl_pl_1(mac_free_predicates(F, S), Options) :-
	format('~@ \\assign \\pplkbFreePredicates{~@}',
	       [write_form(S, Options), write_form(F, Options)]).

ppl_pl_1(mac_free_predicates_as_scse(F, S), Options) :-
	format('~@ \\assign \\pplkbFreePredicates{~@}\\; \\mathrm{in\\ scope\\ representation} ',
	       [write_form(S, Options), write_form(F, Options)]).


ppl_pl_1(mac_transfer_clauses(S1,POL,S2,T), Options) :-
	( POL = p -> S3 = S1, S4 = S2 ; S3 = S2 , S4 = S1 ),
	format('~@ \\assign \\mathrm{transfer\\ clauses}\\; ~@ \\rightarrow ~@',
	       [write_form(T, Options), write_form(S3, Options), write_form(S4, Options)]).

ppl_pl_1(mac_rename_free_predicate(F, P, POL, F1, P1), Options) :-
	( POL = pn -> POL1 = ''
	; format(atom(POL1), '\\textrm{-}\\f{~w}', [POL])
	),
	format('~@ \\assign ~@[~@~w \\mapsto ~@]',
	       [write_form(F1, Options),
		write_form(F, Options),
		write_form(P, Options),
		POL1,
		write_form(P1, Options)]).

% 	format('\\langle ~@,~@ \\rangle \\assign \\pplkbRenameFreeOccurrences{~@}{~@}{\\f{~w}}',
% 	       [write_form(F1, Options),
% 		write_form(P1, Options),
% 		write_form(F, Options),
% 		write_form(P, Options),
% 		POL]).

ppl_pl_1(mac_sc_rename_free_predicates(P, F, P1, F1), Options) :-
	format('~@ \\assign ~@[~@ \\mapsto ~@]',
	       [write_form(F1, Options),
		write_form(F, Options),
		write_form(P, Options),
		write_form(P1, Options)]).

ppl_pl_1(mac_rename_free_functions(F, X, X1, F1), Options) :-
	format('~@ \\assign ~@[~@ \\mapsto ~@]',
	       [write_form(F1, Options),
		write_form(F, Options),
		write_form(X, Options),
		write_form(X1, Options)]).

ppl_pl_1(mac_make_atom(F, Ts, X), Options) :-
	A = '$varfunctor'(F, Ts),
	format('~@ \\assign ~@(~@)',
	      [write_form(X, Options),
	       write_form(A, Options),
	       write_form(Ts, Options)]).

ppl_pl_1(mac_like(F, Modifiers, F1), Options) :-
	format('~@ \\textrm{ is like } ~@ \\textrm{ except }',
	       [write_form(F1, Options),
		write_form(F, Options)]),
	( member(M, Modifiers),
	  ppl_pl_newline,
	  ppl_pl_modifier(M, Options),
	  fail
	; true
	).

ppl_pl_1(logform_negate(F1, F2), Options) :-
	format('~@ \\assign \\mathrm{negate\\ } ~@',
	      [write_form(F2, Options),
	       write_form(F1, Options)]).




ppl_pl_modifier(replace(F, F1), Options) :-
	!,
	format('\\hspace*{2em} ~@ \\textrm{ instead of } ~@',
	       [write_form(F1, Options),
		write_form(F, Options)]).
ppl_pl_modifier(conjoin(F), Options) :-
	!,
	format('\\hspace*{2em} ~@ \\textrm{is conjoined}',
	       [write_form(F, Options)]).
ppl_pl_modifier(add_precondition(F), Options) :-
	!,
	format('\\hspace*{2em} ~@ \\textrm{as precondition }',
	       [write_form(F, Options)]).
ppl_pl_modifier(_, _) :-
	format('Unprintable like-modifier').

ppl_pl_newline :-
	writeln('\\\\').
