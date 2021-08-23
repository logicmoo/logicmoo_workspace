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

:- module(unskolemize,
	  [unskolemize/4,
	   tnnfpl_unskolemize/4]).

:- use_module(logop_fol).
:- use_module(auxiliary_fol).
:- use_module(simp_fol, [quants_gather/2, topex_outwards/2]).
:- use_module(tform).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Unskolemize
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Status: success, fail/1, henkin
%%%% 

/*
Not yet implemented (see Goranko ??? conradie ???):
- techniques involving distribution (F & (G v H)) == (F & G) v (F & H)
  these would allow to rename the function names of Skolem functions,
  such that less variable sharing is required:
  (we use sk1x here as placeholder for a formula with sk1x):
  == ex sk1 : allx sk1x & ((ally sk1y) v (allz sk1z)) 
  == ex sk1 :   (allx sk1x & all y sk1y)
              v (allx sk1x & all z sk1z)
  == ex sk1 :   (allx sk1x & sk1x)
              v (allz sk1z & sk1z)
  ==   ex sk1 : (allx sk1x & sk1x)
     v ex sk2 : (allz sk2z & sk2z)	      	      
- issue: identify those subformulas where this is useful
*/

%%%% 
%%%% 
%%%%
unskolemize(F, Options, F1, Status) :-
	so_to_tnnfpl(F, F2),
	tnnfpl_unskolemize(F2, Options, F3, Status),
	logform_gather_quantifiers(F3, F4),
	tnnfpl_to_so(F4, F1).

tnnfpl_unskolemize(F, _, F, success) :-
	skolem_terms(F, SK),
	SK = [],
	!.
tnnfpl_unskolemize(F, Os, F1, Status) :-
	( F \= all(_, _) ->
	  %% this is a patch to handle e.g.
	  %% unskolemize((p(sk1),q(sk1)), [], F1, S).
	  F0 = all(_, F)
	; F0 = F
	),
	merge_allquants_for_unskolemization(F0, Os, F2),
	skolem_terms(F2, SK),
	skolem_terms_requirements(SK, Os, Req),
	( Req \= [] ->
	  F1 = F2,
	  tnnfpl_to_so(F2,F2Src),
	  Status = fail(skolem_term_standardization,
			skolem_terms_requirements(F2Src,SK))
	; map_add_var(SK, XSK),
	  insert_ex_quantifiers(XSK, F2, Os, F3, Status),
	  ( Status = success ->
	    replace_nonvar_subterm(F3, XSK, F4),
	    simp_after_unskolemization(F4, F1)
	  ; F1 = F2
	  )
	).

map_add_var([X|Xs], [_-X|Xs1]) :-
	map_add_var(Xs, Xs1).
map_add_var([], []).

insert_ex_quantifiers([X-SK|XSKs], F, Os, F1, Status) :-
	SK =.. [_|Vars],
	sort(Vars, Vars1),
	( option(henkin, Os), ieq_if(F, X, Vars1, [], F2) ->
	  insert_ex_quantifiers(XSKs, F2, Os, F1, Status)
	; ieq(F, X, Vars1, F2) ->
	  insert_ex_quantifiers(XSKs, F2, Os, F1, Status)
	; Status = fail(insert_ex_quantifiers,
			insert_ex_quantifiers([X-SK|XSKs], F, Os, F1, _)),
	  F1 = F
	).
insert_ex_quantifiers([], F, _, F, success).

ieq((F;G), X, Vars, (F1;G)) :- ieq(F, X, Vars, F1), !.
ieq((F;G), X, Vars, (F;G1)) :- ieq(G, X, Vars, G1), !.
ieq((F,G), X, Vars, (F1,G)) :- ieq(F, X, Vars, F1), !.
ieq((F,G), X, Vars, (F,G1)) :- ieq(G, X, Vars, G1), !.
ieq(ex(V,F), X, Vars, ex(V,F1)) :- !, ieq(F, X, Vars, F1).
ieq(all(V,F), X, Vars, F1) :-
	!,
	logupon_list(V, V1),
	deleteq(Vars, V1, RestVars, RestV1),
	( RestVars = [] ->
	  ( RestV1 = [] ->
	    F1 = all(Vars, ex([X], F))
	  ; F1 = all(Vars, ex([X], all(RestV1, F)))
	  )
	; RestV1 = [] ->
	  F1 = all(V1, F2),
	  ieq(F, X, RestVars, F2)
	).

ieq_if((F;G), X, Vars, Minus, (F1;G)) :- ieq_if(F, X, Vars, Minus, F1), !.
ieq_if((F;G), X, Vars, Minus, (F;G1)) :- ieq_if(G, X, Vars, Minus, G1), !.
ieq_if((F,G), X, Vars, Minus, (F1,G)) :- ieq_if(F, X, Vars, Minus, F1), !.
ieq_if((F,G), X, Vars, Minus, (F,G1)) :- ieq_if(G, X, Vars, Minus, G1), !.
ieq_if(ex(V,F), X, Vars, Minus, ex(V,F1)) :- !, ieq_if(F, X, Vars, Minus, F1).
ieq_if(all(V,F), X, Vars, Minus, F1) :-
	!,
%	qvar_variables(V, V1),
	logupon_list(V, V1),
	deleteq(Vars, V1, RestVars, RestV1),
	( RestVars = [] ->
	  mk_if_quantifier(X, Minus, X1),
	  ( RestV1 = [] ->
	    F1 = all(Vars, ex(X1, F))
	  ; F1 = all(Vars, ex(X1, all(RestV1, F)))
	  )
	; append(Minus, RestV1, Minus1),
	  F1 = all(V1, F2),
	  ieq_if(F, X, RestVars, Minus1, F2)
	).

mk_if_quantifier(X, Minus, X1) :-
	( Minus=[] -> X1 = X
	; Minus = [M] -> X1 = X-M
	; X1 = X-Minus
	).

merge_allquants_for_unskolemization(F, Os, F1) :-
	logform_gather_quantifiers(F, F2),   % *** ???
	topex_outwards(F2, F2a),
	logform_gather_quantifiers(F2a, F4), % *** ???
	skolem_terms(F4, SK),
	skolem_terms_requirements(SK, Os, Req),
	merge_sk_quants(F4, Req, F1).

merge_sk_quants((F, G), Req, FG) :-
	!,
	merge_sk_quants(F, Req, F1),
	merge_sk_quants(G, Req, G1),
	conjoin_sk_quants(F1, G1, Req, FG).	
merge_sk_quants((F; G), Req, FG) :-
	!,
	merge_sk_quants(F, Req, F1),
	merge_sk_quants(G, Req, G1),
	disjoin_sk_quants(F1, G1, Req, FG).
merge_sk_quants(all(V, F), Req, F1) :-
	!,
	get_uprefix(all(V, F), V1, F2),
	merge_sk_quants(F2, Req, F3),
	get_uprefix(F3, V3, F4),
	append(V1, V3, V4),
	( V4 = [] ->
	  F1 = F4
	; F1 = all(V4, F4)
	).
merge_sk_quants(ex(V, F), Req, ex(V, F1)) :-
	!,
	merge_sk_quants(F, Req, F1).
merge_sk_quants(F, _, F).

conjoin_sk_quants(F, G, Req, FG) :-
	!,
	get_uprefix(F, VF, F1),
	get_uprefix(G, VG, G1),
	combine_req_quants(VF, VG, Req, VF1, VG1, VFG),
	conjoin_with_uprefixes(F1, G1, VF1, VG1, VFG, FG).

disjoin_sk_quants(F, G, Req, FG) :-
	!,
	get_uprefix(F, VF, F1),
	get_uprefix(G, VG, G1),
	outwards_req_quants(VF, VG, Req, VF1, VG1, VFG),
	disjoin_with_uprefixes(F1, G1, VF1, VG1, VFG, FG).

%%%%
%%%% Idea: Push just those outwards that need to be shared since they occur in
%%%% the requirements list (i.e. as arguments in same position of some Skolem
%%%% function). They are pushed outwards over "and" and "or" such that they
%%%% can reach the position where they can be potentially merged.
%%%%
%%%% Note: there is another reason for sharing variables: to obtain a shared
%%%% prefix of different Skolem functions for unskolemization. The generally
%%%% inwards policy for "and" and the possibility to insert the existential
%%%% variables not just "globally" at a single prefix at the front seems
%%%% to give few reasons for this situation. (Can currently not find an example,
%%%% where unskolemization fails due to this issue).
%%%%
combine_req_quants(VF, VG, Req, VF1, VG1, [X|VFG]) :-
	select(X, VF, VF2),
	select(Y, VG, VG2),
	memberq(X-Y, Req),
	!,
	X = Y,
	combine_req_quants(VF2, VG2, Req, VF1, VG1, VFG).
combine_req_quants(VF, VG, Req, VF1, VG1, [X|VFG]) :-
	select(X, VF, VF2),
	member(X1-_, Req),
	X1 == X,
	!,
	combine_req_quants(VF2, VG, Req, VF1, VG1, VFG).
combine_req_quants(VF, VG, Req, VF1, VG1, [X|VFG]) :-
	select(X, VG, VG2),
	member(X1-_, Req),
	X1 == X,
	!,
	combine_req_quants(VF, VG2, Req, VF1, VG1, VFG).
combine_req_quants(VF, VG, _, VF, VG, []).

outwards_req_quants(VF, VG, Req, VF1, VG1, [X|VFG]) :-
	select(X, VF, VF2),
	member(X1-_, Req),
	X1 == X,
	!,
	outwards_req_quants(VF2, VG, Req, VF1, VG1, VFG).
outwards_req_quants(VF, VG, Req, VF1, VG1, [X|VFG]) :-
	select(X, VG, VG2),
	member(X1-_, Req),
	X1 == X,
	!,
	outwards_req_quants(VF, VG2, Req, VF1, VG1, VFG).
outwards_req_quants(VF, VG, _, VF, VG, []).

get_uprefix(all(V, F), V1, F) :-
	!,
	term_variables(V, V1).
get_uprefix(F, [], F).

conjoin_with_uprefixes(F, G, VF, VG, VFG, FG) :-
	( VF = [] ->
	  F1 = F
	; F1 = all(VF, F)
	),
	( VG = [] ->
	  G1 = G
	; G1 = all(VG, G)
	),
	( VFG = [] ->
	  FG = (F1, G1)
	; FG = all(VFG, (F1, G1))
	).

disjoin_with_uprefixes(F, G, VF, VG, VFG, FG) :-
	( VF = [] ->
	  F1 = F
	; F1 = all(VF, F)
	),
	( VG = [] ->
	  G1 = G
	; G1 = all(VG, G)
	),
	( VFG = [] ->
	  FG = (F1; G1)
	; FG = all(VFG, (F1; G1))
	).


/*

Example that succeeds with option "nosepfun" (this option is experimental)
and fails otherwise

  unskolemize((all([x], p(sk1(x))), all([y], q(sk2(y)))), [nosepfun], U, S).

This succeeds also without "nosepfun:"

  unskolemize((all([x], p(sk1(x))), all([y], q(sk1(y)))), [], U, S).

*/

skolem_terms_requirements([], _, []) :-
	!.
skolem_terms_requirements([T|Ts], Os, R) :-
	sort([T|Ts], [T1|Ts1]),
	!,
	( option(nosepfun, Os) ->
	  skolem_terms_requirements(T1, Ts1, nosepfun, R1, [])
	; skolem_terms_requirements(T1, Ts1, default, R1, [])
	),
	graphq_st_closure(R1, R).

skolem_terms_requirements(T, [T1|Ts], Sepfun, R, R1) :-
	( Sepfun = nosepfun ->
	  functor(T, _, N),
	  functor(T1, _, N)
	; functor(T, F, N),
	  functor(T1, F, N)
	),
	!,
	T =.. [_|Args],
	T1 =.. [_|Args1],
	map_add_reqs(Args, Args1, R, R2),
	skolem_terms_requirements(T, Ts, Sepfun, R2, R1).
skolem_terms_requirements(_, [T|Ts], Sepfun, R, R1) :-
	!,
	skolem_terms_requirements(T, Ts, Sepfun, R, R1).
skolem_terms_requirements(_, [], _, R, R).

map_add_reqs([X|Xs], [Y|Ys], [X-Y|R], R1) :-
	X \== Y,
	!,
	map_add_reqs(Xs, Ys, R, R1).
map_add_reqs([_|Xs], [_|Ys], R, R1) :-
	map_add_reqs(Xs, Ys, R, R1).
map_add_reqs([], [], R, R).

skolem_terms(F, Ts) :-
	skolem_terms(F, Ts1, []),
	sort(Ts1, Ts).

skolem_terms(F, T, T) :-
	var(F),
	!.
skolem_terms(F, [F|T], T) :-
	functor(F, SK, _),
	logform_is_skolem_functor(SK),
	!.
skolem_terms(F, T, T1) :-
	F =.. [_|Args],
	map_skolem_terms(Args, T, T1).

map_skolem_terms([], T, T).
map_skolem_terms([F|Fs], T, T1) :-
	skolem_terms(F, T, T2),
	map_skolem_terms(Fs, T2, T1).

graphq_st_closure(G, G1) :-
	graphq_add_symmetry(G, G2),
	graphq_add_trans(G2, G1).

graphq_add_symmetry(G, G1) :-
	graphq_add_symmetry_1(G, G2),
	sort(G2, G1).

graphq_add_symmetry_1([X-Y|G], [X-Y,Y-X|G1]) :-
	graphq_add_symmetry_1(G, G1).
graphq_add_symmetry_1([], []).

graphq_add_trans(G, G1) :-
	%% naive implementation
	graphq_trans_conseq(G, XY),
	!,
	graphq_add_trans([XY|G], G1).
graphq_add_trans(G, G).

graphq_trans_conseq(G, X-V) :-
	member(X-Z, G),
	member(U-V, G),
	Z == U,
	X \== V,
	\+ memberq(X-V, G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simp_after_unskolemization(F, F1) :-
	%% To remove quantifiers that are now unused since
	%% they only appeared in skolem functions.
	%% TODO: Could be more sensible about henkin quantifiers.
	quants_gather(F, F1).
