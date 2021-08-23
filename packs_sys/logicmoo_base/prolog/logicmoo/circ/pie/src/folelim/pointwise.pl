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

:- module(pointwise,
	  [elim_points/4,
	   elim_point/4,
	   point_solform/4,
	   points_solforms/4,
	   apply_point_solform/5,
	   apply_points_solforms/5,
	   rigorous_pointsolform/6]).
	   
:- use_module(folelim(utils_fol)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(simp_fol)).
:- use_module(folelim(solution)).
:- use_module(folelim(prettysymbols_fol)).
:- use_module(swilib(err)).

%%%%
%%%% elim_point(+LogicAtom, +F, +Options, -F1)
%%%%
%%%% Eliminate ground atom LogicAtom from F, basically with the expansion
%%%% technique from Lin and Reiter: "Forget it!", 1994. Options includes
%%%% simplification specifiers: "simp_branch", "simp", whose values are passed
%%%% to simp_form/4.
%%%%
elim_point(PA, F, Options, F1) :-
	logform_to_nnf(F, F2),
	logform_vars_to_pretty_symbols((PA,F2), F3),
	F3 = (_, F4),
	elim_point_1(F4, PA, Options, F1).

elim_point_1(F, PA, _, F) :-
	functor(PA, P, N),
	f_signature(F, Ps, _),
	\+ memberchk(P/N, Ps),
	!.
elim_point_1((F1;F2), PA, Options, F3) :-
	!,
	elim_point_1(F1, PA, Options, F4),
	elim_point_1(F2, PA, Options, F5),
	logform_disjoin(F4, F5, F6),
	( memberchk(simp=Simp, Options) -> true ; Simp = fast ),
	simp_form(F6, Simp, 'elim_point_disjoin', F3).
elim_point_1(ex(X, F), PA, Options, ex(X, F1)) :-
	!,
	elim_point_1(F, PA, Options, F1).
elim_point_1(F, PA, Options, F1) :-
	functor(PA, P, N),
	functor(PX, P, N),
	PA =.. [_|A],
	PX =.. [_|X],
	equate(A, X, EP),
	logform_negate(EP, EN),
	logform_disjoin(EP, PX, EPPX),
	SP = X-EPPX,
	logform_conjoin(EN, PX, ENPX),
	SN = X-ENPX,
	( memberchk(simp_branch=SimpE, Options) ->
	  Options1 = [simp_apply=SimpE|Options]
	; Options1 = Options
	),
	apply_sol(F, P/N, SP, Options1, FP),
	apply_sol(F, P/N, SN, Options1, FN),
	( memberchk(simp=Simp, Options) -> true ; Simp = fast ),
	simp_form((FP;FN), Simp, 'elim_point', F1).

equate([X], [Y], X=Y) :-
	!.
equate([X|Xs], [Y|Ys], (X=Y , XYs)) :-
	equate(Xs, Ys, XYs).
equate([], [], true).

%%%%
%%%% elim_points(+LogicAtoms, +F, +Options, -F1)
%%%%
elim_points([], F, _Options, F).
elim_points([A|As], F, Options, F1) :-
	elim_point(A, F, Options, F2),
	elim_points(As, F2, Options, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% point_solform(+PA, +F, +Options, -S)
%%%%
%%%% Compute witness for eliminating logic ground atom PA in F.  Options
%%%% include simplification specifiers: "simp_branch", "simp", whose values
%%%% are handled by simp_form/4.
%%%%
%%%% A solution is represented by a formula (in contrast to
%%%% LambdaList-Formula) as for a single point it is "nullary". Hence we call
%%%% a solution for a point "solform".
%%%%
%%%% Options allows to specify alternate methods:
%%%% Basic method: "t": F[pa\true],
%%%% Experimental: "expand": F[pa\true] v ~F[pa\false], ipol
%%%%
%%%% Option point_optim_trivial=false suppresses use of trivial solutions for
%%%% predicates occurring only in a single polarity.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
point_solform(PA, F, Options, S) :-
	\+ memberchk(point_optim_trivial=false, Options),
	point_solform_trivialcase(PA, F, Options, S),
	info(10, 'Trivial solution found for point ~q: ~q', [PA, S]),
	!.
point_solform(PA, F, Options, S) :-
	( memberchk(point_method=Method, Options) -> true
	; Method = t ),
	( Method = ipol ->
	  point_solform_by_ipol(PA, F, Options, S)
	; Method = expand ->
	  point_solform_by_expand(PA, F, Options, S)
	; Method = t ->
	  point_solform_by_t(PA, F, Options, S)
	; err('Undefined point_method: ~q', [Method])
	).

point_solform_trivialcase(PA, F, _Options, S) :-
	functor(PA, P, N),
	findall(Pol, ( logform_enum_atoms(F, Atom, Pol),
		       functor(Atom, P, N) ),
		Pols),
	( memberchk(pn, Pols) -> fail
	; memberchk(n, Pols) ->
	  ( memberchk(p, Pols) -> fail
	  ; S = false
	  )
	; S = true
	).
	  
point_solform_by_t(PA, F, Options, S) :-
	functor(PA, P, N),
	functor(PX, P, N),
	PA =.. [_|A],
	PX =.. [_|X],
	equate(A, X, EP),
	logform_disjoin(EP, PX, EPPX),
	SP = X-EPPX,
	( memberchk(simp_branch=SimpE, Options) ->
	  Options1 = [simp_apply=SimpE|Options]
	; Options1 = Options
	),
	apply_sol(F, P/N, SP, Options1, FP),
	S1 = FP,
	( memberchk(point_simp=Simp, Options) -> true ; Simp=fast ),
	simp_form(S1, Simp, point_solform_by_t, S).

%%%%
%%%% Applies the EHW combination to the two elements: F[pt\true] and
%%%% F[pt\false]
%%%%
%%%%       (F[true] -> true)
%%%%     & (~F[true] & F[false] -> false)
%%%% ==  F[true] v ~F[false]
%%%%
%%%% 
point_solform_by_expand(PA, F, Options, S) :-
	functor(PA, P, N),
	functor(PX, P, N),
	PA =.. [_|A],
	PX =.. [_|X],
	equate(A, X, EP),
	logform_negate(EP, EN),
	SP = X-(EP;PX),
	SN = X-(EN,PX),
	( memberchk(simp_branch=SimpE, Options) ->
	  Options1 = [simp_apply=SimpE|Options]
	; Options1 = Options
	),
	apply_sol(F, P/N, SP, Options1, FP),
	apply_sol(F, P/N, SN, Options1, FN),
	S1 = (FP ; ~FN),
	( memberchk(point_simp=Simp, Options) -> true ; Simp=fast ),
	simp_form(S1, Simp, 'point_solform_by_expand', S).

%%%%
%%%%       pt <-> G |= F
%%%%  iff  ~F |= ~pt <-> G
%%%%  iff  ~F & ~pt |= G |= ~(~F' & ~p't)
%%%%
point_solform_by_ipol(PA, F, Options, S) :-
	( memberchk(ipol_options=IPOptions, Options) -> true
	; psi_default_ipol_options(IPOptions)
	),
	PA =.. [P|A],
	logform_gen_predicate(P1),
	P1A =.. [P1|A],
	logform_rename_free_predicates(F, [P-P1], pn, F1),
	logform_negate(F, NF),
	LEFT = (NF, ~PA),
	logform_negate((~F1, P1A), RIGHT),
	once(cm_craigproof((LEFT -> RIGHT), [ip=S1|IPOptions])),
	( memberchk(point_simp=Simp, Options) -> true ; Simp=fast ),
	simp_form(S1, Simp, 'point_solform_by_ipol', S).

% 
% psi_default_ipol_options(X) :-
% 	X = [debug=3,
% 	     ip_simp_condense=false,
%       	     goal_from_shared_signature=true,
%  	     steq_options=[tsimp(0)],
%  	     add_cm_options=[hs,steq_x_off,steq_a_off,steq_s_off]-[hd1],
%  	     eq=steq].
% psi_default_ipol_options(X) :-
% 	X = [debug=3,
% 	     goal_from_shared_signature=true,
% 	     % add_cm_options=[hs,steq_x_off,steq_a_off,steq_s_off]-[hd1],
%  	     add_cm_options=[hd]-[hd1],
%  	     eq=steq].
psi_default_ipol_options(X) :-
	X = [add_cm_options=[hd]-[hd1],
 	     eq=steq].

%%%%
%%%% points_solforms(+PAs, +F, +Options, -Ss)
%%%%
%%%% Returns an n-ary solution, as a list of formsols, one for each member of
%%%% PAs. The method is "inside-out witness construction".
%%%%
points_solforms(PAs, F, Options, Ss) :-
	points_solforms_1(PAs, F, Options, Ss1),
	points_solforms_2(PAs, Ss1, Options, Ss).

points_solforms_2([_|PAs], [S|Ss], Options, [S1|Ss1]) :-
	ps_3(PAs, Ss, S, Options, S1),
	points_solforms_2(PAs, Ss, Options, Ss1).
points_solforms_2([], [], _, []).

ps_3([PA|PAs], [S|Ss], S1, Options, S2) :-
	apply_point_solform(S1, PA, S, Options, S3),
	ps_3(PAs, Ss, S3, Options, S2).
ps_3([], [], S, _, S).

points_solforms_1([PA|PAs], F, Options, [S|Ss]) :-
	info(10, 'Finding solution for point ~q', [PA]),
	point_solform(PA, F, Options, S),
	apply_point_solform(F, PA, S, Options, F1),
	points_solforms_1(PAs, F1, Options, Ss).
points_solforms_1([], _, _, []).

%%%%
%%%% apply_point_solform(+F, +PA, +G, +Options, -F1)
%%%%
%%%% The general substitution of pt by G:
%%%% 
%%%% p \ lambda x . ((x=t & G) v (x#t & px))
%%%% 
%%%% G = true and G = false are explainable as special cases:
%%%% p \ lambda x . (x=t v px),   p \ lambda x . (x#t & px)
%%%%
apply_point_solform(F, PA, G, Options, F1) :-
	functor(PA, P, N),
	functor(PX, P, N),
	PA =.. [_|A],
	PX =.. [_|X],
	equate(A, X, EP),
	logform_negate(EP, EN),
	( G = true ->
	  logform_disjoin(EP, PX, G1)
	; G = false ->
	  logform_conjoin(EN, PX, G1)
	; logform_conjoin(EP, G, EPG),
	  logform_conjoin(EN, PX, ENPX),
	  logform_disjoin(EPG, ENPX, G1)
	),
	apply_sol(F, P/N, X-G1, Options, F2),
	( memberchk(simp=Simp, Options) -> true ; Simp=fast ),
	simp_form(F2, Simp, 'apply_point_solform', F1).

apply_points_solforms(F, [PA|PAs], [S|Ss], Options, F1) :-
	!,
	apply_point_solform(F, PA, S, Options, F2),
	apply_points_solforms(F2, PAs, Ss, Options, F1).
apply_points_solforms(F, [], [], _, F).

%%%% 
%%%% Rigorous solution based on point solution
%%%% 
rigorous_pointsolform(F, PA, T, G, Options, F1) :-
	apply_point_solform(F, PA, T, Options, FT),
	R1 = ((G, ~FT) ; (T, FT)),
	( memberchk(simp=Simp, Options) -> true ; Simp=fast ),
	simp_form(R1, Simp, 'rigorous_pointsolform', F1).
