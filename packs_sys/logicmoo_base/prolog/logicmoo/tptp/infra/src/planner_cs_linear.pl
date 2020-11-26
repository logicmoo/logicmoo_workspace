%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Planner Constraints - Linear Equation Solver
%%%% - Experimental -
/*

- The base algorithm follows the description in:
  Thom Fruehwirt: Theory and Practice of Constraint Handling Rules,
  Journal of Logic Programming, 1994:19, Section 8.6.

- Input expressions are translated "definitoric" by introducing variables
  for subterms.

- Problems:

  - Doubts about completeness:

    csl([N =:= N+3-3+X]). - solution is X = 0

    Translated into three polynominals:
    (Syntax: poly([A- -1, B-1, C-1], 0) stands for A*-1+B*1+C*1 = 0 )
    
    [ poly([A- -1, B-1, C-1], 0),
      poly([C- -1, D-1], 3),
      poly([A-1, D- -1], -3)],

    These allow one elimination step:
  
    [ poly([B-1, C-1, D- -1], -3),
      poly([A- -1, B-1, C-1], 0),
      poly([C- -1, D-1], 3)]

    now it gets stuck.

  - Projection

    How polynomicals should be translated?
    Here (experimental):
          - use the form that has some exported var on the left size
          - add the forms with non-exported vars on the left side
	    as auxiliary equations
	  - for each auxiliary equation, try to unify left side with right
	    side. If not possible, leave it.
          - result seems to depend on the ordering of auxiliary equations.

  - Reduction of arithmetic terms is ad hoc.

*/

:- module(cs_linear, [reduce_cs/2,
		      compile_merge_cs/4,
		      make_true_cs/1,
		      project_cs/3,
		      csl/1]).

:- use_module('swilib/pretty').

make_true_cs([]).

%% Test it.
csl(X) :-
	term_variables(X, Vs),
	compile_cs(X, Vs, Y),
	pp(co(X-Y)), nl,
	project_cs(Y,Vs,Z),
	pp(X-Z), nl.

compile_merge_cs(Cs, Cs1, Cs2, Calls) :-
	term_variables(Cs, Vs),
	compile_cs(Cs, Vs, Cs3),
	append(Cs3, Cs1, Cs4),
	Calls = [cs_linear:reduce_cs(Cs4, Cs2)].

compile_cs(Cs, Vs, Cs1) :-
	in_to_eq(Cs, Cs2),
	pp(eq(Cs2)), nl,
	map_eq_to_npoly(Cs2, Cs3),
	pp(np(Cs3)), nl,
	term_variables(Cs, Vs),
	reduce_cs(Cs3, Cs4),
	remove_isol(Cs4, Vs, Cs1),
	pp(re(Cs1)), nl.

reduce_cs_slack(Cs, Cs1) :-
	red_locally_slack(Cs, Cs2),
	elim(Cs2, Cs3, ReducibleP),
	pp(elim(Cs2, Cs3, ReducibleP)), nl,
	( ReducibleP == true ->
	  reduce_cs_slack(Cs3, Cs1)
	; Cs1 = Cs3
	).

reduce_cs_noslack(Cs, Cs1) :-
	red_locally(Cs, Cs2),
	pp(elim_before(Cs-Cs2)), nl,
	elim(Cs2, Cs3, ReducibleP),
	pp(elim(Cs2, Cs3, ReducibleP)), nl,
	( ReducibleP == true ->
	  reduce_cs_noslack(Cs3, Cs1)
	; Cs1 = Cs3
	).

reduce_cs(Cs, Cs1) :-
	( has_slack_vars(Cs) ->
	  reduce_cs_slack(Cs, Cs1)
	; reduce_cs_noslack(Cs, Cs1)
	).


has_slack_vars(Cs) :-
	member(C, Cs),
	( C = pos(_) ; C = posq(_) ),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Input Compilation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% A "definitoric" transformation into polynomicals, i.e. the
%%%% distributive law is not applied explicitly.
%%%% 

in_to_eq([X =:= Y|Cs], Cs1) :-
	!,
	in_to_eq_1(X, Cs1, Cs2, X1),
	in_to_eq_1(Y, Cs2, Cs3, X1),
	in_to_eq(Cs, Cs3).
in_to_eq([X =< Y|Cs], Cs1) :-
	!,
	in_to_eq([Y >= X|Cs], Cs1).
in_to_eq([X < Y|Cs], Cs1) :-
	!,
	in_to_eq([Y > X|Cs], Cs1).
in_to_eq([X >= Y|Cs], [S >= 0| Cs1]) :-
	!,
	in_to_eq([Y + S =:= X|Cs], Cs1).
in_to_eq([X > Y|Cs], [S > 0| Cs1]) :-
	!,
	in_to_eq([Y + S =:= X|Cs], Cs1).
in_to_eq([C|Cs], [C|Cs1]) :-
	in_to_eq(Cs, Cs1).
in_to_eq([], []).

in_to_eq_1(X, Cs, Cs, X) :-
	var(X),
	!.
in_to_eq_1(X, Cs, Cs, X) :-
	number(X),
	!.
in_to_eq_1(X, [X1 =:= X2|Cs], Cs1, X1) :-
	functor(X, F, N),
	functor(X2, F, N),
	map_in_to_eq_1(N, X, X2, Cs, Cs1).

map_in_to_eq_1(0, _, _, Cs, Cs) :-
	!.
map_in_to_eq_1(N, X, X1, Cs, Cs1) :-
	arg(N, X, Y),
	arg(N, X1, Y1),
	in_to_eq_1(Y, Cs, Cs2, Y1),
	N1 is N - 1,
	map_in_to_eq_1(N1, X, X1, Cs2, Cs1).

%%%% 
%%%% Other arithmetic functions than +,-,*,/ are are written as equations,
%%%% where the left side is a variable, e.g. "X =:= sin(Y)".
%%%% 
eq_to_npoly(X =:= Y + Z, npoly([X-(-1),Y-1,Z-1], 0)) :-
	!.
eq_to_npoly(X =:= Y - Z, npoly([X-(-1),Y-1,Z-(-1)], 0)) :-
	!.
eq_to_npoly(X =:= Y * Z, npoly([X-(-1),A-B], 0)) :-
	!,
	( var(Y) -> Y = A, Z = B ; Y = B, Z = A ).
eq_to_npoly(X =:= Y/Z, Poly) :-
	!,
	eq_to_npoly(Y =:= X*Z, Poly).
eq_to_npoly(X >= 0, posq(X)) :-
	!.
eq_to_npoly(X > 0, pos(X)) :-
	!.
eq_to_npoly(X, X).

map_eq_to_npoly([X|Xs], [X1|Xs1]) :-
    eq_to_npoly(X, X1),
    map_eq_to_npoly(Xs, Xs1).
map_eq_to_npoly([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% - input: must be locally reduced
%%%% - deterministic, does not fail.
%%%%
%%%% - output:
%%%%   - no further vars are bound
%%%%   - sorting and var uniqueness of polys is preserved
%%%%   - poly(...X-0..., K) is already reduced here
%%%%
%%%%   - another poly can be at the start of a clause
%%%%   - poly([X-F], K) can be produced
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elim([poly([X1-F1|XFs1],K1)|Cs], [poly(XFs4, K4)|Cs1], true) :-
	select(poly([X2-F2|XFs2], K2), Cs, Cs2),
	X1 == X2,
	% select(poly(XFs0, K2), Cs, Cs2),
	% select(X2-F2, XFs0, XFs2),
	% X1 == X2,
	!,
	F is F2/F1,
	poly_multiply_factor(XFs1, K1, F, XFs3, K3),
	pp(xfs3(XFs3)), nl,

	pp(poly_subtract(XFs2, K2, XFs3, K3, XFs4, K4)), nl,
	
	poly_subtract(XFs2, K2, XFs3, K3, XFs4, K4),
	elim([poly([X1-F1|XFs1],K1)|Cs2], Cs1, _).
elim([C|Cs], [C|Cs1], ElimP) :-
	elim(Cs, Cs1, ElimP).
elim([], [], _).

poly_multiply_factor(XFs, K, F, XFs1, K1) :-
	K1 is K * F,
	map_multiply(XFs, F, XFs1).

map_multiply([X-F|Xs], Y1, [X-F1|Xs1]) :-
	F1 is F * Y1,
	map_multiply(Xs, Y1, Xs1).
map_multiply([], _, []).

poly_subtract(XFs, K, XFs1, K1, XFs2, K2) :-
	K2 is K - K1,
	map_poly_subtract_1(XFs, XFs1, XFs2).

map_poly_subtract_1([X-F|XFs], [X1-F1|XFs1], XFs2) :-
	compare(O, X, X1),
	( O = (=) ->
	  F2 is F - F1,
	  ( zerop(F2) ->
	    XFs2 = XFs3
	  ; XFs2 = [X-F2|XFs3]
	  ),
	  map_poly_subtract_1(XFs, XFs1, XFs3)
	; O = (<) ->
	  XFs2 = [X-F|XFs3],
	  map_poly_subtract_1(XFs, [X1-F1|XFs1], XFs3)
	; F2 is -F1,
	  XFs2 = [X1-F2|XFs3],
	  map_poly_subtract_1([X-F|XFs], XFs1, XFs3)
	).
map_poly_subtract_1([], XFs, XFs1) :-
	!,
	map_multiply(XFs, -1, XFs1).
map_poly_subtract_1(XFs, [], XFs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


red_locally_slack(Cs, Cs1) :-
	red_locally(Cs, Cs2),
	red_slack_1(Cs2, Cs2, Cs3, UnifiedP),
	( UnifiedP == true ->
	  red_locally_slack(Cs3, Cs1)
	; Cs1 = Cs3
	).

is_slack_poly(poly(XFs, _), Cs) :-
	XFs \= [],
	\+ ( member(X-_, XFs),
	     ( nonvar(X) ->
	       true
	     ; \+ ( member(C, Cs),
		    ( C = pos(X1) ->
		      true
		    ; C = posq(X1) ->
		      true
		    ),
		    X == X1 )
	     )
	   ).

	
red_slack_1([poly(XFs, K)|Cs], CsAll, Cs1, UnifiedP) :-
	is_slack_poly(poly(XFs, K), CsAll),
	!,
	( zerop(K),
	  XFs = [_-F1|XFs2],
	  ( F1 >= 0 ->
	    all_positive(XFs2)
	  ; all_negative(XFs2)
	  ) ->
	  all_to_zero(XFs),
	  Cs1 = Cs2,
	  UnifiedP = true
	; ( K >= 0 ->
	    once(( select(X-F, XFs, XFs1), F >= 0 ))
	  ; once(( select(X-F, XFs, XFs1), F < 0 ))
	  ),
	  Cs1 = [poly([X-F|XFs1], K)|Cs2]
	),
	red_slack_1(Cs, CsAll, Cs2, UnifiedP).
red_slack_1([C|Cs], CsAll, [C|Cs1], UnifiedP) :-
	red_slack_1(Cs, CsAll, Cs1, UnifiedP).
red_slack_1([], _, [], _).	  

all_positive([_-F|XFs]) :-
	F >= 0,
	all_positive(XFs).
all_positive([]).

all_negative([_-F|XFs]) :-
	F < 0,
	all_negative(XFs).
all_negative([]).

all_to_zero([0-_|XFs]) :-
	%% Assume input has only vars, which holds if is_slack_poly is true.
	all_to_zero(XFs).
all_to_zero([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^
%%%% 
%%%% Red Locally
%%%% 
%%%% Reduce each constraint "locally", i.e. without reductions that involve
%%%% several constraints at once.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red_locally(Cs, Cs1) :-
	%% red each C locally until no more red is possible.
	red_locally_1(Cs, Cs2, UnifiedP),
	( UnifiedP == true ->
	  %% one of the local reds effected a variable binding, so
	  %% we repeat red_locally.
	  red_locally(Cs2, Cs1)
	; Cs1 = Cs2
	).

red_locally_1([poly(Ps,K)|Cs], Cs1, UnifiedP) :-
	!,
	keysort(Ps, Ps1),
	red_poly(Ps1, K, Ps2, K1),
	( Ps2 = [] ->
	  ( zerop(K1) -> Cs1 = Cs2 ; fail )
	; Ps2 = [X-Y] ->
	  %% possible BIND var to number  
	  (X is K1/Y -> Cs1 = Cs2, UnifiedP = true ; fail)
	; Cs1 = [poly(Ps2,K1)|Cs2]
	),
	red_locally_1(Cs, Cs2, UnifiedP).
red_locally_1([X =:= Y|Cs], Cs1, UnifiedP) :-
	ground(Y),
	!,
	( var(X) -> UnifiedP = true ; true ),
	X is Y, %% possibly BIND var to number
	red_locally_1(Cs, Cs1, UnifiedP).
%% TODO: inverse templates for number(X) ...
red_locally_1([X =\= Y|Cs], Cs1, UnifiedP) :-
	ground(X),
	ground(Y),
	!,
	X =\= Y,
	red_locally_1(Cs, Cs1, UnifiedP).
red_locally_1([X \= Y|Cs], Cs1, UnifiedP) :-
	!,
	X \== Y,
	( \+ unify_with_occurs_check(X, Y) ->
	  Cs1 = Cs2
	; Cs1 = [X \= Y|Cs2]
	),
	red_locally_1(Cs, Cs2, UnifiedP).
red_locally_1([pos(X)|Cs], Cs1, UnifiedP) :-
	number(X),
	!,
	X > 0,
	red_locally_1(Cs, Cs1, UnifiedP).
red_locally_1([posq(X)|Cs], Cs1, UnifiedP) :-
	number(X),
	!,
	X >= 0,
	red_locally_1(Cs, Cs1, UnifiedP).
red_locally_1([npoly(Ps,K)|Cs], Cs1, UnifiedP) :-
	npoly_to_poly(Ps, Ps1),
	!,
	red_locally_1([poly(Ps1,K)|Cs], Cs1, UnifiedP).
red_locally_1([C|Cs], [C|Cs1], UnifiedP) :-
	red_locally_1(Cs, Cs1, UnifiedP).
red_locally_1([], [], _).



npoly_to_poly([X-F|XFs], [X-F|XFs1]) :-
	ground(F),
	!,
	npoly_to_poly(XFs, XFs1).
npoly_to_poly([X-F|XFs], [F-X|XFs1]) :-
	ground(X),
	var(F),
	!,
	npoly_to_poly(XFs, XFs1).
npoly_to_poly([], []).

%%%% 
%%%% Polynomical Reduction
%%%% 

red_poly([_-F|Ps], K, Ps1, K1) :-
	zerop(F),
	!,
	red_poly(Ps, K, Ps1, K1).
red_poly([X-Y, X1-Y1|Ps], K, Ps1, K1) :-
	var(X),
	X == X1,
	!,
	Y2 is Y+Y1,
	red_poly([X-Y2|Ps], K, Ps1, K1).
red_poly([X-Y|Ps], K, [X-Y|Ps1], K1) :-
	var(X),
	!,
	red_poly(Ps, K, Ps1, K1).
red_poly([X-Y|Ps], K, Ps1, K1) :-
	number(X),
	!,
	K2 is K-X*Y,
	red_poly(Ps, K2, Ps1, K1).
red_poly([], K, [], K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zerop(N) :-
	number(N),
	N =:= 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% equiv(exp(Y), X) :-
% 	Y is log(X).
% equiv(log(Y), X) :-
% 	Y is exp(X).
% equiv(sqrt(Y), X) :-
% 	X >= 0,
% 	Y is X**2.
% equiv(Y**2, X) :-
% 	X >= 0,
% 	Y =:= sqrt(X).
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Projection
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ***  vars might be BOUND by INNER VARS (i.e. arithm expressions)
%%%%     what to do:
%%%%          - copy the stuff
%%%%          - re-equate it 

% input: reduced

project_cs(Cs, Vs, Cs1) :-
	remove_isol(Cs, Vs, Cs2),
	get_slack_vars(Cs2, SVs),
	out_to_eq(Cs2, Vs, SVs, Es, []),
	sort(Es, EsX),
	Es1 = EsX,
	% reverse(EsX, Es1),
	pp(pr1(vs(Vs),inpu(Cs),isol(Cs2),equs(Es1))), nl,
	map_proj(Es1, Vs, Es2),	
	map_red_arithm(Es2, Es3),
	map_cleanup(Es3, Cs1),
	pp(pr2(vs(Vs),inpu(Cs),isol(Cs2),equs(Es1),proj(Es2), reda(Es3), clea(Cs1))), nl.

get_slack_vars([pos(X)|Cs], [X|Xs]) :-
	!,
	get_slack_vars(Cs, Xs).
get_slack_vars([posq(X)|Cs], [X|Xs]) :-
	!,
	get_slack_vars(Cs, Xs).
get_slack_vars([_|Cs], Xs) :-
	get_slack_vars(Cs, Xs).
get_slack_vars([], []).

map_cleanup([X =:= Y|Cs], Cs1) :-
	X == Y,
	!,
	map_cleanup(Cs, Cs1).
map_cleanup([C|Cs], [C1|Cs1]) :-
	match((N+(-A)>Z), C),
	zerop(Z),
	var(A),
	!,
	C1 = (A < N),
	map_cleanup(Cs, Cs1).
map_cleanup([C|Cs], [C1|Cs1]) :-
	match((N+(-A)>=Z), C),
	zerop(Z),
	var(A),
	!,
	C1 = (A =< N),
	map_cleanup(Cs, Cs1).
map_cleanup([C|Cs], [C1|Cs1]) :-
	match((N+A>Z), C),
	zerop(Z),
	var(A),
	!,
	negate(N, N1),
	C1 = (A > N1),
	map_cleanup(Cs, Cs1).
map_cleanup([C|Cs], [C1|Cs1]) :-
	match((N+A>=Z), C),
	zerop(Z),
	var(A),
	!,
	negate(N, N1),
	C1 = (A >= N1),
	map_cleanup(Cs, Cs1).
map_cleanup([C|Cs], [C|Cs1]) :-
	map_cleanup(Cs, Cs1).
map_cleanup([], []).

negate(N, N1) :-
	number(N),
	!,
	N1 is -N.
negate((- N), N) :-
	!.
negate(N, (- N)).


red_arithm(X =:= Y, X1 =:= Y1) :-
	!,
	arithm_red(X, X1),
	arithm_red(Y, Y1).
red_arithm(X > Y, X1 > Y1) :-
	!,
	arithm_red(X, X1),
	arithm_red(Y, Y1).
red_arithm(X >= Y, X1 >= Y1) :-
	!,
	arithm_red(X, X1),
	arithm_red(Y, Y1).
red_arithm(X, X).


map_red_arithm([X|Xs], [X1|Xs1]) :-
	red_arithm(X, X1),
	map_red_arithm(Xs, Xs1).
map_red_arithm([], []).

map_proj([aux(X,Y)|Cs], Vs, Cs1) :-
	!,
	( var(X),
	  ( member(X1, Vs), X == X1 ->
	    Cs1 = Cs2
	  ; unify_with_occurs_check(X, Y) ->
	    Cs1 = Cs2
	  ; Cs1 = [X =:= Y|Cs2]
	  )
	; Cs1 = [X =:= Y|Cs2]
	),
	map_proj(Cs, Vs, Cs2).
map_proj([C|Cs], Vs, [C|Cs1]) :-
	map_proj(Cs, Vs, Cs1).
map_proj([], _, []).	

out_to_eq([poly(XFs,K)|Cs], Vs, SVs, Es, Es1) :-
	!,
	pe_main(XFs, K, Vs, SVs, Es, Es2),
	pe_aux(XFs, K, Vs, SVs, Es2, Es3),
	out_to_eq(Cs, Vs, SVs, Es3, Es1).
out_to_eq([npoly(XFs,K)|Cs], Vs, SVs, Es, Es1) :-
	!,
	pe_main(XFs, K, Vs, SVs, Es, Es2),
	pe_aux(XFs, K, Vs, SVs, Es2, Es3),
	out_to_eq(Cs, Vs, SVs, Es3, Es1).
out_to_eq([pos(X)|Cs], Vs, SVs, [X > 0|Es], Es1) :-
	out_to_eq(Cs, Vs, SVs, Es, Es1).
out_to_eq([posq(X)|Cs], Vs, SVs, [X >= 0|Es], Es1) :-
	out_to_eq(Cs, Vs, SVs, Es, Es1).
out_to_eq([C|Cs], Vs, SVs, [C|Es], Es1) :-
	out_to_eq(Cs, Vs, SVs, Es, Es1).
out_to_eq([], _, _, Es, Es).
% neq ***
% other stuff directly

pe_main(XFs, K, Vs, _, [X =:= RHS|EQs], EQs) :-
	select(X-F, XFs, XFs1),
	member(X1, Vs),
	X1 == X,
	!,
	pe_rhs(XFs1, K, F, RHS).
pe_main([X-F|XFs], K, _, _, [aux(X, RHS)|EQs], EQs) :-
	pe_rhs(XFs, K, F, RHS).

% pe_main(_, _, _, EQs, EQs).
	
pe_rhs(XFs, K, F, RHS) :-	
	map_div_xf(XFs, F, XFs1),
	( ground(F) ->
	  K1 is K/F
	; zerop(K) ->
	  K1 = 0  
	; K1 = K/F
	),
	map_subtract_xf(XFs1, K1, RHS).

pe_aux(XFs, K, Vs, SVs, EQs, EQs1) :-
	pe_aux(XFs, [], K, Vs, SVs, EQs, EQs1).

pe_aux([X-F|XFs], XFs1, K, Vs, SVs, EQs, EQs1) :-
	( member(X1, Vs), X1 == X ->
	  EQs = EQs2
	; rev_append(XFs1, XFs, XFs2),
	  pe_rhs(XFs2, K, F, RHS),	
	  EQs = [aux(X,RHS)|EQs2]
	),
	pe_aux(XFs, [X-F|XFs1], K, Vs, SVs, EQs2, EQs1).
pe_aux([], _, _, _, _, EQs, EQs).

map_subtract_xf([X-F|XFs], E, E1) :-
	( ground(F) ->
	  F1 is F,
	  ( F1 =:= 1 ->
	    e_minus(E, X, E2)
	  ; F1 =:= -1 ->
	    e_plus(E, X, E2)  
	  ; F1 > 0 ->
	    e_minus(E, X*F1, E2)
	  ; F1 < 0 ->
	    F2 is -F1,
	    e_plus(E, X*F2, E2)
	  ; F1 =:= 0 ->
	    E2 = E
	  )
 	; simplify_xf(F, X, X1, Sign) ->
	  ( Sign = (+) ->
	    e_minus(E, X1, E2)
	  ; e_plus(E, X1, E2)
	  )
	; e_minus(E, X*F, E2)
	),
	map_subtract_xf(XFs, E2, E1).
map_subtract_xf([], E, E).

simplify_xf(A/B, X, X/B, +) :- ground(A), A =:= 1, !.
simplify_xf(A/B, X, X/B, -) :- ground(A), A =:= -1, !.
simplify_xf(A/B, X, X*A, +) :- ground(B), B =:= 1, !.
simplify_xf(A/B, X, X*A, -) :- ground(B), B =:= -1, !.

e_plus(E, E1, E1) :-
	zerop(E),
	!.
e_plus(E, E1, (E + E1)).
	
e_minus(E, E1, (- E1)) :-
	zerop(E),
	!.
e_minus(E, E1, (E - E1)).
    
map_div_xf([X-F|Xs], F1, [X-F/F1|Xs1]) :-
    map_div_xf(Xs, F1, Xs1).
map_div_xf([], _, []).

px([X|Xs], Ys) :-
	rev_append(Ys, Xs, Ys1),
	writeln(X-Ys1),
	px(Xs, [X|Ys]).
px([], _).

rev_append([X|Xs], Ys, Zs) :-
	rev_append(Xs, [X|Ys], Zs).
rev_append([], Xs, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Arithmetic Expression Reducer
%%%% 
%%%% Tries to reduce an arithmetic expression with variables.
%%%% Used in the output projection phase.
%%%%
%%%% Notes:
%%%%     Reduction rules are rather ad hoc and could be made more systematic,
%%%%     involving some notion of completeness.
%%%%     If subsumes_chk is not built in, the implementation is not very good.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arithm_red(E, E1) :-
	arithm_red_step(E, E2),
	pp(E2), nl,
	!,
	arithm_red(E2, E1).
arithm_red(E, E).

arithm_red_step(E, E1) :-
	subexp(E, E1, S, S1),
	arithm_red_rule(S, S1),
	!.

arithm_red_rule(E, E1) :-
	compound(E),
	ground(E),
	!,
	E1 is E.
arithm_red_rule(E, E1) :-
	match(X1 - X2, E),
	E1 = (X1 + (- X2)).
arithm_red_rule(E, E1) :-
	match(X1 / X2, E),
	\+ onep(X1),
	E1 = (X1 * (1/X2)).
arithm_red_rule(E, E1) :-
	match(-(-(X1)), E),
	E1 = X1.
arithm_red_rule(E, E1) :-
	match(-(X1 + X2), E),
	E1 = ((-X1) + (-X2)).
arithm_red_rule(E, E1) :-
	match(-(X1 * X2), E),
	E1 = (X1 * (-X2)).
arithm_red_rule(E, E1) :-
	match(X1 + X2, E),
	zerop(X1),
	!,
	E1 = X2.
arithm_red_rule(E, E1) :-
	match(X1 * X2, E),
	onep(X1),
	!,
	E1 = X2.
arithm_red_rule(E, E1) :-
	match(X1 + X2, E),
	ord_less(X2, X1),
	!,
	E1 = (X2 + X1).
arithm_red_rule(E, E1) :-
	match(((X1 + X2) + X3), E),
	ord_less(X3, X2),
	!,
	E1 = ((X1 + X3) + X2).
arithm_red_rule(E, E1) :-
	match((X1 + (X2 + X3)), E),
	E1 = ((X1 + X2) + X3).
arithm_red_rule(E, E1) :-
	match(((- X1) + X1), E),
	!,
	E1 = 0.
arithm_red_rule(E, E1) :-
	match(X1 * X2, E),
	ord_less(X2, X1),
	!,
	E1 = (X2 * X1).
arithm_red_rule(E, E1) :-
	match(((X1 * X2) * X3), E),
	ord_less(X3, X2),
	!,
	E1 = ((X1 * X3) * X2).
arithm_red_rule(E, E1) :-
	match((X1 * (X2 * X3)), E),
	E1 = ((X1 * X2) * X3).

onep(X) :-
	number(X),
	X =:= 1.

ord_less(X, Y) :- var(X), var(Y), !, X @< Y.
ord_less(X, Y) :- nonvar(X), X = (- Z), !, ( Y == Z ; ord_less(Z, Y) ).
ord_less(X, Y) :- nonvar(Y), Y = (- Z), !, ord_less(X, Z).

	
ord_less(X, _) :- var(X), !, fail.
ord_less(_, Y) :- var(Y), !.

% ord_less(X, (- Y)) :- !, ord_less(X, Y).
% ord_less((- X), Y) :- !, ord_less(X, Y).


% ord_less((- X), (- Y)) :-
% 	ord_less(X, Y).
% 
% ord_less((- X), Y) :-
% 	ord_less(X, Y).
% ord_less(X, (- Y)) :-
% 	X == Y.


match(Pattern, Expression) :-
	subsumes_chk(Pattern, Expression),
	unify_with_occurs_check(Pattern, Expression).
		
subexp(E, S, E, S).
subexp(E, E1, S, S1) :-
	compound(E),
	functor(E, F, N),
	functor(E1, F, N),
	map_subexp(1, N, E, E1, S, S1).

map_subexp(N, M, E, E1, S, S1) :-
	arg(N, E, E2),
	arg(N, E1, E3),
	bind_rest(N, M, E, E1),
	subexp(E2, E3, S, S1).
map_subexp(N, M, E, E1, S, S1) :-
	N < M,
	arg(N, E, X),
	arg(N, E1, X),
	N1 is N+1,
	map_subexp(N1, M, E, E1, S, S1).

bind_rest(N, M, E, E1) :-
	N < M,
	!,
	N1 is N+1,
	arg(N1, E, X),
	arg(N1, E1, X),
	bind_rest(N1, M, E, E1).
bind_rest(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Isol
%%%%
%%%% Remove polynomicals with an isolated (i.e. not occuring in another
%%%% constraint nor in the exported vars). Rationale: this polynomical is
%%%% trivially true, since the variable can take an arbitrary value.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_isol(Cs, ExportedVars, Cs1) :-
	remove_isol_1(Cs, ExportedVars, Cs1),
	pp(isol(Cs, Cs1)), nl.

% *** for now npoly are left as they are
remove_isol_1([poly(XFs,_)|Cs], VT, Cs1) :-
	is_isolated(XFs, Cs-VT),
	!,
	remove_isol_1(Cs, [XFs|VT], Cs1).
remove_isol_1([C|Cs], VT, [C|Cs1]) :-
	remove_isol_1(Cs, [C|VT], Cs1).
remove_isol_1([], _, []).

is_isolated(XFs, VT) :-
	member(X-_, XFs),
	var(X),
	\+ contains_var(X, VT),
	!.

