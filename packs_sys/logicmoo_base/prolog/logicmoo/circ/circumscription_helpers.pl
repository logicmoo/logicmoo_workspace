:- module(circumscription_helpers,
	  [ relativize/3,
			mac_transfer_clauses_strict_subset/3,
			mac_transfer_clauses_strict_subset/5,
			sc_to_scsa/3,
			scsae_to_scsa/2,
			mac_free_predicates_as_scsa/2,
			mac_sc_rename_free_functions/4,
			mac_free_functions_as_scsa/2,
			mac_free_functions_as_sc/2,
			tff1_pol/2,
			fun_closures/3,
			list2tuple/2,
			mkAtom/3,
			logform_enum_functions/3,
			logform_enum_functions/2,
			logform_enum_terms/2,
			constant_term/1,
			term_arity/2,
			mac_transfer_clausesL/4,
			rel_subset/4,
			rel_subset_strict/5,
			pred/3,
			succ/3,
			dual/4,
			mkConjunct/3,
			prio_subset_rel/7,
			prio_subset_rel/6,
			prio_strictness/5,
			ppl_print/1,
			logform_gen_predicate/2,
			last_ppl_form_result/1,
			set_last_ppl_form_result/1,
			insertAtPos/4
	    ]).

:- use_module(folelim(support_scratch)).
:- use_module(folelim(support_macrokb)).
:- use_module(folelim(logop_fol)).
:- use_module(library(lists)).


%%%%
%%%% relativize(+RelativizerPredicate, +InputFormula, -OutputFormula)
%%%%
relativize(P, F, F1) :-
	logform_process_subforms(F, relativize1(P), F1).

relativize1(P, all(X, F), all(X, (PX -> F))) :-
	!,
	add_relat(X, P, PX).
relativize1(P, ex(X, F), ex(X, (PX, F))) :-
	!,
	add_relat(X, P, PX).
relativize1(_, F, F).

add_relat([], _, true) :-
	!.
add_relat(X, P, PX) :-
	atom(X),
	!,
	add_relat([X], P, PX).
add_relat([X|Xs], P, PXs) :-
	PX =.. [P, X],
	( Xs = [] ->
	  PXs = PX
	; PXs = (PX, PXs1),
	  add_relat(Xs, P, PXs1)
	).

:- assert(ppl_pl(relativize(P, F, F1), Options) :-
	format('~@ \\assign ~@^~@',
	       [write_form(F1, Options), write_form(F, Options), write_form(P, Options)])).



mac_transfer_clauses_strict_subset(S, S1, Cs) :-
	mac_transfer_clausesL(S, p, S1, Cs1),
	mac_transfer_clausesL(S, n, S1, Cs2),
	Cs = (Cs1, ~Cs2).


mac_transfer_clauses_strict_subset(_, _, [], [], true) :- !.
mac_transfer_clauses_strict_subset(Dom1, Dom2, S, S1, Cs) :-
	mac_transfer_clausesL(S, p, S1, Cs1),
	relativize(Dom1, Cs1, Cs1Rel),
	mac_transfer_clausesL(S, n, S1, Cs2),
	relativize(Dom2, Cs2, Cs2Rel),
	Cs = (Cs1Rel, ~Cs2Rel).

:- assert(ppl_pl(mac_transfer_clauses_strict_subset(D1, D2, S1,S2,T), Options) :-
	format('~@ \\assign (~@ \\subset ~@)^{~@, ~@}',
	       [write_form(T, Options), write_form(S1, Options), write_form(S2, Options), write_form(D1, Options), write_form(D2, Options)])).

%def(fun_closure(P, F)) :: F1 ::-
%	fun_closures(P, F, F1).
%

transform([A], A):-
  A=..[_].
transform([A,B], (A,B)):-
  B=..[_].
transform([A,B,C|Tail], L):-
  L=..[',',A,T],
  transform([B,C|Tail], T).


sc_to_scsa(S, F, S1) :-
	sc_to_scsae(S, F, S2),
	scsae_to_scsa(S2, S3),
	sort(S3, S1).

scsae_to_scsa([], []).
scsae_to_scsa([X-_|Xs], [X|Zs]) :- scsae_to_scsa(Xs, Zs).


mac_free_predicates_as_scsa(F, S) :-
	mac_free_predicates_as_scsae(F, S1),
	scsae_to_scsa(S1, S2),
	sort(S2, S).


mac_sc_rename_free_functions(F, Fs1, Gs, F1) :-
	mac_free_functions_as_sc(F, Fs1),
	findall(F1-G1, ( member(F1, Fs1), logform_gen_function(G1) ), FGs),
	findall(Q2, member(_-Q2, FGs), Gs ),
	mac_expand(F, F2),
	logform_rename_free_functions(F2, FGs, F1).

%return the functions and their polarities in the formula
mac_free_functions_as_scsa(F, Fs1) :-
	mac_expand(F, F1),
	findall(X, tff1_pol(F1, X) ,Fs),
	sort(Fs, Fs1).

mac_free_functions_as_sc(F, Fs3) :-
	mac_free_functions_as_scsa(F, Fs1),
	findall(F2, member(F2/_, Fs1), Fs2),
	sort(Fs2, Fs3).


tff1_pol(F, Func) :-
	logform_enum_atoms(F, A, _, B),
	A =.. [_|Terms],
	member(Term, Terms),
	logform_enum_functions(Term, B, Func).


fun_closures(P, F, F1) :-
	mac_expand(F, F2),
	findall(X, tff1_pol(F2, X) ,Fs),
	sort(Fs, Fs1),
	maplist(fun_closure(P), Fs1, Fs2),
	list2tuple(Fs2, F1).


fun_closure(P, F/0, F1) :- !, F1 =.. [P, F].
fun_closure(P, F/N, F1) :- !,
	mac_make_fresh_args(N, Args),
	maplist(mkAtom(P), Args, Left),
	Fs =.. [F| Args],
	Ps =.. [P , Fs],
	list2tuple(Left, LeftT),
	F1 = (all(Args, (LeftT -> Ps))).


list2tuple([], true) :- !.
list2tuple(X, Y) :- list2tuple1(X,Y), !.

list2tuple1([X|Xs], (X, Ys)) :-
	list2tuple1(Xs, Ys).
list2tuple1([X], (X)) :- !.



mkAtom(P, X, F) :-
	F =.. [P, X].


logform_enum_functions(Term, Bound, F/A) :-
	logform_enum_terms(Term, T),
	functor(T, F, A),
	\+ memberchk(F, Bound).

% given a term enumerate all function symbols an their arity
logform_enum_functions(Term, F/A) :-
	logform_enum_terms(Term, T),
	functor(T, F, A).

% given a term enumerate all subterms
logform_enum_terms(T, T). %each term is subterm of itself
logform_enum_terms(T, Ts) :-
	\+ constant_term(T),
	T =.. [_|Ts1],
	member(CurT, Ts1),
	logform_enum_terms(CurT, Ts).


constant_term(T) :-
	term_arity(T, A),
	A = 0.

term_arity(T, A) :-
	functor(T, _, A).


%


insertAtPos(List, Elem, 0, [Elem|List]) :- !.
insertAtPos([X|Xs], Elem, Pos, [Y|Ys]) :-
	Y = X,
	Pos1 is Pos - 1,
	insertAtPos(Xs, Elem, Pos1, Ys).




mac_transfer_clausesL([P/N|Ps], Dir, [Q|Qs], Cs) :- mac_transfer_clauses([P/N-p|Ps], Dir, [Q|Qs], Cs).


rel_subset(R, P1/N, P2, F) :-
	mac_transfer_clausesL([P1/N], p, [P2], Cs1),
	relativize(R, Cs1, F).

rel_subset_strict(R1, R2, P1/N, P2, (F1, ~F2)) :-
	rel_subset(R1, P1/N, P2, F1),
	rel_subset(R2, P2/N, P1, F2).

% Is X predecessor of Y?
pred(Ord, X, Y) :- member((X, Y), Ord).
pred(Ord, X, Y) :- member((X, Z), Ord), pred(Ord, Z, Y).

succ(Ord, X, Y) :- pred(Ord, Y, X).

%smaller(Ord, X, Y) :- call(Ord, X, Y).
%smaller(Ord, X, Y) :- call(Ord, Z, Y), smaller(Ord, X, Z).

dual(Xs, Ys, X, Y) :-
	nth0(I, Xs, X),
	nth0(I, Ys, Y).

%zip([], [], []).
%zip([X| Xs], [Y|Ys], [Z|Zs]) :- Z = (X, Y), zip(Xs, Ys, Zs).

mkConjunct(A, B, (A;B)).


%%
%Ord must be a partial order relation defined on the Outer Predicates either as a relation or given as a list of tuples
%InnerPredicates are expected to be in sca form (with arity annotations).
prio_subset_rel(OuterDom, InnerDom, Ord, OuterPreds, InnerPreds, XInner/N, (~Pre -> Cs)) :-
	dual(InnerPreds, OuterPreds, XInner/N, XOuter),
	rel_subset(InnerDom, XInner/N, XOuter, Pre),
	findall(C,
			(	pred(Ord, ZOuter, XOuter),
				dual(OuterPreds, InnerPreds, ZOuter, ZInner),
				rel_subset_strict(InnerDom, OuterDom, ZInner, ZOuter, C))
		, CsList),
	foldl(mkConjunct, CsList, false, Cs).




prio_subset_rel(_, _, _, [], [], true) :- !.
prio_subset_rel(OuterDom, InnerDom, Ord, OuterPreds, InnerPreds, Cs) :-
	findall(C,
		( member(XInner/N, InnerPreds),
			prio_subset_rel(OuterDom, InnerDom, Ord, OuterPreds, InnerPreds, XInner/N, C))
		, CsList),
	list2tuple(CsList, Cs).

prio_strictness(OuterDom, InnerDom, OuterPreds, InnerPreds, Cs) :-
	findall(C,
		(  member(XInner/N, InnerPreds),
			 dual(OuterPreds, InnerPreds, XOuter, XInner/N),
			 rel_subset_strict(InnerDom, OuterDom, XInner/N, XOuter, C))
		,  CsList),
	foldl(mkConjunct, CsList, false, Cs).


%

:- dynamic last_ppl_form_result/1.

set_last_ppl_form_result(X) :-
	retractall( last_ppl_form_result( _ ) ),
	assert( last_ppl_form_result(X) ).


%


ppl_print(Label) :-
	ppl_print(Label, [input=Label, expand=true]).

ppl_print(Label, Options) :-
	append(Options, [input=Label, expand=true], Options1),
	ppl_form(Label, Options1).

logform_gen_predicate(Pred, _) :- logform_gen_predicate(Pred).
