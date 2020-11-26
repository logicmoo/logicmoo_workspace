
% Code to include in compiled lf_patterns (see Prolog/compile_lf_patterns.pl)

extract_feature_values(LF, FeatureVals) :-
	extract_feature_values1(LF, LF, []-FeatureVals0),
	sort(FeatureVals0, FeatureVals).

extract_feature_values1(LF, FullLF, In-Out) :-
	findall(FeatVal,
		compiled_lf_pattern(LF, FullLF, FeatVal),
		FeatVals),
	append(FeatVals, In, Next),
	!,
	extract_feature_values2(LF, FullLF, Next-Out).

extract_feature_values2(LF, FullLF, In-[BoundaryExpression | In]) :-
	current_predicate(compiled_lf_boundary/3),
	compiled_lf_boundary(LF, FullLF, X^BoundaryExpression),
	!,
	extract_feature_values3(LF, FullLF, []-InnerList),
	X = InnerList.
extract_feature_values2(LF, FullLF, In-Out) :-
	!,
	extract_feature_values3(LF, FullLF, In-Out).

extract_feature_values3(NonTerm, _FullLF, In-In) :-
	\+ compound(NonTerm),
	!.
extract_feature_values3(Term, FullLF, In-Out) :-
	compound(Term),
	functor(Term, _F, N),
	extract_feature_values_args(N, Term, FullLF, In-Out).
 
extract_feature_values_args(0, _Term, _FullLF, In-In) :-
	!.
extract_feature_values_args(I, Term, FullLF, In-Out) :-
	I > 0,
	arg(I, Term, Arg),
	extract_feature_values1(Arg, FullLF, In-Next),
	I1 is I - 1,
	extract_feature_values_args(I1, Term, FullLF, Next-Out).

match_sub_pattern((P, Q), CurrentLF, FullLF) :-
	!,
	match_sub_pattern(P, CurrentLF, FullLF),
	match_sub_pattern(Q, CurrentLF, FullLF),
	!.
match_sub_pattern(Disjunction, CurrentLF, FullLF) :-
	compound(Disjunction),
	Disjunction =.. [or | Args],
	match_sub_pattern_disjunctive_list(Args, CurrentLF, FullLF),
	!.
match_sub_pattern(not(P), CurrentLF, FullLF) :-
	!,
	\+ match_sub_pattern(P, CurrentLF, FullLF),
	!.
match_sub_pattern(local(P), CurrentLF, _FullLF) :-
	!,
	match_sub_pattern(P, CurrentLF, CurrentLF),
	!.
match_sub_pattern(P, _CurrentLF, FullLF) :-
	!,
	match_subterm(FullLF, P),
	!.

match_sub_pattern_disjunctive_list([Single], CurrentLF, FullLF) :-
	!,
	match_sub_pattern(Single, CurrentLF, FullLF).
match_sub_pattern_disjunctive_list([F | R], CurrentLF, FullLF) :-
	(   match_sub_pattern(F, CurrentLF, FullLF)
	;
	    match_sub_pattern_disjunctive_list(R, CurrentLF, FullLF)
	),
	!.

match_subterm(T, Subterm) :-
	T = Subterm,
	!.
match_subterm(T, Subterm) :-
	nonvar(T),
	functor(T, _F1, N1),
	match_subterm_args(N1, T, Subterm),
	!.

match_subterm_args(I, T, Subterm) :-
	I > 0,
	arg(I, T, Arg),
	match_subterm(Arg, Subterm), 
	!.
match_subterm_args(I, T, Subterm) :-
	I > 1,
	I1 is I - 1, 
	!,
	match_subterm_args(I1, T, Subterm).
