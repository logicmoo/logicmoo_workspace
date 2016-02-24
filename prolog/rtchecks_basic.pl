:- module(rtchecks_basic, [
		collapse_prop/4,
		diff_props/3,
		get_pretty_names/4,
		checkif_to_lit/3,
		get_checkc/5,
		get_checkc/6,
		is_member_prop/2,
		is_same_prop/2,
		lists_to_lits/2,
		remove_element/3
	    ]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- doc(author, "Edison Mera").

:- doc(module, "Basic predicates used in rtchecks expansion.").

check_props_names(Name, Check, Value, (\+Check, Name=Value)).

compound_checkc(CheckProps0, Name, Props, CheckC) :-
	list_to_disj(CheckProps0, CheckProps),
	CheckC = findall(Name, CheckProps, Props).

get_checkc(_, _, [], _, [], true) :- !.
get_checkc(compat, M, Props, Names, PropValues, CheckC) :-
	compound_check_props(compat, M, Props, CheckProps0),
	maplist(check_props_names(Name), CheckProps0, Names, CheckProps),
	compound_checkc(CheckProps, Name, PropValues, CheckC).
get_checkc(call, M, Props, Names, PropValues, CheckC) :-
	compound_check_props(instance, M, Props, CheckProps0),
	maplist(check_props_names(Name), CheckProps0, Names, CheckProps),
	compound_checkc(CheckProps, Name, PropValues, CheckC).

explicit_fail(Check, PropValues,
	      (\+ Check -> PropValues = [_|_] ; PropValues = [])).

compound_checkc(CheckProps0, PropValues, CheckC) :-
	list_to_lits(CheckProps0, CheckProps),
	explicit_fail(CheckProps, PropValues, CheckC).

get_checkc(_, _, [], [], true) :- !.
get_checkc(compat, M, Props, PropValues, CheckC) :-
	compound_check_props(compat, M, Props, CheckProps0),
	compound_checkc(CheckProps0, PropValues, CheckC).
get_checkc(call, M, Props, PropValues, CheckC) :-
	compound_check_props(instance, M, Props, CheckProps0),
	compound_checkc(CheckProps0, PropValues, CheckC).


compound_check_props(Check, M, Props, CheckProps) :-
	maplist(compound_check_prop(Check, M), Props, CheckProps).

compound_check_prop(_, M, compat(Prop), CheckProp) :- !,
	compound_check_prop(compat, M, Prop, CheckProp).
compound_check_prop(_, M, instance(Prop), CheckProp) :- !,
	compound_check_prop(instance, M, Prop, CheckProp).
compound_check_prop(_, _, succeeds(Prop), Prop) :- !.
compound_check_prop(Check, _, M:Prop, CheckProp) :- !,
	compound_check_prop(Check, M, Prop, CheckProp).
compound_check_prop(Check, M, Prop, CheckProp) :-
	CheckProp =.. [Check, M:Prop].

compound_checkif(IfValues, ErrType, PredName, CheckProps, ALoc, PropValue,
		 ( IfValues = []
		 ->findall(PropValue, CheckProps, Props),
		   send_rtcheck(Props, ErrType, PredName, ALoc)
		 ; true
		 )).

% TODO: fail: Exit \= [].  true: Exit == [].

get_checkif(_, _, _, _, [], _, _, true) :- !.
get_checkif(_, Exit, _, _, _, _, _, true) :- Exit \= [], !.
get_checkif(success, Exit, PredName, M, Props, Names, ALoc, CheckIf) :-
	compound_check_props(instance, M, Props, CheckProps0),
	maplist(check_props_names(NameProp), CheckProps0, Names, CNs),
	list_to_disj(CNs, CheckProps),
	compound_checkif(Exit, success, PredName, CheckProps, ALoc, NameProp, CheckIf).
get_checkif(compatpos, Exit, PredName, M, Props, Names, ALoc, CheckIf) :-
	compound_check_props(compat, M, Props, CheckProps0),
	maplist(check_props_names(NameProp), CheckProps0, Names, CNs),
	list_to_disj(CNs, CheckProps),
	compound_checkif(Exit, success, PredName, CheckProps, ALoc, NameProp, CheckIf).

short_prop_name(Prop, Name-[]) :-
	callable(Prop),
	compound(Prop),
	arg(1, Prop, Arg),
	var(Arg),
	Prop =.. [FName, _|Args],
	gnd(Args) ->
	Name =.. [FName|Args]
    ;
	Name = Prop.

short_prop_names(Props, Names) :-
	maplist(short_prop_name, Props, Names).

select_applicable(Dict, Prop, PropDict) :-
    exclude(not_aplicable(Prop), Dict, PropDict).

not_aplicable(Prop, _=Var) :-
    not_occurrences(Prop, Var).

not_occurrences(Term, Var) :-
    occurrences_of_var(Var, Term, 0).

long_prop_names(Props, PropNames, Dict, Names) :-
    maplist(select_applicable(Dict), Props, PropDicts),
    pairs_keys(  Names, PropNames),
    pairs_values(Names, PropDicts).

% in this predicate, PredName and the name of each property must be ground
% to avoid undesired unifications.
get_pretty_names(short, n(Pred, Compat, Call, Succ, Comp), _, TermName) :-
	functor(Pred, F, A),
	short_prop_names(Compat, CompatName),
	short_prop_names(Call,   CallName),
	short_prop_names(Succ,   SuccName),
	short_prop_names(Comp,   CompName),
	TermName = n(F/A, CompatName, CallName, SuccName, CompName).
get_pretty_names(long, Term, Dict0, TermName) :-
    Term = n(_Pred, Compat, Call, Succ, Comp),
    term_variables(Term, Vars),
    include(not_occurrences(Dict0), Vars, NVars),
    copy_term(t(Term, NVars, Dict0), t(TermName0, Names, Dict1)),
    maplist(varnamep, NVars, Names, CDict),
    numbervars(Names, 0, _),
    append(Dict0, CDict, Dict),
    maplist(apply_varname, Dict1),
    TermName0 = n(PredName, CompatName0, CallName0, SuccName0, CompName0),
    long_prop_names(Compat, CompatName0, Dict, CompatName),
    long_prop_names(Call,   CallName0,   Dict, CallName),
    long_prop_names(Succ,   SuccName0,   Dict, SuccName),
    long_prop_names(Comp,   CompName0,   Dict, CompName),
    TermName = n(PredName, CompatName, CallName, SuccName, CompName).

apply_varname(Name='$VAR'(Name)).

varnamep(Var, Name, Name=Var).

% note that the following predicates do partial unification, and comparison
% over the given term: cui(Compare, Unify, Ignore)

diff_props(L,      [], L) :- !. % Minor optimization
diff_props(L1, [H|L2], L3) :- diff_props_2(L1, [H|L2], L3).

diff_props_2([],     _,  []).
diff_props_2([H|L1], L2, L3) :-
	is_member_prop(L2, H),
	!,
	diff_props(L1, L2, L3).
diff_props_2([H|L1], L2, [H|L3]) :-
	diff_props_2(L1, L2, L3).

is_member_prop([T0|_], T1) :-
	is_same_prop(T0, T1),
	!.
is_member_prop([_|L], X) :- is_member_prop(L, X).

is_same_prop(cui(C0, U0, _), cui(C1, U1, _)) :-
	C0 == C1,
	% The unification should be done After the comparison, to avoid
	% problems if [U0,U1] share variables with [C0,C1]:
	U0 = U1.

collapse_prop(T0, T1, Es, Es) :-
	is_same_prop(T0, T1),
	!.
collapse_prop(_, cui(_, _, I), [I|Es], Es).

remove_element(A,     _, A) :- var(A), !.
remove_element([],    _, []).
remove_element([X|Y], E, Z0) :-
	(
	    X == E ->
	    Z0 = Z
	;
	    Z0 = [X|Z]
	),
	remove_element(Y, E, Z).

lists_to_lits(A0, L) :-
	flatten(A0, A1),
	remove_element(A1, true, A2),
	list_to_lits(A2, L).

list_to_lits([],     true).
list_to_lits([X|Xs], Lits) :-
	list_to_lits2(Xs, X, Lits).

list_to_lits2([],     X,  X).
list_to_lits2([X|Xs], X0, (X0, Lits)) :-
	list_to_lits2(Xs, X, Lits).

list_to_disj([],     fail).
list_to_disj([X|Xs], Lits) :-
	list_to_disj2(Xs, X, Lits).

list_to_disj2([],     X,  X).
list_to_disj2([X|Xs], X0, (X0 ; Lits)) :-
	list_to_disj2(Xs, X, Lits).

checkif_to_lit(pos(M, PType),
	       infl(ALoc, PredName, Compat, CompatNames, Exit),
	       CheckPos) :-
    get_checkif(PType, Exit, PredName, M, Compat, CompatNames, ALoc, CheckPos).
