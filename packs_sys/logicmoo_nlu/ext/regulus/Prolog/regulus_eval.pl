% regulus_eval.pl

% Post-processing of Regulus semantic forms

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_eval,
	  [regulus_eval_text/2,
	   regulus_eval_speech/2,

	   regulus_eval_text/3,
	   regulus_eval_speech/3,
	   
	   beta_reduce/2,

	   switch_off_lf_post_processing/0,
	   switch_on_lf_post_processing/0,
	   
	   sem_atom_in_lf/2,
	   sem_triple_in_lf/2,

	   close_list/1,
	   merge_globals/2,
	   slot_value/3]
      ).

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
      
:- use_module(library(lists)).
:- use_module(library(terms)).

%---------------------------------------------------------------

:- dynamic no_lf_post_processing/0.

switch_off_lf_post_processing :-
	retractall(no_lf_post_processing),
	assertz(no_lf_post_processing),
	!.
switch_on_lf_post_processing :-
	retractall(no_lf_post_processing),
	!.

%---------------------------------------------------------------

regulus_eval_text(In, Out) :-
	(   user:regulus_config(lf_postproc_pred, PostProcPred) ->
	    true
	;
	    PostProcPred = no_post_proc_pred
	),
	regulus_eval_text(In, Out, PostProcPred).

regulus_eval_speech(In, Out) :-
	(   user:regulus_config(lf_postproc_pred, PostProcPred) ->
	    true
	;
	    PostProcPred = no_post_proc_pred
	),
	regulus_eval_speech(In, Out, PostProcPred).

%---------------------------------------------------------------

%regulus_eval_text(In, Out, PostProcPred) :-
%	regulus_eval1(In, Next),
%	regulus_eval_speech(Next, Out, PostProcPred),
%	!.
regulus_eval_text(In, Out, _PostProcPred) :-
	no_lf_post_processing,
	In = Out,
	!.
regulus_eval_text(In, Out, PostProcPred) :-
	% Do this initial eval to mimic what happens in the Nuance parser
	regulus_eval1(In, Next1),
	(   term_contains_functor(Next1, apply/0) ->
	    beta_reduce(Next1, Next2) ;
	    Next1 = Next2
	),
	regulus_eval1(Next2, Next3),
	(   PostProcPred = no_post_proc_pred ->
	    Next3 = Out
	;
	    Call =.. [PostProcPred, Next3, Out],
	    call(Call) 
	),
	!.
regulus_eval_text(In, Out, PostProcPred) :-
	format('~NWarning: call failed: ~w~n', [regulus_eval_text(In, Out, PostProcPred)]),
	(   Out = In ->
	    true
	;
	    true
	).

regulus_eval_speech(In, Out, _PostProcPred) :-
	no_lf_post_processing,
	In = Out,
	!.
regulus_eval_speech(In, Out, PostProcPred) :-
	(   term_contains_functor(In, apply/0) ->
	    beta_reduce(In, Next) ;
	    In = Next
	),
	(   PostProcPred = no_post_proc_pred ->
	    Next = Out
	;
	    Call =.. [PostProcPred, Next, Out],
	    call(Call) 
	),
	!.
regulus_eval_speech(In, Out, PostProcPred) :-
	format('~NWarning: call failed: ~w~n', [regulus_eval_speech(In, Out, PostProcPred)]),
	(   Out = In ->
	    true ;
	    true
	).

%---------------------------------------------------------------

beta_reduce(X, X) :-
	var(X),
	!.
beta_reduce(Sem, Result) :-
	safe_subsumes_chk([apply, [lambda, Var, Body], Arg], Sem),
	Sem = [apply, [lambda, Var, Body], Arg],
	beta_reduce(Body, Body1),
	beta_reduce_substitute(Body1, Var, Arg, Result),
	!.
beta_reduce(X, X) :-
	atomic(X),
	!.
beta_reduce(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	beta_reduce_args(N, T, T1).

beta_reduce_args(0, _T, _T1).
beta_reduce_args(I, T, T1) :-
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	beta_reduce(Arg, Arg1),
	I1 is I - 1,
	!,
	beta_reduce_args(I1, T, T1).

beta_reduce_substitute(X, _Var, _Val, X) :-
	var(X),
	!.
beta_reduce_substitute(X, _Var, _Val, X) :-
	atomic(X),
	!.
beta_reduce_substitute([X | Rest], Var, Val, Result) :-
	atomic(X),
	X = Var,
	append(Val, Rest, Result),
	!.
beta_reduce_substitute(T, Var, Val, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	beta_reduce_substitute_args(N, T, Var, Val, T1).

beta_reduce_substitute_args(0, _T, _Var, _Val, _T1).
beta_reduce_substitute_args(I, T, Var, Val, T1) :-
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	beta_reduce_substitute(Arg, Var, Val, Arg1),
	I1 is I - 1,
	!,
	beta_reduce_substitute_args(I1, T, Var, Val, T1).

%---------------------------------------------------------------

regulus_eval1(X, X) :-
	var(X),
	!.
regulus_eval1(X, X) :-
	atomic(X),
	!.
regulus_eval1(slot_value(Val, SlotName), Result) :-
	regulus_eval1(Val, Val1),
	slot_value(Val1, SlotName, Result),
	!.
regulus_eval1(add(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	add(X1, Y1, Result),
	!.
regulus_eval1(sub(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	sub(X1, Y1, Result),
	!.
regulus_eval1(mul(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	mul(X1, Y1, Result),
	!.
regulus_eval1(div(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	div(X1, Y1, Result),
	!.
regulus_eval1(neg(X), Result) :-
	regulus_eval1(X, X1),
	neg(X1, Result),
	!.
regulus_eval1(strcat(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	strcat(X1, Y1, Result),
	!.
regulus_eval1(concat(X, Y), Result) :-
	regulus_eval1(X, X1),
	regulus_eval1(Y, Y1),
	concat(X1, Y1, Result),
	!.
regulus_eval1(T, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	regulus_eval1_args(N, T, T1).

regulus_eval1_args(0, _T, _T1).
regulus_eval1_args(I, T, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	regulus_eval1(Arg, Arg1),
	I1 is I - 1,
	!,
	regulus_eval1_args(I1, T, T1).

%---------------------------------------------------------------

close_list(Var) :-
	var(Var),
	!,
	Var = [].
close_list([_F | R]) :-
	close_list(R).

%---------------------------------------------------------------

slot_value(Var, _SlotName, _UninstantiatedValue) :-
	var(Var),
	!.
slot_value([], _SlotName, _UninstantiatedValue) :-
	!.
slot_value([SlotName=Val | _R], SlotName, Val) :-
	!.
slot_value([_F | R], SlotName, Val) :-
	slot_value(R, SlotName, Val).

/*
% This won't work if we use more than one global slot, but we should probably 
% discourage people from doing that...

merge_globals(A, A).
*/

merge_globals([], _Globals) :-
	!.
merge_globals([SlotName=Val | R], Globals) :-
	merge_global(SlotName=Val, Globals),
	!,
	merge_globals(R, Globals).

merge_global(SlotName=Val, [SlotName=Val1 | _R]) :-
	!,
	Val = Val1.
merge_global(SlotName=Val, [_F | R]) :-
	merge_global(SlotName=Val, R).

%---------------------------------------------------------------

%gsl_function(add/2).

add(Var, X, X) :- var_or_null_value(Var), !.
add(X, Var, X) :- var_or_null_value(Var), !.
add(X, Y, Z) :- Z is X + Y.

%gsl_function(sub/2).

sub(Var, X, X) :- var_or_null_value(Var), !.
sub(X, Var, X) :- var_or_null_value(Var), !.
sub(X, Y, Z) :- Z is X - Y.

%gsl_function(mul/2).

mul(Var, X, X) :- var_or_null_value(Var), !.
mul(X, Var, X) :- var_or_null_value(Var), !.
mul(X, Y, Z) :- Z is X * Y.

%gsl_function(div/2).

div(Var, X, X) :- var_or_null_value(Var), !.
div(X, Var, X) :- var_or_null_value(Var), !.
div(X, Y, Z) :- Z is X / Y.

%gsl_function(neg/1).

neg(Var, Var) :- var_or_null_value(Var), !.
neg(X, Y) :- Y is -1 * X.

%gsl_function(strcat/2).

strcat(Var, X, Result) :-
	var_or_null_value(Var),
	!,
	strcat('[]', X, Result).
strcat(X, Var, Result) :-
	var_or_null_value(Var),
	!,
	strcat(X, '[]', Result).
strcat(X, Y, Z) :- 
	name(X, XChars),
	name(Y, YChars),
	append(XChars, YChars, ZChars),
	name(Z, ZChars).

%gsl_function(insert_begin/2).
%gsl_function(insert_end/2).
%gsl_function(concat/2).

concat(Var, X, X) :- var_or_null_value(Var), !.
concat(X, Var, X) :- var_or_null_value(Var), !.
concat(X, Y, Z) :- append(X, Y, Z).

%gsl_function(first/1).
%gsl_function(last/1).
%gsl_function(rest/1).

var_or_null_value(Var) :-
	var(Var),
	!.
var_or_null_value('*null_value*').

%---------------------------------------------------------------

/*
Default role at top-level is 'null'.
*/
role_marked_nested_postproc_lf(In, Out) :-
	role_marked_nested_postproc_list(In, null, Out-[]),
	!.
role_marked_nested_postproc_lf(In, Out) :-
	format2error('~N*** Error: bad call: ~w~n', [role_marked_nested_postproc_lf(In, Out)]),
	fail.

role_marked_nested_postproc_list([], _Role, Out-Out).
role_marked_nested_postproc_list([F | R], Role, In-Out) :-
	role_marked_nested_postproc_element(F, Role, In-Next),
	!,
	role_marked_nested_postproc_list(R, Role, Next-Out).

/*
Element is of form [role, Role, Body].

Set the default role in Body to Role and process it to add all its elements to the current list.
*/
role_marked_nested_postproc_element([role, Role, Body], _RoleAbove, In-Out) :-
	role_marked_nested_postproc_list(Body, Role, In-Out),
	!.
/*
Element is of form [clause, Body].

Convert the list of elements in Body to Body1, using 'null' as the initial role.
Add an element of form Role-[clause, Body1] to the current list.
*/
role_marked_nested_postproc_element([clause, Body], Role, [Clause | Out]-Out) :-
	role_marked_nested_postproc_list(Body, null, Body1-[]),
	Clause = (Role=[clause, Body1]),
	!.
/*
Element is of form [conj, Conj | Args].

Convert the list of elements in each Arg using the current Role, and add an element
of the form [conj, Conj | Args1], where Arg1 is the converted version of Args.
*/
role_marked_nested_postproc_element([conj, Conj | Args], Role, [[conj, Conj | Args1] | Out]-Out) :-
	role_marked_nested_postproc_conj_body(Args, Role, Args1),
	!.
role_marked_nested_postproc_element(Element, Role, [Role=Element | Out]-Out).

role_marked_nested_postproc_conj_body([], _Role, []).
role_marked_nested_postproc_conj_body([F | R], Role, [F1 | R1]) :-
	role_marked_nested_postproc_list(F, Role, F1-[]),
	!,
	role_marked_nested_postproc_conj_body(R, Role, R1).

%---------------------------------------------------------------

riacs_postproc_lf(Var, Var) :-
	var(Var),
	!.
riacs_postproc_lf(Atom, Atom) :-
	atomic(Atom),
	!.
riacs_postproc_lf(Feat=Val, Feat=Val1) :-
	riacs_postproc_lf(Val, Val1),
	!.
riacs_postproc_lf(NPList, term(Spec, Head, Mods)) :-
	is_list(NPList),
	consume_several([[head, Head0], [spec, Spec0]], NPList, Mods0),
	riacs_postproc_lf_list([Spec0, Head0, Mods0], [Spec, Head, Mods]),
	!.
% In Japanese, at any rate, times can be modified
riacs_postproc_lf(NPList, time(H, M, Timesuffix, Qualifiers)) :-
	is_list(NPList),
	consume_several([[special_np, time], [hour, H], [minute, M], [day_part, Timesuffix]], NPList, Qualifiers0),
	Qualifiers0 \== [],
	riacs_postproc_lf_list(Qualifiers0, Qualifiers),
	!.
riacs_postproc_lf(NPList, time(H, M, Timesuffix)) :-
	is_list(NPList),
	consume_several([[special_np, time], [hour, H], [minute, M], [day_part, Timesuffix]], NPList, []),
	!.
% In Japanese, at any rate, times can be modified
riacs_postproc_lf(NPList, time(H, M, any, Qualifiers)) :-
	is_list(NPList),
	consume_several([[special_np, time], [hour, H], [minute, M]], NPList, Qualifiers0),
	Qualifiers \== [],
	riacs_postproc_lf_list(Qualifiers0, Qualifiers),
	!.
riacs_postproc_lf(NPList, time(H, M, any)) :-
	is_list(NPList),
	consume_several([[special_np, time], [hour, H], [minute, M]], NPList, []),
	!.
riacs_postproc_lf(NPList, date(Year, Month, Day)) :-
	is_list(NPList),
	consume_several([[special_np, date], [year, Year], [month, Month], [day, Day]], NPList, []),
	!.
riacs_postproc_lf(NPList, date(Year, Month, unspecified)) :-
	is_list(NPList),
	consume_several([[special_np, date], [year, Year], [month, Month]], NPList, []),
	!.
riacs_postproc_lf(NPList, date(unspecified, Month, Day)) :-
	is_list(NPList),
	consume_several([[special_np, date], [month, Month], [day, Day]], NPList, []),
	!.
riacs_postproc_lf(NPList, date(unspecified, Month, unspecified)) :-
	is_list(NPList),
	consume_several([[special_np, date], [month, Month]], NPList, []),
	!.
riacs_postproc_lf(NPList, date(Year, unspecified, unspecified)) :-
	is_list(NPList),
	consume_several([[special_np, date], [year, Year]], NPList, []),
	!.
riacs_postproc_lf(NPList, measure(A, U)) :-
	is_list(NPList),
	consume_several([[special_np, measure], [amount, A], [unit, U]], NPList, []),
	!.
riacs_postproc_lf(VPList, form(Tense, [VPMain | VPMods])) :-
	is_list(VPList),
	consume_several([[verb, Verb], [verb_type, VerbType]], VPList, VPList1),
	vp_list_to_vp_main_and_rest(VerbType, Verb, VPMain, VPList1, VPList2),
	vp_list_to_tense_and_rest(VPList2, Tense, VPList3),
	riacs_postproc_lf(VPList3, VPMods),
	!.
riacs_postproc_lf([np_and | Conjuncts], term(and, Conjuncts1, [])) :-
	riacs_postproc_lf_list(Conjuncts, Conjuncts1),
	!.
riacs_postproc_lf([s_and | Conjuncts], Conjuncts1) :-
	riacs_postproc_lf_list(Conjuncts, Conjuncts1),
	!.
riacs_postproc_lf(List, List1) :-
	is_list(List),
	riacs_postproc_lf_list(List, List1),
	!.
riacs_postproc_lf(Other, Other1) :-
	format2error('~NError: bad call: ~w~n', [riacs_postproc_lf(Other, Other1)]),
	fail.

riacs_postproc_lf_list([], []).
riacs_postproc_lf_list([F | R], [F1 | R1]) :-
	riacs_postproc_lf(F, F1),
	riacs_postproc_lf_list(R, R1),
	!.

%-----------------------------------------------------------------------------------

%vp_list_to_vp_main_and_rest(VerbType, Verb, VPMain, VPListIn, VPListOut)

%no_complements

vp_list_to_vp_main_and_rest(no_complements, Verb, [Verb], VPListIn, VPListIn) :-
	!.

%intrans

vp_list_to_vp_main_and_rest(intrans, Verb, [Verb, Subj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0], [Subj]),
	!.

% passivised transitive with deep roles

vp_list_to_vp_main_and_rest(trans_passivised, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0], [Subj, Obj]),
	!.

vp_list_to_vp_main_and_rest(trans_passivised, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[obj, Obj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Obj0], [Obj]),
	Subj = term(bare, passive_agent, []),
	!.

% passivised transitive with surface roles

vp_list_to_vp_main_and_rest(trans_passivised_surface_roles, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[subj, Obj0], [passive_compl, Subj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0], [Subj, Obj]),
	!.

vp_list_to_vp_main_and_rest(trans_passivised_surface_roles, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[subj, Obj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Obj0], [Obj]),
	Subj = term(bare, passive_agent, []),
	!.

%pp complement

vp_list_to_vp_main_and_rest(pp, Verb, [Verb, Subj, PObj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [subcat_pp, [PObj0]]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, PObj0], [Subj, PObj]),
	!.

% passivised np+pp

vp_list_to_vp_main_and_rest(np_pp_passivised, Verb, [Verb, Subj, Obj, PObj], VPListIn, VPListOut) :-
	consume_several([[obj, Obj0], [subcat_pp, [PObj0]]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Obj0, PObj0], [Obj, PObj]),
	Subj = term(bare, passive_agent, []),
	!.

%np+pp complements

vp_list_to_vp_main_and_rest(np_pp, Verb, [Verb, Subj, Obj, PObj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0], [subcat_pp, [PObj0]]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0, PObj0], [Subj, Obj, PObj]),
	!.

%trans

vp_list_to_vp_main_and_rest(trans, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0], [Subj, Obj]),
	!.

%ind obj only

vp_list_to_vp_main_and_rest(ind_obj, Verb, [Verb, Subj, IndObj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [indobj, IndObj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, IndObj0], [Subj, IndObj]),
	!.

% passivised ditransitive

vp_list_to_vp_main_and_rest(ditrans_passivised, Verb, [Verb, Subj, Obj, IndObj], VPListIn, VPListOut) :-
	consume_several([[obj, Obj0], [indobj, IndObj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Obj0, IndObj0], [Obj, IndObj]),
	Subj = term(bare, passive_agent, []),
	!.

%de-PP (Romance languages)

vp_list_to_vp_main_and_rest(de_pp, Verb, [Verb, Subj, Obj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [de, Obj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0], [Subj, Obj]),
	!.

%ditrans

vp_list_to_vp_main_and_rest(ditrans, Verb, [Verb, Subj, Obj, IndObj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0], [indobj, IndObj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0, IndObj0], [Subj, Obj, IndObj]),
	!.

%existential

vp_list_to_vp_main_and_rest(existential, Verb, [Verb, Subj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0], [Subj]),
	!.

%vp_modal


%vp_nonmodal
%to_vp

vp_list_to_vp_main_and_rest(VerbType, Verb, [Verb, Subj, [VPMain | VPMods]], VPListIn, VPListOut) :-
	( VerbType = vp_nonmodal ; VerbType = to_vp ),
	consume_several([[subj, Subj0], [prop_obj, VP0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0], [Subj]),
	consume_several([[verb, Verb1], [verb_type, VerbType1]], [[subj, Subj0] | VP0], VPRest),
	vp_list_to_vp_main_and_rest(VerbType1, Verb1, VPMain, VPRest, VPRest1),
	remove_aspect_info(VPRest1, VPRest2),
	riacs_postproc_lf_list(VPRest2, VPMods),
	!.

% passivised np + to_vp

vp_list_to_vp_main_and_rest(VerbType, Verb, [Verb, Subj, Obj, [VPMain | VPMods]], VPListIn, VPListOut) :-
	VerbType = np_to_vp_passivised,
	consume_several([[obj, Obj0], [prop_obj, VP0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Obj0], [Obj]),
	consume_several([[verb, Verb1], [verb_type, VerbType1]], [[subj, Obj0] | VP0], VPRest),
	vp_list_to_vp_main_and_rest(VerbType1, Verb1, VPMain, VPRest, VPRest1),
	remove_aspect_info(VPRest1, VPRest2),
	riacs_postproc_lf_list(VPRest2, VPMods),
	Subj = term(bare, passive_agent, []),
	!.

%np_to_vp

vp_list_to_vp_main_and_rest(VerbType, Verb, [Verb, Subj, Obj, [VPMain | VPMods]], VPListIn, VPListOut) :-
	VerbType = np_to_vp,
	consume_several([[subj, Subj0], [obj, Obj0], [prop_obj, VP0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0], [Subj, Obj]),
	consume_several([[verb, Verb1], [verb_type, VerbType1]], [[subj, Obj0] | VP0], VPRest),
	vp_list_to_vp_main_and_rest(VerbType1, Verb1, VPMain, VPRest, VPRest1),
	remove_aspect_info(VPRest1, VPRest2),
	riacs_postproc_lf_list(VPRest2, VPMods),
	!.

%s_prop

vp_list_to_vp_main_and_rest(s_prop, Verb, [Verb, Subj, Prop], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [dcl, Prop0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Prop0], [Subj, Prop]),
	!.

%s_embedded_q

vp_list_to_vp_main_and_rest(s_embedded_q, Verb, [Verb, Subj, Prop], VPListIn, VPListOut) :-
	(   consume_several([[subj, Subj0], [whq, Prop0]], VPListIn, VPListOut) ;
	    consume_several([[subj, Subj0], [ynq, Prop0]], VPListIn, VPListOut)
	),
	riacs_postproc_lf_list([Subj0, Prop0], [Subj, Prop]),
	!.

%s_np_embedded_q

vp_list_to_vp_main_and_rest(s_np_embedded_q, Verb, [Verb, Subj, Obj, Prop], VPListIn, VPListOut) :-
	(   consume_several([[subj, Subj0], [obj, Obj0], [whq, Prop0]], VPListIn, VPListOut) ;
	    consume_several([[subj, Subj0], [obj, Obj0], [ynq, Prop0]], VPListIn, VPListOut)
	),
	riacs_postproc_lf_list([Subj0, Obj0, Prop0], [Subj, Obj, Prop]),
	!.

%adj

vp_list_to_vp_main_and_rest(adj, Verb, [Verb, Subj, Adj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [adj, Adj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Adj0], [Subj, Adj]),
	!.

%np_adj

vp_list_to_vp_main_and_rest(np_adj, Verb, [Verb, Subj, Obj, Adj], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0], [adj, Adj0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0, Adj0], [Subj, Obj, Adj]),
	!.

%np_p

vp_list_to_vp_main_and_rest(np_p, Verb, [Verb, Subj, Obj, Prep], VPListIn, VPListOut) :-
	consume_several([[subj, Subj0], [obj, Obj0], [prep, Prep0]], VPListIn, VPListOut),
	riacs_postproc_lf_list([Subj0, Obj0, Prep0], [Subj, Obj, Prep]),
	!.

% error

vp_list_to_vp_main_and_rest(VerbType, Verb, VPMain, VPListIn, VPListOut) :-
	format2error('~NError: bad call: ~w~n', [vp_list_to_vp_main_and_rest(VerbType, Verb, VPMain, VPListIn, VPListOut)]),
	fail.	

%-----------------------------------------------------------------------------------

vp_list_to_tense_and_rest(VPListIn, TenseAspect, VPListOut) :-
	consume_several([[tense, Tense], [aspect, Aspect]], VPListIn, VPListOut),
	combine_tense_and_aspect(Tense, Aspect, TenseAspect),
	!.
% "Going to" is represented like this
vp_list_to_tense_and_rest(VPListIn, future, VPListOut) :-
	consume_several([[tense, future], [tense, present]], VPListIn, VPListOut),
	!.
vp_list_to_tense_and_rest(VPListIn, Tense, VPListOut) :-
	consume_several([[tense, Tense]], VPListIn, VPListOut),
	!.
vp_list_to_tense_and_rest(VPListIn, Modal, VPListOut) :-
	consume_several([[modal, Modal]], VPListIn, VPListOut),
	!.
vp_list_to_tense_and_rest(VPListIn, Tense, VPListOut) :-
	format2error('~NError: bad call: ~w~n', [vp_list_to_tense_and_rest(VPListIn, Tense, VPListOut)]),
	fail,
	!.

combine_tense_and_aspect(Tense, Aspect, [Tense, Aspect]).

%-----------------------------------------------------------------------------------

remove_aspect_info([], []) :-
	!.
remove_aspect_info([[aspect, _Aspect] | R], Out) :-
	remove_aspect_info(R, Out),
	!.
remove_aspect_info([F | R], [F | R1]) :-
	remove_aspect_info(R, R1),
	!.
remove_aspect_info(X, Y) :-
	format2error('~NError: bad call: ~w~n', [remove_aspect_info(X, Y)]),
	fail,
	!.

%-----------------------------------------------------------------------------------

consume_several([], L, L).
consume_several([F | R], In, Out) :-
	member(F, In),
	delete(In, F, Next),
	!,
	consume_several(R, Next, Out).

%---------------------------------------------------------------

sem_atom_in_lf(Atom, Atom) :-
	atomic(Atom),
	!.
sem_atom_in_lf([interjection, Interjection], Interjection) :-
	!.
sem_atom_in_lf([Operator, Body], Atom) :-
	lf_top_level_operator(Operator),
	!,
	(   Atom = Operator ;
	    sem_atom_in_lf(Body, Atom)
	).
sem_atom_in_lf([F | R], Atom) :-
	!,
	(   sem_atom_in_lf(F, Atom) ;
	    sem_atom_in_lf(R, Atom)
	).
sem_atom_in_lf(form(TenseAndAspect, Body), Atom) :-
	!,
	(   sem_atom_in_lf(TenseAndAspect, Atom) ;
	    sem_atom_in_lf(Body, Atom)
	).
sem_atom_in_lf(term(Spec, Head, Mods), Atom) :-
	!,
	(   sem_atom_in_lf(Spec, Atom) ;
	    sem_atom_in_lf(Head, Atom) ;
	    sem_atom_in_lf(Mods, Atom)
	).
sem_atom_in_lf(measure(Amount, Unit), Atom) :-
	!,
	(   Atom = measure(Amount, Unit) ;
	    sem_atom_in_lf(Amount, Atom) ;
	    sem_atom_in_lf(Unit, Atom)
	).
sem_atom_in_lf(time(H, M), Atom) :-
	!,
	Atom = time(H, M).
sem_atom_in_lf(date(M, D), Atom) :-
	!,
	Atom = date(M, D).

%---------------------------------------------------------------

sem_triple_in_lf([interjection, Interjection], Triple) :-
	!,
	Triple = [utterance, interjection, Interjection].
sem_triple_in_lf([elliptical, Body], Triple) :-
	!,
	(   sem_head(Body, Head),
	    Triple = [utterance, elliptical, Head] ;
	    sem_triple_in_lf(Body, Triple)
	).
sem_triple_in_lf([Operator, Body], Triple) :-
	lf_top_level_operator(Operator),
	!,
	sem_triple_in_lf(Body, Triple).
sem_triple_in_lf([F | R], Triple) :-
	!,
	(   sem_triple_in_lf(F, Triple) ;
	    sem_triple_in_lf(R, Triple)
	).
sem_triple_in_lf(form(TenseAndAspect, [Matrix | Mods]), Triple) :-
	!,
	sem_head(form(TenseAndAspect, [Matrix | Mods]), Head),
	(   sem_triple_in_form_matrix(Matrix, Head, Triple) ;
	    sem_triple_in_form_mods(Mods, Head, Triple)
	).
sem_triple_in_lf(term(Spec, Head, Mods), Triple) :-
	!,
	(   Triple = [Head, spec, Spec] ;
	    sem_triple_in_term_mods(Mods, Head, Triple)
	).
sem_triple_in_lf(measure(Amount, Unit), Triple) :-
	!,
	Triple = [Amount, measure, Unit].
/*
sem_triple_in_lf(time(H, M), Triple) :-
	!,
	(   Triple = [time, hours, H] ;
	    Triple = [time, minutes, M]
	).
sem_triple_in_lf(date(M, D), Triple) :-
	!,
	(   Triple = [date, month, M] ;
	    Triple = [date, day, D]
	).
*/

%---------------------------------------------------------------

sem_triple_in_form_matrix(Matrix, Head, Triple) :-
	length(Matrix, N),
	sem_triple_in_form_matrix1(Matrix, Head, Triple, N).
sem_triple_in_form_matrix([_Rel, RestMatrix], _Head, Triple) :-
	adjacent_members(Complement1, Complement2, RestMatrix),
	sem_head_from_matrix_complement(Complement1, Head1),
	sem_head_from_matrix_complement(Complement2, Head2),
	Triple = [Head1, adjacent_complement, Head2].

sem_triple_in_form_matrix1(Matrix, Head, Triple, I) :-
	I >= 2,
	safe_nth(I, Matrix, Arg),
	sem_head(Arg, ArgHead),	
	(   join_with_underscore([arg, I], ArgI),
	    Triple = [Head, ArgI, ArgHead] ;
	    sem_triple_in_lf(Arg, Triple)
	).
sem_triple_in_form_matrix1(Matrix, Head, Triple, I) :-
	I >= 3,
	I1 is I - 1,
	sem_triple_in_form_matrix1(Matrix, Head, Triple, I1).

sem_head_from_matrix_complement([_Rel, Body], Head) :-
	sem_head(Body, Head),
	!.
sem_head_from_matrix_complement(Other, Head) :-
	sem_head(Other, Head).

%---------------------------------------------------------------

sem_triple_in_form_mods(Mods, Head, Triple) :-
	member(Mod, Mods),
	sem_triple_in_form_mod(Mod, Head, Triple).

sem_triple_in_form_mod([Rel, ModBody], Head, Triple) :-
	sem_head(ModBody, ModHead),
	(   Triple = [Head, Rel, ModHead] ;
	    sem_triple_in_lf(ModBody, Triple)
	).

%---------------------------------------------------------------

sem_triple_in_term_mods(Mods, Head, Triple) :-
	member(Mod, Mods),
	sem_triple_in_term_mod(Mod, Head, Triple).
sem_triple_in_term_mods(Mods, _Head, Triple) :-
	adjacent_members(Mod1, Mod2, Mods),
	Mod1 = [_Rel1, ModBody1],
	Mod2 = [_Rel2, ModBody2],
	sem_head(ModBody1, Head1),
	sem_head(ModBody2, Head2),
	Triple = [Head1, adjacent_mod, Head2].

sem_triple_in_term_mod([Rel, ModBody], Head, Triple) :-
	sem_head(ModBody, ModHead),
	(   Triple = [Head, Rel, ModHead] ;
	    sem_triple_in_lf(ModBody, Triple)
	).

%---------------------------------------------------------------

sem_head(form(_TenseAndAspect, [[Head | _RestMatrix] | _Mods]), Head).
sem_head(term(_Spec, Head, _Mods), Head).
sem_head([_Prep, Body], Head) :-
	sem_head(Body, Head).
sem_head(measure(Amount, Unit), measure(Amount, Unit)).
sem_head(time(H, M), time(H, M)).
sem_head(date(D, M), date(D, M)).
sem_head(Atom, Atom) :-
	atomic(Atom).

lf_top_level_operator(imp).
lf_top_level_operator(dcl).
lf_top_level_operator(ynq).
lf_top_level_operator(whq).
lf_top_level_operator(elliptical).

%---------------------------------------------------------------

adjacent_members(X, Y, [X, Y | _R]).
adjacent_members(X, Y, [_F | R]) :-
	adjacent_members(X, Y, R).
