:- module(rtchecks_gen, [valid_commands/1,
			 collect_assertions/4,
			 current_assertion/4,
			 generate_rtchecks/6,
			 generate_ctchecks/4,
			 proc_ppassertion/3,
			 current_assertion/12]).

:- use_module(library(assertions)).
:- use_module(library(assrt_lib)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(library(rtchecks_basic)).
:- use_module(library(rtchecks_meta)).
:- use_module(library(qualify_meta_goal)).

/*

Algorithm:

pred :- body.

is transformed in:

pred :-                        \
	"check entry...",       \___________ STEP
	"check exit...",        /            ONE
	'pred$rtc1'.           /

'pred$rtc1' :-                            \
	"check compat pre..."              \
	"check calls...",                   \
	"check success pre",                 \__________ STEP
	"check comp..."(                     /           TWO
	'pred$rtc2',                        /
	"check success pos",               /
	"check compat pos..."             /

'pred$rtc2' :-
	body.

And goals preds are renamed to 'pred$rtc1'.  There are other steps in
order to simplify the generated code as far as possible.

*/

generate_common_rtchecks(Assertions, M, CompatAssrt,
			 CallAssrt, SuccAssrt, CompAssrt) -->
    compat_rtchecks(   Assertions, M, CompatAssrt, [], ChkCompatL0),
    calls_rtchecks(    Assertions, M, CallAssrt,   [], CheckedL0),
    success_rtchecks(  Assertions, M, SuccAssrt,   CheckedL0, CheckedL1),
    compatpos_rtchecks(Assertions, M, CompatAssrt, ChkCompatL0),
    comp_rtchecks(     Assertions, M, CompAssrt,   CheckedL1).

generate_step_rtchecks(Step, Assertions, M, Goal0, Goal) :-
    step_rtchecks_options(Step, CompatAssrt, CallAssrt, SuccAssrt, CompAssrt),
    generate_common_rtchecks(Assertions, M, CompatAssrt,
			     CallAssrt, SuccAssrt, CompAssrt, Goal0, Goal).

step_rtchecks_options(step1, CompatAssrt, CallAssrt, SuccAssrt, CompAssrt) :-
    current_prolog_flag(rtchecks_level, Level0 ),
    neg_level(Level0, Level),
    compat_assrt(Level, [test], CompatAssrt, []),
    call_assrt(Level, [entry], CallAssrt, []),
    succ_assrt(Level, [exit, success, pred], SuccAssrt, []),
    comp_assrt(Level, [comp, pred], CompAssrt, []).
step_rtchecks_options(step2, CompatAssrt, CallAssrt, SuccAssrt, CompAssrt) :-
    current_prolog_flag(rtchecks_level, Level),
    compat_assrt(Level, [], CompatAssrt, []),
    call_assrt(Level, [], CallAssrt, []),
    succ_assrt(Level, [test, success, pred], SuccAssrt, []),
    comp_assrt(Level, [test, comp, pred], CompAssrt, []).

% ----------------------------------------------------------------------------
neg_level(inner,   exports).
neg_level(exports, inner).

compat_assrt(exports, _) --> [].
compat_assrt(inner,  AL) -->
	findall((A,pred), (valid_assertions(A,pred), \+ memberchk(A, AL))).

call_assrt(Level, BL0) -->
	{Level = exports -> BL = BL0 ; BL= [calls, pred|BL0]},
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).

succ_assrt(Level, BL0) -->
	{Level = exports -> subtract(BL0, [test, success, pred], BL);BL = BL0},
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).

comp_assrt(exports, _) --> [].
comp_assrt(inner, BL) -->
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).

% ----------------------------------------------------------------------------

/*
  Combination of status and rtcheck indicators, to control the compile
  and run-time checking:

  ===========+============++===========+==========
  Status     | + Command  || ctchecked | rtchecked
  ===========+============++===========+==========
  true/trust | -          || no        | no
  true/trust | rtcheck    || no        | yes
  check      | no_rtcheck || yes       | no
  check      | -          || yes       | yes
  ===========+============++===========+==========
  Note: Is weird to preserve ciao-compatibility
*/

valid_ctcheck_assertions(Status, Type) :-
    ctcheck_assr_status(Status),
    ctcheck_assr_type(Type).

ctcheck_assr_status(trust).
ctcheck_assr_status(check).

ctcheck_assr_type(calls).
ctcheck_assr_type(entry).
ctcheck_assr_type(pred).
ctcheck_assr_type(prop).
ctcheck_assr_type(exit).
ctcheck_assr_type(success).

valid_assertions(Status, Type) :-
	rtcheck_assr_type(Type),
	( Type = (entry)
	->Status = true
	; rtcheck_assr_status(Status)
	).

rtcheck_assr_status(true)  :- current_prolog_flag(rtchecks_true,  yes).
rtcheck_assr_status(trust) :- current_prolog_flag(rtchecks_trust, yes).
rtcheck_assr_status(debug) :- current_prolog_flag(rtchecks_debug, yes).
rtcheck_assr_status(trace) :- current_prolog_flag(rtchecks_trace, yes).
rtcheck_assr_status(check) :- current_prolog_flag(rtchecks_check, yes).

rtcheck_assr_type(calls).
rtcheck_assr_type(entry) :- current_prolog_flag(rtchecks_entry, yes).
rtcheck_assr_type(pred).
rtcheck_assr_type(prop).
rtcheck_assr_type(test) :- current_prolog_flag(rtchecks_test, yes).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit) :- current_prolog_flag(rtchecks_exit, yes).
rtcheck_assr_type(success).

black_list_pred(_=_).

assertion_is_valid(ctcheck, Status, Type, _) :-
    valid_ctcheck_assertions(Status, Type).
assertion_is_valid(rtcheck, Status, Type, Comp) :-
	( \+ memberchk(rtcheck, Comp) ->
	  valid_assertions(Status, Type),
	  \+ memberchk(no_rtcheck, Comp)
	; true % Force run-time checking
	).

current_assertion(Pred, M, CM, TimeCheck, Status, Type, Comp, Call, Succ, Glob,
		  Dict, Loc) :-
    assertion_db(Pred, M, CM, Status, Type, Comp, Call, Succ, Glob, _, Dict, Loc),
    assertion_is_valid(TimeCheck, Status, Type, Glob),
    ( current_prolog_flag(rtchecks_level, inner)
    ->true
    ; current_prolog_flag(rtchecks_level, exports),
      predicate_property(M:Pred, export)
    ->true
    ),
    \+ black_list_pred(Pred).

current_assertion(Pred, M, TimeCheck,
		  assr(Pred, Status, Type, Comp, Call, Succ, Glob, Loc, PredName, CompName, CallName, SuccName, GlobName)) :-
    current_assertion(Pred, M, CM, TimeCheck, Status, Type, Comp0, Call0,
		      Succ0, Glob0, Dict0, Loc),
    collapse_dups(Glob0, Glob1),
    maplist(add_arg(Pred), Glob1, Glob2),
    ( M \= CM
    ->maplist(maplist(qualify_with(CM)),
	      [Comp0, Call0, Succ0, Glob2],
	      [Comp,  Call,  Succ,  Glob ])
    ; [Comp0, Call0, Succ0, Glob2] = [Comp,  Call,  Succ,  Glob ]
    ),
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    get_pretty_names(NameFmt, n(Pred, Comp, Call, Succ, Glob), Dict0, TermName),
    TermName = n(PredName, CompName, CallName, SuccName, GlobName).

assertion_pred(Pred, assr(Pred, _, _, _, _, _, _, _, _, _, _, _, _)).

collect_assertions(Pred, M, TimeCheck, Assertions) :-
    findall(Assertion, current_assertion(Pred, M, TimeCheck, Assertion), Assertions),
    maplist(assertion_pred(Pred), Assertions).

do_prop_rtcheck(compat, M,
		assr(_, _, _, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _),
		pre(ChkCompat, Compat,
		    send_rtcheck(PropValues, compat, PName, ALoc),
		    PropValues)) :-
    get_checkc(compat, M, Compat, CompatNames, PropValues, ChkCompat).
do_prop_rtcheck(calls, M,
		assr(_, _, _, _, Call, _, _, ALoc, PName, _, CallNames, _, _),
		pre(ChkCall, Call,
		    send_rtcheck(PropValues, calls, PName, ALoc),
		    PropValues)) :-
    get_checkc(call, M, Call, CallNames, PropValues, ChkCall).
do_prop_rtcheck(success, M,
		assr(_, _, _, _, Call, Succ, _, ALoc, PName, _, _, SuccNames, _),
		succ(ChkCall, Call, PropValues, ChkSucc, Succ)) :-
    get_checkc(call, M, Call, PropValues, ChkCall),
    check_poscond(ALoc, PName, Succ, SuccNames, PropValues, ChkSucc).
do_prop_rtcheck(compatpos, M,
		assr(_, _, _, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _),
		compatpos(ChkCompat, ChkCompatPos, Compat, PropValues)) :-
    get_checkc(compat, M, Compat, PropValues, ChkCompat),
    check_poscond(ALoc, PName, Compat, CompatNames, PropValues, ChkCompatPos).
do_prop_rtcheck(comp, M,
		assr(_, _, _, _, Call, _, Comp, ALoc, PName, _, _, _, CompNames),
		comp(ChkCall, Call, PropValues, ChkComp, Comp)) :-
    get_checkc(call, M, Call, PropValues, ChkCall),
    check_poscond(ALoc, PName, Comp, CompNames, PropValues, ChkComp).

is_prop_rtcheck(StatusTypes, assr(_, Status, Type, _, _, _, _, _, _, _, _, _, _)) :-
    memberchk((Status, Type), StatusTypes).

compat_rtchecks(Assertions, M, StatusTypes, CheckedL0, CheckedL) -->
    prop_rtchecks(compat, body_check_pre(pre_lit, pre_fails, pre_error, collapse_prop),
		  Assertions, M, StatusTypes, CheckedL0, CheckedL).

calls_rtchecks(Assertions, M, StatusTypes, CheckedL0, CheckedL) -->
    prop_rtchecks(calls, body_check_pre(pre_lit, pre_fails, pre_error, collapse_prop),
		  Assertions, M, StatusTypes, CheckedL0, CheckedL).

success_rtchecks(Assertions, M, StatusTypes, CheckedL0, CheckedL) -->
    prop_rtchecks(success, body_check_pos(success_call_lit, success_succ_lit, collapse_prop,
					  pos(M, success)),
		  Assertions, M, StatusTypes, CheckedL0, CheckedL).
compatpos_rtchecks(Assertions, M, StatusTypes, CheckedL) -->
    prop_rtchecks(compatpos, body_check_pos(compatpos_compat_lit, compatpos_lit, collapse_prop,
					    pos(M, compatpos)),
		  Assertions, M, StatusTypes, CheckedL, _).

comp_rtchecks(Assertions0, M, StatusTypes, CheckedL) -->
    prop_rtchecks(comp, body_check_comp(M),
		  Assertions0, M, StatusTypes, CheckedL, []).

:- meta_predicate prop_rtchecks(+, 5, +, +, +, +, -, ?, ?).
prop_rtchecks(PType, BodyCheckProp, Assertions0, M, StatusTypes,
	      CheckedL0, CheckedL) -->
    { include(is_prop_rtcheck(StatusTypes), Assertions0, Assertions),
      maplist(do_prop_rtcheck(PType, M), Assertions, ChkCalls)
    },
    call(BodyCheckProp, ChkCalls, CheckedL0, CheckedL).

body_check_comp(_, [],       _,  _, Body,  Body) :- !.
body_check_comp(M, ChkComps, CheckedL0, CheckedL, Body0, Body) :-
	compound_rtchecks_end(comp_call_lit, collapse_prop, ChkComps, CheckedL0, CompCall),
	compound_rtchecks_end(comp_comp_lit, collapse_prop, ChkComps, CheckedL,  CompComp),
	maplist(comp_to_lit(M), CompComp, ChkComp0),
	sort(ChkComp0, ChkComp1),
	comps_to_goal(ChkComp1, compound_comp, CompBody, Body),
	Body0 = [CompCall, CompBody].

pre_lit(pre(ChkProp, Prop, _, PropValues), cui(Prop - [], PropValues, ChkProp)).

pre_fails(pre(_, _, _, PropValues), cui(PropValues, _, (PropValues \= []))).

pre_error(pre(_, _, Error, PropValues), cui(PropValues, _, Error)).

success_call_lit(succ(ChkCall, Call, PropValues, _, _),
		 cui(Call - [], PropValues, ChkCall)).

success_succ_lit(succ(_, _, PropValues, ChkSucc, Succ),
		 cui(Succ - PropValues, _, ChkSucc)).

check_poscond(ALoc, PredName, Prop, PropNames, PropValues, infl(ALoc, PredName, Prop, PropNames, PropValues)).

compatpos_compat_lit(compatpos(ChkCompat, _, Compat, PropValues),
	    cui(Compat - [], PropValues, ChkCompat)).

compatpos_lit(compatpos(_, ChkCompatPos, Compat, PropValues),
	    cui(Compat - PropValues, _, ChkCompatPos)).

:- pred collapse_dups(+list, ?list) # "Unifies duplicated terms.".

collapse_dups([],            []).
collapse_dups([Comp|Comps0], Comps) :-
	collapse_dups2(Comp, Comps0, Comps).

collapse_dups2(Comp0, Comps0, Comps) :-
	select(Comp, Comps0, Comps1),
	Comp==Comp0 ->
	collapse_dups2(Comp, Comps1, Comps)
    ;
	collapse_dups3(Comps0, Comp0, Comps).

collapse_dups3([],             Comp, [Comp]).
collapse_dups3([Comp0|Comps0], Comp, [Comp|Comps]) :-
	collapse_dups2(Comp0, Comps0, Comps).

comps_to_comp_lit(PropValues, Comp, M, Info, Body0, Body) :-
	comps_parts_to_comp_lit(PropValues, Comp, M, Info, Body1, Body),
	lists_to_lits(Body1, Body0).

valid_commands([times(_, _), try_sols(_, _)]).

comps_parts_to_comp_lit(PropValues, Comp0, M, Info, Body0, Body) :-
	valid_commands(VC),
	subtract(Comp0, VC, Comp),
	comps_to_goal(Comp, Body1, Body2),
	( Body1 == Body2
	->Body0 = Body
	; PropValues == [] ->
	  Body2 = Body,
	  Body0 = M:Body1
	; Body0 = checkif_comp(PropValues, Info, M:Body1, Body2, M:Body)
	).

comp_call_lit(comp(ChkCall, Call, PropValues, _, _),
	      cui(Call - [], PropValues, ChkCall)).

comp_comp_lit(comp(_, _, PropValues, ChkComp, Comp), cui(Comp - PropValues, _, ChkComp)).

compound_comp(Goal0-Goal, Goal0, Goal).

comp_to_lit(M, infl(ALoc, PredName, Comp, _CompNames, PropValues), ChkComp-Goal) :-
    comps_to_comp_lit(PropValues, Comp, M, info(PredName, ALoc), ChkComp, Goal).

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

proc_ppassertion(PPAssertion, CM, rtc_call(Type, CM:Goal)) :-
    ppassertion_type_goal(PPAssertion, Type, Goal).

generate_rtchecks(Assrs, M, G1, G2, G3, G) :-
	generate_step_rtchecks(step1, Assrs, M, G1, G2),
	generate_step_rtchecks(step2, Assrs, M, G3, G).

% Generate compile-time checks, currently only compatibility is checked,
% fails if no ctchecks can be applied to Pred
%
generate_ctchecks(Pred, M, CM, Lits) :-
    functor(Pred, F, A),
    functor(Head, F, A),
    collect_assertions(Head, M, ctcheck, Assertions0),
    maplist(abstract_assertions, Assertions0, Assertions),
				% Abstraction step, here we lose precision
				% but we gain computability of checks at
				% earlier, even compile-time. --EMM
    compat_rtchecks(Assertions, M, _, [], _, Goal0, []),
    qualify_meta_goal(Pred, M, CM, Head), % ???
    lists_to_lits(Goal0, Lits).

%% Trivial abstraction: Check for compatibility issues in properties,
%% compatibility is an abstraction that makes static check decidable.
%% The pay-off is a lose of precision. TBD: Formal demostration. --EMM
abstract_assertions(assr(Pred, Status, Type, Compat0, Call, Succ, _Comp, ALoc, PredName, CompatName0, CallName, SuccName, _CompName),
		    assr(Pred, Status, Type, Compat, [], [], [], ALoc, PredName, CompatName, [], [], [])) :-
    append([CompatName0, CallName, SuccName], CompatName),
    append([Compat0, Call, Succ], Compat).
