:- module(rtchecks_gen, [valid_commands/1,
			 collect_assertions/4,
			 current_assertion/4,
			 generate_rtchecks/8,
			 generate_ctchecks/4,
			 proc_ppassertion/5,
			 generate_step_rtchecks/7,
			 current_assertion/16],
	  [assertions, nortchecks, nativeprops, isomodes, dcg, hiord]).

:- use_module(library(lists)).
:- use_module(rtchecks(rtchecks_basic)).
:- use_module(rtchecks(rtchecks_meta)).
:- use_module(rtchecks(term_list)).
:- use_module(library(hiordlib), [map/3]).
:- use_module(library(assertions(assrt_lib)),
	      [assertion_read/9, assertion_body/7,
	       comps_to_goal/3, comps_to_goal/4]).

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
	call_stack('pred$rtc2', Loc)),      /
	"check success pos",               /
	"check compat pos..."             /

call_stack(Goal, Loc) :-
	intercept(Goal,
	    rtcheck(LocStack, ...),
	    send_signal(rtcheck([Loc|LockStack], ...))).

'pred$rtc2' :-
	body.

And goals preds are renamed to 'pred$rtc1'.  There are other steps in
order to simplify the generated code as far as possible.

*/

generate_common_rtchecks(Assertions, Pred, M, PLoc, PosLocs, CompatAssrt,
			 CallAssrt, SuccAssrt, CompAssrt) -->
    compat_rtchecks(Assertions, Pred, M, PLoc, PosLocs, CompatAssrt, [], ChkCompatL0),
    calls_rtchecks(Assertions, Pred, M, PLoc, PosLocs, CallAssrt, [], CheckedL0),
    success_rtchecks(Assertions, Pred, M, PLoc, PosLocs, SuccAssrt, CheckedL0, CheckedL1),
    compatpos_rtchecks(Assertions, Pred, M, PLoc, PosLocs, CompatAssrt, ChkCompatL0),
    comp_rtchecks(Assertions, Pred, M, PLoc, PosLocs, CompAssrt, CheckedL1).

generate_step_rtchecks(Step, Assertions, Pred, M, PLoc, Goal0, Goal) :- 
    step_rtchecks_options(Step, CompatAssrt, CallAssrt, SuccAssrt, CompAssrt),
    generate_common_rtchecks(Assertions, Pred, M, PLoc, PosLocs0, CompatAssrt,
			     CallAssrt, SuccAssrt, CompAssrt, Goal1, Goal),
    once(reverse(PosLocs0, PosLocs1)),
    collapse_terms(Goal1, PosLocs1, PosLocs2),
    reverse(PosLocs2, PosLocs),
    append(PosLocs, Goal1, Goal0).

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

assertion_is_valid(ctcheck, Status, Type, _) :-
    valid_ctcheck_assertions(Status, Type).
assertion_is_valid(rtcheck, Status, Type, Comp) :-
	( \+ memberchk(_:rtcheck(_), Comp) ->
	  valid_assertions(Status, Type),
	  \+ memberchk(_:no_rtcheck(_), Comp)
	; true % Force run-time checking
	).

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

% rtcheck_assr_status(true) :- current_prolog_flag(rtchecks_true, yes).
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

black_list_pred('=', 2).

current_assertion(Pred0, TimeCheck, Status, Type, Pred, Compat,
		  Call, Succ, Comp, Dict0, S, LB, LE, F, A, M) :-
	assertion_read(Pred0, M, Status, Type, ABody, Dict0, S, LB, LE),
	assertion_body(Pred, Compat, Call, Succ, Comp, _Comm, ABody),
	assertion_is_valid(TimeCheck, Status, Type, Comp),
	functor(Pred, F, A),
	( current_prolog_flag(rtchecks_level, inner) -> true
	; current_prolog_flag(rtchecks_level, exports),
	  predicate_property(M:Pred, export) -> true
	),
	\+ black_list_pred(F, A).

current_assertion(Pred0, M, TimeCheck,
		  assr(Pred, Status, Type, Compat, Call, Succ, Comp, Loc, PredName,
		       CompatName, CallName, SuccName, CompName, Dict)) :-
	current_assertion(Pred0, TimeCheck, Status, Type, Pred, Compat, Call,
			  Succ, Comp0, Dict0, S, LB, LE, _F, _A, M),
	Loc = loc(S, LB, LE),
	collapse_dups(Comp0, Comp),
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_pretty_names(NameFmt, n(Pred, Compat, Call, Succ, Comp), Dict0, TermName, Dict),
	TermName = n(PredName, CompatName, CallName, SuccName, CompName).

assertion_pred(assr(Pred, _, _, _, _, _, _, _, _, _, _, _, _, _), Pred).

collect_assertions(Pred, M, TimeCheck, Assertions) :-
	findall(Assertion, current_assertion(Pred, M, TimeCheck, Assertion), Assertions),
	list(Assertions, assertion_pred(Pred)).

pre_lit(pre(ChkProp, Prop, _, PropValues), cui(Prop - [], PropValues, ChkProp)).

pre_fails(pre(_, _, _, PropValues), cui(PropValues, _, (PropValues \= []))).

pre_error(pre(_, _, Error, PropValues), cui(PropValues, _, Error)).

compat_rtcheck(assr(Pred, Status, Type, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _, Dict),
	       Pred, M, PLoc, PosLocs, StatusTypes,
	       pre(ChkCompat, Compat,
		   rtchecks_send:send_rtcheck(PropValues, compat, PredName, Dict, PosLoc),
		   PropValues)) :-
	memberchk((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(compat, M, Compat, CompatNames, PropValues, ChkCompat).

compat_rtchecks(Assertions, Pred, M, PLoc, PosLocs, StatusTypes, CheckedL0, CheckedL) -->
	{collect_checks(Assertions, compat_rtcheck(Pred, M,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

calls_rtcheck(assr(Pred, Status, Type, _, Call, _, _, ALoc, PName, _, CallNames, _, _, Dict),
	      Pred, M, PLoc, PosLocs, StatusTypes,
	      pre(ChkCall, Call,
		  rtchecks_send:send_rtcheck(PropValues, calls, PredName, Dict, PosLoc),
		  PropValues)) :-
	memberchk((Status, Type), StatusTypes),
	\+(Call == []),
	insert_posloc(PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(call, M, Call, CallNames, PropValues, ChkCall).

calls_rtchecks(Assertions, Pred, M, PLoc, PosLocs, StatusTypes, CheckedL0, CheckedL) -->
	{collect_checks(Assertions, calls_rtcheck(Pred, M,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

success_call_lit(succ(ChkCall, Call, PropValues, _, _),
	    cui(Call - [], PropValues, ChkCall)).

success_succ_lit(succ(_, _, PropValues, ChkSucc, Succ),
	    cui(Succ - PropValues, _, ChkSucc)).

:- pred success_rtchecks/10 + not_fails.

success_rtchecks(Assertions, Pred, M, PLoc, PosLocs, StatusTypes, CheckedL0, CheckedL) -->
	{collect_checks(Assertions, success_rtcheck(Pred, M,
		    PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, success_call_lit, success_succ_lit,
	    collapse_prop, pos(Pred, M, success), CheckedL0,
	    CheckedL).

check_poscond(PosLoc, PredName, Dict, Prop, PropNames, PropValues,
	    i(PosLoc, PredName, Dict, Prop, PropNames, PropValues)).

success_rtcheck(assr(Pred, Status, Type, _, Call, Succ, _, ALoc, PName, _, _, SuccNames, _, Dict),
		Pred, M, PLoc, PosLocs, StatusTypes,
		succ(ChkCall, Call, PropValues, ChkSucc, Succ)) :-
	member((Status, Type), StatusTypes),
	\+(Succ == []),
	insert_posloc(PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(call, M, Call, PropValues, ChkCall),
	check_poscond(PosLoc, PredName, Dict, Succ, SuccNames, PropValues, ChkSucc).

compatpos_compat_lit(compatpos(ChkCompat, _, Compat, PropValues),
	    cui(Compat - [], PropValues, ChkCompat)).

compatpos_lit(compatpos(_, ChkCompatPos, Compat, PropValues),
	    cui(Compat - PropValues, _, ChkCompatPos)).

compatpos_rtchecks(Assertions, Pred, M, PLoc, PosLocs, StatusTypes,
	    CheckedL0) -->
	{collect_checks(Assertions, compatpos_rtcheck(Pred, M,
		PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, compatpos_compat_lit, compatpos_lit,
	    collapse_prop, pos(Pred, M, compatpos), CheckedL0, _).

compatpos_rtcheck(assr(Pred, Status, Type, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _, Dict),
		  Pred, M, PLoc, PosLocs, StatusTypes,
		  compatpos(ChkCompat, ChkCompatPos, Compat, PropValues)) :-
	member((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(compat, M, Compat, PropValues, ChkCompat),
	check_poscond(PosLoc, PredName, Dict, Compat, CompatNames, PropValues,
	    ChkCompatPos).

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

comps_to_comp_lit(PropValues, Comp, M, Body0, Body) :-
	comps_parts_to_comp_lit(PropValues, Comp, M, Body1, Body),
	lists_to_lits(Body1, Body0).

valid_commands([times(_, _), try_sols(_, _)]).

comps_parts_to_comp_lit(PropValues, Comp0, M, Body0, Body) :-
	valid_commands(VC),
	difference(Comp0, VC, Comp),
	comps_to_goal(Comp, Body1, Body2),
	( Body1 == Body2 ->
	  Body0 = Body
	; PropValues == [] ->
	  Body2 = Body,
	  Body0 = Body1
	; Body0 = @(rtchecks_rt:checkif_comp(PropValues, Body1, Body2, Body), M)
	).

:- use_module(library(implementation_module)).

comp_rtcheck(assr(Pred, Status, Type, _, Call, _, Comp, ALoc, PName, _, _, _, CompNames, Dict),
	     Pred, M, PLoc, PosLocs, StatusTypes,
	     comp(ChkCall, Call, PropValues, ChkComp, Comp)) :-
	member((Status, Type), StatusTypes),
	\+(Comp == []),
	get_checkc(call, M, Call, PropValues, ChkCall),
	insert_posloc(PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	check_poscond(PosLoc, PredName, Dict, Comp, CompNames, PropValues, ChkComp).

comp_call_lit(comp(ChkCall, Call, PropValues, _, _),
	      cui(Call - [], PropValues, ChkCall)).

comp_comp_lit(comp(_, _, PropValues, ChkComp, Comp), cui(Comp - PropValues, _, ChkComp)).

compound_comp(Goal0-Goal, Goal0, Goal).

body_check_comp([],       _,         _,    _, Body,  Body) :- !.
body_check_comp(ChkComps, CheckedL0, Pred, M, Body0, Body) :-
	compound_rtchecks_end(comp_call_lit, collapse_prop, ChkComps, CheckedL0, CompCall),
	compound_rtchecks_end(comp_comp_lit, collapse_prop, ChkComps, [],        CompComp),
	map(CompComp, comp_to_lit(M), ChkComp0),
	sort(ChkComp0, ChkComp1),
	comps_to_goal([(rtchecks_rt:with_goal(M:G, Pred))-G|ChkComp1],
		      compound_comp, CompBody, Body),
	Body0 = [CompCall, CompBody].

comp_rtchecks(Assertions, Pred, M, PLoc, PosLocs, StatusTypes, CheckedL) -->
	{ collect_checks(Assertions,
			 comp_rtcheck(Pred, M, PLoc, PosLocs, StatusTypes),
			 ChkComps)
	},
	body_check_comp(ChkComps, CheckedL, Pred, M).

comp_to_lit(i(PosLoc, PredName, Dict, Comp, _CompNames, PropValues), M, ChkComp-Goal) :-
	comps_to_comp_lit(PropValues, Comp, M, ChkComp, Goal).

proc_ppassertion(check(Goal), PredName, Dict, Loc,
		 rtcheck(Goal, PredName, Dict, Loc)).
proc_ppassertion(trust(Goal), PredName, Dict, Loc,
		 rttrust(Goal, PredName, Dict, Loc)).
proc_ppassertion(true(_),  _, _, _, true).
proc_ppassertion(false(_), _, _, _, true).

generate_rtchecks(Assrs, Pred, M, PLoc, G1, G2, G3, G) :-
	generate_step_rtchecks(step1, Assrs, Pred, M, PLoc, G1, G2),
	generate_step_rtchecks(step2, Assrs, Pred, M, PLoc, G3, G).

% Generate compile-time checks, currently only compatibility is checked,
% fails if no ctchecks can be applied to Pred
%
generate_ctchecks(Pred, M, Loc, Lits) :-
    collect_assertions(Pred, M, ctcheck, Assertions0),
    maplist(abstract_assertions, Assertions0, Assertions),
				% Abstraction step, here we lose precision
				% but we gain computability of checks at
				% earlier, even compile-time. --EMM
    compat_rtchecks(Assertions, Pred, M, Loc, PosLocs0, _,
		    [], _, Goal1, []),
    once(reverse(PosLocs0, PosLocs1)),
    collapse_terms(Goal1, PosLocs1, PosLocs2),
    reverse(PosLocs2, PosLocs),
    append(PosLocs, Goal1, Goal0),
    Goal0 \= [],
    lists_to_lits(Goal0, Lits).

%% Trivial abstraction: Check for compatibility issues in properties,
%% compatibility is an abstraction that makes static check decidable.
%% The pay-off is a lose of precision. TBD: Formal demostration. --EMM
abstract_assertions(assr(Pred, Status, Type, Compat0, Call, Succ, _Comp,
			 Loc, PredName, CompatName0, CallName, SuccName, _CompName, Dict),
		    assr(Pred, Status, Type, Compat, [], [], [],
			 Loc, PredName, CompatName, [], [], [], Dict)) :-
    append([CompatName0, CallName, SuccName], CompatName),
    append([Compat0, Call, Succ], Compat).
