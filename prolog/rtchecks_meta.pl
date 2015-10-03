:- module(rtchecks_meta, [body_check_pos/9,
			  body_check_pre/9,
			  collapse_redundants/3,
			  compound_rtchecks/6,
			  compound_rtchecks_end/5
			 ]).

:- use_module(library(assertions)).
:- use_module(library(lists)).
:- use_module(rtchecks(rtchecks_basic)).

:- doc(author, "Edison Mera").

:- doc(module, "Meta predicates used in rtcheck expansion.").

:- meta_predicate collapse_redundants2(?, ?, 4, ?).
collapse_redundants2([], T, Goal, Es0) :-
	call(Goal, _, T, Es0, []).
collapse_redundants2([T0|Ts], T, Goal, Es0) :-
	call(Goal, T0, T, Es0, Es),
	collapse_redundants2(Ts, T0, Goal, Es).

:- meta_predicate collapse_redundants(?, 4, ?).
collapse_redundants([],     _,    []).
collapse_redundants([T|Ts], Goal, Es) :-
	collapse_redundants2(Ts, T, Goal, Es).

:- meta_predicate compound_rtchecks2(2, 4, ?, ?, ?, ?).
compound_rtchecks2(CheckToProps, Collapser, CheckProps, CheckedPropsCUI0,
	    PropsCUI, PropsL) :-
	maplist(CheckToProps, CheckProps, PropsCUI0),
	sort(PropsCUI0, PropsCUI1),
	diff_props(PropsCUI1, CheckedPropsCUI0, PropsCUI),
	collapse_redundants(PropsCUI, Collapser, PropsL).

:- meta_predicate compound_rtchecks(2, 4, ?, ?, ?, ?).
compound_rtchecks(CheckToProps, Collapser, CheckProps, CheckedPropsCUI0,
	    CheckedPropsCUI, PropsL) :-
	compound_rtchecks2(CheckToProps, Collapser, CheckProps,
	    CheckedPropsCUI0, PropsCUI0, PropsL),
	sort(PropsCUI0, PropsCUI),
	append(PropsCUI, CheckedPropsCUI0, CheckedPropsCUI).

:- meta_predicate compound_rtchecks_end(2, 4, ?, ?, ?).
compound_rtchecks_end(CheckToProps, Collapser, CheckProps, CheckedPropsL,
		      PropsL) :-
    compound_rtchecks2(CheckToProps, Collapser, CheckProps,
		       CheckedPropsL, _, PropsL).

:- meta_predicate body_check_pos(2, 2, 4, ?, ?, ?, ?, ?, ?).

body_check_pos(_, _, _, _, [], CheckedL, CheckedL, Body, Body) :- !.
body_check_pos(CheckToProps, CheckToPropsPos, Collapser, Params,
	       CheckPos, CheckedL0, CheckedL, Body0, Body) :-
    compound_rtchecks(CheckToProps, Collapser, CheckPos, CheckedL0,
		      CheckedL, Pre),
    compound_rtchecks_end(CheckToPropsPos, Collapser, CheckPos, [], PosL),
    maplist(checkif_to_lit(Params), PosL, Pos),
    Body0 = [Pre, Body, Pos].

:- meta_predicate body_check_pre(2, 2, 2, 4, ?, ?, ?, ?, ?).

body_check_pre(_, _, _, _, [], Checked, Checked, Body, Body) :- !.
body_check_pre(CheckToProps, CheckToFails, CheckToError, Collapser,
		ChkProp, CheckedL0, CheckedL, Body0, Body) :-
	compound_rtchecks(CheckToProps, Collapser, ChkProp, CheckedL0,
	    CheckedL, Prop),
	compound_rtchecks_end(CheckToFails, Collapser, ChkProp, [],
	    PropFails0),
	lists_to_lits(PropFails0, PropFails),
	( PropFails == ([]\=[])
	->Body0 = Body
	; compound_rtchecks_end(CheckToError, Collapser, ChkProp, [],
				PropError0 ),
	  lists_to_lits(PropError0, PropError),
	  Body0 = [Prop, (PropFails -> PropError ; true), Body]
	).
