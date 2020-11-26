% ebl_operational.pl

%---------------------------------------------------------------

:- module(ebl_operational,
	  [defined_operationality/1,
           operational_goal/4,
	   built_in_goal/2,
	   goal_fringe/2,

	   goal_for_cat/2,
	   nongap_goal_for_cat/2,
	   gap_goal_for_cat/2,
	   gap_goal/1,
	   possibly_gap_goal_for_cat/2,
	   goal_dominates_cat/2,
	   goal_dominates_cat_but_not_through/3,
	   goal_immediately_dominates_cat/2,
	   goal_dominates_lex/2,
	   goal_immediately_dominates_lex/2,
	   lexical_goal/1
          ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

defined_operationality(trivial_operationality).
defined_operationality(lexical_operationality).
defined_operationality(np_operationality).
defined_operationality(np_pp_operationality).
defined_operationality(utt_np_pp_operationality).
defined_operationality(utt_np_post_mods_operationality).
defined_operationality(utt_np_post_mods_optional_operationality).
defined_operationality(utt_np_simple_np_pp_operationality).
defined_operationality(np_post_mods_operationality).
defined_operationality(np_vbar_post_mods_operationality).
defined_operationality(simple_np_vbar_post_mods_operationality).
defined_operationality(simple_np_number_operation).
defined_operationality(simple_np_post_mods_number_operationality).
defined_operationality(s_simple_np_post_mods_number_operationality).
defined_operationality(np_simple_np_post_mods_number_operationality).
defined_operationality(simple_np_vbar_post_mods_number_operationality).
defined_operationality(simple_np_vbar_optional_post_mods_number_operationality).
defined_operationality(np_simple_np_vbar_post_mods_operationality).
defined_operationality(np_simple_np_vbar_optional_post_mods_operationality).

% Operationality for Japanese grammars
defined_operationality(jap_np_subordinate_clause_operationality).

% Operationality for French grammars
defined_operationality(fre_np_simple_np_vbar_pp_operationality).
defined_operationality(fre_np_lexical_simple_np_vbar_pp_operationality).

% Operationality for Spanish grammars
defined_operationality(spa_np_simple_np_vbar_pp_operationality).
defined_operationality(spa_np_lexical_simple_np_vbar_pp_operationality).

%---------------------------------------------------------------

% trivial operationality
%
% everything is operational

operational_goal(Goal, trivial_operationality, _Context, Cat) :-
	nongap_goal_for_cat(Goal, Cat).
operational_goal(Goal, trivial_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% lexical operationality
%
% only lexical goals can be operational

operational_goal(Goal, lexical_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).
operational_goal(Goal, lexical_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% NP (two-level) operationality
%
% NP operational under TOP
%
% Lexical goals operational anywhere

operational_goal(Goal, np_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).
operational_goal(Goal, np_operationality, top, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% NP_PP (three-level) operationality
%
% NP and PP operational under TOP
%
% NP operational under PP
%
% Lexical goals operational anywhere

operational_goal(Goal, np_pp_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).
operational_goal(Goal, np_pp_operationality, top, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_pp_operationality, top, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, np_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_pp_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% UTT_NP_PP (four-level) operationality
%
% UTTERANCE operational under TOP
%
% NP and PP operational under UTTERANCE
%
% NP operational under PP
%
% PP operational under NP
%
% Lexical goals operational anywhere

operational_goal(Goal, utt_np_pp_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, utt_np_pp_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, utt_np_pp_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, utt_np_pp_operationality, utterance, pp) :-
	nongap_goal_for_cat(Goal, pp).

operational_goal(Goal, utt_np_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, utt_np_pp_operationality, np, pp) :-
	nongap_goal_for_cat(Goal, pp).

operational_goal(Goal, utt_np_pp_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% UTT_NP_POST_MODS (four-level) operationality
%
% UTTERANCE operational under TOP
%
% NP and POST_MODS operational under UTTERANCE
%
% NP operational under POST_MODS
%
% POST_MODS operational under NP
%
% Lexical goals operational anywhere

operational_goal(Goal, utt_np_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, utt_np_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, utt_np_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, utt_np_post_mods_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, utt_np_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, utt_np_post_mods_operationality, np, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, utt_np_post_mods_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% UTT_NP_POST_MODS_OPTIONAL (four-level) operationality
%
% UTTERANCE operational under TOP
%
% NP and POST_MODS operational under UTTERANCE. POST_MODS can be empty.
%
% NP operational under POST_MODS
%
% POST_MODS operational under NP
%
% Lexical goals operational anywhere

operational_goal(Goal, utt_np_post_mods_optional_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, utt_np_post_mods_optional_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, utt_np_post_mods_optional_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, utt_np_post_mods_optional_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, utt_np_post_mods_optional_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, utt_np_post_mods_optional_operationality, np, post_mods) :-
	possibly_gap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, utt_np_post_mods_optional_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% UTT_NP_SIMPLE_NP_PP (five-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% NP and PP operational under UTTERANCE
%
% Simple NP and PP operational under NP
%
% NP operational under PP
%
% NUMBER operational under simple NP

operational_goal(Goal, utt_np_simple_np_pp_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, utt_np_simple_np_pp_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, utt_np_simple_np_pp_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, utt_np_simple_np_pp_operationality, utterance, pp) :-
	nongap_goal_for_cat(Goal, pp).

operational_goal(Goal, utt_np_simple_np_pp_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, utt_np_simple_np_pp_operationality, np, pp) :-
	nongap_goal_for_cat(Goal, pp).

operational_goal(Goal, utt_np_simple_np_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, utt_np_simple_np_pp_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% NP_POST_MODS (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% Simple NP and POST_MODS operational under UTTERANCE
%
% Simple NP operational under POST_MODS
%
% Lexical goals operational anywhere

operational_goal(Goal, np_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, np_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, np_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, np_post_mods_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, np_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, np_post_mods_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% NP_VBAR_POST_MODS (four-level) operationality
%
% UTTERANCE operational under TOP
%
% NP, VBAR and POST_MODS operational under UTTERANCE
%
% NP operational under POST_MODS
%
% NP operational under VBAR
%
% Lexical goals operational anywhere, except that V is not operational under VBAR

operational_goal(Goal, np_vbar_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, np_vbar_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, np_vbar_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_vbar_post_mods_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, np_vbar_post_mods_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, np_vbar_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_vbar_post_mods_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_vbar_post_mods_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context \== vbar ;
	    \+ goal_for_cat(Goal, v)
	).

%---------------------------------------------------------------

% SIMPLE_NP_VBAR_POST_MODS (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% Simple NP, VBAR and POST_MODS operational under UTTERANCE
%
% Simple NP operational under POST_MODS
%
% Simple NP operational under VBAR
%
% Lexical goals operational anywhere, except that 
%    i)  V is not operational under VBAR
%    ii) nothing is operational under simple NP (i.e. all simple NPs are lexical)

operational_goal(Goal, simple_np_vbar_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, simple_np_vbar_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, simple_np_vbar_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_post_mods_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_post_mods_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, simple_np_vbar_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_post_mods_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_post_mods_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = np ->
	    fail ;

	    Context = vbar ->
	    \+ goal_for_cat(Goal, v) ;

	    true
	).

%---------------------------------------------------------------

% SIMPLE_NP_VBAR_POST_MODS_NUMBER (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% Simple NP, VBAR and POST_MODS operational under UTTERANCE
%
% Simple NP operational under POST_MODS
%
% Simple NP operational under VBAR
%
% Lexical goals operational anywhere, except that 
%    i) only NUMBER and ADJ are operational under simple NP (i.e. all simple NPs are lexical except for numbers and adjs)

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_post_mods_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = np ->
	    ( goal_for_cat(Goal, number) ; goal_for_cat(Goal, adj) ) ;

	    true
	).

%---------------------------------------------------------------

% SIMPLE_NP_VBAR_OPTIONAL_POST_MODS_NUMBER (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% NP, VBAR and POST_MODS operational under UTTERANCE. POST_MODS can be null.
%
% VP, simple NP and POST_MODS operational under NP. POST_MODS can be null.
%
% NP operational under POST_MODS
%
% NP operational under VP
%
% NP operational under VBAR
%
% Lexical goals operational anywhere, except that 
%    i) only NUMBER and ADJ are operational under simple NP (i.e. all simple NPs are lexical except for numbers and adjs)

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, utterance, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, utterance, change_context(gap_np)) :-
	gap_goal_for_cat(Goal, np).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, utterance, post_mods) :-
	possibly_gap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, np, post_mods) :-
	possibly_gap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, np, vp) :-
	nongap_goal_for_cat(Goal, vp).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, post_mods, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, vbar, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, vp, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, vp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, vp, change_context(gap_np)) :-
	gap_goal_for_cat(Goal, np).

operational_goal(Goal, simple_np_vbar_optional_post_mods_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = simple_np ->
	    ( goal_for_cat(Goal, number) ; goal_for_cat(Goal, adj) ) ;

	    true
	).

%---------------------------------------------------------------

% NP_SIMPLE_NP_VBAR_POST_MODS (five-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% NP, VBAR and POST_MODS operational under UTTERANCE
%
% Simple NP and POST_MODS operational under NP
%
% NP operational under POST_MODS
%
% NP operational under VBAR
%
% Lexical goals operational anywhere

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, np, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_simple_np_vbar_post_mods_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% NP_SIMPLE_NP_VBAR_OPTIONAL_POST_MODS (recursive) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% NP, VBAR and POST_MODS operational under UTTERANCE. POST_MODS can be null.
%
% VP, simple NP and POST_MODS operational under NP. POST_MODS can be null.
%
% NP operational under POST_MODS
%
% NP operational under VP
%
% NP operational under VBAR
%
% Lexical goals operational anywhere
                       
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, utterance, change_context(gap_np)) :-
	gap_goal_for_cat(Goal, np).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, utterance, post_mods) :-
	possibly_gap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, np, post_mods) :-
	possibly_gap_goal_for_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, np, vp) :-
	nongap_goal_for_cat(Goal, vp).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, vp, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, vp, change_context(gap_np)) :-
	gap_goal_for_cat(Goal, np).

operational_goal(Goal, np_simple_np_vbar_optional_post_mods_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% SIMPLE_NP_POST_MODS_NUMBER (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% Simple NP and POST_MODS operational under UTTERANCE
%
% Simple NP operational under POST_MODS
%
% NUMBER operational under simple NP
%
% Lexical goals operational anywhere, except that only NUMBER is operational under simple NP

operational_goal(Goal, simple_np_post_mods_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, simple_np_post_mods_number_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, simple_np_post_mods_number_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, simple_np_post_mods_number_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, simple_np_post_mods_number_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_post_mods_number_operationality, np, number) :-
	nongap_goal_for_cat(Goal, number).

operational_goal(Goal, simple_np_post_mods_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = np ->

	    goal_for_cat(Goal, number) ;

	    true
	).

%---------------------------------------------------------------

% S_SIMPLE_NP_POST_MODS_NUMBER (four-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% S, simple NP and POST_MODS operational under TOP
%
% Simple NP and POST_MODS operational under S
%
% Simple NP operational under POST_MODS
%
% NUMBER operational under simple NP
%
% Lexical goals operational anywhere, except that only NUMBER is operational under simple NP

operational_goal(Goal, s_simple_np_post_mods_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, s_simple_np_post_mods_number_operationality, top, s) :-
	nongap_goal_for_cat(Goal, s).
operational_goal(Goal, s_simple_np_post_mods_number_operationality, top, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, s_simple_np_post_mods_number_operationality, top, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, s_simple_np_post_mods_number_operationality, s, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, s_simple_np_post_mods_number_operationality, s, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, s_simple_np_post_mods_number_operationality, post_mods, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, s_simple_np_post_mods_number_operationality, np, number) :-
	nongap_goal_for_cat(Goal, number).

operational_goal(Goal, s_simple_np_post_mods_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = np ->

	    goal_for_cat(Goal, number) ;

	    true
	).

%---------------------------------------------------------------

% NP_SIMPLE_NP_POST_MODS_NUMBER (five-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% NP and POST_MODS operational under UTTERANCE
%
% Simple NP operational under POST_MODS
%
% Simple NP and POST_MODS operational under NP
%
% NUMBER operational under simple NP
%
% Lexical goals operational anywhere, except that only NUMBER is operational under simple NP

operational_goal(Goal, np_simple_np_post_mods_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, np_simple_np_post_mods_number_operationality, utterance, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, post_mods, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).
operational_goal(Goal, np_simple_np_post_mods_number_operationality, np, post_mods) :-
	nongap_goal_for_cat(Goal, post_mods).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, simple_np, number) :-
	nongap_goal_for_cat(Goal, number).

operational_goal(Goal, np_simple_np_post_mods_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = simple_np ->

	    goal_for_cat(Goal, number) ;

	    true
	).

%---------------------------------------------------------------

% SIMPLE_NP_NUMBER (three-level) operationality
%
% (Simple NP == NP which does not dominate POST_MODS)
%
% UTTERANCE operational under TOP
%
% Simple NP operational under UTTERANCE
%
% Lexical goals operational anywhere, except that only NUMBER is operational under simple NP

operational_goal(Goal, simple_np_number_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, simple_np_number_operationality, top, utterance) :-
	nongap_goal_for_cat(Goal, utterance).

operational_goal(Goal, simple_np_number_operationality, utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, post_mods).

operational_goal(Goal, simple_np_number_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = np ->

	    goal_for_cat(Goal, number) ;

	    true
	).

%---------------------------------------------------------------

% jap_np_subordinate_clause_operationality (three-level) operationality
%
% NP, VBAR and SUBORDINATE_CLAUSE operational under TOP
%
% NP operational under SUBORDINATE_CLAUSE
%
% Lexical goals operational anywhere

operational_goal(Goal, jap_np_subordinate_clause_operationality, _Context, med_utterance) :-
	nongap_goal_for_cat(Goal, med_utterance).
operational_goal(Goal, jap_np_subordinate_clause_operationality, med_utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, jap_np_subordinate_clause_operationality, med_utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).
operational_goal(Goal, jap_np_subordinate_clause_operationality, med_utterance, subordinate_clause) :-
	nongap_goal_for_cat(Goal, subordinate_clause).
operational_goal(Goal, jap_np_subordinate_clause_operationality, subordinate_clause, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, jap_np_subordinate_clause_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% FRE_NP_SIMPLE_NP_VBAR_PP_OPERATIONALITY (recursive) operationality
%
% (Simple NP == NP which does not dominate PP)
%
% NP, VBAR, PP and OPTIONAL_PP operational under MED_UTTERANCE. 
%
% simple NP and PP operational under NP. 
%
% NP operational under PP
%
% NP operational under OPTIONAL_PP
%
% NP and OPTIONAL_ADVERB operational under VBAR
%
% Lexical goals operational anywhere
                       
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, _Context, med_utterance) :-
	nongap_goal_for_cat(Goal, med_utterance).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, med_utterance, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, med_utterance, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, med_utterance, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, med_utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, pp).
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, np, pp) :-
	possibly_gap_goal_for_cat(Goal, pp).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, optional_pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, vbar, optional_adverb) :-
	nongap_goal_for_cat(Goal, optional_adverb).

operational_goal(Goal, fre_np_simple_np_vbar_pp_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% FRE_NP_LEXICAL_SIMPLE_NP_VBAR_PP_OPERATIONALITY (recursive) operationality
%
% (Simple NP == NP which does not dominate PP or DE_PP)
%
% VP, NP, VBAR, PP and OPTIONAL_PP operational under MED_UTTERANCE. 
%
% NP, VBAR, PP and OPTIONAL_PP operational under VP. 
%
% simple NP and PP operational under NP. 
%
% NP and VP operational under PP
%
% NP and VP operational under OPTIONAL_PP
%
% NP and OPTIONAL_ADVERB operational under VBAR
%
% Lexical goals operational anywhere, except that
% only NUMBER and ADJP are operational under simple NP
% (i.e. all simple NPs are lexical except for numbers and adjps)
                       
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, _Context, med_utterance) :-
	nongap_goal_for_cat(Goal, med_utterance).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, med_utterance, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vp, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vp, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vp, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, np, pp) :-
	possibly_gap_goal_for_cat(Goal, pp).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, pp, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, pp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, optional_pp, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, optional_pp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, optional_pp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vbar, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_fre_np_postmod(Goal).
operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, vbar, optional_adverb) :-
	possibly_gap_goal_for_cat(Goal, optional_adverb).

operational_goal(Goal, fre_np_lexical_simple_np_vbar_pp_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = simple_np ->
	    ( goal_for_cat(Goal, number) ; goal_for_cat(Goal, adjp) ) ;

	    true
	).

%---------------------------------------------------------------

% SPA_NP_SIMPLE_NP_VBAR_PP_OPERATIONALITY (recursive) operationality
%
% (Simple NP == NP which does not dominate PP)
%
% NP, VBAR, PP and OPTIONAL_PP operational under MED_UTTERANCE. 
%
% simple NP and PP operational under NP. 
%
% NP operational under PP
%
% NP operational under OPTIONAL_PP
%
% NP and OPTIONAL_ADVERB operational under VBAR
%
% Lexical goals operational anywhere
                       
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, top, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, top, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, top, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, top, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_cat(Goal, pp).
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, np, pp) :-
	possibly_gap_goal_for_cat(Goal, pp).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, optional_pp, np) :-
	nongap_goal_for_cat(Goal, np).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np).
operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, vbar, optional_adverb) :-
	nongap_goal_for_cat(Goal, optional_adverb).

operational_goal(Goal, spa_np_simple_np_vbar_pp_operationality, _Context, lexical) :-
	lexical_goal(Goal).

%---------------------------------------------------------------

% SPA_NP_LEXICAL_SIMPLE_NP_VBAR_PP_OPERATIONALITY (recursive) operationality
%
% (Simple NP == NP which does not dominate PP or OF_PP)
%
% VP, NP, VBAR, PP and OPTIONAL_PP operational under TOP. 
%
% NP, VBAR, PP and OPTIONAL_PP operational under VP. 
%
% simple NP and PP operational under NP. 
%
% NP and VP operational under PP
%
% NP and VP operational under OPTIONAL_PP
%
% NP and OPTIONAL_ADVERB operational under VBAR
%
% Lexical goals operational anywhere, except that
% only NUMBER and ADJP are operational under simple NP
% (i.e. all simple NPs are lexical except for numbers and adjps)
                       
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, _Context, top) :-
	nongap_goal_for_cat(Goal, top).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, top, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vp, pp) :-
	nongap_goal_for_cat(Goal, pp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vp, optional_pp) :-
	possibly_gap_goal_for_cat(Goal, optional_pp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vp, vbar) :-
	nongap_goal_for_cat(Goal, vbar).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, np, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, np, pp) :-
	possibly_gap_goal_for_cat(Goal, pp).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, pp, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, pp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, pp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, optional_pp, vp) :-
	nongap_goal_for_cat(Goal, vp).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, optional_pp, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, optional_pp, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vbar, np) :-
	nongap_goal_for_cat(Goal, np),
	goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vbar, simple_np) :-
	nongap_goal_for_cat(Goal, np),
	\+ goal_dominates_spa_np_postmod(Goal).
operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, vbar, optional_adverb) :-
	possibly_gap_goal_for_cat(Goal, optional_adverb).

operational_goal(Goal, spa_np_lexical_simple_np_vbar_pp_operationality, Context, lexical) :-
	lexical_goal(Goal),
	(   Context = simple_np ->
	    ( goal_for_cat(Goal, number) ; goal_for_cat(Goal, adjp) ) ;

	    true
	).

%---------------------------------------------------------------

goal_dominates_fre_np_postmod(Goal) :-
	(   goal_dominates_cat(Goal, pp) ;
	    goal_dominates_cat(Goal, de_pp)
	).

%---------------------------------------------------------------

goal_dominates_spa_np_postmod(Goal) :-
	(   goal_dominates_cat(Goal, pp) ;
	    goal_dominates_cat(Goal, of_pp)
	).

%---------------------------------------------------------------

nongap_goal_for_cat(Goal, CatName) :-
	goal_for_cat(Goal, CatName),
	tree_in_goal(Goal, Tree),
	term_contains_functor(Tree, lex/1).

%---------------------------------------------------------------

possibly_gap_goal_for_cat(Goal, CatName) :-
	goal_for_cat(Goal, CatName).

%---------------------------------------------------------------

gap_goal_for_cat(Goal, CatName) :-
	goal_for_cat(Goal, CatName),
	gap_goal(Goal).

%---------------------------------------------------------------

gap_goal(Goal) :-
	tree_in_goal(Goal, Tree),
	\+ term_contains_functor(Tree, lex/1).

%---------------------------------------------------------------

lexical_goal(Goal) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_CatName, _LineInfo, Items),
	term_contains_functor(Tree, lex/1),
	is_list_of_lexical_items(Items).

%---------------------------------------------------------------

goal_dominates_cat(Goal, DominatedCatName) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	term_contains_functor(Daughters, DominatedCatName/0).

%---------------------------------------------------------------

goal_immediately_dominates_cat(Goal, DominatedCatName) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	comma_list_to_list(Daughters, DaughtersList),
	member(phrase(DominatedCatName, _LineInfo1, _), DaughtersList).

%---------------------------------------------------------------

goal_dominates_cat_but_not_through(Goal, DominatedCatName, NotThroughCatName) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	comma_list_to_list(Daughters, DaughtersList),
	cat_in_tree_daughters_list_but_not_through(DaughtersList, DominatedCatName, NotThroughCatName),
	!.

cat_in_tree_daughters_list_but_not_through([F | _R], DominatedCatName, NotThroughCatName) :-
	cat_in_tree_but_not_through(F, DominatedCatName, NotThroughCatName),
	!.
cat_in_tree_daughters_list_but_not_through([_F | R], DominatedCatName, NotThroughCatName) :-
	cat_in_tree_daughters_list_but_not_through(R, DominatedCatName, NotThroughCatName),
	!.

cat_in_tree_but_not_through(Tree, DominatedCatName, NotThroughCatName) :-
	Tree = phrase(Cat, _LineInfo, Daughters),
	Cat \== NotThroughCatName,
	(   Cat = DominatedCatName ->
	    true
	;
	    otherwise ->
	    comma_list_to_list(Daughters, DaughtersList),
	    cat_in_tree_daughters_list_but_not_through(DaughtersList, DominatedCatName, NotThroughCatName)
	),
	!.

%---------------------------------------------------------------

goal_dominates_lex(Goal, DominatedWord) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	term_contains_subterm(Daughters, lex(DominatedWord)).

%---------------------------------------------------------------

goal_immediately_dominates_lex(Goal, DominatedWord) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	comma_list_to_list(Daughters, DaughtersList),
	member(lex(DominatedWord), DaughtersList).

%---------------------------------------------------------------

goal_fringe(Goal, Fringe) :-
	tree_in_goal(Goal, Tree),
	nonvar(Tree),
	tree_fringe(Tree, Fringe-[]),
	!.
goal_fringe(_Goal, Fringe) :-
	Fringe = [].

tree_fringe(phrase(_Cat, _LineInfo, Daughters), FringeIn-FringeOut) :-
	tree_fringe(Daughters, FringeIn-FringeOut),
	!.
tree_fringe((F, R), FringeIn-FringeOut) :-
	tree_fringe(F, FringeIn-FringeNext),
	tree_fringe(R, FringeNext-FringeOut),
	!.
tree_fringe(lex(Item), [Item | FringeOut]-FringeOut) :-
	!.
tree_fringe(empty_constituent, FringeIn-FringeIn) :-
	!.
tree_fringe(Other, Fringe) :-
	format2error('~N*** Error: bad call: ~w~n', [tree_fringe(Other, Fringe)]),
	fail.

%---------------------------------------------------------------

built_in_goal(Goal, Goal) :-
	predicate_property(Goal, built_in),
	!.
built_in_goal(merge_globals(X, Y), regulus_eval:merge_globals(X, Y)).

%----------------------------------------------------------------------

goal_for_cat(Goal, Cat) :-
	functor(Goal, Cat, 6).

tree_in_goal(Goal, Tree) :-
	arg(1, Goal, Tree).

%----------------------------------------------------------------------

is_list_of_lexical_items(lex(_)) :-
	!.
is_list_of_lexical_items(empty_constituent) :-
	!.
is_list_of_lexical_items((lex(_), R)) :-
	is_list_of_lexical_items(R),
	!.
is_list_of_lexical_items((empty_constituent, R)) :-
	is_list_of_lexical_items(R),
	!.

