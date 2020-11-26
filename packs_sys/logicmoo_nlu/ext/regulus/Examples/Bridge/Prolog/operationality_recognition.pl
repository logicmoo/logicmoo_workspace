
% Operationality criteria for English MedSLT recognition

% Simple NP == NP which does not dominate POST_MODS

%----------------------------------------------------------------------------

% Start with TOP rule
change_rule_and_context(_Context, top) :-
	cat(top),
	\+ gap.

% UTTERANCE operational under TOP
change_rule_and_context(top, utterance) :-
	cat(utterance),
	\+ gap.

% NP and S operational under UTTERANCE.
%
% *** IMPORTANT ***
% Introduce the S layer because we don't want POST_MODS to be
% directly under UTTERANCE - this makes temporal NPs ambiguous.
change_rule_and_context(utterance, np) :-
	cat(np),
	\+ gap,
	dominates(post_mods).
change_rule_and_context(utterance, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).
change_rule_and_context(utterance, s) :-
	cat(s).

% NP, VBAR and POST_MODS operational under S. POST_MODS can be null.
change_rule_and_context(s, np) :-
	cat(np),
	\+ gap,
	dominates(post_mods).
change_rule_and_context(s, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).
change_rule_and_context(s, post_mods) :-
	cat(post_mods).
change_rule_and_context(s, vbar) :-
	cat(vbar),
	\+ gap.

% VP, simple NP and POST_MODS operational under NP. POST_MODS can be null.
change_rule_and_context(np, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).
change_rule_and_context(np, post_mods) :-
	cat(post_mods).
change_rule_and_context(np, vp) :-
	cat(vp),
	\+ gap.	

% NP operational under POST_MODS
change_rule_and_context(post_mods, np) :-
	cat(np),
	\+ gap,
	dominates(post_mods).
change_rule_and_context(post_mods, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).

% NP operational under VBAR
change_rule_and_context(vbar, np) :-
	cat(np),
	\+ gap,
	dominates(post_mods).
change_rule_and_context(vbar, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).

% NP operational under VP
change_rule_and_context(vp, np) :-
	cat(np),
	\+ gap,
	dominates(post_mods).
change_rule_and_context(vp, simple_np) :-
	cat(np),
	\+ gap,
	\+ dominates(post_mods).

% NUMBER operational under simple NP
change_rule_and_context(simple_np, number) :-
	cat(number).

% Lexical goals operational anywhere
change_rule_and_context(_Anything, lexical) :-
	lexical.

%----------------------------------------------------------------------------

% Treat gap NPs specially - change the context to "gap_np".
% The problem is that we can otherwise have gap NPs containing empty post_mods,
% which create spurious rules.
change_context(utterance, gap_np) :-
	cat(np),
	gap.
change_context(vp, gap_np) :-
	cat(np),
	gap.

% Following to handle the case where we have a bare subordinate clause at top-level.
% We want to switch context when we get to the post_mods node, so that we don't
% induce a declarative s rule.
change_context(utterance, post_mods) :-
	cat(post_mods),
	\+ gap.
