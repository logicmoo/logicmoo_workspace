
% Start new rule at UTTERANCE 
change_rule_and_context(_Context, utterance) :-
	cat(utterance),
	\+ gap.

% Start new rule at NP or POST_MODS if under UTTERANCE
change_rule_and_context(utterance, np) :-
	cat(np),
	\+ gap.
change_rule_and_context(utterance, post_mods) :-
	cat(post_mods),
	\+ gap.

% Start new rule at NP if under POST_MODS
%change_rule_and_context(post_mods, np) :-
%	cat(np),
%	\+ gap.

% Start new rule at POST_MODS if under NP
change_rule_and_context(np, post_mods) :-
	cat(post_mods),
	\+ gap.

% Always start new rule at lexical node
change_rule_and_context(_Context, lexical) :-
	lexical.
