
% Start with TOP rule
change_rule_and_context(_Context, top) :-
	cat(top),
	\+ gap.

% Start new rule at UTTERANCE 
change_rule_and_context(_Context, utterance) :-
	cat(utterance),
	\+ gap.

% Start new rule at NP or DP if under UTTERANCE
change_rule_and_context(utterance, dp) :-
	cat(dp),
	\+ gap.

% Always start new rule at lexical node
change_rule_and_context(_Context, lexical) :-
	lexical.
