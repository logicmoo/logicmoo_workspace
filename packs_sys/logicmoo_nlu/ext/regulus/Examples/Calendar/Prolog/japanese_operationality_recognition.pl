
% TOP operational
change_rule_and_context(_Context, top) :-
	cat(top).

% UTTERANCE operational
change_rule_and_context(_Context, utterance) :-
	cat(utterance).

% NP, PP, VBAR operational under UTTERANCE
change_rule_and_context(utterance, np) :-
	cat(np).
change_rule_and_context(utterance, pp) :-
	cat(pp).
change_rule_and_context(utterance, vbar) :-
	cat(vbar).

% NP and SUBORDINATE_CLAUSE operational under PP
change_rule_and_context(pp, np) :-
	cat(np).
change_rule_and_context(pp, subordinate_clause) :-
	cat(subordinate_clause).

% N operational under SUBORDINATE_CLAUSE
change_rule_and_context(subordinate_clause, n) :-
	cat(n).

% Lexical goals operational anywhere
change_rule_and_context(_Anything, lexical) :-
	lexical.
