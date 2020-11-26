% Add all entries for verbs or adjectives from the general grammar into the specialised grammar,
% if any entry for that word is present.
/*
include_lex(verb:[sem=[action, Value]], Tags) :-
	rule_exists(verb:[sem=[[action, Value]]], Tags).
include_lex(verb:[sem=[event, Value]], Tags) :-
	rule_exists(verb:[sem=[[event, Value]]], Tags).
include_lex(verb:[sem=[path_proc, Value]], Tags) :-
	rule_exists(verb:[sem=[[path_proc, Value]]], Tags).
include_lex(verb:[sem=[state, Value]], Tags) :-
	rule_exists(verb:[sem=[[state, Value]]], Tags).

include_lex(verb:[sem=[Type, Value]], Tags) :-
	rule_exists(verb:[sem=[[tense, Tense], [Type, Value]]], Tags).
*/

include_lex(verb:[], []).

include_lex(adjp:[sem=[Type, Value]], Tags) :-
	rule_exists(adjp:[sem=[[Type, Value]]], Tags).

%include_lex(adjp:[], []).

% Include all adverbs and other subdomain-independent categories

include_lex(adv:[], []).
include_lex(pronoun:[], []).
include_lex(reflexive_pronoun:[], []).
include_lex(aux_verb:[], []).
include_lex(spec:[], []).
include_lex(number:[], []).


