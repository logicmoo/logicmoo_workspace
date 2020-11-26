
% Add all entries for numbers, months, days and names
% from the general grammar into the specialised grammar.

include_lex(number:[], [default]).
include_lex(month:[], [default]).
include_lex(day:[], [default]).
include_lex(name:[], [default]).

% Add all inflected forms for each verb occurring in the training corpus.

include_lex(v:[sem=[verb, Value]], Tags) :-
	rule_exists(v:[sem=[[verb, Value]]], Tags).

include_lex(v:[sem=[verb, Value]], Tags) :-
	rule_exists(v:[sem=[[tense, Tense], [verb, Value]]], Tags).
