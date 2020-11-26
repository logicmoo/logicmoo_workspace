
% Add entries from the general grammar into the specialised grammar.

% Add all inflected forms for each verb occurring in the headache training corpus.

% Need to enumerate cases, since if we write
%
% include_lex(v:[sem=[Type, Value]], Tags) :-
%	 rule_exists(v:[sem=[[Type, Value]]], Tags).
%
% the tense auxiliaries screw everything up.

include_lex(v:[sem=[action, Value]], Tags) :-
	rule_exists(v:[sem=[[action, Value]]], Tags).
include_lex(v:[sem=[event, Value]], Tags) :-
	rule_exists(v:[sem=[[event, Value]]], Tags).
include_lex(v:[sem=[state, Value]], Tags) :-
	rule_exists(v:[sem=[[state, Value]]], Tags).

include_lex(v:[sem=[Type, Value]], Tags) :-
	rule_exists(v:[sem=[[tense, Tense], [Type, Value]]], Tags).

% ... except a few auxiliaries that cause problems
dont_include_lex(v:[words='wasn''t']).
dont_include_lex(v:[words='weren''t']).
dont_include_lex(v:[words='isn''t']).
dont_include_lex(v:[words='aren''t']).

dont_include_lex(v:[words=(was, not)]).
dont_include_lex(v:[words=(were, not)]).
dont_include_lex(v:[words=(is, not)]).
dont_include_lex(v:[words=(are, not)]).

dont_include_lex(v:[words=be]).
dont_include_lex(v:[words=being]).

