
% Add entries from the general grammar into the specialised grammar.

% Include all cardinal numbers 

include_lex(number:[], [default]).

% Include all ordinal numbers (needed for dates) and months
%include_lex(ordinal:[], [default]).
%include_lex(month:[], [default]).

% Add all names (we assume that there aren't any irrelevant names)

%include_lex(name:[], [default]).

% Add all inflected forms for each verb occurring in the training corpus.

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
dont_include_lex(v:[words='wasn''t'], [default]).
dont_include_lex(v:[words='weren''t'], [default]).
dont_include_lex(v:[words='isn''t'], [default]).
dont_include_lex(v:[words='aren''t'], [default]).

dont_include_lex(v:[words=(was, not)], [default]).
dont_include_lex(v:[words=(were, not)], [default]).
dont_include_lex(v:[words=(is, not)], [default]).
dont_include_lex(v:[words=(are, not)], [default]).

dont_include_lex(v:[words=be], [default]).
dont_include_lex(v:[words=being], [default]).

dont_include_lex(v:[words='''re'], [default]).
dont_include_lex(v:[words='''ve'], [default]).

