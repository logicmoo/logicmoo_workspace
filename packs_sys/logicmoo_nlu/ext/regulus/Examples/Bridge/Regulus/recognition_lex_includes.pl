
% Add entries from the general grammar into the specialised grammar.

% Include all bids, cards, agents and suits

include_lex(n:[sem='one_c'], [default]).
include_lex(n:[sem='one_d'], [default]).
include_lex(n:[sem='one_h'], [default]).
include_lex(n:[sem='one_s'], [default]).
include_lex(n:[sem='one_nt'], [default]).
include_lex(n:[sem='two_c'], [default]).
include_lex(n:[sem='two_d'], [default]).
include_lex(n:[sem='two_h'], [default]).
include_lex(n:[sem='two_s'], [default]).
include_lex(n:[sem='two_nt'], [default]).
include_lex(n:[sem='three_c'], [default]).
include_lex(n:[sem='three_d'], [default]).
include_lex(n:[sem='three_h'], [default]).
include_lex(n:[sem='three_s'], [default]).
include_lex(n:[sem='three_nt'], [default]).
include_lex(n:[sem='four_c'], [default]).
include_lex(n:[sem='four_d'], [default]).
include_lex(n:[sem='four_h'], [default]).
include_lex(n:[sem='four_s'], [default]).
include_lex(n:[sem='four_nt'], [default]).
include_lex(n:[sem='five_c'], [default]).
include_lex(n:[sem='five_d'], [default]).
include_lex(n:[sem='five_h'], [default]).
include_lex(n:[sem='five_s'], [default]).
include_lex(n:[sem='five_nt'], [default]).
include_lex(n:[sem='six_c'], [default]).
include_lex(n:[sem='six_d'], [default]).
include_lex(n:[sem='six_h'], [default]).
include_lex(n:[sem='six_s'], [default]).
include_lex(n:[sem='six_nt'], [default]).
include_lex(n:[sem='seven_c'], [default]).
include_lex(n:[sem='seven_d'], [default]).
include_lex(n:[sem='seven_h'], [default]).
include_lex(n:[sem='seven_s'], [default]).
include_lex(n:[sem='seven_nt'], [default]).

include_lex(n:[sem=a], [default]).
include_lex(n:[sem=k], [default]).
include_lex(n:[sem=q], [default]).
include_lex(n:[sem=j], [default]).
include_lex(n:[sem=10], [default]).
include_lex(n:[sem=9], [default]).
include_lex(n:[sem=8], [default]).
include_lex(n:[sem=7], [default]).
include_lex(n:[sem=6], [default]).
include_lex(n:[sem=5], [default]).
include_lex(n:[sem=4], [default]).
include_lex(n:[sem=3], [default]).
include_lex(n:[sem=2], [default]).

include_lex(n:[sem=club], [default]).
include_lex(n:[sem=diamond], [default]).
include_lex(n:[sem=heart], [default]).
include_lex(n:[sem=spade], [default]).

include_lex(n:[sem=north], [default]).
include_lex(n:[sem=south], [default]).
include_lex(n:[sem=east], [default]).
include_lex(n:[sem=west], [default]).

% Distribution adjectives

include_lex(adj:[sem=distributed_1_1], [default]).
include_lex(adj:[sem=distributed_2_0], [default]).
include_lex(adj:[sem=distributed_3_0], [default]).
include_lex(adj:[sem=distributed_4_0], [default]).
include_lex(adj:[sem=distributed_5_0], [default]).
include_lex(adj:[sem=distributed_6_0], [default]).
include_lex(adj:[sem=distributed_2_1], [default]).
include_lex(adj:[sem=distributed_3_1], [default]).
include_lex(adj:[sem=distributed_4_1], [default]).
include_lex(adj:[sem=distributed_5_1], [default]).
include_lex(adj:[sem=distributed_6_1], [default]).
include_lex(adj:[sem=distributed_2_2], [default]).
include_lex(adj:[sem=distributed_3_2], [default]).
include_lex(adj:[sem=distributed_4_2], [default]).
include_lex(adj:[sem=distributed_5_2], [default]).
include_lex(adj:[sem=distributed_6_2], [default]).
include_lex(adj:[sem=distributed_3_3], [default]).
include_lex(adj:[sem=distributed_4_3], [default]).
include_lex(adj:[sem=distributed_5_3], [default]).
include_lex(adj:[sem=distributed_6_3], [default]).
include_lex(adj:[sem=distributed_4_4], [default]).
include_lex(adj:[sem=distributed_5_4], [default]).
include_lex(adj:[sem=distributed_6_4], [default]).
include_lex(adj:[sem=distributed_5_5], [default]).
include_lex(adj:[sem=distributed_6_5], [default]).

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

include_lex(v:[sem=[verb, Value]], Tags) :-
	rule_exists(v:[sem=[[verb, Value]]], Tags).
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

