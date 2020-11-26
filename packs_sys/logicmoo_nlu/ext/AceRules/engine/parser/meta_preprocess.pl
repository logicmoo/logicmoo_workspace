% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(meta_preprocess, [
		meta_preprocess/4  % +Codes, -Text, -LabelMap, -Overrides
	]).

:- use_module(ape('parser/tokenizer')).
:- use_module(ape('logger/error_logger')).
:- use_module('../utils').
:- use_module('../list_utils').

/** <module> Meta preprocessor

Processes the meta-structures of a rule definition, i.e. labels and overrides-statements.

@author Tobias Kuhn
@version 2007-02-07
*/


%% meta_preprocess(+Codes, -PlainText, -LabelMap, -Overrides)
%
% Splits the input text (Codes) into a map of labels (LabelMap maps labels to sentence numbers),
% a set of override-statements (Overrides), and the remaining text (PlainText).

meta_preprocess(Codes, PlainText, [0-''|LabelMap], Overrides) :-
    clear_messages,
	tokenize(Codes, Tokens), !,
	(Tokens = [] ->
		throw(ar_error('parser.meta-preprocess.EmptyProgram', 'The program is empty.'))
	;
		true
	),
	split(Tokens, SentenceLists),
	extract_meta_info(SentenceLists, 1, PlainSentences, LabelMap, Overrides),
	check_overrides(Overrides, LabelMap),
	concat_tokens(PlainSentences, PlainText),
	!.


%% split(+Tokens, -SentenceLists)
%
% Splits a list of tokens (Tokens) into a list of sentences (SentenceLists)
% where each sentence itself is represented as a list of tokens.

split([], []).

split(Tokens, Sentences) :-
	get_sentence(Tokens, [], Sentence, RestTokens),
	split(RestTokens, RestSentences),
	add_sentence(Sentence, RestSentences, Sentences).


add_sentence([], Sentences, Sentences) :-
    !.

add_sentence(Sentence, Sentences, [Sentence|Sentences]).


%% get_sentence(+Tokens, +AccTokens, -Sentence, -RestTokens)
%
% Returns the tokens from the beginning of the list Tokens upto the first
% occurence of a full stop '.'. This is stored in Sentence. AccTokens contains
% the accumulated tokens that are already processed. RestTokens returns the rest
% of the tokens that are beyond the first full stop.

get_sentence(['<p>'|RestTokens], Sentence, Sentence, RestTokens) :-
	!.

get_sentence(['.'|RestTokens], AccSentence, Sentence, RestTokens) :-
	!,
	append(AccSentence, ['.'], Sentence).

get_sentence(['?'|_], AccSentence, _, _) :-
    concat_tokens(AccSentence, AccSentenceAtom),
    concat_list(['Queries are not allowed: "', AccSentenceAtom, '?"'], ErrorMessage),
    throw(ar_error('parser.meta-preprocess.ContainsQuery', ErrorMessage)).

get_sentence([T|RestTokens1], AccSentence1, Sentence, RestTokens2) :-
	append(AccSentence1, [T], AccSentence2),
	get_sentence(RestTokens1, AccSentence2, Sentence, RestTokens2).

get_sentence([], _, _, _) :-
    throw(ar_error('parser.meta-preprocess.NoFullStopAtEnd', 'The program must end with a full stop.')).


%% extract_meta_info(+SentLists, +StartNumber, -PlainSent, -LabelMap, -Overrides)
%
%

extract_meta_info([], _, [], [], []).

extract_meta_info(SentLists, N, [PlainSentAtom|PlainSentRest], [N-Label|LabelMapRest], Overrides) :-
	SentLists = [[Label, ':'|PlainSentTokens]|SentListsRest],
	valid_label(Label),
	!,
	concat_tokens(PlainSentTokens, PlainSentAtom),
	NP is N + 1,
	extract_meta_info(SentListsRest, NP, PlainSentRest, LabelMapRest, Overrides).

extract_meta_info(SentLists, N, PlainSent, LabelMap, Overrides) :-
	SentLists = [[Label1, 'overrides', Label2, '.']|SentListsRest],
	!,
	Overrides = [overrides(Label1, Label2)|OverridesRest],
	extract_meta_info(SentListsRest, N, PlainSent, LabelMap, OverridesRest).

extract_meta_info(SentLists, N, [PlainSentAtom|PlainSentRest], [N-''|LabelMapRest], Overrides) :-
	SentLists = [PlainSentTokens|SentListsRest],
	concat_tokens(PlainSentTokens, PlainSentAtom),
	NP is N + 1,
	extract_meta_info(SentListsRest, NP, PlainSentRest, LabelMapRest, Overrides).


check_overrides([], _).

check_overrides([overrides(Label1, Label2)|Rest], LabelMap) :-
    (\+ member(_-Label1, LabelMap) ->
    	concat_list(['The label "', Label1, '" does not exist.'], Message),
    	throw(ar_error('parser.meta-preprocess.InexistentLabel', Message))
    ;
    	true
    ),
    (\+ member(_-Label2, LabelMap) ->
    	concat_list(['The label "', Label2, '" does not exist.'], Message),
    	throw(ar_error('parser.meta-preprocess.InexistentLabel', Message))
    ;
    	true
    ),
    !,
    check_overrides(Rest, LabelMap).


valid_label(Label) :-
    atom_codes(Label, [First|_]),
    First >= 65,   % "A"
    First =< 90.   % "Z"
