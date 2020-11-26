% cause.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 04-20-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%

:- ensure_loaded('wn_cs.pl').
:- ensure_loaded('meronym_holonym.pl').
:- ensure_loaded('wn_s_convert.pl').

% cause(+Word,-WordList)
% Given a verb, create a list of verbs that are causes of the given verb.
% The predicate all_one/4 is part of the file meronym_holonym.pl

cause(Word,WordList)	:- all_one(Word,WordList,cs,v).
