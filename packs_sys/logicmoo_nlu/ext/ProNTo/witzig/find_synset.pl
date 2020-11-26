% find_synset.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 04-19-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
:- ensure_loaded('wn_s_convert.pl').

% find_synset(+ID,-WordList)
% Given a synset ID, return a list of all words that are in this synset

find_synset(ID,WordList) :-	findall(	Word,
						s(Word,ID,_,_,_,_),
						WordList
				).

