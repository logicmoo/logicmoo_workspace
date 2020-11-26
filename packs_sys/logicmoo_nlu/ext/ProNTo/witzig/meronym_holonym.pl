% meronym_holonym.pl
%
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 03-20-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%

:- ensure_loaded('wn_mm.pl').
:- ensure_loaded('wn_ms.pl').
:- ensure_loaded('wn_mp.pl').
:- ensure_loaded('wn_s_convert.pl').

% Operator(+Word,-List)
% Depending on the operator requested, call the predicate all_one/4 or all_two/4
% with the third argument instantiated to the predicate that needs to be
% consulted later. Use n as the fourth argument as the category for
% meronyms and holonyms is always n.
% The category argument needs to be introduced, as the predicates
% all_one/4 and all_two/4 will be reused in other files.

% Given a noun, find the list of groups that the word is a member of.

member_of(Word,WordList)	:- all_one(Word,WordList,mm,n).

% Given a noun, find the list of members that are in the group.

has_member(Word,WordList)	:- all_two(Word,WordList,mm,n).

% Given a noun find the list of words that the word is a substance of.

substance_of(Word,WordList)	:- all_one(Word,WordList,ms,n).

% Given a noun find the list of substances that are in the word.

has_substance(Word,WordList)	:- all_two(Word,WordList,ms,n).

% Given a noun, find the list of things that the word is a part of.

part_of(Word,WordList)		:- all_one(Word,WordList,mp,n).

% Given a noun, find the list of things that are part of the word.

has_part(Word,WordList)		:- all_two(Word,WordList,mp,n).


% all_one(+Word,-WordList,+Pred)
% Find the synsets of which the given word is part of.
% Get the information from the predicate Pred, which is changed
% according to which predicate was queried by the user.
% Then, call make_list/3 to make a list of words from the
% produced list of synset IDs.

all_one(Word,WordList,Pred,Cat) :- 	atom(Word), X =.. [Pred,Num,MMNum],
					findall(	MMNum,
							(s([Word],Num,_,Cat,_,_),X),
							MMList
					), 
					make_list(MMList,WordList,Cat), !.

all_one(OpenList,WordList,Pred,Cat) :- 	reverse(OpenList,WordL), reverse(WordL,WordL2),!,
					X =.. [Pred,Num,MMNum],
					findall(	MMNum,
							(s(WordL2,Num,_,Cat,_,_),X),
							MMList
					), 
					make_list(MMList,WordList,Cat).

all_two(Word,WordList,Pred,Cat) :- 	atom(Word), X =.. [Pred,MMNum,Num],
					findall(	MMNum,
							(s([Word],Num,_,Cat,_,_),X),
							MMList
					), 
					make_list(MMList,WordList,Cat), !.

all_two(OpenList,WordList,Pred,Cat) :- 	reverse(OpenList,WordL), reverse(WordL,WordL2),!,
					X =.. [Pred,MMNum,Num],
					findall(	MMNum,
							(s(WordL2,Num,_,Cat,_,_),X),
							MMList
					), 
					make_list(MMList,WordList,Cat).


% make_list(+SynList,-WordList)
% Find all words that belong to the synsets in the given SynList 
% and put them in one list to return to the second argument.

make_list([First|Rest],WordList,Cat) :-	findall(	Word,
							s(Word,First,_,Cat,_,_), 
							WordListFirst
					), 
					make_list(Rest,WordListRest,Cat),
					append(WordListFirst,WordListRest,WordList).
make_list([],[],_).









				
