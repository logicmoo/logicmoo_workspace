% find_sim.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 02-13-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
:- ensure_loaded('wn_s_convert.pl').
:- ensure_loaded('wn_sim.pl').

% find_sim_meanings(+Word)
% Given a word, find all its synset numbers in WordNet by using 
% the predicate s/6. The category must be a or s as this operator 
% is just defined for adjectives. Then, get all its similar meanings, and
% call the predicate print_sim_list to print the similar meanings on the screen.
% If the input word is an atom, then look for the list of this word as
% wn_s_convert is handling only open lists. If the input is an open list 
% reverse it twice, to convert it in a normal list and
% look for it in wn_s_convert.pl. Otherwise the computer would find all 
% expressions in that the word is in, but we only want to have exactly 
% this word and not an expression containing that word.

find_sim_meanings(Word) :- 	atom(Word),
				findall(	SimNum,
						(	(s([Word],Num,_,a,_,_);s([Word],Num,_,s,_,_)),
							sim(Num,SimNum)
						),
						SimList
				), print_sim_list(SimList), !.
			
find_sim_meaning(OpenList) :- 	reverse(OpenList,WordL), reverse(WordL,WordL2),!,
				findall(	SimNum,
						(	(s(WordL2,Num,_,a,_,_);s(WordL2,Num,_,s,_,_)),
							sim(Num,SimNum)
						),
						SimList
				), print_sim_list(SimList).
				

% print_sim_list(+SimList)
% Take a list of synset numbers, representing the similar meanings.
% Find all adjectives in these synsets and print them on the screen.

print_sim_list([First|Rest]) :-	findall(	Word,
						(s(Word,First,_,a,_,_);s(Word,First,_,st,_,_)), 
						List
				), 
				(List \= [], write(List), nl, print_sim_list(Rest));
				print_sim_list(Rest).
print_sim_list([]).


% find_sim(+Word,-List)
% Find all similar meanings of a word, and put them in a list. Similar to find_sim_meaning,
% but instead of printing the similar meanings on the screen with print_sim_list call
% make_sim_list/2 and return a list of similar meanings to the second argument.

find_sim(Word,WordList) :- 	atom(Word),
				findall(	SimNum,
						(	(s([Word],Num,_,a,_,_);s([Word],Num,_,s,_,_)),
							sim(Num,SimNum)
						),
						SimList
				), make_sim_list(SimList,WordList), !.

find_sim(OpenList,WordList) :- 	reverse(OpenList,WordL), reverse(WordL,WordL2),!,
				findall(	SimNum,
						(	(s(WordL2,Num,_,a,_,_);s(WordL2,Num,_,s,_,_)),
								sim(Num,SimNum)
						),
						SimList
				), make_sim_list(SimList,WordList).

% make_sim_list(+SynList,-WordList)
% Find all words, that belong to the synsets in the given SynList and put them in one list to return to
% the second argument.

make_sim_list([First|Rest],WordList) :-	findall(	Word,
							(s(Word,First,_,a,_,_);s(Word,First,_,st,_,_)), 
							WordListFirst
					), 
					make_sim_list(Rest,WordListRest),
					append(WordListFirst,WordListRest,WordList).
make_sim_list([],[]).				
