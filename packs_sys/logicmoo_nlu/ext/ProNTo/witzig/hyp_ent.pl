% hyp_ent.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 04-27-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
% Given a noun or a verb, find all its hypernyms and return them in a list.
% Given a verb, find all its entailments and return them in a list.

:- ensure_loaded('wn_s_convert.pl').
:- ensure_loaded('wn_hyp.pl').
:- ensure_loaded('wn_ent.pl').

% find_hyp_chains(+Word,+Cat), find_ent_chains(+Word,+Cat)
% Call find_chains/3 with the predicate that should be used as a third argument,
% so we don't need to write the algorithm twice.

find_hyp_chains(Word,Cat) :- find_chains(Word,Cat,hyp).
find_ent_chains(Word) :- find_chains(Word,v,ent).

% find_hyp(+Word,+Cat,-List), find_ent(+Word,-List)
% Call find/4 with the predicate that should be used as the fourth argument,
% so we don't need to write the algorithm twice.
% The hypernym can be a noun or a verb, so the category is a part of the user input.
% In the case of the entailment the user doesn't need to input any category,
% as the relation only works for verbs.

find_hyp(Word,Cat,List) :- 	find(Word,Cat,List,hyp).
find_ent(Word,List) :-		find(Word,v,List,ent).

% find_chains(+Word,+Cat,+Pred)
% Given a word and its category, find its number in the synset in WordNet, by using 
% the predicate s/6. Then, get all its hypernyms/entailments which should have the 
% same category and print them on the screen with print_out/1.
% If the input word is an atom, then look for the list of this word as
% wn_s_convert.pl is handling only open lists.
% If the input is an open list reverse it twice, to convert it in a normal list and
% look for it in wn_s_convert.pl. Otherwise the computer would find all expressions 
% in that the word is in, but we only want to have exactly this word, and not an 
% expression containing that word.

find_chains(Word,Cat,Pred) :- 	atom(Word),
				s([Word],Num,_,Cat,_,_),
				find_chains_aux(Num,Pred),
				write('The word '), 
				write(Word), 
				write(' belonging to the synset number '),
				write(Num), 
				write(' has the following '), 
				write(Pred), 
				write(' chain: '), nl,
				print_out(Cat), nl, 
				fail, !.

find_chains(OpenList,Cat,Pred) :- 	reverse(OpenList,WordL), 
					reverse(WordL,WordL2),!,
					s(WordL2,Num,_,Cat,_,_),
					find_chains_aux(Num,Pred),
					write('The word '), 
					write(OpenList), 
					write(' belonging to the synset number '),
					write(Num), 
					write(' has the following '), 
					write(Pred), 
					write(' chain: '), nl,
					print_out(Cat), nl, 
					fail.

% find_chains_aux(+Num)
% Take the synset number of a word and find its hypernyms or entailments (also get all hypernyms/entailments of
% the hypernyms/entailments with find_chain_aux/2). Then, assert the number of the hypernym/entailment in
% the knowledge base with the predicate know/1 and backtrack to get all other hypernyms/entailments.
% After all are found, the predicate succeeds. The predicate that looks for the entailment or hypernym 
% is created by the operator univ and the given argument Pred.

find_chains_aux(Num1,Pred)	:- 	X =.. [Pred,Num1,Num2],
					X, 
					find_chains_aux(Num2,Pred), 
					assertz(know(Num2)), 
					fail.
find_chains_aux(_,_).

% print_out(+Cat)
% Take a number from the knowledge base. Find all the corresponding
% words of this synset number and write them on the screen. 
% Retract the synset number from the knowledge base.
% Go on like this, until there is nothing in the knowledge base left.

print_out(Cat):- 	know(Num), 
			findall(	Word,
					s(Word,Num,_,Cat,_,_), 
					List
			),
			write(List), nl, 
			retract(know(Num)), 
			print_out(Cat), !.
print_out(_). 

% find(+Word,+Cat,-List)
% Find all hypernyms/entailments of a word and put them in a list. Similar to find_chains,
% but instead of printing the hypernyms/entailments on the screen with print_out/1, call
% make_list/2 and return a list of hypernyms/entailments to the third argument.
% By typing ; you can get the other hypernym/entailment chains of the synsets that the
% word belongs to.

find(Word,Cat,List,Pred) :- 	atom(Word), 
				s([Word],Num,_,Cat,_,_),
				find_chains_aux(Num,Pred), nl, 
				write('The word '), 
				write(Word), 
				write(' belonging to the synset number '),
				write(Num), 
				write(' has the following '), 
				write(Pred), 
				write(':'), nl,
				make_list(Cat,List), 
				write('Type ; to get '), 
				write(Pred), 
				write(' of the word '),
				write(Word), 
				write(' belonging to another synset!'), nl.

find(OpenList,Cat,List,Pred) :- \+ atom(OpenList),
				reverse(OpenList,WordL), 
				reverse(WordL,WordL2),!,
				s(WordL2,Num,_,Cat,_,_),
				find_chains_aux(Num,Pred), nl, 
				write('The word '), 
				write(WordL2), 
				write(' belonging to the synset number '),
				write(Num), write(' has the following '), 
				write(Pred), 
				write(':'), nl,
				make_list(Cat,List), 
				write('Type ; to get '), 		
				write(Pred), 
				write(' of the word '),
				write(WordL2), 
				write(' belonging to another synset!'), nl.

% make_list(+Cat,-List2)
% Find all words that belong to a synset of all numbers that are in the knowledge base.
% Then, retract all predicates know/1 and remove the duplicates from the list.

make_list(Cat,List2):-	findall(	Word,
					(know(Num),s(Word,Num,_,Cat,_,_)),
					List1
			), 
			retractall(know(_)), 
			remove_duplicates(List1,List2),	!.

% remove_duplicates(+List1,-List2)
% Take a list and return a list after removing all its duplicates.

remove_duplicates([],[]).
remove_duplicates([First|List1],List2) :- 		member(First,List1), 
							remove_duplicates(List1,List2),!.
remove_duplicates([First|List1],[First|List2]) :- 	remove_duplicates(List1,List2). 



				
