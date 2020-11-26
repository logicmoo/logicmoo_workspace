% lookup.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 02-13-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia	
%
% Look up a word in wn_s_convert.pl and print out the syntactiv category and definition.
% Or, look up a word and return a list of all synsets in that the word was found. Each synset 
% in the list is a list of words.

:- ensure_loaded('wn_s_convert.pl').
:- ensure_loaded('convert_file.pl').
:- ensure_loaded('wn_g.pl').
:- ensure_loaded('find_synset.pl').

% lookup(+Word)
% Look up a word. If it is an atom, then check if it is in the open list of the s/6 predicate.
% If yes, get the definition and print out the category and its definition.
% If it is an open list, then just find the matching s/6 predicates and print out the
% requested information.
% If there are no definitions or no more definitions, print out 'No (more) definitions'.
% If you want to look up an expression like physical thing use the open list input format.

lookup(Word) :- atom(Word), 
		s([Word],Number,_,Category,_,_), 
		g(Number,Definition),
		write(Word), nl,
		write(' Syntactic category: '), 
		write(Category), nl,
		write(' '), 
		write(Definition), nl,
		fail, !.
		

lookup(OpenList) :-	reverse(OpenList,List1), 
			reverse(List1,WordList),!,
			s(WordList,Number,_,Category,_,_),
			g(Number,Definition),
			write(OpenList),nl,
			write(' Syntactic category: '), 
			write(Category), nl,
			write(' '), 
			write(Definition), nl,
			fail.

lookup(_) :- 	write('No (more)  definitions'),
		nl.

% create_open_list_1
% If there is no underscore in the words, then make an open list containing only the word.
% If there is an underscore in the word make an open list from all the words.

create_open_list_1(WordAtoms,[Word|Rest],_,EndList) :- 	\+ member(95,WordAtoms),
							name(Word,WordAtoms),
							reverse([Word|Rest],EndList), !.
						
create_open_list_1([95|WordAtoms],[Word|Rest],WordList,_) :- 	name(Word,WordList),
								create_open_list_1(WordAtoms,Rest,[],_),
								!.

create_open_list_1([First|WordAtoms],_,WordList,_) :- 	append(WordList,First,WordList2),
							create_open_list_1(WordAtoms,_,WordList2,_). 		
% lookup(+Word,-SynList)
% Take a word and find all Synset Numbers to which this word belongs.
% Put them all in one list and call lookup_aux/2.
% Also, check whether you are handling an atom or an open list.

lookup(Word,SynList) :-	atom(Word), 
			findall(	Number,
					s([Word],Number,_,_,_,_),
					List
			),
			lookup_aux(List,SynList), !.


lookup(OpenList,SynList) :-	reverse(OpenList,List1), reverse(List1,WordList),!,
				findall(	Number,
						s(WordList,Number,_,_,_,_),
						List
				),
				lookup_aux(List,SynList).

% lookup_aux(+NumList,-SynList)
% Take a list of synset numbers and find to every number all words that are in this synset.
% Put these words in a list and recurse to get the rest of the list.  

lookup_aux([First|Rest1],[WordList|Rest2]) :- 	find_synset(First,WordList),
						lookup_aux(Rest1,Rest2), !.
	
lookup_aux([],[]).	


