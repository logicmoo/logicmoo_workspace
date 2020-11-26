% lookup_text.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 02-13-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
% Try looking up all the words in a text, to find out which words 
% are not in WordNet and return them in a list.
%  

:- ensure_loaded('hyp_ent.pl').
:- ensure_loaded('et.pl').		% efficient tokenizer, see ProNTo
:- ensure_loaded('wn_s_convert.pl').	% word net synsets

% lookup_text(FileName)
% Take a text and tokenize it with ET, the efficient tokenizer 
% from Michael A. Covington.
% Eliminate everything that is not a word. The tokenizer returns 
% a list of letters of each word. This list needs to be changed into an
% atom. Before going on, remove all duplicates to reduce the number
% of words. Then, check which atoms are in WordNet and print out the answer.

lookup_text(FileName) :- 	tokenize_file(FileName,Tokens),	
				eliminate_words(Tokens,WordList),
				make_atoms(WordList,AtomList),
				remove_duplicates(AtomList,AtomList1),
				check_list(AtomList1,NotList),
				print_answer(NotList).

% eliminate_words(+List,-List)
% The predicate tokenize_file gives a list of all kinds of tokens.
% All tokens that are not words / not in the form of w(T) are eliminated.

eliminate_words([],[]) :- !.
eliminate_words([w(A)|Tokens],[w(A)|EliminatedTokens]) :- 	eliminate_words(Tokens,EliminatedTokens),!. 
eliminate_words([_|Tokens],EliminatedTokens) :- 		eliminate_words(Tokens,EliminatedTokens).

% make_atoms(+WordList,-AtomList)
% Takes a list of tokens, created by the ET, and turns it into a list of atoms.

make_atoms([],[]).
make_atoms([w(First)|WordList],[Atom|AtomList]) :- 	atom_chars(Atom,First), 
							make_atoms(WordList,AtomList).

% check_list(+AtomList,-NotList)
% Takes a list of atoms and checks if each of them is in WordNet.
% It returns a list of the atoms that can't be found in WordNet.

check_list([],[]).
check_list([First|AtomList],NotList) :- lookup1(First), check_list(AtomList,NotList).
check_list([First|AtomList],[First|NotList]) :- \+ lookup1(First), check_list(AtomList,NotList).

% lookup1(Word)
% Looks up if a word can be found in the WordNet database

lookup1(Word) :-	s([Word],_,_,_,_,_).

% print_answer(+NotList)
% If NotList is empty, all words have been found in WordNet, so a note is printed on
% the screen. If it isn't empty printList/1 is called which prints out all the words.

print_answer([]) :- 		write('All words in this text have been found in WordNet').
print_answer(NotList) :- 	write('The following words have not been found in WordNet: '), nl,
				printList(NotList).

% printList(+List)
% Print each member of a list on the screen. Succeed if there are no members left.

printList([]).
printList([First|Rest]) :- write(First), nl, printList(Rest).

% lookup_text(+FileName,-NotList)
% Works as lookup_text/1, but instead of printing the words, not found
% in WordNet on the screen, it returns them in a list.

lookup_text(FileName,NotList) :- 	tokenize_file(FileName,Tokens),	
					eliminate_words(Tokens,WordList),
					make_atoms(WordList,AtomList),
					remove_duplicates(AtomList,AtomList1),
					check_list(AtomList1,NotList), !.


