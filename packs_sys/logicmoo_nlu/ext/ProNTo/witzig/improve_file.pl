% improve_file.pl
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 02-13-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
% Opens the wn_s.pl file for input, makes some changes to the order of the 
% arguments to improve the efficiency and gives out a new file wn_s_new.pl.
%
% improve_file
% Open wn_s.pl, to read from it.
% Open a new file to write the new predicates in it. 
% Call improve_file/2 to make the changes. Close the two Streams.

improve_file :- 	open('wn_s.pl',read,InStream),
			open('wn_s_improve.pl',write,OutStream), 
			improve_file(InStream,OutStream), 	
			close(OutStream), 
			close(InStream).


% improve_file(+InStream,-OutStream)
% Read a predicate from the stream. Change the order of its arguments.
% To improve indexing the word should be the first argument.
% Make sure, that the word is in apostrophes. Also, apostrophes in the word 
% have to be written twice, like in the original file.
% Therefore take the ASCII list of the word and check if it has apostrophes
% with test_ap/2. If no, just write out the Word.
% If yes, put an extra apostrophe in the word with put_ap/2.
% Convert the ASCII list with the extra apostrophe into an atom.
% Finally, write out the other arguments of the predicate.
% Call improve_file/2 recursively to convert all other s predicates.

improve_file(Stream,_) :- at_end_of_stream(Stream).

improve_file(InStream,OutStream) :-	read(InStream,s(Num,Syn,Word,Cat,X,Y)), 
					write(OutStream,s), 
					write(OutStream,'('), 
					put(OutStream,39),
					atom_chars(Word,WordAtoms),
					(	
						(	test_ap(WordAtoms,no),
							write(OutStream,Word)
						)
					;
						(	test_ap(WordAtoms,yes),
							put_ap(WordAtoms,WordAtoms1),
							atom_chars(Word2,WordAtoms1), 
							write(OutStream,Word2)
						) 
					),	
					put(OutStream,39), 
					write(OutStream,','),
					write(OutStream,Num), 
					write(OutStream,','),
					write(OutStream,Syn), 
					write(OutStream,','),
					write(OutStream,Cat), 
					write(OutStream,','),
					write(OutStream,X), 
					write(OutStream,','),
					write(OutStream,Y), 
					write(OutStream,')'), 
					put(OutStream,46),
					put(OutStream,10),
					improve_file(InStream,OutStream).
				

% test_ap(+WordAtoms,-YesOrNo)
% Checks whether there is an apostrophe in the list.

test_ap(WordAtoms,no) :- \+member('\'',WordAtoms).
test_ap(WordAtoms,yes) :- member('\'',WordAtoms).

% put_ap(+List,-List1)
% Takes a List and puts an extra apostrophe in and returns the new List1.

put_ap([],[]).
put_ap(['\''|List],['\'','\''|List1]) :- put_ap(List,List1), !.
put_ap([First|List],[First|List1]) :- put_ap(List,List1).
