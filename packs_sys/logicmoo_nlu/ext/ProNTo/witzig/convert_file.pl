% convert_file.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 02-13-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
% This is an extended version of improve_file.pl. Additionally to
% changing the word order of the arguments to improve indexing, 
% it changes all upper case letters to lower case, removes all 
% underscores and represents the words as open lists. The new 
% predicates of the wn_s_new.pl files have the following structure: 
% s([human,action|_G4016],100022113,2,n,1,1).
%
% convert_file
% Open a wn_s.pl to read from it.
% Open a new file to write the new predicates in it. 
% Call improve_file/2 to make the changes, close the two Streams.

convert_file :- 	open('wn_s.pl',read,InStream),
			open('wn_s_convert.pl',write,OutStream), 
			convert_file(InStream,OutStream), 	
			close(OutStream), 
			close(InStream).


% convert_file(+InStream,-OutStream)
% Read a predicate from the stream and change the order of its arguments.
% To improve indexing, the word should be the first argument.
% Test whether the word has capital letters, apostrophes and make the
% required changes. Call create_open_list/2 to convert the word
% into an open list and then write out the new predicate in the
% output file. Call convert_file/2 recursively until all predicates
% are converted

convert_file(Stream,_) :- at_end_of_stream(Stream).

convert_file(InStream,OutStream) :-	read(InStream,s(Num,Syn,Word,Cat,X,Y)), 
					write(OutStream,s), 
					write(OutStream,'('), 
					name(Word,WordAtoms),
					(
						test_nocap_noap(WordAtoms,WordAtoms1);
						test_nocap_ap(WordAtoms,WordAtoms1);
						test_cap_noap(WordAtoms,WordAtoms1);
						test_cap_ap(WordAtoms,WordAtoms1)
					),	
					create_open_list(WordAtoms1,OpenList),
					write(OutStream,OpenList),	
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
					convert_file(InStream,OutStream).

% test_nocap_noap(+WordAtoms,-WordAtoms)
% Test succeeds, if there is no apostrophe and no capital letter in the atom list of the word.
% If it succeeds, it returns the atom list to the second argument.

test_nocap_noap(WordAtoms,WordAtoms) :- \+ member(39,WordAtoms), \+ capital_in_list(WordAtoms).

% test_nocap_ap(+WordAtoms,-WordAtoms1)
% Test succeeds, if there are apostrophes in the list, but no capital letters.
% The new ASCII list is changed by the predicate remove_ap/2, which removes the apostrophes.

test_nocap_ap(WordAtoms,WordAtoms1) :- 	member(39,WordAtoms), \+ capital_in_list(WordAtoms),
					remove_ap(WordAtoms,WordAtoms1).

% test_cap_noap(+WordAtoms,-WordAtoms1)
% Succeeds, if there are no apostrophes in the word, but capital letters. So the predicate
% change_to_lower/2 is called that converts all capital letters into lower case letters.

test_cap_noap(WordAtoms,WordAtoms1) :- 	\+ member(39,WordAtoms), capital_in_list(WordAtoms),
					change_to_lower(WordAtoms,WordAtoms1).

% test_cap_ap(+WordAtoms,-WordAtoms1)
% Succeeds, if there are apostrophes and capital letters in the word. Remove_ap/2 takes care of
% the apostrophes, change_to_lower/2 changes the capital letters to lower case letters.

test_cap_ap(WordAtoms,WordAtoms1) :-  	member(39,WordAtoms), capital_in_list(WordAtoms),
					remove_ap(WordAtoms,WordAtomsTemp), 
					change_to_lower(WordAtomsTemp,WordAtoms1).
							
				
% remove_ap(+List,-List1)
% Takes a List and changes the apostrophes to an underscore.
% So later, when the underscores get removed and the open
% list gets created, the apostrophe in a word will 
% basically divide the word, e.g. mother's becomes [mother,s|_].

remove_ap([],[]).
remove_ap([39|List],[95|List1]) :- 		remove_ap(List,List1), !.
remove_ap([First|List],[First|List1]) :- 	remove_ap(List,List1).

% change_to_lower(+List,-List)
% Takes a list and changes all capital letters into lower case letters.

change_to_lower([],[]).
change_to_lower([First|Rest],[First|Rest1]) :- 	\+ capital(First), 
						change_to_lower(Rest,Rest1), !.
change_to_lower([First|Rest],[First1|Rest1]) :- First1 is First + 32, 
						change_to_lower(Rest,Rest1).

% capital_in_list(+List)
% Checks whether there is a capital letter in an ASCII code list.

capital_in_list([F|_]) :- 	capital(F), !.
capital_in_list([_|Rest]) :- 	capital_in_list(Rest).

% capital(+X)
% X is a capital letter if its ASCII code is between 65 and 90.

capital(X) :- X >=65, X =< 90. 

% create_open_list(+WordAtoms,-OpenList)
% If there is no underscore in WordAtoms, then make an open list containing only the one word.
% If there is an underscore in WordAtoms, then call change_underscore/2 to put commas
% instead of the underscore, and make an open list out of all the words.

create_open_list(WordAtoms,OpenList) :- 	\+ member(95,WordAtoms), 
						append([91,39],WordAtoms,List1),
						append(List1,[39,124,95,93],List2),
						name(OpenList,List2).

create_open_list(WordAtoms,OpenList) :- 	member(95,WordAtoms),
						change_underscore(WordAtoms,WordAtoms2),
						append([91,39],WordAtoms2,List1),
						append(List1,[39,124,95,93],List2),
						name(OpenList,List2).	


% change_underscore(+List1,-List2)
% Change all underscores of a list into commas.

change_underscore([],[]).

change_underscore([95|Rest1],[39,44,39|Rest2]) :- change_underscore(Rest1,Rest2), !.

change_underscore([First|Rest1],[First|Rest2]) :- change_underscore(Rest1,Rest2).
