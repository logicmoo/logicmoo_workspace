% subset_wn_s.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 03-20-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%

% subset_wn_s.pl
% Look up the most common words in WordNet and make a new file just containing these words.
% The amount of words that should be in this subset of WordNet can be set by the user.
% The input file opened in the algorithm is wn_s.pl, the output file is calles wn_s_subset.pl.

% make sure improve_file.pl is loaded as we will need the predicate put_ap/2 from it 

:- ensure_loaded('improve_file.pl').

% subset_wn_s(+Number+NumList)
% Open wn_s.pl to read from it.
% Open a new file to write the chosen predicates in it. 
% Call subset_wn_s/3 to chose which words are the most common ones. Close the two Streams.
% The argument Number sets how big the subset should be.
% The second argument returns a list of all IDs that are in the subset.
% This list is needed to modify all other WordNet files matching the subset.

subset_wn_s(Number,NumList) :- 	open('wn_s.pl',read,InStream),
				open('wn_s_subset.pl',write,OutStream), 
				subset_wn_s(InStream,OutStream,Number,NumList), 	
				close(OutStream), 
				close(InStream).

% subset_wn_s(+Input.-Output,+Number, -NumList)
% Calls makelist/2 which makes a list containing all the s/6 predicates in the file wn_s.pl.
% In this list the argument order of s/6 has changed, so that the last argument, containing
% information about the frequency of the word in a corpus, is the first argument.
% Msort and reverse this list, so that the most common words are the first ones in the list.
% The predicate write_output/4 takes care of writing the chosen s/6 predicates into a file.
% The last argument of the predicate returns the list of all IDs of the subset.

subset_wn_s(InStream,OutStream,Number,NumList) :-	makelist(InStream,List),
							msort(List,ListS),
							reverse(ListS,ListR),
							write_output(OutStream,Number,ListR,NumList).

% makelist(+InStream,-Pred)
% Reads in all the s predicates and stores them into a list.
% Also, it puts the last argument that contains information about the word frequency 
% in the beginning.

makelist(InStream,[]) :- 					at_end_of_stream(InStream).
makelist(InStream,[s(Count,Num,Syn,Word,Cat,X)|List])	:-	read(InStream,s(Num,Syn,Word,Cat,X,Count)),
								makelist(InStream,List).

% write_output(-OutStream,+C,+ListOfPredicates,-ListOfIDs)
% Write out all the first s/6 predicates into a new file until the requested number is reached. 
% Also, put back the first argument Count into its original position. 
% Save all IDs of the words that go into the subset and write them into a list.
% We will need that feature for the predicate subset_wordnet.
% Make sure that apostrophes are doubled while written in the subset file!

write_output(_,0,_,[]) :- !.
write_output(OutStream,C,[s(Count,Num,Syn,Word,Cat,X)|ListR],[Num|Rest]) :-	write(OutStream,s),
										write(OutStream,'('),
										write(OutStream,Num),
										write(OutStream,','),
										write(OutStream,Syn),
										write(OutStream,','),
										put(OutStream,39),
										atom_chars(Word,WordAtoms),
										(
											(	\+ member('\'',WordAtoms),
												write(OutStream,Word)
											);	
											(	put_ap(WordAtoms,NewWordAtoms),
												atom_chars(NewWord,NewWordAtoms),
												write(OutStream,NewWord)
											)
										),
										put(OutStream,39),
										write(OutStream,','),
										write(OutStream,Cat),
										write(OutStream,','),
										write(OutStream,X),
										write(OutStream,','),
										write(OutStream,Count),
										write(OutStream,')'),
										write(OutStream,'.'), 
										put(OutStream,10),
										C2 is C - 1, 
										write_output(OutStream,C2,ListR,Rest).

				

				 




	
