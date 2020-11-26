% subset_wordnet.pl
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 03-20-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%
% Call subset_wn_s and then convert all other WordNet files accordingly.

:- ensure_loaded('subset_wn_s.pl').

% subset_wordnet(+Number)
% Call subset_wn_s to make a subset of the file wn_s.pl which has an amount of words equal to
% the argument Number. Subset_wn_s/2 also returns an IDList of all the words of the subset.
% Then, call subset to convert all files of WordNet.
% The first argument of the predicate subset stores the number of arguments that a predicate
% has in each file to indentify which procedure is needed.
% Exceptions: The predicate g of the file wn_g.pl has two arguments, but gets the argument number
% 1 as its structure doesn't match with the other 2-place predicates.

subset_wordnet(Number) :- 	write('Processing wn_s.pl ...'),nl,
				subset_wn_s(Number,IDList),
				write('Processing wn_g.pl ...'),nl,
				subset(1,'wn_g.pl',g,IDList),
				write('Processing wn_at.pl ...'),nl,
				subset(2,'wn_at.pl',at,IDList),
				write('Processing wn_cs.pl ...'),nl,
				subset(2,'wn_cs.pl',cs,IDList),
				write('Processing wn_ent.pl ...'),nl,
				subset(2,'wn_ent.pl',ent,IDList),
				write('Processing wn_hyp.pl ...'),nl,
				subset(2,'wn_hyp.pl',hyp,IDList),
				write('Processing wn_mm.pl ...'),nl,
				subset(2,'wn_mm.pl',mm,IDList),
				write('Processing wn_mp.pl ...'),nl,
				subset(2,'wn_mp.pl',mp,IDList),
				write('Processing wn_ms.pl ...'),nl,
				subset(2,'wn_ms.pl',ms,IDList),
				write('Processing wn_sim.pl ...'),nl,
				subset(2,'wn_sim.pl',sim,IDList),
				write('Processing wn_fr.pl ...'),nl,
				subset(3,'wn_fr.pl',fr,IDList),
				write('Processing wn_ant.pl ...'),nl,
				subset(4,'wn_ant.pl',ant,IDList),
				write('Processing wn_per.pl ...'),nl,
				subset(4,'wn_per.pl',per,IDList),
				write('Processing wn_ppl.pl ...'),nl,
				subset(4,'wn_ppl.pl',ppl,IDList),
				write('Processing wn_sa.pl ...'),nl,
				subset(4,'wn_sa.pl',sa,IDList),
				write('Processing wn_vgp.pl ...'),nl,
				subset(4,'wn_vgp.pl',vgp,IDList),
				write('Done.').


% subset_two_args(+Args,+Filename,+Pred,+IDList)
% The predicate takes four arguments, one to store the Number of Arguments Args that each
% predicate in the file has, the Filename itself, then the predicate name Pred that is 
% described in the file and finally the IDList of the words that should be in the subset 
% of the files.
% First open the file to read from it. Then, open a new file to write in it.
% The name of the new file should have the following structure: wn_PREDICATENAME_subset.pl.
% Call make_subset/4 and close the InStream and OutStream.

subset(Args,Filename,Pred,IDList) :- 	open(Filename,read,InStream),
					atom_chars(Pred,List),
					append([w,n,'_'],List,NewList),
					append(NewList,['_',s,u,b,s,e,t,'.',p,l],EndList),
					atom_chars(X,EndList),
					open(X,write,OutStream),
					make_subset(Args,InStream,OutStream,IDList),
 					close(OutStream), 
					close(InStream).

% make_subset(+Args,+Instream,-OutStream,+IDList)
% The first argument of make_subset tells us how many arguments a predicate
% in the file has. According to that, we modify the algorithm, so that
% we can check whether an ID Number is in the IDList or not.
% If the whole file was read and we are at the end of the InStream, succeed.
% Else read the first line of the file which is a predicate structure
% to get the arguments that are the ID Numbers. Use the univ operator.
% Then check whether one of the ID Numbers is in the IDList.
% If yes, write the predicate structure in the new file and call
% make_stream recursivly. Else call make_subset. In that case the predicate 
% structure won't be in the new file.
% Make sure that no apostrophes get lost in the wn_g.pl file!!!


make_subset(_,InStream,_,_) :-			at_end_of_stream(InStream).

make_subset(1,InStream,OutStream,IDList) :- 	read(InStream,WholePred),
						WholePred =.. [Pred,ID1,Def],
						member(ID1,IDList), 
						!,
						write(OutStream,Pred),
						write(OutStream,'('),
						write(OutStream,ID1),
						write(OutStream,','),
						put(OutStream,39),
						write(OutStream,Def),
						put(OutStream,39),
						write(OutStream,')'),
						write(OutStream,'.'),
						put(OutStream,10),
						make_subset(1,InStream,OutStream,IDList).

make_subset(2,InStream,OutStream,IDList) :- 	read(InStream,WholePred),
							WholePred =.. [_,ID1,ID2],
						(	member(ID1,IDList); 
							member(ID2,IDList)
						),!,
						write(OutStream,WholePred),
						write(OutStream,'.'),
						put(OutStream,10),
						make_subset(2,InStream,OutStream,IDList).

make_subset(3,InStream,OutStream,IDList) :- 	read(InStream,WholePred),
						WholePred =.. [_,ID,_,_],
						member(ID,IDList), 
						!,
						write(OutStream,WholePred),
						write(OutStream,'.'),
						put(OutStream,10),
						make_subset(3,InStream,OutStream,IDList).

make_subset(4,InStream,OutStream,IDList) :- 	read(InStream,WholePred),
							WholePred =.. [_,ID1,_,ID2,_],
						(	member(ID1,IDList); 
							member(ID2,IDList)
						),!,
						write(OutStream,WholePred),
						write(OutStream,'.'),
						put(OutStream,10),
						make_subset(4,InStream,OutStream,IDList).

make_subset(Args,InStream,OutStream,IDList) :- 	make_subset(Args,InStream,OutStream,IDList).
