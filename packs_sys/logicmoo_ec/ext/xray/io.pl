%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  4/04/95   File: parser.pl                    %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  4/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog parser.pl                                                 %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reads the knowledge base from the file 'Name.kb'

read_kb(Name,Wff) :-
	concatenate(Name,'.kb',KBFile),
	read_clauses(KBFile,Wff).	

read_ckb(Name,Wff) :-
	concatenate(Name,'.ckb',CKBFile),
	read_clauses(CKBFile,Wff).	

read_que(Name,Wff) :-
	concatenate(Name,'.que',QFile),
	read_clauses(QFile,Wff).	

read_clauses(File,Wff) :-
	open(File,read,Stream),
	read_wff_loop(Stream,Wff),
	close(Stream).

read_wff_loop(Stream,Wff) :-
	read(Stream,Wff1),
	(Wff1 == end_of_file ->
	           Wff = true;
	 %true               ->
		   read_wff_loop(Stream,Wff2),
		   conjoin(Wff1,Wff2,Wff)).

read_matrix(File,Wff) :-
	open(File,read,Stream),
	read_matrix_loop(Stream,Wff),
	close(Stream).

read_matrix_loop(Stream,Matrix) :-
	read(Stream,Elem),
	(Elem == end_of_file ->
	           Matrix = [];
	%true                ->
		   read_matrix_loop(Stream,L),
		   Matrix = [Elem|L]).

% writes a compiled knowledge base consisting of contrapositives only
% to the file 'Name.ckb'

write_ckb(File,KB) :-
	concatenate(File,'.ckb',KBFile),
	open(KBFile,write,KBStream),
	concatenate(File,'.que',QFile),
	open(QFile,write,QStream),
        write_contrapositives(streams(KBStream,QStream),KB),
        close(KBStream),
        close(QStream),
	get_file_info(KBFile,size,KBFileSize),
	get_file_info(QFile,size,QFileSize),
	nl,nl,
	write(KBFile),write(" written "),write(KBFileSize),writeln(" bytes"),
	write(QFile), write(" written "),write(QFileSize), writeln(" bytes"),
	!.

write_cmm(File,Matrix) :-
	concatenate(File,'.cmm',MFile),
	open(MFile,write,MStream),
        write_matrix(MStream,Matrix),
        close(MStream),
	!.

write_query(File,Query) :-
	concatenate(File,'.que',QFile),
	open(QFile,write,QStream),
        write_query_only(QStream,Query),
        close(QStream),
	!.

write_query_only(Stream,(A,B)) :-
	!,
        write_query_only(Stream,A),
        write_query_only(Stream,B).
write_query_only(Stream,(A:-B)) :-
	functor(A,query,_) ->
		write_clauses(Stream,(A:-B));
	%true ->
		true.


write_contrapositives(Streams,(A,B)) :-
	!,
        write_contrapositives(Streams,A),
        write_contrapositives(Streams,B).
write_contrapositives(streams(KBStream,QStream),(A:-B)) :-
	functor(A,query,_) ->
		write_clauses(QStream,(A:-B));
	%true ->
		write_clauses(KBStream,(A:-B)).	


write_clauses(Stream,(A,B)) :-
        write_clauses(Stream,A),
        write_clauses(Stream,B),
        !.
write_clauses(Stream,A) :-
        write(Stream,A),
        write(Stream,.),
        nl(Stream),
        !.
write_clauses(A) :-
	File = 'temp.pl',
	open(File,write,Stream),
	write_clauses(Stream,A),
	close(Stream).


write_matrix(Stream,[]) :-
	nl(Stream),
        !.
write_matrix(Stream,[E|L]) :-
        write(Stream,E),
        write(Stream,.),
        nl(Stream),
	write_matrix(Stream,L),
        !.
write_matrix(L) :-
	File = 'temp.pl',
	open(File,write,Stream),
	write_matrix(Stream,L),
	close(Stream).


compile_ckb(File) :-	
	concatenate(File,'.ckb',KBFile),
	compile(KBFile).

compile_query(File) :-	
	concatenate(File,'.que',QFile),
	compile(QFile).

ask(Name,Query) :-
	(variables(Query,[]) ->
	         classical_clauses(Query,Q0),
		 cnf(Q0,Q1),
		 make_matrix(Q1,Q2),
		 matrix_reduction(Q2,Q);
	%true ->
		 Q = []),

	concatenate(Name,'.cmm',MFile),
	read_matrix(MFile,Matrix),
	
	dpttp1((query:-Query),Q,Matrix,Query1:Matrix),

	nl,
        write('XRay writing query ... '),
        write_query(Name,Query1),
	write('done.'),

	nl,
        write('XRay compiling query ... '),
        compile_query(Name),
	write('done.'),
        nl,
        !.

tell(Name,Wff) :-	
	read_kb(Name,KB),
	conjoin(Wff,KB,NewKB),
	dpttp(Name,NewKB).

write_proved(Proof,ProofEnd) :-
        write('proved'),
	verbose_flag ->
	        write(' by:'),
		write_proof(Proof,ProofEnd);
	%true->
		length(Proof,X),
		write(' qed ;-) ':X).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
	nl,
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).
