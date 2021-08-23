%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2010, 2015 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(external_hyper, [run_hyper/3,
			   nf_to_tme/2,
			   tme_to_nf/2,
			   nf_to_tme_with_equality_predicate/2]).

:- use_module(swilib(fromonto)).
:- use_module(swilib(tempfiles)).
:- use_module(nf(nfutils)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Calling Hyper from SWI-Prolog
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%%  run_hyper(Input, Options, Output)
%%%%
%%%%    Input and Output are lists of terms. Hyper is invoked with
%%%%    the input statements as input. Its output terms are returned
%%%%    as Output.
%%%% 
%%%%    Options:
%%%%
%%%%      timeout=Seconds (a natural number)
%%%%             - If the timeout is reached, the hyper process is aborted
%%%%               and outputs written so far are returned.
%%%%
%%%%      result=Result (an integer or a variable)
%%%%             - unified with the result of the subprocess evaluation.
%%%%
%%%%      hyper=HYPER (an atom)
%%%%             - specifies the hyper binary. Overrides default settings.
%%%%
%%%%      version=VERSION (an atom)
%%%%             - specifies whether the old krhyper or the new hyper
%%%%               is used (they differ in input syntax and proof format)
%%%%               default: new_hyper, other possibility: old_krh
%%%%
%%%%      status=STATUS
%%%%             - number or variable: will be unified with status value
%%%%               returned by the hyper invocation
%%%%                           
%%%%      tmp_input_file=FILE (an atom)
%%%%             - specifies the name of the file to be used as temporary
%%%%               file for the input to hyper. Overrides default settings.
%%%%
%%%%      tmp_output_file=FILE (an atom)
%%%%             - specifies the name of the file to be used as temporary
%%%%               file for the input to hyper. Overrides default settings.
%%%%
%%%%      tmp_error_file=FILE (an atom)
%%%%             - specifies the name of the file to be used for error
%%%%               output.
%%%%
%%%%      error_output=SPEC (an atom)
%%%%             - if SPEC is
%%%%               - stderr : Error output will not be redirected to a file
%%%%               - append : Error output will be appended to error output
%%%%                          file
%%%%               - write  : Error output will be written to error output
%%%%                          file (default)
%%%%
%%%%    Example:
%%%%
%%%%      ?- run_hyper([p(a), q(X) :- p(X), #(run), #(print_extent)], [], T).
%%%%  

run_hyper(Input, Options, Output) :-
	( memberchk(hyper=HYPER, Options) -> true
	; HYPER=hyper
	),
	( memberchk(version=VERSION, Options) -> true
	; VERSION=new_hyper
	),
	( memberchk(tmp_input_file=INFILE, Options) -> true
	; tempfile(hyper, '1', INFILE)
	),
	( memberchk(tmp_output_file=OUTFILE, Options) -> true
	; tempfile(hyper, '2', OUTFILE)
	),
	( memberchk(tmp_error_file=ERRORFILE, Options) -> true
	; tempfile(hyper, '3', ERRORFILE)
	),
	( memberchk(status=STATUS, Options) -> true
	; true
	),
	( memberchk(error_output=ERRORSPEC, Options) -> true
	; ERRORSPEC=write
	),
	( ERRORSPEC = stderr ->  
          ERRORDIRECT = ''
	; ERRORSPEC = append ->
	  sformat(ERRORDIRECT, '2>>~w', ERRORFILE)
	; sformat(ERRORDIRECT, '2>~w', ERRORFILE)
	),
	onto_file(( member(X, Input),
		    ( X = (X1 :- true) -> true
		    ; X1 = X
		    ),
		    \+ \+ ( numbervars(X1, 0, _),
			    ( VERSION == new_hyper ->
			      write_term(X1, [quoted, numbervars])
			    ; write_term(X1, [quoted, numbervars, ignore_ops])
			    )),
		    writeln('.'),
		    fail
		  ; true
		  ),
		  INFILE),
	( memberchk(timeout=TimeoutSpec, Options) ->
	  sformat(TIMEOUT, 'ulimit -t ~w ; ', [TimeoutSpec])
	; TIMEOUT = ''
	),
	sformat(Cmd, '~w ~w ~w ~w >~w',
		[TIMEOUT, HYPER, ERRORDIRECT, INFILE, OUTFILE]),
	catch(( info(40, 'Calling hyper: ~w', [Cmd]),
		shell(Cmd, STATUS1),
		STATUS = STATUS1,
		info(40, 'Hyper status: ~w', [STATUS]),
		file_clauses_1(OUTFILE, Output),
		cleanup(Options)
		),
	      E,
	      ( cleanup(Options),
		throw(E)
	      )).

cleanup(Options) :-
	memberchk( cleanup=false, Options ),
	!.
cleanup(_) :-
	delete_tempfiles(hyper).

file_clauses_1(File, Clauses) :-
	from_file(findall(T,
			  ( repeat,
			    read(T),
			    ( T == end_of_file ->
				!,
		              fail
			    ; true
			    )
			  ),
			  Clauses),
		  File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Support for the TME Format
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tme_to_nf(M, M1) :-
	map_tme_clause_to_nf(M, M1).

map_tme_clause_to_nf([X|Xs], [X1|Xs1]) :-
	tme_clause_to_nf(X, X1),
	map_tme_clause_to_nf(Xs, Xs1).
map_tme_clause_to_nf([], []).

tme_clause_to_nf((H :- B), C) :-
        !,
	positives(H, C1),
	negatives(B, C2),
	append(C1, C2, C).
tme_clause_to_nf(H, C) :-
	positives(H, C).

positives(false, []) :-
	!.
positives((false ; Ls), Ls1) :-
	!,
	positives(Ls, Ls1).
positives((L ; Ls), [L|Ls1]) :-
	!,
	positives(Ls, Ls1).
positives(L, [L]).

negatives(true, []) :-
	!.
negatives((true , Ls), Ls1) :-
	!,
	negatives(Ls, Ls1).
negatives((L , Ls), [~(L)|Ls1]) :-
	!,
	negatives(Ls, Ls1).
negatives(L, [~(L)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nf_to_tme(M, M1) :-
	map_nf_clause_to_tme(M, M1).

nf_to_tme_with_equality_predicate(M, M1) :-
	m_add_equality(M, M2),
	m_equality_to_predicate(M2, M3),
	nf_to_tme(M3, M1).

map_nf_clause_to_tme([X|Xs], [X1|Xs1]) :-
	nf_clause_to_tme(X, X1),
	map_nf_clause_to_tme(Xs, Xs1).
map_nf_clause_to_tme([], []).

nf_clause_to_tme(Lits, TME) :-
	split_pn(Lits, PLits, NLits),
	syntactic_list_to_litdisj(PLits, Head),
	( NLits = [] ->
	  TME = Head
	; syntactic_list_to_litconj(NLits, Body),
          TME = (Head :- Body)
	).

split_pn([~(A)|Ls], P, [A|N]) :-
	!,
	split_pn(Ls, P, N).
split_pn([A|Ls], [A|P], N) :-
	split_pn(Ls, P, N).
split_pn([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_equality_to_predicate(M, M1) :-
	map_c_equality_to_predicate(M, M1).

map_c_equality_to_predicate([X|Xs], [X1|Xs1]) :-
	map_l_equality_to_predicate(X, X1),
	map_c_equality_to_predicate(Xs, Xs1).
map_c_equality_to_predicate([], []).

map_l_equality_to_predicate([X|Xs], [X1|Xs1]) :-
	l_equality_to_predicate(X, X1),
	map_l_equality_to_predicate(Xs, Xs1).
map_l_equality_to_predicate([], []).

l_equality_to_predicate(X=Y, equal(X,Y)) :- !.
l_equality_to_predicate(~(X=Y), ~(equal(X,Y))) :- !.
l_equality_to_predicate(L, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% An Example that does not work with negative_literals set to true
%% (seem that simulation of complement splitting by hypertableau
%% would need to be added)
%% 
%% hyper_prove([[p,q],[~p,~q],[~p,q],[p,~q]], [], X)
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

syntactic_list_to_litconj([], true).
syntactic_list_to_litconj([L], L) :-
	!.
syntactic_list_to_litconj([L|Ls], (L,F)) :-
	syntactic_list_to_litconj(Ls, F).

syntactic_list_to_litdisj([], false).
syntactic_list_to_litdisj([L], L) :-
	!.
syntactic_list_to_litdisj([L|Ls], (L;F)) :-
	syntactic_list_to_litdisj(Ls, F).

