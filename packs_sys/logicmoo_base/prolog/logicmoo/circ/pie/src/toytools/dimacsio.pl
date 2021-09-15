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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Handling of dimacs files.
%%%%
%%%% History:
%%%% Partially after scripts/dimacs.pl, derived from sat/satio.pl
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(dimacsio, [read_dimacs_file/2,
		     write_dimacs_file/2,
		     write_qdimacs_file/3,
		     read_dimacs_file/3,
		     write_dimacs_file/3,
		     write_qdimacs_file/4,
		     read_satcomp_output_file/2,
		     read_satcomp_output_file/3,
		     read_minisat_models_file/2,
		     read_minisat_models_file/3,
		     symbol_table_invert/2,
		     symbol_table_to_kv_list/2,
		     symbol_table_get/3,
		     kv_list_to_symbol_table/2]).

:- use_module(swilib(fromonto)).
:- use_module(swilib(hash)).
:- use_module(swilib(sysdep)).

read_dimacs_file(Filename, Matrix) :-
	read_dimacs_file(Filename, _SymbolTable, Matrix).

%%%% 
%%%% read_dimacs_file(+Filename, +SymbolTable, -Matrix)
%%%% read_dimacs_file(+Filename, -SymbolTable, -Matrix)
%%%%
%%%% Read-in Dimacs file, converting the number representation of
%%%% propositional variables into terms according to SymbolTable. Numbers not
%%%% present in the table are converted to terms of the form gen(_)
%%%% and added to SymbolTable. If SymbolTable is a variable, it will be
%%%% bound to a newly constructed symbol table.
%%%%
%%%% SymbolTable is as obtained by write_dimacs_file/3, and subjected to
%%%% symbol_table_invert/2.
%%%%
read_dimacs_file(Filename, SymbolTable, Matrix) :-
	( var(SymbolTable) ->
	  mk_symbol_table(100, SymbolTable)
	; true
	),
	flag(gen_dimacsio_atom, _, 1),
	from_file(( current_input(In),
		    convert_cnf(In, SymbolTable, Matrix) ),
		  Filename).

symbol_table_get(ST, K, V) :-
	ht_get(ST, K, V).

kv_list_to_symbol_table(KVs, ST) :-
	length(KVs, L),
	Cap is max(100, L),
	mk_ht(Cap, ST),
	( member(K-V, KVs),
	  ht_put(ST, K, V),
	  fail
	; true
	).

symbol_table_to_kv_list(ST, KVs) :-
	findall(K-V, (ht_key(ST, K), ht_get(ST, K, V)), KVs1),
	keysort(KVs1, KVs).


%%%% 
%%%% symbol_table_invert(+SymbolTable1, -SymbolTable2)
%%%% 
%%%% SymbolTable2 is a new symbol table with the same content as SymbolTable1
%%%% but keys and values flipped.
%%%%
symbol_table_invert(ST, ST1) :-
	ht_capacity(ST, Cap),
	mk_ht(Cap, ST1),
	( ht_key(ST, K),
	  ht_get(ST, K, V),
	  ht_put(ST1, V, K),
	  fail
	; true
	).

symbol_table_lookup_num(T, N, A) :-
	( ht_get(T, N, A) ->
	  true
	; flag_inc(gen_dimacsio_atom, GN),
	  A = gen(GN),
	  ht_put(T, N, A)
	).

mk_symbol_table(Capacity, T) :- 
	mk_ht(Capacity, T).

fill_symbol_table(T, Matrix, Size) :-
	flag(fill_symbol_table, _, 1),
	( member(C, Matrix),
	  member(L, C),
	  lit_atom(L, A),
	  ( ht_get(T, A, _) ->
	    true
	  ; flag_inc(fill_symbol_table, N),
	    ht_put(T, A, N)
	  ),
	  fail
	; flag(fill_symbol_table, N, N),
	  Size is N - 1
	).

write_dimacs_file(Filename, Matrix) :-
	write_dimacs_file(Filename, Matrix, _).

write_dimacs_file(Filename, Matrix, SymbolTable) :-
	write_qdimacs_file(Filename, [], Matrix, SymbolTable).

write_qdimacs_file(Filename, QuantifierPrefix, Matrix) :-
	write_qdimacs_file(Filename, QuantifierPrefix, Matrix, _).

%%%% 
%%%% write_qdimacs_file(+Filename, +QuantifierPrefix, +Matrix, -SymbolTable
%%%% write_qdimacs_file(+Filename, +QuantifierPrefix, +Matrix, +SymbolTable)
%%%% 
%%%% Write Matrix as Dimacs file.
%%%%
%%%% If SymbolTable is a variable, it will be bound to a representation
%%%% of the applied mapping from the representation of propositional
%%%% variables as terms (in Matrix) to numbers (in the Dimacs file). It can,
%%%% after submission to symbol_table_invert/2, be used as imput to
%%%% read_dimacs_file/3.
%%%%
%%%% SymbolTable can also be bound to a symbol table which then must
%%%% provide a value for each propositional variable in Matrix and
%%%% QuantifierPrefix.
%%%%
write_qdimacs_file(Filename, QPrefix, Matrix, SymbolTable) :-
	length(Matrix, NClauses),
	( var(SymbolTable) ->
	  Capacity is 1 + NClauses // 4,
	  mk_symbol_table(Capacity, SymbolTable),
	  fill_symbol_table(SymbolTable, Matrix, NVars)
	; %% just to compute NVars:
	  Capacity is 1 + NClauses // 4,
	  mk_symbol_table(Capacity, SymbolTable1),
	  fill_symbol_table(SymbolTable1, Matrix, NVars)
	),
	onto_file(( format('p cnf ~d ~d~n', [NVars, NClauses]),
		    ( member(Q, QPrefix),
	              ( Q = all(QVs) ->
		        format('a ', [])
		      ; Q = ex(QVs) ->
			format('e ', [])
		      ),
	              ( member(QV, QVs),
			lit_to_dimacs(SymbolTable, QV, QV1),
		        format('~d ', [QV1]),
			fail
		      ; format('0~n', [])
		      ),
	              fail		
		    ; true
		    ),
		    ( member(C, Matrix),
	              ( member(L, C),
			lit_to_dimacs(SymbolTable, L, L1),
		        format('~d ', [L1]),
			fail
		      ; format('0~n', [])
		      ),
		      fail
		    ; true
		    )),
		  Filename).

lit_to_dimacs(SymbolTable, L, N) :-
	( L = ~(A) ->
	  ht_get(SymbolTable, A, N1),
	  N is -N1  
	; ht_get(SymbolTable, L, N)
	).

lit_atom(~(X), X) :-
	!.
lit_atom(X, X) :-
	\+ X = ~(_).

convert_cnf(In, SymbolTable, Cs) :-
	( at_end_of_stream(In) ->
	  Cs = []
	; read_line_to_codes(In, Line),
%	  atom_codes(ALine, Line),
%	  writeln(line(ALine)),
	  ( Line = [] ->
	    print_message(warning, format('Skipping empty line', [])),
	    Cs = Cs1
	  ;
	    ( Line = [0' |Line1] ->
              %% this occurs in some satlib files  		
	      true
	    ; Line1 = Line
	    ),
	    ( phrase(line(Line2), Line1, Rest) ->
	      ( Line2 == comment ->
	        Cs = Cs1
	      ; Line2 == problem_line ->
	        Cs = Cs1
	      ; Line2 = status(Status) ->
		( Status = sat ->
		  Cs = Cs1
		; Cs = []
		)
	      ; Line2 = minisat_result_status(Status) ->
		( Status = sat ->
		  Cs = Cs1
		; Cs = []
		)
	      ; Line2 = value(Lits), Rest == [] ->
	        convert_clause(Lits, SymbolTable, Clause),
		Cs = [Clause|Cs1]
	      ; Line2 = clause(Lits), Rest == [] ->
	        convert_clause(Lits, SymbolTable, Clause),
                Cs = [Clause|Cs1]
	      ; atom_codes(Info, Line1),
		throw(dimacs_parse_error(Info))
	      )
	    ; atom_codes(Info, Line1),
	      throw(dimacs_parse_error(Info))
	    )
	  ),
	  convert_cnf(In, SymbolTable, Cs1)
	).

convert_clause(X, SymbolTable, Y) :-
	cc(X, SymbolTable, Y).
	
cc([L|Ls], T, [A|Ls1]) :-
	L > 0,
	!,
	symbol_table_lookup_num(T, L, A),
	cc(Ls, T, Ls1).
cc([L|Ls], T, [~(A)|Ls1]) :-
	L < 0,
	!,
	L1 is L * -1,
	symbol_table_lookup_num(T, L1, A),
	cc(Ls, T, Ls1).
cc([], _, []).

line(comment) -->
	comment.
line(problem_line) -->
	problem_line.
line(clause(C)) -->
	cnf_clause(C).
line(status(S)) -->
	status_line(S).
line(value(V)) -->
	value_line(V).

line(minisat_result_status(S)) -->
	minisat_result_status_line(S).

comment -->
	[0'c].

problem_line -->
	[0'p].

status_line(sat) -->
	[0's, 0'  ,0'S, 0'A, 0'T, 0'I, 0'S, 0'F, 0'I, 0'A, 0'B, 0'L, 0'E].
status_line(unsat) -->
	[0's, 0'  ,0'U, 0'N, 0'S, 0'A, 0'T, 0'I, 0'S, 0'F, 0'I, 0'A,
	 0'B, 0'L, 0'E].
status_line(unknown) -->
	[0's, 0'  ,0'U, 0'N, 0'K, 0'N, 0'O, 0'W, 0'N].

minisat_result_status_line(sat) -->
	[0'S, 0'A, 0'T].
minisat_result_status_line(unsat) -->
	[0'U, 0'N, 0'S, 0'A, 0'T].
minisat_result_status_line(unknown) -->
	[0'I, 0'N, 0'D, 0'E, 0'T].

value_line(V) -->
	[0'v, 0' ],
	cnf_clause(V).

cnf_clause([]) -->
	space,
	[0'0],
	space,
	!.
cnf_clause([I|Is]) -->
	integer(I),
	space,
	cnf_clause(Is).

integer(I) -->
        digit(D0),
        digits(D),
        { number_codes(I, [D0|D])
        }.
integer(I) -->
        [0'-],
        digits(D),
        { number_codes(I, [0'-|D])
        }.

digits([D|T]) -->
        digit(D),
	!,
        digits(T).
digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.

space -->
        [C],
        { code_type(C, space)
        },
	!,
        space.
space -->
        [].

read_minisat_models_file(Filename, Models) :-
	read_dimacs_file(Filename, Models).

%%%%
%%%% read_models_file(+Filename, +SymbolTable, -Models)
%%%% read_satcomp_output_file(+Filename, +SymbolTable, -Models)
%%%%
%%%% Returns a list of models. This list is empty if the formula
%%%% does not have status SATISFIABLE.
%%%% 
read_minisat_models_file(Filename, SymbolTable, Models) :-
	read_dimacs_file(Filename, SymbolTable, Models).
	

read_satcomp_output_file(Filename, Models) :-
	read_dimacs_file(Filename, ValueLines),
	( ValueLines = [] ->
	  Models = []
	; apply_append(ValueLines, Model),
	  Models = [Model]
	).

read_satcomp_output_file(Filename, SymbolTable, Models) :-
	read_dimacs_file(Filename, SymbolTable, ValueLines),
	( ValueLines = [] ->
	  Models = []
	; apply_append(ValueLines, Model),
	  Models = [Model]
	).

apply_append([L], L) :-
	!.
apply_append([L|Ls], L1) :-
	append(L, Ls1, L1),
	apply_append(Ls, Ls1).
apply_append([], []).


% convert_file(Filename) :-
% 	from_file(convert_cnf, Filename).
% 
% convert_argv :-
% 	current_prolog_flag(argv, Argv), 
% 	append(_, [--|Args], Argv),
% 	!,
% 	concat_atom(Args, ' ', SingleArg),  
% 	convert_file(SingleArg).
% 
% main :-
%         catch(convert_argv,
% 	      E,
% 	      (print_message(error, format('~q', [E])),
% 	       fail)),
%         halt.                                           
% main :-
%         halt(1).
