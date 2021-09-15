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

:- module(external, [external_solve_form/2,
		     external_solve_matrix/2,

		     set_tmpfile_number/1,

		     %% The "dry" predicates just write out solver inputs,
		     %% without calling solvers
		     dry_rnd_varelim_task/4,
		     dry_varelim_task/6]).

:- use_module(nf(nf)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(options)).
:- use_module(swilib(sysdep)).
:- use_module(config).
:- use_module(auxiliary).
:- use_module(dimacsio).

tmpfile(Key, Id, FileName) :-
	tmpfile('te_', Key, Id, FileName).

tmpfile(Prefix, Key, Id, FileName) :-
	get_conf(tmpdir, Dir),
	format(atom(FileName), '~w/~w~w~w.txt', [Dir, Prefix, Id, Key]).

:- flag(tmp_file_nr, _, 1).

set_tmpfile_number(N) :-
	flag(tmp_file_nr, _, N).


solver(Id, CallPrefix) :-
	get_conf(external_solvers, Solvers),
	memberchk(Id-CallPrefix, Solvers).

external_solve_form(F, Options) :-
	info(70, 'Converting to cnf'),
	( from_options(direct_cnf-true, Options, false) ->
	  F1 = F,
	  cnf_prop(F1, M)
	; from_options(cnf_trafo-CNFM, Options, def) ->
	  ( CNFM = def ->
	    df(F, D, L),
	    F1 = (L, D),
	    cnf_prop_nosimp(F1, M)
	  ; CNFM = simp ->
	    F1 = F,
	    cnf_prop(F1, M)
	  ; CNFM = nosimp ->
	    F1 = F,
	    cnf_prop_nosimp(F1, M)
	  ; err('Unknown propositional CNF trafo: ~q', [CNFM])
	  )
	),
	info(70, 'Solving the cnf'),
	external_solve_matrix(M, Options).

%%%% 
%%%% QDimacs Note: for compatibility with the qdimacs version 1.1 some
%%%% constraints on the prefix must hold. These are not ensured
%%%% here. The predicate cleanup_quants in toyproject.pl can be used
%%%% to convert prefixes appropriately.
%%%% 
external_solve_matrix(M, Options) :-
	from_options(qprefix-QPrefix, Options, []),
	from_options(white-White, Options, []),
	( QPrefix = [] ->
	  ( White = [] -> MODE = sat ; MODE = elim )
	; MODE = qbf
	),
	( MODE = sat ->
	  get_conf(default_external_sat_solver, DefaultSolver)
	; MODE = qbf ->
	  get_conf(default_external_qbf_solver, DefaultSolver)
	; MODE = elim ->
	  get_conf(default_external_elim_solver, DefaultSolver)
	),
	from_options(solver-SolverId, Options, DefaultSolver),
        ( get_conf(keep_tmpfiles, true) ->
	  flag_inc(tmp_file_nr, InFileNr),
	  format(atom(Id), '~48t~d~5+_', [InFileNr])
	; Id = ''
	),
	tmpfile(input_file, Id, InFile),
	tmpfile(output_file, Id, OutFile),
	tmpfile(black_file, Id, BlackFile),
	tmpfile(white_file, Id, WhiteFile),
	tmpfile(trafo_file, Id, TrafoFile),
	tmpfile(models_file, Id, ModsFile),
	tmpfile(err_file, Id, ErrFile),
	from_options(timeout-Timeout, Options, 10),
	from_options(solution-Solution, Options, _),
	from_options(models-Models, Options, _),
	from_options(out_cnf-OutCNF, Options, _),
	from_options(out_dnf-OutDNF, Options, _),
	from_options(black-Black, Options, []),
	info(70, 'Writing dimacs file'),
	( QPrefix = [] ->
	  write_dimacs_file(InFile, M, ST)
	; write_qdimacs_file(InFile, QPrefix, M, ST)
	),
	solver(SolverId, Solver),
	info(55, 'Calling external solver ~q', [SolverId]),
	( memberchk(SolverId, [minisat, minisat2, minisat2(_), qbf, qbf(_)]) ->
	  ( memberchk(SolverId, [minisat, minisat2, minisat2(_)]) ->
	    format(atom(Call), 'ulimit -t ~w ; ~w ~w ~w >~w',
		   [Timeout, Solver, InFile, ModsFile, OutFile])
	  ;  SolverId = qbf(_) ->    
	    format(atom(Call), 'ulimit -t ~w ; ~w ~w >~w',
		   [Timeout, Solver, InFile, OutFile])
	  ),
	  info(60, 'External solver call: ~w', [Call]),
	  shell(Call, Result),
	  info(60, 'External solver status ~w', [Result]),
	  ( Result = 10 ->
	    Solution = satisfiable,
	    ( \+ is_specified_option(models, Options) ->
	      true
	    ; info(70, 'Reading minisat models file ~w', [ModsFile]),
	      symbol_table_invert(ST, ST1),
	      read_minisat_models_file(ModsFile, ST1, Models)
	    )
	  ; Result = 20 ->
	    Models = [],
	    Solution = unsatisfiable
	  ; Solution = unknown,
	    Models = []
	  )
	; SolverId = riss(_) ->
	  format(atom(Call), 'ulimit -t ~w ; ~w ~w ~w &>~w',
		   [Timeout, Solver, InFile, ModsFile, OutFile]),
	  info(60, 'External solver call: ~w', [Call]),
	  shell(Call, Result),
	  info(60, 'External solver status ~w', [Result]),
	  ( Result = 10 ->
            Solution = satisfiable,
	    ( ModsFile = '' ->
              true
	    ; info(70, 'Reading satcomp output file ~w', [ModsFile]),
	      symbol_table_invert(ST, ST1),
	      read_satcomp_output_file(ModsFile, ST1, Models)
	    )
	  ; Result = 20 ->
	    Models = [],
	    Solution = unsatisfiable
	  ; Solution = unknown,
	    Models = []
	  )
	; SolverId = riss_elim(_) ->
	  write_blackwhite_list(Black, ST, BlackFile),
	  write_blackwhite_list(White, ST, WhiteFile),
	  RISS_ELIM_OPTIONS = '-CP_ee=1 -CP_er=0 -CP_print=1 -CP_pure=0 -CP_bva=0',
%	  RISS_ELIM_OPTIONS = '-CP_ee=0 -CP_er=0 -CP_print=1 -CP_pure=0 -CP_bve=1 -CP_bce=1 -CP_bva=0',
	  format(atom(Call),
		 'ulimit ~w ; ~w ~w ~w -CP_whiteFile=~w -CP_blackFile=~w 2>~w >~w',
		 [Timeout, Solver, InFile, RISS_ELIM_OPTIONS,
		  WhiteFile, BlackFile, ErrFile, TrafoFile]),
	  info(60, 'External solver call: ~w', [Call]),
	  shell(Call, Result),
	  info(60, 'External solver status ~w', [Result]),
	  ( (Result = 0 ; Result = 12 ; Result = 10) ->
	    info(70, 'Reading dimacs file ~w', [TrafoFile]),
	    symbol_table_invert(ST, ST1),
	    read_dimacs_file(TrafoFile, ST1, OutCNF)
	  ; Result = 20 ->
	    OutCNF = [[]]
	  ; true
	  )
	; SolverId = dnf_compiler(_) ->
	  write_blackwhite_list(White, ST, WhiteFile),
	  format(atom(Call),
		 'ulimit ~w ; ~w ~w ~w ~w',
		 [Timeout, Solver, InFile, WhiteFile, TrafoFile]),
	  info(60, 'External solver call: ~w', [Call]),
	  shell(Call, Result),
	  info(60, 'External solver status ~w', [Result]),
	  ( Result = 0 ->
	    info(70, 'Reading dnf file ~w', [TrafoFile]),
	    symbol_table_invert(ST, ST1),
	    read_dimacs_file(TrafoFile, ST1, OutDNF)
	  ; Result = 20 ->
	    OutDNF = []
	  ; true
	  )
	; err('Undefined solver ~q', [SolverId])
	).

write_blackwhite_list(Atoms, ST, File) :-
	onto_file(( member(A, Atoms),
		    symbol_table_get(ST, A, V),
		    write(V),
		    nl,
		    fail
		  ; true ),
		  File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


dry_rnd_varelim_task(File, Prefix, Id, BlackPercentage) :-
	read_dimacs_file(File, ST, M),
	m_prop_signature(M, Vars),
	length(Vars, N),
	N1 is round(N * BlackPercentage/100),
	info(0, 'Eliminating ~w of ~w', [N1, N]),
	set_random(seed(10)),
	sort_random(Vars, Vars1),
	first_n(N1, Vars1, Black),
	sort(Black, Black1),
	sort(Vars, Vars2),
	ord_subtract(Vars2, Black1, White),
	symbol_table_invert(ST, ST1),
	dry_varelim_task(Prefix, M, White, Black1, Id, ST1).

first_n(0, _, []).
first_n(N, [X|Xs], [X|Xs1]) :-
	N > 0,
	N1 is N -1,
	first_n(N1, Xs, Xs1).
	
sort_random(Xs, Ys) :-
	sort_by_predicate(Xs, rnd, Ys).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

rnd(_, K) :-
	K is random(32000).

sort_by_predicate(Xs, P, Ys) :-
	map_add_key(Xs, P, Ys1),
	keysort(Ys1, Zs),
	map_val(Zs, Ys).

map_add_key([X|Xs], P, [K-X|Xs1]) :-
	Call =.. [P,X,K],
	call(Call),
	map_add_key(Xs, P, Xs1).
map_add_key([], _, []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

projectile_converter_bin('/Users/ch/te/auxfiles/convert-for-projectile.sh').

dry_varelim_task(Prefix, CNF, WhiteList, BlackList, Id, ST) :-
	( var(Id),
	  get_conf(keep_tmpfiles, true) ->
	  flag_inc(tmp_file_nr, InFileNr),
	  format(atom(Id), '~48t~d~5+_', [InFileNr])
	; var(Id) ->
	  Id = ''
	; true
	),
	tmpfile(Prefix, input, Id, InFile),
	tmpfile(Prefix, black, Id, BlackFile),
	tmpfile(Prefix, white, Id, WhiteFile),
	tmpfile(Prefix, projectile, Id, ProjectileFile),
	info(0, 'Writing files like ~w', [InFile]),
	write_dimacs_file(InFile, CNF, ST),
	write_blackwhite_list(BlackList, ST, BlackFile),
	write_blackwhite_list(WhiteList, ST, WhiteFile),
	projectile_converter_bin(ProjectileConverter),
	format(atom(Cmd), '~w ~w ~w >~q',
	       [ProjectileConverter, InFile, WhiteFile,	ProjectileFile]),
	shell(Cmd, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


