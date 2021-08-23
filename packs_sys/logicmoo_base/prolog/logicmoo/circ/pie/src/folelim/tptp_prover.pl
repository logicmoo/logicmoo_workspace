%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(tptp_prover,
	  [is_valid_tptpstmts/3,
	   is_valid_fol_tptp/1,
	   is_valid_fol_tptp/2]).

:- use_module(tptpio).
:- use_module(swilib(info)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(options)).
:- use_module(swilib(sysdep)).
:- use_module(swilib(tempfiles)).

tptpstmts_to_format(Stmts, Format, Extra, OutFile) :-
	tempfile(tptp_prover, '1', TmpFile),
	onto_file(( member(Stmt, Stmts),
		    pp_tptp(Stmt),
		    format('.~n~n'),
		    fail
		  ; true
		  ),
		  TmpFile),
	format(atom(ConvertCall),
	       'tptp2X -q2 -d - ~w -f ~w ~w >~w',
	       [Extra, Format, TmpFile, OutFile]),
	info(40, 'Calling ~w', [ConvertCall]),
	shell(ConvertCall, _),
	info(40, 'Written ~w', [OutFile]).

is_valid_tptpstmts(Stmts, Prover, Options) :-
	from_options(timeout=TimeOut, Options, 10),
	tempfile(tptp_prover, '2', InFile),
	tptp_prover_call(Prover, Options, InFile, TimeOut, Format, Extra, Call),
	tptpstmts_to_format(Stmts, Format, Extra, InFile),
	catch(( Call = shell(Call1, StatusForValid) ->
		info(40, 'Calling prover: ~w', [Call1]),
		shell(Call1, Status),
		( Status == StatusForValid ->
		  cleanup(Options)
		; cleanup(Options),
		  fail
		)
	      ; ( call(Call) ->
		  cleanup(Options)
		; cleanup(Options),
		  fail
		)),
	      E,
	      ( cleanup(Options),
		throw(E)
	      )).

cleanup(Options) :-
	from_options(cleanup=Cleanup, Options, true),
	( Cleanup=true ->
	  delete_tempfiles(tptp_prover)
	; true
	).
	
tptp_prover_call(eprover, Options, InFile, TimeOut, tptp, '', shell(Call,0)) :-
	!,
	( memberchk(eprover_options=ProverOptions, Options) ->
	  true
	; ProverOptions='' ),
	tempfile(tptp_prover, '3', OutFile),
	format(atom(Call),
	       'eprover --tstp-format --soft-cpu-limit=~w ~w ~w -o ~w',
	       [TimeOut, ProverOptions, InFile, OutFile]).
tptp_prover_call(vampire, Options, InFile, TimeOut, tptp, '', shell(Call,0)) :-
	!,
	( memberchk(vampire_options=ProverOptions, Options) ->
	  true
	; ProverOptions='' ),
	tempfile(tptp_prover, '3', OutFile),
	format(atom(Call),
	       'vampire --input_syntax tptp --time_limit ~w \c
	        ~w ~w > ~w ; grep -q "^% SZS status Theorem" ~w',
	       [TimeOut, ProverOptions, InFile, OutFile, OutFile]).
tptp_prover_call(krhmg, _, _, protein, '-t clausify:otter', fail) :-
	!.
tptp_prover_call(X, _, _, X, '', fail) :-
	!.

is_valid_fol_tptp(F) :-
	is_valid_fol_tptp(F, []).
is_valid_fol_tptp(F, Options) :-
	flag(tptp_gen_id, _, 1),
	( ( F = (A -> C) ; F = (C <- A) ) ->
	  conj_to_list(A, Axioms),
	  map_mk_tptp_axiom(Axioms, Axioms1),
	  mk_tptp_stmt(C, conjecture, Conjecture),
	  append(Axioms1, [Conjecture], TPTPStmts)
	; mk_tptp_stmt(F, conjecture, Conjecture),
	  TPTPStmts = [Conjecture]
	),
	from_options(prover=Prover, Options, eprover),
	is_valid_tptpstmts(TPTPStmts, Prover, Options).

map_mk_tptp_axiom([X|Xs], [X1|Xs1]) :-
	mk_tptp_stmt(X, axiom, X1),
	map_mk_tptp_axiom(Xs, Xs1).
map_mk_tptp_axiom([], []).

mk_tptp_stmt(F, Kind, fof(Label, Kind, F1)) :-
	flag_inc(tptp_gen_id, N),
	concat_atom(['f', N], Label),
	nf_to_tptp(F, F1).

conj_to_list((X,Y), [X|Ys]) :-
	!,
	conj_to_list(Y, Ys).
conj_to_list(X, [X]).
