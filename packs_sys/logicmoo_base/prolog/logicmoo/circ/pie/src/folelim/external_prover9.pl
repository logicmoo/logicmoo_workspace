%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016, 2019 Christoph Wernhard
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

:- module(external_prover9, [call_prover9/3,
			     call_mace4/3,
			     fol_status_prover9/3
			    ]).

:- use_module(elim_fol).
:- use_module(prooftrans).
:- use_module(toytools(config)).
:- use_module(nf(nf)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(info)).
:- use_module(swilib(err)).
:- use_module(swilib(tempfiles)).

fol_status_prover9(F, Options, Status) :-
	( memberchk( mace=false, Options ) ->
	  Mace4Status = not_run
	; call_mace4(F, Options, Mace4Status)
	),
	( Mace4Status = 0 ->
	  Status = satisfiable
	; call_prover9(F, Options, Prover9Status),
	  Prover9Status = 0 ->
	  Status = unsatisfiable
	; Status = unknown
	).

call_prover9(F, Options, Status) :-
	( memberchk(input_file=In, Options) -> true
	; tempfile(prover9, input, In)
	),
	( memberchk(output_file=Out, Options) -> true
	; tempfile(prover9, output, Out)
	),
	( memberchk(error_file=Err, Options) -> true
	; tempfile(prover9, error, Err)
	),
	tempfile(prover9, prooftrans, PTFile),
	get_conf(prover9_cmd, Prover9),
	( memberchk(prooftrans_options=PTOptions, Options) ->
	  true
	; PTOptions = 'renumber'
	),
	PT=prooftrans,
	write_prover9_input(F, prover9, Options) onto_file In,
	format(atom(Cmd), '~w <~w >~w 2>>~w', [Prover9, In, Out, Err]),
	info(10, 'Calling ~w', [Cmd]),
	catch(( shell(Cmd, Status),
		(( Status=0, memberchk( process_output=Post, Options ) ->
		   Post =.. [Pred, Arg],
		   once(call(Pred, Out, Arg))
		 ; true
		 ),
		 ( Status=0, memberchk(proof=Proof, Options) ->
		   format(atom(PTCmd), '~w xml ~w <~w >~w 2>>~w',
			  [PT, PTOptions, Out, PTFile, Err]),
		   info(10, 'Calling ~w', [PTCmd]),
		   shell(PTCmd, PTStatus),
		   ( PTStatus = 0 ->
		     prover9_proofs(PTFile, Proof)
		   ; err('prooftrans failure')
		   )
		 ; true
		 )),
		cleanup(Options)),
	      E,
	      (cleanup(Options), throw(E))).

call_mace4(F, Options, Status) :-
	tempfile(mace4, input, In),
	tempfile(mace4, output, Out),
	tempfile(mace4, error, Err),
	tempfile(mace4, cmodel, MFile),
	get_conf(mace4_cmd, Mace4),
	write_prover9_input(F, mace4, Options) onto_file In,
	format(atom(Cmd), '~w -c <~w >~w 2>>~w', [Mace4, In, Out, Err]),
	info(10, 'Calling ~w', [Cmd]),
	catch((shell(Cmd, Status),
	       ( Status = 0, member(cmodel=Model, Options) ->
		 format(atom(MCmd), 'interpformat standard <~w >~w 2>>~w',
			[Out, MFile, Err]),
		 info(10, 'Calling ~w', [MCmd]),
		 shell(MCmd, MStatus),
		 ( MStatus = 0 ->
		   read_file_to_terms(MFile, Model, [])
		 ; err('mace4 model conversion failure')
		 )
	       ; true
	       ),
	       cleanup(Options)),
	      E,
	      (cleanup(Options), throw(E))).

cleanup(Options) :-
	memberchk(cleanup=false, Options),
	!.
cleanup(_) :-
	delete_tempfiles(prover9),
	delete_tempfiles(mace4).

write_prover9_input(F, Mode, Options) :-
	format('set(prolog_style_variables).~n', []),
	( Mode = mace4 ->
	  ( memberchk(mace4_settings=Settings, Options) -> true
	  ; Settings = [assign(iterate_up_to, 5)]
	  ),
	  ( member(Setting, Settings),
	    format('~w.~n', [Setting]),
	    fail
	  ; true
	  ),
	  ( memberchk(mace4_timeout=Seconds, Options) -> true
	  ; memberchk(timeout=Seconds, Options) -> true
	  ; get_conf(mace4_timeout, Seconds)
	  )
	; ( memberchk(prover9_settings=Settings, Options) -> true
	  ; Settings = [clear(auto_denials)]
	  ),
	  ( member(Setting, Settings),
	    format('~w.~n', [Setting]),
	    fail
	  ; true
	  ),
	  ( memberchk(prover9_timeout=Seconds, Options) -> true
	  ; memberchk(timeout=Seconds, Options) -> true
	  ; get_conf(prover9_timeout, Seconds)
	  )
	),
	format('assign(max_seconds, ~w).~n', [Seconds]),
	format('formulas(sos).~n'),
	( memberchk(left_right=(Left-Right), Options) ->
	  ( F = ~(A->B) -> true
	  ; err('left_right option not compatible with formula shape: ~q', [F])
	  ),
	  %% Return lists of assumptions from left and right side of
	  %% an implication, for application in interpolation
	  findall(A1, enum_toplevel_conjuncts(A, A1), Left),
	  findall(B1, enum_toplevel_conjuncts(~B, B1), Right),
	  format('% Left assumptions~n'),
	  ( member(G, Left), formott(G), fail ; true ),
	  format('% Right assumptions~n'),
	  ( member(G, Right), formott(G), fail ; true )
	; ( enum_toplevel_conjuncts(F, G),
	    formott(G),
	    fail
	  ; true
	  )
	),
	format('end_of_list.~n').

enum_toplevel_conjuncts((F,G), H) :-
	!,
	( enum_toplevel_conjuncts(F, H)
	; enum_toplevel_conjuncts(G, H)
	).
enum_toplevel_conjuncts(~(~(F)), H) :-
	!,
	enum_toplevel_conjuncts(F, H).
enum_toplevel_conjuncts(~(F;G), H) :-
	!,
	enum_toplevel_conjuncts((~F,~G), H).
enum_toplevel_conjuncts(~(F->G), H) :-
	!,
	enum_toplevel_conjuncts((F,~G), H).
enum_toplevel_conjuncts(~(F<-G), H) :-
	!,
	enum_toplevel_conjuncts((~F,G), H).
enum_toplevel_conjuncts(true, _) :-
	!,
	fail.
enum_toplevel_conjuncts(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



