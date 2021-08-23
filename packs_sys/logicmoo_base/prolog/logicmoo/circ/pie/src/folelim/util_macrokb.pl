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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% User utilities for "macrokb" documents
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(util_macrokb,
	  [ppl/0,
	   ppl/1,
	   ppl_set_source/0,
	   ppl_set_options/2,
	   ppl_override_options/2,
	   ppl_set_options/1,
	   ppl_override_options/1,
	   ppl_form/1,
	   ppl_form/2,
	   ppl_elim/1,
	   ppl_elim/2,
	   ppl_ipol/1,
	   ppl_ipol/2,
	   ppl_valid/1,
	   ppl_valid/2,
	   ppl_set_verbosity/1,
	   last_ppl_result/1,
	   tptp_export/3,
	   mac_expand/1,
	   mac_expand_1/1,
	   prettify_form/3]).

:- use_module(folelim(external_prover9)).
:- use_module(folelim(tptpio)).
:- use_module(folelim(util_macrokb)).
:- use_module(folelim(macrokb)).
:- use_module(pplatex(pplatex)).
:- use_module(folelim(ppl_macrokb)).
:- use_module(folelim(elim_fol)).
:- use_module(folelim(prep_fol)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(tptp_prover)).
:- use_module(folelim(utils_fol)).
:- use_module(folelim(simp_fol)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(fromonto)).
:- use_module(folelim(craigtask_cm)).
:- use_module(folelim(craigtask_hyper)).
:- use_module(folelim(prooftask_cm)).
:- use_module(folelim(prettysymbols_fol)).

tmp_dir('/tmp').
tmp_latex_file('tmp_ppl.tex').

% [ip_dotgraph='/Users/ch/tmp/tmp_ip.gif']

:- dynamic last_source/1.
:- dynamic ppl_verbosity/1.

:- dynamic last_ppl_result/1.

set_last_ppl_result(X, Options) :-
	( memberchk(r=Result, Options) -> X=Result ; true ),
	retractall( last_ppl_result( _ ) ),
	assert( last_ppl_result(X) ).

ppl_set_verbosity(V) :-
	retractall( ppl_verbosity(_) ),
	assert( ppl_verbosity(V) ).

:- initialization(ppl_set_verbosity(5)).

ppl :-
	ppl([]).
ppl(Options) :-
	ppl_effective_options(ppl, Options, Options1),
	info(50, 'Effective ppl options: ~q', [Options1]),
	( last_source(ThisFile) ->
	  true
	; err('No last_source for ppl/0 specified')
	),
	tmp_dir(Outdir),
	tmp_latex_file(Outfile),
	format(atom(Out), '~w/~w', [Outdir, Outfile]),
	get_info_verbosity(V),
	ppl_verbosity(V1),
	catch(( set_info_verbosity(V1),
		ppl_file(ThisFile, Options1, Out) ),
	      E,
	      ( set_info_verbosity(V),
		ppl_debug_info(Info),
		info(0, 'PIE processing context: ~w', [Info]),
		throw(E) )),
	set_info_verbosity(V),
	info(10, 'Written ~w', [Out]),
	( memberchk(latex_processing=PROC, Options1) ->
	  process_latex(PROC, Outdir, Outfile, ThisFile, Options1),
	  info(10, 'Generated LaTeX file is ~w', [Out])
	; true
	).

process_latex(P) :-
	atom(P),
	!,
	process_latex([P]).

process_latex([pdflatex|Ps], Outdir, Outfile, Document, Options) :-
	!,
	info(10, 'Invoking pdflatex ~w', [Outfile]),
	format(atom(Cmd),
	       'cd ~w ; pdflatex -interaction=batchmode ~w',
	       [Outdir, Outfile]),
	( shell(Cmd) ->
	  process_latex(Ps, Outdir, Outfile, Document, Options)
	; info(10, 'Error at pdflatex processing of ~w/~w', [Outdir, Outfile])
	).
process_latex([bibtex|Ps], Outdir, Outfile, Document, Options) :-
	!,
	( atom_concat(Outfile1, '.tex', Outfile) ->
	  true
	; Outfile1 = Outfile
	),
	info(10, 'Invoking bibtex ~w', [Outfile1]),
	format(atom(Cmd),
	       'cd ~w ; bibtex ~w',
	       [Outdir, Outfile1]),
	( shell(Cmd) ->
	  process_latex(Ps, Outdir, Outfile, Document, Options)
	; info(10, 'Error at bibtex processing of ~w/~w', [Outdir, Outfile])
	).
process_latex([P|Ps],  Outdir, Outfile, Document, Options) :-
	info(10, 'Ignoring unknown latex processing command ~w', [P]),
	process_latex(Ps, Outdir, Outfile, Document, Options).
process_latex([], _, _, _, _).
	      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic ppl_task_options_1/3.

ppl_set_options(Options) :-
	( enum_task(Task),
	  ppl_set_options(Task, Options),
	  fail
	; true
	).
	
ppl_override_options(Options) :-	
	( enum_task(Task),
	  ppl_override_options(Task, Options),
	  fail
	; true
	).

enum_task(elim).
enum_task(form).
enum_task(ipol).
enum_task(ppl).
enum_task(valid).
	
ppl_set_options(Task, Options) :-
	( prolog_load_context(source, Source) ->
	  ppl_set_source
	; Source = user
	),
	sort([user, Source], Sources),
	( member(Source1, Sources),
	  retractall( ppl_task_options_1(Source1, Task, _) ),
	  asserta( ppl_task_options_1(Source1, Task, Options) ),
	  fail
	; true
	).

ppl_effective_options(Task, Options, Options1) :-
	( last_source(Source) -> true ; Source = user ),
	( ppl_task_options_1(Source, Task, Options2) -> true
	; ppl_default_options(Task, Options2) -> true
	; Options2 = []
	),
	override_kvs(Options, Options2, Options1).

ppl_default_options(ppl,
		    [latex_processing=[pdflatex],
		     show_code=false,style=brief,
		     rightcols=4,maxpos=60,aoparen=true]).

ppl_override_options(Task, Options) :-
	( prolog_load_context(source, Source) ->
	  true
	; Source = user
	),
	sort([user, Source], Sources),
	( member(Source1, Sources),
	  ( ppl_task_options_1(Source1, Task, Options1) -> true
	  ; ppl_default_options(Task, Options1) -> true
	  ; Options1 = []
	  ),
	  override_kvs(Options, Options1, Options2),
	  retractall( ppl_task_options_1(Source1, Task, _) ),
	  asserta( ppl_task_options_1(Source1, Task, Options2) ),
	  fail
	; true
	).

override_kvs(Xs, Ys, Zs) :-
	findall(A=B, (member(A=B, Ys), \+ memberchk(A=_, Xs)), Ys1),
	append(Xs, Ys1, Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppl_set_source :-
	prolog_load_context(source, Src),
	retractall(last_source(_)),
	assert(last_source(Src)).

ppl_validity_status(Status, F, Options) :-
	vs_to_result(Status, Result),
	set_last_ppl_result(Result, Options),
	( memberchk(printing=false, Options) ->
	  true
	; ppl_is_at_printtime ->
	  vs_fmt_latex(Status, Fmt),
	  inner_write_form_options(Options, Options1),
	  mark_installed_macros(F, F1),
	  format(Fmt, [write_form(F1, Options1)])
	; vs_fmt_prolog(Status, Fmt),
	  format(Fmt)
	).

vs_fmt_latex(valid, '\\pplIsValid{~@}~n').
vs_fmt_latex(unknown, '\\pplFailedToValidate{~@}~n').
vs_fmt_latex(not_valid, '\\pplIsNotValid{~@}~n').

vs_fmt_prolog(valid, '~N*Valid*~n').
vs_fmt_prolog(unknown, '~NFailed to validate~n').
vs_fmt_prolog(not_valid, '~N*Not valid*~n').

vs_to_result(valid, true).
vs_to_result(unknown, unknown).
vs_to_result(not_valid, false).

ppl_form(F) :-
	ppl_form(F, []).
ppl_form(F, Options) :-
	ppl_effective_options(form, Options, Options1),
	( memberchk(printing=false, Options1) ->
	  set_last_ppl_result(F, Options1)
	; ppl_form_1(F, Options1)
	).
ppl_form_1(F, Options) :-	
	( memberchk(expand=true, Options) ->
	  mac_expand(F, F0)
	; F0 = F
	),
	prettify_form(F0, F1, Options),
	set_last_ppl_result(F1, Options),
	( ppl_is_at_printtime ->
	  inner_write_form_options(Options, Options1),
	  ( memberchk(input=Input, Options1) ->
	    mark_installed_macros(Input, Input1),
	    format('~n\\noindent Input: $~@$\\\\',
		   [write_form(Input1, Options1)])
	  ; true
	  ),
	  ( memberchk(opstring=OpString, Options1), OpString \= none ->
	    format('~n\\noindent Result of ~w:', [OpString])
	  ; true
	  ),
          format('~N\\['),
          mark_installed_macros(F1, F2),				
	  pp_form(F2, Options1),
	  format('\\]~n')
	; append(Options, [format=prolog, style=brief], Options1),
	  pp_form(F1, Options1)
	).

inner_write_form_options(Options, Options1) :-
	 append(Options,
		[format=latex,
		 style=brief,
		 rightcols=4,
		 maxpos=50,
		 finally='.'],
		Options1).

ppl_elim(Label) :-
	ppl_elim(Label, []).
ppl_elim(Label, Options) :-
	ppl_effective_options(elim, Options, Options1),
	reset_counters(Options1),
	mac_expand(Label, F),
        ( memberchk(elim_options=ElimOptions, Options1) -> true
	; ElimOptions = []
	),
        once(elim_fol(F, ElimOptions, F1)),
	append(Options1, [opstring='elimination', input=Label], Options2),
	ppl_form(F1, Options2).

:- dynamic ppl_ipol_result/1.
	 
ppl_ipol(Label) :-
	ppl_ipol(Label, []).
ppl_ipol(Label, Options) :-
	ppl_effective_options(ipol, Options, Options1),
	reset_counters(Options1),
        mac_expand(Label, F0),
	( F0 = (_ -> _) ->
	  F = F0
	; F0 = (FA <- FB) ->
	  F = (FB -> FA)
	; err('Formula for interpolation is no implication: ~q', [F])
	),
	elim_by_renaming_for_validity(F, F1),
	( logform_has_second_order_quantifier(F1) ->
	  asserta(errform(F1)),
	  err('Second-order quantifiers that can not be eliminated by renaming in interpolation input for ~q',
	      [Label])
	; true
	),
	( memberchk(prover=Prover, Options1) -> true
	; Prover = cm
	),
	ppl_ipol_1(Prover, F1, IP, Options1),
	append(Options1, [opstring='interpolation', input=Label], Options2),
	prettify_form(IP, IP1, Options2),
	ppl_form(IP1, Options2).

ppl_ipol_1(hyper, F1, IP, Options) :-
	!,
	hyper_craigproof(F1, [ip=IP|Options]).
ppl_ipol_1(cm, F1, IP, Options) :-
	!,
	retractall( ppl_ipol_result(_) ),
	cm_craigproof(F1,
		      [ip=IP|Options]),
	( memberchk(enum_ips=N, Options), N \= false ->
	  ( ppl_ipol_result(X), X == IP ->
	    fail
	  ; number(N),
	    predicate_property(ppl_ipol_result(_), number_of_clauses(N1)),
	    N1 >= N - 1 ->
	    !
	  ; asserta( ppl_ipol_result(IP) )
	  )
	; !
	).
ppl_ipol_1(Prover, _, _, _) :-
	err('No interpolation prover: ~w', [Prover]).
	
ppl_valid(F) :-
	ppl_valid(F, []).
ppl_valid(F, Options) :-
	ppl_effective_options(valid, Options, Options1),
	reset_counters(Options1),
	mac_expand(F, F1),
	( memberchk(split_iff=true, Options1),
	  split_biconditional(F1, G1, G2) ->
	  ppl_valid(G1, [printing=false|Options1]),
	  last_ppl_result(Result1),
	  ( Result1 = true ->
	    ppl_valid(G2, [printing=false|Options1]),
	    last_ppl_result(Result2),
	    ( Result2 = true ->
	      ppl_validity_status(valid, F, Options1)
	    ; Result2 = false ->
	      ppl_validity_status(not_valid, F, Options1)
	    ; ppl_validity_status(unknown, F, Options1)
	    )
	  ; Result1 = false ->
	    ppl_validity_status(not_valid, F, Options1)
	  ; ppl_validity_status(unknown, F, Options1)
	  )
	; elim_by_renaming_for_validity(F1, F2),
	  ( logform_has_second_order_quantifier(F2) ->
	    ( memberchk(elim=true, Options1) ->
	      info(20, 'Eliminating second-order quantifiers.'),
	      ( memberchk(elim_options=ElimOptions, Options1) -> true
	      ; ElimOptions = []
	      ),
	      once(elim_fol(F2, ElimOptions, F3)),
	      info(20, 'Eliminating second-order quantifiers done.')
	    ; asserta(errform(F2)),
	      err('Second-order quantifiers that can not be eliminated by renaming in validity test for ~q, try option elim=true',
		  [F])
	    )
	  ; F3 = F2
	  ),
	  fol_validity_status(F3, Options1, Status),
	  ppl_validity_status(Status, F, Options1)
	).

split_biconditional((F<->G),
		    (F->G),
		    (F<-G)) :-
	!.
split_biconditional((H -> (F<->G)),
		    (H -> (F->G)),
		    (H -> (F<-G))) :-
	!.
split_biconditional(all(X, (F<->G)),
		    all(X, (F->G)),
		    all(X, (F<-G))) :-
	!.
split_biconditional(all(Y, (H -> (F<->G))),
		    all(Y, (H -> (F->G))),
		    all(Y, (H -> (F<-G)))) :-
	!.
split_biconditional((H -> all(X, (F<->G))),
		    (H -> all(X, (F->G))),
		    (H -> all(X, (F<-G)))) :-
	!.
split_biconditional(all(Y, (H -> all(X, (F<->G)))),
		    all(Y, (H -> all(X, (F->G)))),
		    all(Y, (H -> all(X, (F<-G))))) :-
	!.


fol_validity_status(F, Options, Status) :-
	( memberchk(nf=NF, Options) -> true
	; NF = false
	),
	( NF \= false ->
	  normalize_formulas([~F], NF, F2),
	  cnf_to_form(F2, F3),
	  logform_negate(F3, F1)
	; F1 = F
	),
	( select(prover=Prover, Options, Options1) -> true
	; Prover = default,
	  Options1 = Options
	),
	( Prover = cm ->
	  ( Options1=[] ->
	    Options2=[goal_clauses=all, goal_clause_sign=minority]
	  ; Options2=Options1
	  ),
	  ( is_valid_fol_cm(F1, Options2) ->
	    Status = valid
	  ; Status = unknown
	  )
	; Prover = tptp(Prover1) ->
	  ( is_valid_fol_tptp(F1, [prover=Prover1|Options1]) ->
	    Status = valid
	  ; Status = unknown
	  )
	; Prover = satsolver ->
	  fol_status_satsolver(~F1, Options, NStatus),
	  ( NStatus = unsatisfiable ->
	    Status = valid
	  ; NStatus = satisfiable ->
	    Status = not_valid
	  ; Status = unknown
	  )
	; fol_status_prover9(~F1, Options, NStatus),
	  ( NStatus = unsatisfiable ->
	    Status = valid
	  ; NStatus = satisfiable ->
	    Status = not_valid
	  ; Status = unknown
	  )
	).

tptp_export(Label, File, Options) :-
	reset_counters(Options),
	mac_to_tptp(Label, Stmts),
	onto_file(( member(Stmt, Stmts),
		    pp_tptp(Stmt),
		    format('.~n~n'),
		    fail
		  ; true
		  ),
		  File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fol_status_satsolver(F, Options, Status) :-
	fol_to_prop_for_unsat(F, F1),
	( memberchk(sat_options=SATOptions, Options) ->
	  true
	; SATOptions = []
	),
	( memberchk(cmodel=_, Options) ->
	  ( memberchk(models=Ms, SATOptions) ->
	    SATOptions1 = SATOptions
	  ; SATOptions1 = [models=Ms|SATOptions]
	  )
	; SATOptions1 = SATOptions
	),
	( memberchk(timeout=Timeout, Options) ->
	  SATOptions2 = [timeout=Timeout|SATOptions1]
	; SATOptions2 = SATOptions1
	),
	( memberchk(solution=Status, SATOptions2) ->
	  SATOptions3 = SATOptions2
	; SATOptions3 = [solution=Status|SATOptions2]
	),
	external_solve_form(F1, SATOptions3),
	( Status=satisfiable, memberchk(cmodel=M, Options) ->
	  Ms = [M|_]
	; true
	).

fol_to_prop_for_unsat(F, F1) :-
	( logform_has_first_order_quantifier(F) ->
	  logform_clean_vars_to_symbols(F, F2),
	  logform_process_subforms_with_polarity(F2, rm_ex, F1),
	  ( logform_has_first_order_quantifier(F1) ->
	    err('Failed to convert first-order formula to equi-satisfiable propositional formula')
	  ; true
	  )
	; F1 = F
	).

rm_ex(F, P, F1) :-
	( P = p, F = ex(_, F1) -> true
	; P = n, F = all(_, F1) -> true
	; F1 = F
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mac_expand(F) :-
	logform_reset_counters,
	mac_expand(F, F1),
	logform_vars_to_pretty_symbols(F1, F2),
	check_form(F2),
	set_last_ppl_result(F2, []),
	pp(F2),
	nl.

mac_expand_1(F) :-
	logform_reset_counters,
	mac_expand_toplevel(F, F1),
	logform_vars_to_pretty_symbols(F1, F2),
	check_form(F2),
	set_last_ppl_result(F2, []),
	pp(F2),
	nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_counters(Options) :-
	( memberchk(counters=keep, Options) ->
	  true
	; logform_reset_counters
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prettify_form(F, F1, Options) :-
	( memberchk(simp_result=Simp, Options) ->
	  simp_form(F, Simp, result, F2)
	; F2 = F
	),
	logform_remove_void_quantifiers(F2, F3),
	logform_gather_quantifiers(F3, F4),
	( memberchk(prettify=Prettify, Options) ->
	  true
	; Prettify = imp
	),
	prettify_form_1(Prettify, F4, F1).

prettify_form_1(keep, F, F1) :-
	!,
	logform_vars_to_pretty_symbols(F, F1).
prettify_form_1(imp, F, F1) :-
	!,
	prettify_imp(F, F2),
	logform_vars_to_pretty_symbols(F2, F1).
prettify_form_1(cnf_imp, F, F1) :-
	!,
	cnf3(F, F2),
	cnf_to_form_u(F2, F3),
	prettify_imp(F3, F1).
prettify_form_1(cnflike, F, F1) :-
	!,
	prettify_form_1(cnf_imp, F, F1).
prettify_form_1(cnf, F, F1) :-
	!,
	cnf3(F, F2),
	cnf_to_form_u(F2, F3),
	logform_vars_to_pretty_symbols(F3, F1).
prettify_form_1(dnf, F, F1) :-
	!,
	dnf3(F, F2),
	dnf_to_form_u(F2, F1).
prettify_form_1(cnf_u1, F, F1) :-
	!,
	cnf3(F, F2),
	cnf_to_form_u1(F2, F3),
	logform_vars_to_pretty_symbols(F3, F1).
prettify_form_1(dnf_u1, F, F1) :-
	!,
	dnf3(F, F2),
	dnf_to_form_u1(F2, F3),
	logform_vars_to_pretty_symbols(F3, F1).
prettify_form_1(_, F, F).
