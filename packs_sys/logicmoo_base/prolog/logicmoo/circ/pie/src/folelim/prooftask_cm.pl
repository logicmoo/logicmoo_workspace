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

:- module(prooftask_cm,
	  [ cmpr/2,
	    cmpr3/3,
	    cmprove/0,
	    m_features/2,
	    m_info/3,
	    m_info_features/3,
	    m_red_subs_limited/3,
	    m_set_equality/5,
	    m_set_goal_clauses/4,
	    normalize_formulas/3,
	    reset_gensyms/0,
	    threshold/2,
	    is_valid_fol_cm/1,
	    is_valid_fol_cm/2]).
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(swilib(options)).
:- use_module(swilib(info)).
:- use_module(swilib(pretty)).
:- use_module(tptpio).
:- use_module(preprocexp).
:- use_module(prep_resol).
:- use_module(prep_identify).
:- use_module(cm_option_sets).
:- use_module(proofs_cm).
:- use_module(tabx_dotgraph).
:- use_module(swilib(err)).
:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(cmprover(cm)).
:- use_module(cmprover(pre_cm)).
:- use_module(cmprover(compile_term)).
:- use_module(nf(brand)).
:- use_module(utils_fol).

:- (prolog_flag(dialect, yap) -> use_module(swilib(yap_support)) ; true ).

inf_limit_factor(10000).
threshold(inferences_definitional_cnf, 1000).
threshold(inferences_fol_red_subs, 1000).
threshold(inferences_m_red_condense, 1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% For invocation from shell, see scripts/cm-prove-problem.sh
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmprove :-
        catch(cmprove_process, E, (print_message(error, E), fail)),
        halt.
cmprove :-
        halt(1).

cmprove_process :-
        current_prolog_flag(argv, Args),
	format(user_error, '% Argument vector passed to Prolog: ~q~n', [Args]),
        ( select(OptionArg, Args, Files),
          sub_atom(OptionArg, 0, B, _, '--options='),
          sub_atom(OptionArg, B, _, 0, OptionsAtom),
          term_to_atom(Options, OptionsAtom)
        ; Files = Args,
          Options = []
        ),
        cmprove_process_files(Files, Options).

cmprove_process_files(Files, Options) :-
	( member(File, Files),
	  once(cmpr(File, Options)),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info_time(V, _, _, _) :-
	get_info_verbosity(V1),
	V1 < V,
	!.
info_time(V, Info, T1, T2) :-
	T is T1 - T2,
	format(atom(Fmt), 'CPUTIME: ~w ~t~3f sec.~76|', [Info, T]),
	info(V, Fmt).

get_cputime(T) :-
	prolog_flag(dialect, yap),
	!,
	statistics(cputime, [T1,_]),
	T is T1/ 1000.
get_cputime(T) :-
	statistics(cputime, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Note: the options for the unary version are quite different
%%%% than the setting with Options=[]
%%%%
is_valid_fol_cm(F) :-
	is_valid_fol_cm(F, [goal_clauses=all,
			    goal_clause_sign=minority]).
%%%%
is_valid_fol_cm(F, Options) :-
	((( F  = (_ -> _) ; F = (_ <- _))) -> F1 = F
	; F1 = (true -> F)
	),
	cmpr(F1, [result=Result|Options]),
	!,
	Result == proved.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmpr(F, Options) :-
	cmpr_initialize(Options, Options1, T1),
	( var(F) ->
	  err('Variable given as cmpr input: ~q', [F])
	; atom(F) ->
	  tptp_problem_1(F, Options1, MT, MA),
	  get_cputime(T2),
	  cmpr2(MT, MA, Options1),
	  info_time(20, 'TPTP problem preparation', T2, T1)
	; ( F = [_|_] ; F = [] ) ->
	  cmpr2(F, [], Options1)
	; once((F = (FA->FT) ; F = (FT<-FA))) ->
	  normalize_problem([FT], [FA], Options1, MT, MA),
	  get_cputime(T2),
	  cmpr2(MT, MA, Options1),
	  info_time(20, 'Normalization', T2, T1)
	; err('Unrecognized cmpr input format: ~q', [F])
	),
	cmpr_conclude(F, Options1, T1).

reset_gensyms :-
	set_sk_counter(0),
	set_p_counter(0),
	set_split_pred_counter(0).

cmpr_initialize(Options, Options1, T1) :-
	get_cputime(T1),
	from_options(info_progress=IP, Options, 1),
	set_info_progress(IP),
	( from_options(keep_gensyms=true, Options, false) -> true ; reset_gensyms ),
	default_options(Options, [result=_], Options2),
	default_options(Options2, [result_proof=_], Options1).

cmpr_conclude(ProblemSpec, Options, T1) :-
	from_options(result=Result, Options),
	( select(result=_, Options, Options2) -> true ; Options2 = Options ),
	( select(result_proof=_, Options2, Options3) -> true ; Options3 = Options2 ),	
	get_cputime(T2),
	info_time(10, 'Overall', T2, T1),
	T is T2 - T1,
	date_atom(Date),
	prolog_flag(version_data, Version),
	gethostname(HostName),
	( atom(ProblemSpec) ->
	  info(10, 'cm_result(~q, ~q, ~t~3f~10|, ~q, ~q, ~q, ~q).',
	       [ProblemSpec, Result, T, Options3, Date, Version, HostName])
	; info(10, 'cm_result(~q, ~t~3f~10|, ~q, ~q, ~q, ~q).',
	       [Result, T, Options3, Date, Version, HostName])
	),
	( Result == proved,
	  memberchk(proof=ProofSpec, Options),
	  memberchk(result_proof=Proof, Options) ->
	  ( ProofSpec == writeq ->
	    format('% Proof:~n'),
	    writeq(proof(Proof)),
	    writeln('.'),
	    format('% Proof printed~n'),
	    get_cputime(T4),
	    info_time(20, 'Proof printing', T4, T2)
	  ; ProofSpec == write_canonical ->
	    format('% Proof:~n'),
	    write_canonical(proof(Proof)),
	    writeln('.'),
	    format('% Proof printed~n'),
	    get_cputime(T4),
	    info_time(20, 'Proof printing', T4, T2)
	  ; %% writeq and write_canonical cause a segmentation failure for
	    %% large proof terms, writex is an implementation that seems
	    %% to work for these
	    ProofSpec == writex ->
	    format('% Proof:~n'),
	    writex(proof(Proof)),
	    writeln('.'),
	    format('% Proof printed~n'),
	    get_cputime(T4),
	    info_time(20, 'Proof printing', T4, T2)
	  ; ProofSpec == pp ->
	    format('% Proof:~n'),
	    pp(proof(Proof)),
	    writeln('.'),
	    format('% Proof printed~n'),
	    get_cputime(T4),
	    info_time(20, 'Proof printing', T4, T2)
	  ; true
	  ),
	  from_options(tabx_dotgraph=Dotgraph, Options, none),
	  ( Dotgraph \== none ->
	    cm_proof_to_tabx(Proof, TabX),
	    tabx_to_dotgraph(TabX, Dotgraph)
	  ; true
	  )
	; true
	).


date_atom(A) :-
	date(date(Y,M,D)),
	format(atom(A), '~|~`?t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+', [Y,M,D]).

cmpr2(MT, MA, Options) :-
	get_cputime(T1),
	simplify_problem(MT, MA, Options, MT1, MA1),
	m_prepare(MT1, MA1, Options, M, Features),
	get_cputime(T2),
	cmpr3(M, Features, Options),
	info_time(20, 'Matrix preparation 1', T2, T1).

cmpr3(M, Features, Options) :-
	get_cputime(T1),
	from_options(cm_options=CMOptions0, Options, CMOptions0),
	from_options(pr_options=PROptions0, Options, PROptions0),
	( var(CMOptions0) -> determine_cm_options(Features, Options, CMOptions0) ; true ),
	( var(PROptions0) -> determine_pr_options(Options, PROptions0) ; true ),
	collect_options(Options, add_cm_options, ACO, RCO),
	collect_options(Options, add_pr_options, APO, RPO),
	subtract(CMOptions0, RCO, CMOptions00),
	subtract(PROptions0, RPO, PROptions00),
	append(ACO, CMOptions00, CMOptions),
	append(APO, PROptions00, PROptions),
	( memberchk(proof=ProofSpec, Options) ->
	  CMOptions1  = [p|CMOptions],
	  ( var(ProofSpec) -> ProofSpec = Proof ; true ),
	  ( memberchk(result_proof=Proof, Options) -> true ; true ),
	  PROptions1 = [p(Proof)|PROptions]
	; CMOptions1 = CMOptions, PROptions1 = PROptions
	),
	( memberchk(lex=Lex, Options) ->
	  CMOptions2 = [lex(Lex)|CMOptions1]
	; CMOptions2 = CMOptions1
	),
	from_options(lean_cuts=LeanCutsMode, Options, none),
	( LeanCutsMode = none ->
	  M1 = M
	; m_add_cuts(M, LeanCutsMode, CMOptions2, M1)
	),
	%% asserta(mmmcpr(M)),
	%% nl, pp(M), nl,
	( is_horn_matrix(M1) -> fol_sort_clauses_for_horn(M1, M2) ; M2 = M1 ),
	%% *** pass further options...
	Options1 = Options,
	% Options1 = [file='/tmp/cm.out'|Options],
	% Options1 = [mode=rerun, file='/tmp/cm.out'|Options],
	Options2 = [goal=[['$query']] | Options1],
	%%
	%% asserta(mmm(M2)), %% debug
	m_info(30, M2, 'Matrix passed to CM'),
	get_cputime(T2),
	cmpr_core(M2, CMOptions2, PROptions1, Options2),
	info_time(20, 'Matrix preparation 2', T2, T1).

collect_options([K=(A-D)|KVs], K, Add, Del) :-
	!,
	append(A, A1, Add),
	append(D, D1, Del),
	collect_options(KVs, K, A1, D1).
collect_options([K=A|KVs], K, Add, Del) :-
	!,
	collect_options([K=(A-[])|KVs], K, Add, Del).
collect_options([_|KVs], K, A, D) :-
	collect_options(KVs, K, A, D).
collect_options([], _, [], []).


cmpr_core(M, _, _, Options) :-
	memberchk(effective_matrix_only=M, Options),
	!.
cmpr_core(M, CMOptions, PROptions, Options) :-
	get_cputime(T1),
	info(40, 'Effective cm_options=~q', [CMOptions]),
	info(40, 'Effective pr_options=~q', [PROptions]),
	info(40, 'Calling CMProver: Compiling'),
	( memberchk(file=File, Options) ->
	  ( from_options(mode=rerun, Options, default) ->
	    info(40, 'Rerunning on file ~w', [File])
	  ; cm(M, File, CMOptions)
	  ),
	  get_cputime(T2),
	  info(20, 'Loading file: ~w', [File]),
	  consult(File)
	; cm(M, KB, CMOptions),
	  get_cputime(T2),
	  compile_term(KB, runcm)
	),
	get_cputime(T3),
	from_options(timeout=Timeout, Options, 0),
	from_options(goal=Goal, Options, [['$query']]),
	to_options(result=Result, Options),
	info(10, 'Calling CMProver: Running'),
	info(50, 'Call ~q', [cm:proof(Goal, PROptions)]),
	( Timeout =:= 0 ->
	  %% enumerates solutions
	  info(10, 'Calling CM without timeout to enumerate solutions'),
	  ( proof(Goal, PROptions),
	    Result = proved
	  ; Result = failed_exhausted
	  )
	; %% call_with_time_limit: like in "once"
	  info(10, 'Calling CM with timeout ~w', [Timeout]),
	  catch( call_with_time_limit(Timeout,
				      ( cm:proof(Goal, PROptions),
					Result = proved
				      ; Result = timeout
				      )),
		 time_limit_exceeded,
		 Exceeded = true
	       ),
	  ( Exceeded == true ->
	    Result = timeout
	  ; true
	  )
	),
	get_cputime(T4),
	info_time(20, 'CM compilation phase', T2, T1),
	info_time(20, 'CM target code loading', T3, T2),
	info_time(20, 'CM proving phase', T4, T3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalize_formulas(Formulas, Options, Formulas1) :-
	length(Formulas, Len),
	threshold(inferences_definitional_cnf, CNFLimit),
	( from_options( cnf_trafo=auto(CNFLimit1), Options, auto(CNFLimit) ) ->
	  CNFLimit2 is CNFLimit1 / max(1, Len),
	  CNFAxOptions = [auto(CNFLimit2)|Options]
	; CNFAxOptions = Options
	),
	map_cnf_3(Formulas, CNFAxOptions, Formulas1).

%%%% 
%%%% Conjectures is a list of formulas, understood in the sense of the TPTP:
%%%% all of them must be theorems.
%%%% 
normalize_problem(Conjectures, Axioms, Options, MTheorem, MAxioms) :-
	info(20, 'Normalizing'),
	length(Axioms, LenAx),
	info(10, 'Normalizing ~w axioms', [LenAx]),
	normalize_formulas(Axioms, Options, MAxioms),
	length(Conjectures, LenConjectures),
	info(20, 'Normalizing ~w conjectures', [LenConjectures]),
	list_to_conjunction_1(Conjectures, Conjectures1),
	normalize_formulas([~(Conjectures1)], Options, MTheorem).

list_to_conjunction_1([F], F) :-
	!.
list_to_conjunction_1([F|G], (F, G1)) :-
	list_to_conjunction_1(G, G1).
list_to_conjunction_1([], true).

toplevel_conjunction_to_list_1((X,Y), [X|Ys]) :-
	!,
	toplevel_conjunction_to_list_1(Y, Ys).
toplevel_conjunction_to_list_1(X, [X]).

cnf_3(X, Options, Y) :-
	threshold(inferences_definitional_cnf, Limit),
	from_options( cnf_trafo=Mode, Options, auto(Limit) ),
	( Mode = fewsimp ->
	  cnf_fewsimp(X, Y)
	; Mode = pred(Predicate) ->
	  Call =.. [Predicate,X,Y],
	  call(Call)
	; Mode = std ->
	  cnf4(X, Y)
	; Mode = c4 ->
	  cnf4(X, Y)
	; Mode = c5 ->
	  cnf5(X, Y)
	; Mode = c6 ->
	  cnf6(X, Y)
	; Mode = pg ->
	  cnf_def(X, Y)
	; Mode = pgi ->
	  cnf_def(X, Y1),
	  prep_identify(Y1, defpreds, Y)
	; Mode = miniscope ->
	  %% old implementation, not much tested
	  cnf_miniscope(X, Y)
	; Mode = auto(Limit) ->
	  inf_limit_factor(InfFactor),
	  Limit1 is InfFactor*Limit,
	  call_with_inference_limit( cnf4(X, Y), Limit1, Result ),
	  % call_with_inference_limit( cnf_fewsimp(X, Y), Limit1, Result ),
	  ( Result = inference_limit_exceeded ->
	    info(10, 'Retrying with definitional CNF transformation'),
	    cnf_def(X, Y)
	  ; true
	  )
	; err('Invalid cnf trafo mode: ~q', [Mode])
	).

cnf_def(F, M) :-
	pgnf(F, F1, A1),
	cnf4((F1, A1), M).
%	cnf_fewsimp((F1, A1), M).

map_cnf_3([X|Xs], Options, M) :-
	cnf_3(X, Options, M1),
	append(M1, M2, M),
	map_cnf_3(Xs, Options, M2).
map_cnf_3([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_problem(MTheorem, MAxioms, Options, MTheorem1, MAxioms1) :-
	info(10, 'Simplifying theorem'),
	m_simp(MTheorem, Options, MTheorem1),
	m_info(30, MTheorem, 'Theorem before simplification'),
	m_info(30, MTheorem1, 'Theorem after simplification'),
	info(10, 'Simplifying axioms'),
	m_simp(MAxioms, Options, MAxioms1),
	m_info(30, MAxioms, 'Axioms before simplification'),
	m_info(30, MAxioms1, 'Axioms after simplification'),
	info(10, 'Simplifying done').

simp(sort_lits, M, M1) :- !, fol_sort_lits_by_pred_occurrences(M, M1).
simp(condense(Limit), M, M1) :-	!, m_red_condense_limited(M, Limit, M1).
simp(subs(Limit), M, M1) :- !, m_red_subs_limited(M, Limit, M1).
simp(split, M, M1) :- !, m_split(M, M1).
simp(X, _, _) :- err('No such simplification: ~q', [X]).

m_simp(M, Options, M1) :-
	threshold(inferences_fol_red_subs, LimitRedSubs),
	threshold(inferences_m_red_condense, LimitRedCondense),
	from_options(simp=Simp, Options,
		     [sort_lits,
		      condense(LimitRedCondense),
		      subs(LimitRedSubs),
		      split]),
	m_simp_1(Simp, M, M1).

m_simp_1([Simp|Simps], M, M1) :-
	info(20, 'Simplifying: ~q', [Simp]),
	simp(Simp, M, M2),
	m_simp_1(Simps, M2, M1).
m_simp_1([], M, M).

m_red_subs_limited(M, Limit, M1) :-
	inf_limit_factor(InfFactor),
	LimitRedSubs1 is InfFactor*Limit,
	call_with_inference_limit( fol_red_subs(M, M1),
				   LimitRedSubs1,
				   Result ),
	( Result = inference_limit_exceeded ->
	  info(20, 'Subsumption limit exceeded, trying subsumption by units'),
	  call_with_inference_limit( fol_red_subs_by_unit(M, M1),
				     LimitRedSubs1,
				     Result1 ),
	  ( Result1 = inference_limit_exceeded ->
	    info(20, 'All subsumption limits exceeded'),
	    M1 = M
	  ; true
	  )
	; true
	).

% 	info(20, 'Enriching units'),
% 	length(M3, M3Len),
% 	MaxSol is M3Len*50,
% 	cm_lem(M3, [limit=10000, max_sol=MaxSol], M4),
% 	info(20, 'Simplification done'),
%	M1 = M3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptp_problem_1(FileSpec, Options, MTheorem, MAxioms) :-
	tptp_problem(FileSpec, Options, Format, MTheorem1, MAxioms1),
	( Format=fof, from_options(normalize=true, Options, true) ->
	  toplevel_conjunction_to_list_1(MTheorem1, Conjectures),
	  toplevel_conjunction_to_list_1(MAxioms1, Axioms),
	  normalize_problem(Conjectures, Axioms, Options, MTheorem, MAxioms)
	; MTheorem = MTheorem1,
	  MAxioms = MAxioms1
	).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	M_HORN = [r4,r5,hd1,mrn,no_fan,sg(0),r7(0.2)],
% 	XL_NONHORN = [mrn,hd1,r5,sn],
% 	M_NONHORN = [r1,r4,r5,hd1,r4_limit(10000000),sh],
% 	M_HORN = [r4,r5,hd1,mrn,no_fan,sh],
% 	XL_HORN = [r5,hd1,mrn,no_fan,sh],	
% 	XL_NONHORN = [mrn,hd,r5,sn],
%  	M_NONHORN = [r1,r4,r5,hd,r4_limit(10000000),sh],
%  	M_HORN = [r4,r5,hd,mrn,no_fan,sh],
%  	XL_HORN = [r5,hd,mrn,no_fan,sn],

determine_pr_options(Options, PROptions) :-
	from_options(pr_cfg=CFGSet, Options, std),
	( pr_option_set(CFGSet, PROptions) ->
	  true
	; err('No suitable pr option set for ~q', [CFGSet])
	).
	
determine_cm_options(Features, Options, CMOptions) :-
	from_options(cm_cfg=CFGSet, Options, std),
	( memberchk(npreds=NPreds, Features),
	  memberchk(nclauses=NClauses, Features),
	  NPreds * NClauses > 10000 ->
	  Size = 8
	; Size = 3
	),
	( memberchk(horn, Features) -> Horn = horn ; Horn = nonhorn ),
	( memberchk(equality, Features) -> Eq = eq ; Eq = neq ),
	( setof(Size1, O^cm_option_set(CFGSet, Horn, Eq, Size1, O), Sizes),
	  reverse(Sizes, Sizes1),
	  member(Size2, Sizes1),
	  Size2 =< Size ->
	  cm_option_set(CFGSet, Horn, Eq, Size2, CMOptions1),
	  !,
	  ( memberchk(steq, Features),
	    \+ memberchk(steq, CMOptions1) ->
	    CMOptions = [steq|CMOptions1]
	  ; CMOptions = CMOptions1
	  )
	; err('No suitable cm option set for ~w ~w ~w and size ~w',
	      [CFGSet, Horn, Eq, Size])
	).

m_info_features(V, Features) :-
	m_info_features(V, Features, '').
m_info_features(V, Features, Header) :-
	info(V, '--------------------'),
	( Header = '' -> true ;	info(V, 'Feature info about: ~w', [Header]) ),
	( memberchk(horn, Features) ->
	  info(V, 'Feature: Horn') ; true ),
	( memberchk(nonhorn, Features) ->
	  info(V, 'Feature: Non-Horn') ; true ),
	( memberchk(equality, Features) ->
	  info(V, 'Feature: With equality') ; true ),
	( memberchk(steq, Features) ->
	  info(V, 'Feature: STEQ transformed') ; true ),
	( memberchk(noequality, Features) ->
	  info(V, 'Feature: No equality') ; true ),
	( memberchk(nclauses=NC, Features) ->
	  info(V, 'Feature: ~w clauses', [NC]) ; true ),
	( memberchk(npreds=NP, Features) ->
	  info(V, 'Feature: ~w predicates', [NP]) ; true ),
	( memberchk(npreds0=NP0, Features) ->
	  info(V, 'Feature: ~w propositional predicates', [NP0]) ; true ),
	( memberchk(npreds1=NP1, Features) ->
	  info(V, 'Feature: ~w monadic predicates', [NP1]) ; true ),
	( memberchk(npredsN=NPN, Features) ->
	  info(V, 'Feature: ~w predicates with arity >=2', [NPN]) ; true ),
	( memberchk(nconsts=NK, Features) ->
	  info(V, 'Feature: ~w constants', [NK]) ; true ),
	( memberchk(nfuns=NF, Features) ->
	  info(V, 'Feature: ~w functions', [NF]) ; true ).

m_features(M, Features) :-
	F1 = [],
	( is_horn_matrix(M) -> F2 = [horn|F1] ; F2 = [nonhorn|F1] ),
	m_signature(M, Ps, Fs),
	length(M, MLen),
	length(Ps, PsLen),
	findall(C, member(C/0, Fs), Cs),
	length(Cs, CsLen),
	findall(F, ( member(F/N, Fs), N > 0 ), Fs1),
	length(Fs1, FsLen),

	findall(k, ( member(F/N, Ps), N = 0 ), PsLen0s),
	findall(k, ( member(F/N, Ps), N = 1 ), PsLen1s),
	findall(k, ( member(F/N, Ps), N > 1 ), PsLenNs),
	length(PsLen0s, PsLen0),
	length(PsLen1s, PsLen1),
	length(PsLenNs, PsLenN),
	
	F3 = [nclauses=MLen, npreds=PsLen, npreds0=PsLen0, npreds1=PsLen1,
	      npredsN=PsLenN, nfuns=FsLen, nconsts=CsLen|F2],
	( memberchk((=)/2, Ps) -> F4=[equality|F3] ; F4=[noequality|F3] ),
	Features = F4.

m_num_of_positive_clauses(M, N) :-
	findall(k, ( member(C, M), c_is_positive(C) ), Ks),
	length(Ks, N).

m_num_of_negative_clauses(M, N) :-
	findall(k, ( member(C, M), c_is_negative(C) ), Ks),
	length(Ks, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_set_goal_clauses(MT, MA, Options, M) :-
	from_options(goal_clauses=none, Options, theorem),
	!,
	append(MT, MA, M).
m_set_goal_clauses(MT, MA, _, M) :-
	( member(C, MT) ; member(C, MA) ),
	memberchk('$query', C),
	!,
	info(40, 'Using goals found in input'),
	append(MT, MA, M).
m_set_goal_clauses(MT, MA, Options, M) :-
	m_num_of_positive_clauses(MT, NPT),
	m_num_of_negative_clauses(MT, NNT),
	m_num_of_positive_clauses(MA, NPA),
	m_num_of_negative_clauses(MA, NNA),
	from_options(goal_pick=Pick, Options, all),
	( ( MT = [], M1 = MA
	  ; MA = [], M1 = MT
	  ; from_options(goal_clauses=all, Options, theorem),
	    append(MT, MA, M1)
	  ) ->
	  from_options(goal_clause_sign=Sign, Options, minority),
	  ( Sign \= positive, Sign \= negative ->
	    ( NPT+NPA < NNT+NNA ->
	      Sign1 = positive
	    ; Sign1 = negative
	    )
	  ; Sign1 = Sign
	  ),
	  ( Sign1 = positive ->
	    NG is NPT+NPA, NGO is NNT+NNA, Sign1O = negative
	  ; NG is NNT+NNA, NGO is NPT+NPA, Sign1O = positive ),
	  info(40, 'Picking ~w from the ~w ~w clauses as goals (there would be ~w ~w)',
	       [Pick, NG, Sign1, NGO, Sign1O]),
	  m_add_query(M1, [to=Sign1, pick=Pick], M2)
	; ( NNA = 0 ->
	    info(40, 'Picking ~w from the ~w negative theorem clauses as goals', [Pick, NNT]),
	    m_add_query(MT, [to=negative, pick=Pick], MT1)
	  ; NPA = 0 ->
	    info(40, 'Picking ~w from the ~w positive theorem clauses as goals', [Pick, NPT]),
	    m_add_query(MT, [to=positive, pick=Pick], MT1)
	  ; length(MT, NG),
	    info(40, 'Picking ~w from the ~w theorem clauses as goals', [Pick, NG]),
	    m_add_query(MT, [pick=Pick], MT1)
	  ),
	  append(MT1, MA, M2)
	),
	append([[~'$query']], M2, M).

maq_pick(N, Options) :-
	from_options(pick=Pick, Options, all),
	( Pick = all ->
	  true
	; Pick = N ->
	  true
	; Pick = [_|_] ->
	  memberchk(N, Pick)
	).

m_add_query(M, Options, M1) :-
	m_add_query(M, 1, Options, M1).

m_add_query([C|M], N, Options, M1) :-
	( from_options(to=negative, Options, all) ->
	  c_is_negative(C)
	; from_options(to=positive, Options, all) ->
	  c_is_positive(C)
	; true
	),
	!,
	( maq_pick(N, Options) ->
	  M1 = [['$query'|C]|M2]
	; M1 = M2
	),
	N1 is N+1,
	m_add_query(M, N1, Options, M2).
m_add_query([C|M], N, Options, [C|M1]) :-
	m_add_query(M, N, Options, M1).
m_add_query([], _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_prepare(MT, MA, Options, M, Features) :-
	info(10, 'Preparing task matrix'),
	%% pp(MT), nl, pp(MA), nl,
	m_set_goal_clauses(MT, MA, Options, M2),
	m_features(M2, Features1),
	%% asserta(mmmpr0(M2)),
	m_set_equality(M2, Features1, Options, M3, Features),
	m_simp_prepare(M3, Features, Options, M4),
	m_info(0, M4, 'Preparation result'),
	m_info_features(20, Features, 'Preparation result'),
	M = M4.

m_set_equality(M, Features, Options, M1, Features1) :-
	( memberchk(equality, Features) ->
	  info(10, 'Matrix has equality feature'),
	  DEFAULT_EQUALITY_MODE = std_r_st,
	  %% DEFAULT_EQUALITY_MODE = steq,
	  from_options(eq=Equality, Options, DEFAULT_EQUALITY_MODE) ->
	  ( ( Equality=std ->
	      info(10, 'Adding equality: subs + ref + sym + trans'),
	      m_add_equality_r_s_t(M, M1)
	    ; Equality=std_r_st->
	      info(10, 'Adding equality: subs + ref + symtrans'),
	      m_add_equality_r_st(M, M1)
	    ; Equality=std_r_ts->
	      info(10, 'Adding equality: subs + ref + transsym'),
	      m_add_equality_r_ts(M, M1)
	    ) ->
	    Features1 = Features
	  ; Equality=steq ->
	    info(10, 'Adding equality: steq'),
	    from_options(lex=Lex, Options, NoLexSpecified),
	    from_options(steq_options=STEQ_Options, Options, []),
	    ( Lex \== NoLexSpecified ->
	      STEQ_Options1 = [lex(Lex)|STEQ_Options]
	    ; STEQ_Options1 = STEQ_Options
	    ),
	    m_add_equality_steq(M, STEQ_Options1, M1),
	    Features1 = [steq|Features]
	  ; info(10, 'Not adding equality axioms'),
	    M1 = M,
	    Features1 = Features
	  )
	; M1 = M,
	  Features1 = Features
	).
	
m_simp_prepare(M, Features, Options, M1) :-
	from_options(simp_prepare=Simp,
		     Options,
		     [purity, elim_cheap]),
	m_simp_prepare_1(Simp, M, Features, Options, M1).


m_simp_prepare_1([S|Ss], M, F, O, M1) :-
	simp_prepare(S, M, F, O, M2),
	m_simp_prepare_1(Ss, M2, F, O, M1).
m_simp_prepare_1([], M, _, _, M).

simp_prepare(purity, M, _, _, M1) :-
	!,
	predicates_to_protect(M, Keep),
	m_red_purity(M, Keep, M1).
simp_prepare(elim_cheap, M, _, _, M1) :-
	!,
	predicates_to_protect(M, Keep),
	m_red_elim_cheap(M, Keep, M1).
simp_prepare(purity(KeepPs), M, _, _, M1) :-
	!,
	predicates_to_protect(M, Keep),
	append(Keep, KeepPs, Keep1),
	sort(Keep1, Keep2),
	m_red_purity(M, Keep2, M1).
simp_prepare(elim_cheap(KeepPs), M, _, _, M1) :-
	!,
	predicates_to_protect(M, Keep),
	append(Keep, KeepPs, Keep1),
	sort(Keep1, Keep2),
	m_red_elim_cheap(M, Keep2, M1).


simp_prepare(X, _, _, _, _) :-
	err('No such prepare simplification: ~q', [X]).

predicates_to_protect(M, Ps) :-
	m_predicates(M, Ps1),
	findall(P/N, (member(P/N, Ps1), protected(P)), Ps).

protected('$query') :- !.
protected(P) :- atom_prefix(P, '$s').
protected(P) :- atom_prefix(P, '$prolog').
protected(P) :- atom_prefix(P, '$constrain').
protected(P) :- atom_prefix(P, '$cost').
protected(P) :- atom_prefix(P, '$eq').

% m_simp_prepare(M, _, _, M1) :-
% 	!,
% 	fol_red_pure(M, M1).

% m_simp_prepare(M, _, _, M1) :- 
% 	fol_red_condense(M, M1).
	
% m_simp_prepare(M, _, _, M1) :-
% 	info(0, 'Adding factors'),
% 	fol_add_factors(M, 2, M1),
% 	info(0, 'Adding factors done').

% % m_prepare_short(M, M1) :-
% % 	binres(M, [binres_units=pos], M1),
% % 	!.
% 
% 
% m_prepare_short(M, M1) :-
% 	binres(M, [binres_units=pos], M2),
% 	lub_loop(M2, 9, M1),
% 	!.
% 	
% m_prepare_short(M2, M) :-
%  	lub_loop(M2, 8, M4),
% % 	info(10, 'LUB'),
% % 	time(lub_step(M2, M3)),
% % 	info_effect(lub, M2, M3),
% % 	info(10, 'UNF'),
% % 	time(fol_red_unfounded(M3, M4)),
% % 	info_effect(unf, M3, M4),
% 	m_info(0, M4),
% 	info(10, 'SRT'),
% 	fol_sort_lits_by_pred_occurrences(M4, M5),
% 	%% *** CARE ABOUT QUERY !!!
% %	binres(M5, [binres_units=pos], M6),
% %	binres(M5, [binres_add_query=neg], M6),
% %	binres(M5, [], M6),
% 	M5 = M6,
% 	M = M6.


fol_sort_clauses_for_horn(M, M1) :-
	map_csfh(M, M1).

csfh(C, [L|C1]) :-
	select(L, C, C1),
	L \= ~(_),
	!.
csfh(C, C).

map_csfh([X|Xs], [X1|Xs1]) :-
	csfh(X, X1),
	map_csfh(Xs, Xs1).
map_csfh([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c_is_positive(C) :-
	\+ ( member(~(A), C), \+ a_is_builtin(A) ).
c_is_negative(C) :-
	\+ ( member(L, C), L \= ~(_) ).

a_is_builtin('$prolog'(_)).
a_is_builtin('$prolog'(_,_)).
a_is_builtin('$prolog_post'(_)).
a_is_builtin('$prolog_post'(_,_)).
a_is_builtin('$hconstrain'(_,_,_)).
a_is_builtin('$options'(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_info(V, M) :-
	m_info(V, M, '').

m_info(V, M, Header) :-
	length(M, NC),
	m_num_of_literal_occurrences(M, NL),
	m_max_c_size(M, CMax),
	m_num_of_units(M, NUnits),
	findall(k, member([_,_], M), Bins),
	length(Bins, NBins),
	NNonunits is (NC - NUnits) - NBins,
	( NL > 0 -> CAvg is NL / NC ; CAvg = 0 ),
	info(V, '--------------------'),
	( Header = '' -> true ;	info(V, 'Matrix info about: ~w', [Header]) ),
	info(V, 'Matrix info: ~w clauses, ~w literal occurrences', [NC, NL]),
	info(V, 'Matrix info: ~w units, ~w binary, ~w >= 3', [NUnits, NBins, NNonunits]),
	info(V, 'Matrix info: avg clause size: ~1f, max clause size: ~w',
	     [CAvg, CMax]).

m_num_of_units(M, N) :-
	m_num_of_units(M, 0, N).
m_num_of_units([[_]|Cs], SoFar, N) :-
	!,
	N1 is SoFar + 1,
	m_num_of_units(Cs, N1, N).
m_num_of_units([_|Cs], SoFar, N) :-
	!,
	m_num_of_units(Cs, SoFar, N).
m_num_of_units([], N, N).

m_num_of_literal_occurrences(M, N) :-
	m_num_of_literal_occurrences(M, 0, N).
m_num_of_literal_occurrences([C|Cs], SoFar, N) :-
	length(C, N1),
	N2 is N1+SoFar,
	m_num_of_literal_occurrences(Cs, N2, N).
m_num_of_literal_occurrences([], N, N).

m_max_c_size(M, N) :-
	m_max_c_size(M, 0, N).
m_max_c_size([C|Cs], SoFar, N) :-
	length(C, N1),
	N2 is max(N1, SoFar),
	m_max_c_size(Cs, N2, N).
m_max_c_size([], N, N).


map_key([X-_|Xs], [X|Xs1]) :-
	map_key(Xs, Xs1).
map_key([], []).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Incomplete Backtracking as in LeanCop
%%%% 

m_add_cuts(M, none, _, M) :-
	!.
m_add_cuts(M, Mode, CMOptions, M1) :-
	map_add_cutcode(M, Mode, CMOptions, M1).
	
map_add_cutcode([X|Xs], Mode, Y1, [X1|Xs1]) :-
	add_cutcode(Mode, X, Y1, X1),
	map_add_cutcode(Xs, Mode, Y1, Xs1).
map_add_cutcode([], _, _, []).

add_cutcode(cut, C, Os, [~Call|C]) :-
	!,
	mk_post_code( !, _Depth, Os, Call ).
add_cutcode(cut_below(Depth), C, Os, [~Call|C]) :-
	!,
	mk_post_code( ( Depth1 < Depth -> ! ; true ), Depth1, Os, Call ).
add_cutcode(Mode, _, _, _) :-
	err('Undefinded mode for adding cuts: ~q', [Mode]).

mk_post_code(Call, Depth, Os, '$prolog_post'(Call, I)) :-
	constr_i(Os, I, [d(Depth-_)]).

mk_pre_code(Call, Depth, Os, '$prolog'(Call, I)) :-
	constr_i(Os, I, [d(Depth-_)]).


% 	mk_post_code( ((50 < random(100)) -> ! ; true), _Depth, Os, Call ).
%	mk_post_code( !, _Depth, Os, Call ).
%	mk_post_code( ( Depth > 5 -> ! ; true ), Depth, Os, Call ).
%	mk_post_code( ( Depth < 5 -> ! ; true ), Depth, Os, Call ).
%	mk_post_code( format('At dept ~q~n', [Depth]), Depth, Os, Call ).


% add_cutcode(C, Os, [~Pre,~Post|C]) :-
% %	mk_pre_code( ((60 < random(100)) -> ! ; true), _Depth, Os, Pre ),
% 	mk_pre_code( ((10 < random(100)) -> fail ; true), _Depth, Os, Pre ),
% 	mk_post_code( ((60 < random(100)) -> ! ; true), _Depth, Os, Post ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
writex(X) :-
	copy_term(X, X1),
	numbervars(X1, 0, _),
	writex1(X1).

writex1(X) :-
	atomic(X),
	!,
	writeq(X).
writex1('$VAR'(N)) :-
	!,
	write_term('$VAR'(N), [numbervars(true)]).
writex1(X) :-
	X =.. [F|Args],
	writeq(F),
	write('('),
	writex1_args(Args),
	write(')').

writex1_args([X]) :-
	!,
	writex1(X).
writex1_args([X|Xs]) :-
	writex1(X),
	write(','),
	writex1_args(Xs).
