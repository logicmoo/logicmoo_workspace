%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 1992,1993,1996,1997,1998,2015,2016 Christoph Wernhard
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
%%%% CM-PR  Version: 1.7, May 2016
%%%%
%%%% PTTP-like prover (compiler & runtime system). After the papers on PTTP
%%%% [Stickel 88] and Setheo [SETHEO 92]. Didn't know about Stickel's
%%%% implementation of PTTP in Prolog at that time.
%%%% 
%%%% Just with the aim of learning and experimenting, not to write a 
%%%% competitive prover.
%%%% 
%%%% Inference Rules:
%%%% - Extension
%%%% - ME reduction
%%%% 
%%%% Supported Reductions:
%%%% - Subsumption and tautology nequations
%%%% - Identical ancestor, complementary ancestor, unit subsumption 
%%%%
%%%% Advantages:
%%%% - Small
%%%% - Integration into prolog for query, answer and proof (tableaux)
%%%%   processing
%%%%
%%%% Performance:
%%%% - 28 [Setheo: 31] problems would have been solved at the CADE-13 
%%%%   competition
%%%% - 28 at CADE-14: Horn with no equality:      13 [Setheo: 14]
%%%%                  Horn with equality:          1 [Setheo:  4]
%%%%                  Non-horn with no equality:   8 [Setheo:  8]
%%%%                  Non-horn with equality:      6 [Setheo: 11]
%%%% 
%%%% [Such results often depend very much on clause ordering, they were all
%%%% obtained using the same ordering algorithm (sort_cps_old/2) applied to
%%%% the problem as stated in the original tptp file. Since in subsequent
%%%% versions of the prover I tried to make the clause ordering less
%%%% dependent on the input ordering, the test results might be different
%%%% with the current version of the prover.]
%%%% [Options used: non-horn: [r1,r4,r5,hd], horn: [r4,r5,hd,mrn,no_fan]]
%%%% [Now I use different standard options:
%%%%  non-horn: [r1,r4,r5,hd1], horn: [r4,r5,hd1,mrn,no_fan]] r6?
%%%%
%%%% doc.txt contains usage documentation.
%%%%
%%%% History: First quick version written in winter 92/93 using SB-Prolog.
%%%% Ported to Quintus Prolog and improved in spring 93. Winter 96/97: port
%%%% to Eclipse Prolog, simplification, fixes & improvements.
%%%% 
%%%% (c) 1992,1993,1996,1997,1998 Christoph Wernhard
%%%% 
%%%% Fri Jun 26 19:29:12 1998
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/******************************************************************************
CHANGES

1.3 October 2014

* adaption and cleanup: SWI-Prolog 7, use with other code
* proof terms for builting $prolog

1.2

* experimental use_bound (cm option 'ub', proof option u(N)) added:
  limit number of uses of a nonunit clause for extension to N.
  (Something like this was used in the tableaux prover described
  by Harrison for mizar. First tests with CM show no positive results.)
  notice: use_bound might not work with freezesolve and runsolve.
  (I left the implementation because it perhaps can be extended to
  something like resource orienteded planning and might be serve as
  pattern for other extensions).
* added heuristic support for '$query' to hd1
* added ws bound (works not with inc)
* added steq support

1.1

* term handling is now in a separate module
* code should now be ready for using the Eclipse handler stuff
* added '$eq'/2 builtin

******************************************************************************/
/*****************************************************************************

TODO/BUGS

Separate the runtime system - different versions for different
purposes: - prolog system dependent, - generic, - for partial
evaluation. I haven't done that yet since maintenance is simpler.

There is still the kb, or M argument around (from old version, (used
to distingush kb/query)) (now only in the compiler, not the
generated code) (harmless)
 
the cost clause probably makes only sense with 'hs', otherwise it
is redundant (but harmless)

\n within quoted term-names is not handled as newline by all prologs

Check/test the heuristic subgoal ordering, adjusting of parameters...

Check/test r6 (antilemma stuff)

not sure whether freezesolve, runsolve work with hs and hw

a query matrix might not get full reductions (r4,r5) etc.
use of an artifial start literal '$query' is recommended.


******************************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CM - THE COMPILER
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SYSTEM DEPENDENT: ECLIPSE
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% REMARKS
%% 
%% *** for now: compile the output file of cm/3 into the module cm
%%     (since it requires the runtime stuff from cm)
%% 
%% nodbgcomp.
%% ("no debug compilation mode") makes things faster (see eclipse doc).
%% 
%% set_flag(occur_check, on). 
%% (While compiling libraries, cm.pl and later the output files)
%% and setting the 'oen' and 'orn' options (see doc.txt) speeds up
%% a little ---- but see following remark.
%% 
%% remark: there is a bug with eclipse's occur check, so using
%% oen and orn with occur_check on can cause unsoundness, example:
%% tptp wos20. Using the "procom"-version of an explicit unify 
%% (and not using oen/orn) seems to work correctly.
%%
%% remark: eclipse's write/1 writes the original names of variables
%% even if they are distinct objects (so the solutions displayed in
%% the console might look confusing).
%%
%% remark: eclipse's true is not a real "skip", the compiled code
%% shown by als/1 is different.
%%

%% 
%% now using eclipse's delayed inequality ~= to implement
%% a syntactic equality builtion $eq/2
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cm, [ cm/3, 
                proof/1,
                proof/2,
                describe_i/0,
		describe_i/1,
		constr_i/3,
		depth_bound_reached/0
              ]).

:- use_module(general_cm).
:- use_module(swilib(term_handling)).
:- use_module(swilib(err)).
:- use_module(swilib(pretty)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(info)).
:- use_module(swilib(sysdep)).
:- use_module(toytools(lrpo)).

% cm_version('1.80').

:- (prolog_flag(dialect, yap) -> use_module(swilib(yap_support)) ; true ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SYSTEM DEPENDENT: SWI
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def_flag(Flag) :- flag(Flag, _, 0).
setval(Flag, New) :- flag(Flag, _, New).
getval(Flag, Value) :- flag(Flag, Value, Value).
incval(Flag) :-	flag_inc(Flag, _).
% decval(Flag) :-	flag(Flag, Old, Old-1).

def_array(_Id, _Size) :- true.
setval(_Array, _I, _New) :- writeln('Arrays are not implemented !'), abort.
getval(_Array, _I, _Value) :- writeln('Arrays are not implemented !'), abort.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cm_verbose :-
 	get_info_verbosity(V),
	V >= 8.

%% "stderr" like output for monitoring compilation:
%%
dot(_) :- ( cm_verbose -> fail ; ! ).
dot(X) :- write(user_error, X), flush_output(user_error).

% info(_) :- ( cm_verbose -> fail ; ! ).
% info(X) :-
% 	( stream_property(user_error, position(Pos)),
% 	  stream_position_data(line_position, Pos, N),
% 	  N =\= 0 ->
% 	  nl(user_error)
% 	; true
% 	),
% 	write(user_error, '% '),
% 	write(user_error, X),
% 	flush_output(user_error).

% cm_banner :- cm_version(V), info(10, 'CM Version: ~w', [V]).
% pr_banner :- cm_version(V), info(10, 'PR Version: ~w', [V]).

cm_banner :- info(10, 'CM Prover: Compiler').
pr_banner :- info(10, 'CM Prover: Runtime System').

% no_single_var_check_declaration((:- set_flag(variable_names, off))).

no_single_var_check_declaration((:- style_check(-singleton))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Runtime Stuff
%%%% 

runinfo(_) :- ( cm_verbose -> fail ; ! ).
runinfo([X|Xs]) :- runinfo1(X), runinfo(Xs).
runinfo([]) :- flush_output(user_error).

runinfo1(nl) :-
	!,
	stream_property(user_error, position(Pos)),
	stream_position_data(line_position, Pos, N),
	( N =\= 0 ->
	  write(user_error, '\n% ')
	; write(user_error, '% ')
	).
runinfo1(d5(N)) :- 
	!,
	write_natnum(user_error, N, 15).
runinfo1(d10(N)) :-
	!,
	write_number(user_error, N, 10).
runinfo1(X) :- write(user_error, X).


%%%% 
%%%% Bound Increments
%%%% 

:- def_flag(bound_increment).

init_bound_increment(D) :- setval(bound_increment, D).

get_bound_increment(D) :- getval(bound_increment, D).

%% The macro version cannot be overdefined after loading this file by
%% extensions implemented as patches.
%
% def_inline( subsumes_chk(A, B), (copy_term(A,A1),
%                                  compare_instances(C,B,A1), 
%                                  once((C = < ; C = (=))))).
% def_inline( subsumes_chk_noshare(A, B), (compare_instances(C,B,A), 
% 	                                 once((C = < ; C = (=))))).
% :- define_macro( subsumes_chk/2, def_inline/2, []).
% :- define_macro( subsumes_chk_noshare/2, def_inline/2, []).% 

/*
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%% 
% %%%% Inline Version of hw Stuff
% %%%% 
% 
% hw_def_inline(hw_wdb_w1(N, _L, N1),
% 	N1 is N - 1).
% hw_def_inline(hw_wdb_w2(N, L, N1),
% 	N1 is N - L).
% hw_def_inline(hw_wdb_w3(N, DeltaI, N1),
% 	N1 is N / (1 + DeltaI)).
% 
% :- define_macro(hw_wdb_w1/3, hw_def_inline/2, []).
% :- define_macro(hw_wdb_w2/3, hw_def_inline/2, []).
% :- define_macro(hw_wdb_w3/3, hw_def_inline/2, []).
% 
% hw_def_inline_1(hw_set_resources(NFree, NAdd, InfIn, InfOut, N1),
% 	( DeltaI is InfOut - InfIn,
% 	  hw_wdb_w3(NAdd, DeltaI, NAdd1),
% 	  N1 is NFree + NAdd1
%         )).
% 
% :- define_macro(hw_set_resources/5, hw_def_inline_1/2, []).
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% subsumes_chk_pe/2
%%%% 
%%%% A very efficient version of subsumes_chk with the following
%%%% restrictions:
%%%% 
%%%% - the first argument (the more general one in case of success)
%%%%   must already be there at compile (= macro expansion) time.
%%%%   
%%%% - the first argument shares no variables with any other term,
%%%%   including the second argument.
%%%%
%%%% *** The current implementation generates predicates pe_#/1
%%%% without removing them.
%%%%
% *** times cm, compile_term, compile_file ...
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Auxiliary predicates (... there is no 'lambda' in prolog) can be
%% "dropped" during cm/3 to the dynamic predicate pe_term.
%% 

:- dynamic( pe_term/1 ).

init_pe_terms :-
	retractall( pe_term(_) ).

mk_pe_terms(Terms) :-
	findall(Clause, (pe_term(Term), member(Clause, Term)), Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- def_flag(gen_pe_pred_name).

init_gen_pe_pred_name :- setval(gen_pe_pred_name, 0).

gen_pe_pred_name(V) :-
	incval(gen_pe_pred_name),
	getval(gen_pe_pred_name, N),
	concat_atom(['pe_', N], V).

:- init_gen_pe_pred_name.


% % version 2 - fastest (with Eclipse)
% 
% subsumes_chk_pe_call(A, B, Call) :-
% 	gen_pe_pred_name(V),
% 	Call =.. [V,B],
% 	Def =.. [V,A],
% 	assert( pe_term([Def :- -?-> true]) ).
% 

% version 1 
subsumes_chk_pe_call(A, B, Call) :-
	Call = run_subsumes_chk_noshare(A, B).


% could perhaps be used when reading files:
%
% def_gen_pe_pred( subsumes_chk_pe(A, B), Call) :-
% 	gen_pe_pred_name(V),
% 	Call =.. [V,B],
% 	Def =.. [V,A],
% 	compile_term([Def :- -?-> true]).
% 
% :- define_macro( subsumes_chk_pe/2,  def_gen_pe_pred/2, []).
% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% "Standard" library stuff
%%%%

subseq([], [], []).
subseq([Head|Tail], Sbsq, [Head|Cmpl]) :-
	subseq(Tail, Sbsq, Cmpl).
subseq([Head|Tail], [Head|Sbsq], Cmpl) :-
	subseq(Tail, Sbsq, Cmpl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% END - FOR ECLIPSE
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- op(550, fy, user:(~)).

remove_keys([_-V|Vs], [V|Vs1]) :-
	remove_keys(Vs, Vs1).
remove_keys([], []).

cm_option(O,Os) :- memberchk(O,Os).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CM
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cm(M,KB,Os) :-
  cm_banner,
  init_pe_terms,
  check_cm_options(Os),
  default_abstrterm(M,M1),
  
  M1 = _-InputClauses,
  length(InputClauses, LastClauseIndex),
  NextClauseIndex is 1 + LastClauseIndex,

  matrix_functions(InputClauses, Functions),

  matrix_to_cps(M1,1,PREs,MEDs,POSs,Predicates,ignore,Os),

  info(20, 'Merging contrapositives'),
  
  merge_cps(PREs,MEDs,CPs1),
  merge_cps(CPs1,POSs,CPs),

  
  ( memberchk(lex(Lex), Os) ->
    check_lex(Lex, Functions)
  ; Lex=default
  ),
  
  ( nonvar(KB) ->
    info(20, 'Writing output file: ~w', [KB]),
    onto_file((
	       write('\n\n%%%%\n%%%% CM-PR Annotation Section\n%%%% '),
	       date_atom(Date), write(Date), write('\n%%%%\n\n'),
	       runtime_module_file_declarations(Os, ModuleDeclarations),
	       ( member(ModuleDeclaration,  ModuleDeclarations),
		 pp_clause(ModuleDeclaration), nl, fail
	       ; true
	       ),
	       no_single_var_check_declaration(NSVCD),
	       pp_clause(NSVCD), nl,
	       writeq(dec_next_clause_index(NextClauseIndex)), write('.'), nl,
	       writeq(dec_kb_predicates(Predicates)), write('.'), nl,
	       writeq(dec_kb_functions(Functions)), write('.'), nl,
	       writeq(dec_kb_lex(Lex)), write('.'), nl,
	       writeq(dec_cm_options(Os)), write('.'), nl,
	       write('\n\n%%%%\n%%%% CM-PR Base Section\n%%%% '),
	       date_atom(Date1), write(Date1), write('\n%%%%\n\n'),
	       write('\n%%%\n%%% Built-Ins.\n%%%\n\n'),
	       mk_builtin_clauses(Os, BINS),
	       ( member(BIN, BINS),
		 pp_clause(BIN), nl, fail
	       ; true
	       ),
	       write('\n%%%\n%%% Auxiliary \"PE\" Predicates.\n%%%\n\n'),
	       mk_pe_terms(PEs),
	       ( member(PE, PEs), pp_clause(PE), fail ; true ),
	       write('\n%%%\n%%% Contrapositives.\n%%%\n\n'),
	       length(CPs, LenCPs),
	       info(20, 'Writing ~w target clauses', [LenCPs]),
	       info_progress_start,
	       ( cm_option(ff, Os) ->
		 write_cps_ff(CPs)
	       ; cm_option(fq, Os) ->
		 write_cps_fq(CPs)
	       ; write_cps(CPs)
	       ),
	       info_progress_done,
	       write('\n\n%%% File End\n')
	      ),
	      KB),
    info(20, 'Written output file')
  ; 
    info(20, 'Preparing clause list'),
    C_NC = dec_next_clause_index(NextClauseIndex),
    C_KB = dec_kb_predicates(Predicates),
    C_FS = dec_kb_functions(Functions),
    C_LE = dec_kb_lex(Lex),
    C_OPTIONS = dec_cm_options(Os),
    mk_builtin_clauses(Os, C_BINs),
    mk_cp_clauses(CPs, C_CPs),
    mk_pe_terms(PEs),
    append(PEs, C_CPs, Clauses1),
    append(C_BINs, Clauses1, Clauses2),
    KB1 = [C_NC, C_KB, C_FS, C_LE, C_OPTIONS | Clauses2],
    runtime_module_term_declarations(Os, ModuleDeclarations),
    append(ModuleDeclarations, KB1, KB)
      
  ),
  !. % blue cut


runtime_module_term_declarations(_, ModuleDeclarations) :-
	ModuleDeclarations =
	[ (:- use_module(cmprover(runtime_support_cm))),
	  (:- use_module(library(lists)))
	  % , (:- set_prolog_flag(unknown, fail)
	].

runtime_module_file_declarations(Options, ModuleDeclarations) :-
	run_module(Options, RUNCM),
	ModuleDeclarations =
	[ (:- module(RUNCM, [])),
	  (:- use_module(cmprover(runtime_support_cm))),
	  (:- use_module(library(lists)))	  
%	  , (:- set_prolog_flag(unknown, fail))
	  ].

check_cm_options(Options) :-
	( member(Option, Options),
	  \+ memberchk(Option, [r1,r2,r3,r4,r4a,r5,r6,r7(_),r8(_),
				hs,hd,hd1,hd2,hw,hb,hb1,
				hd1x(_), hdx(_), hsx(_),
				hform(_,_,_),
				hdef(_,_,_,_),hsub(_,_,_,_),heq(_,_,_,_,_),
				hc_ins, hc_off, hc_susp, hc_post, hc_post_accu,
				hc_list,
				r4_limit(_), r4a_max(_),
				steq, lex(_),
				steq_s_off, steq_d_off, steq_x_off,
				steq_a_off,
				ub,
				spec(_,_),
				cost(_),
				scn,
				sn,sh,sg(_),sq(_),
				incs,
				inc,inc1,t,p,mrn,oen,orn,rus,
				mrc, mr2,
				acn,
				ordp,ordp1,
				l,
				xlc,
				frs,no_fan,xv1,tunit,
				ff, fq,
				run_module(_)]) ->
	  err('Bad option as input to cm: ~q', [Option])
	; true
	).

default_abstrterm(T-G,T-G).
default_abstrterm([C|Cs],k-[C|Cs]).
default_abstrterm([],k-[]).

check_lex(X, _) :-
	atom(X),
	!.
check_lex(Spec, Functions) :-
	member(F/N, Functions),
	\+ memberchk(F/N, Spec),
	\+ memberchk(F/N-_, Spec),
	!,
	err('A function is missing in the lex specification: ~q', [F/N]).
check_lex(_, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% MATRIX_TO_CPS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% matrix_to_cps returns contrapositives (cps) as terms
%%% scp(head,pre,body,value), pre is a part of the body that comes before
%%% the "contrapositive" body literals (eg occur checks, depth-decrement).
%%% where value is the number of subgoals for heuristic purposes (usually
%%% not identical to number of literals in the body).
%%% cp_to_prolog_clause(scp(H,[],_),H).
%%% 
%%% PREs and POSs are both sorted (see merge_cps) lists of cps: the cps in
%%% PREs have to be asserted before the cps in POSs. furthermore, the cps
%%% in PREs depend only of the options Os: they have to be asserted for
%%% each predicate only once.
%%% 
%%% the M parameter is simply shared by the i-parts of all cps, it is used
%%% to distinguish clauses which belong to the kb from clauses installed
%%% for solving a particular query.
%%% 
%%% Sun Dec 15 20:03:46 1996:
%%% returns now PRE, MED, POS
%%% - PRE "before" stuff
%%% - MED the main contrapositives (former POS)
%%% - POS "after" stuff
%%% 
%%% - FNs is a list of F-N of all predicate symbols (including any extra
%%%   args) used in the proof (does not contain builtins and runtime
%%%   support predicates) (what about "rewrite" versions of predicates
%%%   ???) compilation uses some (arbitrary) ordering of the F-Ns which is
%%%   accessible at runtime through dec_kb_predicates/1
%%% 

matrix_to_cps(T-Cs,IndexOfFirstClause,PREs,MEDs,POSs,FNs,M,Os) :-
  info(20, 'Translation to contrapositives'),
  matrix_predicates(Cs,FNs),
  ( memberchk('c_='-_, FNs) -> Equality = true ; Equality = false ),
  Cs1 = Cs,
  ( (cm_option(r4,Os);cm_option(r4a,Os);cm_option(r5,Os)) ->
    prepare_reqs_binding_list(Cs1,Os,Bindings)
  ; true
  ),

  length(Cs1, LenCs1),

  info(20, 'Converting ~w clauses to contrapositives', [LenCs1]),
  info_progress_start,
  matrix_to_cps1(Cs1,T,Bindings,CPs1,_Runsolve,M,FNs,IndexOfFirstClause,Os),
  info_progress_done,

  ( cm_option(scn, Os) ->
    MEDs1 = CPs1
  ; length(CPs1, LenCPs1),
    info(20, 'Sorting ~w contrapositives', [LenCPs1]),
    info_progress_start,
    sort_cps(CPs1,MEDs1),
    info_progress_done
  ),
  
  ( cm_option(sn,Os) -> 
    MEDs2 = MEDs1
  ; ( cm_option(sio,Os) ->
      info(20, 'Sorting subgoals - io mode'),
      info(20, 'Sorting subgoals - io mode is no longer supported'),
      %% io_sort_subgoals(MEDs1,MEDs1,M,MEDs2)
      fail
    ; cm_option(sg(Mode),Os) ->
      info(20, 'Sorting subgoals - greedy heuristic mode'),
      sg_sort_subgoals(MEDs1,Mode,full,MEDs2)
    ; cm_option(sq(Mode),Os) ->
      info(20, 'Sorting subgoals - quick heuristic mode'),
      sg_sort_subgoals(MEDs1,Mode,quick,MEDs2)
    ; info(20, 'Sorting subgoals - heuristic mode'),
      sh_sort_subgoals(MEDs1,MEDs2)
    )
  ),

  ( Equality = true, cm_option(steq, Os), \+ cm_option(steq_s_off, Os) ->
    info(20, 'STEQ: Re-sorting subgoals'),
    steq_after_sort_subgoals(MEDs2, MEDs2c)
  ; MEDs2c = MEDs2
  ),
  
  cps_disable_unknowns(MEDs2c, FNs, MEDs2b, _FNs1),

  info(20, 'Adding infos to contrapositives'),
  cps_set_body_is(MEDs2b,Os),

  ( cm_option(hw, Os) ->
    hw_insert_set_resources(MEDs2b, Os, MEDs2a)
  ; cm_option(hb1, Os) ->
    hb_insert_set_resources(MEDs2b, Os, MEDs2a)
  ; MEDs2a = MEDs2b
  ),

  ( (cm_option(r4,Os) ; cm_option(r4a,Os) ; cm_option(r5,Os)) ->
    ( info(20, 'Adding taut/subs nequations to contrapositives'),
      ( cm_option(rsub,Os) ->
	info(20, 'Using subsumes_chk for r4 and r5')
      ; true
      ),
      info_progress_start,
      insert_red_equations(MEDs2a,Os,MEDs3),
      info_progress_done
    )
  ; MEDs3 = MEDs2a
  ),

  info(20, 'Preparing hconstraints'),
  insert_hconstraints(MEDs3, Os, FNs, MEDs3a),

  info(20, 'Preparing built-ins'),
  unwrap_built_ins(MEDs3a, Os, MEDs4),	
  
  ( cm_option(mrn, Os) ->
    MeReds = []
  ; info(20, 'Me-reduction clauses'),
    mereds_from_cps(FNs,MeReds,M, Os)
  ),
  ( cm_option(l, Os) ->
    info(20, 'Lemma solution clauses'),
    lemsols_from_cps(FNs,LemSols,M,Os)
  ; LemSols = []
  ),
  ( cm_option(r1, Os) ->
    info(20, 'Red1 clauses'),	
    red1s_from_cps(FNs,R1s,M, Os)
  ; R1s = []
  ),
  ( cm_option(r2, Os) ->
    info(20, 'Red2 clauses'),	
    red2s_from_cps(FNs,R2s,M, Os)
  ; R2s = []
  ),
  ( cm_option(r3, Os) ->
    info(20, 'Red3 clauses'),	
    red3_cps(Cs1,M, Os,R3s)
  ; R3s = []
  ),
  ( cm_option(r7(R7Factor), Os) ->
    info(20, 'Red7 clauses'),	
    red7s_from_cps(FNs, R7s, R7Factor, M, Os) ;
    R7s = []),
  ( cm_option(rus, Os) ->
    info(20, 'Runsolve clauses'),
    runsolve_cps(FNs,M, Os,RunsolveCPs)
  ; RunsolveCPs = []
  ),
  ( cm_option(frs, Os) ->
    info(20, 'Freezesolve clauses'),
    freezesolve_cps(FNs,M, Os,FreezesolveCPs)
  ; FreezesolveCPs = []
  ),

  PREs1 = R3s,

  merge_cps(PREs1, R7s, PREs1a),
  merge_cps(PREs1a, R1s, PREs2),
  merge_cps(PREs2, R2s, PREs3),

  % me-extension, runsolve, me-reduction, can happen theoretically in
  % any order - experiment with different orderings?
  % for now:	

  % freezesolve as pres since "goal related"???

  append(RunsolveCPs, FreezesolveCPs, POSs1),

  (   cm_option(co1,Os) ->         
      %% me-reduction after extension (perhaps makes no sense)
      merge_cps(POSs1, MeReds, POSs2),
      PREs4 = PREs3

  ;   %% me-reduction before extension
      merge_cps(PREs3, MeReds, PREs4),
      POSs2 = POSs1
  ),

  merge_cps(PREs4, LemSols, PREs4a),

  ( cm_option(r8(R8Mode), Os) ->
    info(20, 'Local solution subs wrappers'),
    rename_heads(PREs4a, lss_, PREs5),	
    rename_heads(MEDs4, lss_, MEDs5),	
    rename_heads(POSs2, lss_, POSs3),	
    lss_cps(FNs, R8Mode, Os, LssCPs),
    append(LssCPs, PREs5, PREs6) % needs no merge since different predicates 
  ;  PREs6 = PREs4a, MEDs5 = MEDs4, POSs3 = POSs2
  ),

  
%   ( cm_option(r6, Os) ->
%     info(20, 'Antilemma wrappers'),
%     rename_heads(PREs4a, alem_, PREs5),	
%     rename_heads(MEDs4, alem_, MEDs5),	
%     rename_heads(POSs2, alem_, POSs3),	
%     alem_cps(FNs, Os, AlemCPs),
%     append(AlemCPs, PREs5, PREs6) % needs no merge since alem defines
%                                   % different predicates 
%   ;  PREs6 = PREs4a, MEDs5 = MEDs4, POSs3 = POSs2
%   ),

  PREs = PREs6,
  MEDs = MEDs5,
  POSs = POSs3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ME REDUCTION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mereds_from_cps(FNs,CPs1,M,Os) :-
	map_mered_cp(FNs,M,FNs,Os,CPs1).

map_mered_cp([],_,_,_,[]).
map_mered_cp([F-N|FNs],M,AllFNs,Os,[CP|CPs]) :-
	mered_cp(F,N,CP,M,AllFNs,Os),
	map_mered_cp(FNs,M,AllFNs,Os,CPs).

mered_cp(F,N,scp(H,B,[],[comment('%% ME-Reduction.')]),_M,FNs,Os) :-
	n_var_list1(N,Xs,I),
	ct_make_ancestor_table(FNs,Os,A),
	constr_i(Os,I,[d(D-D),a(A),fns(FNs),t(T-T),p(r(HP)),
	               inf(Inf-Inf), hc(Hc-Hc)]),
	H =.. [F|Xs], lit_complement(H,H1),
	proof_rep(H,HP),
	( cm_option(mrc, Os) ->
	  ct_ancestor_member_and_cut(H1,A,FNs,Os,MEMBER)
	; cm_option(mr2, Os) ->
	  ct_ancestor_member_mr2(H1,A,FNs,Os,MEMBER)
	; ct_ancestor_member(H1,A,FNs,Os,MEMBER)
	),
	B = MEMBER.

%%
%% representation of ancestors - a term containing a list for each
%% positive and negative predicate
%%

%% use this at runtime to create the intial empty ancestor table
%%	
make_empty_ancestor_table(FNs,_Os,Table) :-
	mea1(FNs,Args),
	Table =.. [a|Args].
mea1([],[]).
mea1([_|Xs],[[]|Xs1]) :- mea1(Xs,Xs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_make_ancestor_table(FNs,_Os,Table) :-
	ctma1(FNs,Args),
	Table =.. [a|Args].
ctma1([],[]).
ctma1([_|Xs],[_|Xs1]) :- ctma1(Xs,Xs1).

ct_add_ancestor(G,As,As1,FNs,_Os,Code) :-
	% hmm - here G has the original arity (without the extra arg)
	As =.. [a|List],
	functor(G,F,N1),
	N is N1 + 1,
	ctaa1(FNs,F-N,G,List,List1),
	As2 =.. [a|List1],
	Code = [As1 = As2].

ctaa1([F-N|_],F-N,G,[L|Ls],[[G|L]|Ls]) :- !.
ctaa1([_|FNs],F-N,G,[L|Ls],[L|Ls1]) :-
	ctaa1(FNs,F-N,G,Ls,Ls1).

ct_ancestor_absmember(G,As,FNs,_Os,Code) :-
	ct_ancestor_list(G,As,FNs,List),
	rem_argument(G,G1),
	Code = [run_absmember(G1,List)].

ct_ancestor_member(G,As,FNs,Os,Code) :-
	ct_ancestor_list(G,As,FNs,List),
	rem_argument(G,G1),
        ( cm_option(orn,Os) ->
	  Code = [member(G1,List)]
	; Code = [run_oc_member(G1,List)]
	).

% old version, the memberchk with cut seems to destroy
% completeness
%
% ct_ancestor_member(G,As,FNs,Os,Code) :-
% 	ct_ancestor_list(G,As,FNs,List),
% 	rem_argument(G,G1),
% 	( is_ground(G1) ->
%           Code = [memberchk(G1,List), !]
%         ; ( cm_option(orn,Os) ->
% 	    Code = [member(G1,List)]
%           ; Code = [run_oc_member(G1,List)]
%           )
%         ).

ct_ancestor_member_and_cut(G,As,FNs,_,Code) :-
	ct_ancestor_list(G,As,FNs,List),
	rem_argument(G,G1),
	Code = [run_oc_member(G1,List), !].

ct_ancestor_member_mr2(G,As,FNs,_,Code) :-
	ct_ancestor_list(G,As,FNs,List),
	rem_argument(G,G1),
	Code = [( run_absmember(G1, List) -> !
		; run_oc_member(G1, List)
		)].
        
% ct_ancestor_member_mr2(G,As,FNs,_,Code) :-
% 	ct_ancestor_list(G,As,FNs,List),
% 	rem_argument(G,G1),
% 	Code = [( member(X, List),
% 		  ( X == G1 -> !
% 		  ; unify_with_occurs_check(G1, X)
% 		  )
% 		)].

% auxiliary	

ct_ancestor_list(G,As,FNs,List) :-
	% auxiliary fun, not part of the interface
	% list of ancestors with given predicate and same sign
	As =.. [a|Lists],
	functor(G,F,N),
	ct_al2(FNs,F-N,Lists,List).

ct_al2([F-N|_],F-N,[List|_],List) :- !.
ct_al2([_|FNs],F1-N1,[_|Ls],L1) :- 
	ct_al2(FNs,F1-N1,Ls,L1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REDUCTION r1
%%%% 
%%%% If identical to ancestor literal then cut and fail.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red1s_from_cps(FNs,CPs1,M,Os) :-
	map_red1_cp(FNs,M,FNs,Os,CPs1).

map_red1_cp([],_,_,_,[]).
map_red1_cp([F-N|FNs],M,AllFNs,Os,[CP|CPs]) :-
	red1_cp(F,N,CP,M,AllFNs,Os),
	map_red1_cp(FNs,M,AllFNs,Os,CPs).

red1_cp(F,N,scp(H,B,[],[comment('%% Identical ancestor pruning [r1].')]),
        _M,FNs,Os) :-
	n_var_list1(N,Xs,I),
	ct_make_ancestor_table(FNs,Os,A),
	constr_i(Os,I,[a(A),fns(FNs)]),
	H =.. [F|Xs],
	ct_ancestor_absmember(H,A,FNs,Os,ABSMEMBER),
	append(ABSMEMBER, [!, fail], B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REDUCTION r2
%%%% 
%%%% If complement is identical to ancestor literal then me-reduce, cut.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red2s_from_cps(FNs,CPs1,M,Os) :-
	map_red2_cp(FNs,M,FNs,Os,CPs1).

map_red2_cp([],_,_,_,[]).
map_red2_cp([F-N|FNs],M,AllFNs,Os,[CP|CPs]) :-
	red2_cp(F,N,CP,M,AllFNs,Os),
	map_red2_cp(FNs,M,AllFNs,Os,CPs).

red2_cp(F,N,
        scp(H,B,[],[comment('%% ME-reduction with identical complement [r2].')]),
        _M,FNs,Os) :-
	n_var_list1(N,Xs,I),
	ct_make_ancestor_table(FNs,Os,A),
	constr_i(Os,I,[d(D-D),a(A),fns(FNs),t(T-T),p(r(HP)),
	               inf(Inf-Inf), hc(Hc-Hc)]),
	H =.. [F|Xs], lit_complement(H,H1),
	proof_rep(H,HP),
	ct_ancestor_absmember(H1,A,FNs,Os,ABSMEMBER),
	append(ABSMEMBER, [!], B).

% rem_argument(H,H1), ????


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REDUCTION r3
%%%% 
%%%% Unit clause subsumption.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% starts from the original matrix (not the cps)
%%

red3_cps(A,B,C,D) :-
	red3_cps1(A,B,C,D1),
	% consecutive predicates as required by merge_cps:
	sort(D1,D).

red3_cps1([],_,_,[]).
red3_cps1([[L]|Cs],M,Os,[CP|CPs]) :- !,
	red3_cp(L,M,Os,CP),
	red3_cps1(Cs,M,Os,CPs).
red3_cps1([_|Cs],M,Os,CPs) :-
	red3_cps1(Cs,M,Os,CPs).

red3_cp(L,_M,Os,
        scp(H,[run_subsumes_chk(H2,H1), !],[],
            [comment('%% Unit clause subsumption [r3].')])) :-
	cm_head_literal(L,I,Os,L1),
	functor(L1,F,N),
	n_var_list1(N,Xs,I),
	H =.. [F|Xs], 
	rem_argument(H,H1),
	rem_argument(L1,H2),
	proof_rep(H,HP),
	constr_i(Os,I,[d(D-D),t(T-T),p(e(HP,[])),
	               inf(Inf-Inf), hc(Hc-Hc)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% RUNSOLVE
%%%% 
%%%% Possibility to solve goals by clauses specified at runtime
%%%% (e.g. negated query)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runsolve_cps(FNs,M,Os,CPs) :-
	map_runsolve_cp(FNs,M,Os,CPs).
	
map_runsolve_cp([],_,_,[]).
map_runsolve_cp([F-N|FNs],M,Os,[CP|CPs]) :-
	runsolve_cp(F,N,M,Os,CP),
	map_runsolve_cp(FNs,M,Os,CPs).


runsolve_cp(F,N,_M,Os,scp(H1,B,[],
        [comment('%% Solve by runtime supplied clauses.')])) :-
	n_var_list1(N,Xs,I),
	constr_i(Os,I, [runsolve(Runsolve)]),
	H1 =.. [F|Xs],	
	%
	% H must be renamed below like the heads of the other clauses will be
	%
	( cm_option(r6,Os) ->
	  lit_add_prefix(H1, alem_, H)
        ; H = H1
        ),
	B = [copy_term(Runsolve, Runsolve1), 
             run_oc_member(clause(H, Body), Runsolve1), 
             call(Body)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% FREEZESOLVE
%%%% 
%%%% Possibility to solve goals by clauses specified at runtime
%%%% (e.g. negated query)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freezesolve_cps(FNs,M,Os,CPs) :-
	map_freezesolve_cp(FNs,M,Os,CPs).
	
map_freezesolve_cp([],_,_,[]).
map_freezesolve_cp([F-N|FNs],M,Os,[CP|CPs]) :-
	freezesolve_cp(F,N,M,Os,CP),
	map_freezesolve_cp(FNs,M,Os,CPs).


freezesolve_cp(F,N,_M,Os,scp(H1,B,[],
              [comment('%% Solve by freezeed runtime supplied clauses.')])) :-
	n_var_list1(N,Xs,I),
	constr_i(Os,I,[]),
	H1 =.. [F|Xs],	
	%
	% H must be renamed below like the heads of the other clauses will be
	%
	( cm_option(r6,Os) ->
	  lit_add_prefix(H1, alem_, H)
        ; H = H1
        ),
	B = [freezesolve(H)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ALEM
%%%%
%%%% Antilemmas (experimental)
%%%%
%%%% NOTE Mon Apr 11 19:14:20 2016
%%%%
%%%% Antilemmas (option r6) have not been ported to SWI Prolog.  See older
%%%% versions of cm.pl for code that once worked in other Prologs.
%%%% 
%%%% It seems that the new (2016) option r8 provides a similar functionality.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% This section was implemented for eclipse.
%% Later implementation using a global array - see limits/2 below.
%%
%% could also check and cut for most general solution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% ALEM RUNTIME SUPPORT
%%%%

limits(max_alem, 10000).

:- limits(max_alem, MaxAlem),
   def_array(alem_array, MaxAlem).

:- def_flag(alem).

init_alem :-
	setval(alem, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% ALEM COMPILE TIME
%%%%

alem_cps([], _Os, []).
alem_cps([FN|FNs], Os, [CP|CPs]) :-
	alem_cp(FN, Os, CP),
	alem_cps(FNs, Os, CPs).

% +++***


%% *** cutcall stuff might be wrong - prevents antilemma cleanup

alem_cp(F-N, Os, scp(Call1,B,[],[comment('%% Antilemma Wrapper [r6].')])) :-

	n_var_list1(N, XIs, I),
	Call1 =.. [F|XIs],
	butlast(XIs, Xs),

	% *** incs has nothing to do with alem, just easy to implement here
	%
	( cm_option(incs, Os) ->
	  n_var_list1(N, XIsNew, INew),
	  Call2 =.. [F|XIsNew],
	  butlast(XIsNew, Xs),
	  constr_depth_i(Os, I, SrcDepth, INew, CallDepth),
	  IncsDepthCall = ( incs(0, SrcDepth, CallDepth) )

        ; Call2 = Call1,
	  IncsDepthCall = true
        ),


	lit_add_prefix(Call2, alem_, Call),


	( cm_option(mrn, Os) ->
	  Vterm =.. [v|Xs],
	  MkVterm = true,
	  CutCall = true
	  % CutCall = ( Vterm = v -> ! ; true )

        ; constr_i(Os, I, [a(Ancestors)]),
	  append(Xs, [Ancestors], Vargs),
	  Vterm1 =.. [v|Vargs],

	  %% version 1:
	  MkVterm = run_std_term_variables(Vterm1, Vterm),

          %% version 2:
	  % MkVterm = true,
	  % Vterm = Vterm1,

	  CutCall = true
	  % CutCall = ( Vterm = [] -> ! ; true )

	  %% version 3:
	  % MkVterm = ( run_std_term_variables(Vterm1, Vterm),
	  %             copy_term(Vterm, Varlist) 
          %          ),
          % CutCall = ( run_variant_chk(Varlist, Vterm) -> ! ; true )

        ),

	% probably needs to consider the returned depth if in 'hs' mode
	limits(max_alem, MaxAlem),
	
	B = [ 
              MkVterm,

	      % write('*'),
              flag_inc(alem, AlemPos),

	      % writeln(ap(AlemPos)),

	      ( AlemPos >= MaxAlem ->
	        info(4, 'Warning: alem limit reached'),
	        % ... might allocate a larger array here
	        Call
              ; run_setval(alem_array, AlemPos, []),
	        ( 
		  IncsDepthCall,  

		  Call,
                  % write('-') ,

		  CutCall,
		  run_getval(alem_array, AlemPos, Vterms),

		  % (Vterm \= [] -> nl, writeln(Vterm-Vterms) ; true),

 	          run_alem_entry(Vterm, Vterms),
 	          run_setval(alem_array, AlemPos, [Vterm | Vterms])

                  %  , write('+')
                ; run_setval(alem_array, AlemPos, []),
		  flag(alem, _, AlemPos),
		  fail
                )
              )
	    ].

butlast([X,Y|Xs], [X|Xs1]) :-
	!,
	butlast([Y|Xs], Xs1).
butlast([_],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% INCSDEPTH for now implemented parasiticly in the antilemma wrappers
%%%% 

constr_depth_i(Os, ISource, DSource, I, D) :-
	constr_i(Os,ISource,
	         [d(DSource-_),a(AB),t(T1-T2),p(P),runsolve(Runsolve)]),
	constr_i(Os,I,
	         [d(D-_),a(AB),t(T1-T2),p(P),runsolve(Runsolve)]).

%% RUNTIME

incs(I, _, I).
incs(I, N, J) :-
        I < N,
	I1 is I + 1,
	incs(I1, N, J).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% MATRIX_TO_CPS1
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% cps are assumed to share variables with the original matrix !!!
%%% 
%%% Es are the taut/subs bindings, Vs is the corresponding list of vars 
%%% used for the r4,r5 options
%%% 
matrix_to_cps1([],_T,[],[],_,_,_,_,_).
matrix_to_cps1([C|Cs],T,[Bindings|Bindingss],CPs,Runsolve,M,FNs,ClauseIndex,Os) :-
	info_progress(20, 10),
	abstrterm(T,C,T1),
	( bagof(X,(contrapos(C,Bindings,Os,T1,Runsolve,M,FNs,ClauseIndex,X)),CPs1) ->
	  true
	; CPs1 = []
	),
	ClauseIndex1 is 1 + ClauseIndex,
	append(CPs1,CPs2,CPs),
	matrix_to_cps1(Cs,T,Bindingss,CPs2,Runsolve,M,FNs,ClauseIndex1,Os).


% matrix_to_cps1(_-[],[],[],_,_,_,_,_).
% matrix_to_cps1(T-[C|Cs],[Bindings|Bindingss],CPs,Runsolve,M,FNs,ClauseIndex,Os) :-
% 	info_progress(20, 10),
% 	abstrterm(T,C,T1),
% 	( bagof(X,contrapos(C,Bindings,Os,T1,Runsolve,M,FNs,ClauseIndex,X),CPs1) ->
% 	  true
% 	; CPs1 = []
% 	),
% 	ClauseIndex1 is 1 + ClauseIndex,
% 	append(CPs1,CPs2,CPs),
% 	matrix_to_cps1(T-Cs,Bindingss,CPs2,Runsolve,M,FNs,ClauseIndex1,Os).



contrapos(C0,Bindings,Os,T,Runsolve,_M,FNs,ClauseIndex,
          scp(H,B1,B2,
	        [
                 d1(DB), dn(DN), % depth in, (and out for hs)
		 t1(TB), tn(TN), % abstraction term in, out
		 ps(Ps),     % proof (tableaux) term
		 a(AB),      % ancestor table
		 l(Lemmas),  % lemma table
		 rec(Rec),   % term expressing recognized contrapositives
		 inf_1(InfB), inf_n(InfN), % inferences in and out
		                           % used eg. for hw
	         hc_1(HcB), hc_n(HcN),     % hconstraints (hc_list mode)
		                           % in and out
		 nfree_add(NFree-NAdd),    % used for hw
		 clause_options(ClauseOptions), % supplied clause options
		 cm_options(Os),                % cm options
		 body_length(N), % used for heuristics
		 cp_loop_value(LoopValue), % used for heuristics
		 is_theorem(IsTheorem), % used for heuristics
	         orig_head(H1), % orig_head is used eg. in heuristic 
		                % evaluation,
		                % must share vars with the body,
		                % perhaps could contain the repeated vars of
		                % original goal and an empty last arg
                 comment(Comment), % atom used as comment for the contrapos
                 fns(FNs), % list of all predicates as functor-arity pairs
                 binding_list(Bindings), % used by r4, r5
                 runsolve(Runsolve),     % obsolete feature ?
		 use_bound_table(UseBoundTable), % to simulate the
		                                 % "global logical variables"
		                                 % for use_bound
	         clause_number(ClauseIndex), % clause number, used for 
	                                     %   indexing an array (use_bound)
	         hconstraints(HConstraints)  % heuristic constraints, list
	                                     % of '$hconstrain'/3 literals
                ])) :-

	select_hconstraints(C0, HConstraints, C00),

	( select(~'$options'(ClauseOptions), C00, C) -> true 
        ; C = C00, ClauseOptions = []
        ),

	( recognize(C, Rec) -> 
	  term_atom(Rec, Comment0),
	  concat_atom([' Contrapositive of clause ', ClauseIndex, 
                      ' recognized as: ', Comment0], Comment1)
        ; Rec = no,
	  concat_atom([' Contrapositive of clause ', ClauseIndex], Comment1)
        ),
           
	( clause_options_is_theorem(ClauseOptions) -> 
	  IsTheorem = yes
	; memberchk(~'$query', C) ->
	  IsTheorem = yes
        ; IsTheorem = no
        ),

  	length(C,N1), N is N1 - 1,
	
 % 	( cm_option(spec(NormalFactor,SpecialFactor), Os) ->
 % 	  ( special_clause(C) ->
 % 	    ClauseFactor = SpecialFactor
 % 	  ; ClauseFactor = NormalFactor 
 %           )
 %         ; ClauseFactor = 1
 %         ),
	   
	( member(cost(ClauseFactor), ClauseOptions) ->
	  true
        ; cm_option(cost(ClauseFactor), Os) ->
	  true
        ; cm_option(hform(CLength,Formula,CResult), Os) ->
	  copy_term(CLength-Formula-CResult, N1-Formula1-ClauseFactor),
	  call(Formula1)
	; ClauseFactor = 1
        ),

  	select(L,C,C1),
	
	( member(no_cp(NoCp), ClauseOptions), L == NoCp ->
	  % writeln(no_cp(NoCp)),
	  fail
        ; true
        ),


	%% HDEF ... (experimental)
	( cm_option(heq(_,_,_,_,_), Os) ->
	  ( eqclause(L, [rec(Rec)], C1) ->
	    nl, pp(eqclause(L-C1)), 
	    % IsSubsClause = head
	    IsSubsClause = eq
          ; IsSubsClause = no
          )
        ; cm_option(hdef(_,_,_,_), Os) ->
	  ( defclause(L, [rec(Rec)], C1) ->
	    nl, pp(defclause(L-C1)), 
	    IsSubsClause = head
          ; IsSubsClause = no
          )
        ; cm_option(hsub(_,_,_,_), Os) ->
	  %% the subsumption depth handling doesn't (yet?) work for units
	  ( C = [_,_|_] -> 
	    IsSubsClause = head
          ; IsSubsClause = no
          )
        ; IsSubsClause = no
        ),

	cm_add_abstrterm(Os,T,TN,T2),

	( cm_option(oen,Os) -> 
          OCEs = [], L1 = L
        ; oc_equations(L,L1,OCEs)
        ),

	cm_head_literal(L1,IH,Os,H1),

	( IsSubsClause = no ->
	  H = H1
        ; subs_make_var_head(H1, H),
	  subs_subsumes_chk_call(IsSubsClause, L, H, SubsCall),
	  subs_unify_call(H1, H, UnifyCall)
        ),  

	% no contrapositives with built ins as head:
	built_in_fns(BUILT_INS),
	functor(H1,Functor,Arity),
	\+ memberchk(Functor/Arity, BUILT_INS),

        proof_rep(H1,HP),

	constr_i(Os,IH,
	         [d(D1-DN),a(AH), l(Lemmas), fns(FNs), t(TB-T2),p(e(HP,Ps)),
		  inf(Inf1-InfN), hc(Hc1-HcN),
		  use_bound_table(UseBoundTable),
                  runsolve(Runsolve)]),
	cm_body_literals(C1,C2),


	Subs = subs(IsSubsClause, SubsCall, UnifyCall),

	( N = 0, IsSubsClause \= no ->
	  NPre = -1
        ; NPre = N
        ),

	cm_pre_literals(Os,NPre,
	                d(D1,DB),
                        a(L1,AH,AB), %% ? or L here
			inf(Inf1,InfB,NFree,NAdd),
			hc(Hc1, HcB),
	                FNs,
	                OCEs,
			ClauseFactor,
	                Subs,
			UseBoundTable,
			ClauseIndex,
			C1, %% SourceBody
                        PREs),

	% cm_post_literals(Os,T,L1,C,TN,T2,POSs),
			
        ( cm_option(hc_list, Os),
	  HConstraints \= [] ->
	  POSs1 = [check_hconstraints_post(_I)]
        ; POSs1 = []
        ),
	
	( cm_option(acn, Os) ->
	  POSs = POSs1
	; ( atom(L) ; L = ~TMPA, atom(TMPA)) ->
	  POSs = ['c_$prolog_post'(!,_,_)|POSs1]
	; POSs = POSs1
	),
	
	%% POSs = [],

	append(C2,POSs,B2), 

	B1 = PREs,

	cp_loop_value(H1, B2, LoopValue),

	( ( cm_option(no_fan, Os) ; memberchk(no_fan, ClauseOptions)) ->
          !,                       %% just the first if no_fan
          atom_concat(Comment1, ' (no_fan)', Comment2)
        ; 
          Comment2 = Comment1
        ),

	concat_atom(['%%', Comment2, '.'], Comment).



select_hconstraints([~'$hconstrain'(A,B,C)|Ls], 
	            ['$hconstrain'(A,B,C)|HCs], Ls1) :-
	!,
	select_hconstraints(Ls, HCs, Ls1).
select_hconstraints([L|Ls], HCs, [L|Ls1]) :-
	select_hconstraints(Ls, HCs, Ls1).
select_hconstraints([], [], []).

%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SUBS (hdef, hsub) STUFF
%%%% 

subs_make_var_head(FullHead, VarHead) :-
	functor(FullHead, F, N),
	functor(VarHead, F, N),
	arg(N, FullHead, IStruct),
	arg(N, VarHead, IStruct).

subs_subsumes_chk_call(Mode, SrcHead, VarHead, Call) :-
	%% we use source head here since it still contains
	%% multiply occuring vars.
	( SrcHead = ~SrcHA -> true ; SrcHA = SrcHead ),
	copy_term(SrcHA, SrcHead1),
	SrcHead1 =.. [_|SrcArgs1],
	rem_argument(VarHead, VarHead1),
	VarHead1 =.. [_|VarArgs1],
	( Mode = head ->
	  ssc_call_1(SrcArgs1, VarArgs1, Call)
        ; Mode = eq ->
	  SrcArgs1 = [SArg1,SArg2],
	  VarArgs1 = [VArg1,VArg2],
	  ssc_call_1([SArg1], [VArg1], Call1),
	  ssc_call_1([SArg2], [VArg2], Call2),
	  Call = ( once((Call1 ; Call2)) )
        ).

ssc_call_1(SrcArgs1, VarArgs1, Call) :-
	optim_ssc_args(SrcArgs1, [], VarArgs1, SrcArgs2, VarArgs2),
	( SrcArgs2 = [] ->
	  Call = true
        ; SrcArgs2 = [TLeft] ->
	  VarArgs2 = [TRight],
	  subsumes_chk_pe_call(TLeft, TRight, Call)
	; TLeft =.. [t|SrcArgs2],    
	  TRight =.. [t|VarArgs2],
	  %% or a sequence of subsumes_chk calls
	  subsumes_chk_pe_call(TLeft, TRight, Call)
        ).


optim_ssc_args([], _, [], [], []).
optim_ssc_args([T1|T1s], T0, [_|T2s], T1s1, T2s2) :-
	var(T1),
	\+ contains_var(T1, T1s),
	\+ contains_var(T1, T0),
	!,
	optim_ssc_args(T1s, T0, T2s, T1s1, T2s2).
optim_ssc_args([T1|T1s], T0, [T2|T2s], [T1|T1s1], [T2|T2s2]) :-
	optim_ssc_args(T1s, [T1|T0], T2s, T1s1, T2s2).


subs_unify_call(FullHead, VarHead, Call) :-
	%%
	%% '=' is sufficient for unifiction in Call.
	%%
	FullHead =.. [_|FArgs],
	VarHead =.. [_|VArgs],
	optim_suc(FArgs, [], VArgs, FArgs1, VArgs1),
	( FArgs1 = [] ->
	  Call = true
        ; FArgs1 = [T1] ->
	  VArgs1 = [T2],
	  Call = (T2 = T1)
        ; T1 =.. [t|FArgs1],
          T2 =.. [t|VArgs1],
	  Call = (T2 = T1)
        ).

optim_suc([_], _, [_], [], []) :- !.
optim_suc([F|Fs], F0, [V|Vs], Fs1, Vs1) :-
	var(F),
	%% In case of 'oen' with an occur checking prolog the FullArg
	%% can contain repeated variables, so we check this here:
	\+ contains_var(F, Fs),
	\+ contains_var(F, F0),
	!,
	F = V,
	optim_suc(Fs, F0, Vs, Fs1, Vs1).
optim_suc([F|Fs], F0, [V|Vs], [F|Fs1], [V|Vs1]) :-	
	optim_suc(Fs, [F|F0], Vs, Fs1, Vs1).


%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% cp_loop_value - This is a heuristic value used to sort the
%%%% contrapositives. (experimental).
%%%% 

cp_loop_value(H,B,N) :-
	copy_term(H,H1), %% to get weak unification 
	cplv(H1,B,0,N).

cplv(_, [], N, N).
cplv(H, [L|Ls], N, N1) :-
	\+ \+ unify_butlast_arg(H,L),
	!,
	N2 is N + 1,
	cplv(H, Ls, N2, N1).
cplv(H, [_|Ls], N, N1) :-
	cplv(H, Ls, N, N1).
unify_butlast_arg(X1,X2) :-
	functor(X1,F,N),
	functor(X2,F,N),
	N1 is N - 1,
	map_unify(X1,X2,N1).

map_unify(_,_,0) :- !.
map_unify(X1,X2,N) :-
	arg(N,X1,A1),
	arg(N,X2,A2),
	unify_with_occurs_check(A1,A2),
	N1 is N - 1,
	map_unify(X1,X2,N1).

%%%%%%%%%%%%%%%%%%%%

cm_pre_literals(_Os,0,d(D1,D1),_A,
	        inf(Inf,Inf,_NFree,_NAdd),
		hc(Hc1, Hc1),
                _FNs,OCEs,_ClauseFactor,_Subs,
		_UseBoundTable,_ClauseIndex,_SourceBody,
		OCEs) :-
	!.
cm_pre_literals(Os,Nin,d(D1,DB),a(H,AH,AB),
	        inf(Inf1,InfB,NFree,NAdd),
		hc(Hc1, HcB),
                FNs,OCEs,ClauseFactor,Subs,
		UseBoundTable,ClauseIndex,SourceBody,
		Literals) :-

	%% Fri Sep 19 22:20:21 2014: N seems not unsed
	%% ( Nin >= 0 -> N = Nin ; N is Nin * -1 ),

	%% negative Nin for unit clauses that should just get
	%% some PRE stuff
	( cm_option(mrn,Os), \+cm_option(r1,Os), \+cm_option(r2,Os) -> 
 	  L1 = [] 
        ; Nin < 0 ->
	  L1 = []
        ; cm_ancestor_literal(H,Os,H1),
	  ct_make_ancestor_table(FNs,_Os,AH),
	  ct_add_ancestor(H1,AH,AB,FNs,Os,ADD_ANCESTOR),
          L1 = ADD_ANCESTOR
        ),

	L2 = L1,

        ( cm_option(hs, Os) ->
	  cm_pre_body_length(SourceBody, Os, N1),   
	  PRE = OCEs,
	  TEST = (D1 >= N1),
	  REQ = N1,
	  DEC = (DB is D1 - N1)
        ; ( cm_option(hd1, Os) ; cm_option(hb1, Os) ) ->
	  cm_pre_body_length(SourceBody, Os, N1), 
          NF is N1 * ClauseFactor,
	  PRE = OCEs,
	  TEST = (D1 >= NF),
	  REQ = NF,
	  DEC = (DB is D1 - NF)
	%%%%
	%%%% Experimental:
	%%%%
        ; cm_option(hd1x(HXMIN), Os) ->
	  cm_pre_body_length(SourceBody, Os, N1), 
          NF is N1 * ClauseFactor,
	  PRE = OCEs,
	  TEST = ( D1 >= NF -> DB is D1 - NF
		 ; D1 >= HXMIN -> DB is min(0, D1 - 1)
		 ),
	  DEC = true,
	  REQ = NF
        ; cm_option(hsx(HXMIN), Os) ->
	  cm_pre_body_length(SourceBody, Os, N1),   
	  PRE = OCEs,
	  TEST = ( D1 >= N1 -> DB is D1 - N1
		 ; D1 >= HXMIN -> DB is min(0, D1 - 1)
		 ),
	  DEC = true,
	  REQ = N1
        ; cm_option(hdx(HXMIN), Os) ->
	  PRE = OCEs,
	  TEST = ( D1 >= ClauseFactor -> DB is D1 - ClauseFactor
		 ; D1 >= HXMIN -> DB is min(0, D1 - 1)
		 ),
	  DEC = true,
	  REQ = ClauseFactor
	%%%%
	%%%%
        ; cm_option(hw, Os) ->
	  cm_pre_body_length(SourceBody, Os, N1), 
          NF is N1 * ClauseFactor,
	  Infs = N1, 
	  TEST =  ( hw_wdb_w1(D1, NF, NW1),    %% eg.  NW1 is D1 - 1.
	            NW1 > 0,
		    hw_wdb_w2(NW1, NF, NFree), %% eg.  NFree is NW1 - NF.
		    NFree > 0
		  ),
	  DEC =   ( InfB is Inf1 + Infs,
	            NAdd is NW1 - NFree     %% eg.  NF (ie. L).
		    %% not calling hw_wdb_w3 here, assuming hw_wdb3(X,0)=X
		    %% so the 1st literal is called with NW1
		  ),
	  DB = NW1,
	  PRE = OCEs,
	  REQ = inc_is_not_implemented_for_hw
        ; cm_option(hd2, Os) ->
	  cm_pre_body_length(SourceBody, Os, N1),
	  PRE = OCEs,
	  N2 is N1 + 1,
	  TEST = (D1 >= N2),      
	  REQ = N2,
	  DEC = (DB is (D1 - 1) // N1)
        ; ( cm_option(hdef(Mode, NormalCost1, SubsCost1, SubsThreshold1), Os),
	    NonSubsCost1 = NormalCost1
          ; cm_option(hsub(Mode, NormalCost1, SubsCost1, SubsThreshold1), Os),
	    NonSubsCost1 = NormalCost1
          ; cm_option(heq(Mode, NormalCost1, SubsCost1, NonSubsCost1, SubsThreshold1),
	              Os)
          ) ->

	  ( Mode = d ->
	    Factor = 1 
          ; cm_pre_body_length(SourceBody, Os, Factor)
          ),

	  NormalCost is Factor * NormalCost1,
	  NonSubsCost is Factor * NonSubsCost1,
	  SubsCost is Factor * SubsCost1,
	  SubsThreshold is Factor * SubsThreshold1,


	  ( Subs = subs(no, _, _) ->
	    
	    PRE = OCEs,
	    TEST = (D1 >= NormalCost),
	    REQ = NormalCost,
	    DEC = (DB is D1 - NormalCost)

          ; Subs = subs(_, SubsChkCall, UnifyCall) ->

	    %% ? REQ is min(NormalCost, SubsThreshold),
	    %% REQ is only used with the inc option, todo: check
	    %% possible uses of inc with hdef

	    cm_list_to_andseq([UnifyCall | OCEs], Unify),

	    LsTotal = [(( D1 >= SubsThreshold, SubsChkCall ->
	                  %% D1 >= SubsCost
			  %% SubsThreshold is usually >= SubsCost
			  DB is D1 - SubsCost
		        ; D1 >= NonSubsCost ->
		          DB is D1 - NonSubsCost  
                        ; %% set bound inc is set here even if they 
			  %% don't unify ***
			  %% (seems a bit faster than unifying
			  %% before depth check here)
                          RUN_SET_BOUND_INC,
		          fail
                        ),
			Unify
                       )
		      ]
          ) % end hdef, hsubs 		     
	; %% ( cm_option(hd, Os) ; cm_option(hb, Os) ) ->
	  %%
          %% hd is the default
	  %%
	  PRE = OCEs,
	  TEST = (D1 >= ClauseFactor),
	  REQ = ClauseFactor,
	  DEC = (DB is D1 - ClauseFactor)
        ),


	( cm_option(inc, Os) ->
	  RUN_SET_BOUND_INC = run_set_bound_increment(D1, REQ)
        ; cm_option(inc1, Os) ->
	  RUN_SET_BOUND_INC = run_set_bound_increment
	; RUN_SET_BOUND_INC = true
        ),

	( var(LsTotal) ->
	  ( RUN_SET_BOUND_INC = true ->
	    Ls1 = [TEST, DEC | L2]
          ; Ls1 = [((TEST -> true ; RUN_SET_BOUND_INC, fail)), DEC |L2]
          ),
          append(PRE, Ls1, Ls)
        ; append(LsTotal, L2, Ls)
        ),

	( cm_option(ub, Os) ->
	  CHECK_USE_BOUND = 
                  [ arg(ClauseIndex, UseBoundTable, FreeUses),
         	    ( FreeUses > 0 ->
		      true
		    ; % write('x'), flush_output(output),
		      setval(use_bound_reached, true),
		      fail
		    ),
		    FreeUses1 is FreeUses - 1,
		    setarg(ClauseIndex, UseBoundTable, FreeUses1)
		  ],
          append(Ls, CHECK_USE_BOUND, Ls101)
	; Ls101 = Ls
        ),

	( cm_option(hc_list, Os) ->
	  Ls102 = Ls101, Hc1 = HcB
	  %% append(Ls101, [check_hconstraints_pre(Hc1, HcB)], Ls102)
        ; Ls102 = Ls101
        ),
      
	Literals = Ls102.

cm_pre_body_length(SourceBody, Os, N) :-
	cm_pbl(SourceBody, Os, N1),
	( N1 = 0, SourceBody \= [] ->  %% *** perhaps treat in a cleaner way
	  N = 1
        ; N = N1
        ).

cm_pbl([L|Ls], Os, N) :-
	cm_pre_body_length_ignore_literal(L, Os),
	!,
	cm_pbl(Ls, Os, N).
cm_pbl([_|Ls], Os, N) :-
	cm_pbl(Ls, Os, N1),
	N is N1 + 1.
cm_pbl([], _, 0).

cm_pre_body_length_ignore_literal(L, _) :-
	( L = ~A -> true ; A = L ),
	functor(A, F, _),
	name(F, [0'$|_]), %'
	!.
cm_pre_body_length_ignore_literal(~(_=X), Os) :-
	var(X),
	cm_option(steq, Os),
	\+ cm_option(steq_d_off, Os).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% defclause(SrcHead, _, SrcBody)
%%%% 
%%%% Succeeds if the contrapositive SrcHead, SrcBody might be part of a
%%%% "definition" of a symbol in SrcHead.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% *** CONSTRUCTION SITE
% 

eqclause(SrcHead, _Args, _SrcBody) :-
	%% SrcHead = (X = Y),
	once(( SrcHead = (X = Y) ; SrcHead = ~(X = Y))),
	once(( nonvar(X) ; nonvar(Y) )).
	% \+ memberchk(rec(eq(_, subst(_))), Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defclause(SrcHead, Options, SrcBody) :-
	\+ defclause_fail(SrcHead, Options, SrcBody),
	( SrcHead = ~SrcAH -> true ; SrcAH = SrcHead ),
	term_functors(SrcAH, HeadConcepts),
	term_functors(SrcBody, BodyConcepts),
	member(HConc, HeadConcepts),
	\+ memberchk(HConc, BodyConcepts),
	!.

% 
% special_clause(C) :-
% 	select(L, C, C1),
% 	defclause(L, C1),
% 	!,nl,
% 	pp(special(L,C1)).
% 
% 
% 
% defclause(SrcHead, SrcBody) :-
% 	\+ defclause_fail(SrcHead, SrcBody),
% 	ord_greater(SrcHead, SrcBody).
% 

%%%%%%%%%%%%%%%%%%%%
%%% crude, just to test, for now:

ord_greater(SrcHead, SrcBody) :-    
	( SrcHead = ~SrcAH -> true ; SrcAH = SrcHead ),
	term_functors(SrcAH, HeadConcepts),
	term_functors(SrcBody, BodyConcepts),
	maplist(ord_concept_size, HeadConcepts, HSizes),
	maplist(ord_concept_size, BodyConcepts, BSizes),
	max_num(HSizes, HSize),
	max_num(BSizes, BSize),
	HSize > BSize,
	!. % blue

max_num([N|Ns], Max) :-
	max_num(Ns, N, Max).
max_num([N|Ns], SoFar, Max) :-
	N1 is max(N, SoFar),
	max_num(Ns, N1, Max).
max_num([], SoFar, SoFar).
	
ord_ering([in/2, subseteq/2,
          (=)/2, equal/2, 
          cap/2, cup/2,
          setminus/2, big_delta/2, meets/2]).

ord_concept_size(Con,Size) :-
	ord_ering(Cons),
	ord_cs(Con, Cons, 1, Size).

ord_cs(Con, [Con|_], N, N) :- !.
ord_cs(Con, [_|Cons], N, N1) :-
	N2 is N + 1,
	ord_cs(Con, Cons, N2, N1).
ord_cs(_, [], _, 0).


% % defclause_fail(s(_, _), [~ (_ = _)]) :- !. % *** 
% 
% defclause_fail(L, _) :-
% 	functor(L,F,_),
% 	name(F,[100|_]),
% 	!,
% 	fail.
% defclause_fail(_, _) :- !.
% 

defclause_fail(_, _, []) :-
        %% the subsumption depth handling doesn't (yet?) work for units
	%% *** NOW FIXED
	!.
defclause_fail(H, _, _) :-
	( H = ~A -> true ; A = H),
	term_size_1(A, Size),
	Size > 4, % ***
	!.
defclause_fail(~Q, _, _) :-
	functor(Q, '$query', _),
	!.
defclause_fail(Q, _, _) :-
	functor(Q, '$query', _),
	!.
defclause_fail(_, _, L) :-
	member(Q, L),
	functor(Q, '$query', _),
	!.
defclause_fail(_, Options, _) :-
	getarg_chk( rec(eq(_, _)), Options),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% RECOGNIZE
%%%% 
%%%% recognize certain [kinds of] clauses
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recognize(Clause, Name) :-
	rec(Clause, Name),
	!.

%%%%
%%%% rec_eq_atom/4 identifies and destructures equality atoms
%%%%
rec_eq_atom(A, F/2, X, Y) :-
	A =.. [F, X, Y],
	rec_eq_functor(F).

rec_eq_functor(F) :-
	( (F = '=') -> true 
        ; name(F, [61 | _]) -> true
          %% i.e. functor name starts with '='
	%%
	%% ; name(F, [101, 113 | _]) -> true
        %%  %% i.e. functor name starts with 'eq'
	%% perhaps collides with some tptp stuff ?
	%%
        ; fail
        ).


rec([L], eq(E, ax(reflexivity))) :-
	rec_eq_atom(L, E, X, Y),
	var(X),
	X == Y.
rec(C, eq(E, ax(symmetry))) :-
	length(C, 2),
	once( (select(L, C, C1), rec_eq_atom(L, E, X, Y)) ),
	var(X), var(Y),
	C1 = [~L1],
	rec_eq_atom(L1, E, Y1, X1),
	X == X1, Y == Y1.
rec(C, eq(E, ax(transitivity))) :-
	length(C, 3),
	once(( select(L, C, C1), rec_eq_atom(L, E, X, Z) )),
	var(X), var(Z),
	once(( select( ~L1, C1, C2),
               rec_eq_atom(L1, E, X1, Y1),
	       X1 == X,
	       var(Y1)
             )),
	C2 = [~L2],
	rec_eq_atom(L2, E, Y2, Z2),
	Y2 == Y1, Z2 == Z.
rec(C, eq(E, ax(symtrans_v1))) :-           	      %% x = y, x = z -> y = z
	length(C, 3),
	once(( select(L, C, C1), rec_eq_atom(L, E, Y, Z) )),
	var(Y), var(Z),
	once(( select( ~L1, C1, C2 ),
	       rec_eq_atom(L1, E, X1, Y1),
	       Y1 == Y,
	       var(X1)
             )),
	C2 = [~L2],
	rec_eq_atom(L2, E, X2, Z2),
	X2 == X1, Z2 == Z.
rec(C, eq(E, ax(symtrans_v2))) :-           	      %% y = x, z = x -> y = z
	length(C, 3),
	once(( select(L, C, C1), rec_eq_atom(L, E, Y, Z) )),
	var(Y), var(Z),
	once(( select( ~L1, C1, C2 ),
	       rec_eq_atom(L1, E, Y1, X1),
	       Y1 == Y,
	       var(X1)
             )),
	C2 = [~L2],
	rec_eq_atom(L2, E, Z2, X2),
	X2 == X1, Z2 == Z.
rec(C, eq(E, ax(symtrans_v3))) :-           	      %% x = y, y = z -> z = x
	length(C, 3),
	once(( select(L, C, C1), rec_eq_atom(L, E, Z, X) )),
    	var(Z), var(X),
	once(( select( ~L1, C1, C2 ),
	       rec_eq_atom(L1, E, X1, Y1),
	       X1 == X,
	       var(Y1)
             )),
	C2 = [~L2],
	rec_eq_atom(L2, E, Y2, Z2),
	Y2 == Y1, Z2 == Z.
rec(C, eq(E, subst(fun(F/A-N)))) :-
	length(C, 2),
	once(( select(~L, C, C1) , rec_eq_atom(L, E, X, Y))),
	var(X), var(Y),
	C1 = [L1],
	rec_eq_atom(L1, E, T1, T2),
	nonvar(T1),
	nonvar(T2),
	functor(T1, F, A),
	functor(T2, F, A),
	A > 0,
	contains_n_vars(A, T1), % ie. T1 contains at least A different vars
	contains_n_vars(A, T2),
	rec_compare_subst(A, T1, T2, X, Y, N).
rec(C, eq(E, subst(pred(F/A-N)))) :-
	length(C, 3),
	once(( select(~L, C, C1), rec_eq_atom(L, E, X, Y) )),
	var(X), var(Y),
	once( select( ~T1, C1, [T2]) ),
	functor(T1, F, A),
	functor(T2, F, A),
	\+ rec_eq_atom(T1, E, _, _),
	A > 0,
	contains_n_vars(A, T1), % ie. T1 contains at least A different vars
	contains_n_vars(A, T2),
	rec_compare_subst(A, T1, T2, X, Y, N).

rec_compare_subst(0, _, _, _, _, N) :-
	!,
	nonvar(N).
rec_compare_subst(A, T1, T2, X, Y, N) :-
	arg(A, T1, S1),
	var(S1),
	arg(A, T2, S2),
	var(S2),
	( S1 == S2 ->
	  A1 is A - 1, 
	  rec_compare_subst(A1, T1, T2, X, Y, N)
        ; N = A,             %% fails if N is already bound
          once(( S1 == X, S2 == Y
               ; S1 == Y, S2 == X
               )),
	  A1 is A - 1, 
	  rec_compare_subst(A1, T1, T2, _, _, N)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
% cm_post_literals(Os,T,H,C,TN,T2,Ls) :-
% 
% 	writeln(cm_post_literals(Os,T,TN,T2)),
% 
% 	( %% building absterms in this way we can make 
% 	  %% runtime checks
%           cm_option(tunit,Os) -> 
% 		( T == k -> 
% 		    L2 = [add_ttt(T,TN,T2)]
% 		       % L2 = [],  T2 = TN 
% 	        ; L2 = [add_ttt(T,TN,T2)]
% 	        )
%         ; L2 = []
%         ),
% 	Ls = L2.
% 
% 
% add_ttt(A,B,C) :-
% 	writeln(add_ttt(A,B,C)).

cm_add_abstrterm(Os,T,TN,T2) :-
	%% fast way for building absterms
	( cm_option(tunit,Os) ->  % *** might not work
	  true % absterm compiled by cm_pos_literals
        ; cm_option(t,Os) -> 
          ( T == k -> 
	    T2 = TN 
          ; T2 = [T|TN]
          )
	; true
        ).

cm_head_literal(L,IH,_Os,L1) :-
	rename_source_lit_pos(L,L3),
	add_argument(L3,IH,L1).

cm_ancestor_literal(L,_Os,L1) :-
	% L and L1 are without the extra argument here
	rename_source_lit_pos(L,L1).

cm_body_literals([],[]).
cm_body_literals([L|Ls],[L1|Ls1]) :-
	cm_body_literal(L,L1),
	cm_body_literals(Ls,Ls1).

cm_body_literal(L,L1) :-
	rename_source_lit_neg(L,L2),
	add_argument(L2,_,L1).

%% 
%% cps_set_body_is can+must be done after sorting the subgoals
%%
%% 

cps_set_body_is([CP|CPs],Os) :-
	cp_set_body_is(CP,Os),
	cps_set_body_is(CPs,Os).
cps_set_body_is([],_).



cp_set_body_is(scp(_,_,B,Arglist),Os) :-
	( ( cm_option(hb, Os) ; cm_option(hb1, Os) ) ->
	  %% don't share the "output" resources of pre with
	  %% the resources for the 1st literal
	  cp_set_body_is_1_d1_noshare(B, Os, Arglist)
        ; cp_set_body_is_1(B,Os,Arglist)
        ).

cp_set_body_is_1([L|Ls], Os, Arglist) :-
	getarg(a(AB), Arglist),
	getarg(l(Lemmas), Arglist),
	getarg(d1(D1), Arglist),
	getarg(dn(DN), Arglist),
	getarg(t1(T1), Arglist),
	getarg(tn(TN), Arglist),
	getarg(ps(Ps0), Arglist),
	getarg(inf_1(Inf1), Arglist),
	getarg(inf_n(InfN), Arglist),
	getarg(runsolve(Runsolve), Arglist),
	getarg(use_bound_table(UseBoundTable), Arglist),
	getarg(fns(FNs), Arglist),
	functor(L,_,N),
	arg(N,L,I),
	constr_i(Os,I,[d(D1-D2),a(AB),l(Lemmas),t(T1-T2),p(P),
	               inf(Inf1-Inf2),
	               runsolve(Runsolve),
		       use_bound_table(UseBoundTable)]),
	( cm_option(hs,Os) -> true 
        ; cm_option(hw,Os) -> true 
        ; cm_option(hb,Os) -> true 
        ; cm_option(hb1,Os) -> true 
        ; D2 = D1
        ),
	( has_no_proof(L) ->
	  Ps0 = Ps
	; Ps0 = [P|Ps]
	),
	( cm_option(l, Os) ->
	  ct_add_lemma(Lemmas, L, P, Os, FNs, Lemmas1)
	; true
	),
	cp_set_body_is_1(Ls,Os, [a(AB), l(Lemmas1),
				 d1(D2), dn(DN), t1(T2), tn(TN),
                                ps(Ps),
				inf_1(Inf2), inf_n(InfN),
                                runsolve(Runsolve),
				use_bound_table(UseBoundTable)]).
cp_set_body_is_1([], _, Arglist) :-
	getarg(d1(D), Arglist),
	getarg(dn(D), Arglist),
	getarg(t1(T), Arglist),
	getarg(tn(T), Arglist),
	getarg(ps([]), Arglist),
	getarg(inf_1(Inf), Arglist),
	getarg(inf_n(Inf), Arglist).

has_no_proof('c_$prolog_post'(_,_,_)).
has_no_proof('c_$prolog'(_,_,_)).
	
cp_set_body_is_1_d1_noshare(Ls, Os, Arglist) :-
	%% all args with exception of d1 are kept 
	getarg(a(AB), Arglist),
	getarg(l(Lemmas), Arglist),
	getarg(d1(_), Arglist),
	getarg(dn(DN), Arglist),
	getarg(t1(T1), Arglist),
	getarg(tn(TN), Arglist),
	getarg(ps(PS), Arglist),
	getarg(inf_1(Inf1), Arglist),
	getarg(inf_n(InfN), Arglist),
	getarg(hc_1(Hc1), Arglist),
	getarg(hc_n(HcN), Arglist),
	getarg(runsolve(Runsolve), Arglist),
	getarg(use_bound_table(UseBoundTable), Arglist),
	Arglist1 =  [a(AB), l(Lemmas),
		     d1(_), dn(DN), t1(T1), tn(TN), ps(PS),
	             inf_1(Inf1), inf_n(InfN),
	             hc_1(Hc1), hc_n(HcN),
                     runsolve(Runsolve),
		     use_bound_table(UseBoundTable)],
        cp_set_body_is_1(Ls, Os, Arglist1).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% "IS" - I STRUCTURES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% constr_i now more flexible with "keyterm" arguments.
%% old signature: constr_i(Os,I,D1-D2,A,T1-T2,P,Runsolve,M)
%%
%% see also describe_i/0
%%

getarg(Keyterm, Arglist) :- memberchk(Keyterm, Arglist), !.
getarg(_, _).

getarg_chk(Keyterm, Arglist) :- memberchk(Keyterm, Arglist).

%%
%% getarg cannot be used to set parameters not already in the list.
%%
	
constr_i(Os, I, Arglist) :-
	getarg(d(D1-D2), Arglist),
	getarg(a(A), Arglist),
	getarg(t(T1-T2),Arglist),
	getarg(p(P), Arglist),
	getarg(inf(Inf1-Inf2), Arglist),
	getarg(hc(Hc1-Hc2), Arglist),
	getarg(runsolve(Runsolve), Arglist),
	getarg(use_bound_table(UseBoundTable), Arglist),
	getarg(l(Lemmas), Arglist),
	% getarg(fns(FNs), Arglist),
	(cm_option(p,Os) -> L1 = [P] ; L1 = []),
	(cm_option(t,Os) -> L2 = [T1,T2|L1] ; L2 = L1),
	((cm_option(mrn,Os), \+cm_option(r1,Os), \+cm_option(r2,Os))
	  -> L3 = L2 ; L3 = [A|L2]), 
	( cm_option(hs,Os) ->
	  L4 = [D1,D2|L3]
	; cm_option(hw, Os) ->
	  L4 = [D1,Inf1,Inf2|L3]
        ; L4 = [D1|L3]
        ),
	(cm_option(rus, Os) ->
	    L5 = [Runsolve|L4]
        ;
	    L5 = L4 
        ),
	(cm_option(ub, Os) ->
	    L6 = [UseBoundTable|L5]
        ;
	    L6 = L5
        ),

	(cm_option(hc_list, Os) ->
	    L7 = [Hc1,Hc2|L6]
        ;
	    L7 = L6
        ),
	(cm_option(l, Os) ->
	    append(L7, [Lemmas], L8)
	;   L8 = L7
	),
	I =.. [i|L8].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% create list of n vars, return also pointer to the last element
%%% (which is the i-structure if the list is used as argumentlist of
%%% a contrapositive literals).
%%% 
n_var_list1(1,[X],X).
n_var_list1(N,[_|X],Y) :- N > 1, N1 is N - 1, n_var_list1(N1,X,Y).


%%% 
%%% matrix_predicates always gives positive and negative for any predicate 
%%% found  - since the "pre" and "post"  parts for a predicate might be
%%% needed for solving goals given at runtime (runsolve), whereas the 
%%% contrapositives generated from the axioms might only
%%% contain the predicate for one sign.
%%% 
%%% (in the old version this was cps_fns, but:)
%%% we need FNs during generation of the contrapositves
%%% (for ancestor table):
%%% this repeats information about contrapositive generation:
%%%  - renaming
%%%  - addition of the extra arg
%%% 
matrix_predicates(Clauses,FNs) :-
	matrix_predicates(Clauses,[],FNs).

matrix_predicates([],FNs,FNs).
matrix_predicates([C|Cs],FNs,FNs1) :-
	clause_fns(C,FNs,FNs2),
	matrix_predicates(Cs,FNs2,FNs1).

clause_fns([],FNs,FNs).
clause_fns([L|Ls],FNs,FNs1) :-
	rename_source_lit_pos(L, LPOS),
	functor(LPOS, FPOS, N1),
	N is N1 + 1,
	built_in_fns(BUILT_INS),
	( memberchk(FPOS/N, BUILT_INS) ->
	  clause_fns(Ls,FNs,FNs1)
        ; memberchk(FPOS-N,FNs) ->
	  clause_fns(Ls,FNs,FNs1)
        ; rename_source_lit_neg(L, LNEG),
	  functor(LNEG, FNEG, _),
	  clause_fns(Ls, [FPOS-N,FNEG-N|FNs], FNs1)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstrterm(T,C,T) :-
	std_term_variables(T, Vs),
	member(V, Vs),
	contains_var(V, C),
	!.
abstrterm(_,_,k).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrix_functions(M, Fs) :-
	m_fs(M, [], F1s),
	sort(F1s, Fs).

m_fs([C|Cs], F, F1) :- c_fs(C, F, F2), m_fs(Cs, F2, F1).
m_fs([], F, F).

c_fs([L|Ls], F, F1) :- l_fs(L, F, F2), c_fs(Ls, F2, F1).
c_fs([], F, F).

l_fs(~L, F, F1) :- !, l_fs(L, F, F1).
l_fs('$hconstrain'(_,_,T), F, F1) :- !, f(T, F, F1).
l_fs('$options'(_), F, F) :- !.
l_fs('$prolog'(_), F, F) :- !.
l_fs('$prolog'(_,_), F, F) :- !.
l_fs('$prolog_post'(_), F, F) :- !.
l_fs('$prolog_post'(_,_), F, F) :- !.
l_fs(L, F, F1) :- functor(L, _, N), map_f(N, L, F, F1).

map_f(0, _, F, F) :- !.
map_f(N, A, F, F1) :-
	arg(N, A, E), f(E, F, F2), N1 is N-1, map_f(N1, A, F2, F1).

f(E, F, F) :- var(E), !.
f(L, F, [Op/N|F1]) :- functor(L, Op, N), map_f(N,L,F,F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% OCCUR CHECK EQUATIONS
%%%% 
%%%% oc_equations to the body, renaming of multiple occuring 
%%%% vars in the head.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oc_equations(H,H1,Es) :- oce(H,[],[],H1,_,Es).

oce(X,Vs,Es,X,Vs,Es) :- atomic(X), !.
oce(X,Vs,Es,X1,Vs,[unify_with_occurs_check(X,X1)|Es]) :- var(X), absmember(X,Vs), !. 
oce(X,Vs,Es,X,[X|Vs],Es) :- var(X), !.
oce(X,Vs,Es,X1,Vs1,Es1) :- 
  X =.. [F|As],
  oces(As,Vs,Es,As1,Vs1,Es1),
  X1 =.. [F|As1].
oces([],Vs,Es,[],Vs,Es).
oces([A|As],Vs,Es,[A1|As1],Vs1,Es1) :- 
  oce(A,Vs,Es,A1,Vs2,Es2),
  oces(As,Vs2,Es2,As1,Vs1,Es1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% LITERAL RENAMING, PREFIXES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rename_source_lit_pos(~T,T1) :- !, source_lit_add_prefix('c_not_',T,T1).
rename_source_lit_pos(T,T1) :- source_lit_add_prefix('c_',T,T1).
rename_source_lit_neg(~T,T1) :- !, source_lit_add_prefix('c_',T,T1).
rename_source_lit_neg(T,T1) :- source_lit_add_prefix('c_not_',T,T1).

source_lit_add_prefix(P,T,T1) :-
	name(P,PN),
	T =.. [F1|Xs], name(F1,N1), append(PN,N1,N2), name(F2,N2),
	T1 =.. [F2|Xs].


lit_complement(L,L1) :- 
	L =.. [F|Xs], 
	name(F,N), complem_name(N,N1), name(F1,N1), 
	L1 =.. [F1|Xs].

complem_name([99,95,110,111,116,95|X],[99,95|X]) :- %% "c_not_..." to "c_..."
	!.
complem_name([99,95|X],[99,95,110,111,116,95|X]).   %% "c_..." to "c_not_..."

rem_cp_sign_prefix(L, L1) :-
	L =.. [F|Xs], 
	name(F,N), 
	( N = [99,95,110,111,116,95|N1] ->            %% c_not_ ...
	  name(F1,N1), L2 =.. [F1|Xs], L1 = ~L2
        ; N =[99,95|N1] ->                            %% else c_ ...
          name(F1,N1), L1 =.. [F1|Xs]
        ).

cp_literal_src(L, Sign, SrcFunctor, SrcArgs) :-
	L =.. [F|Args], 
	name(F,N), 
	( N = [99,95,110,111,116,95|N1] ->            %% c_not_ ...
	  name(SrcFunctor,N1), Sign = neg
        ; N =[99,95|N1] ->                            %% else c_ ...
          name(SrcFunctor,N1), Sign = pos
        ),
	butlast(Args, SrcArgs).



lit_add_prefix(L, Prefix, L1) :-
	%% prefix is inserted between sign and original name:
        %% eg. c_not_prefix_p(X), c_prefix_p(X)
	L =.. [F|Xs],
	name(F, N),
	name(Prefix, PN),
	( N = [99,95,110,111,116,95|X] ->             %% c_not_ ...
	  append([99,95,110,111,116,95], PN, N1),
	  append(N1, X, N2),
	  name(F2, N2)
        ; N = [99,95|X] ->                            %% c_ ... 
	  append([99,95], PN, N1),
	  append(N1, X, N2),
	  name(F2, N2)
        ; L1 = L                                      %% leave other literals
        ),
        L1 =.. [F2|Xs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rename_heads([], _Prefix, []).
rename_heads([scp(H,B,V,A)|CPs], Prefix, [scp(H1,B,V,A)|CPs1]) :-
	lit_add_prefix(H, Prefix, H1),
	rename_heads(CPs, Prefix, CPs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% add rsp. remove the last argument
%%% 

add_argument(T,A,T1) :- T =.. L, append(L,[A],L1), T1 =.. L1.
rem_argument(T,T1)   :- 
	T =.. L, reverse(L,[_|L1]), 
	reverse(L1,L2), T1 =.. L2.

proof_rep(L,L1) :-
	rem_argument(L,L2),
	rem_cp_sign_prefix(L2,L1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CONTRAPOSITIVES TO PROLOG CLAUSES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cp_to_prolog_clause(scp(H,B1,B2,_),C) :-
	append(B1,B2,B),
	cp_to_prolog_clause1(H,B,C).

cp_to_prolog_clause1(H,[],H).
cp_to_prolog_clause1(H,[L|Ls],(H :- B)) :- cm_list_to_andseq([L|Ls],B).

cm_list_to_andseq([X],X).
cm_list_to_andseq([X,Y|Z],(X,YZ1)) :- cm_list_to_andseq([Y|Z],YZ1).

write_cp_comment(scp(_,_,_,Arglist)) :-
	( nonvar(Arglist), Arglist = [_|_],
	  getarg(comment(COMMENT), Arglist),
          nonvar(COMMENT) ->
  	  write(COMMENT)
        ; true
        ).

write_cps([]).
write_cps([CP|CPs]) :-
	info_progress(20, 50),
	nl,
	write_cp_comment(CP),
	nl, 
	cp_to_prolog_clause(CP,C),
%	writeq(C),
	pp_clause(C),
%	portray_clause(C),
%	write_canonical(C),
	write_cps(CPs).

write_cps_ff([]).
write_cps_ff([CP|CPs]) :-
	info_progress(20, 100),
	cp_to_prolog_clause(CP,C),
	write_canonical(C),
	writeln('.'),
	write_cps_ff(CPs).

write_cps_fq([]).
write_cps_fq([CP|CPs]) :-
	info_progress(20, 100),
	cp_to_prolog_clause(CP,C),
	writeq(C),
	writeln('.'),
	write_cps_fq(CPs).

mk_cp_clauses([],[]).
mk_cp_clauses([CP|CPs],[C|Cs]) :-
	cp_to_prolog_clause(CP,C), 
	mk_cp_clauses(CPs,Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CONTRAPOSITIVE MANIPULATION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SORT CPS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% not all cps (eg. those PREs used for reduction operations) have a 
%%% properly initialized 4th argument, the ones which should be sorted
%%% however do.
%%% 

sort_cps(X,Y) :-
	% sort_cps_old(X,Y).
	sort_cps_new(X,Y).
% maplist(prt_cp,Y,_).

   % sort_cps_new(X,Y).
% 
% 
% prt_bf(H, F) :- 
% 	H =.. [P|L],
% 	append(RM, [_], L),
% 	F =.. [P|RM].
% 
% prt_cp(CP,_) :-
% 	CP = scp(H,_,Body,Arglist),
% 	maplist(prt_bf,Body, B1),
% 	H =.. [F|L],
% 	append(RM, [_], L),
% 	H1 =.. [F|RM],
% 	functor(H,Functor,Arity),
% 	cp_value(CP,_,Value),
% 	\+ \+ ( numbervars(H1-B1-Value,1,_),
% 	         writeln((Value-H1-B1))).
% 

is_comp_functor(F1,F2) :-
	name(F1, [_, _ | N1]),
	name(F2, [_, _ | N2]),
	( N1 = [110, 111, 116, 95|N2] -> true
        ; N2 = [110, 111, 116, 95|N1] -> true
        ).


cp_predloop_value(H,B,Val) :-
	functor(H,F,N),
	findall(1, ( member(L,B), functor(L,F,N)), PLs),
	length(PLs, Val).


sort_cps_new(CPs, CPs1) :-
	%% writeln(sort_new),
	map_cp_value(CPs, CPs, KCPs),
	keysort(KCPs, KCPs1),
	remove_keys(KCPs1, CPs2),
	%% variants - eg through a clause: [p(X,Y), p(Y,X)]
	%% perhaps they should be prevented earlier.
	rm_cp_variants(CPs2, CPs1).

rm_cp_variants([], []).
rm_cp_variants([C1,C2|Cs], Cs1) :-
	variant_chk(C1, C2),
	!,
	rm_cp_variants([C2|Cs], Cs1).
rm_cp_variants([C1|Cs], [C1|Cs1]) :-
	rm_cp_variants(Cs, Cs1).


map_cp_value([CP|CPs], AllCPs, [Val-CP|VCPs]) :-
	info_progress(20, 100),
	% cp_value(CP, AllCPs, Val),
	cp_value(CP, Val),
	map_cp_value(CPs, AllCPs, VCPs).
map_cp_value([], _, []).
	

cp_value(scp(_H,_,Body,Arglist), Val) :-
	%%
	%% This is more superstitious then experiment based.
	%% Motivation: make sorting less dependent on random 
	%% input ordering.
	%%
	getarg(orig_head(H), Arglist),
	Val =
	  v(Functor,
            Arity,
	    % ConnVal,
	    OrdpKey_00,
	    ReflexVal,
	    BLVal,
	    TermDepthChange,
	    SubstVal,
	    LoopValue,
	    ThmVal,
	    BodyLength,
	    OrdpKey_01,
	    TSize, 
	    PLV,
	    OrdpKey
	    % BSize,
	    % HSize
	    % '----',
	    % HVal,
	    % 
	    % LoopValue,
	    % HVal,
	    % BSize
	    % BodyLength,
	    % LoopValue
	    ),
	  
	( memberchk(cm_options(Os), Arglist) ->
	  true
        ; Os = [] %% should not happen for normal scps
        ),
	( memberchk(rec(eq(_, subst(_))), Arglist) -> 
	  SubstVal = 1 
        ; SubstVal = 0
        ),

	( cm_option(steq, Os),
	  memberchk(rec(eq(_, ax(reflexivity))), Arglist) ->
	  ( cm_option(steq_x_off, Os) ->
	    ReflexVal = 2 %% for reflexivity after the others
	  ; ReflexVal = 0 %% for reflexivity before the others
	  )
	; ReflexVal = 1
        ),
	
	% *** other equality stuff **** 
	functor(H,Functor,Arity),
	cp_predloop_value(H,Body,PLV),
	% cp_body_conn_value(Body, CPs, ConnVal),
	term_size_1(H-Body, TSize),
	% term_size_1(H, HSize), HVal is 1 / HSize,
	% term_size_1(Body, BSize),
	getarg(is_theorem(IsTheorem), Arglist),
	( IsTheorem = yes -> ThmVal = 0 ; ThmVal = 1),
	getarg(body_length(BodyLength), Arglist),
	( BodyLength = 0 -> BLVal = 0 ; BLVal = 1),
	getarg(cp_loop_value(LoopValue), Arglist),

	ordp_term_depth_change(H, Body, TermDepthChange),

	ordp_key(Body, OrdpKey),
	( cm_option(ordp, Os) ->
	  OrdpKey_00 = OrdpKey,
	  OrdpKey_01 = 0
	; cm_option(ordp1, Os) -> %% useful?
	  OrdpKey_00 = 0,
	  OrdpKey_01 = OrdpKey
	; OrdpKey_00 = 0,
	  OrdpKey_01 = 0
	).
	
	% writeln(Val).

ordp_term_depth_change(H, B, D) :-
	term_depth_butlast(H, D1),
	findall(D2, ( member(B1, B),
		      term_depth_butlast(B1, D2)
		    ),
		Ds2),
	minimum(Ds2, D3),
	D is D3 - D1.

term_depth(F, D) :-
	term_depth_1(F, 0, 0, D).

term_depth_butlast(F, D) :-
	F =.. [_|Args],
	butlast(Args, Args1),
	term_depth_2(Args1, 1, 1, D).

term_depth_1(F, D, M, D1) :-
	compound(F),
	!,
	F =.. [_|Args],
	D2 is D+1,
	term_depth_2(Args, D2, M, D1).
term_depth_1(_, D, M, D1) :-
	D1 is max(M, D).

term_depth_2([X|Xs], D, M, D1) :-
	term_depth_1(X, D, M, D2),
	M1 is max(M, D2),
	term_depth_2(Xs, D, M1, D1).
term_depth_2([], _, M, M).


	
minimum([], 0).
minimum([N|Ns], M) :- minimum(Ns, N, M).
	
minimum([N|Ns], M, M1) :- N < M, !, minimum(Ns, N, M1).
minimum([_|Ns], M, M1) :- minimum(Ns, M, M1).
minimum([], M, M).




%% 
%% Idea here: Prefer those with names that are smaller in the Prolog term
%% ordering. Application: interpolant computation for view rewriting.
%%  
ordp_key(Body, Key) :-
	map_functor_1(Body, Body2),
	sort(Body2, Key).

map_functor_1([X|Xs], Xs1) :-
	functor(X, F, _),
	( atom_prefix(F, 'c_$') ->
	  %% ignore auxiliary symbols here
	  Xs1 = Xs2
	; Xs1 = [F|Xs2]
	),
	map_functor_1(Xs, Xs2).
map_functor_1([], []).


term_size_1(T,N) :-
	term_size_1(T,0,N).

term_size_1(T,N,N) :- var(T), !.
term_size_1(T,N,N1) :- atomic(T), !, N1 is N + 1.
term_size_1(T,N,N1) :-
	functor(T,_,A),
	N2 is N + 1,
	map_term_size_1(T,A,N2,N1).
map_term_size_1(_,0,N,N) :- !.
map_term_size_1(T,A,N,N1) :-
	arg(A,T,T1),
	term_size_1(T1,N,N2),
	A1 is A - 1,
	map_term_size_1(T,A1,N2,N1).


term_functors(T, Fs) :-
	findall(F, term_functor(T,F), Fs1),
	sort(Fs1, Fs).
term_functor(T, _) :-
	var(T),
	!,
	fail.
term_functor(T, T/0) :-
	atomic(T),
	!.
term_functor(T, F) :-
	compound(T),
	functor(T, F1, N1),
	( F = F1/N1
        ; map_term_functor(T, F, N1)
        ).
map_term_functor(_, _, 0) :- !, fail.
map_term_functor(T, F, N) :-
	arg(N, T, T1),
	( term_functor(T1, F)
        ; N1 is N - 1,
          map_term_functor(T, F, N1)
        ).


cp_body_conn_value(Body, CPs, Val) :-
	% product of number of connections of each literal in body
	copy_term(Body, Body1),
	cp_conn_value1(Body1, CPs, 1, Val).

cp_conn_value1([L|Ls], CPs, N, N1) :-
	findall(CP, ( member(CP, CPs),
	              CP = scp(_, _, _, Args),
		      getarg(orig_head(H), Args),
		      \+ \+ unify_with_occurs_check(L, H)
                    ),
		Conns),
	length(Conns, N2),
	N3 is N * N2,
	cp_conn_value1(Ls, CPs, N3, N1).
cp_conn_value1([], _CPs, N, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_cps_old(CPs,CP1s) :-
  writeln(sort_old),	
  cpsize_qsort(CPs,CP1s).	
cpsize_lessq(scp(H1,_,_,Arglist1),scp(H2,_,_,Arglist2)) :- 
  getarg(body_length(BN1), Arglist1),	
  getarg(body_length(BN2), Arglist2),	
  functor(H1,F1,N1),
  functor(H2,F2,N2),
  ( F1 @< F2
  ;  F1 == F2, N1 < N2
  ;  F1 == F2, N1 =:= N2, BN1 < BN2
  ;  F1 == F2, N1 =:= N2, BN1 == BN2,
     getarg(cp_loop_value(LV1), Arglist1),	
     getarg(cp_loop_value(LV2), Arglist2),
     LV1 =< LV2
  ).

cpsize_qsort(Xs,Ys) :-
	cpsize_qsort_dl(Xs,Ys-[]).

cpsize_qsort_dl([],Xs-Xs).
cpsize_qsort_dl([X|Xs],Ys-Zs) :-
	cpsize_partition(Xs,X,Ls,Bs), 
	cpsize_qsort_dl(Ls,Ys-[X|Ys1]), 
	cpsize_qsort_dl(Bs,Ys1-Zs).

cpsize_partition([],_,[],[]).
cpsize_partition([X|Xs],Y,[X|Ls],Bs) :- 
	cpsize_lessq(X,Y), 
	!,
	cpsize_partition(Xs,Y,Ls,Bs).
cpsize_partition([X|Xs],Y,Ls,[X|Bs]) :- 
	cpsize_partition(Xs,Y,Ls,Bs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% MERGE CPS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% merge_cps(CPs1,CPs2,CPs3) both CPs1 and CPs2 are assumed sorted such
%%% that all cps for a predicate follow each other, the clauses of CPs1
%%% are inserted before the clauses of CPs2 for a given predicate. the
%%% order of the clauses within CPs1 and CPs2 remains unaltered. CPs3 also
%%% contains the clauses for predicates occuring only in one of the CP1s
%%% or CP2s
%%% 

merge_cps(CPs2,[],CPs2).
merge_cps(CPs2,[scp(H,B1,B2,V)|CPs],CPs3) :-
	functor(H,F,N),
	xtract_cps_with_pred(F-N,CPs2,CPs3,[scp(H,B1,B2,V)|CPs1],CPs21),
	skip_to_next_pred(F-N,CPs,CPs1,CPs31,CPs4),
	merge_cps(CPs21,CPs4,CPs31).

xtract_cps_with_pred(F-N,[scp(H,B1,B2,V)|Z],[scp(H,B1,B2,V)|Z1],Z2,R) :-
 	functor(H,F,N),
	!,
	xtract_cps_with_pred1(F-N,Z,Z1,Z2,R).	
xtract_cps_with_pred(F-N,[scp(H,B1,B2,V)|Z],Z1,Z2,[scp(H,B1,B2,V)|R]) :- 
	\+(functor(H,F,N)),
	!,
	xtract_cps_with_pred(F-N,Z,Z1,Z2,R).
xtract_cps_with_pred(_,[],Z,Z,[]).

xtract_cps_with_pred1(F-N,[scp(H,B1,B2,V)|Z],[scp(H,B1,B2,V)|Z1],Z2,R) :-
	functor(H,F,N),
	!,
	xtract_cps_with_pred1(F-N,Z,Z1,Z2,R).	
xtract_cps_with_pred1(F-N,[scp(H,B1,B2,V)|Z],Z1,Z1,[scp(H,B1,B2,V)|Z]) :- 
	\+(functor(H,F,N)),
	!.
xtract_cps_with_pred1(_,[],Z,Z,[]).

skip_to_next_pred(_,[],Z,Z,[]) :-
	!.
skip_to_next_pred(F-N,[scp(H,B1,B2,V)|Z],[scp(H,B1,B2,V)|Z1],Z2,Z3) :-
	functor(H,F,N),
	!,
	skip_to_next_pred(F-N,Z,Z1,Z2,Z3).
skip_to_next_pred(F-N,[scp(H,B1,B2,V)|Z],Z1,Z1,[scp(H,B1,B2,V)|Z]) :-
	\+(functor(H,F,N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% STANDARD STUFF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CODE PATTERNS
%%%%
%% 
%% This section contained readable template versions of some algorithms
%% used elsewhere in the program (sorting etc.), might be obsolete now.
%% 
%%% 
%%% skip (element, list, total_result, place_to_insert, rest_after_inserting)
%%% extract (element, list, result, result_rest, not_extracted_elems)
%%% 
%% 
%% skip(X,[],Z,Z,[]).
%% skip(X,[Y|Z],[Y|Z1],V,W) :- X = Y, skip(X,Z,Z1,V,W).
%% skip(X,[Y|Z],Z1,Z1,[Y|Z]) :- \+(X = Y).
%% 
%% xtract(X,[],Z,Z,[]).
%% xtract(X,[Y|Z],[Y|Z1],Z2,R) :- X = Y, xtract1(X,Z,Z1,Z2,R).	
%% xtract(X,[Y|Z],Z1,Z2,[Y|R]) :- \+(X = Y), xtract(X,Z,Z1,Z2,R).
%% xtract1(X,[],Z,Z,[]).
%% xtract1(X,[Y|Z],[Y|Z1],Z2,R) :- X = Y, xtract1(X,Z,Z1,Z2,R).	
%% xtract1(X,[Y|Z],Z1,Z1,[Y|Z]) :- \+(X = Y).
%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SH - HEURISTIC (SETHEO-LIKE) SUBGOAL ORDERING
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sh_sort_subgoals(CPs, CPs1) :-
	info_progress_start,
	length(CPs, LenCPs),
	info(20, 'Heuristic subgoal sorting of ~w contrapositives', [LenCPs]),
	remove_eq_cps(CPs, CPsHeuristic),
	sh_sort_subgoals1(CPs, CPsHeuristic, CPs1),
	info_progress_done.

remove_eq_cps([scp(_,_,_,N)|CPs], CPs1) :-
	getarg_chk(rec(eq(_, _)), N),
	!,
	remove_eq_cps(CPs, CPs1).
remove_eq_cps([CP|CPs], [CP|CPs1]) :-
	remove_eq_cps(CPs, CPs1).
remove_eq_cps([], []).

sh_sort_subgoals1([CP1|CPs1], CPs, CPs2) :-
	info_progress(20, 10),
	sh_sort_cp_subgoals(CP1, CPs, CP3s),
	append(CP3s, CPs4, CPs2),
	sh_sort_subgoals1(CPs1, CPs, CPs4).
sh_sort_subgoals1([], _, []).

%%% no need to sort clauses of length <= 1
%%%
sh_sort_cp_subgoals(scp(H,P,[],N),_, [scp(H,P,[],N)]).
sh_sort_cp_subgoals(scp(H,P,[G],N),_, [scp(H,P,[G],N)]) :- !. 
sh_sort_cp_subgoals(scp(H,P,G,N),_, [scp(H,P,G,N)]) :-
	getarg_chk(clause_options(COs), N),
	memberchk(no_sort, COs),
	!.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% *** disabled, since copy_scp seems too complex to implement correctly
%
% %%%
% %%% special sorting for certain kinds of clauses:
% %%% [experimental, these clauses could be commented out]
% %%%
% %%% * predicate substitutivity
% %%%
% sh_sort_cp_subgoals(scp(H,P,[G1,G2],N),_, SCPs) :-
% 	getarg_chk(rec(eq(_, subst(pred(_-Pos)))), N),
% 	!,
% 	( %% head is the ~ X = Y
%           cp_literal_src(H, neg, EQF, [X, _]), rec_eq_functor(EQF) ->
% 	  arg(Pos, G1, X1),
% 	  ( X1 == X -> 
% 	    XY = [G1,G2], YX = [G2,G1]
% 	  ; XY = [G2,G1], YX = [G1,G2]
%           ),
% 	  PVarX = [var(X) | P],
% 	  PNonvarX = [nonvar(X) | P],
% 
% 	  copy_scp(scp(H,PVarX,YX,N), SCP2),
% 	  SCPs = [scp(H,PNonvarX,XY,N), SCP2]
% 
% 	  % SCPs = [scp(H,PNonvarX,XY,N)]  %% incomplete ?
% 
% 
%         ; ( cp_literal_src(G1, pos, EQF1, [_, _]), rec_eq_functor(EQF1) ->
% 	    EP = [G1,G2], PE = [G2,G1]
% 	  ; EP = [G2,G1], PE = [G1,G2]
%           ),
% 	  arg(Pos, H, X1),
% 	  PVarX = [var(X1) | P],
% 	  PNonvarX = [nonvar(X1) | P],
% 
% 	  copy_scp(scp(H,PVarX,PE,N), SCP2),
% 	  SCPs = [scp(H,PNonvarX,EP,N), SCP2]
% 
% 	  % SCPs = [scp(H,PNonvarX,EP,N)]  %% incomplete ?
%         ).
% %%%
% %%% * transitivity of equality
% %%%
% sh_sort_cp_subgoals(scp(H,P,[G1,G2],N),_, SCPs) :-
% 	getarg_chk(rec(eq(_, ax(transitivity))), N),
% 	cp_literal_src(H, _, _, [X, _]),
% 	!,                        
%         ( cp_literal_src(G1, _, _, [X1, Y1]), ( X1 == X  ; Y1 == X ) ->
% 	  XYYZ = [G1, G2], YZXY = [G2, G1]
%         ; XYYZ = [G2, G1], YZXY = [G1, G2]
%         ),
% 	PVarX = [var(X) | P],
% 	PNonvarX = [nonvar(X) | P],
% 
% 	copy_scp(scp(H,PVarX,YZXY,N), SCP2),
% 
% 	SCPs = [scp(H,PNonvarX,XYYZ,N), SCP2].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% heuristic sorting
%%%
sh_sort_cp_subgoals(scp(H,P,[G|Gs],N), CPs, [scp(H,P,Gs100,N)]) :-
	getarg(orig_head(H0), N),
	rem_argument(H0,H1),
	map_subgoal_vars([G|Gs],Gs1),         % Vars-Goal
	map_subgoal_valspecs(Gs1,H1,CPs,Gs2), % cshl(CV,SV,HV,LV)-Goal
	valspec_sums(Gs2,cshl(0,0,0,0),Sums),

	% nl,
        % pp(specs(Gs2)), nl,
	getarg(cm_options(Os), N),
	map_subgoal_values(Gs2,Sums,Os,Gs3),      % Value-Goal 

        % pp(vals(Gs3)), nl,
	keysort(Gs3,Gs4),

% 	(Gs3 \== Gs4 ->
% 	    nl, nl, 
% 	 pp(specs(Gs2)), nl,
% 	 pp(H-Gs3), nl
%         ; true
%         ),
	
	remove_keys(Gs4, Gs5),
	Gs100 = Gs5.
	    
% copy_scp(SCP1, SCP2) :-
% 	%% copy term - but let the "object-variables" of the clause be shared
% 	%% with original head as required.
% 
% 	%% *** this might be not suffient - other stuff perhaps must be
% 	%% shared too (binding requirements, hconstraints etc)
% 
% 	copy_term(SCP1, SCP2),
% 	SCP1 = scp(_, _, _, Args1),
% 	SCP2 = scp(_, _, _, Args2),
% 	getarg(orig_head(H), Args1),
% 	getarg(orig_head(H), Args2).


valspec_sums([cshl(C,S,H,L)-_|Gs], cshl(C1,S1,H1,L1), X) :-
	C2 is C1 + C,
	S2 is S1 + S,
	H2 is H1 + H,
	L2 is L1 + L,
	valspec_sums(Gs, cshl(C2,S2,H2,L2), X).
valspec_sums([], X, X).


map_subgoal_values([G|Gs],Sums,Os,[G1|Gs1]) :-
	subgoal_value(G,Sums,Os,G1),
	map_subgoal_values(Gs,Sums,Os,Gs1).
map_subgoal_values([],_,_,[]).


pre_1_value(-2000.0).
pre_2_value(-1000.0).
post_value('a').

subgoal_value(cshl(C,S,H,L)-G, cshl(CS,SS,HS,LS), _Os, V-G) :-
	( G = 'c_not_$eq'(_, _) -> pre_1_value(V)
        ; ( G = 'c_$prolog'(_, _) ; G = 'c_$prolog'(_, _, _)) -> pre_2_value(V)
        ; ( G = 'c_$prolog_post'(_, _) ; G = 'c_$prolog_post'(_, _, _)) -> 
	  post_value(V)
        ; G = 'c_$constrain'(_,_,_) -> post_value(V)
        ; G = check_hconstraints_post(_) -> post_value(V)
        ;
 	  %% C - connectivity
	  %% S - speciality
	  %% H - head sharing vars
	  %% L - loop
	  V is ((C / CS) * 1) +
               ((S / SS) * 1) +
	       ((H / HS) * 1) +
	       ((L / LS) * 0)  %% *** ???
        ).


%%
%% H is already stripped off the i-structure argument here
%%
subgoal_valspec(G,_,_,_,cshl(1,1,1,1)) :-
	subgoal_valspec_built_in(G),
	!.
subgoal_valspec(G,Vars,H,CPs,cshl(CV,SV,HV,LV)) :-
	% dot(':'),
	connective_value(G,CPs,CV),
	speciality_value(G,SV),
	head_vars_value(Vars,H,HV),
	loop_value(G,H,LV).

subgoal_valspec_built_in('c_$prolog'(_, _)).
subgoal_valspec_built_in('c_$prolog'(_, _, _)).
subgoal_valspec_built_in('c_$prolog_post'(_, _)).
subgoal_valspec_built_in('c_$prolog_post'(_, _, _)).
subgoal_valspec_built_in('c_$constrain'(_, _, _)).
subgoal_valspec_built_in('c_$eq'(_, _, _)).
subgoal_valspec_built_in('c_not_$eq'(_, _, _)).
subgoal_valspec_built_in('c_$cost'(_, _)).
subgoal_valspec_built_in(check_hconstraints_post(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SORTING STEQ EQUATIONS INTO BODIES
%%%% 
%%%% Todo: more experiments


steq_after_sort_subgoals(CPs, CPs1) :-
	map_steq_cp_after_sort_subgoals(CPs, CPs1).

map_steq_cp_after_sort_subgoals([X|Xs], [X1|Xs1]) :-
	steq_cp_after_sort_subgoals(X, X1),
	map_steq_cp_after_sort_subgoals(Xs, Xs1).
map_steq_cp_after_sort_subgoals([], []).

steq_cp_after_sort_subgoals(scp(H,Pre,B,K), scp(H,Pre,B1,K)) :-
	steq_cp_after_sort_subgoals_1(H, B, B1).

%%%% 
%%%% for hg, experimental: 
%%%%  
% steq_cp_after_sort_subgoals_for_hg(Head, Body, B1) :-
% 	BPost = [],
% 	B = Body,
% 	select_steq_equations(B, EQs, B2),
% 	steq_insert_equations(['$goal'(Head)|B2], EQs, B3),
% 	select('$goal'(_), B3, B4),
% 	append(B4, BPost, B1).

steq_cp_after_sort_subgoals_1(H, B, B1) :-
	select_steq_post_body(B, B2, BPost),
	select_steq_equations(B2, EQs, B3),
	steq_insert_equations(['$goal'(H)|B3], EQs, B4),
	select('$goal'(_), B4, B5),
	append(B5, BPost, B1).

select_steq_post_body([G|Gs], B, BPost) :-
	( is_post_subgoal(G) ->
	  B = B1,
	  BPost = [G|BPost1]
        ; B = [G|B1],
          BPost = BPost1
        ),
	select_steq_post_body(Gs, B1, BPost1).
select_steq_post_body([], [], []).

select_steq_equations([L|Ls], EQs, Ls1) :-
	( steq_equation(L) ->
	  EQs = [L|EQs1],
	  Ls1 = Ls2
        ; EQs = EQs1,
          Ls1 = [L|Ls2]
        ),
	select_steq_equations(Ls, EQs1, Ls2).
select_steq_equations([], [], []).

steq_equation('c_='(_, X, _)) :- var(X).
steq_equation('c_not_='(_, X, _)) :- var(X). %% ?

steq_equation_var('c_='(_,V,_), V).
steq_equation_var('c_not_='(_,V,_), V).
steq_equation_term('c_='(T,_,_), T).
steq_equation_term('c_not_='(T,_,_), T).
steq_equation_term_var('c_='(T,V,_), T, V).
steq_equation_term_var('c_not_='(T,V,_), T, V).

%% - "output" vars in the goal before the others (especially the
%% right argument of an equality goal. [heuristic - to test]
%%
steq_literal_vars('$goal'('c_='(T1, T2, _)), Vars) :-
	!,
	ordered_term_variables(T2-T1, Vars).
steq_literal_vars('$goal'('c_not_='(T1, T2, _)), Vars) :-
	!,
	ordered_term_variables(T2-T1, Vars).
steq_literal_vars('$goal'(L), Vars) :-
	!,
	steq_literal_vars(L, Vars1),
	reverse(Vars1, Vars).
steq_literal_vars(L, Vars) :-
	L =.. [_|Args],
	butlast(Args, Args1),
	ordered_term_variables(Args1, Vars).

steq_insert_equations(B, EQs, B1) :-
	steq_ie_1(B, EQs, B1).

steq_ie_1([L|Ls], EQs, B1) :-
	steq_literal_vars(L, Vs),
	steq_equations_for_vars(Vs, EQs, EQs1, EQsR),
	append(EQs1, [L|B2], B1),
	steq_ie_1(Ls, EQsR, B2).
steq_ie_1([], EQs, B1) :-	
	steq_topsort_equations(EQs, B1).

steq_equations_for_vars([V|Vs], EQs, EQs1, EQsR) :-
	%% var by var, for each var topsorted
	steq_get_equations_for_var(V, EQs, EQs2, EQsR1),
	steq_topsort_equations(EQs2, EQs3),
	append(EQs3, EQs4, EQs1),
	steq_equations_for_vars(Vs, EQsR1, EQs4, EQsR).
steq_equations_for_vars([], EQs, [], EQs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

steq_get_equations_for_var(V, EQs, EQs1, EQsR) :-
	%% EQs1 are the equations that define V and recursively
	%% equations that define vars used in the definitions.
	%% EQsR are the remaining equations
	steq_geqv([V], [V], EQs, EQs1, EQsR).

steq_geqv([V|Vs], AllVs, EQs, EQs1, EQsR) :-
	( select(EQ, EQs, EQs2),
	  steq_equation_var(EQ, V1),
	  V == V1                     ->
	  steq_equation_term(EQ, T),
	  std_term_variables(T, Vs1),
	  subtract_vars(Vs1, AllVs, Vs2),
	  append([V|Vs2], Vs, Vs3),
	  append(Vs2, AllVs, AllVs1),
          EQs1 = [EQ|EQs3]
        ; Vs3 = Vs,
          AllVs1 = AllVs,
          EQs2 = EQs,
	  EQs1 = EQs3
        ),
	steq_geqv(Vs3, AllVs1, EQs2, EQs3, EQsR).
steq_geqv([], _, EQs, [], EQs).

subtract_vars([V|Vs], Vs1, Vs2) :-
	member(V1, Vs1),
	V == V1,
	!,
	subtract_vars(Vs, Vs1, Vs2).
subtract_vars([V|Vs], Vs1, [V|Vs2]) :-
	subtract_vars(Vs, Vs1, Vs2).
subtract_vars([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

steq_topsort_equations(EQs, EQs1) :-
	maplist(steq_equation_var, EQs, ConstrainedVars),
	maplist(steq_add_eq_sizeval, EQs, EQs2),
	keysort(EQs2, EQs3),
	remove_keys(EQs3, EQs4),
	steq_tsh(EQs4, [], ConstrainedVars, EQs1).

steq_tsh([], _, _, []) :- !.
steq_tsh(EQs, SortedVars, ConstrainedVars, [EQ|EQs1]) :-
	( select(EQ, EQs, EQs2),
	  steq_equation_term_var(EQ, T, V),
	  std_term_variables(T, Vs),
	  \+ ( member(V1, Vs), 
	       once(( member(V2, ConstrainedVars), V1 == V2 )),
	       \+ (member(V3, SortedVars), V1 == V3)
	     )
	  -> true
        ; %% allow cycles in the input, use the one with smallest 
	  %% "sizeval" then
	  EQs = [EQ|EQs2],
	  steq_equation_term_var(EQ, _T, V)
        ),
	steq_tsh(EQs2, [V|SortedVars], ConstrainedVars, EQs1).

steq_add_eq_sizeval(EQ, NoOfVars-TermSize-EQ) :-
	steq_equation_term(EQ, T),
	std_term_variables(T, Vs),
	length(Vs, NoOfVars),
	term_size_1(T, TermSize).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% LOOP VALUE
%%%% 

%% 
%% Perhaps more is possible here, for now just loops within the same
%% clause (useful for transitivity clauses).
%%
%% ["loop value" here might be nonsense]

loop_value(G, H, 2) :-
	rem_argument(G, G1),
	copy_term(G1, G2), % weak matching - is copying necessary?
	subsumes_chk(H, G2), %% the subgoal is an instance of the head
	!.
loop_value(_, _, 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SPECIALITY VALUE (Setheo Heuristic)
%%%% 

%%
%% G has one additional arg (its "i-structure")
%%
speciality_value(G,V) :- 
	functor(G,_,N1),
	M1 is N1-1,
	no_of_nonvars(G,M1,N2),
	V is (N1 / (N2+1)).	
no_of_nonvars(T,N,V) :- 
	N > 0, 
	N1 is N - 1, 
	no_of_nonvars(T,N1,V1),
	arg(N,T,A),
	(nonvar(A) -> (V is (1 + V1)) ; V is V1).
no_of_nonvars(_,0,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SHARING OF VARS WITH THE HEAD (Setheo Heuristic)
%%%% 

%% 
%% H is stripped off i-structure argument
%%
head_vars_value(GVars,H,V) :-
	length(GVars,Ng),
	hvv_nh(GVars,H,0,Nh),
	V is ((1 + Ng) / (1 + Nh)).
hvv_nh([V|Vs],H,N,N1) :-
	contains_var(V,H),
	!,
	N2 is N+1,
	hvv_nh(Vs,H,N2,N1).
hvv_nh([_|Vs],H,N,N1) :-
	hvv_nh(Vs,H,N,N1).
hvv_nh([],_,N,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CONNECTIVE VALUE (Setheo Heuristic)
%%%% 

connective_value(G,CPs,V) :-
	copy_term(G, G1),
	cv_sum(G1, CPs , 1, V). 

cv_sum(_,[],N,N).
cv_sum(H,[scp(_,_,_,Arglist)|CPs],N1,N2) :- 
	getarg(body_length(N), Arglist),
	getarg(orig_head(H1), Arglist),
	\+ \+ unify_with_occurs_check(H,H1),
	!,
	N3 is N1 + (1  / (N + 1)),
	cv_sum(H,CPs,N3,N2).
cv_sum(H,[_|CPs],N1,N2) :-
	cv_sum(H,CPs,N1,N2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SORT
%%%% 

%%
%% attach info to the subgoals
%%
map_subgoal_vars([],[]).
map_subgoal_vars([G|Gs],[Vars-G|VGs]) :-
	rem_argument(G,G1),
	ordered_term_variables(G1,Vars),
	map_subgoal_vars(Gs,VGs).

%%
%% H is already stripped off the i-structure argument here
%%
map_subgoal_valspecs([],_,_,[]). 
map_subgoal_valspecs([V-G|VGs],H,CPs,[Val-G|ValGs1]) :-
	subgoal_valspec(G,V,H,CPs,Val),
	map_subgoal_valspecs(VGs,H,CPs,ValGs1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hw_insert_set_resources([CP1|CPs1], Os, [CP2|CPs2]) :-
	hw_cp_insert_set_resources(CP1, Os, CP2),
	hw_insert_set_resources(CPs1, Os, CPs2).
hw_insert_set_resources([], _, []).

hw_cp_insert_set_resources(scp(H, X1, B1, Arglist), Os, 
	                 scp(H, X1, B2, Arglist)) :-
	getarg(d1(DB), Arglist),
	getarg(nfree_add(NFree-NAdd), Arglist),
	hw_cp_isr(B1, Arglist, Os, DB, NFree, NAdd, B2).


hw_cp_isr([L|Ls], Arglist, Os, D1, NFree, NAdd, Ls1) :-
	constr_i(Os, I, [d(D1-_), inf(InfIn-InfOut)]),
	functor(L, _, IIndex),
	arg(IIndex, L, I),
	( Ls = [] ->
	  Ls1 = [L]
        ; SetResources = run_hw_set_resources(NFree, NAdd, InfIn, InfOut, D2),
          Ls1 = [L, SetResources | Ls2],
          hw_cp_isr(Ls, Arglist, Os, D2, NFree, NAdd, Ls2)
        ).
hw_cp_isr([], _Arglist, _Os, _, _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hb_insert_set_resources(CPs, Os, CPs1) :-
	hb_insert_set_resources_1(CPs, Os, CPs, CPs1).

hb_insert_set_resources_1([CP1|CPs1], Os, AllCPs, [CP2|CPs2]) :-
	hb_cp_insert_set_resources(CP1, Os, AllCPs, CP2),
	hb_insert_set_resources_1(CPs1, Os, AllCPs, CPs2).
hb_insert_set_resources_1([], _, _, []).

hb_cp_insert_set_resources(scp(H, X1, B1, Arglist), Os, AllCPs,
	                 scp(H, X1, B2, Arglist)) :-
	getarg(d1(DB), Arglist),
	hb_cp_isr(B1, Arglist, Os, DB, AllCPs, B2).


hb_cp_isr([L|Ls], Arglist, Os, DB, AllCPs, Ls1) :-
	constr_i(Os, I, [d(D1-_)]),
	functor(L, _, IIndex),
	arg(IIndex, L, I),
	hb_compute_factor(L, Os, AllCPs, Factor),
	( Factor = 1 ->
	  D1 = DB,
	  Ls1 = [L | Ls2]
        ; SetResources = run_hb_set_resources(DB, Factor, D1),
	  Ls1 = [SetResources, L | Ls2]
        ),
	hb_cp_isr(Ls, Arglist, Os, DB, AllCPs, Ls2).
hb_cp_isr([], _Arglist, _Os, _, _, []).


hb_compute_factor(Lit, Os, AllCPs, N) :-
	dot('.'),
	hb_no_of_connections(Lit, Os, AllCPs, N).

hb_no_of_connections(Lit, _Os, CPs, N) :-
	hb_conns(Lit, CPs, ConnLits),
	length(ConnLits, N).

hb_conns(Lit, [scp(Lit1,_,Body1,_)|CPs], [Lit1|Lits1]) :-
	Body1 \= [],
	\+ \+ unify_with_occurs_check(Lit, Lit1),
	!,
	hb_conns(Lit, CPs, Lits1).
hb_conns(Lit, [_|CPs], Lits1) :-
	hb_conns(Lit, CPs, Lits1).
hb_conns(_, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% INSERT HCONSTRAINTS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% several ways to insert:
%% clause-local: once after all vars [the constrained var] occur (
%%               accumulated
%% global: implementation with meta-terms
%% (notice: reduction equations could be implemented with eclipse's ~=)
%%
%% currently no inter-resolving of constraints
%%

insert_hconstraints([CP1|CPs1], Os, KbPreds, [CP2|CPs2]) :-
	cp_insert_hconstraints(CP1, Os, KbPreds, CP2),
	insert_hconstraints(CPs1, Os, KbPreds, CPs2).
insert_hconstraints([], _, _, []).


cp_insert_hconstraints(X, Os, _, X) :-
	cm_option(hc_off, Os),
	!.
% cp_insert_hconstraints(scp(H, X1, B1, Arglist), Os, _,
% 	               scp(H, X1, B2, Arglist)) :-
% 	cm_option(hc_ins, Os),
% 	%% first version - perhaps obsolete now (hc_post is better)
% 	!,
% 	getarg(hconstraints(HConstraints), Arglist),
% 	std_term_variables(H, Vars),
% 	irh_ins(B1, Vars, HConstraints, Os, B2).
cp_insert_hconstraints(scp(H, X1, B1, Arglist), Os, _,
	               scp(H, X1, B2, Arglist)) :-
	cm_option(hc_susp, Os),
	!,
	getarg(hconstraints(HConstraints), Arglist),
	std_term_variables(H, Vars),
	irh_susp(B1, Vars, HConstraints, Os, B2).
cp_insert_hconstraints(scp(H, X1, B1, Arglist), Os, KbPreds,
	               scp(H, X1, B2, Arglist)) :-
	cm_option(hc_list, Os),
	!,
	getarg(hconstraints(HConstraints), Arglist),
	getarg(hc_1(Hc1), Arglist),
	getarg(hc_n(HcN), Arglist),
	irh_list(B1, HConstraints, Os, KbPreds, Hc1, HcN, B2).
cp_insert_hconstraints(scp(H, X1, B1, Arglist), Os, _,
	               scp(H, X1, B2, Arglist)) :-
	cm_option(hc_post_accu, Os),
	!,
	%% seems not to bring anything
	getarg(hconstraints(HConstraints), Arglist),
	irh_post_accu(B1, HConstraints, Os, B2).
cp_insert_hconstraints(scp(H, X1, B1, Arglist), Os, _,
	               scp(H, X1, B2, Arglist)) :-
	getarg(hconstraints(HConstraints), Arglist),
	irh_post(B1, HConstraints, Os, B2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_get_hc_in_out(Lit, Os, Hc1, HcN) :-
	constr_i(Os, I, [hc(Hc1-HcN)]),
	functor(Lit, _, N),
	arg(N, Lit, I).

irh_list(Ls, HCs, Os, KbPreds, Hc1, HcN, Ls1) :-
	irh_list_1(Ls, HCs, Os, KbPreds, Hc1, HcN, Ls2, _HCs1),
	% append(HCs1, Ls2, Ls1).
	% append(Ls2, HCs1, Ls1).
	Ls1 = Ls2.

irh_list_1([L|Ls], HCs, Os, KbPreds, Hc1, HcN, [AddHC,L|Ls1], Rest) :-
	( L = 'c_='(E, V, _)
        ; L = 'c_not_='(E, V, _)  %% ?
        ),
	select(HC, HCs, HCs1),
	HC = '$hconstrain'(V1, _, E1),
	V1 == V, E1 == E,
	!,
	lit_get_hc_in_out(L, Os, Hc2, Hc3),
	AddHC = add_hconstraint(HC, Os, Hc1, Hc2),
	irh_list_1(Ls, HCs1, Os, KbPreds, Hc3, HcN, Ls1, Rest).
irh_list_1([L|Ls], HCs, Os, KbPreds, Hc1, HcN, [L|Ls1], Rest) :-
	functor(L, F, N),
	( ( memberchk(F-N, KbPreds)
	  ; L = check_hconstraints_post(_)
          ) ->
	  %% ... maybe also for some other built ins eg $prolog/3 ???
	  lit_get_hc_in_out(L, Os, Hc1, Hc2)
        ; Hc1 = Hc2
        ),
	irh_list_1(Ls, HCs, Os, KbPreds, Hc2, HcN, Ls1, Rest).
irh_list_1([], HCs, _, _, HcN, HcN, [],  HCs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irh_post(Ls, HCs, Os, Ls1) :-
	irh_post_1(Ls, HCs, Ls2, HCs1),
	( cm_option(steq_a_off, Os) ->
	  append(Ls2, HCs1, Ls1)
	; Ls1 = Ls2
	).

% 	% append(HCs1, Ls2, Ls1).
% 	% append(Ls2, HCs1, Ls1).
% %% Fri Apr 20 19:42:56 2018
% %% This was chosen before (some constraints are dropped)
% %% - for what reason?
% %% Did it showed better in experiments?
%% 	Ls1 = Ls2.

irh_post_1([L|Ls], HCs, [L,HC|Ls1], Rest) :-
	L = 'c_='(E, V, _),
% 	( L = 'c_='(E, V, _)
%         ; L = 'c_not_='(E, V, _)  %% ?
%         ),
	select(HC, HCs, HCs1),
	HC = '$hconstrain'(V1, _, E1),
	V1 == V, E1 == E,
	!,
	irh_post_1(Ls, HCs1, Ls1, Rest).
irh_post_1([L|Ls], HCs, [L|Ls1], Rest) :-
	irh_post_1(Ls, HCs, Ls1, Rest).
irh_post_1([], HCs, [], HCs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irh_post_accu(Ls, HCs, _Os, Ls1) :-
	irh_post_accu_1(Ls, HCs, [], Ls2, _HCs1),
	Ls1 = Ls2.

irh_post_accu_1([L|Ls], HCs, SoFar, Ls1, Rest) :-
	( L = 'c_='(E, V, _)
        ; L = 'c_not_='(E, V, _)  %% ?
        ),
	select(HC, HCs, HCs1),
	HC = '$hconstrain'(V1, _, E1),
	V1 == V, E1 == E,
	!,
	append([L,HC|SoFar], Ls2, Ls1),
	irh_post_accu_1(Ls, HCs1, [HC|SoFar], Ls2, Rest).
irh_post_accu_1([L|Ls], HCs, SoFar, [L|Ls1], Rest) :-
	irh_post_accu_1(Ls, HCs, SoFar, Ls1, Rest).
irh_post_accu_1([], HCs, _, [], HCs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

irh_ins(Ls, _, [], _, Ls) :- !.
irh_ins(Ls, Vars, HCs, Os, Ls1) :-
	select_hconstraints_ins(HCs, Vars, Os, HCs1, HCs2),
	append(HCs1, Ls2, Ls1),
	( Ls = [] ->
	  Ls2 = []
        ; Ls = [L|Ls3],
	  Ls2 = [L|Ls4],
	  std_term_variables(Vars-L, Vars1),
	  irh_ins(Ls3, Vars1, HCs2, Os, Ls4)
        ).

select_hconstraints_ins([HC|HCs], Vars, Os, HCs1, HCs2) :-
	std_term_variables(HC, Vars1),
	( each_var_in_term(Vars1, Vars) ->
	  HCs1 = [HC|HCs3],
	  HCs4 = HCs2
        ; HCs1 = HCs3,
          [HC|HCs4] = HCs2
        ),
	select_hconstraints_ins(HCs, Vars, Os, HCs3, HCs4).
select_hconstraints_ins([], _, _, [], []).

% select_hconstraints_accumulate([HC|HCs], Vars, Os, HCs1) :-
% 	std_term_variables(HC, Vars1),
% 	( each_var_in_term(Vars1, Vars) ->
% 	  HCs1 = [HC|HCs3]
%         ; HCs1 = HCs3
%         ),
% 	select_hconstraints_accumulate(HCs, Vars, Os, HCs3).
% select_hconstraints_accumulate([], _, _, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% :- lib(suspend). Eclipse only.
%% 

irh_susp(Ls, _, [], _, Ls) :- !.
irh_susp(Ls, _Vars, HCs, _Os, Ls1) :-
	append(HCs, Ls, Ls1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% hc_list
%%%% 

make_empty_hconstraints([]).

expand_add_hconstraint(HCE, L, L1) :-
	append(HCE, L, L1).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% check_hconstraints(L, L) :-
% 	call(L).
% 
% make_empty_hconstraints(true).
% 
% expand_add_hconstraint(HCE, L, (HCE1, L)) :-
% 	cm_list_to_andseq(HCE, HCE1).
%  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REDUCTION EQUATIONS
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_red_equations([CP1|CPs1], Os, [CP2|CPs2]) :-
	info_progress(20,100),
	cp_insert_red_equations(CP1, Os, CP2),
	!, %% for SWI's local stack size
	insert_red_equations(CPs1, Os, CPs2).
insert_red_equations([], _, []).

cp_insert_red_equations(scp(H, X1, B1, Arglist), Os,
	                scp(H, X1, B2, Arglist)) :-
	getarg(binding_list(Bindings), Arglist),
	ire(B1, [H], Bindings, Os, B2).

ire([L|Ls], Prefix, Bindings, Os, [L|Ls1]) :-
	no_red_equation_literal(L),
	!,
	ire(Ls, Prefix, Bindings, Os, Ls1).
ire([L|Ls], Prefix, Bindings, Os, Ls2) :-
	map_reqs(Bindings, Prefix, [L|Ls3], Os, Ls2),
	ire(Ls, [L|Prefix], Bindings, Os, Ls3).
ire([], Prefix, Bindings, Os, Ls) :-
	map_reqs(Bindings, Prefix, [], Os, Ls).


%%
%% todo: no_red_eq_lits are maybe better AFTER the last constraints.
%%
no_red_equation_literal( run_hw_set_resources(_, _, _, _, _) ).
no_red_equation_literal( run_hb_set_resources(_, _, _) ).
no_red_equation_literal( check_hconstraints_post(_) ).

%%%% 
%%%% Version without constraints
%%%% 
map_reqs([], _, Es, _, Es).
map_reqs([binding(L,R)|Bindings], Prefix, Es1, Os, Es2) :-
	each_var_in_term(L, Prefix),
	( cm_option(rsub, Os) ->

	  % hmm ? subsumes_chk seemed incomplete (experiment) but not
          % with all_vars_also_in here too:
	  all_vars_also_in(R, Prefix), % ???

	  true
        ;
	  %  
	  % things like \+ subsumes_chk(s(X,Y), s(f(A),g(A)))
          % (with A not occuring before) make sense, 
          % s(X,Y) \== s(f(A),g(A)) however not
          %
	  all_vars_also_in(R, Prefix)
        ),
	!,
	termlist_to_equation_side(L,E1),
	termlist_to_equation_side(R,E2),
	( cm_option(rsub, Os) ->
	  NEQ = \+(run_subsumes_chk(E1, E2))
	; NEQ = (E1 \== E2)
        ),
	map_reqs(Bindings, Prefix, [NEQ|Es1] , Os, Es2).
map_reqs([_|Bs], Prefix, Es1, Os, Es2) :-
	map_reqs(Bs, Prefix, Es1, Os, Es2).


termlist_to_equation_side([], empty). % empty \== empty is generated 
                                      % for clauses always subsumed
termlist_to_equation_side([Term], Term).
termlist_to_equation_side([T1,T2|Terms], Side) :-
	Side =.. [s,T1,T2|Terms].

all_vars_also_in(Term1, Term2) :-
	% all vars in term1 also in term2
	std_term_variables(Term1, Vars1),
	each_var_in_term(Vars1, Term2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SUBSUMPTION NEQUATIONS (see setheo) (r4)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% clause_subsumes(+Clause1,+Clause2,-Bindings)
%%% 
%%% Enumerates bindings of variables in Clause2 for which C1 subsumes C2.
%%% The Cs are clauses as lists of literals. C1 subsumes C2 if C1 is a
%%% (not necessarily proper) subset of C2. A binding is represented as
%%% list of values in the same order as a list of variables obtained by
%%% ordered_term_variables/2.
%%% 
%%% The bindings are also applied to C1 and C2.
%%% In comparing literals the occur-check unify/2 is used.
%%% 
%%% [ this extends what is written about setheo 92 by optionally using
%%% subsumes_chk instead of neq (might be useful if function symbols
%%% are involved). 
%%% ??? under what condition subsumes_chk is incomplete/wrong ?
%%% --- currently not used ]
%%%

clause_subsumes(C1,C2,X) :-
	ordered_term_variables(C2, X),
	clause_subsumes1(C1, C2).

clause_subsumes1([],_).
clause_subsumes1([L|Ls],C2) :-
	oc_member(L,C2),
	clause_subsumes1(Ls,C2).

%%% 
%%% clause_proper_subsumes(+Clause1,+Clause2,-Bindings)
%%%
%%% as clause_subsumes/3, but C1 must be a proper subset of C2
%%% 

clause_proper_subsumes(C1,C2,X) :-
	clause_subsumes(C1,C2,X),
	\+ clause_subsumes1(C2,C1).


%%% 
%%% map_subs_binding_rights(+Cs,-Bindingss)
%%%
%%% Cs - list of clauses (see clause subsumes)
%%% Bindingss - a list of lists of bindings for each clause 
%%% (same order as Cs) under which the clause is subsumed by another one.
%%% 
%%% [precise result might depend on ordering of clauses]
%%%

map_subs_binding_rights(Cs, Limit, Bss) :-
	length(Cs, LenCs),
	info(20, 'Determining subsumption bindings for ~w clauses', [LenCs]),
	info_progress_start,
	( Limit =< 0 ->
	  msb([], Cs, Limit, Bss)
	; call_with_inference_limit( msb([], Cs, Limit, Bss),
				     Limit,
				     Result ),
	  ( Result = inference_limit_exceeded ->
	    info(20, 'Inference limit ~w reached: resorting to unit subsumption bindings', [Limit]), %
	    msb_unit([], Cs, Limit, Bss)
	    % map_emptylist(Cs, Bss)
	  ; true
	  )
	),
	info_progress_done.

map_emptylist([_|Xs], [[]|Xs1]) :-
	map_emptylist(Xs, Xs1).
map_emptylist([], []).

msb(_, [], _, []).
msb(Cs1, [C|Cs], Limit, [Bs|Bss]) :-
	info_progress(20, 10),
	copy_term(C, CC),
	findall(B, msb_1(Cs1, Cs, CC, B), Bs0),
        sort(Bs0,Bs),
	msb([C|Cs1], Cs, Limit, Bss).

msb_1(Cs1, Cs, CC, B) :-
	( member(C0,Cs1),
	  clause_subsumes(C0,CC,B)
	; member(C0,Cs),
	  clause_proper_subsumes(C0,CC,B)
	).





% msb(_, [], _, []).
% msb(Cs1, [C|Cs], Limit, [Bs|Bss]) :-
% 	copy_term(C, CC),
% 	% C = CC,
% 	findall(B,
% 	        ( member(C0,Cs1),
% 		  call_bounded(call_bounded_1, Limit,
% 		      clause_subsumes(C0,CC,B))
%                 ; member(C0,Cs),
% 		  call_bounded(call_bounded_1, Limit,
% 		      clause_proper_subsumes(C0,CC,B))
% 		),
% 		Bs0),
%         sort(Bs0,Bs),
% 	msb([C|Cs1], Cs, Limit, Bss).
% 
% :- def_flag(call_bounded_1).
% 
% call_bounded(Counter, N, Call) :-
% 	setval(Counter, N),
% 	call(Call),
% 	decval(Counter),
% 	( getval(Counter, -1) ->
% 	  !,
% 	  fail
%         ; true
%         ).% 

%%% 
%%% Remarks
%%% 
%%% clause_[proper_]subsumes must be called with standardized apart
%%% "variants" of the Cs. (this is done implicitely here within "findall").
%%% [hmm - seems not be done implicitely, therefore inserted copy_term]
%%%
%%% the returned bss also contain "variants" of the bindings, which should
%%% describe bindings to atoms or repeated variables properly
%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TAUTOLOGY NEQUATIONS (see setheo) (r5)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% clause_tautological(+Clause,-Bindings)
%%% 
%%% Enumerates bindings of variables in Clause for which it 
%%% becomes tautological (analogous to clause_subsumes/3).
%%% 

clause_tautological([X=Y],_) :-
	%% keep reflexivity clause for =/2
	X == Y,
	!,
	fail.
clause_tautological(C,_) :-
	memberchk(~'$options'(ClauseOptions), C),
	memberchk(accept_taut, ClauseOptions),
	!,
	fail.
clause_tautological(C,Bs) :-
	ordered_term_variables(C,Bs),
	ct1(C).

%% new 2015
%%
ct1([X=Y|_]) :-
	unify_with_occurs_check(X, Y).
%%
ct1([L|Ls]) :-
	matlit_complement(L,L1),
	oc_member(L1,Ls).
ct1([_|Ls]) :-
	ct1(Ls).

%%% 
%%% map_taut_bindings(+Clauses,-Bindings)
%%% 
%%% analogous to map_subs_bindings
%%% 

map_taut_binding_rights([],[]).
map_taut_binding_rights([C|Cs],[Bs|Bss]) :-
        findall(B, clause_tautological(C,B), Bs0),
	sort(Bs0,Bs),
        map_taut_binding_rights(Cs,Bss).


matlit_complement(~X,X) :- !.
matlit_complement(X,~X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PREPARE REDUCTION EQUATIONS 
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% prepare reduction equations 
%%% 
%%% Extract reduction equations from the matrix.
%%% 
%%% Vs contains the variable (left) side of the equations
%%% Reqs the right sides.
%%% both are list of lists (mappable to Clauses), of lists
%%% (left rsp. right sides, mappable to each other)
%%% 

prepare_reqs_binding_list(Clauses, Os, Bindings) :-
	
	% this is based on the input matrix

	remove_option_literals(Clauses, Clauses1),

	( cm_option( r4_limit(Limit), Os) -> true ; Limit = -1 ),

	% writeln(prep_r4(Limit)),
	
	( cm_option(r4, Os) ->
	  map_subs_binding_rights(Clauses1, Limit, SubsRights)
	; cm_option(r4a, Os) ->
	  ( cm_option( r4a_max(Max), Os) -> true ; Max = -1 ),
	  map_subs_binding_rights_unit(Clauses1, Max, SubsRights)
	; true
	),
	
	% writeln(prep_r4_done),

	( cm_option(r5, Os) ->
	  map_taut_binding_rights(Clauses1, TautRights)
	; true
	),

	((cm_option(r4, Os);cm_option(r4a, Os)) ->
	  ( cm_option(r5, Os) -> 
	    merge_binding_rights(SubsRights, TautRights, Rights1)
	  ; Rights1 = SubsRights
	  )
        ; ( cm_option(r5, Os) ->
	    Rights1 = TautRights
	  ; true
	  )
        ),

	convert_to_bindings(Clauses1, Rights1, Bindings).

remove_option_literals(M, M1) :-
	maplist(rol, M, M1).
rol([],[]).
rol([~'$options'(_)|Ls], Ls1) :- 
	!,
        rol(Ls, Ls1).
%% hconstraints might need to be considered for subsumption too,
%% so they are not removed here.
rol([L|Ls], [L|Ls1]) :-
	rol(Ls, Ls1).


convert_to_bindings(Clauses, Rights, Bindings) :-
	ctb1(Clauses, Rights, Bindings).

ctb1([], [], []).
ctb1([C|Cs], [Rs|Rss], [Bs|Bss]) :-
	ordered_term_variables(C,Ls),
	ctb2(Ls, Rs, Bs1),
	simplify_bindings(Bs1, Bs),
	ctb1(Cs, Rss, Bss).
	
ctb2(_, [], []).
ctb2(L, [R|Rs], [binding(L,R)|Bs]) :-
	ctb2(L, Rs, Bs).
	
merge_binding_rights([],[],[]).	  
merge_binding_rights([Bs1|Bss1],[Bs2|Bss2],[Bs|Bss]) :-
	append(Bs1,Bs2,Bs),
	merge_binding_rights(Bss1,Bss2,Bss).

simplify_bindings(Bindings,Bindings1) :-
  map_simplify_binding(Bindings, Bindings2),
  sort(Bindings2, Bindings1). % remove duplicates modulo '=='

 
map_simplify_binding([],[]).
map_simplify_binding([B|Bs],[B1|Bs1]) :-
	simplify_binding(B,B1),
	map_simplify_binding(Bs,Bs1).

simplify_binding(binding(L,R), binding(L1,R1)) :-
	simplify_binding(L,R,[],[],L1,R1).

simplify_binding([], [], _, _, [], []).
simplify_binding([L|Ls], [R|Rs], Vars, PrevRs, Ls1, Rs1) :-
        %%
        %% this is unfortunately not so trivial
        %% (1st occurence of a var is propagated by unification into
        %% the right side)
        %% see also map_reqs/5
        %%
	( var(R)                     ->  %% right side is a variable
          ( ( contains_var(R, Rs)
            ; contains_var(R, PrevRs)
            )                        ->  %%   contained in another right side
	    ( absmember(R, Vars)     ->  %%     already treated
	      Vars1 = Vars, 
              Ls1 = [L|Ls2],             %%       take the item     
	      Rs1 = [R|Rs2]
            ;                            %%     not yet treated
	      L = R,                     %%       unify it with left side
	      Vars1 = [R|Vars],          %%       mark as treated
              Ls1 = Ls2,                 %%       drop the item
	      Rs1 = Rs2	       
            )
	  ;                              %%   not contained in another right s.
	      Vars1 = Vars, 
              Ls1 = Ls2,                 %%     drop the item
	      Rs1 = Rs2	       
	  )
        ;                                %% right side is not a variable
	      Vars1 = Vars, 
              Ls1 = [L|Ls2],             %%     take the item     
	      Rs1 = [R|Rs2]
        ),	  
	simplify_binding(Ls, Rs, Vars1, [R|PrevRs], Ls2, Rs2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% BUILT-INS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% (might the info passed by I be useful for some uses of prolog-calls?)
%% 
%% propagation of negation?
%% 

built_in_fns(['c_$prolog'/2, 'c_not_$prolog'/2,
              'c_$prolog'/3, 'c_not_$prolog'/3,
	      'c_$prolog_post'/2, 'c_not_$prolog_post'/2,
              'c_$prolog_post'/3, 'c_not_$prolog_post'/3,
	      'c_$options'/2, 'c_not_$options'/2,
	      'c_$constrain'/3, 'c_not_$constrain'/3,
	      'c_$hconstrain'/4, 'c_not_$hconstrain'/4,
	      'c_$eq'/3, 'c_not_$eq'/3,
              'c_$cost'/2, 'c_not_$cost'/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% a "for now" implementation, see also subgoal_value
%% 

'c_$constrain'(X,Y,_) :-
	oc_member(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% % only for hs:
% mk_cost_clause(Os,('c_$cost'(X,I) :- D1 >= X, D2 is D1-X)) :-
% 	constr_i(Os,I,[d(D1-D2),t(T-T),p(cost)]).
% 

mk_builtin_clauses(_Os, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unwrap_built_ins([],_,[]).
unwrap_built_ins([scp(H,P,B,I) |CPs],Os,[scp(H,P,B1,I)|CPs1]) :-
	ubi1(B,Os,B1),
	unwrap_built_ins(CPs,Os,CPs1).

%% $eq implementation in Eclipcse:
%%
%% does ~= need occur check ??? Eclipse's X ~= f(X) seems to behave 
%% identically with either setting of the flag. (ie. creates the 
%% delayed goal X ~= f(X)).
%% 

%%
%% unify D and T from previous and next subgoal (rsp. head)
%%
ubi1(X, _, X) :-
	%% Dummy clauses to have predicates defined might come with a a
	%% variable here
	var(X),
	!.
ubi1([L|Ls], Os, Ls1) :-
	ubi_lit(L, Os, Ls2),
	!,
	append(Ls2, Ls3, Ls1),
	ubi1(Ls, Os, Ls3).
ubi1([L|Ls], Os, [L|Ls1]) :-
	ubi1(Ls, Os, Ls1).
ubi1([], _, []).

ubi_lit('c_$eq'(X,Y,I), Os, Ls)  :- 
	constr_ubi_standard_i(Os, I),
	( unify_with_occurs_check(X, Y) -> Ls = [] ; Ls = [fail] ).
ubi_lit('c_not_$eq'(X,Y,I), Os, ['~='(X, Y)]) :- constr_ubi_standard_i(Os, I).
ubi_lit('c_$prolog'(X,I), Os, [X])          :- constr_ubi_prolog_call_i(X, Os, I).
ubi_lit('c_$prolog_post'(X,I), Os, [X])     :- constr_ubi_prolog_call_i(X, Os, I).
ubi_lit('c_$prolog'(X,I1,I2), Os, Code)        :-
	%% do not unify I1 and I2 at compilation because different contrapositives at
	%% this point may share variables
	constr_ubi_standard_i(Os, I2),
	term_variables(X, Vs1),
	sort(Vs1, Vs1a),
	term_variables(I1, Vs2),
	sort(Vs2, Vs2a),
	( ord_intersect(Vs1a, Vs2a) ->
	  Code = [I1 = I2, X]
	; Code = [X]
	).

ubi_lit('c_$prolog_post'(X,I1,I2), Os, R)   :-
	ubi_lit('c_$prolog'(X,I1,I2), Os, R).

	
ubi_lit('$hconstrain'(A, B, C), Os, Ls)     :-
	hconstraint_expansion('$hconstrain'(A, B, C), Os, Ls).
ubi_lit(add_hconstraint(HC, _Os, L, L1), Os, []) :-
	std_hconstraint_expansion(HC, Os, HCE),
	expand_add_hconstraint(HCE, L, L1).
ubi_lit(check_hconstraints_post(I), Os, [run_check_hconstraints(Hc1, HcN)]) :- 
	constr_i(Os, I, [d(D-D),t(T-T),inf(Inf-Inf),
	                 hc(Hc1-HcN)]).

constr_ubi_standard_i(Os, I) :-
	constr_i(Os,I,[d(D-D),t(T-T),inf(Inf-Inf),hc(Hc-Hc)]).

constr_ubi_prolog_call_i(Call, Os, I) :-
	constr_i(Os,I,[d(D-D),t(T-T),inf(Inf-Inf),hc(Hc-Hc),p(b(Call))]).

hconstraint_expansion(A, Os, B) :-
	( cm_option(hc_susp, Os) ->
	  susp_hconstraint_expansion(A, Os, B)
        ; std_hconstraint_expansion(A, Os, B)
        ).

std_hconstraint_expansion('$hconstrain'(X, lex_lessq, T), _Os, Ls) :-
	!,
	Ls = [\+ run_lrpo_greater(X, T)].
std_hconstraint_expansion('$hconstrain'(X, lex_less, T), _Os, Ls) :-
	!,
	%% for STEQ application X \== T is (clause locally) already
	%% ensured by r4 the with reflexivity axiom (less_lex is
	%% only used for positive equations)
        Ls = [ X \== T,
	       \+ run_lrpo_greater(X, T)
	     ].
std_hconstraint_expansion(X, _Os, _) :-
	err('No expansion for constraint: ~q', [X]).

susp_hconstraint_expansion('$hconstrain'(X, lex_lessq, T), _, Ls) :-
	!,
	Ls = [ ( var(X) -> 
	         suspend( \+ run_lrpo_greater(X, T), 3, (X->inst))
               ; \+ run_lrpo_greater(X, T)
	       ) ].
susp_hconstraint_expansion('$hconstrain'(X, lex_less, T), _, Ls) :-
	!,
	Ls = [ ( var(X) ->
	         suspend( X \== T, 3, (X->inst)),
		 suspend( \+ run_lrpo_greater(X, T), 3, (X->inst))
	       ; X \== T,
          	 \+ run_lrpo_greater(X, T)
	       )
	     ].
susp_hconstraint_expansion(X, _Os, _) :-
	err('No susp-expansion for constraint: ~q', [X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% END-OF CM
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PR - RUNTIME
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% USE BOUND
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- def_flag(use_bound).
:- def_flag(use_bound_reached).

:- setval(use_bound, 1).

init_use_bound(UseBoundTable, CMOptions) :-
	run_module(CMOptions, RUNCM),
	setval(use_bound_reached, false),
	getval(use_bound, UseBound),
	RUNCM:dec_next_clause_index(I),
	iubt1(I, UseBound, Args),
	UseBoundTable =.. [u|Args].

iubt1(1, _, []) :- !.
iubt1(N, B, [B|Bs1]) :-
	N1 is N - 1,
	iubt1(N1, B, Bs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PROOF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proof(M) :- proof(M,[]).

proof(M,Os) :-
	run_module(Os, RUNCM),
	check_proof_args(M,Os),
	RUNCM:dec_cm_options(CMOs),
	optmember(t(T),Os,t(T)),
	optmember(p(P),Os,p(P)),
	optmember(d(D),Os,d(0)),
	optmember(i(I),Os,i(1)),
	optmember(m(MAX),Os,m(-1)),
	optmember(u(UMAX),Os,u(1)),
	catch( proof1(M,D,I,MAX,UMAX,T,P,CMOs,Os),
	       proof,
	       fail
             ).

% this is just for now to trap common errors
check_proof_args([[_|_]|_],Os) :- !, check_proof_options(Os).
check_proof_args(_-[[_|_]|_],Os) :- !, check_proof_options(Os).
check_proof_args(_,_) :-
	err('No proper matrix as input to proof').

check_proof_options(Options) :-
	( member(Option, Options),
	  \+ member(Option, [t(_), p(_), d(_), i(_), m(_), u(_), cq,
			     depth_timeout(_),
			     calls, run_module(_)]) ->
	  err('Bad option as input to proof: ~q', [Option])
	; true
	).

optmember(X,[X|_],_) :- !.
optmember(X,[_|Xs],D) :- optmember(X,Xs,D).
optmember(X,[],X).

proof1(M,D,I,MAX,UMAX,T,P,CMOs,PROs) :-
	pr_banner,
	run_module(PROs, RUNCM),
	( cm_option(steq, CMOs) ->
	  install_lex_ordering(RUNCM)
        ; true
        ),
	setval(use_bound, UMAX),
	default_abstrterm(M,T1-G),
	(   ( cm_option(rus, CMOs) ; cm_option(frs, CMOs)) -> 
            negated_query_rules(T1-G, CMOs, NQCPs)
        ;   NQCPs = [],
	    true
	),
	( cm_option(frs, CMOs) ->
	  install_freezesolve(NQCPs)
        ; true
        ),
	info(20, 'Translating query'),
	matrix_to_query(T1-G, NQCPs, CMOs, Q),
	( memberchk(cq, PROs) ->
	  choose_query(Q,Q1)
        ; Q1 = Q
        ),
	( memberchk(calls, PROs) ->
	  write('\n--- The following calls are generated from the query:\n\n'),
	  ( member(qu(Q2,_,_,_,_,_,_,_),Q1),
	    pp_clause(Q2), 
            fail
          ; nl, nl, write('---'),
            !, true
          )
        ; init_alem,
          current_cpu_time(T0),
	  flag(duration_in_last_depth, _, 0),
	  flag(duration_in_last_last_depth, _, 0),
% 	  flag(last_depth, _, 0),
% 	  flag(last_last_depth, _, 0),
	  info(20, 'Starting search'),
	  ( memberchk( depth_timeout(DTO), PROs) ->
	    it_deep_proof(Q1,D,I,-1,MAX,T,P,T0,DTO,RUNCM)
	  ; it_deep_proof(Q1,D,I,-1,MAX,T,P,T0,-1,RUNCM)
	  )
        ).

%%%%%%%%%%%%%%%%%%%%

install_lex_ordering(RUNCM) :-
	!,
	RUNCM:dec_kb_lex(Lex),
	RUNCM:dec_kb_functions(Functions),
	( atom(Lex) ->
	  default_lrpo_ordering(Lex, Functions, Spec)
	; Spec = Lex
	),
	runinfo([nl, 'Using lex: ', Spec]),
	install_lrpo_ordering(Spec).

%%%%%%%%%%%%%%%%%%%%

choose_query([Q], [Q]) :- !.
choose_query(Qs, [Q]) :-
	maplist(cq_value, Qs, Qs1),
	keysort(Qs1, [_-Q|_]).

cq_value(qu(Q1,D,DN,T,P,U,I1,IN), (PV-SV)-qu(Q1,D,DN,T,P,U,I1,IN)) :-
	( cq_positive(Q1) -> PV = 0 ; PV = 1 ),
	cq_size(Q1, SV).

cq_positive((~_,_)) :- !, fail.
cq_positive((_,Y)) :- !, cq_positive(Y).
cq_positive(~_) :- !, fail.
cq_positive(_).

cq_size((_,Y), N) :- !, cq_size(Y, N1), N is 1 + N1.
cq_size((_), 1).

%%%%%%%%%%%%%%%%%%%%


install_freezesolve(CPS) :-
	writeln(install(CPS)),
	ifr1(CPS, Clauses),
	% eclipse specific

	writeln(Clauses).
	
	% compile_term(Clauses). *** not ported to SWI

ifr1([],[]).
ifr1([clause(H,B)|CPs], [(freezesolve(H) :- B)|Cs]) :-
	ifr1(CPs, Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ITERATIVE DEEPENING
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% it_deep_proof
%%% 
%%% start at depth D, increment depth by INC (0 for automatic)
%%% don't report solutions below or equal level DMIN (works not for 'hd'),
%%% abstraction terms T, proof P 
%%% 
%%% fail if depth exceeds DMAX (-1 for don't fail)
%%%
it_deep_proof(Q,D,_,DMIN,DMAX,T,P,T0,DTO,RUNCM) :-
	(( DMAX = -1 ; D =< DMAX ) -> true ; !, fail),
	runinfo([nl, 'depth ', d5(D)]),
	RUNCM:dec_cm_options(CMOS),
	( cm_option(inc, CMOS) ->
	  init_bound_increment(0)
        ; cm_option(inc1, CMOS) ->
	  init_bound_increment(0)
        ; init_bound_increment(1)
        ),
	( cm_option(ub, CMOS) ->
	  init_use_bound(UBT, CMOS)
        ; true
        ),
	member(qu(Q1,D,DN,T,P,UBT,0,_),Q),
	( DTO = -1 ->
	  RUNCM:call(Q1)
	; number(DTO) ->
	  catch( call_with_time_limit(DTO, RUNCM:call(Q1)), time_limit_exceeded, fail )
	; DTO = d(NDTO) ->
	  NDTO1 is D*NDTO,
	  catch( call_with_time_limit(NDTO1, RUNCM:call(Q1)), time_limit_exceeded, fail )
	),  
	%% 'hd' does not return solution depth (for now?)
	%% this saves a variable
	%% 'hs' returns depths anyway
	( cm_option(hs, CMOS) ->
          DN1 is (D - DN),
	  DN1 > DMIN
        ; true
        ),
	runtime_since(T0,TD),
	runinfo([nl, '----- solution after  ', d10(TD), ' msec.\n']).
it_deep_proof(Q,D,INC,_,DMAX,T,P,T0,DTO,RUNCM) :-
	get_bound_increment(REQ),
	REQ > 0,	           %% depth bound has been reached
	runtime_since(T0,TD),
	flag(duration_in_last_depth, TDLast, TD),
	flag(duration_in_last_last_depth, TDLastLast, TDLast),
% 	flag(last_depth, LD, D),
% 	flag(last_last_depth, LLD, LD),
	runinfo([' ', d10(TD), ' msec']),
	( INC = 0 ->
	  D1 is D + REQ
	; INC = dyna(DynaThresholdFactor,
		     DynaIncreaseFactor,
		     DynaThresholdTime,
		     DynaThresholdDepth,
		     DefaultIncrease) ->
	  MAXNUM = 9223372036854775808,
	  ( D > MAXNUM -> !, fail ; true ),
	  ( once(( D > DynaThresholdDepth ; TD > DynaThresholdTime )),
	    TD < DynaThresholdFactor*TDLast,
	    TDLast < DynaThresholdFactor*TDLastLast,
	    D < MAXNUM
	    ->
	    runinfo([' dynamic increase']),
	    D1 is max(D*DynaIncreaseFactor, D+DefaultIncrease)
	  %% nl, writeln(dyna(D, LD, LLD, DynaIncreaseFactor, DefaultIncrease, D1))
	  ; D1 is D+DefaultIncrease
	  )
	; number(INC) -> D1 is D + INC
	; err('Bad increment specifier: ~q', [INC])
        ),
	current_cpu_time(T1),
	it_deep_proof(Q,D1,INC,D,DMAX,T,P,T1,DTO,RUNCM).

runtime_since(Since,TD) :-
	current_cpu_time(T1),
	TD is T1 - Since.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% QUERY INSTALLATION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% install_query
%%% 
%%% installing the contrapositives corresponding to the negated query,
%%% installing also other clauses for predicates in the query which are
%%% not in the kb
%%% 

negated_query_rules(T-G, Os, CPs) :-
	info(20, 'Preparing negated query'),
	run_module(Os, RUNCM),
	negate_seq_seq(G,G1),
	RUNCM:dec_next_clause_index(NextClauseIndex),
	matrix_to_cps(T-G1, NextClauseIndex, _PREs, CPs1, _POSs, _FNs, ignore, Os),
	map_prepare_cp(CPs1, CPs).

map_prepare_cp([],[]). 
map_prepare_cp([X|Xs],[Y|Ys]) :- 
	copy_term(X,X1), % perhaps not really needed here ?
	cp_to_prolog_clause(X1,C),
	( C = (Head :- Body) ->
	  Y = clause(Head,Body)
        ; Y = clause(C,true)
        ),
	map_prepare_cp(Xs,Ys).

negate_seq_seq([C|Cs],[C1|Cs1]) :- 
	negate_seq(C,C1), negate_seq_seq(Cs,Cs1).
negate_seq_seq([],[]).
negate_seq([L|Ls],[L1|Ls1]) :- 
	matlit_complement(L,L1), negate_seq(Ls,Ls1).
negate_seq([],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% INPUT MATRIX TO QUERY
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% 
%%% matrix_to_query
%%% 
%%% matrix to corresponding prolog-query (or is not yet expressed by ;)
%%% 

matrix_to_query(M, N, Os, Q) :-
	matrix_to_query_1(M, N, Os, Q).

matrix_to_query_1(_-[],_,_,[]).
matrix_to_query_1(T-[C|Cs], NQCPs, Os, 
	          [qu(Q,D,D3,T3,q(Ps),UBT,Inf1,InfN)|Q1]) :-
	run_module(Os, RUNCM),
	abstrterm(T,C,T1),	
	cm_add_abstrterm(Os,T1,[],T2),
	cm_query_literals(C,B),
	% just a body (no head)
	RUNCM:dec_kb_predicates(FNs),
	make_empty_ancestor_table(FNs,Os,A),
	make_empty_ancestor_table(FNs,Os,A),
	make_empty_lemma_table(FNs,Os,Lemmas),
	make_empty_hconstraints(HC),
	
	( ( cm_option(hb, Os)
          ; cm_option(hb1, Os)
          ; cm_option(hw, Os)
          ; cm_option(hc_list, Os)
          ) ->
	  ( B = [QLit] ->
	    constr_i(Os,QLitI,[d(D-D3),a(A),l(Lemmas),t(T2-T3),p(QLitProof),
	                       inf(Inf1-InfN), hc(HC-_)]),
	    Ps = [QLitProof],
	    functor(QLit, _, QLitIIndex),
	    arg(QLitIIndex, QLit, QLitI)
	  ; err('Options given as input to cm are incompatible with non-unit clauses in queries')
          )
        ; cp_set_body_is(scp(_,_,B,
	                  [d1(D),dn(D3),t1(T2),tn(T3),ps(Ps),
                           a(A),l(Lemmas), 
			   inf_1(Inf1), inf_n(InfN),
			   runsolve(NQCPs),
			   use_bound_table(UBT)]),
			 Os)
        ),
	cm_list_to_andseq(B,Q),
	matrix_to_query_1(T-Cs,NQCPs,Os,Q1).


%%% 
%%% the same as cm_body_literals except rename_source_lit_pos instead of 
%%% rename_source_lit_neg 
%%% 

cm_query_literals([],[]).
cm_query_literals([L|Ls],[L1|Ls1]) :-
	rename_source_lit_pos(L,L2),
	add_argument(L2,_,L1),
	cm_query_literals(Ls,Ls1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% RUNTIME UTILITIES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% describe_i
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

describe_i :-
	describe_i(runcm).

describe_i(Module) :-
	Module:dec_cm_options(Os),
	describe_i_for_options(Os).

describe_i_for_options(Os) :-
	constr_i(Os,I, [d(depth1-depth2),
	                a(ancestors),
			l(lemmas),
			t(absterm1-absterm2),
			p(proof),
			inf(inferences1-inferences2),
			hc(hc_list1-hc_list2),
			use_bound_table(use_bound_table),
			runsolve(runsolve)]),
	write('\n --- Information attached to predicates'),
        write(' for theorem proving ---'),
	write('\nOptions: '), write(Os),
        write('\nI-Structure: '), write(I), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

depth_bound_reached :-
	get_bound_increment(REQ),
	REQ > 0.	         
 
run_module(Os, Module) :-
	( memberchk(run_module(Module), Os) ->
	  true
	; Module = runcm
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clause_options_is_theorem(Options) :-
	( memberchk(theorem, Options) ->
	  true
	; memberchk(role=negated_conjecture, Options) ->
	  true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% New Stuff 2015
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% REDUCTION r7
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red7s_from_cps(FNs,CPs1,Factor,M,Os) :-
	map_red7_cp(FNs,Factor,M,FNs,Os,CPs1).

map_red7_cp([],_,_,_,_,[]).
map_red7_cp([F-N|FNs],Factor,M,AllFNs,Os,[CP|CPs]) :-
	red7_cp(F,N,CP,Factor,M,AllFNs,Os),
	map_red7_cp(FNs,Factor,M,AllFNs,Os,CPs).

red7_cp(F,N,scp(H,B,[],[comment('%% Argument size checking [r7].')]),
        Factor,_M,_FNs,Os) :-
	n_var_list1(N, Xs, I),
	append(Xs1, [_], Xs),
	Term =.. [t|Xs1],
	H =.. [F|Xs],
	constr_i(Os, I, [d(D-_)]),
	B = [run_term_limit_reached(Term, Factor, D), !, fail].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The following criteria are used negatively:
%% - number of vars not appearing in head or earlier subgoal
%% - number of variables not in head
%% - same predicate as head 
%% - number of other subgoals with same predicate
%% - number of variables

sg_sort_subgoals(CPs, Mode, Quick, CPs1) :-
	info_progress_start,
	length(CPs, LenCPs),
	info(20, 'Greedy ~w subgoal sorting of ~w contrapositives',
	     [Quick, LenCPs]),
	sg_cps_pred_occurrence_table(CPs, POCCs),
	sg_sort_subgoals_1(CPs, POCCs, Mode, Quick, CPs1),
	info_progress_done.

sg_cp_pred(scp(H,_,_,_), F/N) :-
	functor(H, F, N).

sg_cps_pred_occurrence_table(CPs, POCCs) :-
	findall(P, (member(C,CPs), sg_cp_pred(C,P)), Ps),
	msort(Ps, Ps1),
	( Ps1 = [P|Ps2] ->
	  map_occs(Ps2, P, 1, POCCs)
	; POCCs = []
	).

map_occs([X|Xs], Y, N, Xs1) :-
	X == Y,
	!,
	N1 is N+1,
	map_occs(Xs, Y, N1, Xs1).
map_occs([X|Xs], Y, N, [Y-N|Xs1]) :-
	map_occs(Xs, X, 1, Xs1).
map_occs([], X, N, [X-N]).

sg_sort_subgoals_1([CP1|CPs1], POCCs, Mode, Quick, CPs2) :-
	info_progress(20, 100),
	sg_sort_cp_subgoals(CP1, POCCs, Mode, Quick, CP3),
	CPs2 = [CP3|CPs4],
	sg_sort_subgoals_1(CPs1, POCCs, Mode, Quick, CPs4).
sg_sort_subgoals_1([], _, _, _, []).


%%% no need to sort clauses of length <= 1
%%%
sg_sort_cp_subgoals(scp(H,P,[],N), _, _, _, scp(H,P,[],N)) :- !.
sg_sort_cp_subgoals(scp(H,P,[G],N), _, _, _, scp(H,P,[G],N)) :- !. 
sg_sort_cp_subgoals(scp(H,P,G,N), _, _, _, scp(H,P,G,N)) :-
	getarg_chk(clause_options(COs), N),
	memberchk(no_sort, COs),
	!.
%%%
%%% heuristic sorting
%%%
sg_sort_cp_subgoals(scp(H,P,[G|Gs],N), POCCs, Mode, Quick, scp(H,P,Gs100,N)) :-
	getarg(orig_head(H0), N),
	rem_argument(H0, H1),
	top_level_vars(H1, VsHeadTopLevel),
	ord_term_variables(H1, VsHead),
	map_hg_subgoal_vars([G|Gs], Gs1),
	functor(H1, HF, HN),
	map_hg_subgoal_predicate([G|Gs], GPs),
	Args = sgt(VsHead, VsHeadTopLevel, HF, HN, GPs, POCCs, Mode),
	( Quick = quick ->
	  sort_hq_1(Gs1, VsHead, Args, Gs2)
	; sort_hg_1(Gs1, VsHead, Args, Gs2)
	),
	map_hg_subgoal(Gs2, Gs100).

top_level_vars(T, Vs) :-
	T =.. [_|Args],
	tlvs1(Args, Vs1),
	sort(Vs1, Vs).

tlvs1([X|Xs], [X|Xs1]) :-
	var(X),
	!,
	tlvs1(Xs, Xs1).
tlvs1([_|Xs], Xs1) :-
	tlvs1(Xs, Xs1).
tlvs1([], []).

ord_term_variables(T, Vs) :-
	term_variables(T, Vs1),
	sort(Vs1, Vs).

map_hg_subgoal([g(G,_)|Xs], [G|Xs1]) :-
	map_hg_subgoal(Xs, Xs1).
map_hg_subgoal([], []).

map_hg_subgoal_vars([],[]).
map_hg_subgoal_vars([G|Gs],[g(G,Vars)|VGs]) :-
	rem_argument(G,G1),
	ord_term_variables(G1,Vars),
	map_hg_subgoal_vars(Gs,VGs).

map_hg_subgoal_predicate([X|Xs], [F/N|Xs1]) :-
	functor(X, F, N1),
	N is N1-1,
	map_hg_subgoal_predicate(Xs, Xs1).
map_hg_subgoal_predicate([], []).

sort_hg_1([], _, _, []) :-
	!.
sort_hg_1(Gs, LVs, Args, [G1|Gs1]) :-
	map_add_hg_value(Gs, LVs, Args, Gs2),
	keysort(Gs2, Gs3),
%	nl, pp(keysort(LVs, Gs3)), nl,
	map_val(Gs3, [G1|Gs4]),
	G1 = g(_, Vs),
	ord_union(Vs, LVs, LVs1),
	sort_hg_1(Gs4, LVs1, Args, Gs1).

sort_hq_1([], _, _, []) :-
	!.
sort_hq_1(Gs, LVs, Args, [G1|Gs1]) :-
	map_add_hg_value(Gs, LVs, Args, Gs2),
	keysort(Gs2, Gs3),
	map_val(Gs3, [G1|Gs4]),
	Gs1 = Gs4.

add_hg_value(g(G,V), _, _, Val-g(G,V)) :-
	subgoal_valspec_built_in(G),
	!,
	builtin_hg_value(G, Val).
add_hg_value(g(G,Vs), LeftVs, Args, Val-g(G,Vs)) :-
	Args = sgt(HeadVs, TLHeadVs, HeadF, HeadN, SubgoalPreds, POCCs, Mode),

	( Mode = 0 ->
	  Val = v(IsPropositional,NoNewVs,Connects,LenCoveredVs,LenNewVs,LenNonTLHeadVs,LenNonheadVs,Loop,LenVars,Sibl,GN)
	%%	  Val = v(IsPropositional,NoNewVs,Connects,LenCoveredVs,LenNonTLHeadVs,LenNewVs,LenNonheadVs,Loop,LenVars,Sibl,GN)
	; Mode = 1 ->
	  Val = v(IsPropositional,Connects,NoNewVs,LenCoveredVs,LenNonTLHeadVs,LenNonheadVs,LenNewVs,Loop,LenVars,Sibl,GN)
	; Mode = 2 ->
	  Val = v(IsPropositional,Connects,NoNewVs,LenCoveredVs,LenNonTLHeadVs,LenNonheadVs)
	; Mode = 3 ->
	  Val = v(IsPropositional,NoNewVs,Connects,LenCoveredVs,LenNewVs,LenNonTLHeadVs)
	; Mode = 4 ->
	  Val = v(IsPropositional,NoNewVs,Connects,LenCoveredVs,LenNonTLHeadVs,LenNonheadVs)
	; Mode = exp ->
	  Val = v(IsPropositional,NoNewVs,Connects,LenCoveredVs,LenNonTLHeadVs,LenNonheadVs)
	; Val = v(IsPropositional,Connects,NoNewVs,LenCoveredVs,LenNonTLHeadVs,LenNonheadVs)
	),

	ord_subtract(LeftVs, Vs, CoveredVs),
	ord_subtract(Vs, LeftVs, NewVs),
	ord_subtract(Vs, HeadVs, NonheadVs),
	ord_subtract(Vs, TLHeadVs, NonTLHeadVs),
	( NewVs = [] ->
	  NoNewVs = 0
	; NoNewVs = 1
	),
	length(CoveredVs, LenCoveredVs),
	length(NewVs, LenNewVs),
	length(NonheadVs, LenNonheadVs),
	length(NonTLHeadVs, LenNonTLHeadVs),
	length(Vs, LenVars),
	functor(G, GF, GN),
	( GN =< 1 ->
	  IsPropositional = 0
	; IsPropositional = 1
	),
	( memberchk(GF/GN-Connects, POCCs) -> true ; Connects = 0 ),
	( GF=HeadF, GN=HeadN -> Loop = 1 ; Loop = 0 ),
	findall(k, member(GF/GN, SubgoalPreds), Ks),
	length(Ks, Sibl).

map_add_hg_value([X|Xs], Y1, Y2, [X1|Xs1]) :-
	add_hg_value(X, Y1, Y2, X1),
	map_add_hg_value(Xs, Y1, Y2, Xs1).
map_add_hg_value([], _, _, []).


hg_pre_1_value(1).
hg_pre_2_value(2).
hg_post_value(v(k,k,k,k,k,k,k,k,k,k,k,k,k,k,k)).

builtin_hg_value('c_not_$eq'(_, _), V) :-  !, hg_pre_1_value(V).
builtin_hg_value('c_$prolog'(_, _), V) :- !, hg_pre_2_value(V).
builtin_hg_value('c_$prolog'(_, _, _), V) :- !, hg_pre_2_value(V).
builtin_hg_value(G, V) :- is_post_subgoal(G), !, hg_post_value(V).
builtin_hg_value(check_hconstraints_post(_), V) :- !, hg_post_value(V).
builtin_hg_value(_, V) :- hg_post_value(V).

is_post_subgoal('c_$prolog_post'(_, _)) :- !.
is_post_subgoal('c_$prolog_post'(_, _, _)) :- !.
is_post_subgoal('c_$constrain'(_, _, _)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_subs_binding_rights_unit(Cs, Max, Bss) :-
	length(Cs, LenCs),
	info(20, 'Determining unit subsumption bindings for ~w clauses', [LenCs]),
	info_progress_start,
	msb_unit([], Cs, Max, Bss),
	info_progress_done.

msb_unit(_, [], _, []).
msb_unit(Cs1, [C|Cs], Max, [Bs|Bss]) :-
	info_progress(20, 10),
	copy_term(C, CC),
	( Max = -1 ->
	  findall(B, msb_unit_1(Cs1, Cs, CC, B), Bs0)
	; flag(sol_count, _, 0),
	  findall(B, ( msb_unit_1(Cs1, Cs, CC, B),
		       flag_inc(sol_count, Count),
		       ( Count >= Max -> ! ; true ) ),
		  Bs0)
	),
        sort(Bs0,Bs),
	msb_unit([C|Cs1], Cs, Max, Bss).

msb_unit_1(Cs1, Cs, CC, B) :-
	( member(C0,Cs1),
	  C0 = [_],
	  clause_subsumes(C0,CC,B)	  
	; member(C0,Cs),
	  C0 = [_],
	  clause_proper_subsumes(C0,CC,B)
	).

max_n_first(0, _, []) :-
	!.
max_n_first(_, [], []) :-
	!.
max_n_first(N, [X|Xs], [X|Xs1]) :-
	N1 is N-1,
	max_n_first(N1, Xs, Xs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% LEMMA SOLUTIONS
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemsols_from_cps(FNs,CPs1,M,Os) :-
	map_lemsol_cp(FNs,M,FNs,Os,CPs1).

map_lemsol_cp([],_,_,_,[]).
map_lemsol_cp([F-N|FNs],M,AllFNs,Os,[CP|CPs]) :-
	lemsol_cp(F,N,CP,M,AllFNs,Os),
	map_lemsol_cp(FNs,M,AllFNs,Os,CPs).

lemsol_cp(F,N,scp(H,B,[],[comment('%% Lemma Solutions.')]),_M,FNs,Os) :-
	n_var_list1(N,Xs,I),
	constr_i(Os,I,[d(D-D),l(Lemmas),fns(FNs),t(T-T),p(P),
	               inf(Inf-Inf), hc(Hc-Hc)]),
	H =.. [F|Xs],
	ct_lemma_lookup_code(H, Lemmas, P, FNs, Os, CODE),
	B = CODE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_empty_lemma_table(_, _, []).

ct_add_lemma(Ls, L, _, _, _, Ls) :-
	is_built_in(L),
	!.
ct_add_lemma(Ls, L, P, Os, _FNs, [L1|Ls]) :-
	cm_lemma_literal(L, P, Os, L1).

is_built_in(L) :-
	built_in_fns(BUILT_INS),
	functor(L,Functor,Arity),
	memberchk(Functor/Arity, BUILT_INS).

cm_lemma_literal(L, P, Os, L1) :-
	rem_argument(L, L2),
	( cm_option(p, Os) ->
	  L1 = lp(L2, P)
	; L1 = L2
	).

ct_lemma_lookup_code(L, Lemmas, P, _FNs, Os, Code) :-
	rem_argument(L, L1),
	( cm_option(p, Os) ->
	  Code = [ member(lp(L2, P1), Lemmas),
		   ( L2 == L1 -> !
		   ; unify_with_occurs_check(L1, L2)
		   ),
		   P = P1 | Rest ]
	; Code = [ member(L2, Lemmas),
		   ( L2 == L1 -> !
		   ; unify_with_occurs_check(L1, L2)
		   ) | Rest]
	),
	( cm_option(xlc, Os) ->
	  Rest = [!]
	; Rest = []
	).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% R8: "Local Solution Subsumption Wrapper"
%%%% 
%%%% r8(al) - consider also bindings for ancestors and lemmas
%%%% r8(a)  - consider also bindings for ancestors
%%%% r8(l)  - consider also bindings for lemmas
%%%% r8(v)  - consider only bindings for arguments
%%%% 
%%%% - r8(a), r8(l), r8(v) are incomplete if ancestors or lemmas are used
%%%% - r8(al) can be used without overhead also with options that do
%%%%   not introduce ancestors or lemmas
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lss_cps([], _M, _Os, []).
lss_cps([FN|FNs], M, Os, [CP|CPs]) :-
	lss_cp(FN, M, Os, CP),
	lss_cps(FNs, M, Os, CPs).

lss_cp(F-N, Mode, Os,
       scp(Call,B,[],
	   [comment('%% Local Solution Subsumption Wrapper [r8].')])) :-
	n_var_list1(N, XIs, I),
	Call =.. [F|XIs],
	butlast(XIs, Xs),
	constr_i(Os, I, [a(A), l(L)]),
	( has_ancestor_argument(Os), (Mode=a ; Mode=al) ->
	  ( has_lemma_argument(Os), Mode=al ->
	    T =.. [t,A,L|Xs]
	  ; T =.. [t,A|Xs]
	  )
	; has_lemma_argument(Os), (Mode=al ; Mode=l) ->
	  T =.. [t,L|Xs]
	; T =.. [t|Xs]
	),
	lit_add_prefix(Call, lss_, LssCall),
	B = [ run_call_suppressing_duplicates(LssCall, T) ].
	  

has_ancestor_argument(Os) :-
	constr_i(Os, I, [a(A)]),
	contains_var(A, I).

has_lemma_argument(Os) :-
	constr_i(Os, I, [l(A)]),
	contains_var(A, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Background: set_prolog_flag(unknown, false) seems to effect
%%%% nontermination in recent SWI-Prologs (7.6.4).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cps_disable_unknowns(CPs, FNs, CPs1, Ps1) :-
	findall(P-A, (member(CP, CPs), CP = scp(H,_,_,_), functor(H, P, A)),
		Ps),
	sort(Ps, Ps1),
	map_disable_unknowns(CPs, Ps1, FNs, CPs2),
	ensure_query_cp(CPs2, Ps1, FNs, CPs1).

map_disable_unknowns([X|Xs], Y1, Y2, [X1|Xs1]) :-
	disable_unknowns(X, Y1, X1),
	map_disable_unknowns(Xs, Y1, Y2, Xs1).
map_disable_unknowns([], _, _, []).

ensure_query_cp(Xs, YNew, YOld, Xs1) :-
	memberchk('c_$query'-N, YOld),
	\+ memberchk('c_$query'-N, YNew),
	!,
	functor(Q, 'c_$query', N),
	SCP = scp(Q,[fail],[],
		  [comment('%% Added this disabled clause to have $query defined.')]),
	Xs1 = [SCP|Xs].
ensure_query_cp(Xs, _, _, Xs).

disable_unknowns(scp(H,P,B,V), Ps, SCP1) :-
	cp_calls_undefined(scp(H,P,B,V), Ps, Reason),
	!,
	( select(comment(Comment), V, V1) ->
	  format(atom(Comment1),
		 '~w (disabled because undefined ~w)',
		 [Comment, Reason])
	; format(atom(Comment1),
		 '%% Disabled because undefined: ~w.', [Reason])
	),
	SCP1 = scp(H, [fail], [], [comment(Comment1)|V1]).
disable_unknowns(X, _, X).

cp_calls_undefined(scp(_, _, B, _), Ps, Reason) :-
	member(L, B),
	functor(L, P, A),
	sub_atom(P, 0, _, _, c_),
	( sub_atom(P, 0, _, _, 'c_$query') -> true
	; sub_atom(P, 0, _, _, 'c_$') -> false
	; sub_atom(P, 0, _, _, 'c_not_$') -> false
	; true
	),
	\+ memberchk(P-A, Ps),
	!,
	Reason = P/A.
