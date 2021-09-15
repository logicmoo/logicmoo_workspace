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
%%%% Reading/Writing TPTP Syntax
%%%% 
%%%% example call: tptp_read('MSC/MSC006-1', X)
%%%%
%%%% Tests whether given file (absolute or relative pathname) exists -
%%%% otherwise uses tptp_directory and extension ".p" to determine
%%%% a tptp file name.
%%%%
%%%% Reads in the file (TPTP CNF format) and includes specified in the
%%%% file. Returns the corresponding matrix with extra information
%%%% in ~'$options'(ListOfOptions) literals.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(tptpio, 
	  [tptp_read/2,
	   tptp_to_nf/2,
	   nf_to_tptp/2,
	   pp_tptp/1,
	   writeq_tptp/1,
	   set_tptp_directory/1,
	   tptp_directory/1,
	   tptp_problem/5,
	   validate_tptp_problem/1,
	   tptp_problem_format/2]).

:- use_module(swilib(info)).
:- use_module(swilib(err)).
:- use_module(swilib(options)).
:- use_module(nf(nfutils)).
:- use_module(logop_fol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Operator Section from tptp2x.main
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%------------------------------------------------------------------------------
%----These are used in the TPTP and need to exist before the 
%----transformations and formats are loaded. They are also declared at 
%----runtime in tptp2X/4.
:-op(70,fx,'$$').
:-op(80,fx,'$').
:-op(90,xfx,/).     %----Rationals need to bind tighter than =
:-op(100,fx,++).
:-op(100,fx,--).
%----Postfix for !=
:-op(100,xf,'!').
%---- .. used for range in tptp2X. Needs to be stronger than :
:-op(400,xfx,'..').
%----! and ? are of higher precedence than : so ![X]:p(X) is :(![X],p(X))
%----Otherwise ![X]:![Y]:p(X,Y) cannot be parsed.
%----! is fy so Prolog can read !! (blah) as !(!(blah)) and it gets fixed
:-op(400,fy,!).
:-op(400,fx,?).
:-op(400,fx,^).
:-op(400,fx,'!>').
:-op(400,fx,'?*').
:-op(400,fx,'@-').
:-op(400,fx,'@+').
%----= must bind more tightly than : for ! [X] : a = X. = must binder looser
%----than quantifiers for otherwise X = ! [Y] : ... is a syntax error (the =
%----grabs the quantifier). That means for thf it is necessary to bracket 
%----formula terms, e.g., a = (! [X] : p(X))
:-op(405,xfx,'=').
%---!= not possible because ! is special - see postfix cheat :-op(405,xfx,'!=').
:-op(440,xfy,>).     %----Type arrow
:-op(450,xfx,'<<').     %----Subtype arrow
%----Need : stronger than binary connectives for ! [X] : p(X) <=> !Y ...
%----Need ~ and : equal and right-assoc for ![X] : ~p and for ~![X] : ...
:-op(450,xfy,:).
:-op(450,fx,:=).
:-op(450,fx,'!!').
:-op(450,fx,'??').
:-op(450,fy,~).
:-op(480,yfx,*).     %----X product
:-op(480,yfx,+).     %----Union
:-op(501,yfx,@).
%%%%
%%%% We use for the remaining priorities >= 1001 because of the
%%%% restriction by SWI prolog for bar (|). The system_mode seems
%%%% no help there in version 7.2.3
%%%%
:-op(1002,xfy,'|').
:-op(1002,xfx,'~|').
:-op(1003,xfy,&).
:-op(1003,xfx,~&).
:-op(1004,xfx,=>).
:-op(1004,xfx,<=).
:-op(1005,xfx,<=>).
:-op(1005,xfx,<~>).
:-op(1010,xfx,-->).
:-op(1050,xfx,:=).

% % :-op(502,xfy,'|').
% % FOR SWI 5.10.2
% %%%% seems to have no effect:
% :-(system_mode(true),op(502,xfy,'|'),system_mode(false)).
% :-op(502,xfx,'~|').
% :-op(503,xfy,&).
% :-op(503,xfx,~&).
% :-op(504,xfx,=>).
% :-op(504,xfx,<=).
% :-op(505,xfx,<=>).
% :-op(505,xfx,<~>).
% :-op(510,xfx,-->).
% %----Must be weak to allow any formulae on RHS
% :-op(550,xfx,:=).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TPTP Extensions for Second-Order Quantification
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

Note: the "!" is handled specially in the ISO-Prolog syntax, differently from
"?". Thus it seems not easily possible to use combinations of "!" and "?" with
some other token in a symmetric way.

*/

:-op(400,fy,'ex2').
:-op(400,fy,'all2').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic tptp_directory/1.

set_tptp_directory(X) :-
	info(10, 'Setting TPTP directory to: ~w', [X]),
	retractall(tptp_directory(_)),
	assert(tptp_directory(X)).

:- ( getenv('TPTPDirectory', TPTPDir) ->
     true
   ; TPTPDir = '/usr/local/tptp'
   ),
   set_tptp_directory(TPTPDir).

tptp_extension(problem, '.p').
tptp_extension(axioms, '.ax').

tptp_subdir(problem, 'Problems').
tptp_subdir(axioms, 'Axioms').

tptp_file_spec(problem(Spec), Spec, problem) :- !.
tptp_file_spec(axioms(Spec), Spec, axioms) :- !.
tptp_file_spec(Spec, Spec, problem).

tptp_read(FileSpec, Items) :-
	tptp_file_spec(FileSpec, Spec, Kind),
	tptp_read(Spec, Kind, Items).

tptp_read(FileSpec, Kind, Items) :-
	load_problem(FileSpec, Kind),
	make_result(Items),
	cleanup_tptp_predicates.

%%%% 
%%%% Global declarations for YAP, harmless for SWI
%%%% 
:- dynamic include/1.
:- dynamic cnf/3.
:- dynamic fof/3.
:- multifile include/1.
:- multifile cnf/3.
:- multifile fof/3.

cleanup_tptp_predicates :-
	prolog_flag(dialect, yap),
	!,
	retractall(include(_)),
	retractall(cnf(_,_,_)),
	retractall(fof(_,_,_)).
cleanup_tptp_predicates :-
	abolish(include/1),
	abolish(cnf/3),
	abolish(fof/3).

load_problem(FileSpec, Kind) :-
	cleanup_tptp_predicates,
	( prolog_flag(dialect, yap) -> true
	; multifile(include/1),
	  multifile(cnf/3),
	  multifile(fof/3)
	),
	( exists_file(FileSpec) ->
	  AbsFile = FileSpec
        ; tptp_directory(Dir),
	  tptp_extension(Kind, Ext),
	  ( Kind = problem ->
	    ( sub_atom(FileSpec, _, _, _, '/') ->
	      FileSpec1 = FileSpec
	    ; sub_atom(FileSpec, 0, 3, _, Pre),
	      concat_atom([Pre, '/', FileSpec], FileSpec1)
	    )
	  ; FileSpec1 = FileSpec
	  ),
	  tptp_subdir(Kind, SubDir),
	  concat_atom([Dir, '/', SubDir, '/', FileSpec1, Ext], AbsFile)
        ),
	info(10, "Loading tptp file: ~w", [AbsFile]),
	style_check(-singleton),
	catch( ( load_files(AbsFile, []),
		 style_check(+singleton) ),
	       Err,
	       ( style_check(+singleton),
		 throw(Err) )),
	( predicate_property(include(_), _) ->
	  ( include(Include),
	    ( special_include(Include) ->
	      true
	    ; concat_atom([Dir, '/', Include], AbsInclude),
	      info(10, "For ~w: loading included: ~w",
		   [FileSpec, AbsInclude]),
	      style_check(-singleton),
	      catch( ( load_files(AbsInclude, []),
		       style_check(+singleton) ),
		     Err,
		     ( style_check(+singleton),
		       throw(Err) ))
	    ),
	    fail
	  ; true
	  )
        ; true
        ).

special_include(_) :- fail.

% special_include('Axioms/EQU001-0.ax') :-
% 	writeln(using_special('Axioms/EQU001-0.ax')),
% 	%% more efficient? equality axiomatization
% 	compile_term(
% 	  [input_clause(reflexivity,axiom,
% 	                [++equal(X,X)]),
%            input_clause(symtrans,axiom,
% 	                [--equal(X,Y), --equal(X,Z), ++equal(Y,Z)])]).

make_result(Items) :-
	findall( Info-Clause,
		 ( cnf(Name, Role, C),
	           tptp_map_trafo_literal(C, Clause),
		   Info = [stmt=cnf, name=Name, role=Role]
	         ),
		 Items1 ),
	findall( Info1-Form,
		 ( fof(Name1, Role1, F),
	           tptp_to_nf(F, Form),
		   Info1 = [stmt=fof, name=Name1, role=Role1]
	         ),
		 Items2 ),
	append(Items1, Items2, Items).
	     
tptp_map_trafo_literal(true,[]).
tptp_map_trafo_literal((X|Y),[X1|Y1]) :-
	!,
	tptp_trafo_literal(X,X1),
	tptp_map_trafo_literal(Y,Y1).
tptp_map_trafo_literal(X, [X1]) :-
	tptp_trafo_literal(X,X1).

tptp_trafo_literal(L, ~(X=Y)) :-
	subsumes_chk((_ != _), L),
	!,
	L = (X != Y).
tptp_trafo_literal(L, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TPTP Syntax:
%%%% 
%%%% The universal quantifier is !, the existential quantifier is ?, and the
%%%% lambda binder is ^. Quantified formulae are written in the form
%%%% Quantifier [Variables] : Formula.
%%%% 
%%%% The binary connectives are infix | for disjunction, infix & for
%%%% conjunction, infix <=> for equivalence, infix => for implication, infix
%%%% <= for reverse implication, infix <~> for non-equivalence (XOR), infix ~|
%%%% for negated disjunction (NOR), infix ~& for negated conjunction (NAND),
%%%% infix @ for application. The only unary connective is prefix ~ for
%%%% negation.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptp_to_nf(F, F1) :-
	prolog_vars_to_atoms(F, F2),
	tptp_to_nf_1(F2, F1).

tptp_to_nf_1(F1, F2) :-
	syn_map(F1, F2, SF),
	!,
	map_tptp_to_nf_1(SF).
tptp_to_nf_1(F, F).

map_tptp_to_nf_1([]).
map_tptp_to_nf_1([F-F1|Fs]) :-
	tptp_to_nf_1(F, F1),
	map_tptp_to_nf_1(Fs).

nf_to_tptp(F, F1) :-
	fix_special_function_symbols_for_tptp(F, F0),
	vars_to_prolog(F0, F2),
	fix_so_quantifiers_from_vars_to_prolog(F2, F3),
	nf_to_tptp_1(F3, F1).

fix_special_function_symbols_for_tptp(F, F1) :-
	f_signature(F, _, Funs),
	%% Symbols not allowed as function symbols in the TPTP but possibly
	%% used by PIE users:
	Forbidden = [('=>'),('<='),('<=>'),('+'),('-')],
	mk_replacement_map(Forbidden, 0, Funs, Map),
	%% All occurrences of those symbols are replaced by f,f1,f2 etc.
	replace_symbols(F, Map, F1).

mk_replacement_map([], _, _, []).
mk_replacement_map([F|Fs], N, Gs, Map) :-
	memberchk(F/_, Gs),
	!,
	( N = 0 -> New = f ; atom_concat(f, N, New) ),
	N1 is N+1,
	( memberchk(New/_, Gs) ->
	  mk_replacement_map([F|Fs], N1, Gs, Map)
	; Map = [F-New|Map1],
	  mk_replacement_map(Fs, N1, Gs, Map1)
	).
mk_replacement_map([_|Fs], N, Gs, Map) :-
	mk_replacement_map(Fs, N, Gs, Map).

replace_symbols(T, Map, T1) :-
	atomic(T),
	!,
	( memberchk(T-T1, Map) -> true ; T1 = T ).
replace_symbols(T, Map, T1) :-
	compound(T),
	!,
	T =.. [F|Ts],
	( memberchk(F-F1, Map) -> true ; F1 = F ),
	map_replace_symbols(Ts, Map, Ts1),
	T1 =.. [F1|Ts1].
replace_symbols(T, _, T).

map_replace_symbols([X|Xs], Y1, [X1|Xs1]) :-
	replace_symbols(X, Y1, X1),
	map_replace_symbols(Xs, Y1, Xs1).
map_replace_symbols([], _, []).

nf_to_tptp_1(F1, F2) :-
	syn_map(F2, F1, SF),
	!,
	map_nf_to_tptp_1(SF).
nf_to_tptp_1(F, F).

map_nf_to_tptp_1([]).
map_nf_to_tptp_1([F-F1|Fs]) :-
	nf_to_tptp_1(F1, F),
	map_nf_to_tptp_1(Fs).

syn_map((! [V] : F), all(V, F1), [F-F1]) :- \+ subsumes_chk([_|_], V).
syn_map((? [V] : F), ex(V, F1), [F-F1]) :- \+ subsumes_chk([_|_], V).
syn_map((! Vs : F), all(Vs, F1), [F-F1]).
syn_map((? Vs : F), ex(Vs, F1), [F-F1]).

syn_map((all2 [V] : F), all2(V, F1), [F-F1]) :- \+ subsumes_chk([_|_], V).
syn_map((ex2 [V] : F), ex2(V, F1), [F-F1]) :- \+ subsumes_chk([_|_], V).
syn_map((all2 Vs : F), all2(Vs, F1), [F-F1]).
syn_map((ex2 Vs : F), ex2(Vs, F1), [F-F1]).

syn_map(~(F), ~(F1), [F-F1]).
syn_map((F | G), (F1 ; G1), [F-F1, G-G1]).
syn_map((F & G), (F1 , G1), [F-F1, G-G1]).
syn_map((F => G), (F1 -> G1), [F-F1, G-G1]).
syn_map((F <= G), (F1 <- G1), [F-F1, G-G1]).
syn_map((F <=> G), (F1 <-> G1), [F-F1, G-G1]).
syn_map((F <~> G), ~((F1 <-> G1)), [F-F1, G-G1]).
syn_map((F ~& G), ~((F1 , G1)), [F-F1, G-G1]).
syn_map((F '~|' G), ~((F1 ; G1)), [F-F1, G-G1]).
syn_map((X != Y), ~(X = Y), []).

syn_map($false, false, []).
syn_map($true, true, []).

syn_map(false, '$predicate_false', []).
syn_map(true, '$predicate_true', []).
syn_map(all(X,Y), '$predicate_all'(X,Y), []).
syn_map(ex(X,Y), '$predicate_ex'(X,Y), []).
syn_map(all2(X,Y), '$predicate_all2'(X,Y), []).
syn_map(ex2(X,Y), '$predicate_ex2'(X,Y), []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% pp with TPTP operator settings
%%%% 
pp_tptp(X) :-
	pp(X).

writeq_tptp(X) :-
	\+ \+ ( numbervars(X, 0, _), 
		write_term(X, [module(tptpio),
			       quoted(true),
			       character_escapes(true),
			       numbervars(true)
			      ])
	      ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% nf:vars_to_prolog (as of Jan 2016) inserts terms for the second-order
%%%% variable specifiers, we just want symbols here
%%%%
fix_so_quantifiers_from_vars_to_prolog(F, F1) :-
	logform_process_subforms(F, fso, F1).

fso(ex2(Xs, F), ex2(Xs1, F)) :-	!, map_fso_1(Xs, Xs1).
fso(all2(Xs, F), all2(Xs1, F)) :- !, map_fso_1(Xs, Xs1).
fso(X, X).

map_fso_1([X|Xs], [X1|Xs1]) :-
	fso_1(X, X1),
	map_fso_1(Xs, Xs1).
map_fso_1([], []).

fso_1(pred(_,P,_), P) :- !.
fso_1(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
works also for axioms, e.g.
  tptp_problem(axioms('GRP005-0'), [], F, T, A)
  T is then empty or true, respectively
*/  

tptp_problem(FileSpec, Options, Format, MTheorem, MAxioms) :-
	info(10, 'Reading TPTP file'),
	tptp_file_spec(FileSpec, Spec, Kind),
 	tptp_read(Spec, Kind, X),
	tptp_problem_format(X, Format),
	( from_options(validate=true, Options, true) ->
	  info(20, 'Validating read TPTP statements'),
	  validate_tptp_problem(X, Kind),
	  info(20, 'Validated read TPTP statements')
	; true
	),
	( Format = cnf ->
	  split_tptp_cnf_problem(X, MTheorem, MAxioms)
	; Format = fof ->
	  split_tptp_fol_problem(X, Conjectures, Axioms),
	  list_to_conjunction_1(Conjectures, MTheorem),
	  list_to_conjunction_1(Axioms, MAxioms)
	; err('Unsupported TPTP format: ~w', [Format])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_tptp_fol_problem([I-F|IFs], [F|Fs], Gs) :-
	memberchk(role=conjecture, I),
	!,
	split_tptp_fol_problem(IFs, Fs, Gs).
split_tptp_fol_problem([_-F|IFs], Fs, [F|Gs]) :-
	split_tptp_fol_problem(IFs, Fs, Gs).
split_tptp_fol_problem([], [], []).
	     
split_tptp_cnf_problem([I-F|IFs], [F|Fs], Gs) :-
	memberchk(role=negated_conjecture, I),
	!,
	split_tptp_cnf_problem(IFs, Fs, Gs).
split_tptp_cnf_problem([_-F|IFs], Fs, [F|Gs]) :-
	split_tptp_cnf_problem(IFs, Fs, Gs).
split_tptp_cnf_problem([], [], []).

tptp_problem_format(X, Format) :-
	( X = [Info-_|_] ->
	  ( memberchk(stmt=fof, Info) ->
	    Format=fof
	  ; memberchk(stmt=cnf, Info) ->
	    Format=cnf
	  ; memberchk(stmt=Format1, Info) ->
	    Format=Format1
	  ; Format=unspecified_format
	  )
	; Format=unspecified_format
	).

validate_tptp_problem(X) :-
	validate_tptp_problem(X, problem).
validate_tptp_problem(X, Kind) :-
	tptp_problem_format(X, Format),
        ( Format = fof ->
	  ( member(Info1-_, X),
	    \+ memberchk(stmt=fof, Info1) ->
	    err('TPTP statements with incoherent formats: ~w and ~w',
		[Format, Info1])
	  ; true
	  ),
	  findall(k, ( member(Info1-_, X),
		       member(role=conjecture, Info1) ),
		  Conjectures),
	  ( member(Info1-_, X), member(role=Role, Info1),
	    \+ memberchk(Role, [conjecture, axiom, hypothesis, lemma,
				definition]) ->
	    err('TPTP role not supported for FOF: ~w', [Role])
	  ; true
	  ),
	  ( Kind = problem, Conjectures == [] ->
	    %% these would not be handled correctly so far
	    err('FOF TPTP problem without conjecture')
	  ; Kind = axioms, Conjectures \== [] ->
	    err('FOF TPTP axioms with conjecture')
	  ; true
	  )
	; Format=cnf ->
	  ( member(Info1-_, X),
	    \+ memberchk(stmt=cnf, Info1) ->
	    err('TPTP statements with incoherent formats: ~w and ~w',
		[Format, Info1])
	  ; true
	  )
	; err('Unsupported TPTP format: ~w', [Format])
	).

list_to_conjunction_1([F], F) :-
	!.
list_to_conjunction_1([F|G], (F, G1)) :-
	list_to_conjunction_1(G, G1).
list_to_conjunction_1([], true).
