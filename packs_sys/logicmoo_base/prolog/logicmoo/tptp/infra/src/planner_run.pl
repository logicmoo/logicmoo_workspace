/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Planner Runtime
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(planner_run, [ plan/4,
			 plan/5,
			 pool_literal/3,
			 clean_module/1,
			 describe_i/0,
			 describe_i/1
		       ]).
			
:- use_module(planner_cm).
:- use_module('swilib/err').

%% Just to ensure that it is loaded at runtime:
:- use_module(planner_cs_simple). 

%%%% 
%%%% plan(+Options, +Goals, +Starts, +Constraints, +Rules)
%%%% 
%%%% Call the planner on the task specified by Goals, Starts, Constraints
%%%% and Rules.
%%%% 
%%%% Results are returned via variables in Options.
%%%%
plan(Os, G, S, R) :-
	plan(Os, G, S, [], R).
plan(Os, G, S, C, R) :-
	( memberchk(module(Module), Os) -> true ; Module = out_planner ),
	( memberchk(out(Out), Os) ->
	  ( var(Out) -> Mode = term ; Mode = file )
	; Out = '/tmp/out_planner.pl', Mode = file
	),
	( memberchk(run, Os) ->
	  ( Mode = file ->
	    load_file(Out)
	  ; err('Only file mode is supported for for run option.')
	  )
	; compile_rules(Os, R),
	  clean_module(Module),
	  ( Mode = term ->
	    compile_term(Out, Module)
	  ; Mode = file ->
	    load_file(Out)
	  )
	),
	qu1(Os, G, S, C, Module).

qu1(Options, Goal, Start, Constraints, Module) :-
	Module:dec_fluent_heads(FluentHeads),
	( memberchk(dj(DJ), Options) -> true ; DJ = d ),
	statistics(cputime, CPU),
	gen_dj(DJ, Depth, Infs),
	%% query_clause is called after gen_dj, so it can be
	%% nondeterministic. It can also fail, if e.g. constraints
	%% are contradictory.
	maxdj(MaxDJ),
	( Depth == MaxDJ ->
	  msg('--- Planner run: Infs: ~t~d~16|', [Infs])
	; Infs == MaxDJ ->
	  msg('--- Planner run: Depth: ~t~d~16|', [Depth])
	; msg('--- Planner run: Depth: ~t~d~16| Infs: ~t~d~28|', [Depth, Infs])
	),
	query_clause(Goal, Start, Constraints, Options, FluentHeads,
		     Depth, Infs, Query),
	qu2(Module:Query, CPU).
	
qu2(Query, CPU) :-
	statistics(cputime, CPU1),
	( call(Query),
	  statistics(cputime, CPU2),
	  TDepth is CPU2 - CPU1,
	  TTotal is CPU2 - CPU,  
	  msg('    Planner run: Success after ~t~3f~26| sec. (~t~3f~41| sec. total)',
	      [TDepth, TTotal])  
        ; statistics(cputime, CPU2),
	  TDepth is CPU2 - CPU1,
	  TTotal is CPU2 - CPU,	    
	  msg('    Planner run: Leaving after ~t~3f~26| sec. (~t~3f~41| sec. total)',
	      [TDepth, TTotal]),
          fail
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Pool Access
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% pool_literal(+Module, +Pool, ?Literal)
%%%% 
%%%% Used to access results returned in the poole(Poole) option. Enumerates
%%%% literals in the poole. The compiled rules must still be installed.
%%%% 
pool_literal(Module, Pool, Literal) :-
	member(Entry, Pool),
	Module:dec_poolfact_literal(Entry, Literal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Countup
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_dj(d, N, M) :- maxdj(M), countup(0, N).
gen_dj(j, M, N) :- maxdj(M), countup(0, N).
gen_dj(d(B), N, M) :- maxdj(M), countup(B, N).
gen_dj(j(B), M, N) :- maxdj(M), countup(B, N).
gen_dj(dkj(K), K, J) :- countup(0, J).
gen_dj(djk(K), D, K) :- countup(0, D).
gen_dj(dkjk(D,J), D, J).

maxdj(N) :-
	prolog_flag(max_tagged_integer, N).  %% ?

countup(N, N).
countup(N, N1) :-
	N2 is 1 + N,
	countup(N2, N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary Predicates
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_term(Clauses, Module) :-
	findall(P/N, ( member(Clause, Clauses),
	               ( Clause = (H :- _) ->
			 true
		       ; Clause = (:- _) ->
		         fail
		       ; Clause = H
		       ),
		       functor(H, P, N)
		     ),
		PNs),
	sort(PNs, PNs1),
	( member(P1/N1, PNs1),
	  functor(Template, P1, N1),
	  Module:retractall(Template),
	  fail
	; true
	),
	( member(Clause, Clauses),
	  ( Clause = (:- Declaration) ->
	    Module:call(Declaration)
	  ; Module:assert(Clause)
	  ),
	  fail
	; true
	).

%%%% 
%%%% clean_module(+Module)
%%%% 
%%%% Abolish predicates of given module. Unfortunately it seems not
%%%% possible in SWI Prolog to completely remove a module.
%%%% 
clean_module(Module) :-
	( Module:current_predicate(P/A),
	  functor(F,P,A),
	  \+(Module:predicate_property(F, imported_from(_))),
	  Module:abolish(P/A),
	  fail
        ; true
	).

load_file(File) :-
	style_check(-singleton),
	catch( consult(File), E, ( style_check(+singleton), throw(E) ) ),
	style_check(+singleton).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% describe_i(+Module)
%%%% 
%%%% Print the layout of the extra information attached to compiled
%%%% predicates in symbolic form. For debugging purposes.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

describe_i :-
	describe_i(out_planner).

describe_i(Module) :-
	Module:dec_plan_options(Os),
	describe_i_structure(Os).



