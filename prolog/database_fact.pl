/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(database_fact,
	  [database_fact/1,
	   database_fact/3,
	   database_fact_ort/4,
	   database_def_fact/2,
	   database_mod_fact/2,
	   database_use_fact/2,
	   clause_head/2,
	   fa_to_head/3
	  ]).

:- use_module(library(normalize_pi)).
:- use_module(library(static_strip_module)).

:- create_prolog_flag(check_database_preds, false, [type(boolean)]).

% help analyzers to track indirect calls via prolog database manipulation:
%

prolog:called_by(H, IM, CM, [F]) :-
    current_prolog_flag(check_database_preds, true),
    database_use_fact(IM:H, F),
    static_strip_module(F, C, M, CM),
    callable(C),
    nonvar(M).

:- multifile
	database_var_fact/1,
	database_def_fact/3,
	database_retract_fact/3,
	database_query_fact/3.

database_fact(G) :- database_fact(G, _).
database_fact(G) :- normalize_pi(G, PI), database_var_fact(PI).

database_mod_fact(M:G, F) :- database_def_fact(    G, M, F).
database_mod_fact(M:G, F) :- database_retract_fact(G, M, F).

database_use_fact(M:G, F) :- database_query_fact(  G, M, F).
database_use_fact(M:G, F) :- database_retract_fact(G, M, F).

clause_head(A,          A) :- var(A), !.
clause_head(M:A,        M:A) :- var(A), !.
clause_head((A :- _),   A) :- !.
clause_head(M:(A :- _), M:A) :- !.
clause_head(A,          A).

database_fact(def, Goal, Fact) :- database_def_fact(Goal, Fact).
database_fact(use, Goal, Fact) :- database_use_fact(Goal, Fact).
database_fact(mod, Goal, Fact) :- database_mod_fact(Goal, Fact).

% ortogonal operations:
database_fact_ort(def,     G, M, F) :- database_def_fact(G, M, F).
database_fact_ort(retract, G, M, F) :- database_retract_fact(G, M, F).
database_fact_ort(query,   G, M, F) :- database_query_fact(G, M, F).

database_fact(M:G, F) :- database_fact_ort(_, G, M, F).

database_def_fact(M:H, F) :- database_def_fact(H, M, F).

database_def_fact(asserta_with_names(A, _), ifprolog,   F) :- clause_head(A, F).
database_def_fact(assertz_with_names(A, _), ifprolog,   F) :- clause_head(A, F).
database_def_fact(lasserta(A),              pce_config, F) :- clause_head(A, F).
database_def_fact(assert_cyclic(A),         plunit,     F) :- clause_head(A, F).
database_def_fact(abolish(F, A),            system,     H) :- fa_to_head(F, A, H).
database_def_fact(abolish(PI),              system,     H) :- pi_to_head(PI, H).
database_def_fact(assert(A),                system,     F) :- clause_head(A, F).
database_def_fact(assert(A, _),             system,     F) :- clause_head(A, F).
database_def_fact(asserta(A),               system,     F) :- clause_head(A, F).
database_def_fact(asserta(A, _),            system,     F) :- clause_head(A, F).
database_def_fact(assertz(A),               system,     F) :- clause_head(A, F).
database_def_fact(assertz(A, _),            system,     F) :- clause_head(A, F).
database_def_fact(retractall(F),            system,     F).
database_def_fact(update_fact_from(A),      from_utils, F) :- clause_head(A, F).

database_var_fact(check:check_trivial_fail/3).
database_var_fact(prolog_codewalk:walk_clauses/2).
database_var_fact(check_abstract_domains:abstract_execute_goal/9).
database_var_fact(check_non_mutually_exclusive:mutually_exclusive/3).
database_var_fact(check_non_mutually_exclusive:collect_non_mutually_exclusive/1).
database_var_fact(check_trivial_fails:check_trivial_fail/3).
database_var_fact(ifprolog:clause_with_names/3).
database_var_fact(ifprolog:retract_with_names/2).
database_var_fact(implemented_in:implemented_in/1).
database_var_fact(predicate_options:assert_option_clause/2).
database_var_fact(man_data:fix_source_path/2).
database_var_fact(pce_prolog_tracer:show_source/2).
database_var_fact(prolog_codewalk:clause_not_from_development/4).
database_var_fact(prolog_codewalk:walk_called_by_pred/2).
database_var_fact(prolog_listing:list_clauserefs/1).
database_var_fact(prolog_listing:list_clauses/2).
database_var_fact(prolog_metainference:meta_pred_args_in_clause/3).
database_var_fact(shlib:abolish_foreign/1).
database_var_fact(shlib:unload_foreign/1).
database_var_fact(user:prolog_clause_name/2).
database_var_fact(user:prolog_list_goal/1).
database_var_fact(prolog_trace_utils:list_clauses/3).
database_var_fact(prolog_term_view:print_clause_properties/2).
database_var_fact(from_utils:update_fact_from/1).

database_retract_fact(retract(A),  system,     F) :- clause_head(A, F).
database_retract_fact(lretract(A), pce_config, F) :- clause_head(A, F).

database_query_fact(clause(A, _),       system,   F) :- clause_head(A, F).
database_query_fact(clause(A, _, _),    system,   F) :- clause_head(A, F).
database_query_fact(unfold_goal(_,A,_), refactor, F) :- clause_head(A, F).

pi_to_head(PI, H) :- nonvar(PI) -> PI=F/A, fa_to_head(F, A, H) ; true.

fa_to_head(M:F, A, M:H) :- atomic(M) -> fa_to_head_(F, A, H), !.
fa_to_head(F,   A, H) :- fa_to_head_(F, A, H).

fa_to_head_(F, A, H) :- atomic(F), integer(A) -> functor(H, F, A) ; true.
