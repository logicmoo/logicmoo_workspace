:- module(database_fact,
	  [database_fact/1,
	   database_fact/3,
	   database_fact_ort/3,
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
	database_def_fact/2,
	database_retract_fact/2,
	database_query_fact/2.

database_fact(G) :- database_fact(G, _).
database_fact(G) :- normalize_pi(G, PI), database_var_fact(PI).

database_mod_fact(G, F) :- database_def_fact(G, F).
database_mod_fact(G, F) :- database_retract_fact(G, F).

database_use_fact(G, F) :- database_query_fact(G, F).
database_use_fact(G, F) :- database_retract_fact(G, F).

clause_head(A,          A) :- var(A), !.
clause_head(M:A,        M:A) :- var(A), !.
clause_head((A :- _),   A) :- !.
clause_head(M:(A :- _), M:A) :- !.
clause_head(A,          A).

database_fact(def, Goal, Fact) :- database_def_fact(Goal, Fact).
database_fact(use, Goal, Fact) :- database_use_fact(Goal, Fact).
database_fact(mod, Goal, Fact) :- database_mod_fact(Goal, Fact).

% ortogonal operations:
database_fact_ort(def,     G, F) :- database_def_fact(G, F).
database_fact_ort(retract, G, F) :- database_retract_fact(G, F).
database_fact_ort(query,   G, F) :- database_query_fact(G, F).

database_fact(G, F) :- database_fact_ort(_, G, F).

database_def_fact(ifprolog:asserta_with_names(A, _), F) :- clause_head(A, F).
database_def_fact(ifprolog:assertz_with_names(A, _), F) :- clause_head(A, F).
database_def_fact(pce_config:lasserta(A),            F) :- clause_head(A, F).
database_def_fact(pce_config:lasserta(A),            F) :- clause_head(A, F).
database_def_fact(plunit:assert_cyclic(A),           F) :- clause_head(A, F).
database_def_fact(system:abolish(F, A),              H) :- fa_to_head(F, A, H).
database_def_fact(system:abolish(PI),                H) :- pi_to_head(PI, H).
database_def_fact(system:assert(A),                  F) :- clause_head(A, F).
database_def_fact(system:assert(A, _),               F) :- clause_head(A, F).
database_def_fact(system:asserta(A),                 F) :- clause_head(A, F).
database_def_fact(system:asserta(A, _),              F) :- clause_head(A, F).
database_def_fact(system:assertz(A),                 F) :- clause_head(A, F).
database_def_fact(system:assertz(A, _),              F) :- clause_head(A, F).
database_def_fact(system:retractall(F),              F).

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
database_var_fact(shlib:unload_foreign/1).
database_var_fact(user:prolog_clause_name/2).
database_var_fact(user:prolog_list_goal/1).
database_var_fact(prolog_trace_utils:list_clauses/3).
database_var_fact(prolog_term_view:print_clause_properties/2).

database_retract_fact(system:retract(A),      F) :- clause_head(A, F).
database_retract_fact(pce_config:lretract(A), F) :- clause_head(A, F).

database_query_fact(system:clause(A, _),         F) :- clause_head(A, F).
database_query_fact(system:clause(A, _, _),      F) :- clause_head(A, F).
database_query_fact(refactor:unfold_goal(_,A,_), F) :- clause_head(A, F).

pi_to_head(PI, H) :- nonvar(PI) -> PI=F/A, fa_to_head(F, A, H).

fa_to_head(M:F, A, M:H) :- !, (atomic(M) -> fa_to_head_(F, A, H) ; true).
fa_to_head(F,   A, H) :- fa_to_head_(F, A, H).

fa_to_head_(F, A, H) :- atomic(F), integer(A) -> functor(H, F, A) ; true.
