
:- ensure_loaded(dmiles).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- ensure_loaded_if_exists(library(prompt)).

:- ensure_loaded(home(argument_types)).
:- ensure_loaded(home(bu_basics)).
:- ensure_loaded(home(clause_heads)).
:- ensure_loaded(home(div_utils)).
:- ensure_loaded(home(environment)).
:- ensure_loaded(home(evaluation)).
:- ensure_loaded(home(filter)).
:- ensure_loaded(home(flatten)).
:- ensure_loaded(home(g1_ops)).
:- ensure_loaded(home(g2_ops)).
:- ensure_loaded(home(interpreter)).
:- ensure_loaded(home(kb)).
:- ensure_loaded(home(lgg)).
:- ensure_loaded(home(show_utils)).
:- ensure_loaded(home(td_basic)).
:- ensure_loaded(home(tdref_it)).
:- ensure_loaded(home(var_utils)).
:- ensure_loaded(home(newpred)).
:- ensure_loaded(home(gencon)).
:- ensure_loaded(home('gencon_instances/rul.pl')).
:- ensure_loaded(home('gencon_instances/constrained_clauses.pl')).
:- ensure_loaded(home('gencon_instances/foil.pl')).

:- retractall(interpreter:depth_bound(_)), 
   assert(interpreter:depth_bound(10)).
