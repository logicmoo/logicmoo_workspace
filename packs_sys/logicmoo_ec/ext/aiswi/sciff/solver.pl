:- module(solver,
    [is_constraint_functor/1,
    restriction_entailed/2,
    fd_or_num/1,
    reified_equality_solver/3,
    binary_domain/1,
    cstr_var/1,
    neq/2,lt/2,eq/2,gt/2,leq/2,geq/2,
    is_identical/2,
    impose_neg_constraints/3,
    solver_search/1,
    is_clp_functor/1,
    solver_rewrite_constraint/2,
    term_unify/2,
    opposite/2,
    rewrite_restriction/2,
    rewrite_restr_rules/2,
    add_default_domain/1,
    term_equality/2,
    is_number/1
    ]).

%:- ensure_loaded(fd_solver), use_module(library(clpfd)).
:- ensure_loaded(r_solver), use_module(library(clpr)). 
