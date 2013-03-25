:- module(compound_expand, []).

/* This module allows to define compositional term and goal expansions,
   using this module in a module that already defines the predicates
   term_expansion/2 and goal_expansion/2 but don't export them.
   
   The composition of expansions is instrumental to grammar and
   syntacx extensions. We do not need to deal with all the complexity
   that the Ciao package system have, so with this helper the port of
   Ciao Packages to SWI Prolog can be achieved smoothly and such
   modules can be used in SWI Programs that do not requires the Ciao
   dialect.
*/

:- multifile
    system:term_expansion/2,
    system:goal_expansion/2.

:- meta_predicate no_duplicates(0, ?).
no_duplicates(Goal, Vars) :-
    S = nd([]),
    Goal,
    arg(1, S, X),
    ( memberchk(Vars, X) -> fail
    ; nb_setarg(1, S, [Vars|X])
    ).

% Kludge: using swipl internals. Perhaps is not a good idea --EMM
expansion_module(M, EM) :-
    context_module(CM),
    module_property(CM, file(CF)),
    no_duplicates('$load_context_module'(EF, M, _), [EF, M]),
    module_property(EM, file(EF)),
    no_duplicates('$load_context_module'(CF, EM, _), [CF, EM]).

system:goal_expansion(Goal0, Goal) :-
    '$set_source_module'(M, M),
    expansion_module(M, EM),
    EM:goal_expansion(Goal0, Goal),
    !.

system:term_expansion(Term0, Term) :-
    '$set_source_module'(M, M),
    findall(EM, expansion_module(M, EM), ML),
    ML \= [],
    '$expand':call_term_expansion(ML, Term0, Term).
