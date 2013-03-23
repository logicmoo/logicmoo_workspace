:- module(compound_expand, []).

/* This module allows to define compositional term and goal expansions,
   using this module in a module that already defines the predicates
   term_expansion/2 and goal_expansion/2.
   
   The composition of expansions is instrumental to grammar and
   syntactic extensions. We do not need to deal with all the
   complexity that the Ciao package system have, so with this helper
   the poft of Ciao packages to SWI prolog can be achieved smoothly.
*/

:- multifile
    system:term_expansion/2,
    system:goal_expansion/2.

% Kludge: using swipl internals. Perhaps is not a good idea --EMM
expansion_module(M, EM) :-
    context_module(CM),
    module_property(CM, file(CF)),
    '$load_context_module'(CF, EM, _),
    module_property(EM, file(EF)),
    '$load_context_module'(EF, M, _).

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
