:- module(compound_expand, []).

% Kludge: using swipl internals. Perhaps is not a good idea --EMM
expansion_module(M, EM) :-
    module_property(expansion_module, file(CF)),
    '$load_context_module'(CF, EM, _),
    module_property(EM, file(EF)),
    '$load_context_module'(EF, M, _).

goal_expansion(Goal0, Goal) :-
    '$set_source_module'(M, M),
    expansion_module(M, EM),
    EM:goal_expansion(Goal0, Goal),
    !.

term_expansion(Term0, Term) :-
    '$set_source_module'(M, M),
    findall(EM, expansion_module(M, EM), ML),
    ML \= [],
    '$expand':call_term_expansion(ML, Term0, Term).
