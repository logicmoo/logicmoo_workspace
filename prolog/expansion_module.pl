:- module(expansion_module, [expansion_module/2]).

:- meta_predicate no_duplicates(0, ?).
no_duplicates(Goal, Term) :-
    setup_call_cleanup(S = nd([]),
		       Goal,
		       ( arg(1, S, X),
			 ( memberchk(Term, X) -> fail
			 ; nb_setarg(1, S, [Term|X])
			 )
		       )).

% Kludge: using swipl internals. Perhaps is not a good idea --EMM
expansion_module(M, EM) :-
    CM = compound_expand,
    module_property(CM, file(CF)),
    no_duplicates('$load_context_module'(CF, EM, _), CF-EM),
    module_property(EM, file(EF)),
    no_duplicates('$load_context_module'(EF, M, _), EF-M).

