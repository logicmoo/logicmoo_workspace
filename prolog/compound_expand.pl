:- module(compound_expand, []).

/* This module allows to define compositional term and goal expansions,
   using this module in a module that already defines the predicates
   term_expansion/2/4 and goal_expansion/2/4 but don't export them.
   
   The composition of expansions is instrumental to grammar and
   syntax extensions. We do not need to deal with all the complexity
   that the Ciao package system have, so with this helper the port of
   Ciao Packages to SWI Prolog can be achieved smoothly and such
   modules can be used in SWI Programs that do not requires the Ciao
   dialect.
*/

:- use_module(library(expansion_module)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4.

:- public implemented_pi/1.
:- meta_predicate implemented_pi(:).
implemented_pi(M:F/A) :-
    functor(H, F, A),
    once(predicate_property(M:H, visible)),
    \+ predicate_property(M:H, imported_from(_)).

system:goal_expansion(Goal0, Pos0, Goal, Pos) :-
    '$set_source_module'(M, M),
    expansion_module(M, EM),
    ( implemented_pi(EM:goal_expansion/4) ->
      EM:goal_expansion(Goal0, Pos0, Goal, Pos)
    ; EM:goal_expansion(Goal0, Goal),
      Pos = Pos0
    ),
    Goal0 \== Goal,
    !.

system:term_expansion(Term0, Pos0, Term, Pos) :-
    '$set_source_module'(M, M),
    findall(EM-PI, ( expansion_module(M, EM),
		     ( implemented_pi(EM:term_expansion/4)
		     ->PI=[term_expansion/4]
		     ; PI=[term_expansion/2]
		     )), ML),
    ML \= [],
    '$expand':call_term_expansion(ML, Term0, Pos0, Term, Pos),
    Term0 \== Term,
    [Term0] \== Term.		% Fail to try other expansions
