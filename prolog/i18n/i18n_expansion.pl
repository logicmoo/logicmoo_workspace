:- module(i18n_expansion, []).

:- reexport(library(i18n/i18n_op)).
:- use_module(library(compound_expand)).
:- use_module(library(i18n/i18n_support)).

translate_args(Meta, M, Goal0, Goal) :-
    functor(Meta, F, A),
    functor(Goal, F, A),
    translate_arg(1, Meta, M, Goal0, Goal).

translate_arg(N, Meta, M, Goal0, Goal) :-
    arg(N, Meta, Spec),
    !,
    arg(N, Goal0, Term0),
    arg(N, Goal,  Term),
    translate_non_goal_term(Spec, M, Term0, Term),
    succ(N, N1),
    translate_arg(N1, Meta, M, Goal0, Goal).
translate_arg(_, _, _, _, _).

translate_non_goal_term(0, _, Term, Term) :- !.
translate_non_goal_term(^, _, Term, Term) :- !.
translate_non_goal_term(S, M, Term0, Term) :-
    translate_term(S, M, Term0, Term).

translate_term(S, M, Term0, Term) :-
    '$expand':meta_arg(S),
    !,
    translate_term_meta(M, Term0, Term).
translate_term(_, M, Term0, Term) :-
    translate_term(M, Term0, Term).

translate_term_meta(_, Term0, M:Term) :-
    nonvar(Term0),
    Term0 = M:Term1,
    !,
    translate_term_meta(M, Term1, Term).
translate_term_meta(M, Term0, Term) :-
    translate_term(M, Term0, Term).

gtabling(Elem, Goal) :-
    ( \+ Elem->
      ( Goal,
	compile_aux_clauses(Elem),
	fail
      ; true
      )
    ; true
    ).

tabulate_i18n_records(M) :-
    gtabling(i18n_support:i18n_record(M, L, I, S),
	     current_i18n_record(M, L, I, S)).

translate_term(Term0, Term) :-
    '$set_source_module'(M, M),
    translate_term(M, Term0, Term),
    Term0 \== Term.

translate_term(M, Term0, Term) :-
    tabulate_i18n_records(M),
    expand_i18n_term(i18n_entry_expander, M, Term0, Term).


goal_expansion(V:Goal0, M, V:Goal) :-
    var(M),
    !,
    translate_term(M, Goal0, Goal).
goal_expansion(M:Goal0, _, M:Goal) :- !,
    goal_expansion(Goal0, M, Goal).
goal_expansion(Goal0, M, Goal) :-
    ( predicate_property(M:Goal0, meta_predicate(Meta))
    ->translate_args(Meta, M, Goal0, Goal)
    ; translate_term(M, Goal0, Goal)
    ),
    !,
    Goal0 \== Goal.

goal_expansion(Goal0, Goal) :-
    callable(Goal0),
    '$set_source_module'(M, M),
    goal_expansion(Goal0, M, Goal).
goal_expansion(A=~B0, A=B) :-
    nonvar(B0),
    !,
    translate_term(~B0, B).
goal_expansion(A=~~B0, A=B) :-
    nonvar(B0),
    !,
    translate_term(~~B0, B).
goal_expansion(A = IB, A=~B) :-
    nonvar(IB),
    IB = ~(B),
    !.
goal_expansion(A = IB, G) :- % A bit complex due to static optimizations:
    nonvar(IB),
    IB = ~~(B),
    ( language(en),
      dictionary(Dict),
      en \= Dict ->
      G = (A~=B)
    ; language(Lang),
      dictionary(Lang),
      \+ ((dictionary(Dict2), Dict2 \= Lang)) ->
      G = (A = B)
    ; language(en),
      \+ dictionary(_) ->
      G = (A = B)
    ; G = (A=~~B)
    ).

term_expansion((:- i18n_resource(PoAlias)),
	       [i18n_support:i18n_resource(M, PoAlias)]) :- !,
    '$set_source_module'(M, M).
term_expansion((:- resourceterm(Term)),
	       [i18n_support:i18n_resourceterm(M, Term)]) :- !,
    '$set_source_module'(M, M).
term_expansion((:- init_i18n),
	       []) :- !,
    '$set_source_module'(M, M),
    tabulate_i18n_records(M).
term_expansion((:- M:init_i18n), [])  :- !, tabulate_i18n_records(M).
term_expansion((:- init_i18n(M)), []) :- !, tabulate_i18n_records(M).
term_expansion((:- _), _) :- !, fail. % Skip declarations
term_expansion((Term0 :- Body), (Term :- Body)) :- !,
    translate_term(Term0, Term).
term_expansion((Term0 --> Body), (Term --> Body)) :- !,
    translate_term(Term0, Term).
term_expansion(Term0, Term) :- !,
    translate_term(Term0, Term).
