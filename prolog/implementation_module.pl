:- module(implementation_module, [implementation_module/2]).

:- meta_predicate implementation_module(?,?).
% BUG: At compile time, if there exist other predicate with the same name in the
% libraries, and it is called before such predicate be defined, it can report
% the wrong implementation module pointing at the libraries. Work around: Never
% use names that are already being used in the libraries. --EMM
implementation_module(M:Goal, IM) :-
    ( atom(M),
      callable(Goal),
      predicate_property(M:Goal, imported_from(IM0))
    ->IM = IM0 %% Allow usage as test
    ; IM = M   %% Asume that if not imported, it is defined here
    ).
