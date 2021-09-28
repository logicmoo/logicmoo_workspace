:- module(pure,
          [is_pure_pred/1,
           is_pure_clause/1
          ]).

:- use_module(library(lists)).
:- use_module(library(resolve_calln)).

:- meta_predicate is_pure_pred(0 ).

%!  is_pure_pred(:Pred) is semidet
%
%   Succeeds if the predicate is pure-prolog, that is, it does not have
%   side-effects.  We check that by ensuring that the predicate is implemented
%   only using prolog clauses or other pure-prolog predicates.  Fails otherwise
%   or if it is unable to determine it.

is_pure_pred(Head) :-
    is_pure_pred(Head, []).

%!  is_pure_clause(Ref) is semidet
%
%   Succeeds if the clause referenced by Ref has been implemented using only
%   pure-prolog predicates.

is_pure_clause(Ref) :-
    is_pure_clause(Ref, []).

is_pure_pred(Head, Stack1) :-
    predicate_property(Head, interpreted),
    findall(Ref, ( catch(clause(Head, _, Ref), _, fail),
                   \+ memberchk(Ref, Stack1)
                 ), RefL),
    append(RefL, Stack1, Stack),
    forall(member(Ref, RefL),
           is_pure_clause(Ref, Stack)).

is_pure_clause(Ref, Stack) :-
    clause_property(Ref, module(CM)),
    catch(clause(_, Body, Ref), _, fail),
    is_pure_body(Body, CM, Stack).

% Note: ->/2, \+/1, *->/2, !, are not consider pure prolog, you can prove that by
% implementing var/1 using those, since var/1 is not pure prolog. For instance:
%
% var(A) :- \+ (A = a), \+ (A = b).

% In general, any predicate that check the instantiation status of a variable is
% not pure-prolog

is_pure_body(G, _, _) :-
    var(G),
    !,
    freeze(G, is_pure_pred(G)).
is_pure_body(true, _, _) :- !.
is_pure_body(fail, _, _) :- !.
is_pure_body(@(G, CM), _, Stack) :-
    !,
    strip_module(CM:G, M, H),
    is_pure_body_mod(H, M, CM, Stack).
is_pure_body(M:G, _, Stack) :-
    !,
    is_pure_body_mod(G, M, M, Stack).
is_pure_body((A,B), M, Stack) :-
    !,
    is_pure_body(A, M, Stack),
    is_pure_body(B, M, Stack).
is_pure_body((A;B), M, Stack) :-
    !,
    is_pure_body(A, M, Stack),
    is_pure_body(B, M, Stack).
is_pure_body(CallN, M, Stack) :-
    do_resolve_calln(CallN, Call),
    is_pure_body(Call, M, Stack).
is_pure_body(Goal, M, Stack) :-
    is_pure_pred(M:Goal, Stack).

/*
is_pure_body_mod(G, M, CM, Stack) :-
    ( atom(M),
      atom(CM)
    ->setup_call_cleanup(
          ( '$current_source_module'(OldM),
            '$set_source_module'(CM)
          ),
          is_pure_body(G, M, Stack),
          '$set_source_module'(OldM))
    ; true
    ).
*/

is_pure_body_mod(H, M, _, Stack) :-
    is_pure_body(H, M, Stack).
