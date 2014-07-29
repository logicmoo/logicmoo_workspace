:- module(implemented_in, [implemented_in/1, implemented_in/3]).

:- use_module(library(prolog_codewalk), []). % for message_location//1
:- use_module(library(normalize_head)).
:- use_module(library(record_locations)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(implemented_in(From, Args))) -->
    prolog:message_location(From),
    ['Implements ~w'-Args].

implemented_in(MGoal0, From, Args) :-
    normalize_head(MGoal0, MGoal),
    M:Goal = MGoal,
    functor(Goal, F, A),
    findall(MI, ( current_module(M),
		  \+ predicate_property(MGoal, imported_from(_)),
		  MI = M
		; predicate_property(MGoal, imported_from(MI))
		), UML),
    sort(UML, ML),
    member(M, ML),
    ( declaration_location(Goal, M, Declaration, From),
      Declaration \= dynamic(query, _),
      Args = [Declaration]
    ;
      Counter=c(1),
      From = clause(ClauseRef),
      catch(clause(M:Goal, _, ClauseRef), _, fail),
      arg(1, Counter, N),
      Args = [M:F/A/N],
      N1 is N + 1,
      nb_setarg(1, Counter, N1)
    ).

implemented_in(Goal) :-
    implemented_in(Goal, From, Args),
    print_message(information, acheck(implemented_in(From, Args))),
    fail.
implemented_in(_).
