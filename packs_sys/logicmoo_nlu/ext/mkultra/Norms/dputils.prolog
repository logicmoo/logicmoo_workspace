% From the book
% PROLOG PROGRAMMING IN DEPTH
% by Michael A. Covington, Donald Nute, and Andre Vellino
% (Prentice Hall, 1997).
% Copyright 1997 Prentice-Hall, Inc.
% For educational use only
%
% Last modified 8/24/2006


% DPUTILS.PL
%
% Utilities for d-Prolog.
%

:- op(1100,fx,@@).

:- dynamic have_seen_predicate_before/2.
:- dynamic dPrologDictionary/2.
:- public dlisting/1.
:- public (@@)/1.

%
% @@(+Goal)
%   Tests to see if Goal is strictly or defeasibly
%   derivable from the d-Prolog knowledge base. Goal
%   may not contain an uninstantiated variable.
%

@@ Goal :-
   \+ ground(Goal),
        write('Improper argument for @@.'), nl,
        write('Argument contains an uninstantiated variable.'), 
        nl,
        !,
        fail.

@@ Goal :-
        strict_der(root,Goal),
        contrary(Goal,Contrary),
        Contrary,
        !,
        write('definitely, yes -'), nl,
        write('and definitely, no - contradictory.'), nl.

@@ Goal :-
        strict_der(root,Goal),
        !,
        write('definitely, yes.'), nl.

@@ Goal :-
        contrary(Goal,Contrary),
        Contrary,
        !,
        write('definitely, no.'), nl.

@@ Goal :-
        def_der(root,Goal),
        !,
        write('presumably, yes.'), nl.

@@ Goal :-
        contrary(Goal,Contrary),
        def_der(root,Contrary),
        !,
        write('presumably, no.'), nl.

@@ _ :-
        write('can draw no conclusion.'), nl.


%
% dlisting(+Predicate)
%   Lists all ordinary Prolog clauses, defeasible rules, and
%   defeaters in memory which have heads of the form
%   Predicate(...Arguments...) or of the form 
%   ~ Predicate(...Arguments...).
%

dlisting(Predicate) :-
        listing(Predicate),
        fail.

dlisting(Predicate) :-
        clause(~ Head,Body),
        functor(Head,Predicate,_),
        pprint(~ Head,' :-',Body),
        fail.

dlisting(Predicate) :-
        (Head := Body),
        dfunctor(Head,Predicate,_),
        pprint(Head,' :=',Body),
        fail.

dlisting(Predicate) :-
        (Head :^ Body),
        dfunctor(Head,Predicate,_),
        pprint(Head,' :^',Body),
        fail.

dlisting(Predicate) :-
        clause(incompatible(Clause1,Clause2),Body),
        dfunctor(Clause1,Predicate,_),
        pprint(incompatible(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(incompatible(Clause1,Clause2),Body),
        dfunctor(Clause2,Predicate,_),
        pprint(incompatible(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(sup(Clause1,Clause2),Body),
        rule_functor(Clause1,Predicate,_),
        pprint(sup(Clause1,Clause2),' :-',Body),
        fail.

dlisting(Predicate) :-
        clause(sup(Clause1,Clause2),Body),
        rule_functor(Clause2,Predicate,_),
        pprint(sup(Clause1,Clause2),' :-',Body),
        fail.

dlisting(_).

%
% dlist
%   dlists all predicates in the d-Prolog dictionary.
%

dlist :-
        dPrologDictionary(Predicate,_),
        dlisting(Predicate),
        fail.

dlist.

%
% dfunctor(+Clause,-Predicate,-Arity)
%   Returns the d-Prolog Predicate of Clause
%   together with its Arity.
%

dfunctor(~ Clause,Predicate,Arity) :-
        functor(Clause,Predicate,Arity),
        !.

dfunctor(Clause,Predicate,Arity):-
        functor(Clause,Predicate,Arity).

%
% rule_functor(+Rule,-Predicate,-Arity)
%   Returns the d-Prolog Predicate of the head of
%   the Rule together with its Arity.
%

rule_functor((Head :- _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

rule_functor((Head := _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

rule_functor((Head :^ _),Predicate,Arity) :-
        dfunctor(Head,Predicate,Arity),
        !.

%
% pprint(+Head,+Operator,+Body)
%   A formatting routine for printing ordinary Prolog clauses,
%   defeasible rules, and defeaters.
%

pprint(Head,' :-',true) :-
        !,
        write(Head), write('.'), nl.

pprint(Head,Operator,Clause) :-
        write(Head), write(Operator), nl,
        pprint(Clause).

pprint((First,Rest)) :-
        !,
        write('    '), write(First), write(','), nl,
        pprint(Rest).

pprint(Clause) :-
        write('    '), write(Clause), write('.'), nl.

%
% rescind(+Predicate/+Arity)
%   Removes all ordinary Prolog clauses, defeasible rules,
%   or defeaters whose heads are of the form
%   Predicate(...Arguments...) or of the form
%   ~ Predicates(...Argument...).
%

rescind(Predicate/Arity) :-
        abolish(Predicate/Arity),
        functor(Clause,Predicate,Arity),
        retractall(Clause),
        retractall(~ Clause),
        retractall((Clause := _)),
        retractall((~ Clause := _)),
        retractall((Clause :^ _)),
        retractall((~ Clause :^ _)),
        retractall(incompatible(Clause,_)),
        retractall(incompatible(_,Clause)),
        retractall(incompatible(~ Clause,_)),
        retractall(incompatible(_,~ Clause)),
        retractall(sup((Clause := _),_)),
        retractall(sup(_,(Clause := _))),
        retractall(sup((~ Clause := _),_)),
        retractall(sup(_,(~ Clause := _))),
        retractall(sup((Clause :^ _),_)),
        retractall(sup(_,(Clause :^ _))),
        retractall(sup((~ Clause :^ _),_)),
        retractall(sup(_,(~ Clause :^ _))),
        retractall(dPrologDictionary(Predicate,Arity)).

%
% rescindall
%   Removes from memory the entire d-Prolog knowledge base,
%   provided it was loaded into memory using reload.
%

rescindall :-
        dPrologDictionary(Predicate,Arity),
        rescind(Predicate/Arity),
        fail.

rescindall.

%
% dload(+Filename)
%   Consults a file containing a d-Prolog knowledge base.
%

dload(Filename) :-
        concat([Filename,'.pl'],NewFilename),
        see(NewFilename),
        repeat,
        read(Term),
        execute_term(Term),
        remember_predicate(Term),
        add_to_memory(Term),
        Term == end_of_file,
        seen,
        !.

%
% reload(+Filename)
%   Reconsults a file containing a d-Prolog knowledge base.
%

reload(Filename) :-
        concat([Filename,'.dpl'],NewFilename),
        see(NewFilename),
        repeat,
        read(Term),
        execute_term(Term),
        rescind_previous_clauses(Term),
        add_to_memory(Term),
        Term == end_of_file,
        retractall(have_seen_predicate_before/2),
        seen,
        !.

%
% execute_term(+Term)
%   During a dload or a reload, executes any queries 
%   read from a d-Prolog knowledge base.
%

execute_term((:- Goal)) :-
        Goal,
        !,
        fail.

execute_term(_).

%
% add_to_memory(+Clause)
%   Adds clauses to memory during a reload.
%

add_to_memory((:- _)) :- !.

add_to_memory(end_of_file) :- !.

add_to_memory(Term) :-
        assertz(Term).

%
% remember_predicate(+Term)
%   Adds new predicates to the dPrologDictionary
%   during a dload.
%

remember_predicate((:- _)) :- !.

remember_predicate(end_of_term) :- !.

remember_predicate(Rule) :-
        rule_functor(Rule,Predicate,Arity),
        add_to_dictionary(Predicate,Arity),
        !.

remember_predicate(sup(Rule1,Rule2)) :-
        rule_functor(Rule1,Predicate1,Arity1),
        add_to_dictionary(Predicate1,Arity1),
        rule_functor(Rule2,Predicate2,Arity2),
        add_to_dictionary(Predicate2,Arity2),
        !.

remember_predicate(incompatible(Goal1,Goal2)) :-
        dfunctor(Goal1,Predicate1,Arity1),
        add_to_dictionary(Predicate1,Arity1),
        dfunctor(Goal2,Predicate2,Arity2),
        add_to_dictionary(Predicate2,Arity2),
        !.

remember_predicate(Goal) :-
        dfunctor(Goal,Predicate,Arity),
        add_to_dictionary(Predicate,Arity),
	  !.

%
% add_to_dictionary(+Predicate,+Arity)
%   Adds a clause to dPrologDictionary/2 for
%   Predicate and Arity if one is not already
%   present.
%

add_to_dictionary(Predicate,Arity) :-
        \+ dPrologDictionary(Predicate,Arity),
        functor(DummyGoal,Predicate,Arity),
        assert(DummyGoal),
        retract(DummyGoal),
        assert(dPrologDictionary(Predicate,Arity)).

add_to_dictionary(_,_).

%
% rescind_previous_clauses(+Term)
%   Removes from memory all ordinary Prolog clauses,
%   defeasible rules, or defeaters that were not loaded
%   during the current invocation of reload and that,
%   from the perspective of the d-Prolog inference engine,
%   are clauses for the same predicate as Term.
%

rescind_previous_clauses((:- _)) :- !. % Query to execute.

rescind_previous_clauses(end_of_file) :- !.

rescind_previous_clauses((Head :- _)) :-
        rescind_previous_clauses(Head),
        !.

rescind_previous_clauses((Head := _)) :-
        rescind_previous_clauses(Head),
        !.

rescind_previous_clauses((Head :^ _)) :-
        rescind_previous_clauses(Head),
        !.

rescind_previous_clauses(incompatible(X,Y)) :-
        rescind_previous_clauses(X),
        rescind_previous_clauses(Y),
        !.

rescind_previous_clauses(sup(Rule1,Rule2)) :-
        rescind_previous_clauses(Rule1),
        rescind_previous_clauses(Rule2),
        !.

rescind_previous_clauses(Clause) :-
        dfunctor(Clause,Predicate,Arity),
        \+ have_seen_predicate_before(Predicate,Arity),
        asserta(have_seen_predicate_before(Predicate,Arity)),
        rescind(Predicate/Arity),
        remember_predicate(Clause),
        !.

rescind_previous_clauses(_).

%
% dictionary
%   Prints a list of all predicates and their arities that 
%   occur in a d-Prolog knowledge base loaded into memory
%   using dload or reload.
%

dictionary :-
        dPrologDictionary(Predicate,Arity),
        write(Predicate),
        write('/'),
        write(Arity),
        nl,
        fail.

dictionary.

%
% contradictions
%   Finds all contradictory goals for the d-Prolog knowledge
%   base that succeed, displays them, and stores them as
%   clauses for the predicate contradictory_pair/2.
%

contradictions :-
        abolish(contradictory_pair/2),
        fail.

contradictions :-
        dPrologDictionary(Predicate,Arity),
        functor(Clause1,Predicate,Arity),
        clause(Clause1,_),
        contrary(Clause1,Clause2),
        contradictions_aux(Clause1,Clause2),
        assert(contradictory_pair(Clause1,Clause2)),
        write(Clause1), write(' - '), write(Clause2), nl,
        fail.

contradictions.

contradictions_aux(X,Y) :- 
        \+ contradictory_pair(X,Y),
        \+ contradictory_pair(Y,X),
        X,
        Y,
        !.

%
% whynot(+Goal)
%   Explains why Goal is not defeasibly derivable.
%

whynot(Goal) :-
        Goal, nl,
        write('Why not indeed!'), nl,
        write(Goal),
        write(' is strictly derivable!'), nl.

whynot(Goal) :-
        (@ Goal), nl,
        write('Why not indeed!'), nl,
        write(Goal),
        write(' is defeasibly derivable!'), nl.

whynot(Goal) :-
        \+ initial_evidence(Goal), nl,
        write('There are no rules in the database for '), nl,
        write(Goal), nl,
        write('whose antecedent is defeasibly derivable.'), nl,
        fail.

whynot(Goal) :-
        setof(pair(Rule,Defeater),
              obstruct(Goal,Rule,Defeater),
              List),
        say_why_not(List).

obstruct(Goal,(Goal :- Body),Opposite) :-
        clause(Goal,Body),
        (@ Body),
        contrary(Goal,Opposite),
        Opposite.

obstruct(Goal,(Goal :- Body),Opposite) :-
        clause(Goal,Body),
        (@ Body),
        contrary(Goal,Opposite),
        \+ Opposite,
        clause(Opposite,Tail),
        (@ Tail).

obstruct(Goal,(Goal := Body),Opposite) :-
        def_rule(root,(Goal := Body)),
        (@ Body),
        contrary(Goal,Opposite),
        Opposite.

obstruct(Goal,(Goal := Body),(Opposite :- Tail)) :-
        def_rule(root,(Goal := Body)),
        (@ Body),
        contrary(Goal,Opposite),
        clause(Opposite,Tail),
        \+ Opposite,
        (@ Tail).

obstruct(Goal,(Goal := Body),(Opposite := Tail)) :-
        def_rule(root,(Goal := Body)),
        (@ Body),
        contrary(Goal,Opposite),
        def_rule(root,(Opposite := Tail)),
        (@ Tail),
        \+ sup_rule((Goal := Body),(Opposite := Tail)).

obstruct(Goal,(Goal := Body),(Opposite :^ Tail)) :-
        def_rule(root,(Goal := Body)),
        (@ Body),
        contrary(Goal,Opposite),
        Opposite :^ Tail,
        (@ Tail),
        \+ sup_rule((Goal := Body),(Opposite :^ Tail)).

%
% say_why_not(+List)
%   displays a list of rules and defeaters for a failed
%   goal.
%

say_why_not([]).

say_why_not([pair((Goal :- Body),(Opposite :- Tail))|Rest]) :-
        !,
        write('The antecedent of the atrict rule'), nl, nl,
        pprint(Goal,' := ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but the condition of the strict rule'), nl, nl,
        pprint(Opposite,' :- ',Tail), nl,
        write('is also defeasibly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

say_why_not([pair((Goal :- Body),Opposite)|Rest]) :-
        !,
        write('The antecedent of the strict rule'), nl, nl,
        pprint(Goal,' :- ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but the condition of the strict rule'), nl, nl,
        pprint(Opposite), nl,
        write('is also strictly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

say_why_not([pair((Goal := Body),(Opposite :- Tail))|Rest]) :-
        !,
        write('The antecedent of the defeasible rule'), nl, nl,
        pprint(Goal,' := ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but the condition of the strict rule'), nl, nl,
        pprint(Opposite,' :- ',Tail), nl,
        write('is also defeasibly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

say_why_not([pair((Goal := Body),(Opposite := Tail))|Rest]) :-
        !,
        write('The antecedent of the defeasible rule'), nl, nl,
        pprint(Goal,' := ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but the condition of the defeasible rule'), nl, nl,
        pprint(Opposite,' := ',Tail), nl,
        write('is also defeasibly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

say_why_not([pair((Goal := Body),(Opposite :^ Tail))|Rest]) :-
        !,
        write('The antecedent of the defeasible rule'), nl, nl,
        pprint(Goal,' := ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but the condition of the defeater'), nl, nl,
        pprint(Opposite,' :^ ',Tail), nl,
        write('is also defeasibly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

say_why_not([pair((Goal := Body),Opposite)|Rest]) :-
        !,
        write('The antecedent of the defeasible rule'), nl, nl,
        pprint(Goal,' := ',Body), nl,
        write('is defeasibly derivable;'), nl,
        write('but'), nl, nl,
        pprint(Opposite), nl,
        write('is strictly derivable.'), nl, nl,
        pause,
        say_why_not(Rest).

pause :-
        nl,nl,
        write('Press any key to continue. '),
        get0(_),
        nl,nl.

%
% initial_evidence(+Goal)
%   Succeeds if there is a rule supporting Goal that
%   is satisfied, i.e., whose body is at least
%   defeasibly derivable.
%

initial_evidence(Goal) :-
        clause(Goal,Body),
        (@ Body).

initial_evidence(Goal) :-
        def_rule(root,(Goal := Body)),
        (@ Body).

%
% concat(+List,-String)
%   Concatenates a List of strings into a single
%   String.
%

concat(List,String) :-
        concat_aux(List,[],String).

concat_aux([],Chars,String) :-
        name(String,Chars).

concat_aux([First|Rest],Chars,String) :-
        name(First,List),
        append(Chars,List,NewChars),
        !,
        concat_aux(Rest,NewChars,String).
