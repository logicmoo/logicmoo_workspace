% From the book
% PROLOG PROGRAMMING IN DEPTH
% by Michael A. Covington, Donald Nute, and Andre Vellino
% (Prentice Hall, 1997).
% Copyright 1997 Prentice-Hall, Inc.
% For educational use only

% DPROLOG.PL
%
% A defeasible inference engine for Prolog.
%

:- op(1100,fx,@),
   op(1100,xfy,:=),
   op(1100,xfy,:^).

:- dynamic (:=)/2, (:^)/2.
:- multifile (:=)/2, (:^)/2.
:- higher_order( (0 := 1) ).

:- external sup/2, incompatible/2, (~)/1.
:- public (@)/1.
:- public preempt/0.

~(~P) :- P.

%
% @(+Goal)
%   Succeeds if Goal is defeasibly derivable from
%   the d-Prolog knowledge base.
%

@ Goal :- def_der(root,Goal).

%
% strict_der(+KB,+Goal)
%   Succeeds if Goal is derivable from the ordinary Prolog
%   facts and rules in the d-Prolog knowledge base.
%

strict_der(root,Goal) :-
      % A goal is derivable from the complete d-Prolog
      % knowledge base if and only if it succeeds.
        !,
        call(Goal).

strict_der(KB,(First,Rest)) :-
      % A conjunction is strictly derivable if and only if
      % both of its conjuncts are strictly derivable.
        !,
        strict_der(KB,First),
        strict_der(KB,Rest).

strict_der(KB,Goal) :-
      % A goal is strictly derivable from a knowledge base
      % other than the complete knowledge base if it is
      % a contained in that knowledge base.
        conjunct(Goal,KB).

strict_der(KB,Goal) :-
      % A goal is strictly derivable from a knowledge base
      % other than the complete knowledge base if it is
      % the head of an ordinary Prolog rule in the complete
      % knowledge base whose nonempty condition is strictly
      % derivable from that knowledge base.
        clause(Goal,Condition),
        Condition \== true,
        strict_der(KB,Condition).

%
% def_der(+KB,+Goal)
%   Succeeds if Goal is defeasibly derivable from the
%   knowledge base KB.
%

def_der(_,true) :- !.
      % `true' is defeasibly derivable from any
      % knowledge base.

def_der(KB,(First,Rest)) :-
      % A conjunction is defeasibly derivable if 
      % and only if both conjuncts are defeasibly 
      % derivable.
        !,
        def_der(KB,First),
        def_der(KB,Rest).

def_der(_,Goal) :-
        %predicate_property(Goal,built_in),
        %\+ functor(Goal,',',_),
      % A goal with a built-in functor is defeasibly
      % derivable if and only if it succeeds. The test
      % used here is for Quintus Prolog. This test may
      % be different for another version of Prolog.
   dprolog_treat_as_primitive(Goal),
        !,
        Goal.

dprolog_treat_as_primitive(kind_of(_,_)).
dprolog_treat_as_primitive(is_a(_,_)).
dprolog_treat_as_primitive(true_after(_,_)).
dprolog_treat_as_primitive(Goal) :-
   predicate_property(Goal, built_in).

def_der(KB,Goal) :-
      % A goal is defeasibly derivable if it is
      % strictly derivable.
        strict_der(KB,Goal).

def_der(KB,Goal) :-
      % A goal is defeasibly derivable if it is
      % the head of an ordinary Prolog rule whose
      % condition is defeasibly derivable and which
      % is not rebutted.
        clause(Goal,Condition),
        Condition \== true,
        def_der(KB,Condition),
        \+ rebutted(KB,(Goal :- Condition)).

def_der(KB,Goal) :-
      % A goal is defeasibly derivable if it is
      % the head of a defeasible rule whose
      % condition is defeasibly derivable and which
      % is neither rebutted nor undercut.
        def_rule(KB,(Goal := Condition)),
        def_der(KB,Condition),
        \+ rebutted(KB,(Goal := Condition)),
        \+ undercut(KB,(Goal := Condition)).

def_der(KB,Goal) :-
        preemption,
      % If defeater preemption is enabled, then
      % a goal is defeasibly derivable if it is 
      % the head of a defeasible rule whose condition
      % is defeasibly derivable, provided every
      % rebutting or undercutting defeater for that
      % rule is itself rebutted by a strict rule or
      % a superior defeasible rule.
        def_rule(KB,(Goal := Condition)),
        \+ (contrary(Goal,Contrary1),
             strict_der(KB,Contrary1)),
        def_der(KB,Condition),
        \+ (contrary(Goal,Contrary2),
             clause(Contrary2,Condition2),
             Condition2 \== true,
             def_der(KB,Condition2)),
        \+ (contrary(Goal,Contrary3),
             def_rule(KB,(Contrary3 := Condition3)),
             def_der(KB,Condition3),
             \+ (preempted(KB,(Contrary3 := Condition3)))),
        \+ (contrary(Goal,Contrary4),
             (Contrary4 :^ Condition4),
             def_der(KB,Condition4),
             \+ (preempted(KB,(Contrary4 :^ Condition4)))).

%
% contrary(+Clause1,-Clause2)
%   Discovers Clause2 which either is the complement of
%   Clause1 or is incompatible with Clause1.
%

contrary(Clause1,Clause2) :-
        incompatible(Clause1,Clause2).

contrary(Clause1,Clause2) :-
        incompatible(Clause2,Clause1).

contrary(Clause1,Clause2) :-
        comp(Clause1,Clause2).

%
% comp(+Clause1,-Clause2)
%   Succeeds if Clause1 is ~Clause2 or if Clause2 is
%   is ~Clause1.
%

comp(~Atom,Atom) :-
        !.

comp(Atom,~Atom).

%
% rebutted(+KB,+Rule)
%   Succeeds if the Rule is defeated in the knowledge
%   base KB by a rebutting defeater (an ordinary
%   Prolog rule or a defeasible rule to which the Rule 
%   is not superior).
%

rebutted(KB,Rule) :-
      % Any rule is rebutted if a contrary of
      % its head is strictly derivable.
        Rule =.. [_,Head,_],
        contrary(Head,Contrary),
        strict_der(KB,Contrary),
        !.

rebutted(KB,Rule) :-
      % Any rule may be rebutted by an ordinary
      % Prolog rule with a contrary head.
        Rule =.. [_,Head,_],
        contrary(Head,Contrary),
        clause(Contrary,Body),
        Body \== true,
        def_der(KB,Body).

rebutted(KB,(Head := Body)) :-
      % Defeasible rules may be rebutted by other
      % defeasible rules with contrary heads.
        contrary(Head,Contrary),
        def_rule(KB,(Contrary := Condition)),
        def_der(KB,Condition),
        \+ sup_rule((Head := Body),(Contrary := Condition)),
        !.

%
% undercut(+KB,+Rule)
%   Succeeds if the Rule is defeated in the knowledge
%   base KB by an undercutting defeater.
%

undercut(KB,(Head := Body)) :-
      % Only defeasible rules may be undercut by pure
      % defeaters.
        contrary(Head,Contrary),
        (Contrary :^ Condition),
        def_der(KB,Condition),
        \+ sup_rule((Head := Body),(Contrary :^ Body)),
        !.

%
% sup_rule(+Rule1,+Rule2)
%   Succeeds if the body of Rule2 is defeasibly derivable 
%   from the body of Rule1, but the body of Rule1 is not 
%   defeasibly derivable from the body of Rule2. The user 
%   can also force superiority by adding clauses for the 
%   predicate `sup'.
%

sup_rule(Rule1,Rule2) :-
        sup(Rule1,Rule2).

sup_rule((_ := Body1),(_ := Body2)) :-
        def_der(Body1,Body2),
        \+ def_der(Body2,Body1).

sup_rule((_ := Body1),(_ :^ Body2)) :-
        def_der(Body1,Body2),
        \+ def_der(Body2,Body1).

%
% conjunct(+Clause1,+Clause2)
%

conjunct(Clause,Clause) :- !.

conjunct(Clause,(Clause,_)) :- !.

conjunct(Clause,(_,Rest)) :-
        conjunct(Clause,Rest).

%
% def_rule(+KB,+Rule)
%   Succeeds if KB is the entire d-Prolog knowledge base
%   and Rule is any defeasible rule in KB, or if Rule is a 
%   defeasible rule in the d-Prolog knowledge base and the 
%   body of Rule is not the atom `true'.
%

def_rule(root,(Head := Body)) :-
        !,
        (Head := Body).

def_rule(_,(Head := Body)) :-
        (Head := Body),
        Body \== true.

%
% preempted(+KB,+Rule)
%   Succeeds if the Rule is defeated in the knowledge
%   base KB by a superior rebutting defeater (an ordinary
%   Prolog rule or a defeasible rule which is superior
%   to the Rule.)
%

preempted(KB,Rule) :-
      % Any rule is preempted if a contrary of
      % its head is strictly derivable.
        Rule =.. [_,Head,_],
        contrary(Head,Contrary),
        strict_der(KB,Contrary),
        !.

preempted(KB,Rule) :-
      % Any rule may be preempted by an ordinary
      % Prolog rule with a contrary head.
        Rule =.. [_,Head,_],
        contrary(Head,Contrary),
        clause(Contrary,Body),
        Body \== true,
        def_der(KB,Body).

preempted(KB,(Head := Body)) :-
      % Defeasible rules may be preempted by superior
      % defeasible rules with contrary heads.
        contrary(Head,Contrary),
        def_rule(KB,(Contrary := Condition)),
        def_der(KB,Condition),
        sup_rule((Contrary := Condition),(Head := Body)),
        !.

%
% preempt
%   Toggles the preemption of defeaters feature between
%   enabled and disabled.
%

:- external preemption/0.

preempt :-
        retract(preemption),
        !,
        write('Preemption is disabled.'), nl.

preempt :-
        assert(preemption),
        write('Preemption is enabled.'), nl.

