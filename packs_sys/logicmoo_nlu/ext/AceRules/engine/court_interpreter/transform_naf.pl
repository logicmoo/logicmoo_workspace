% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(transform_naf, [
		     transform_naf/2  % +ListIn, +ListOut
		    ]).

:- use_module('../op_defs').
:- use_module('../utils').

/** <module> NAF eliminator

This module translates a courteous program with negation as failure (NAF) into
a program without NAF. For that reason auxiliary predicates aux_prop(...) are
introduced.

A rule with NAF has the form

==
label : h <- b1, ..., bn, ~f1, ..., ~fm
==

where h, b*, and f* are literals that may contain variables. It is transformed
into

==
label : h <- b1, ..., bn, aux_prop(n,V1,...,Vp)
-aux_prop(n,V1,...,Vp) <- f1
...
-aux_prop(n,V1,...,Vp) <- fm
aux_prop(n,V1,...,Vp) <-
==

where n is a number that distinguishes the auxiliary propositions (each
occurence of NAF gets its own unique number) and V1,...,Vp are the variables
appearing in one or more of the literals f1,...,fm.

Since the last fact contains universally quantified variables (which we do
not allow in the answer set), we do not add this fact explicitly to the
knowledge base. The instances of this fact are implicitly considered true and
the interpreter acts as if they were in the (temporary) answer set.

@author Marc Doerflinger
@author Tobias Kuhn
@version 2007-02-06
*/


:- dynamic(aux_prop_index/1).


%% transform_naf(+RulesIn, -RulesOut)
% 
% This predicate transforms and eliminates negation as failure.
% This is done by stepping through each rule and if the body contains 
% negation as failure, then these predicates are removed from the body and
% a propositional predicate prop is added. Additionally a prositiv fact with 
% the name of the propositional predicate is added and for each of the removed
% predicates another rule is added. The head of this rule is the negated 
% version of the propositional predicate and the body is made of the positive
% version of the predicate, that was negated by negation as failure. 

transform_naf(RulesIn, RulesOut) :-
    retractall(aux_prop_index(_)),
    assert(aux_prop_index(1)),
    process_rules(RulesIn, RulesOut).


process_rules([], []).

process_rules([Rule|RestIn], RulesOut) :-
    transform_rule(Rule, ModRules),
    process_rules(RestIn, RestOut),
    append(ModRules, RestOut, RulesOut).


%% transform_rule(+RuleIn, -RuleOut)
%
% This predicate removes the negation as failure terms from the body of 
% the rule and adds the propositional predicate and also add the auxiliarty
% rules, that were generated. 

transform_rule(Fact, [Fact]) :-
    Fact = (_, _, []),
    !.

transform_rule((Label, Head, Body), RulesOut) :-
    split_naf(Body, NoNaf, Naf),
    \+ Naf = [],
    !,
    next_aux_prop_index(AuxIndex),
    extract_vars(Naf, NafVars),
    AuxProp =.. [aux_prop, AuxIndex|NafVars],
    create_aux_rules(Naf, AuxProp, AuxRules),
    % The interpreter requires the auxiliary predicate to be at the final position.
    % This ensures that the variables are instantiated when checking the auxiliary
    % predicate.
    append(NoNaf, [AuxProp], NewBody),
    RulesOut = [(Label, Head, NewBody)|AuxRules].

transform_rule(Rule, [Rule]).
    % rule contains no naf.


split_naf([], [], []).

split_naf([~ Term|In], NoNafOut, [Term|NafOut]) :-
    !,
    split_naf(In, NoNafOut, NafOut).

split_naf([Term|In], [Term|NoNafOut], NafOut) :-
    split_naf(In, NoNafOut, NafOut).


%% create_aux_rules(+NafList, +AuxProp, -AuxRules)
%
% Generates the auxilliary rules for all the removes negation as failure
% predicates.

create_aux_rules([], _AuxProp, []).

create_aux_rules([Term|Rest], AuxProp, [AuxRule|AuxRules]) :-
    copy_term((aux, - AuxProp, [Term]), AuxRule),
    create_aux_rules(Rest, AuxProp, AuxRules).


next_aux_prop_index(Index) :-
    aux_prop_index(Index),
    retractall(aux_prop_index(_)),
    NewIndex is Index + 1,
    assert(aux_prop_index(NewIndex)).
