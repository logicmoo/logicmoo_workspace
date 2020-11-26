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


:- module(ground_rules, [
		ground_rules/2  % +RulesIn, -RulesOut
	]).

:- use_module(op_defs).

/** <module> Variable-free rule set generator

This module transforms rule sets that contain variables into rule sets without variables.

The rule sets have to look as follows:

==
[ (label1, head1, [body1a, body1b, ...]),
  (label2, head2, [body2a, body2b, ...]),
  ... ]
==

@author Tobias Kuhn
@version 2007-02-06
*/


%% ground_rules(+RulesIn, -RulesOut)
%
% Transforms a rule set into an equivalent variable-free rule set.
%
% @param RulesIn The input rule set containing variables.
% @param RulesOut The ouput rule set that is equivalent to the input rule set but contains
%   no variables.

ground_rules(RulesIn, RulesOut) :-
    retractall(ground_term(_)),
    trunk_rules(RulesIn, TrunkRules),
    collect_ground_terms(TrunkRules),
    create_ground_rules(RulesIn, RulesOut).


%% ground_term(-Term)
%
% This dynamic predicate stores the collected ground terms.

:- dynamic(ground_term/1).


%% trunk_rules(+Rules, -TrunkRules)
%
% Removes all kinds of negation from the rules. Facts are stored with the dynamic predicate
% ground_term/1 (after the negations are removed).

trunk_rules([], []).

trunk_rules([(_, Fact, [])|RestIn], Out) :-
    !,
    trunk_term(Fact, TrunkFact),
    assert(ground_term(TrunkFact)),
    trunk_rules(RestIn, Out).

trunk_rules([(_, Head, Body)|RestIn], [(TrunkHead, TrunkBody)|RestOut]) :-
    trunk_term(Head, TrunkHead),
    trunk_body(Body, TrunkBody),
    trunk_rules(RestIn, RestOut).

trunk_body([], []).

trunk_body([E|RestIn], [TE|RestOut]) :-
    trunk_term(E, TE),
    trunk_body(RestIn, RestOut).


%% trunk_term(+TermIn, -TermOut)
%
% Removes the negation of the term (strong negation, negation as failure, or both).

trunk_term(~ (- E), E) :-
    !.

trunk_term(- E, E) :-
    !.

trunk_term(~ E, E) :-
    !.

trunk_term(E, E).


%% collect_ground_terms(+TrunkRules)
%
% Collects all the ground terms that can be derived from the truncated rule set, and stores them with
% the dynamic predicate ground_term/1.

collect_ground_terms(TrunkRules) :-
    collect(TrunkRules, NewTerms),
    assert_new_terms(NewTerms),
    (NewTerms == [] ->
        true
    ;
        collect_ground_terms(TrunkRules)
    ).


%% collect(+TrunkRules, -NewTerms)
%
% Collects the ground terms that can be directly derived from the truncated rule set using the terms
% that are stored with the dynamic predicate ground_term/1.

collect([], []).

collect([Rule|RulesRest], NewTerms) :-
    copy_term(Rule, RuleC),
    findall(Head, (get_rule_instance(RuleC), RuleC = (Head,_), \+ ground_term(Head)), NewTerms1),
    collect(RulesRest, NewTermsRest),
    append(NewTerms1, NewTermsRest, NewTerms).


%% assert_new_terms(+TermList)
%
% Stores the terms with the dynamic predicate ground_term/1.

assert_new_terms([]).

assert_new_terms([Term|Rest]) :-
    ground_term(Term),
    !,
    assert_new_terms(Rest).

assert_new_terms([Term|Rest]) :-
    assert(ground_term(Term)),
    assert_new_terms(Rest).


%% create_ground_rules(+RulesIn, -RulesOut)
%
% Transforms a rule set into a variable-free rule set, using the collected ground terms.

create_ground_rules([], []).

create_ground_rules([Rule|RulesRest], RulesOut) :-
    copy_term(Rule, RuleC),
    findall(RuleC, get_rule_instance(RuleC), GroundRules),
    create_ground_rules(RulesRest, RulesOutRest),
    append(GroundRules, RulesOutRest, RulesOut).


%% get_rules_instance(?Rule)
%
% Returns all the possible instantiations for the rule, using the collected ground terms.

get_rule_instance((_, [])).

get_rule_instance((Head, [BodyElement|Rest])) :-
    ground_term(BodyElement),
    get_rule_instance((Head, Rest)).

get_rule_instance((_, _, [])).

get_rule_instance((Label, Head, [~ _|Rest])) :-
    !,
    get_rule_instance((Label, Head, Rest)).

get_rule_instance((Label, Head, [- _|Rest])) :-
    !,
    get_rule_instance((Label, Head, Rest)).

get_rule_instance((Label, Head, [BodyElement|Rest])) :-
    ground_term(BodyElement),
    get_rule_instance((Label, Head, Rest)).
