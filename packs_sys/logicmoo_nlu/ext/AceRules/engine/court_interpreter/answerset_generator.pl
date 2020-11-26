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


:- module(answerset_generator, [
		     get_answerset/2  % +Program, -Output
		    ]).

:- use_module('../op_defs').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module(cycle_checker).

/** <module> Courteous answerset generator

Generates the answerset using courteous semantics.

---+++ Technical Remarks

Rules are represented as:

==
(Label, Head, Body)
==

Label can be empty (''). Head and Body can contain variables.

Facts of the (temporary) answer set are represented as:

==
(Label, FactHead, Conditions)
==

FactHead is the actual fact. Label and Conditions describe how the fact was
infered. This is needed to retract the fact if one of the conditions does not
hold anymore. FactHead and Conditions do not contain unbound variables.

A fact representation is derived from a rule representation by instantiating
the variables of Body.

@author Tobias Kuhn
@version 2007-02-06
*/


:- dynamic(reached/1).


%% get_answerset(+Program, -Output)
%
% Creates the answerset for the program.

get_answerset(Program, Output) :-
	is_open_list(Output),
	retractall(reached(_)),
    retractall(overrides(_,_)),
    retractall(inference_step(_)),
    assert(inference_step(0)),
    read_program(Program, Rules),
    answerset(Rules, Output).


%% overrides(-Label1, -Label2)
%
% Stores the overrides-statements.

:- dynamic(overrides/2).


%% inference_step(-Number)
%
% Stores the count value of inference steps.

:- dynamic(inference_step/1).


%% step
%
% This is called when a new step starts.

step :-
    inference_step(Step),
    retractall(inference_step(_)),
    NewStep is Step + 1,
    assert(inference_step(NewStep)).


%% answerset(+Rules, -Output)

answerset(Rules, Output) :-
    get_facts(Rules, Facts1),
    add_output(Output, raw, Facts1),
    % check for conflicts of the initial facts.
    append_and_resolve_conflicts([], Facts1, Facts2, Del1),
    subtract_list(Facts1, Del1, Facts1M),
    subtract_list(Facts2, Del1, Facts2M),
    handle_unresolved_conflicts([], Facts1M, Del2),
    append(Del1, Del2, DeleteList),
    cascade_delete_list(Facts2M, DeleteList, Facts3),
    assert(reached(Facts3)),
    add_output(Output, deletelist, DeleteList),
    add_output(Output, consistent, Facts3),
    answerset(Rules, Facts3, Final, Output),
    add_output(Output, final, Final).


%% answerset(+Rules, +KnowledgeBase, -NewKnowledgeBase, -Output)
%
% Uses the rules to find new knowledge on the basis of the knowledge base.

answerset(Rules, KB, NewKB, Output) :-
    fire(Rules, KB, NewKnowledge),
    store_edges(NewKnowledge),
    append_and_resolve_conflicts(KB, NewKnowledge, KB2, Del1),
    subtract_list(NewKnowledge, Del1, NewKnowledgeM),
    subtract_list(KB2, Del1, KB2M),
    handle_unresolved_conflicts(KB2M, NewKnowledgeM, Del2),
    append(Del1, Del2, DeleteList),
    cascade_delete_list(KB2, DeleteList, KB3),
    sort(KB3, KB4),
    ( reached(KB4) ->
       % this state of the knowledge base was already reached
       ( \+ KB4 = KB ->
          % state is not a fixpoint -> program was cyclic
          throw(ar_error('court-interpreter.answerset-generator.CyclicADG', 'The program contains cycles.'))
       ;
          % state is a fixpoint -> final answer set
          NewKB = KB
       )
    ;
       step,
       assert(reached(KB4)),
       add_output(Output, raw, KB2),
       add_output(Output, deletelist, DeleteList),
       add_output(Output, consistent, KB4),
       answerset(Rules, KB4, NewKB, Output)
    ). 


%% read_program(+Program, -Rules)
%
% Returns the rules and facts of the specified program. Overrides-statements
% are filtered and asserted as overrides(_,_).

read_program([], []).

read_program([overrides(A,B)|RestIn], Out) :-
    !,
    assert(overrides(A,B)),
	read_program(RestIn, Out).

read_program([Rule|RestIn], [Rule|RestOut]) :-
	read_program(RestIn, RestOut).


%% get_facts(+Rules, -Facts)
%
% Returns the facts of the specified rule set.

get_facts(Rules, Facts) :-
    findall((L, H, []), member((L,H,[]), Rules), Facts).


%% fire(+Rules, +KnowledgeBase, -NewKnowledge)
%
% Fires all rules that can be fired with the current knowledge base and returns
% the new knowledge.

fire([], _, []).

fire([Rule|Rest], KB, NewKnowledge) :-
    findall(Rule, get_rule_instance(Rule, KB), RuleInstances),
    fire(Rest, KB, NewKnowledgeTemp),
    append(RuleInstances, NewKnowledgeTemp, NewKnowledge).


%% get_rule_instance(-Rule, +KnowledgeBase)
%
% Returns an instantiated rule that can be fired using the knowledge base.

get_rule_instance((_, _, []), _).

% Auxiliary predicates can only occur on the final position. This ensures that
% the variables are instantiated.
get_rule_instance((_Label, _Head, [AuxProp]), KB) :-
    AuxProp =.. [aux_prop|_],
    !,
    % An auxiliary predicate is implicitly considered true if the negation
    % thereof is not contained in the knowledge base.
    \+ member((_, -AuxProp, _), KB).

get_rule_instance((Label, Head, [BodyElement|Rest]), KB) :-
    member((_, BodyElement, _), KB),
    get_rule_instance((Label, Head, Rest), KB).


%% append_and_resolve_conflicts(+KnowledgeBase, +NewKnowledge, -NewKnowledgeBase, -DeleteList)
%
% Appends the new knowledge to the knowledge bases and checks for conflicts. In
% case of a conflict, the conflict is resolved by adding the corresponding
% predicates to the delete-list. The new knowledge base is not necessarily
% consistent.

append_and_resolve_conflicts(KB, [], KB, []).

append_and_resolve_conflicts(KB, [Fact|Rest], NewKB, DL) :-
    append_and_resolve_conflicts(KB, Rest, TmpKB, TmpDL),
    (
        is_member(Fact, TmpKB)
    ->
        % fact is already in the knowledge base
        NewKB = TmpKB,
        DL = TmpDL
    ;
        % fact is not yet in the knowledge base
        NewKB = [Fact|TmpKB],
        resolve_conflicts(Fact, TmpKB, Remove),
        append(Remove, TmpDL, DL)
    ).


% A negated auxiliary predicate is always in conflict with the positive auxiliary
% predicate that is implicitly considered true.
resolve_conflicts((aux, -AuxProp, _Conds), _KB, [(aux, AuxProp, [])]) :-
    AuxProp =.. [aux_prop|_],
    !.

resolve_conflicts(Fact, KB, Remove) :-
    Fact = (_, FactHead, _),
    negate(FactHead, NegFactHead),
    findall((L, NegFactHead, C), member((L, NegFactHead, C), KB), ConflictFacts),
    resolve_conflict_list(Fact, ConflictFacts, Remove).


resolve_conflict_list(_NewFact, [], []).

resolve_conflict_list(NewFact, [First|Rest], Remove) :-
    resolve_conflict_list(NewFact, Rest, RemoveRest),
    resolve_conflict(NewFact, First, RemoveFirst),
    append(RemoveFirst, RemoveRest, Remove).


%% resolve_conflict(+NewFact, +ExistingFact, -Remove)
%
% Resolves the conflict between the new and the existing fact. The facts that
% have to be removed are stored in Remove.

resolve_conflict((Label, FactHead, Conds), (CLabel, _, _), Remove) :-
    overrides(CLabel, Label),
    !,
    Remove = [(Label, FactHead, Conds)].

resolve_conflict((Label, _, _), (CLabel, CFactHead, CConds), Remove) :-
    overrides(Label, CLabel),
    !,
    Remove = [(CLabel, CFactHead, CConds)].

resolve_conflict(_, _, []).


handle_unresolved_conflicts(_KB, [], []).

handle_unresolved_conflicts(KB, [Fact|Rest], DL) :-
    handle_unresolved_conflicts(KB, Rest, TmpDL),
    Fact = (_, FactHead, _),
    negate(FactHead, NegFactHead),
    findall((L, NegFactHead, C), member((L, NegFactHead, C), KB), Delete),
    (Delete = [] ->
    	DL = TmpDL
    ;
    	append([Fact|Delete], TmpDL, DL)
    ).


%% cascade_delete(+KnowledgeBase, +DeleteItem, -NewKnowledgeBase)
%
% Removes DeleteItem and all its depending items from the knowledge base.

cascade_delete(KB, DelItem, NewKB) :-
    delete_pattern(KB, DelItem, TKB),
    cascade_delete(TKB, DelItem, Deleted, TmpKB),
    cascade_delete_list(TmpKB, Deleted, NewKB).


%% cascade_delete(+KnowledgeBase, +DeleteItem, -DeletedItems, -NewKnowledgeBase)
%
% Deletes all items dependent on DeleteItem and saves them to DeletedItems

cascade_delete([], _, [], []).

cascade_delete([Fact|Rest], DelFact, [Fact|Deleted], NewKB) :-
    Fact = (_, _, Conds),
    DelFact = (_, DelFactHead, _),
    is_member(DelFactHead, Conds),
    !,
    cascade_delete(Rest, DelFact, Deleted, NewKB).

cascade_delete([Fact|Rest], DelFact, Deleted, [Fact|NewKB]) :-
    cascade_delete(Rest, DelFact, Deleted, NewKB).


%% cascade_delete_list(+KnowledgeBase, +DeleteItemList, -NewKnowledgeBase)
%
% Removes all items of DeleteItemList and all its depending items from the
% knowledge base.

cascade_delete_list(KB, [], KB).

cascade_delete_list(KB, [DelFact|RestDelFacts], NewKB) :-
    cascade_delete(KB, DelFact, TempKB),
    cascade_delete_list(TempKB, RestDelFacts, NewKB).


%% add_output(-Output, +Text, +Facts)
%
% Writes the facts onto the specified trace-stream. Text is used as title.

add_output(Output, Text, Facts) :-
    inference_step(Step),
    copy_term(Facts, FactsCopy),
    TraceElement =.. [Text, Step, FactsCopy],
    member(TraceElement, Output), !.
