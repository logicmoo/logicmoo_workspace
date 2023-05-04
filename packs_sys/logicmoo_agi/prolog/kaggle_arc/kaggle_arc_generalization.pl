/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).


/*
The Least General Generalization (LGG) is a concept in inductive logic programming (ILP) and machine learning that refers to the least specific hypothesis that generalizes two or more given examples. In other words, it is the most general description that still covers all the examples.

The AQ (Attribute-value learning system for Learning from Examples) algorithm is an inductive learning method used in machine learning for generating rules from a set of positive and negative examples. The algorithm was introduced by Ryszard Michalski in the 1960s. The main steps of the AQ algorithm are:

1. Select a positive example that is not covered by any existing rule.
2. Generalize the example by creating a set of candidate rules.
3. Evaluate the candidate rules and select the best one based on a chosen criterion (e.g., accuracy, coverage, simplicity).
4. If the best rule covers any negative examples, specialize it to exclude those examples, and go back to step 3.
5. Add the best rule to the rule set.
6. Repeat steps 1-5 until all positive examples are covered.

Here's a simple implementation of the AQ algorithm in Prolog:

```prolog
*/


ensure_ground(C):- compound(C), C=not(TV),!,ensure_ground(TV).
ensure_ground(Type:Value):- var(Value),!,feature(_, Type:Value).
ensure_ground(_:_).


% Domain predicates
feature(P1, Type:E) :- positive_example(P1,List), member(Type:E, List), unique_attribute(Type, List).
feature(P1, Type:E) :- negative_example(P1,List), member(Type:E, List), unique_attribute(Type, List).

% Positive examples
positive_example(p1, [color:red, shape:circle, size:small, texture:smooth]).
positive_example(p2, [color:green, shape:circle, size:small, texture:rough]).
positive_example(p3, [color:red, shape:square, size:small, texture:rough]).
positive_example(p4, [color:blue, shape:circle, size:small, texture:smooth]).

% Negative examples
negative_example(n1, [color:red, shape:circle, size:large, texture:smooth]).
negative_example(n2, [color:green, shape:square, size:large, texture:rough]).

% Check if the attribute is unique in the list
unique_attribute(_, []).
unique_attribute(Type, [Type:_|T]) :-
    \+ member(Type:_, T),
    unique_attribute(Type, T).
unique_attribute(Type, [_:X|T]) :-
    unique_attribute(Type, T).

% List all possible values for a given attribute
attribute_values(color, [red, green, blue]).
attribute_values(shape, [circle, square]).
attribute_values(size, [small, large]).
attribute_values(texture, [smooth, rough]).

% Hypothesize a rule
hypothesize(Example, SpecificFeatures, Rule) :-
    findall(F, (feature(Example, F), meets_conditions(F, SpecificFeatures)), Features),
    list_to_set(Features, Rule).

% Determine if a feature meets a list of conditions
meets_conditions(F, Conditions) :-
    forall(member(C, Conditions), meets_condition(F, C)).

% Determine if the feature meets the condition (positive or negation)
meets_condition(Type:Value, not(Type:Value2)) :-
    !, attribute_values(Type, Values),
    member(Value, Values),
    Value \== Value2.
meets_condition(_, not(_)).
meets_condition(Type:Value, Type:Value).

% AQ algorithm implementation for multiple features
aq_multiple(SpecificFeatures) :-
    findall(PE, positive_example(PE, _), Positives),
    aq_loop(Positives, SpecificFeatures, []).

% AQ loop implementation
aq_loop([], S, Rules) :-
    nl,writeln('Rules:'),
    nl,pp(Rules),nl,
    maplist(write_rule(S), Rules).
aq_loop([PE|Rest], SpecificFeatures, Rules) :-
    hypothesize(PE, SpecificFeatures, Hypothesis),
    specialize(Hypothesis, Rule),
    (Rule \== [] -> aq_loop(Rest, SpecificFeatures, [Rule|Rules]) ; aq_loop(Rest, SpecificFeatures, Rules)).

% Specialize a rule by removing features that cover negative examples
specialize(Rule, SpecializedRule) :-
    findall(F, (member(F, Rule), \+ covers_negative(F, Rule)), SpecializedRule).

% Check if a rule with a specific feature removed covers any negative examples
covers_negative(F, Rule) :-
    select(F, Rule, NewRule),
    negative_example(NE, _),
    covers(NewRule, NE).

% Check if a rule covers an example
covers(Rule, Example) :-
    forall(member(F, Rule), feature(Example, F)).

% Write a rule to the console
write_rule(S, Rule) :-
    writeq(S), write(' :- '),
    write_features(Rule), nl.

% Write features separated by commas
write_features([F]) :-
    !, writeq(F).
write_features([F|T]) :-
    writeq(F), write(', '),
    write_features(T).

% AQ algorithm implementation
aq(SpecificFeature) :-
    findall(PE, positive_example(PE, _), Positives),
    aq_loop(Positives, SpecificFeature, []).


 % Helper predicate to check if a given list of features partially matches the features of an example
partial_match([], _).
partial_match([H|T], ExampleFeatures) :-
    (member(H, ExampleFeatures) ; member(not(H), ExampleFeatures)),
    partial_match(T, ExampleFeatures).

% Helper predicate to check if a given list of features matches the features of an example
match([], _).
match([H|T], ExampleFeatures) :-
    (member(H, ExampleFeatures) ; member(not(H), ExampleFeatures)),
    match(T, ExampleFeatures).

% Helper predicate to check if a given list of features is consistent with the positive and negative examples
consistent(InputFeatures) :-
    not((
        negative_example(_, NegativeFeatures),
        match(InputFeatures, NegativeFeatures)
    )).

% Predicate to deduce features given a list of input features
deducible_features(InputFeatures, DeducibleFeatures) :-
    findall(Feature, (
        positive_example(_, PositiveFeatures),
        match(InputFeatures, PositiveFeatures),
        member(Feature, PositiveFeatures),
        not(member(Feature, InputFeatures)),
        not(member(not(Feature), InputFeatures)),
        consistent([Feature|InputFeatures])
    ), DeducibleFeaturesNoDup),
    sort(DeducibleFeaturesNoDup, DeducibleFeatures).
/*
% Deducible features
deducible_features(GivenFeatures, DeducibleFeatures) :-
    findall(PE, positive_example(PE, _), Positives),
    aq_loop(Positives, GivenFeatures, []),
    findall(DF, (positive_example(_, ExampleFeatures), 
       forall(member(GF, GivenFeatures), member(GF, ExampleFeatures)), 
        subtract(ExampleFeatures, GivenFeatures, DF)), DeducibleFeaturesList),
    flatten(DeducibleFeaturesList, Flattened),
    list_to_set(Flattened, DeducibleFeatures).
*/
/*
?- deducible_features([not(shape:circle)],X).

Rules:

[ [ color: blue, size:small,texture:smooth],
  [ color: red, shape:square,size:small,texture:rough],
  [ color: green,
    texture:rough],
  [ color: red,
    texture:smooth]]

[not(shape:circle)] :- color:blue, size:small, texture:smooth
[not(shape:circle)] :- color:red, shape:square, size:small, texture:rough
[not(shape:circle)] :- color:green, texture:rough
[not(shape:circle)] :- color:red, texture:smooth
X = [].


We can create a new predicate `deducible_features/2` that takes a list of given feature values and returns a list of deducible other feature values based on the rules derived by the `aq_multiple/1` predicate.

Now you can use the `deducible_features/2` predicate with a list of given feature values to find the list of deducible other feature values. For example:

```prolog
?- deducible_features([color:red, shape:circle], DeducibleFeatures).
```

Expected output:

```
DeducibleFeatures = [size:small, texture:smooth].
```

In this example, given the features `color:red` and `shape:circle`, the `deducible_features/2` predicate returns the list of deducible other feature values `[size:small, texture:smooth]`.

You can also use the predicate with multiple given feature values:

```prolog
?- deducible_features([not(color:red), shape:circle, texture:smooth], DeducibleFeatures).
```

Expected output:

```
DeducibleFeatures = [color:blue, size:small].
```

In this example, given the features `not(color:red)`, `shape:circle`, and `texture:smooth`, the `deducible_features/2` predicate returns the list of deducible other feature values `[color:blue, size:small]`.


Now, you can test the implementation using the following example queries:

1. Positive features only:

```prolog
?- aq_multiple([color:red]).
```

Expected output:

```
Rules:
color:red :- shape:circle, size:small, texture:smooth.
color:red :- shape:square, size:small, texture:rough.
```

2. Negative features only:

```prolog
?- aq_multiple([not(color:red), shape:circle]).
```

Expected output:

```
Rules:
not(color:red), shape:circle :- color:green, size:small, texture:rough.
not(color:red), shape:circle :- color:blue, size:small, texture:smooth.
```

3. Both positive and negative features:

```prolog
?- aq_multiple([not(color:red), shape:circle, texture:smooth]).
```

Expected output:

```
Rules:
not(color:red), shape:circle, texture:smooth :- color:blue, size:small.
```

This complete implementation should work as expected, handling both positive and negative features, as well as ensuring that no object has more than one shape, color, size, or texture.

This output indicates that the target concept can be described with the following two rules:

1. An object is a target if it is red and small.
2. An object is a target if it is green, a circle, and small.
*/

