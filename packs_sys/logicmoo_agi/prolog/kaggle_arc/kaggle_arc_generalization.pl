/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).

end_of_file.

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
unique_attribute(Type, [_:_X|T]) :-
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
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)

The code provided is a Prolog program that represents a rule learning problem. The main goal of the problem is to 
 learn rules from example pairs of property groups, apply these rules to a given group of properties, and guess 
 the resulting group of properties.

To briefly explain the main parts of the code:

1. Constant values: The `constant_vals/17` predicate defines a set of constants that are used thDeleteout the program.

2. Example pairs: The `example_pair_of_property_groups/2` predicates define a series of example pairs of property groups. Each pair consists of two groups of properties (each group is represented as a list of properties), and the goal is to learn rules that can transform the first group into the second group.

3. Extracting properties: The `extract_properties/2` predicate is a helper predicate used to extract properties from example pairs.

4. Updating knowledge: The `update_knowledge/4` predicate is used to update the rule knowledge based on the given pair of property groups. The knowledge update process involves finding the least general generalization (LGG) of the two property groups and updating the knowledge with the LGG.

5. Learning rules: The `learn_rules/3` predicate is a recursive predicate that learns rules from example pairs. The predicate initializes the knowledge base with some basic rules and iteratively updates the knowledge based on the example pairs.

6. Initialization of knowledge: The `init_knowledge/1` predicate is used to initialize the knowledge base with some basic rules.

7. Learning rules from examples: The `learn_rules_from_examples/1` predicate is used to learn rules from example pairs.

8. Guessing rules: The `guess_rules_of_example_pairs/1` predicate is used to guess the rules of example pairs by learning rules from the examples.

9. Calculating offset: The `calculate_offset/2` predicate is used to calculate the offset from example pairs.

10. Applying rules: The `apply_rules_to_group/3` and related predicates are used to apply the learned rules to a given group of properties.

11. Guessing scene pairs: The `guess_scene_pair/2` predicate is the main predicate that takes a group of properties (Silvers) as input, applies the learned rules, and returns the resulting group of properties (Result).

You can run this Prolog program in a Prolog interpreter such as SWI-Prolog. 

The main predicate to execute is `guess_scene_pair/2`, providing a group of properties as input and receiving the resulting 
group of properties as output.

Here are examples of Scene pairs defined in Prolog.  
*/
constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab,
   Size1,Size2,Size3,Active,Inert,Keep,Delete):-
     Color=color,Shape=shape,Size=size,Texture=texture,Location=position,
     Color1=yellow,Color2=blue,Color3=red,Drab=silver,
     Size1=small,Size2=medium,Size3=large,
     Active=round,Inert=square,
     Keep=smooth,Delete=thorny.

:- style_check(-singleton).

example_pair_of_property_groups(
  [
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Delete), prop(Location, 40, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 50, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 60, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 70, 90)])], 
  [
     properties([prop(Color,Color2),   prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 51, 81)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)]), 
     properties([prop(Color,Color3), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 71, 91)])])
  :- constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab, Size1,Size2,Size3,Active,Inert,Keep,Delete).

example_pair_of_property_groups(
  [
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 10, 80)]), 
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 20, 80)]), 
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 30, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 60, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 70, 90)])], 
  [
     properties([prop(Color,Color2),   prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 11, 81)]), 
     properties([prop(Color,Color2),   prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 21, 81)]), 
     properties([prop(Color,Color2),   prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 31, 81)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)]), 
     properties([prop(Color,Color3), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 71, 91)])])
  :- constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab, Size1,Size2,Size3,Active,Inert,Keep,Delete). 

example_pair_of_property_groups(
  [
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 40, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size2), prop(Texture, Delete), prop(Location, 50, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 60, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size3), prop(Texture, Keep), prop(Location, 70, 90)])], 
  [
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 41, 81)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)]), 
     properties([prop(Color,Drab), prop(Shape, Inert), prop(Size, Size3), prop(Texture, Keep), prop(Location, 71, 91)])])
  :- constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab, Size1,Size2,Size3,Active,Inert,Keep,Delete).

example_pair_of_property_groups(
  [
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 40, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 70, 90)])], 
  [
     properties([prop(Color,Color1),    prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 41, 81)]), 
     properties([prop(Color,Color3), prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 71, 91)])])
  :- constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab,Size1,Size2,Size3,Active,Inert,Keep,Delete).

example_pair_of_property_groups(
  [
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 11, 81)]), 
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 21, 81)]), 
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 31, 81)]), 
     properties([prop(Color,Drab),  prop(Shape, Inert),  prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)]), 
     properties([prop(Color,Drab),  prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 71, 91)])], 
  [
     properties([prop(Color,Color1),   prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 12, 82)]), 
     properties([prop(Color,Color2),   prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 22, 82)]), 
     properties([prop(Color,Color3),   prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 32, 82)]), 
     properties([prop(Color,Drab),     prop(Shape, Inert),  prop(Size, Size1), prop(Texture, Keep), prop(Location, 62, 82)]), 
     properties([prop(Color,Color3),   prop(Shape, Active), prop(Size, Size3), prop(Texture, Keep), prop(Location, 72, 92)])])
   :- constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab,Size1,Size2,Size3,Active,Inert,Keep,Delete).


/*

The 5 example pairs have some things in common:

   All LHS properties are all Drab color.

   All example pair property lists seem to have rules about how they are copied to the LHS

   Circle shaped properties get copied to RHS with a new color based on size
    prop(Shape, Active), prop(Size, Size1) -> prop(Color,Color1).
    prop(Shape, Active), prop(Size, Size2) -> prop(Color,Color2).
    prop(Shape, Active), prop(Size, Size3) -> prop(Color,Color3).

   Rough textuColor1 properties in LHS are not copied to RHS
    prop(Texture, Delete) -> removed.

   LHS properties copies are moved down and right by Size1
    prop(Location, X, Y) -> prop(Location, X+Size1, Y+Size1).


Use any algorythemsyou wish if you need a hint try least general generalization along with the AQ algorithm

In order to write `guess_rules_of_example_pairs/Size1`

That it acts like this: 
```Prolog
?- guess_rules_of_example_pairs(Rules).

Rules = 
   [ prop(Shape, Active), prop(Size, Size1) -> prop(Color,Color1), 
     prop(Shape, Active), prop(Size, Size2) -> prop(Color,Color2), 
     prop(Shape, Active), prop(Size, Size3) -> prop(Color,Color3), 
     prop(Texture, Delete) -> removed, 
     prop(Location, X, Y), plus(X, Size1, X1), plus(Y, Size1, Y1) -> prop(Location, X1, Y1)].
```

And next apply_rules_to_group/Size3  
```Prolog
?- guess_rules_of_pairs(Rules), apply_rules_to_group(Rules, 
  [ properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 50, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 40, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 60, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size2), prop(Texture, Delete), prop(Location, 70, 90)])], 
  Guess).

Rules = 
   [ prop(Shape, Active), prop(Size, Size1) -> prop(Color,Color1), 
     prop(Shape, Active), prop(Size, Size2) -> prop(Color,Color2), 
     prop(Shape, Active), prop(Size, Size3) -> prop(Color,Color3), 
     prop(Texture, Delete) -> removed, 
     prop(Location, X, Y), plus(X, Size1, X1), plus(Y, Size1, Y1) -> prop(Location, X1, Y1) ], 
Guess =
  [properties([prop(Color,Color2), prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 51, 81)]), 
   properties([prop(Color,Color1), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 41, 81)]), 
   properties([prop(Color,Color1), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)])]).
```

So the final result is to implement guess_scene_pair/Size2 in Prolog

```Prolog
guess_scene_pair(Silvers, Result):- 
  guess_rules_of_example_pairs(Rules), 
  apply_rules_to_group(Rules, Silvers, Guess).
```

So it operates like 

```Prolog
?- guess_scene_pair(
  [
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 50, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 40, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 60, 80)]), 
     properties([prop(Color,Drab), prop(Shape, Active), prop(Size, Size2), prop(Texture, Delete), prop(Location, 70, 90)])], 
  Guess).

The result should be.

Guess =
  [properties([prop(Color,Color2), prop(Shape, Active), prop(Size, Size2), prop(Texture, Keep), prop(Location, 51, 81)]), 
   properties([prop(Color,Color1), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 41, 81)]), 
   properties([prop(Color,Color1), prop(Shape, Active), prop(Size, Size1), prop(Texture, Keep), prop(Location, 61, 81)])]).
```

Here's the full implementation with all the predicates combined:

```prolog
*/
:- style_check(+singleton).
% Predicate for least general generalization of two properties

lgg(A,B,C):- pp(lgg(A,B,C)),lgg_property(A,B,C).

%lgg(A,B,C):- is_lsimaplist(lgg_property, P1, P2, LGG).
  
lgg_property(A, B, C):- merge_vals(A, B, C).

/*
%lgg_property(A, A, A) :- !. lgg_property(_, _, _).
% Original rule (unchanged)
lgg_property_unused(S, S, S) :-
    constant_vals(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, S).

% Handle numeric properties with average
lgg_property_unused(A, B, C) :-
    number(A), number(B),
    C is (A + B) / 2.

% Handle color properties with a new color (could also be a blend)
lgg_property_unused(A, B, new_color) :-
    constant_vals(_, A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    constant_vals(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    A \= B.

% Handle shape properties with a new shape (could also be a blend)
lgg_property_unused(A, B, new_shape) :-
    constant_vals(_, _, A, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
    constant_vals(_, _, _, B, _, _, _, _, _, _, _, _, _, _, _, _, _),
    A \= B.
*/
% Add other rules for different property types as needed

% Predicate for applying rules to a property
apply_rules(_, [], []).
apply_rules(Rules, [P | Ps], [Q | Qs]) :-
    apply_rules_to_property(Rules, P, Q),
    apply_rules(Rules, Ps, Qs).

apply_rules_to_property([], P, P).
apply_rules_to_property([Rule | Rules], P, Q) :-
    (apply_rule_PR(Rule, P, R) -> apply_rules_to_property(Rules, R, Q);
     apply_rules_to_property(Rules, P, Q)).


apply_rule_PR(prop(Shape, Active),prop(Size, Size1) -> prop(Color, Color1), properties(Ps), properties([prop(Color, Color1) | Ps])) :-
    member(prop(Shape, Active), Ps),
    member(prop(Size, Size1), Ps).
apply_rule_PR(prop(Shape, Active),prop(Size, Size2) -> prop(Color, Color2), properties(Ps), properties([prop(Color, Color2) | Ps])) :-
    member(prop(Shape, Active), Ps),
    member(prop(Size, Size2), Ps).
apply_rule_PR(prop(Shape, Active),prop(Size, Size3) -> prop(Color, Color3), properties(Ps), properties([prop(Color, Color3) | Ps])) :-
    member(prop(Shape, Active), Ps),
    member(prop(Size, Size3), Ps).
apply_rule_PR(prop(Texture, Delete) -> removed, properties(Ps), properties(NewPs)) :-
    (select(prop(Texture, Delete), Ps, NewPs), !; NewPs = Ps).
apply_rule_PR(prop(Location, X,Y),plus(X,1,X1),plus(Y,1,Y1) -> prop(Location, X1,Y1), properties(Ps), properties(NewPs)) :-
    select(prop(Location, X, Y), Ps, Remaining),
    X1 is X + 1,
    Y1 is Y + 1,
    append(Remaining, [prop(Location, X1, Y1)], NewPs).



% Helper predicate to extract properties from example pairs
extract_properties([], []).
extract_properties([example_pair_of_property_groups(P1, P2) | Examples], [P1-P2 | Properties]) :-
    extract_properties(Examples, Properties).

% Helper predicate to update the rule knowledge
update_knowledge(P1, P2, Knowledge, NewKnowledge) :-
    maplist(update_property_knowledge(P2), P1, Knowledge, NewKnowledge).

% Helper predicate to update the property knowledge
update_property_knowledge(P2, P1, OldKnowledge, NewKnowledge) :-
    lgg(P1, P2, LGG), 
    update_knowledge_with_lgg(LGG, OldKnowledge, NewKnowledge).


update_knowledge_with_lgg(Term1, Term2, Properties) :-
    lgg(Term1, Term2, LGGTerm),
    update_properties(LGGTerm, Properties, UpdatedLGGTerm),
    pp(updatedLGGTerm(UpdatedLGGTerm)),
    assertz_if_new(kb(UpdatedLGGTerm)).

update_properties(Term, [], Term).
update_properties(Term, [Prop|Props], UpdatedTerm) :-
    lgg_property(Term, Prop, TempTerm),
    update_properties(TempTerm, Props, UpdatedTerm).


% Predicate to learn rules from example pairs
learn_rules([], Knowledge, Knowledge).
learn_rules([P1-P2 | Examples], Knowledge, Rules) :-
    update_knowledge(P1, P2, Knowledge, NewKnowledge), 
    learn_rules(Examples, NewKnowledge, Rules).

% Predicate to initialize knowledge
init_knowledge([
    prop(Shape, Active), prop(Size, Size1) -> prop(Color,_), 
    prop(Shape, Active), prop(Size, Size2) -> prop(Color,_), 
    prop(Shape, Active), prop(Size, Size3) -> prop(Color,_), 
    prop(Texture, _) -> _, 
    prop(Location, _, _) -> prop(Location, _, _)
]):-
  constant_vals(Color,Shape,Size,Texture,Location,
   _Color1,_Color2,_Color3,_Drab,
   Size1,Size2,Size3,
   Active,_ShapeInert,_Keep,_Delete).

% Predicate to learn rules from example_pair_of_property_groups
learn_rules_from_examples(Rules) :-
    findall(example_pair_of_property_groups(E1, E2), 
      example_pair_of_property_groups(E1, E2), Examples),
    extract_properties(Examples, Properties),
    init_knowledge(Knowledge),
    learn_rules(Properties, Knowledge, Rules).

% Predicate for guessing rules of example pairs
guess_rules_of_example_pairs(Rules) :-
    learn_rules_from_examples(Rules).

% Helper predicate to extract offset from example pairs
extract_offset([], XOffset, YOffset, XOffset, YOffset).
extract_offset([properties(LHS) - properties(RHS) | Pairs], XOffsetAcc, YOffsetAcc, XOffset, YOffset) :-
    member(prop(Location, X1, Y1), LHS), 
    member(prop(Location, X2, Y2), RHS), 
    XOffsetNew is max(XOffsetAcc, X2 - X1), 
    YOffsetNew is max(YOffsetAcc, Y2 - Y1), 
    extract_offset(Pairs, XOffsetNew, YOffsetNew, XOffset, YOffset).

% Predicate to calculate offset from example pairs
calculate_offset(XOffset, YOffset) :-
    findall(E, example_pair_of_property_groups(_, E), Examples),
    extract_properties(Examples, Properties),
    extract_offset(Properties, 0, 0, XOffset, YOffset).

% Predicate to apply rules to a group
apply_rules_to_group([], Group, Group).
apply_rules_to_group([Rule | Rules], Group, Result) :-
    maplist(apply_rule(Rule), Group, NewGroup), 
    apply_rules_to_group(Rules, NewGroup, Result).

% Predicate to apply a single rule to a property
apply_rule(Rule, properties(Props), properties(NewProps)) :-
    apply_rule_to_props(Rule, Props, NewProps).

% Apply rule to properties
apply_rule_to_props(_, [], []).
apply_rule_to_props(Rule, [Prop | Props], [NewProp | NewProps]) :-
    apply_rule_to_prop(Rule, Prop, NewProp), 
    apply_rule_to_props(Rule, Props, NewProps).

% Apply rule to a single property
apply_rule_to_prop(prop(Shape, Active) -> prop(Color,NewColor), prop(Shape, Active), prop(Shape, Active)):- 
  only_modify_when(Color, NewColor, Shape, Active).

apply_rule_to_prop(prop(Size, SizeN) -> prop(Color,NewColor), prop(Size, SizeN), prop(Size, SizeN)):-
  prop_to_prop(Color,NewColor,Size,SizeN).
  %constant_vals(Color,Shape,Size,Texture,Location,Color1,Color2,Color3,Drab, Size1,Size2,Size3, Active,Inert,Keep,Delete).


apply_rule_to_prop(prop(Texture, _) -> removed, prop(Texture, _), removed).
apply_rule_to_prop(prop(Location, X, Y) -> prop(Location, X1, Y1), prop(Location, X, Y), prop(Location, X1, Y1)) :-
    calculate_offset(XOffset, YOffset), 
    X1 is X + XOffset, 
    Y1 is Y + YOffset.
apply_rule_to_prop(Rule, Prop, Prop) :-
    \+ Rule = (Prop -> _).

% Predicate to guess scene pair
guess_scene_pair(Silvers, Result):- 
    guess_rules_of_example_pairs(Rules), 
    apply_rules_to_group(Rules, Silvers, Result).

/*

*/



end_of_file.



:- dynamic positive/1.
:- dynamic negative/1.

% Sample data - positive and negative examples
positive(properties([color(silver), shape(circle), size(1), texture(rough)])).
positive(properties([color(silver), shape(circle), size(2), texture(smooth)])).
% ... add more positive examples

negative(properties([color(blue), shape(circle), size(2), texture(smooth)])).
negative(properties([color(silver), shape(square), size(1), texture(smooth)])).
% ... add more negative examples

% List of attributes
attributes([color(_), shape(_), size(_), texture(_)]).

% Checks if a given example is covered by a list of attribute-value pairs
covered(Example, AttrValues) :-
    forall(member(AttrValue, AttrValues), member(AttrValue, Example)).

% Generates a star from an example
star(Example, Star) :-
    findall(AttrValue, (member(AttrValue, Example), not(member(neg(AttrValue), Example))), Star).

% Determines if a hypothesis covers only positive examples
valid_hypothesis(Hypothesis) :-
    forall(positive(Example), covered(Example, Hypothesis)),
    forall(negative(Example), not(covered(Example, Hypothesis))).

% AQ Algorithm
aq_algorithm :-
    findall(Star, (positive(Example), star(Example, Star)), Stars),
    find_star_combinations(Stars, [], Hypothesis),
    valid_hypothesis(Hypothesis),
    write('Hypothesis: '), writeln(Hypothesis).

% Finds a combination of stars that covers all positive examples and no negative examples
find_star_combinations(_, Hypothesis, Hypothesis) :-
    valid_hypothesis(Hypothesis).
find_star_combinations([Star|Stars], CurrentHypothesis, Hypothesis) :-
    append(CurrentHypothesis, Star, NewHypothesis),
    (valid_hypothesis(NewHypothesis) ->
        Hypothesis = NewHypothesis
    ;
        find_star_combinations(Stars, NewHypothesis, Hypothesis)
    ).








% Candidate Elimination Algorithm

run_candidate_elim :- candidate_elim([[_,_,_]], [], 
	[[small, medium, large], [red, blue, green], [ball, brick, cube]]).

% Candidate_elim implements a top level read-process loop
% It prints out the current G set 
% the S set
%		     
% and calls process to process the input.
candidate_elim([G],[S],_) :-
	covers(G,S),covers(S,G),
	write("target concept is "), write(G),nl.

candidate_elim(G, S, Types) :-
	write("G= "), write(G),nl,
	write("S= "), write(S),nl,
	write("Enter Instance "),
	read(Instance),
	process(Instance, G, S, Updated_G, Updated_S, Types),
	candidate_elim(Updated_G, Updated_S, Types).

% Process takes the instance, 
%	a set of concepts, G and 
%	and a set of concepts, S
% it implements a step of the candidate elimination algorithm.


process(negative(Instance), G, S, Updated_G, Updated_S, Types) :- 
	delete(X, S, covers(X, Instance), Updated_S),
	specialize_set(G,Spec_G, Instance, Types),
	delete(X, Spec_G, (member(Y, Spec_G), more_general(Y, X)), Pruned_G),
	delete(X, Pruned_G, (member(Y, Updated_S), not(covers(X, Y))), Updated_G).

process(positive(Instance), G, [], Updated_G, [Instance],_):- 
% Initialize S

delete(X, G, not(covers(X, Instance)), Updated_G).

process(positive(Instance), G, S, Updated_G, Updated_S,_) :- 
	delete(X, G, not(covers(X, Instance)), Updated_G),
	generalize_set(S,Gen_S, Instance),
	delete(X, Gen_S, (member(Y, Gen_S), more_general(X, Y)), Pruned_S),
	delete(X, Pruned_S, not((member(Y, Updated_G), covers(Y, X))), Updated_S).

process(Input, G, P, G, P,_):- 
	Input \= positive(_),
	Input \= negative(_),
	write("Enter either positive(Instance) or negative(Instance) "), nl

% The following predicate definitions are duplicated in either
% the general to specific searches or the specific to genearal searches.
% 
specialize_set([], [], _, _).
specialize_set([Hypothesis|Rest],Updated_H,Instance, Types):-	
	covers(Hypothesis, Instance),			
	(bagof(Hypothesis, specialize(Hypothesis, Instance, Types), Updated_head); Updated_head = []),
	specialize_set(Rest, Updated_rest, Instance, Types),
	append(Updated_head, Updated_rest, Updated_H).

specialize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance, Types):-
	not (covers(Hypothesis, Instance)),			
	specialize_set(Rest,Updated_rest, Instance, Types).

specialize([Prop|_], [Inst_prop|_], [Instance_values|_]):-
 	var(Prop),
	member(Prop, Instance_values),
	Prop \= Inst_prop.

specialize([_|Tail], [_|Inst_tail], [_|Types]):-
	specialize(Tail, Inst_tail, Types).

%

generalize_set([], [], _).

generalize_set([Hypothesis|Rest],Updated_H,Instance):-			
	not(covers(Hypothesis, Instance)),
	(bagof(X, generalize(Hypothesis, Instance, X), Updated_H); Updated_head = []),
	generalize_set(Rest,Updated_rest, Instance),
	append(Updated_head, Updated_rest, Updated_H).

generalize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance):-
	covers(Hypothesis, Instance),
	generalize_set(Rest,Updated_rest, Instance).

%

generalize([],[],[]).	
generalize([Feature|Rest], [Inst_prop|Rest_inst], [Feature|Rest_gen]) :-
	not(Feature \= Inst_prop),
	generalize(Rest, Rest_inst, Rest_gen).

generalize([Feature|Rest], [Inst_prop|Rest_inst], [_|Rest_gen]) :-
	Feature \= Inst_prop,
	generalize(Rest, Rest_inst, Rest_gen).

% more_general(Feature_vector_1, Feature_vector_2) :- succeeds if
%	Feature_vector_1 is strictly more general than Feature_vector_2

more_general(X, Y) :-  not(covers(Y, X)), covers(X, Y).

% covers(Feature_list_1, Feature_list_2) :- Succeeds if Feature_list_1
%	covers Feature_list_2.  Note that covers, unlike unification is
%	not symmetric: variables in Feature_list_2 will not match constants
%	in Feature_list_1.

covers([],[]).
covers([H1|T1], [H2|T2]) :-
	var(H1), var(H2), 
	covers(T1, T2).
covers([H1|T1], [H2|T2]) :-
	var(H1), atom(H2), 
	covers(T1, T2).	
covers([H1|T1], [H2|T2]) :-
	atom(H1), atom(H2), H1 = H2,
covers(T1, T2).

% delete(Element, List1, Goal, List2) :- List2 contains all bindings
%	of Element to a member of List1 except those that cause 
%	Goal to succeed

delete(X, L, Goal, New_L) :-
	(bagof(X, (member(X, L), not(Goal)), New_L);New_L = []).  

