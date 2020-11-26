# Output Format

`plasp` 3 translates SAS and PDDL files into a uniform ASP fact format.

## Overview

Essentially, `plasp`’s output format consists of [state variables](#variables) that are modified by [actions](#actions) if their preconditions are fulfilled.
Variables reference [entities](#constants-objects) that are affected by the actions.
As with PDDL, the objective is to achieve a specific [goal](#goal) starting from an [initial state](#initial-state) by executing a sequence of actions.

`plasp`’s variables correspond to the multivalued variables in SAS.
PDDL predicates are turned into Boolean variables to make the output format consistent.

Actions are modeled exactly as PDDL actions and SAS operators.

## In a Nutshell

The following illustrates `plasp`’s output format for the problem of turning switches on and off.

```prolog
% declares the type "type(switch)"
type(type(switch)).

% introduces a switch "constant(a)"
constant(constant(a)).
has(constant(a), type(switch)).

% declares a variable "variable(on(X))" for switches X
variable(variable(on(X))) :- has(X, type(switch)).

% the variable may be true or false
contains(variable(on(X)), value(on(X)), true)) :- has(X, type(switch)).
contains(variable(on(X)), value(on(X)), false)) :- has(X, type(switch)).

% declares the action "action(turnOn(X))", which requires switch X to be off and then turns it on
action(action(turnOn(X))) :- has(X, type(switch)).
precondition(action(turnOn(X)), variable(on(X)), value(on(X), false)) :- has(X, type(switch)).
postcondition(action(turnOn(X)), effect(0), variable(on(X)), value(on(X), true)) :- has(X, type(switch)).

% initially, the switch is off
initialState(variable(on(constant(a))), value(on(constant(a)), false)).

% in the end, the switch should be on
goal(variable(on(constant(a))), value(on(constant(a)), true)).
```

## Syntax and Semantics

`plasp` structures the translated ASP facts into multiple sections, which are explained in the following.

### Feature Requirements

```prolog
% declares a required feature
requires(feature(<name>)).
```

`plasp` recognizes and declares advanced features used by the input problem, such as conditional effects, [mutex groups](#mutex-groups) and [axiom rules](#axiom-rules) (currently only SAS).
See the [full list of supported features](feature-requirements.md) for more information.

The feature requirement predicates may be used in meta encodings to warn about unsupported features.

### Types

```prolog
% declares a <type>
type(type(<name>)).

% specifies that <type 1> inherits <type 2>
inherits(type(<type 1>), type(<type 2>)).

% specifies <constant> to have type type(<name>)
has(<constant>, type(<name>)).
```

[Variables](#variables), [constants](#constants-objects), and [objects](#constants-objects) may be typed. Types are only available with PDDL and if typing is enabled.

`plasp` automatically generates all matching `has` predicates for objects with types that inherit other types.

### Variables

```prolog
% declares a <variable>
variable(variable(<name>)).

% adds a <value> to the domain of a <variable>
contains(<variable>, <value>).
```

`plasp`’s variables represent the current state of the planning problem.
Variables are linked to the problem's [objects](#constants-objects) and [constants](#constants-objects).

`plasp`’s variables are multivalued, and each variable has exactly one value at each point in time.

With SAS, variable names are numbers starting at 0, `variable(<number>)`.
SAS variables are inherently multivalued, which results in two or more values of the form `value(<SAS predicate>, <SAS value>)` for each variable.

With PDDL, Boolean variables are created from the PDDL predicates.
Variables are named after the PDDL predicates, `variable(<PDDL predicate>).`
Each variable contains exactly two values (one `true`, one `false`) of the form `value(<PDDL predicate>, <bool>)`.
Note that with PDDL, variables and values are named identically.

### Actions

```prolog
% declares an <action>
action(action(<name>)).

% defines that as a precondition to <action>, <variable> must have value <value>
precondition(<action>, <variable>, <value>).

% defines that after applying <action>, <variable> is assigned <value>
postcondition(<action>, effect(<number>), <variable>, <value>).

% defines the condition of a conditional effect
precondition(effect(<number>), <variable>, <value>).

% specifies the costs of applying <action>
costs(<action>, <number>).
```

Actions may require certain variables to have specific values in order to be executed.
After applying an action, variables get new values according to the action's postconditions.

Actions may have *conditional effects*, that is, certain postconditions are only applied if additional conditions are satisfied.
For this reason, each conditional effect is uniquely identified with a predicate `effect(<number>)` as the second argument of the `postcondition` facts.
The conditions of conditional effects are given by additional `precondition` facts that take the respective `effect(<number>)` predicates as the first argument.

Unconditional effects are identified with `effect(unconditional)`.

Conditional effects are currently only supported with SAS input problems.

Actions may also have *action costs* required to apply them. Action costs are currently supported for SAS only.

### Constants/Objects

```prolog
% declares a <constant> or object
constant(constant(<name>)).

% specifies <constant> to have type type(<name>)
has(<constant>, <type>).
```

Constants and objects are the entities that are affected by [actions](#actions), for instance, the blocks in a Blocks World problem.
Constants are global for a domain, while objects are problem-specific.

`plasp` does not distinguish between the two (modeling both as constants), as both are identically used static identifiers.

### Initial State

```prolog
% initializes <variable> with a specific <value>
initialState(<variable>, <value>).
```

The initial state contains all [variable](#variables) assignments that hold before executing any [actions](#actions).

Note that with PDDL, `plasp` sets all unspecified initial state variables to `false` in order to make the initial state total.

### Goal

```prolog
% specifies that <variable> shall obtain <value> in the end
goal(<variable>, <value>).
```

The goal specifies all variable assignments that have to be fulfilled after executing the plan.

### Mutex Groups

```prolog
% declares a <mutex group>
mutexGroup(mutexGroup(<number>)).

% adds the assignment of <variable> to <value> to a <mutex group>
contains(<mutex group>, <variable>, <value>).
```

SAS contains information about mutually exclusive [variable](#variables) assignments.
That is, *at most one* variable assignment of each mutex group must be satisfied at all times.

Mutex group facts are only present with SAS input programs and not PDDL.

Mutex groups contain essential information in order to find plans correctly.
That is, if mutex groups are present in `plasp`’s output, they have to be accounted for appropriately.

### Axiom Rules

```prolog
% declares an <axiom rule>
axiomRule(axiomRule(<number>)).

% defines that as a precondition to <axiom rule>, <variable> must have value <value>
precondition(<axiom rule>, <variable>, <value>).

% defines that after applying <axiom rule>, <variable> is assigned <value>
postcondition(<axiom rule>, effect(unconditional), <variable>, <value>).
```

Axiom rules are similar to [actions](#actions) in that they modify [variables](#variables) if certain preconditions are satisfied.
However, axiom rules must be applied *immediately* as soon as their preconditions are satisfied.

The second argument of `postcondition`, `effect(unconditional)`, is not used and exists only for consistency with [actions](#actions).

Axiom rule facts are only present with SAS input programs and not PDDL.

Axiom rules contain essential information in order to find plans correctly.
That is, if axiom rules are present in `plasp`’s output, they have to be accounted for appropriately.
