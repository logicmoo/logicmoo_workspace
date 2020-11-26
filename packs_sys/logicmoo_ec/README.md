# logicmoo_ec
 
Logicmoo Event Calc -- A SWI-Prolog Pack that lets Prolog code seamlessly use planners speaking LPS, Decreasoner EventCalc, PDDLish and OCLh

Installation using SWI-Prolog 7.3 or later:

````prolog
    ?- pack_install('https://github.com/TeamSPoon/logicmoo_ec.git').
```` 
![#f03c15](https://placehold.it/15/f03c15/000000?text=+) **TODO**: Andrew Dougherty - Ensure whatever is needed from the interfaces to VAL etc 


# Solvers

ocl  = OCL Planner called HyHTN (.ocl) 
pddl = PDDL Planner by RSasak (.pddl)
dec  = Discrete Event Calc Reasoner (DEC) Shannan''s Event Calc Solver rewritten by DMiles to decreasoner .e files
lps  = Logic Production System (LPS) By LogicalContracts (.lps)
icl  = Independent Choice Logic (ICL) by David Pool


# Conversions
   Conv       Done%
 * pddl_to_ocl 90% - Needs tests
 * pddl_to_lps 50% - In Progress
 * dec_to_lps  90% - Needs tests
 * dec_to_icl  75% - In Progress
 * e_to_dec   100% - not working (lol)
 * ocl_to_lps   0% 
 * lps_to_e     0% 
 * lps_to_icl   0% - Will tell us if LPS is fastest

# Modules

logicmoo_ec
logicmoo_rsasak = Prolog native PDDL solver
logicmoo_ocl =  OCL/PDDL Sover 
logicmoo_dec = DEC Solver
logicmoo_icl = ICL Solver
logicmoo_lps = LPS Solver for PDDL/DEC Files






# Getting Started with PDDL

![#f03c15](https://placehold.it/15/f03c15/000000?text=+) **NOTICE**: This is a work in progress and is being updated weekly.

This guide is designed for first-time readers, people who need refreshers and others, like myself, who sometimes need some syntax sanity-checking.

If you read anything here that you believe needs improvement, [contribute to it on GitHub](https://github.com/TeamSPoon/logicmoo_ec). 

## Introduction

PDDL one of the few languages designed for the purpose of creating a standard for Artificial Intelligence (AI) planning. It was developed in 1998 and was introduced at ICAPS, with improvements and extensions being built into it over the years [[1]](https://en.wikipedia.org/wiki/Planning_Domain_Definition_Language#De_facto_official_versions_of_PDDL). 

The most popular of PDDL used today are PDDL2.1, which is an extension to PDDL for expressing temporal domains [[2]](http://www.jair.org/papers/paper1129.html); PDDL 3 [[3]](http://www.cs.yale.edu/homes/dvm/papers/pddl-ipc5.pdf) which adds trajectory constraints and preferences to PDDL 2.1, and PDDL+ [[4]](http://www.jair.org/papers/paper2044.html) which allows modelling mixed discrete-continuous domains in PDDL.

## SWI-Prolog Interface

A world is described by a set of states, each containing a list of **facts** and/or **objects**. A world begins with an **initial state**, and is governed by a set of rules and constraints that limit which **actions** can be taken in each state, and each action generally represents a transition to a different state. 

There are certain things we need to keep track of in the "world" Workspace. 

- **Objects**: Things in the world that interest us. ``` ?- optic_add_objects(+Workspace,+[Objects..]). ```
- **Predicates**: Facts that we are interested in (e.g. properties of objects), which can be true or false. ``` ?- optic_add_objects(+Workspace,+[Predicates..]). ```
- **An initial state**: The state of the world that we start in, i.e. things that are true at the start.
- **Goal specification**: The state of the world we want to end at, i.e. things that we want to be true at the end.
- **Actions/Operators**: Ways of changing the state of the world, i.e. things that happen that change the facts.


# Compandium 

## PDDL Files Syntax

First thing you need to know, PDDL files usually have the extension `.pddl`. 

There are two PDDL files you need to learn the syntax of:

### The Domain File

The domain file establishes the context of the world. It determines what sorts of details the states can include (predicates), and what can we do to move between states in the world (actions). 

The basic syntax of a domain file is:
```
(define (domain <domain name>)
  (:predicates
    <predicate-list>
  )
  
  (:action
    <action-details>
  )
)
```

where `<domain-name>` is the name of the world.

Both **predicates** and **actions** will become clearer in examples below.

### The Problem File

The problem file represents an instance of the world we established in the domain. It determines what is true at the start of the plan (initial state), and what we want to be true at the end of the plan (goal state). 

The basic syntax of a problem file is:

```
(define (problem <title>)
	(:domain <domain-name>)
	(:objects
    	<object-list>
	)

	(:init
		<predicates>
	)
	(:goal 
		<predicates>
	)
)
```

where `<title>` is the title of the problem file and `<domain-name>` refers to the name of the corresponding domain file.

## Simple Example: Let's Eat!

![Gripper](docs/image/arm-cupcake.png)

Let's imagine we have a robot gripper arm, a cupcake and a plate. The gripper is empty, the cupcake is on the table and we want to put the cupcake on the plate. 

Before we model this in PDDL, let's look at the components of the PDDL problem:

First we define the domain.

```
(define (domain letseat)
```

Then we define the **objects**: plate, gripper, cupcake. We will also mark the cupcake and the arm as locatable, a little hack to help us query the locations of these objects using a predicate we'll create later.
```  
(:requirements :typing) 

(:types         
    location locatable - object
	bot cupcake - locatable
    robot - bot
)
```
We also need to define some **predicates**. Is the gripper arm empty? Where is the cupcake?

```
(:predicates
	(on ?obj - locatable ?loc - location)
	(holding ?arm - locatable ?cupcake - locatable)
    (arm-empty)
    (path ?location1 - location ?location2 - location)
)
```

We'll also have to define **actions/operators**. We need to be able to pick up and drop the cupcake, as well as move the arm between the table and the plate.
```
(:action pick-up
  :parameters
   (?arm - bot
    ?cupcake - locatable
    ?loc - location)
  :precondition
   (and 
      ; Note how we use the same variable loc
      ; in both lines below. This is to make
      ; sure it's looking at the same location.
      (on ?arm ?loc) 
      (on ?cupcake ?loc) 
      (arm-empty)
    )
  :effect
   (and 
      (not (on ?cupcake ?loc))
      (holding ?arm ?cupcake)
      (not (arm-empty))
   )
)

(:action drop
  :parameters
   (?arm - bot
    ?cupcake - locatable
    ?loc - location)
  :precondition
   (and 
      (on ?arm ?loc)
      (holding ?arm ?cupcake)
    )
  :effect
   (and 
      (on ?cupcake ?loc)
      (arm-empty)
      (not (holding ?arm ?cupcake))
   )
)

(:action move
  :parameters
   (?arm - bot
    ?from - location
    ?to - location)
  :precondition
   (and 
    (on ?arm ?from) 
    (path ?from ?to)
   )
  :effect
   (and 
    (not (on ?arm ?from))
    (on ?arm ?to)
   )
)
```
 
Put all the above into a file, and you have [a domain file](https://github.com/TeamSPoon/logicmoo_ec/blob/master/docs/files/letseat/domain.pddl)!

Now we'll look at the problem file. We'll start by letting it know which domain it's associated to, and define the objects that exist in the world.
```
(define (problem letseat-simple)
	(:domain letseat)
	(:objects
    	arm - robot
    	cupcake - cupcake
    	table - location
    	plate - location
	)
```
Then, we'll define the **initial state**: the gripper is empty, the cupcake is on the table, and the arm can move between both.
```
(:init
	(on arm table)
	(on cupcake table)
	(arm-empty)
	(path table plate)
)
```
Finally, we define the **goal specification**: the cupcake on in the plate.
```
(:goal 
	(on cupcake plate)
)
```
Put that all together and you'll have [the problem file](https://github.com/TeamSPoon/logicmoo_ec/blob/master/docs/files/letseat/domain.pddl)!

If you run this using [OPTIC](https://nms.kcl.ac.uk/planning/software/optic.html), you'll get this solution:

```
Initial heuristic = 3
Initial stats: t=0s, 4299060kb
b (2 @ n=3, t=0s, 4300084kb)b (1 @ n=6, t=0s, 4308276kb)
;;;; Solution Found
; Time 0.00
; Peak memory 4308276kb
; Nodes Generated: 5
; Nodes Expanded:  3
; Nodes Evaluated: 6
; Nodes Tunneled:  1
; Nodes memoised with open actions: 0
; Nodes memoised without open actions: 6
; Nodes pruned by memoisation: 0
0: (pick-up arm cupcake table) [1]
1: (move arm table plate) [1]
2: (drop arm cupcake plate) [1]
```

### Exercises:
Here are a few tasks to make it more complex and enforce your understanding.
- Add a second cupcake on the table, and add it to the goal spec to make sure it's put on the plate as well.
- Add a unicorn object to the domain, and make the goal for the unicorn to eat the cupcake. The unicorn can only eat the cupcake if it's on the plate.

## Not-as-Simple Example

If you want to check out something a bit more complex, check out the [driverlog domain](https://github.com/TeamSPoon/logicmoo_ec/tree/master/files/driverlog).

## Past the Basics

If you're a first timer, don't venture into this part until after you've fully understood the basics. 

### Durative Actions

You can actually give actions durations to work in temporal domains.

Each condition and effect is given the time at which it's supposed to happen.

There are a few types of temporal constraints:

`(at start (<condition/effect>))`, which means this must be true or happen at the start of the action.
`(at end (<condition/effect>))` , which means this must be true or happen at the end of the action.
`(over all (<condition>))`, which means this must be true for the full duration of the action.

Below is an example of the `(move)` action from our previous example transformed into a durative action. 
```
(:durative-action move
  :duration (= ?duration 10) ; Duration goes here.
  :parameters
   (?arm - bot
    ?from - location
    ?to - location)
  :condition ; Note how this is "condition" not "pre-condition"
   (and 
    (at start (on ?arm ?from))
    (over all (path ?from ?to))
   )
  :effect
   (and 
    (at start (not (on ?arm ?from)))
    (at end (on ?arm ?to))
   )
)
```

### Functions

TBC

### Processes & Events

TBC

### 2.5 Typesystem Structure

#### 2.5.1 Introduction

The model files are based on an ontology with multiple layers of abstraction. We design our ontology by combining high level concepts and cross-domain relationships borrowed from three areas: CPS; Agent-Based Model (ABM); and Systems-of-Systems (SoS). The proposed ontology consists of an Upper Ontology, which contains the CPS, ABM, and SoS concepts and relations and a general ITS Domain Ontology. The general ITS Domain Ontology can be further referenced from ontologies that instantiate transport-domain specific transitions and states. It is of course possible to extend the upper ontology with ontologies describing other domains than ITS, for example healthcare, energy and utilities, agriculture, etc.

The objective of breaking the ontology into multiple levels is twofold. First, this approach allows to capture and isolate different levels of properties, attributes and relationships. Higher layers provide broader definitions and more abstract concepts, while lower layers are less abstract and can support specific domains and applications with concepts and relations which might not be present in the upper levels.

Second, ontologies are expected to change, grow and evolve as new domains and techniques are contemplated in them (Davies et al., 2006). Leaving the more abstract and general concepts in an upper layer, and the more specific ones in lower layers, reinforces the idea that altering the most general concepts should be avoided, making them less likely to suffer constant modifications that could lead to unnecessary changes throughout the ontology. This is important because ontologies often reuse and extend other ontologies. Updating an ontology without proper care can potentially corrupt the others depending on it and consequently all the systems that use it.

#### 2.5.2 Upper ontology design principles

Upper ontologies should be designed to describe general concepts that can be used across all domains. They have a central role in facilitating interoperability among domain specific ontologies, which are built hierarchically underneath the upper and generic layers, and therefore can be seen as specialization of the more abstract concepts.

![Figure 1](/docs/images/fig1.png)

Figure above presents a subset of the proposed upper ontology. Its development was prompted by our use cases in management and control of complex systems-of-systems, and was inspired by other ontologies such as SUMO (Suggested Upper Mergerd Ontology) (Niles and Pease, 2001), and W3C SSN (Semantic Sensor Network Ontology) (Compton et al., 2012).

Some important concepts defined on the proposed general ontology include System, Cyber-Physical System, Agent and CPS Agent. A System is a set of connected parts forming a complex whole that can also be used as a resource by other systems. A Cyber-Physical System is a system with both physical and computational components. They deeply integrate computation, communication and control into physical systems. An Agent is a system that can act on its own, sense the environment and produce changes in the world. When an agent is embedded into a cyberphysical system it is called a CPS Agent, or cyberphysical
agent.

Important for mathematical desciptions of interrelations between systems are the elements Arc, Node and Graph. Where an Arc is any element of a graph that connects two Nodes, while a Graph is a set of Nodes connected by Arcs.

The concept of System can be further expanded by a number of attributes, such as Capacity, Role and Capability that can also have relationships among them. The System itself is represented within the Declarative Knowledge as an Object. Affordance is a property the defines the tasks that can be done on a specific System, while Capability defines the set of tasks the system can perform. Systems can also have Constraints, which in turn are related to KPIs that are used to measure whether such constraints are satisfied. The higher level of the proposed ontology also provides definitions and relationships between the main Knowlegde Base concepts, the Declarative and Procedural Knowledge. 

In our knowledge model, a Transition is a Procedural Knowledge concept that determines how to achieve a certain state (Action) given that an agent observes a particular state (Precondition) as being true in the world and there is an ordered list of effect free function calls in that state (Computation). Meanwhile, both Precondition and Action have a Predicate Set that is directly related to the concept of State from the Declarative Knowledge. The Goal State, which is an specification of State, is related to the concepts of Task and Workflow from the Procedural Knowledge. Where a Workflow is defined as sequence of Tasks, which in turn is defined by a sequence of Goal States assigned to a single Agent. 

![Figure 2](/docs/images/fig2.png)

Figure above presents the main elements of the knowledge base modeling.

#### 2.5.3 ITS Domain ontology design principles

![Figure 3](/docs/images/fig3.png)

With the support of the presented upper ontology model, in this section we propose an ITS domain specific ontology, as depicted in Figure above. One of the central concepts within the ITS domain is the Transport Agent, that extends Agent from the upper ontology. The Transport Agent encompasses agents that are capable of transporting some entity, ranging from physical goods to virtual data. Some important concepts from the upper layers that apply to the Transport Agent include Dynamics and Capacity, among others. Transport Agents in turn are strongly related to the Abstract concept of Transportation Mode which defines the type of transportion scenario (e.g., Roads, Rail, Telco).

Another important concept is the Transportation Infrastructure which encompasses all elements required by a Transportation Mode, such as Routes, Tracks and Transportation Networks. Most elements within the Transportation Infrastructure are extensions of Graph, Arc and Node, abstract concepts from the Upper ontology. Therefore, by using high level graph definitions it is possible to define most of the transportation infrastructure in an ITS Domain. A node inside the transportation infrastructure is referred to as a POI (Point of Interest) and it can be any desired location within the Transportation Network (e.g., a crossing, a specific point in the route, coordinate, a warehouse, a bus stop). A Traffic Semaphore is modeled as a generic Actuator that is used to control and regulate traffic and it can be applied in any transportation scenario. A Transportable Entity encompasses any element that can be transported by a Transport Agent, such as regular Cargo or network Data. A typical Passenger is also a Transportable Entity and extends the upper ontology concept of Human.



If you'd like to be listed as  a Contributor, make a [pull request](https://github.com/TeamSPoon/logicmoo_ec/pulls).


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

### 3. References

[1] [PDDL's Wikipedia Page](https://en.wikipedia.org/wiki/Planning_Domain_Definition_Language#De_facto_official_versions_of_PDDL)

[2] Fox, M. and Long, D. (2003). PDDL2.1: An Extension to PDDL for Expressing Temporal Planning Domains. [online] Available at: [http://www.jair.org/papers/paper1129.html](http://www.jair.org/papers/paper1129.html) [Accessed 20 Nov. 2017]. 

[3] Gerevini, A. and Long, D. (2005) Plan Constraints and Preferences in PDDL3. Volume 27, pages 235-297. [online] Available at [http://www.cs.yale.edu/homes/dvm/papers/pddl-ipc5.pdf](http://www.cs.yale.edu/homes/dvm/papers/pddl-ipc5.pdf)

[4] Fox, M. and Long, D. (2006) Modelling Mixed Discrete-Continuous Domains for Planning. Volume 27, pages 235-297. [online] Available at [http://www.jair.org/papers/paper2044.html](http://www.jair.org/papers/paper2044.html)

[5] Davies, J., Studer, R., and Warren, P. (2006). Semantic Web technologies: trends and research in ontology-based systems. JohnWiley & Sons, Chichester,West Sussex, PO19 8SQ, England.

[6] Niles, I. and Pease, A. (2001). Towards a Standard Upper Ontology. In Proceedings of the International Conference on Formal Ontology in Information Systems - Volume 2001, FOIS ?01, pages 2?9, New York, NY, USA. ACM.

[7] Compton, M., Barnaghi, P., Bermudez, L., Garcia-Castro, R., Corcho, O., Cox, S., Graybeal, J., Hauswirth, M., Henson, C., Herzog, A., Huang, V., Janowicz, K., Kelsey, W. D., Phuoc, D. L., Lefort, L., Leggieri, M., Neuhaus, H., Nikolov, A., Page, K., Passant, A., Sheth, A., and Taylor, K. (2012). The SSN ontology of the W3C semantic sensor network incubator group. Web Semantics: Science, Services and Agents on the World Wide Web, 17:25 ? 32.

