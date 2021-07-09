# Pfc -- forward chaining in Prolog

Installation using SWI-Prolog 7.1 or later:

    `?- pack_install('https://github.com/logicmoo/pfc.git').`



This is a modification of Tim Finin's PFC.

Notable changes are:

 * Forward chaining `=>` is renamed to `==>` to avoid conflict with extensive downstream use of =>/2 to mean logical implication.
 * Bidirectional Forward chaining `<=>` renamed to `<==>` to avoid conflict with logical equivance `<=>`
 * Memoized backchain `<=` is renamed to `<-` to avoid conflict with extensive downstream use of <=/2 to mean reverse implication..  Historically '<-' had meant what is now know as ":-" being unused it was borrowed as it means "Backchaining"
 * Added Macro Transform =@=> so instead of asserting the Anteceedant to assert the Consequent

 @TODO - MANY MORE CHANGES TO WRITE - for now back to coding...
 @TODO = Added Cutted Forward Chaining =!=> as a signal to stop processing rules on first success

    
# Original README

The Pfc system is a package that provides a forward reasoning capability to be used together with conventional Prolog programs.  The Pfc inference rules are Prolog terms which are asserted as clauses into the regular Prolog database.  When new facts or forward reasoning rules are added to the Prolog database (via a special predicate add/1, forward reasoning is triggered and additional facts that can be deduced via the application of the forward chaining rules are also added to the database.  A simple justification-based truth-maintenance system is provided as well as simple predicates to explore the resulting proof trees.

It was originally written circa 1988 at the [Unisys Paoli Research Center](https://en.wikipedia.org/wiki/Paoli_Research_Center).  For more information, see

* Tim Finin,Rich Fritzson and Dave Matuszek, [Adding Forward Chaining and Truth Maintenance to Prolog](http://ebiq.org/p/682), IEEE Conf. on Artificial Intelligence Applications, pp. 123-130, Miami, March 1989.

* Tim Finin, [Pfc User Manual](https://github.com/finin/pfc/blob/master/man/pfc.pdf), Technical Report, COmputer Science and Electrical Engineering, University of Maryland, Baltimore COunty, August 1999.

or contact Tim Finin, finin@umbc.edu

If you use Pfc in your research, please cite the 1989 IEEE CAIA paper.



## Logicmoo/Pfc User Manual

Douglas Miles
503-427-8020
[dmiles@logicmoo.org](mailto:dmiles@logicmoo.org)

Tim Finin
Unisys Paoli Research Center
PO Box 517
Paoli, PA 19301

215-648-7446
[finin@prc.unisys.com](mailto:finin@prc.unisys.com)

February 22, 2021


### Contents
[TOC]

### 1 Introduction

Prolog, like most logic programming languages, offers backward chaining as the only reasoning scheme. It is well known that sound and complete reasoning systems can be built using either exclusive backward chaining or exclusive forward chaining [[5](#bookmark=id.35nkun2)]. Thus, this is not a theoretical problem. It is also well understood how to “implement” forward reasoning using an exclusively backward chaining system and vice versa. Thus, this need not be a practical problem. In fact, many of the logic-based languages developed for AI applications [[3](#bookmark=id.1ksv4uv), [1](#bookmark=id.44sinio), [6](#bookmark=id.2jxsxqh), [2](#bookmark=id.z337ya)] allow one to build systems with both forward and backward chaining rules.

There are, however, some interesting and important issues which need to be addressed in order to provide the Prolog programmer with a practical, efficient, and well integrated facility for forward chaining. This paper describes such a facility, _Pfc_ , which we have implemented in standard Prolog.

The _Pfc_ system is a package that provides a forward reasoning capability to be used together with conventional Prolog programs. The _Pfc_ inference rules are Prolog terms which are asserted as facts into the regular Prolog database. For example, Figure [1](#bookmark=id.3j2qqm3) shows a file of _Pfc_ rules and facts which are appropriate for the ubiquitous kinship domain.



---


_________________________________________________________________________________________


```prolog
spouse(X,Y) <==> spouse(Y,X).
spouse(X,Y),gender(X,G1),{otherGender(G1,G2)}
     ==>gender(Y,G2).
gender(P,male) <==> male(P).
gender(P,female) <==> female(P).
parent(X,Y),female(X) <==> mother(X,Y).
parent(X,Y),parent(Y,Z) ==> grandparent(X,Z).
grandparent(X,Y),male(X) <==> grandfather(X,Y).
grandparent(X,Y),female(X) <==> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      ==>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <==> grandmother(X,Y).
parent(X,Y),male(X) <==> father(X,Y).
mother(Ma,X),mother(Ma,Y),{X\==Y}
     ==>sibling(X,Y).
```


**Figure 1: Examples of _Pfc_ rules which represent common kinship relations**

_________________________________________________________________________________________



---


The rest of this manual is structured as follows. The next section provides an informal introduction to the _Pfc_ language. Section three describes the predicates through which the user calls _Pfc_?he final section gives several longer examples of the use of _Pfc_


#### Getting and installing _Pfc_

Look for _Pfc_ on ftp.cs.umbc.edu in /pub/pfc/.


### 2 An Informal Introduction to the _Pfc_ language

This section describes _Pfc_ . We will start by introducing the language informally through a series of examples drawn from the domain of kinship relations. This will be followed by an example and a description of some of the details of its current implementation.


#### Overview

The _Pfc_ package allows one to define forward chaining rules and to add ordinary Prolog assertions into the database in such a way as to trigger any of the _Pfc_ rules that are satisfied. An example of a simple _Pfc_ rule is:


```prolog
gender(P,male) ==> male(P)
```


This rule states that whenever the fact unifying with _gender_(_P,male_) is added to the database, then the fact _male_(_P_) is true. If this fact is not already in the database, it will be added. In any case, a record will be made that the validity of the fact _male_(_P_) depends, in part, on the validity of this forward chaining rule and the fact which triggered it. To make the example concrete, if we add _gender_(_john,male_), then the fact _male_(_john_) will be added to the database unless it was already there.

In order to make this work, it is necessary to use the predicate _add/1 _rather than _assert/1 _in order to assert _Pfc_ rules and any facts which might appear in the lhs of a _Pfc_ rule.


#### Compound Rules

A slightly more complex rule is one in which the rule’s left hand side is a conjunction or disjunction of conditions:


```prolog
parent(X,Y),female(X) ==> mother(X,Y)
mother(X,Y);father(X,Y) ==> parent(X,Y)
```


The first rule has the effect of adding the assertion _mother_(_X,Y _) to the database whenever _parent_(_X,Y _) and _female_(_X_) are simultaneously true for some _X _and _Y _. Again, a record will be kept that indicates that any fact _mother_(_X,Y _) added by the application of this rule is justified by the rule and the two triggering facts. If any one of these three clauses is removed from the database, then all facts solely dependent on them will also be removed. Similarly, the second example rule derives the parent relationship whenever either the mother relationship or the father relationship is known.

In fact, the lhs of a _Pfc_ rule can be an arbitrary conjunction or disjunction of facts. For example, we might have a rule like:


```prolog
P, (Q;R), S ==> T
```


_Pfc_ handles such a rule by putting it into conjunctive normal form. Thus the rule above is the equivalent to the two rules:


```prolog
P,Q,S ==> T
P,R,S ==> T
```



#### Bi-conditionals

_Pfc_ has a limited ability to express bi-conditional rules, such as:


```prolog
mother(P1,P2) <==> parent(P1,P2), female(P1).
```


In particular, adding a rule of the form `P<==>Q `is the equivalent to adding the two rules `P==>Q `and `Q==>P`. The limitations on the use of bi-conditional rules stem from the restrictions that the two derived rules be valid horn clauses. This is discussed in a later section.


#### Backward-Chaining _Pfc_ Rules

_Pfc_ includes a special kind of backward chaining rule which is used to generate all possible solutions to a goal that is sought in the process of forward chaining. Suppose we wished to define the _ancestor _relationship as a _Pfc_ rule. This could be done as:


```prolog
parent(P1,P2) ==> ancestor(P1,P2).
parent(P1,P2), ancestor(P2,P3) ==> ancestor(P1,P3).
```


However, adding these rules will generate a large number of assertions, most of which will never be needed. An alternative is to define the _ancestor _relationship by way of backward chaining rules which are invoked whenever a particular ancestor relationship is needed. In _Pfc_ this need arises whenever facts matching the relationship are sought while trying a forward chaining rule.


```prolog
ancestor(P1,P2) <- {\+var(P1)}, parent(P1,X), ancestor(X,P2).
ancestor(P1,P2) <- {var(P1),\+var(P2)}, parent(X,P2), ancestor(P2,X).
```



#### Conditioned Rules

It is sometimes necessary to add some further condition on a rule. Consider a definition of sibling which states:

Two people are siblings if they have the same mother and the same father. No one can be his own sibling.

This definition could be realized by the following _Pfc_ rule


```prolog
mother(Ma,P1), mother(Ma,P2), {P1\==P2},
  father(Pa,P1), father(Pa,P2)
   ==>  sibling(P1,P2).
```


Here we must add a condition to the lhs of the rule which states the the variables _P_1 and _P_2 must not unify. This is effected by enclosing an arbitrary Prolog goal in braces. When the goals to the left of such a bracketed condition have been fulfilled, then it will be executed. If it can be satisfied, then the rule will remain active, otherwise it will be terminated.


#### Negation

We sometimes want to draw an inference from the absence of some knowledge. For example, we might wish to encode the default rule that a person is assumed to be male unless we have evidence to the contrary:


```prolog
person(P), ~female(P) ==> male(P).
```


A lhs term preceded by a ~ is satisfied only if _no _fact in the database unifies with it. Again, the _Pfc_ system records a justification for the conclusion which, in this case, states that it depends on the absence of the contradictory evidence. The behavior of this rule is demonstrated in the following dialogue:


```prolog
?- add_PFC(person(P), ~female(P) ==> male(P)).
yes
?- add_PFC(person(alex)).
yes
?- male(alex).
yes
?- add_PFC(female(alex)).
yes
?- male(alex)
no
```


As a slightly more complicated example, consider a rule which states that we should assume that the parents of a person are married unless we know otherwise. Knowing otherwise might consist of either knowing that one of them is married to a yet another person or knowing that they are divorced. We might try to encode this as follows:


```prolog
parent(P1,X),
parent(P2,X),
{P1\==P2},
~divorced(P1,P2),
~spouse(P1,P3),
{P3\==P2},
~spouse(P2,P4),
{P4\==P1}
  ==>
spouse(P1,P2).
```


Unfortunately, this won’t work. The problem is that the conjoined condition


```prolog
~spouse(P1,P3),{P3\==P2}
```


does not mean what we want it to mean - that there is no _P_3 distinct from _P_2 that is the spouse of _P_1. Instead, it means that _P_1 is not married to any _P_3. We need a way to move the qualification `{P3\==P2} `inside the scope of the negation. To achieve this, we introduce the notion of a qualified goal. A lhs term _P/C_, where P is a positive atomic condition, is true only if there is a database fact unifying with _P _and condition _C _is satisfiable. Similarly, a lhs term ~ _P/C_, where P is a positive atomic condition, is true only if there is no database fact unifying with _P _for which condition _C _is satisfiable. Our rule can now be expressed as follows:


```prolog
parent(P1,X),
  parent(P2,X)/(P1\==P2),
  ~divorced(P1,P2),
  ~spouse(P1,P3)/(P3\==P2),
  ~spouse(P2,P4)/(P4\==P1)
  ==>
  spouse(P1,P2).
```



#### Procedural Interpretation

Note that the procedural interpretation of a _Pfc_ rule is that the conditions in the lhs are checked _from left to right_. One advantage to this is that the programmer can chose an order to the conditions in a rule to minimize the number of partial instantiations. Another advantage is that it allows us to write rules like the following:


```prolog
at(Obj,Loc1),at(Obj,Loc2)/{Loc1\==Loc2}
   ==> {remove(at(Obj,Loc1))}.
```


Although the declarative reading of this rule can be questioned, its procedural interpretation is clear and useful:

If an object is known to be at location _Loc_1 and an assertion is added that it is at some location _Loc_2, distinct from _Loc_1, then the assertion that it is at _Loc_1 should be removed.


#### The Right Hand Side

The examples seen so far have shown a rules rhs as a single proposition to be “added” to the database. The rhs of a _Pfc_ rule has some richness as well. The rhs of a rule is a conjunction of facts to be “added” to the database and terms enclosed in brackets which represent conditions/actions which are executed. As a simple example, consider the conclusions we might draw upon learning that one person is the mother of another:


```prolog
mother(X,Y) ==>
  female(X),
  parent(X,Y),
  adult(X).
```


As another example, consider a rule which detects bigamists and sends an appropriate warning to the proper authorities:


```prolog
spouse(X,Y), spouse(X,Z), {Y\==Z} ==>
   bigamist(X),
   {format("~N~w is a bigamist, married
      to both ~w and ~w~n",[X,Y,Z])}.
```


Each element in the rhs of a rule is processed from left to right — assertions being added to the database with appropriate support and conditions being satisfied. If a condition can not be satisfied, the rest of the rhs is not processed.

We would like to allow rules to be expressed as bi-conditional in so far a possible. Thus, an element in the lhs of a rule should have an appropriate meaning on the rhs as well. What meaning should be assigned to the conditional fact construction (e.g. _P/Q_) which can occur in a rules lhs? Such a term in the rhs of a rule is interpreted as a _conditioned assertion_. Thus the assertion _P/Q _will match a condition _P_' in the lhs of a rule only if _P _and _P_' unify and the condition _Q _is satisfiable. For example, consider the rules that says that an object being located at one place is reason to believe that it is not at any other place:


```prolog
at(X,L1) ==> not(at(X,L2))/L2\==L1
```


Note that a _conditioned assertion _is essentially a Horn clause. We would express this fact in Prolog as the backward chaining rule:


```prolog
not(at(X,L2)) :- at(X,L1),L1\==L2.
```


The difference is, of course, that the addition of such a conditioned assertion will trigger forward chaining whereas the assertion of a new backward chaining rule will not.


#### The Truth Maintenance System

As discussed in the previous section, a forward reasoning system has special needs for some kind of _truth_ _maintenance system_. The _Pfc_ system has a rather straightforward TMS system which records justifications for each fact deduced by a _Pfc_ rule. Whenever a fact is removed from the database, any justifications in which it plays a part are also removed. The facts that are justified by a removed justification are checked to see if they are still supported by some other justifications. If they are not, then those facts are also removed.

Such a TMS system can be relatively expensive to use and is not needed for many applications. Consequently, its use and nature are optional in _Pfc_ and are controlled by the predicate _pfcTmsMode/_1. The possible cases are three:



*   _pfcTmsMode_(_full_) - The fact is removed unless it has _well founded support _(WFS). A fact has WFS if it is supported by the _user _or by _God _or by a justification all of whose justificees have WFS[1](file:///G:\GDrive-logicmoo_org\Logicmoo%20Corp\Technical_Overviews\Citations\MLR_AW\NARS\man2\pfc2.html#fn1x0).
*   _pfcTmsMode_(_local_) - The fact is removed if it has no supporting justifications.
*   _pfcTmsMode_(_none_) - The fact is never removed.

A fact is considered to be supported by _God _if it is found in the database with no visible means of support. That is, if _Pfc_ discovers an assertion in the database that can take part in a forward reasoning step, and that assertion is not supported by either the user or a forward deduction, then a note is added that the assertion is supported by _God_. This adds additional flexibility in interfacing systems employing _Pfc_ to other Prolog applications.

For some applications, it is useful to be able to justify actions performed in the rhs of a rule. To allow this, _Pfc_ supports the idea of declaring certain actions to be _undoable _and provides the user with a way of specifying methods to undo those actions. Whenever an action is executed in the rhs of a rule and that action is undoable, then a record is made of the justification for that action. If that justification is later invalidated (e.g. through the retraction of one of its justificees) then the support is checked for the action in the same way as it would be for an assertion. If the action does not have support, then _Pfc_ trys each of the methods it knows to undo the action until one of them succeeds.

In fact, in _Pfc_ , one declares an action as undoable just by defining a method to accomplish the undoing. This is done via the predicate _pfcUndo/_2. The predicate _pfcUndo_(_A_1_,A_2) is true if executing _A_2 is a possible way to undo the execution of _A_1. For example, we might want to couple an assertional representation of a set of graph nodes with a graphical display of them through the use of _Pfc_ rules:


```prolog
at(N,XY) ==> {displayNode(N,XY)}.
arc(N1,N2) ==> {displayArc(N1,N2}.

pfcUndo(displayNode(N,XY),eraseNode(N,XY)).
pfcUndo(displayArc(N1,N2),eraseArc(N1,N2)).
```



#### Limitations

The _Pfc_ system has several limitations, most of which it inherits from its Prolog roots. One of the more obvious of these is that _Pfc_ rules must be expressible as a set of horn clauses. The practical effect is that the rhs of a rule must be a conjunction of terms which are either assertions to be added to the database or actions to be executed. Negated assertions and disjunctions are not permitted, making rules like


```prolog
parent(X,Y) <==> mother(X,Y);father(X,Y)
male(X) <==> ~female(X)
```


ill-formed.

Another restrictions is that all variables in a _Pfc_ rule have implicit universal quantification. As a result, any variables in the rhs of a rule which remain uninstantiated when the lhs has been fully satisfied retain their universal quantification. This prevents us from using a rule like


```prolog
father(X,Y), parent(Y,Z)
    <==> grandfather(X,Z).
```


with the desired results. If we do add this rule and assert _grandfather(john,mary)_, then _Pfc_ will add the two independent assertions _father(john,_) _(i.e. “John is the father of everyone”) and _parent(_,mary) _(i.e. “Everyone is Mary’s parent”).

Another problem associated with the use of the Prolog database is that assertions containing variables actually contain “copies” of the variables. Thus, when the conjunction


```prolog
add_PFC(father(adam,X)), X=able
```


is evaluated, the assertion `father(adam,_G032) `is added to the database, where _G032 is a new variable which is distinct from X. As a consequence, it is never unified with _able_.


### 3 Predicates


#### 3.1 Manipulating the Database

_________________________________________________________________________________________

**add_PFC(+P) **

The fact or rule P is added to the database with support coming from the user. If the fact already exists, an additional entry will not be made (unlike Prolog). If the facts already exist with support from the user, then a warning will be printed if _pfcWarnings _is true. Add/1 always succeeds.

_________________________________________________________________________________________

**call_PFC(?P) **

The predicate _call_PFC/_1 is the proper way to access terms in the _Pfc_ database. **call_PFC(P) **succeeds if **P** is a term in the current pfc database after invoking any backward chaining rules or is provable by Prolog.

_________________________________________________________________________________________

**rem_PFC(+P) **

The first fact (or rule) unifying with _P _has its user support removed. _rem/_1 will fail if no there are no _Pfc_ added facts or rules in the database which match. If removing the user support from a fact leaves it unsupported, then it will be removed from the database.

_________________________________________________________________________________________

**rem2_PFC(+P)**

The first fact (or rule) unifying with _P _will be removed from the database even if it has valid justifications. _rem/_1 will fail if no there are no _Pfc_ added facts or rules in the database which match. If removing the user support from the fact leaves it unsupported, then it will be removed from the database. If the fact still has valid justifications, then a _Pfc_ warning message will be printed and the justifications removed.

_________________________________________________________________________________________

.

**pfcReset **

Resets the _Pfc_ database by trying to retract all of the prolog clauses which were added by calls to add or by the forward chaining mechanism.

_________________________________________________________________________________________

**Term expansions **

_Pfc_ defines term expansion procedures for the operators _=¿_, _¡= _and _¡=¿ _so that you can have things like the following in a file to be consulted


```prolog
foo(X) ==> bar(X).
==> foo(1).
```


The result will be an expansion to:


```prolog
:- add_PFC((foo(X) ==> bar(X)).
:- add_PFC(foo(1)).
```



#### 3.2 Control Predicates

This section describes predicates to control the forward chaining search strategy and truth maintenance operations.

_________________________________________________________________________________________

**pfcSearch(P) **

This predicate is used to set the search strategy that _Pfc_ uses in doing forward chaining. The argument should be one of direct,depth,breadth.

_________________________________________________________________________________________

**pfcTmsMode(Mode) **

This predicate controls the method used for truth maintenance. The three options are none,local,cycles. Calling pfcTmsMode with an instantiated argument will set the mode to that argument.



*   **none **means that no truth maintenance will be done.
*   **local **means that limited truth maintenance will be done. Specifically, no cycles will be checked.
*   **cycles **means that full truth maintenance will be done, including a check that all facts are well grounded.

_________________________________________________________________________________________

**pfcHalt **

Immediately stop the forward chaining process.

_________________________________________________________________________________________

**pfcRun **

Continue the forward chaining process.

_________________________________________________________________________________________

**pfcStep **

Do one iteration of the forward chaining process.

_________________________________________________________________________________________

**pfcSelect(P) **

Select next fact for forward chaining (user defined)

_________________________________________________________________________________________

**pfcWarnings** \
**pfcNoWarnings **

_________________________________________________________________________________________


#### 3.3 The TMS

The following predicates are used to access the tms information associated with _Pfc_ facts.

**justification_PFC(+P,-J)** \
**justifications_PFC(+P,-Js) **

_________________________________________________________________________________________

**justification_PFC(P,J) **is true if one of the justifications for fact P is J, where J is a list of _Pfc_ facts and rules which taken together deduce P. Backtracking into this predicate can produce additional justifications. If the fact was added by the user, then one of the justifications will be the list **[user]**. **justifications_PFC(P,Js) **is provided for convenience. It binds **Js **to a list of all justifications returned by **(**justification/2).

**base_PFC(+P,-Ps) **

_________________________________________________________________________________________

**assumptions_PFC(+P,-Ps) **

_________________________________________________________________________________________

**pfcChild(+P,-Q)** \
**pfcChildren(+P,-Qs) **

_________________________________________________________________________________________

**pfcDescendant(+P,-Q)** \
**pfcDescendants(+P,-Qs) **

_________________________________________________________________________________________


#### 3.4 Debugging

_________________________________________________________________________________________

**pfcTrace** \
**pfcTrace(+Term)** \
**pfcTrace(+Term,+Mode)** \
**pfcTrace(+Term,+Mode,+Condition) **

This predicate causes the addition and/or removal of _Pfc_ terms to be traced if a specified condition is met. The arguments are as follows:



*   term - Specifies which terms will be traced. Defaults to **_ **(i.e. all terms).
*   mode - Specifies whether the tracing will be done on the addition (i.e. **add**, removal (i.e. **rem**) or both (i.e. **_**) of the term. Defaults to **_**.
*   condition - Specifies an additional condition which must be met in order for the term to be traced. For example, in order to trace both the addition and removal of assertions of the age of people just when the age is greater than 100, you can do **pfcTrace(age(_,N),_,N¿100)**.

Thus, calling **pfcTrace **will cause all terms to be traced when they are added and removed from the database. When a fact is added or removed from the database, the lines


```prolog
1
2
```


are displayed, respectively.

_________________________________________________________________________________________

**pfcUntrace** \
**pfcUntrace(+Term)** \
**pfcUntrace(+Term,+Mode)** \
**pfcUntrace(+Term,+Mode,+Condition) **

The **pfcUntrace **predicate is used to stop tracing _Pfc_ facts. Calling **pfcUntrace(P,M,C) **will stop all tracing specifications which match. The arguments default as described above.

_________________________________________________________________________________________

**pfcSpy(+Term)** \
**pfcSpy(+Term,+Mode)** \
**pfcSpy(+Term,+Mode,+Condition) **

These predicates set spypoints, of a sort.

_________________________________________________________________________________________



**- pfcQueue.**


<p id="gdcalert1" ><span style="color: red; font-weight: bold">>>>>>  gd2md-html alert: Definition term(s) &uarr;&uarr; missing definition? </span><br>(<a href="#">Back to top</a>)(<a href="#gdcalert2">Next alert</a>)<br><span style="color: red; font-weight: bold">>>>>> </span></p>






Displays the current queue of facts in the _Pfc_ queue.

_________________________________________________________________________________________



**- showState **


<p id="gdcalert2" ><span style="color: red; font-weight: bold">>>>>>  gd2md-html alert: Definition term(s) &uarr;&uarr; missing definition? </span><br>(<a href="#">Back to top</a>)(<a href="#gdcalert3">Next alert</a>)<br><span style="color: red; font-weight: bold">>>>>> </span></p>






Displays the state of Pfc, including the queue, all triggers, etc.

_________________________________________________________________________________________

**pfcFact(+P)** \
**pfcFacts(+L) **

pfcFact(P) unifies P with a fact that has been added to the database via _Pfc_?ou can backtrack into it to find more facts. pfcFacts(L) unified L with a list of all of the facts asserted by add.

_________________________________________________________________________________________

**pfcPrintDb** \
**pfcPrintFacts** \
**pfcPrintRules **

These predicates display the the entire _Pfc_ database (facts and rules) or just the facts or just the rules.

_________________________________________________________________________________________


### 4 Examples


#### 4.1 Factorial and Fibonacci

These examples show that the _Pfc_ backward chaining facility can do such standard examples as the factorial and Fibonacci functions.

Here is a simple example of a _Pfc_ backward chaining rule to compute the Fibonacci series.


```prolog
fib(0,1).
fib(1,1).
fib(N,M) <-
  N1 is N-1,
  N2 is N-2,
  fib(N1,M1),
  fib(N2,M2),
  M is M1+M2.
```


Here is a simple example of a _Pfc_ backward chaining rule to compute the factorial function.


```prolog
==> fact(0,1).
fact(N,M) <-
  N1 is N-1,
  fact(N1,M1),
  M is N*M1.
```



#### 4.2 Default Reasoning

This example shows how to define a default rule. Suppose we would like to have a default rule that holds in the absence of contradictory evidence. We might like to state, for example, that an we should assume that a bird can fly unless we know otherwise. This could be done as:


```prolog
bird(X), ~not(fly(X)) ==> fly(X).
```


We can, for our convenience, define a _default _operator which takes a _Pfc_ rule and qualifies it to make it a default rule. This can be done as follows:


```prolog
default((P ==> Q)),{pfcAtom(Q)} ==> (P, ~not(Q) ==> Q).
```


where **pfcAtom(X) **holds if **X **is a “logical atom” with respect to _Pfc_ (i.e . not a conjunction, disjunction, negation, etc).

One we have defined this, we can use it to state that birds fly by default, but penguins do not.


```prolog
% birds fly by default.
==> default((bird(X) ==> fly(X))).

isa(C1,C2) ==>
  % here's one way to do an isa hierarchy.
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 ==> P2).

==> isa(canary,bird).
==> isa(penguin,bird).

% penguins do not fly.
penguin(X) ==> not(fly(X)).

% chilly is a penguin.
==> penguin(chilly).

% tweety is a canary.
==> canary(tweety).
```



#### 4.3 KR example

isa hierarchy. roles. types. classification. etc.


#### 4.4 Maintaining Functional Dependencies

One useful thing that _Pfc_ can be used for is to automatically maintain function Dependencies in the light of a dynamic database of fact. The builtin truth maintenance system does much of this. However, it is often useful to do more. For example, suppose we want to maintain the constraint that a particular object can only be located in one place at a given time. We might record an objects location with an assertion **at(Obj,Loc) **which states that the current location of the object **Obj **is the location **Loc**.

Suppose we want to define a _Pfc_ rule which will be triggered whenever an **at/2 **assertion is made and will remove any previous assertion about the same object’s location. Thus to reflect that an object has moved from location A to location B, we need merely add the new information that it is at location B. If we try to do this with the _Pfc_ rule:


```prolog
at(Obj,Loc1),
at(Obj,Loc2),
{Loc1\==Loc2}
==>
~at(Obj,Loc1).
```


we may or may not get the desired result. This rule will in fact maintain the constraint that the database have at most one **at/2 **assertion for a given object, but whether the one kept is the old or the new depends on the particular search strategy being used by _Pfc_In fact, under the current default strategy, the new assertion will be the one retracted.

We can achieve the desired result with the following rule:


```prolog
at(Obj,NewLoc),
{at(Obj,OldLoc), OldLoc\==NewLoc}
  ==>
  ~at(Obj,OldLoc).
```


This rule causes the following behavior. Whenever a new assertion **at(O,L) **is made, a Prolog search is made for an assertion that object O is located at some other location. If one is found, then it is removed.

We can generalize on this rule to define a meta-predicate **function(P) **which states that the predicate whose name is **P **represents a function. That is, **P **names a relation of arity two whose first argument is the domain of the function and whose second argument is the function’s range. Whenever an assertion **P(X,Y) **is made, any old assertions matching **P(X,_) **are removed. Here is the _Pfc_ rule:


```prolog
function(P) ==>
  {P1 =.. [P,X,Y],
   P2 =.. [P,X,Z]},
  (P1,{P2,Y\==Z} ==> ~P2).
```


We can try this with the following results:


```prolog
| ?- add_PFC(function(age)).
Adding (u) function(age)
Adding age(A,B),{age(A,C),B\==C}==> ~age(A,C)
yes

| ?- add_PFC(age(john,30)).
Adding (u) age(john,30)
yes

| ?- add_PFC(age(john,31)).
Adding (u) age(john,31)
Removing age(john,30).
yes
```


Of course, this will only work for functions of exactly one argument, which in Prolog are represented as relations of arity two. We can further generalize to functions of any number of arguments (including zero), with the following rule:


```prolog

function(Name,Arity) ==>
  {functor(P1,Name,Arity),
   functor(P2,Name,Arity),
   arg(Arity,P1,PV1),
   arg(Arity,P2,PV2),
   N is Arity-1,
   merge(P1,P2,N)},
  (P1,{P2,PV1\==PV2} ==> ~P2).


merge(_,_,N) :- N<1.
merge(T1,T2,N) :-
  N>0,
  arg(N,T1,X),
  arg(N,T2,X),
  N1 is N-1,
  merge(T1,T2,N1).
```


The result is that adding the fact **function(P,N) **declares P to be the name of a relation of arity N such that only the most recent assertion of the form _P_(_a<sub>1</sub>,a<sub>2</sub>,…,a<sub>n-1</sub>,a<sub>n</sub>_) for a given set of constants _a<sub>1</sub>,…,a<sub>n-1</sub>_ will be in the database. The following examples show how we might use this to define a predicate **current_president/1 **that identifies the current U.S. president and **governor/3 **that relates state, a year and the name of its governor.


```prolog
% current_president(Name)
| ?- add_PFC(function(current_president,1)).
Adding (u) function(current_president,1)
Adding current_president(A),
       {current_president(B),A\==B}
==>
        ~current_president(B)
yes

| ?- add_PFC(current_president(reagan)).
Adding (u) current_president(reagan)
yes

| ?- add_PFC(current_president(bush)).
Adding (u) current_president(bush)
Removing current_president(reagan).
yes

% governor(State,Year,Governor)
| ?- add_PFC(function(governor,3)).
Adding (u) function(governor,3)
Adding governor(A,B,C),{governor(A,B,D),C\==D}==> ~governor(A,B,D)
yes

| ?- add_PFC(governor(pennsylvania,1986,thornburg)).
Adding (u) governor(pennsylvania,1986,thornburg)
yes

| ?- add_PFC(governor(pennsylvania,1987,casey)).
Adding (u) governor(pennsylvania,1987,casey)
yes

% oops, we misspelled thornburgh!
| ?- add_PFC(governor(pennsylvania,1986,thornburgh)).
Adding (u) governor(pennsylvania,1986,thornburgh)
Removing governor(pennsylvania,1986,thornburg).
yes
```



#### 4.5 Spreadsheets

One common kind of constraints is often found in spreadsheets in which one value is determined from a set of other values in which the size of the set can vary. This is typically found in spread sheets where one cell can be defined as the sum of a column of cells. This example shows how this kind of constraint can be defined in _Pfc_ as well. Suppose we have a relation **income/4 **which records a person’s income for a year by source. For example, we might have assertions like:


```prolog
income(smith,salary,1989,50000).
income(smith,interest,1989,500).
income(smith,dividends,1989,1200).
income(smith,consulting,1989,2000).
```


We might also with to have a relation **total_income/3 **which records a person’s total income for each year. Given the database above, this should be:


```prolog
total_income(smith,1989,53700).
```


One way to do this in _Pfc_ is as follows:


```prolog
income(Person,Source,Year,Dollars) ==> {increment_income(Person,Year,Dollars)}.

==> pfcUndoMethod(increment_income(P,Y,D),decrement_income(P,Y,D)).

increment_income(P,Y,D) :-
  (retract(total_income(P,Y,Old)) -> New is Old+D ; New = D),
  assert(total_income(P,Y,New)).

decrement_income(P,Y,D) :-
  retract(total_income(P,Y,Old)),
  New is Old-D,
  assert(total_income(P,Y,New)).
```


We would probably want to use the _Pfc_ rule for maintaining functional Dependencies described in Section [4.4](#bookmark=id.3rdcrjn) as well, adding the rule:


```prolog
==> function(income,4).
```



#### 4.6 Extended Reasoning Capability

The truth maintenance system in _Pfc_ makes it possible to do some reasoning that Prolog does not allow. From the facts

_p _? _q _ \
_p _? _r _ \
_q _? _r _

it follows that _r _is true. However, it is not possible to directly encode this in Prolog so that it can be proven. We can encode these facts in _Pfc_ and use a simple proof by contradiction strategy embodied in the following Prolog predicate:


```prolog
prove_by_contradiction(P) :- P.
prove_by_contradiction(P) :-
  \+ (not(P) ; P)
  add_PFC(not(P)),
    P       -> rem_PFC(not(P))
  otherwise -> (rem_PFC(not(P)),fail).
```


This procedure works as follows. In trying to prove P, succeed immediately if P is a know fact. Otherwise, providing that **not(P) **is not a know fact, add it as a fact and see if this gives rise to a proof for **(**P). if it did, then we have derived a contradiction from assuming that **not(P) **is true and **P **must be true. In any case, remove the temporary assertion **not(P)**.

In order to do the example above, we need to add the following rule or **or **and a rule for general implication (encoded using the infix operator **==¿**) which generates a regular forward chaining rule and its counterfactual rule.


```prolog
:- op(1050,xfx,('===>')).

(P ===> Q) ==>
  (P ==> Q),
  (not(Q) ==> not(P)).

or(P,Q) ==>
  (not(P) ==> Q),
  (not(Q) ==> P).
```


With this, we can encode the problem as:


```prolog
==> or(p,q).
==> (p ===> x).
==> (q ===> x).
```


When these facts are added, the following trace ensues:


```prolog
Adding (u) (A===>B)==>(A==>B), (not(B)==>not(A))
Adding (u) or(A,B)==>(not(A)==>B), (not(B)==>A)
Adding (u) or(p,q)
Adding not(p)==>q
Adding not(q)==>p
Adding (u) p===>x
Adding p==>x
Adding not(x)==>not(p)
Adding (u) q===>x
Adding q==>x
Adding not(x)==>not(q)
```


Then, we can call **prove_by_contradiction/1 **to show that **p **must be true:


```prolog
| ?- prove_by_contradiction(x).
Adding (u) not(x)
Adding not(p)
Adding q
Adding x
Adding not(q)
Adding p
Removing not(x).
Removing not(p).
Removing q.
Removing not(q).
Removing p.
Removing x.
yes
```



### References


    [1]   M. Genesereth et. al. _MRS Manual_. Technical Report, Stanford University, 1983.
    [2]   Rich Fritzson and Tim Finin. _Protem - An Integrated Expert SystemsTool_. Technical Report LBS Technical Memo Number 84, Unisys Paoli Research Center, May 1988.
    [3]   Drew McDermott. _DUCK: A Lisp-Based Deductive System_. Technical Report, Computer Science, Yale University, 1983.
    [4]   Drew McDermott and Eugene Charniak. _Introduction to Artificial Intelligence_. Addison Wesley, 1985.
    [5]   Nils Nilsson. _Principles of Artificial Intelligence_. Tioga Publishing Co., Palo Alto, California, 1980.
    [6]   Charles J. Petrie and Michael N. Huhns. _Controlling Forward Rule Inferences_. Technical Report ACA-AI-012-88, MCC, January 1988.


# Some TODOs

Document this pack!
Write tests
Untangle the 'pack' install deps
Still in progress (Moving predicates over here from logicmoo_base)


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and logicmoo
All rights reserved.

# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to logicmoo and Contribute directly !
Still, we wont stop you from doing it the Fork+PullRequest method


