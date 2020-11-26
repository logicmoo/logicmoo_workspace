Towards a Mathematical Science of

Computation

J. McCarthy

Computer Science Department

Stanford University

Stanford, CA 95305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

May 14, 1:20 p.m.

Abstract

Introduction

In this paper I shall discuss the prospects for a mathematical science of

computation. In a mathematical science, it is possible to deduce from the

basic assumptions, the important properties of the entities treated by the

science. Thus, from Newton’s law of gravitation and his laws of motion, one

can deduce that the planetary orbits obey Kepler’s laws.

What are the entities with which the science of computation deals?

What kinds of facts about these entities would we like to derive?

What are the basic assumptions from which we should start?

What important results have already been obtained? How can the math-

ematical science help in the solution of practical problems?

I would like to propose some partial answers to these questions. These

partial answers suggest some problems for future work. First I shall give

some very sketchy general answers to the questions. Then I shall present

some recent results on three speciﬁc questions. Finally, I shall try to draw

some conclusions about practical applications and problems for future work.

This paper should be considered together with my earlier paper [McC63].

The main results of the present paper are new but there is some overlap so

that this paper would be self-contained. However some of the topics in this

paper such as conditional expressions, recursive deﬁnition of functions, and

proof by recursion induction are considered in much greater detail in the

earlier paper.

2 What Are The Entities With Which Com-

puter Science Deals?

These are problems, procedures, data spaces, programs representing proce-

dures in particular programming languages, and computers.

Problems and procedures are often confused. A problem is deﬁned by the

criterion which determines whether a proposed solution is accepted. One can

understand a problem completely without having any method of solution.

Procedures are usually built up from elementary procedures. What these

elementary procedures may be, and how more complex procedures are con-

structed from them, is one of the ﬁrst topics in computer science. This

subject is not hard to understand since there is a precise notion of a com-

putable function to guide us, and computability relative to a given collection

of initial functions is easy to deﬁne.

Procedures operate on members of certain data spaces and produce mem-

bers of other data spaces, using in general still other data spaces as interme-

diates. A number of operations are known for constructing new data spaces

from simpler ones, but there is as yet no general theory of representable data

spaces comparable to the theory of computable functions. Some results are

given in [McC63].

Programs are symbolic expressions representing procedures. The same

procedure may be represented by diﬀerent programs in diﬀerent program-

ming languages. We shall discuss the problem of deﬁning a programming

language semantically by stating what procedures the programs represent.

As for the syntax of programming languages, the rules which allow us to

determine whether an expression belongs to the language have been formal-

ized, but the parts of the syntax which relate closely to the semantics have

not been so well studied. The problem of translating procedures from one

programming language to another has been much studied, and we shall try

to give a deﬁnition of the correctness of the translation.

Computers are ﬁnite automata. From our point of view, a computer is

deﬁned by the eﬀect of executing a program with given input on the state

of its memory and on its outputs. Computer science must study the various

ways elements of data spaces are represented in the memory of the computer

and how procedures are represented by computer programs. From this point

of view, most of the current work on automata theory is beside the point.

3 What Kinds of Facts About Problems, Pro-

cedures, data Spaces, Programs, And Com-

puters Would We Like to Derive?

Primarily. we would like to be able to prove that given procedures solve

given problems. However, proving this may involve proving a whole host of

other kinds of statement such as:

1. Two procedures are equivalent, i.e. compute the same function.

2. A number of computable functions satisfy a certain relationship, such

as an algebraic identity or a formula of the functional calculus.

3. A certain procedure terminates for certain initial data, or for all initial

data.

4. A certain translation procedure correctly translates procedures between

one programming language and another.

5. One procedure is more eﬃcient than another equivalent procedure in

the sense of taking fewer steps or requiring less memory.

6. A certain transformation of programs preserves the function expressed

but increases the eﬃciency.

7. A certain class of problems is unsolvable by any procedure, or requires

procedures of a certain type for its solution.

4 What Are The Axioms And Rules of Infer-

ence of A Mathematical Science of Compu-

tation?

Ideally we would like a mathematical theory in which every true statement

about procedures would have a proof, and preferably a proof that is easy to

ﬁnd, not too long, and easy to check. In 1931, G¨odel proved a result, one

of whose immediate consequences is that there is no complete mathemati-

cal theory of computation. Given any mathematical theory of computation

there are true statements expressible in it which do not have proofs. Never-

theless, we can hope for a theory which is adequate for practical purposes,

like proving that compilers work; the unprovable statements tend to be of a

rather sweeping character, such as that the system itself is consistent.

It is almost possible to take over one of the systems of elementary number

theory such as that given in Mostowski’s book ”Sentences Undecidable in

Formalized Arithmetic” since the content of a theory of computation is quite

similar. Unfortunately, this and similar systems were designed to make it easy

to prove meta-theorems about the system, rather than to prove theorems in

the system. As a result, the integers are given such a special role that the

proofs of quite easy statements about simple procedures would be extremely

long.

Therefore it is necessary to construct a new, though similar, theory in

which neither the integers nor any other domain, (e.g. strings of symbols)

are given a special role. Some partial results in this direction are described

in this paper. Namely, an integer-free formalism for describing computations

has been developed and shown to be adequate in the cases where it can be

compared with other formalisms. Some methods of proof have been devel-

oped, but there is still a gap when it comes to methods of proving that a

procedure terminates. The theory also requires extension in order to treat

the properties of data spaces.

5 What Important Results Have Been Ob-

tained Relevant to A Mathematical Science

of Computation?

In 1936 the notion of a computable function was clariﬁed by Turing, and

he showed the existence of universal computers that, with an appropriate

program, could compute anything computed by any other computer. All our

stored program computers, when provided with unlimited auxiliary storage,

are universal in Turing’s sense. In some subconscious sense even the sales

departments of computer manufacturers are aware of this, and they do not

advertise magic instructions that cannot be simulated on competitors ma-

chines, but only that their machines are faster, cheaper, have more memory,

or are easier to program.

The second major result was the existence of classes of unsolvable prob-

lems. This keeps all but the most ignorant of us out of certain Quixotic

enterprises such as trying to invent a debugging procedure that can infallibly

tell if a program being examined will get into a loop.

Later in this paper we shall discuss the relevance of the results of math-

ematical logic on creative sets to the problem of whether it is possible for a

machine to be as intelligent as a human. In my opinion it is very important

to build a ﬁrmer bridge between logic and recursive function theory on the

one side, and the practice of computation on the other.

Much of the work on the theory of ﬁnite automata has been motivated

by the hope of applying it to computation. I think this hope is mostly in

vain because the fact of ﬁniteness is used to show that the automaton will

eventually repeat a state. However, anyone who waits for an IBM 7090 to

repeat a state, solely because it is a ﬁnite automaton, is in for a very long

wait.

6 How Can A Mathematical Science of Com-

putation Help in The Solution of Practical

Problems?

Naturally, the most important applications of a science cannot be foreseen

when it is just beginning. However, the following applications can be fore-

seen.

1. At present, programming languages are constructed in a very unsys-

tematic way. A number of proposed features are invented, and then we argue

about whether each feature is worth its cost. A better understanding of the

structure of computations and of data spaces will make it easier to see what

features are really desirable.

2. It should be possible almost to eliminate debugging. Debugging is the

testing of a program on cases one hopes are typical, until it seems to work.

This hope is frequently vain.

Instead of debugging a program, one should prove that it meets its speci-

ﬁcations, and this proof should be checked by a computer program. For this

to be possible, formal systems are required in which it is easy to write proofs.

There is a good prospect of doing this, because we can require the computer

to do much more work in checking each step than a human is willing to do.

Therefore, the steps can be bigger than with present formal systems. The

prospects for this are discussed in [McC62].

7 Using Conditional Expressions to Deﬁne

Functions Recursively

In ordinary mathematical language, there are certain tools for deﬁning new

functions in terms of old ones. These tools are composition and the identiﬁ-

cation of variables. As it happens, these tools are inadequate computable in

terms of old ones. It is then customary to deﬁne all functions that can rea-

sonably be regarded as to give a verbal deﬁnition. For example, the function

|x| is usually deﬁned in words.

If we add a single formal tool, namely conditional expressions to our

mathematical notation, and if we allow conditional expressions to be used

recursively in a manner that I shall describe, we can deﬁne, in a completely

formal way, all functions that can reasonably be regarded as computable in

terms of an initial set. We will use the ALGOL 60 notation for conditional

expressions in this paper.

We shall use conditional expressions in the simple form

where p is a propositional expression whose value is true or false. The value

of the conditional expression is a if p has the value true and b if p has the

if p then a else b

value false. When conditional expressions are used, the stock of predicates

one has available for constructing p’s is just as important as the stock of

ordinary functions to be used in forming the a’s and b’s by composition. The

following are examples of functions deﬁned by conditional expressions:

1. |x| = if x < 0 then − x else x

2. n! = if n = 0 then 1 else n × (n − 1)!

3. n! = g(n, 1)

where

g(n, s) = if n = 0 then s else g(n − 1, n × s)

4. n! = f (n, 0, 1)

where

f (n, m, p) = if m = n then p else f (n, m + 1, (m + 1) × p)

5. n− = pred (n, 0)

where

pred (n, m) = if m(cid:48) = n then m else pred (n, m(cid:48))

6. m + n = if n = 0 then m else m(cid:48) + n−

The ﬁrst of these is the only non-recursive deﬁnition in the group. Next,

we have three diﬀerent procedures for computing n!; they can be shown to

be equivalent by the methods to be described in this paper. Then we deﬁne

the predecessor function n− for positive integers (3− = 2) in terms of the

successor function n(cid:48)(2(cid:48) = 3) Finally, we deﬁne addition in terms of the

successor, the predecessor and equality. In all of the deﬁnitions, except for

the ﬁrst, the domain of the variables is taken to be the set of non-negative

integers.

As an example of the use of these deﬁnitions, we shall show how to com-

pute 2! by the second deﬁnition of n!. We have

2! = g(2, 1)

= if 2 = 0 then 1 else g(2 − 1, 2 × 1)

= g(1, 2)

= if 1 = 0 then 2 else g(1 − 1, 1 × 2)

= g(0, 2)

= if 0 = 0 then 2 else g(0 − 1, 0 × 2)

= 2

Note that if we attempted to compute n! for any n but a non-negative

integer the successive reductions would not terminate. In such cases we say

that the computation does not converge. Some recursive functions converge

for all values of their arguments, some converge for some values of the argu-

ments, and some never converge. Functions that always converge are called

total functions, and the others are called partial functions. One of the ear-

liest major results of recursive function theory is that there is no formalism

that gives all the computable total functions and no partial functions.

We have proved [McC63] that the above method of deﬁning computable

functions, starting from just the successor function n(cid:48) and equality, yields all

the computable functions of integers. This leads us to assert that we have the

complete set of tools for deﬁning functions which are computable in terms of

given base functions.

If we examine the next to last line of our computation of 2! we see that

we cannot simply regard the conditional expression

if p then a else b

as a function of the three quantities p, a, and b.

If we did so, we would

be obliged to compute g(−1, 0) before evaluating the conditional expression,

and this computation would not converge. What must happen is that when

p is true we take a as the value of the conditional expression without even

looking at b.

Any reference to recursive functions in the rest of this paper refers to

functions deﬁned by the above methods.

8 Proving Statements About Recursive Func-

tions

In the previous section we presented a formalism for describing functions

which are computable in terms of given base functions. We would like to

have a mathematical theory strong enough to admit proofs of those facts

about computing procedures that one ordinarily needs to show that computer

programs meet their speciﬁcations. In [McC63] we showed that our formalism

for expressing computable functions, was strong enough so that all the partial

recursive functions of integers could be obtained from the successor function

and equality. Now we would like a theory strong enough so that the addition

of some form of Peano’s axioms would allow the proof of all the theorems of

one of the forms of elementary number theory.

We do not yet have such a theory. The diﬃculty is to keep the axioms

and rules of inference of the theory free of the integers or other special do-

main, just as we have succeeded in doing with the formalism for constructing

functions.

We shall now list the tools we have so far for proving relations between

recursive functions. They are discussed in more detail in [McC63].

1. Substitution of expressions for free variables in equations and other

relations.

2. Replacement of a sub-expression in a relation by an expression which

has been proved equal to the sub-expression. (This is known in elementary

mathematics as substitution of equals for equals.) When we are dealing with

conditional expressions, a stronger form of this rule than the usual one is

valid. Suppose we have an expression of the form

if p then a else (if q then b else c)

and we wish to replace b by d. Under these circumstances we do not have to

prove the equation b = d in general, but only the weaker statement

∼ p ∧ q ⊃ b = d

This is because b aﬀects the value of the conditional expression only in case

∼ p ∧ q is true.

3. Conditional expressions satisfy a number of identities, and a complete

theory of conditional expressions, very similar to propositional calculus, is

thoroughly treated in [McC63]. We shall list a few of the identities taken as

axioms here.

1. (if true then a else b) = a

2. (if false then a else b) = b

3. if p then (if q then a else b) else (if q then C else d) =

if q then (if p then a else c) else (if p then b else d)

4. f (if p then a else b) = if p then f (a) else f (b)

4. Finally, we have a rule called recursion induction for making arguments

that in the usual formulations are made by mathematical induction. This

rule may be regarded as taking one of the theorems of recursive function

theory and giving it the status of a rule of inference. Recursion induction

may be described as follows:

Suppose we have a recursion equation deﬁning a function f , namely

f (x1, . . . , xn) = ε{f, x1, . . . , xn}

(1)

where the right hand side will be a conditional expression in all non-trivial

cases, and suppose that

1. the calculation of f (x1, . . . , xn) according to this rule converges for a

certain set A of n-tuples,

2. the functions g(x1, . . . , xn) and h(x1, . . . , xn) each satisfy equation (1)

when g or h is substituted for f . Then we may conclude that g(x1, . . . , xn) =h(x1, . . . , xn) for all n-tuples (x1, . . . , xn) in the set A.

As an example of the use of recursion induction we shall prove that the

function g deﬁned by

g(n, s) = if n = 0 then s else g(n − 1, n × s)

satisﬁes g(n, s) = n! × s given the usual facts about n!. We shall take for

granted the fact that g(n, s) converges for all non-negative integral n and s.

Then we need only show that n! × s satisﬁes the equation for g(n, s). We

have

n! × s = if n = 0 then n! × s else n! × s

if n = 0 then s else (n − 1)! × (n × s)

and this has the same form as the equation satisﬁed by g(n, s). The steps in

this derivation are fairly obvious but also follow from the above-mentioned

axioms and rules of inference. In particular, the extended rule of replacement

is used. A number of additional examples of proof by recursion induction

are given in [McC63], and it has been used to prove some fairly complicated

relations between computable functions.

9 Recursive Functions, Flow Charts, And Al-

golic Programs

In this section 1 want to establish a relation between the use of recursive

functions to deﬁne computations, the ﬂow chart notation, and programs ex-

pressed as sequences of ALGOL-type assignment statements, together with

conditional go to’s. The latter we shall call Algolic programs with the idea

of later extending the notation and methods of proof to cover more of the

language of ALGOL. Remember that our purpose here is not to create yet

another programming language, but to give tools for proving facts about

programs.

In order to regard programs as recursive functions, we shall deﬁne the

state vector ξ of a program at a given time, to be the set of current assign-

ments of values to the variables of the program. In the case of a machine

language program, the state vector is the set of current contents of those reg-

isters whose contents change during the course of execution of the program.

When a block of program having a single entrance and exit is executed,

the eﬀect of this execution on the state vector may be described by a function

ξ (cid:48) = r(ξ) giving the new state vector in terms of the old. Consider a program

described by the ﬂow chart of ﬁg. 1.

r

s

t

f

p

g

q

Fig. 1 Flow chart for equations shown below.

r(ξ) = s(f (ξ))

s(ξ) = if p(ξ) then r(ξ) else t(g(ξ))

t(ξ) = if q1(ξ) then r(ξ) else if q2(ξ) ξ else t(g(ξ))

The two inner computation blocks are described by functions f (ξ) and

g(ξ), telling how these blocks aﬀect the state vector. The decision ovals

are described by predicates p(ξ), q1(ξ), and q2(ξ) that give the conditions for

taking the various paths. We wish to describe the function r(ξ) that gives the

eﬀect of the whole block, in terms of the functions f and g and the predicates

p, q1, and q2. This is done with the help of two auxiliary functions s and t.

s(ξ) describes the change in ξ between the point labelled s in the chart and

the ﬁnal exit from the block; t is deﬁned similarly with respect to the point

labelled t. We can read the following equations directly from the ﬂow chart:

r(ξ) = s(f (ξ))

s(ξ) = if p(ξ) then r(ξ) else t(g(ξ))

t(ξ) = if q1(ξ) then r(ξ) else if q2(ξ) then ξ else t(g(ξ))

In fact, these equations contain exactly the same information as does the

ﬂow chart.

Consider the function mentioned earlier, given by

g(n, s) = if n = 0 then s else g(n − 1, n × s)

It corresponds, as we leave the reader to convince himself, to the Algolic

program

if n = 0 then go to b;

s := n × s;

n := n − 1;

go to a;

a :

b :

Its ﬂow chart is given in ﬁg. 2.

This ﬂow chart is described by the function fac(ξ) and the equation

fac(ξ) = if p(ξ) then ξ else fac(g(f (ξ)))

where p(ξ) corresponds to the condition n = 0, f (ξ) to the statement n :=

n − 1, and g(ξ) to the statement s := n × s.

n = 0?

s := n*s

n := n - 1

Fig. 2 Flow chart for fac(ξ) as described by the following equations.

f ac(ξ) = if p(ξ) then ξ else f ac(g(f (ξ)))

where

and so

p(ξ) ≡ c(n, ξ) = 0,

f (ξ) = a(s, c(n, ξ) ∗ c(s, ξ), ξ),

g(ξ) = a(n, c(n, ξ) − 1, ξ)

f ac(ξ) = a(n, 0, a(s, c(n, ξ)! × c(s, ξ))).

We shall regard the state vector as having many components, but the only

components aﬀected by the present program are the s-component and the

n-component. In order to compute explicitly with the state vectors, we intro-

duce the function c(var , ξ) which denotes the value assigned to the variable

var in the state vector ξ, and we also introduce the function a(var , val , ξ)

which denotes the state vector that results when the value assigned to var in

ξ is changed to val , and the values of the other variables are left unchanged.

The predicates and functions of state vectors mentioned above then be-

come:

p(ξ) = (c(n, ξ) = 0)

g(ξ) = a(n, c(n, ξ) − 1, ξ)

f (ξ) = a(s, c(n, ξ) × c(s, ξ), ξ)

We can prove by recursion induction that

fac(ξ) = a(n, 0, a(s, c(n, ξ)! × c(s, ξ), ξ),

but in order to do so we must use the following facts about the basic state

vector functions:

1. c(u, a(v, α, ξ)) = if u = v then α else c(u, ξ)

2. a(v, c(v, ξ), ξ) = ξ

3. a(u, α, a(v, β, ξ)) = if u = v then a(u, α, ξ) else a(v, β, a(u, α, ξ))

The proof parallels the previous proof that

g(n, s) = n! × s,

but the formulae are longer.

While all ﬂow charts correspond immediately to recursive functions of

the state vector, the converse is not the case. The translation from recursive

function to ﬂow chart and hence to Algolic program is immediate, only if

the recursion equations are in iterative form. Suppose we have a recursion

equation

r(x1, . . . , xn) = E{r, x1, . . . , xn, f1, . . . , fm}

where E{r, x1, = ldots, xn, f1, . . . , fm} is a conditional expression deﬁning the

function r in terms of the functions f1, . . . , fm. E is said to be in iterative

form if r never occurs as the argument of a function but only in terms of the

main conditional expression in the form

. . . then r(. . .).

In the examples, all the recursive deﬁnitions except

n! = if n = 0 then 1 else n × (n − 1)!

In that one, (n − 1)! occurs as a term of a prod-

are in interative form.

uct. Non-iterative recursive functions translate into Algolic programs that

use themselves recursively as procedures. Numerical calculations can usually

be transformed into iterative form, but computations with symbolic expres-

sions usually cannot, unless quantities playing the role of push-down lists are

introduced explicitly.

10 Recursion Induction on Algolic Programs

In this section we extend the principle of recursion induction so that it can

be applied directly to Algolic programs without ﬁrst transforming them to

recursive functions. Consider the Algolic program

We want to prove it equivalent to the program

if n = 0 then go to b;

s := n × s;

n := n − 1;

go to a;

a :

b :

s := n! × s

n := 0

a :

b :

Before giving this proof we shall describe the principle of recursion induction

as it applies to a simple class of ﬂow charts. Suppose we have a block of

program represented by ﬁg. 3a. Suppose we have proved that this block is

equivalent to the ﬂow chart of ﬁg. 3b where the shaded block is the original

block again. This corresponds to the idea of a function satisfying a functional

equation. We may now conclude that the block of ﬁg. 3a is equivalent to the

ﬂow chart of ﬁg. 3c for those state vectors for which the program does not

get stuck in a loop in ﬁg. 3c.

(a)

f

(b)

p

(c)

p

f

g

g

Fig. 3 Recursion induction applied to ﬂow charts.

This can be seen as follows. Suppose that, for a particular input, the

program of ﬁg. 3c goes around the loop n times before it comes out. Consider

the ﬂow chart that results when the whole of ﬁg. 3b is substituted for the

shaded (or yellow) block, and then substituted into this ﬁgure again, etc. for

a total of n substitutions. The state vector in going through this ﬂow chart

will undergo exactly the same tests and changes as it does in going through

ﬁg. 3c, and therefore will come out in n steps without ever going through the

shaded (or yellow) block. Therefore, it must come out with the same value

as in ﬁg. 3c. Therefore, for any vectors for which the computation of ﬁg. 3c

converges, ﬁg. 3a is equivalent to ﬁg. 3c.

Thus, in order to use this principle to prove the equivalence of two blocks

we prove that they satisfy the same relation, of type 3a-3b, and that the

corresponding 3c converges.

We shall apply this method to showing that the two programs mentioned

above are equivalent. The ﬁrst program may be transformed as follows:

a :

if n = 0 then go to b; s := n × s; n := n − 1;

go to a : b :

a :

if n = 0 then go to b; s := n × s; n := n − 1;

a1 : if n = 0 then go to b; s := n × s; n := n − 1;

go to a1; b :

The operation used to transform the program is simply to write separately

the ﬁrst execution of a loop.

In the case of the second program we go:

a : s := n! × s; n := 0; b :

a :

if n = 0 then go to c; s := n! × s; n := 0;

go to b; c : s := n! × s; n := 0; b :

a :

if n = 0 then go to c; s := (n − 1)! × (n × s); n := 0;

go to b; c :; b :

from

to

from

to

to

to

a :

if n = 0 then go to b; s := n × s; n := n − 1; s :=

= n! × s; n := 0; b :

The operations used here are: ﬁrst, the introduction of a spurious branch,

after which the same action is performed regardless of which way the branch

goes; second, a simpliﬁcation based on the fact that if the branch goes one

way then n = 0, together with rewriting one of the right-hand sides of an as-

signment statement; and third, elimination of a label corresponding to a null

statement, and what might be called an anti-simpliﬁcation of a sequence of

assignment statements. We have not yet worked out a set of formal rules jus-

tifying these steps, but the rules can be obtained from the rules given above

for substitution, replacement and manipulation of conditional expressions.

The result of these transformations is to show that the two programs each

satisfy a relation of the form: program is equivalent to

a : if n = 0 then go to b; s := n × s; n := 0; program.

The program corresponding to ﬁg. 3c in this case, is precisely the ﬁrst

program which we shall assume always converges. This completes the proof.

11 The Description of Programming LanguagesProgramming languages must be described syntactically and semantically.

Here is the distinction. The syntactic description includes:

1. A description of the morphology, namely what symbolic expressions

represent grammatical programs.

2. The rules for the analysis of a program into parts and its synthesis

from the parts. Thus we must be able to recognize a term representing a

sum and to extract from it the terms representing the summands.

The semantic description of the language must tell what the programs

mean. The meaning of a program is its eﬀect on the state vector in the case

of a machine independent language, and its eﬀect on the contents of memory

in the case of a machine language program.

12 Abstract Syntax of Programming LanguagesThe Backus normal form that is used in the ALGOL report, describes the

morphology of ALGOL programs in a synthetic manner. Namely, it describes

how the various kinds of program are built up from their parts. This would

be better for translating into ALGOL than it is for the more usual problem

of translating from ALGOL. The form of syntax we shall now describe diﬀers

from the Backus normal form in two ways. First, it is analytic rather than

synthetic; it tells how to take a program apart, rather than how to put it

together. Second, it is abstract in that it is independent of the notation used

to represent, say sums, but only aﬃrms that they can be recognized and

taken apart.

Consider a fragment of the ALGOL arithmetic terms, including constants

and variables and sums and products. As a further simpliﬁcation we shall

consider sums and products as having exactly two arguments, although the

general case is not much more diﬃcult. Suppose we have a term t. From the

point of view of translating it we must ﬁrst determine whether it represents

a constant, a variable, a sum, or a product. Therefore we postulate the

existence of four predicates: isconst(t), isvar (t), issum(t) and isprod (t). We

shall suppose that each term satisﬁes exactly one of these predicates.

Consider the terms that are variables. A translator will have to be able

to tell whether two symbols are occurrences of the same variable, so we need

a predicate of equality on the space of variables. If the variables will have to

be sorted an ordering relation is also required.

Consider the sums. Our translator must be able to pick out the sum-

mands, and therefore we postulate the existence of functions addend (t) and

augend (t) deﬁned on those terms that satisfy issum(t). Similarly, we have

the functions mplier (t) and mpcand (t) deﬁned on the products. Since the

analysis of a term must terminate, the recursively deﬁned predicate

term(t) = isvar (t) ∨ isconst(t) ∨ issum(t) ∧ term

(addend (t)) ∧ term(augend (t)) ∨ isprod (t) ∧ term

(mplier (t)) ∧ term(mpcand (t))

must converge and have the value true for all terms.

The predicates and functions whose existence and relations deﬁne the

syntax, are precisely those needed to translate from the language, or to deﬁne

the semantics. That is why we need not care whether sums are represented

by a + b, or +ab, or (PLUS A B), or even by G¨odel numbers 7a11b.

It is useful to consider languages which have both an analytic and a syn-

thetic syntax satisfying certain relations. Continuing our example of terms,

the synthetic syntax is deﬁned by two functions mksum(t, u) and mkprod (t, u)

which, when given two terms t and u, yield their sum and product respec-

tively. We shall call the syntax regular if the analytic and the synthetic

syntax are related by the plausible relations:

1. issum(mksum(t, u)) and isprod (mkprod (t, u))

2. addend (mksum(t, u)) = t; mplier (mkprod (t, u)) = t

augend (mksum(t, u)) = u, mpcand (mkprod (t, u)) = u

3. issum(t) ⊃ mksum(addend (t), augend (t)) = t and

isprod (t) ⊃ mkprod (mplier (t), mpcand (t)) = t

Once the abstract syntax of a language has been decided, then one can

choose the domain of symbolic expressions to be used. Then one can deﬁne

the syntactic functions explicitly and satisfy one self, preferably by proving

it, that they satisfy the proper relations. If both the analytic and synthetic

functions are given, we do not have the diﬃcult and sometimes unsolvable

analysis problems that arise when languages are described synthetically only.

In ALGOL the relations between the analytic and the synthetic functions

are not quite regular, namely the relations hold only up to an equivalence.

Thus, redundant parentheses are allowed, etc.

13 Semantics

The analytic syntactic functions can be used to deﬁne the semantics of a

programming language.

In order to deﬁne the meaning of the arithmetic

terms described above, we need two more functions of a semantic nature,

one analytic and one synthetic.

If the term t is a constant then val (t) is

the number which t denotes. If α is a number mkconst(α) is the symbolic

expression denoting this number. We have the obvious relations

1. val (mkconst(α)) = α

2. isconst(mkconst(α))

3. isconst(t) ⊃ mkconst(val (t)) = t

Now we can deﬁne the meaning of terms by saying that the value of a

term for a state vector ξ is given by

value(t, ξ) = if isvar (t) then c(t, ξ) else if isconst(t)

then val (t) else if issum(t) then value (addend (t), ξ) +

+value(augend (t), ξ) else if isprod (t) then value(mplier (t),

ξ) × value(mpcand (t), ξ)

We can go farther and describe the meaning of a program in a program-

ming language as follows: The meaning of a program is deﬁned by its eﬀect

on the state vector. Admittedly, this deﬁnition will have to be elaborated to

take input and output into account.

In the case of ALGOL we should have a function

ξ (cid:48) = algol (π, ξ),

which gives the value ξ (cid:48) of the state vector after the ALGOL program π has

stopped, given that it was started at its beginning and that the state vector

was initially ξ. We expect to publish elsewhere a recursive description of the

meaning of a small subset of ALGOL.

Translation rules can also be described formally. Namely,

1. A machine is described by a function

x(cid:48) = machine(p, x)

giving the eﬀect of operating the machine program p on a machine vector x.

2. An invertible representation x = rep(ξ) of a state vector as a machine

3. A function p = trans(π) translating source programs into machine

vector is given.

programs is given.

The correctness of the translation is deﬁned by the equation

rep(algol (π, ξ)) = machine(trans(π), rep(ξ)).

It should be noted that trans(π) is an abstract function and not a machine

program. In order to describe compilers, we need another abstract invertible

function, u = repp(π), which gives a representation of the source program in

the machine memory ready to be translated. A compiler is then a machine

program such that trans(π) = machine(compiler , repp(π)).

14 The Limitations of Machines And Men as

Problem-Solvers

Can a man make a computer program that is as intelligent as he is? The

question has two parts. First, we ask whether it is possible in principle, in

view of the mathematical results on undecidability and incompleteness. The

second part is a question of the state of the programming art as it concerns

artiﬁcial intelligence. Even if the answer to the ﬁrst question is “no”, one

can still try to go as far as possible in solving problems by machine.

My guess is that there is no such limitation in principle. However, a

complete discussion involves the deeper parts of recursive function theory,

and the necessary mathematics has not all been developed.

Consider the problem of deciding whether a sentence of the lower pred-

icate calculus is a theorem. Many problems of actual mathematical or sci-

entiﬁc interest can be formulated in this form, and this problem has been

proved equivalent to many other problems including problem of determining

whether a program on a computer with unlimited memory will ever stop. Ac-

cording to Post, this is equivalent to deciding whether an integer is a member

of what Post calls a creative set. It was proved by Myhill that all creative

sets are equivalent in a quite strong sense, so that there is really only one

class of problems at this level of unsolvability.

Concerning this problem the following facts are known:

1) There is a procedure which will do the following: If the number is in

the creative set, the procedure will say “yes”, and if the number is not in the

creative set the procedure will not say “yes”, it may say “no” or it may run

on indeﬁnitely.

2) There is no procedure which will always say “yes” when the answer

is “yes”, and will always say “no” when the answer is “no” If a procedure

has property (1) it must sometimes run on indeﬁnitely. Thus there is no

procedure which can always decide deﬁnitely whether a number is a member

of a creative set, or equivalently, whether a sentence of the lower predicate

calculus is a theorem, or whether an ALGOL program with given input will

ever stop. This is the sense in which these problems are unsolvable.

3) Now we come to Post’s surprising result. We might try to do as well

as possible. Namely, we can try to ﬁnd a procedure that always says “yes”

when the answer is “yes”, and never says “yes” when the answer is “no”, and

which says “no” for as large a class of the negative cases as possible, thus

narrowing down the set of cases where the procedure goes on indeﬁnitely as

much as we can. Post showed that one cannot even do as well as possible.

Namely, Post gave a procedure which, when given any other partial decision

procedure, would give a better one. The better procedure decides all the

cases the ﬁrst procedure decided, and an inﬁnite class of new ones. At ﬁrst

sight this seems to suggest that Emil Post was more intelligent than any

possible machine, although Post himself modestly refrained from drawing this

conclusion. Whatever program you propose, Post can do better. However,

this is unfair to the programs. Namely, Post’s improvement procedure is

itself mechanical and can be carried out by machine, so that the machine can

improve its own procedure or can even improve Post, if given a description

of Post’s own methods of trying to decide sentences of the lower predicate

calculus. It is like a contest to see who can name the largest number. The

fact that I can add one to any number you give, does not prove that I am

better than you are.

4) However, the situation is more complicated than this. Obviously, the

improvement process may be carried out any ﬁnite number of times and a

little thought shows that the process can be carried out an inﬁnite number of

times. Namely, let p be the original procedure, and let Post’s improvement

procedure be called P , then P np represents the original procedure improved

n times. We can deﬁne P ωp as a procedure that applies p for a while, then

PD for a while, then p again, then Pp, then P 2p, then p, then Pp, then P 2p,

then P 3p, etc. This procedure will decide any problem that any P n will

decide, and so may justiﬁably be called P ωp. However, P ωp is itself subject

to Post’s original improvement process and the result may be called P ω+1p.

How far can this go? The answer is technical. Namely, given any recursive

transﬁnite ordinal α, one can deﬁne P αp. A recursive ordinal is a recursive

ordering of the integers that is a well-ordering in the sense that any subset of

the integers has a least member in the sense of the ordering. Thus, we have

a contest in trying to name the largest recursive ordinal. Here we seem to be

stuck, because the limit of the recursive ordinals is not recursive. However,

this does not exclude the possibility that there is a diﬀerent improvement

process Q, such that Qp is better than P αp for any recursive ordinal α.

5) There is yet another complication. Suppose someone names what he

claims is a large recursive ordinal. We, or our program, can name a larger

one by adding one, but how do we know that the procedure that he claims is

a recursive well-ordering, really is? He says he has proved it in some system

of arithmetic. In the ﬁrst place we may not be conﬁdent that his system of

arithmetic is correct or even consistent. But even if the system is correct,

by G¨odel’s theorem it is incomplete. In particular, there are recursive or-

dinals that cannot be proved to be so in that system. Rosenbloom, in his

book “Elements of Mathematical Logic” drew the conclusion that man was

in principle superior to machines because, given any formal system of arith-

metic, he could give a stronger system containing true and provable sentences

that could not be proved in the original system. What Rosenbloom missed,

presumably for sentimental reasons, is that the improvement procedure is

itself mechanical, and subject to iteration through the recursive ordinals, so

that we are back in the large ordinal race. Again we face the complication

of proving that our large ordinals are really ordinals.

6) These considerations have little practical signiﬁcance in their present

form. Namely, the original improvement process is such that the improved

process is likely to be too slow to get results in a reasonable time, whether

carried out by man or by machine. It may be possible, however, to put this

hierarchy in some usable form.

In conclusion, let me re-assert that the question of whether there are

limitations in principle of what problems man can make machines solve for

him as compared to his own ability to solve problems, really is a technical

question in recursive function theory.

References

[McC62] John McCarthy. Checking mathematical proofs by computer.

In Proceedings Symposium on Recursive Function Theory (1961).

American Mathematical Society, 1962.

[McC63] J. McCarthy. A basis for a mathematical theory of computation.1 In

P. Braﬀort and D. Hirschberg, editors, Computer Programming and

Formal Systems, pages 33–70. North-Holland, Amsterdam, 1963.

/@steam.stanford.edu:/u/jmc/s96/towards.tex: begun 1996

May 1, latexed 1996

May 14 at 1:20 p.m.

1http://www-formal.stanford.edu/jmc/basis/basis.html

