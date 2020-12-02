FIRST ORDER THEORIES OF

INDIVIDUAL CONCEPTS AND

PROPOSITIONS

John McCarthy, Stanford University

2000 Oct 31, 10:36 a.m.

Abstract

We discuss ﬁrst order theories in which individual concepts are

admitted as mathematical objects along with the things that reify

them. This allows very straightforward formalizations of knowledge,

belief, wanting, and necessity in ordinary ﬁrst order logic without

modal operators. Applications are given in philosophy and in artiﬁcial

intelligence. We do not treat general concepts, and we do not present

any full axiomatizations but rather show how various facts can be

expressed.

Introduction

“...it seems that hardly anybody proposes to use diﬀerent variables for

propositions and for truth-values, or diﬀerent variables for individuals

and individual concepts.”—(Carnap 1956, p. 113).

Admitting individual concepts as objects—with concept-valued con-

stants, variables, functions and expressions— allows ordinary ﬁrst or-

der theories of necessity, knowledge, belief and wanting without modal

operators or quotation marks and without the restrictions on substi-

tuting equals for equals that either device makes necessary.

In this paper we will show how various individual concepts and

propositions can be expressed. We are not yet ready to present a full

collection of axioms. Moreover, our purpose is not to explicate what

concepts are in a philosophical sense but rather to develop a language

of concepts for representing facts about knowledge, belief, etc. in the

memory of a computer.

Frege (1892) discussed the need to distinguish direct and indirect

use of words. According to one interpretation of Frege’s ideas, the

meaning of the phrase “Mike’s telephone number” in the sentence “Pat

knows Mike’s telephone number” is the concept of Mike’s telephone

number, whereas its meaning in the sentence “Pat dialed Mike’s tele-

phone number” is the number itself. Thus if we also have “Mary’s tele-

phone number = Mike’s telephone number”, then “Pat dialed Mary’s

telephone number” follows, but “Pat knows Mary’s telephone number

does not.

It was further proposed that a phrase has a sense which is a con-

cept and is its meaning in oblique contexts like knowing and wanting,

and a denotation which is its meaningin direct contexts like dialing.

Denotations are the basis of the semantics of ﬁrst order logic and

model theory and are well understood, but sense has given more trou-

ble, and the modal treatment of oblique contexts avoids the idea. On

the other hand, logicians such as Carnap (1947 and 1956), Church

(1951) and Montague (1974) see a need for concepts and have pro-

posed formalizations. All these formalizations involve modifying the

logic used; ours doesn’t modify the logic and is more powerful, be-

cause it includes mappings from objects to concepts. Robert Moore’s

forthcoming dissertation also uses concepts in ﬁrst order logic.

The problem identiﬁed by Frege—of suitably limiting the appli-

cation of the substitutitivity of equals for equals—arises in artiﬁcial

intelligence as well as in philosophy and linguistics for any system that

must represent information about beliefs, knowledge, desires, or logi-

cal necessity—regardless of whether the representation is declarative

or procedural (as in PLANNER and other AI formalisms).

Our approach involves treating concepts as one kind of object in

an ordinary ﬁrst order theory. We shall have one term that denotes

Mike’s telephone number and a diﬀerent term denoting the concept

of Mike’s telephone number instead of having a single term whose

denotation is the number and whose sense is a concept of it. The

relations among concepts and between concepts and other entities are

expressed by formulas of ﬁrst order logic. Ordinary model theory can

then be used to study what spaces of concepts satisfy various sets of

axioms.

We treat primarily what Carnap calls individual concepts

like

Mike’s telephone number or Pegasus and not general concepts like tele-

phone or unicorn. Extension to general concepts seems feasible, but

individual concepts provide enough food for thought for the present.

This is a preliminary paper in that we don’t give a comprehensive

set of axioms for concepts. Instead we merely translate some English

sentences into our formalism to give an idea of the possibilities.

2 Knowing What and Knowing That

To assert that Pat knows Mike’s telephone number we write

true Know(P at, T elephone M ike)

(1)

with the following conventions:

1. Parentheses are often omitted for one argument functions and

predicates. This purely syntactic convention is not important.

Another convention is to capitalize the ﬁrst letter of a constant,

variable or function name when its value is a concept. (We con-

sidered also capitalizing the last letter when the arguments are

concepts, but it made the formulas ugly).

2. M ike is the concept of Mike; i.e. it is the sense of the expression

“Mike”. mike is Mike himself.

3. T elephone is a function that takes a concept of a person into

a concept of his telephone number. We will also use telephone

which takes the person himself into the telephone number itself.

We do not propose to identify the function T elephone with the

general concept of a person’s telephone number.

4. If P is a person concept and X is another concept, then Know(P, X)

is an assertion concept or proposition meaning that P knows the

value of X. Thus in (1) Know(P at, T elephoneM ike) is a propo-

sition and not a truth value. Note that we are formalizing know-

ing what rather than knowing that or knowing how . For AI and

for other practical purposes, knowing what seems to be the most

useful notion of the three. In English, knowing what is written

knowing whether when the “knowand” is a proposition.

5. It is often convenient to write know(pat, T elephone M ike) in-

stead of

true Know(P at, T elephoneM ike)

when we don’t intend to iterate knowledge further. know is a

predicate in the logic, so we cannot apply any knowledge opera-

tors to it. We will have

know(pat, T elephone M ike) ≡ true Know(P at, T elephone M ike).(2)6. We expect that the proposition Know(P at, T elphone M ike) will

be useful accompanied by axioms that allow inferring that Pat

will use this knowledge under appropriate circumstances, i.e. he

will dial it or retell it when appropriate. There will also be

axioms asserting that he will know it after being told it or looking

it up in the telephone book.

7. While the sentence “Pat knows Mike” is in common use, it is

harder to see how Know(P at, M ike) is to be used and axiom-

atized.

I suspect that new methods will be required to treat

knowing a person.

8. true Q is the truth value, t or f , of the proposition Q, and we

must write true Q in order to assert Q. Later we will consider

formalisms in which true has a another argument—a situation,

a story, a possible world, or even a partial possible world (a

notion we suspect will eventually be found necessary).

9. The formulas are in a sorted ﬁrst order logic with functions and

equality. Knowledge, necessity, etc. will be discussed without

extending the logic in any way—solely by the introduction of

predicate and function symbols subject to suitable axioms. In

the present informal treatment, we will not be explicit about

sorts, but we will use diﬀerent letters for variables of diﬀerent

sorts.

The reader may be nervous about what is meant by concept. He

will have to remain nervous; no ﬁnal commitment will be made in this

paper. The formalism is compatible with many possibilities, and these

can be compared using the models of their ﬁrst order theories. Actu-

ally, this paper isn’t much motivated by the philosophical question of

what concepts really are. The goal is more to make a formal structure

that can be used to represent facts about knowledge and belief so that

a computer program can reason about who has what knowledge in

order to solve problems. From either the philosophical or the AI point

of view, however, if (1) is to be reasonable, it must not follow from

(1) and the fact that Mary’s telephone number is the same as Mike’s,

that Pat knows Mary’s telephone number.

The proposition that Joe knows whether Pat knows Mike’s tele-

phone number, is written

Know(J oe, Know(P at, T elephone M ike))

and asserting it requires writing

true Know(J oe, Know(P at, T elephone M ike))

while the proposition that Joe knows that Pat knows Mike’s telephone

number is written

K(J oe, Know(P at, T elephone M ike)

where K(P, Q) is the proposition that P knows that Q. English does

not treat knowing a proposition and knowing an individual concept

uniformly; knowing an individual concept means knowing its value

while knowing a proposition means knowing that it has a particular

value, namely t. There is no reason to impose this inﬁrmity on robots.

We ﬁrst consider systems in which corresponding to each concept

X, there is a thing x of which X is a concept. Then there is a function

denot such that

x = denot X.

(3)

(4)

(5)

(6)

We call denotX the denotation of the concept X, and (7) asserts that

the denotation of the concept of P ’s telephone number depends only

on the denotation of the concept P . The variables in (7) range over

concepts of persons, and we regard (7) as asserting that T elephone

is extensional with respect to denot. Note that our denot operates

on concepts rather than on expressions; a theory of expressions will

also need a denotation function. From (7) and suitable logical axioms

follows the existence of a function telephone satisfying

(∀P )(denot T elephone P = telephone denot P ).

(8)

Functions like T elephone are then related to denot by equations like

(∀P 1 P 2)(denot P 1 = denot P 2 ⊃ denot T elephone P 1 = denot T elephone P 2).(7)Know is extensional with respect to denot in its ﬁrst argument,

and this is expressed by

(∀P 1 P 2)(denot P 1 = denot P 2 ⊃ denot Know(P 1, X) = denot Know(P 2, X)),(9)(∀P 1 P 2 x u)

(denotes(P 1, x) ∧ denotes(P 2, x) ∧ denotes(T elephone P 1, u)⊃ denotes(T elephone P 2, u))

(12)but it is Not extensional in its second argument. We can therefore

deﬁne a predicate know(p, X) satisfying

(∀P X)(true Know(P, X) ≡ know(denot P, X)).

(10)

(Note that all these predicates and functions are entirely extensional

in the underlying logic, and the notion of extensionality presented here

is relative to denot.)

The predicate true and the function denot are related by

(∀Q)(true Q ≡ (denot Q = t))

(11)

provided truth values are in the range of denot, and denot could also

be provided with a (partial) possible world argument.

When we don’t assume that all concepts have denotations, we use

a predicate denotes(X, x) instead of a function. The extensionality of

T elephone would then be written

We now introduce the function Exists satisfying

(∀X)(true Exists X ≡ (∃x)denotes(X, x))

(13)

Suppose we want to assert that Pegasus is a horse without asserting

that Pegasus exists. We can do this by introducing the predicate

Ishorse and writing

true Ishorse P egasus

(14)

which is related to the predicate ishorse by

(∀X x)(denotes(X, x) ⊃ (ishorse x ≡ true Ishorse X))

(15)

In this way, we assert extensionality without assuming that all con-

cepts have denotations. Exists is extensional in this sense, but the

corresponding predicate exists is identically true and therefore dis-

pensable.

In order to combine concepts propositionally, we need analogs of

the propositional operators such as ∧, etc. which we will write And,

etc., write as inﬁxes, and axiomatize by

(true(Q1 And Q2) ≡ true Q1 ∧ true Q2),

etc. The corresponding formulas for Or, N ot, Implies, and Equiv are

(∀Q1 Q2)(true(Q1 Or Q2) ≡ true Q1 ∨ true Q2),

(true(N ot Q) ≡ ¬true Q),

(true(Q1 Implies Q2) ≡ trueQ1 ⊃ true Q2)

(true(Q1 Equiv Q2) ≡ (true Q1 ≡ true Q2)).

(20)

The equality symbol “=” is part of the logic so that X = Y asserts

that X and Y are the same concept. To write propositions express-

ing equality of the denotations of concepts, we introduce Equal(X, Y )

which is the proposition that X and Y denote the same thing if any-

thing. We shall want axioms 1

(∀X)(true Equal(X, X)),

(∀X Y )(true Equal(X, Y ) ≡ true Equal(Y, X))

(16)

(17)

(18)

(19)

(21)

(22)

and

and

(∀X Y Z)(true Equal(X, Y )∧true Equal(Y, Z) ⊃ true Equal(X, Z)),(23)making true Equal(X, Y ) an equivalence relation, and

(∀X Y x)(true Equal(X, Y )∧denotes(X, x) ⊃ denotes(Y, x)),(24)

which relates it to equality in the logic.

We can make the concept of equality essentially symmetric by

replacing (22) by

(∀X Y )(Equal(X, Y ) = Equal(Y, X)),

(25)

i.e. making the two expressions denote the same concept.

11995: I should have used an inﬁxed Equal here.

The statement that Mary has the same telephone as Mike is as-

serted by

true Equal(T elephone M ary, T elephone M ike)

(26)

and it obviously doesn’t follow from this and (1) that

true Know(P at, T elephone M ary)

(27)

To draw this conclusion we need something like

true K(P at, Equal(T elephone M ary, T elephone M ike))

(28)

and suitable axioms about knowledge.

If we were to adopt the convention that a proposition appearing

at the outer level of a sentence is asserted and were to regard the

denotation-valued function as standing for the sense-valued function

when it appears as the second argument of Know, we would have a

notation that resembles ordinary language in handling obliquity en-

tirely by context. There is no guarantee that general statements could

be expressed unambiguously without circumlocution; the fact that the

principles of intensional reasoning haven’t yet been stated is evidence

against the suitability of ordinary language for stating them.

3 Functions from Things to Concepts

of them

While the relation denotes(X, x) between concepts and things is many-

one, functions going from things to certain concepts of them seem use-

ful. Some things such as numbers can be regarded as having standard

concepts. Suppose that Concept1 n gives a standard concept of the

number n, so that

(∀n)(denot Concept1 n = n)

We can then have simultaneously

true N ot Knew(Kepler, N umber P lanets)

(29)

(30)

and

true Knew(Kepler, Composite Concept1 denot N umber P lanets).(31)

(We have bravely used Knew instead of Know, because we are not

now concerned with formalizing tense.)

(31) can be condensed us-

ing Composite1 which takes a number into the proposition that it is

composite, i.e.

(∀n)(Composite1 n = Composite Concept1 n),

(32)

getting

true Knew(Kepler, Composite1 denot N umber P lanets). (33)

A further condensation can be achieved using Composite2 deﬁned by

(∀N )(Composite2 N = Composite Concept1 denot N ),

(34)

letting us write

which is true even though

true Knew(Kepler, Composite2 N umber P lanets),

(35)

true Knew(Kepler, Composite N umber P lanets)

(36)

is false. (36) is our formal expression of “Kepler knew that the number

of planets is composite”, while (31), (33), and (35) each expresses a

proposition that can only be stated awkwardly in English, e.g. as

“Kepler knew that a certain number is composite, where this number

(perhaps unbeknownst to Kepler) is the number of planets”.

We may also want a map from things to concepts of them in or-

der to formalize a sentence like, “Lassie knows the location of all her

puppies”. We write this

Here Conceptd takes a puppy into a dog’s concept of it, and Locationd

takes a dog’s concept of a puppy into a dog’s concept of its location.

The axioms satisﬁed by Knowd, Locationd and Conceptd can be tai-

lored to our ideas of what dogs know.

A suitable collection of functions from things to concepts might

permit a language that omitted some individual concepts like M ike

(replacing it with Conceptx mike) and wrote many sentences with

quantiﬁers over things rather than over concepts. However, it is still

premature to apply Occam’s razor. It may be possible to avoid con-

cepts as objects in expressing particular facts but impossible to avoid

them in stating general principles.

(∀x)(ispuppy(x, lassie) ⊃ true Knowd(Lassie, Locationd Conceptd x)).(37)4 Relations between Knowing What

and Knowing That

As mentioned before, “Pat knows Mike’s telephone number” is written

true Know(P at, T elephone M ike).

(38)

We can write “Pat knows Mike’s telephone number is 333-3333”

trueK(P at, Equal(T elephone M ike, Concept1 “333−3333(cid:48)(cid:48))),(39)

where K(P, Q) is the proposition that denot(P ) knows the proposi-

tion Q and Concept1(“333 − 3333(cid:48)(cid:48)) is some standard concept of that

telephone number.

The two ways of expressing knowledge are somewhat interdeﬁn-

able, since we can write

(∀P Q)(K(P, Q) = (Q And Know(P, Q)))

(40)

and

(41) by

(∀P X)(true Know(P, X) ≡ (∃A)(constant A∧true K(P, Equal(X, A)))).(41)Here constant A asserts that A is a constant, i.e. a concept such that

we are willing to say that P knows X if he knows it equals A. This is

clear enough for some domains like integers, but it is not obvious how

to treat knowing a person.

Using the standard concept function Concept1, we might replace

(∀P X(true Know(P, X) ≡ (∃a)(true K(P, Equal(X, Concept1 a))))(42)with similar meaning.2

(41) and (42) express a denotational deﬁnition of Know in terms

of K. A conceptual deﬁnition seems to require something like

(∀P X)(Know(P, X) = Exists X And K(P, Equal(X, Concept2 denot X))),(43)where Concept2 is a suitable function from things to concepts and

may not be available for all sorts of objects.3

21995: This idea is used in my Elephant 2000 paper to discuss the notion of a responsive

answer to a question.

31995: At present I don’t see why Concept2 needs to be diﬀerent from Concept1.

5 Replacing Modal Operators by Modal

Functions

Using concepts we can translate the content of modal logic into or-

dinary logic. We need only replace the modal operators by modal

functions. The axioms of modal logic then translate into ordinary

ﬁrst order axioms.

In this section we will treat only unquantiﬁed

modal logic. The arguments of the modal functions will not involve

quantiﬁcation although quantiﬁcation occurs in the outer logic.

N ec Q is the proposition that the proposition Q is necessary, and

P oss Q is the proposition that it is possible. To assert necessity or

possibility we must write true N ec Q or true P oss Q. This can be

abbreviated by deﬁning nec Q ≡ true N ec Q and poss Q correspond-

ingly. However, since nec is a predicate in the logic with t and f as

values, nec Q cannot be an argument of nec or N ec.

Before we even get to modal logic proper we have a decision to

make—shall N ot N ot Q be considered the same proposition as Q, or

is it merely extensionally equivalent? The ﬁrst is written

(∀Q)(N ot N ot Q = Q)

and the second

(∀Q)(true N ot N ot Q ≡ true Q).

(44)

(45)

The second follows from the ﬁrst by substitution of equals for equals,

but the converse needn’t hold.

In Meaning and Necessity, Carnap takes what amounts to the ﬁrst

alternative, regarding concepts as L-equivalence classes of expressions.

This works nicely for discussing necessity, but when he wants to discuss

knowledge without assuming that everyone knows Fermat’s last theo-

rem if it is true, he introduces the notion of intensional isomorphism

and has knowledge operate on the equivalence classes of this relation.

If we choose the ﬁrst alternative, then we may go on to identify any

two propositions that can be transformed into each other by Boolean

identities. This can be assured by a small collection of propositional

identities like (44) including associative and distributive laws for con-

junction and disjunction, De Morgan’s law, and the laws governing

the propositions T and F . In the second alternative we will want the

extensional forms of the same laws. When we get to quantiﬁcation

a similar choice will arise, but if we choose the ﬁrst alternative, it

will be undecideable whether two expressions denote the same con-

cept. I doubt that considerations of linguistic usage or usefulness in

AI will unequivocally recommend one alternative, so both will have to

be studied.

Actually there are more than two alternatives. Let M be the free

algebra built up from the “atomic” concepts by the concept forming

function symbols. If ≡≡ is an equivalence relation on M such that

(∀X1 X2)((X1 ≡≡ X2) ⊃ (true X1 ≡ true X2)),

(46)

then the set of equivalence classes under ≡≡ may be taken as the set

of concepts.

Similar possibilities arise in modal logic. We can choose between

the conceptual identity

(∀W )(P oss Q = N ot N ec N ot Q)

and the weaker extensional axiom

(∀Q)(true P oss Q ≡ true N ot N ec N ot Q).

We will write the rest of our modal axioms in extensional form.

We have

and

(∀Q)(true N ec Q ⊃ true Q)

(∀Q1 Q2)(true N ec Q1∧true N ec(Q1 Implies Q2) ⊃ true N ec Q2)(50)yielding a system equivalent to von Wright’s T.4

S4 is given by adding

(∀Q)(true N ec Q ≡ true N ec N ec Q)

and S5 by adding

(∀Q)(true P oss Q ≡ true N ec P oss Q).

(47)

(48)

(49)

(51)

(52)

4It seems that something to replace necessitation is needed to get T and likewise for

S4 and S5.

Actually, there may be no need to commit ourselves to a particular

modal system. We can simultaneously have the functions N ecT , N ec4

and N ec5, related by axioms such as

(∀Q)(true N ec4 Q ⊃ true N ec5 Q),

(53)

which would seem plausible if we regard S4 as corresponding to prov-

ability in some system and S5 as truth in the intended model of the

system.

Presumably we shall want to relate necessity and equality by the

axiom

(∀X)(true N ec Equal(X, X)).

Certain of Carnap’s proposals translate to the stronger relation

(∀X Y )(X = Y ≡ true N ec Equal(X, Y )),

which asserts that two concepts are the same if and only if the equality

of what they may denote is necessary.

(54)

(55)

6 More Philosophical Examples—Mostly

Well Known

Some sentences that recur as examples in the philosophical literature

will be expressed in our notation so the treatments can be compared.

First we have “The number of planets = 9” and “Necessarily 9 =

9” from which one doesn’t want to deduce “Necessarily the number of

planets = 9”. This example is discussed by Quine (1961) and (Kaplan

1969). Consider the sentences

¬nec Equal(N umber P lanets, Concept1 9)

(56)

and

nec Equal(Concept1 number planets, Concept1 9)

(57)

Both are true. (56) asserts that it is not necessary that the number

of planets be 9, and (57) asserts that the number of planets, once

determined, is a number that is necessarily equal to 9. It is a major

virtue of our formalism that both meanings can be expressed and

are readily distinguished. Substitutivity of equals holds in the logic

but causes no trouble, because “The number of planets = 9” may be

written

number(planets) = 9,

or, using concepts, as

true Equal(N umber P lanets, Concept1 9),

and “Necessarily 9=9” is written

nec Equal(Concept1 9, Concept1 9),

and these don’t yield the unwanted conclusion.

Ryle used the sentences “Baldwin is a statesman” and “Pickwick

is a ﬁction” to illustrate that parallel sentence construction does not

always give parallel sense. The ﬁrst can be rendered in four ways,

namely true Statesman Baldwin or statesman denot Baldwin or

statesman baldwin or statesman1 Baldwin where the last asserts

that the concept of Baldwin is one of a statesman. The second can be

rendered only as as true F iction P ickwick or f iction1 P ickwick.

Quine (1961) considers illegitimate the sentence

(∃x)(Philip is unaware that x denounced Catiline)

(61)

obtained from “Philip is unaware that Tully denounced Catiline” by

existential generalization. In the example, we are also supposing the

truth of “Philip is aware that Cicero denounced Catiline”. These sen-

tences are related to (perhaps even explicated by) several sentences

in our system. T ully and Cicero are taken as distinct concepts. The

person is called tully or cicero in our language, and we have

tully = cicero,

denot T ully = cicero

and

denot Cicero = cicero.

We can discuss Philip’s concept of the person Tully by introducing

a function Concept2(p1, p2) giving for some persons p1 and p2, p1’s

(58)

(59)

(60)

(62)

(63)

(64)

(∃P )(true Denounced(P, Catiline) And N ot K(P hilip, Denounced(P, Catiline)))(68)concept of p2. Such a function need not be unique or always deﬁned,

but in the present case, some of our information may be conveniently

expressed by

Concept2(philip, tully) = Cicero,

(65)

asserting that Philip’s concept of the person Tully is Cicero. The

basic assumptions of Quine’s example also include

true K(P hilip, Denounced(Cicero, Catiline))

(66)

¬true K(P hilip, Denounced(T ully, Catiline)).

(67)

5 From (63), . . ., (67) we can deduce

from (67) and others, and

¬(∃p)(denounced(p, catiline)

∧

¬true K(P hilip, Denounced(Concept2(philip, p), Catiline))),(69)

using the additional hypotheses

(∀p)(denounced(p, catiline) ⊃ p = cicero),

denot Catiline = catiline

(70)

(71)

and

and

(∀P 1 P 2)(denot Denounced(P 1, P 2) ≡ denounced(denot P 1, denot P 2)).(72)Presumably (68) is always true, because we can always construct a

concept whose denotation is Cicero unbeknownst to Philip. The truth

of (69) depends on Philip’s knowing that someone denounced Catiline

and on the map Concept2(p1, p2) that gives one person’s concept of

another. If we refrain from using a silly map that gives something like

51995: Quine would also want true N ot K(P hilip, Denounced(T ully, Catiline)).

Denouncer(Catiline) as its value, we can get results that correspond

to intuition.

The following sentence attributed to Russell is is discussed by Ka-

plan: “I thought that your yacht was longer than it is”. We can write

it

true Believed(I, Greater(Length Y ouryacht,

Concept1 denot Length Y ouryacht)),

(73)

where we are not analyzing the pronouns or the tense, but are us-

ing denot to get the actual length of the yacht and Concept1 to get

back a concept of this true length so as to end up with a proposition

that the length of the yacht is greater than that number. This looks

problematical, but if it is consistent, it is probably useful.

In order to express “Your yacht is longer than Peter thinks it is.”,

we need the expression Denot(P eter, X) giving a concept of what

Peter thinks the value of X is. We now write

longer(youryacht, denot Denot(P eter, Length Y ouryacht)),(74)

but I am not certain this is a correct translation.

Quine (1956) discusses an example in which Ralph sees Bernard J.

Ortcutt skulking about and concludes that he is a spy, and also sees

him on the beach, but doesn’t recognize him as the same person. The

facts can be expresed in our formalism by equations

trueBelieve(Ralph, Isspy P 1)

and

true Believe(Ralph, N ot Issp P 2)

(75)

(76)

where P 1 and P 2 are concepts satisfying denotP 1 = ortcutt and

denotP 2 = ortcutt. P 1 and P 2 are further described by sentences

relating them to the circumstances under which Ralph formed them.

We can still consider a simple sentence involving the persons as

things—write it believespy(ralph, ortcutt), where we deﬁne

(∀p1 p2)(believespy(p1, p2) ≡ true Believe(Concept1 p1, Isspy Concept7 p2))(77)using suitable mappings Concept1 and Concept7 from persons to con-

cepts of persons. We might also choose to deﬁne believespy in such

a way that it requires true Believe(Concept1 p1, Isspy P ) for several

concepts P of p2, e.g. the concepts arising from all p1’s encounters

with p2 or his name. In this case

believespy(ralph, ortcutt)

will be false and so would a corresponding

notbelievespy(ralph, ortcutt)

. However, the simple-minded predicate believespy, suitably deﬁned,

may be quite useful for expressing the facts necessary to predict some-

one’s behavior in simpler circumstances.

Regarded as an attempt to explicate the sentence “Ralph believes

Ortcutt is a spy”, the above may be considered rather tenuous. How-

ever, we are proposing it as a notation for expressing Ralph’s beliefs

about Ortcutt so that correct conclusions may be drawn about Ralph’s

future actions. For this it seems to be adequate.

7 Propositions Expressing Quantiﬁca-

tion

As the examples of the previous sections have shown, admitting con-

cepts as objects and introducing standard concept functions makes

“quantifying in” rather easy. However, forming propositions and in-

dividual concepts by quantiﬁcation requires new ideas and additional

formalism. We are not very conﬁdent of the approach presented here.

We want to continue describing concepts within ﬁrst order logic

with no logical extensions. Therefore, in order to form new concepts

by quantiﬁcation and description, we introduce functions All, Exist,

and T he such that All(V, P ) is (approximately) the proposition that

“for all values of V , P is true”, Exist(V, P ) is the corresponding

existential proposition, and T he(V, P ) is the concept of “the V such

that P ”.

Since All is to be a function, V and P must be objects in the logic.

However, V is semantically a variable in the formation of All(V, P ),

etc., and we will call such objects inner variables so as to distinguish

them from variables in the logic. We will use V , sometimes with

subscripts, for a logical variable ranging over inner variables. We also

need some constant symbols for inner variables (got that?), and we

will use doubled letters, sometimes with subscripts, for these. XX

will be used for individual concepts, P P for persons, and QQ for

propositions.

The second argument of All and friends is a “proposition with

variables in it”, but remember that these variables are inner variables

which are constants in the logic. Got that? We won’t introduce a

special term for them, but will generally allow concepts to include

inner variables. Thus concepts now include inner variables like XX

and P P , and concept forming functions like T elephone and Know

take as arguments concepts containing internal variables in addition

to the usual concepts.

Thus

Child(M ike, P P ) Implies Equal(T elephone P P, T elephone M ike)(78)is a proposition with the inner variable P P in it to the eﬀect that

if P P is a child of Mike, then his telephone number is the same as

Mike’s, and

All(P P, Child(M ike, P P ) Implies Equal(T elephone P P, T elephone M ike))(79)is the proposition that all Mike’s children have the same telephone

number as Mike. Existential propositions are formed similarly to uni-

versal ones, but the function Exist introduced here should not be

confused with the function Exists applied to individual concepts in-

troduced earlier.

In forming individual concepts by the description function T he, it

doesn’t matter whether the object described exists. Thus

T he(P P, Child(M ike, P P ))

(80)

is the concept of Mike’s only child. Exists T he(P P, Child(M ike, P P ))

is the proposition that the described child exists. We have

true Exists T he(P P, Child(M ike, P P ))

≡ true Exist(P P, Child(M ike, P P )

And All(P P 1, Child(M ike, P P 1) Implies Equal(P P, P P 1)))),(81)but we may want the equality of the two propositions, i.e.

Exists T he(P P, Child(M ike, P P ))

= Exist(P P, Child(M ike, P P )

And All(P P 1, Child(M ike, P P 1) Implies Equal(P P, P P 1))).(82)

This is part of general problem of when two logically equivalent con-

cepts are to be regarded as the same.

In order to discuss the truth of propositions and the denotation

of descriptions, we introduce possible worlds reluctantly and with an

important diﬀerence from the usual treatment. We need them to give

values to the inner variables, and we can also use them for axioma-

tizing the modal operators, knowledge, belief and tense. However, for

axiomatizing quantiﬁcation, we also need a function α such that

π(cid:48) = α(V, x, π)

(83)

is the possible world that is the same as the world π except that

the inner variable V has the value x instead of the value it has in

π. In this respect our possible worlds resemble the state vectors or

environments of computer science more than the possible worlds of the

Kripke treatment of modal logic. This Cartesian product structure on

the space of possible worlds can also be used to treat counterfactual

conditional sentences. 6

Let π0 be the actual world. Let true(P, π) mean that the proposi-

tion P is true in the possible world π. Then

(∀P )(true P ≡ true(P, π0)).

(84)

Let denotes(X, x, π) mean that X denotes x in π, and let denot(X, π)

mean the denotation of X in π when that is deﬁned.

The truth condition for All(V, P ) is then given by

(∀πV P )(true(All(V, P ), π) ≡ (∀x)true(P, α(V, x, π)).

(85)

Here V ranges over inner variables, P ranges over propositions, and x

ranges over things. There seems to be no harm in making the domain

of x depend on π. Similarly

(∀πV P )(true(Exist(V, P ), π) ≡ (∃x)true(P, α(V, x, π)).

(86)

61995: (McCarthy 1979) treats “Cartesian counterfactuals”.

The meaning of T he(V, P ) is given by

(∀πV P x)(true(P, α(V, x, π)) ∧ (∀y)(true(P, α(V, y, π)) ⊃ y = x)

(87)

⊃ denotes(T he(V, P ), x, π))

(∀π V P )(¬(∃x)(true(P, α(V, x, π)) ⊃ ¬true Exists T he(V, P ))).(88)

We also have the following syntactic rules governing propositions

involving quantiﬁcation:

(∀π Q1 Q2 V )(absent(V, Q1) ∧ true(All(V, Q1ImpliesQ2), π)

(89)

⊃ true(Q1ImpliesAll(V, Q2), π))

and

and

(∀π V Q X)(true(All(V, Q), π) ⊃ true(Subst(X, V, Q), π)) (90)

where absent(V, X) means that the variable V is not present in the

concept X, and Subst(X, V, Y ) is the concept that results from sub-

stituting the concept X for the variable V in the concept Y . absent

and Subst are characterized by the following axioms:

(∀V 1 V 2)(absent(V 1, V 2) ≡ V 1 (cid:54)= V 2),

(91)

(∀ V P X)(absent(V, Know(P, X)) ≡ absent(V, P )∧absent(V, X)),(92)

axioms similar to (92) for other conceptual functions,

(∀V Q)absent(V, All(V, Q)),

(∀V Q)absent(V, Exist(V, Q)),

(∀V Q)absent(V, T he(V, Q)),

(∀V X)(Subst(V, V, X) = X),

(∀X V )(Subst(X, V, V ) = X)

(∀X V P Y )(Subst(X, V, Know(P, Y ))

= Know(Subst(X, V, P ), Subst(X, V, Y ))),

(93)

(94)

(95)

(96)

(97)

(98)

axioms similar to (98) for other functions,

(∀X V Q)(absent(V, Y ) ⊃ Subst(X, V, Y ) = Y ),

(99)

(∀X V 1 V 2 Q)(V 1 (cid:54)= V 2 ∧ absent(V 2, X)

⊃ Subst(X, V 1, All(V 2, Q)) = All(V 2, Subst(X, V 1, Q)))

(100)and corresponding axioms to (100) for Exist and T he.

Along with these comes an axiom corresponding to α-conversion,

(∀V 1 V 2 Q)(All(V 1, Q) = All(V 2, Subst(V 2, V 1, Q))).

(101)

The functions absent and Subst play a “syntactic” role in describ-

ing the rules of reasoning and don’t appear in the concepts themselves.

It seems likely that this is harmless until we want to form concepts of

the laws of reasoning.

We used the Greek letter π for possible worlds, because we did not

want to consider a possible world as a thing and introduce concepts of

possible worlds. Reasoning about reasoning may require such concepts

or else a formulation that doesn’t use possible worlds.

Martin Davis (in conversation) pointed out the advantages of an

alternate treatment avoiding possible worlds in case there is a single

domain of individuals each of which has a standard concept. Then we

can write

(∀V Q)(true All(V, Q) ≡ (∀x)(true Subst(Concept1x, V, Q)).(102)

8 Possible Applications to Artiﬁcial In-

telligence

The foregoing discussion of concepts has been mainly concerned with

how to translate into a suitable formal language certain sentences of

ordinary language. The success of the formalization is measured by

the extent to which the logical consequences of these sentences in the

formal system agree with our intuitions of what these consequences

should be. Another goal of the formalization is to develop an idea

of what concepts really are, but the possible formalizations have not

been explored enough to draw even tentative conclusions about that.

For artiﬁcial intelligence, the study of concepts has yet a diﬀerent

motivation. Our success in making computer programs with general

intelligence has been extremely limited, and one source of the limita-

tion is our inability to formalize what the world is like in general. We

can try to separate the problem of describing the general aspects of

the world from the problem of using such a description and the facts

of a situation to discover a strategy for achieving a goal. This is called

separating the epistemological and the heuristic parts of the artiﬁcial

intelligence problem and is discussed in (McCarthy and Hayes 1969).

We see the following potential uses for facts about knowledge:

1. A computer program that wants to telephone someone must rea-

son about who knows the number. More generally, it must reason

about what actions will obtain needed knowledge. Knowledge in

books and computer ﬁles must be treated in a parallel way to

knowledge held by persons.

2. A program must often determine that it does not know some-

thing or that someone else doesn’t. This has been neglected

in the usual formalizations of knowledge, and methods of prov-

ing possibility have been neglected in modal logic. Christopher

Goad (to be published) has shown how to prove ignorance by

proving the existence of possible worlds in which the sentence

to be proved unknown is false. Presumably proving one’s own

ignorance is a stimulus to looking outside for the information.

In competitive situations, it may be important to show that a

certain course of action will leave competitors ignorant.

3. Prediction of the behavior of others depends on determining

what they believe and what they want.

It seems to me that AI applications will especially beneﬁt from

ﬁrst order formalisms of the kind described above. First, many of

the present problem solvers are based on ﬁrst order logic. Morgan

(1976) in discussing theorem proving in modal logic also translates

modal logic into ﬁrst order logic. Second, our formalisms leaves the

syntax and semantics of statements not involving concepts entirely

unchanged, so that if knowledge or wanting is only a small part of

a problem, its presence doesn’t aﬀect the formalization of the other

parts.

9 Abstract Languages

The way we have treated concepts in this paper, especially when we

put variables in them, suggests trying to identify them with terms in

some language. It seems to me that this can be done provided we use

a suitable notion of abstract language.

Ordinarily a language is identiﬁed with a set of strings of symbols

taken from some alphabet. McCarthy (1963) introduces the idea of

abstract syntax, the idea being that it doesn’t matter whether sums

are represented a + b or +ab or ab+ or by the integer 2a3b or by the

LISP S-expression (PLUS A B), so long as there are predicates for de-

ciding whether an expression is a sum and functions for forming sums

from summands and functions for extracting the summands from the

sum. In particular, abstract syntax facilitates deﬁning the semantics

of programming languages, and proving the properties of interpreters

and compilers. From that point of view, one can refrain from specify-

ing any concrete representation of the “expressions” of the language

and consider it merely a collection of abstract synthetic and analytic

functions and predicates for forming, discriminating and taking apart

abstract expressions. However, the languages considered at that time

always admitted representations as strings of symbols.

If we consider concepts as a free algebra on basic concepts, then

we can regard them as strings of symbols on some alphabet if we

want to, assuming that we don’t object to a non-denumerable alpha-

bet or inﬁnitely long expressions if we want standard concepts for all

the real numbers. However, if we want to regard Equal(X, Y ) and

Equal(Y, X) as the same concept, and hence as the same “expres-

sion” in our language, and we want to regard expressions related by

renaming bound variables as denoting the same concept, then the al-

gebra is no longer free, and regarding concepts as strings of symbols

becomes awkward even if possible.

It seems better to accept the notion of abstract language deﬁned by

the collection of functions and predicates that form, discriminate, and

extract the parts of its “expressions”. In that case it would seem that

concepts can be identiﬁed with expressions in an abstract language.

10 Remarks and Acknowledgements

The treatment given here should be compared with that in (Church

1951b) and in (Morgan 1976). Church introduces what might be called

a two-dimensional type structure. One dimension permits higher or-

der functions and predicates as in the usual higher order logics. The

second dimension permits concepts of concepts, etc. No examples or

applications are given. It seems to me that concepts of concepts will

be eventually required, but this can still be done within ﬁrst order

logic.

Morgan’s motivation is to use ﬁrst order logic theorem proving pro-

grams to treat modal logic. He gives two approaches. The syntactic

approach—which he applies only to systems without quantiﬁers—uses

operations like our And to form compound propositions from elemen-

tary ones. Provability is then axiomatized in the outer logic. His

semantic approach uses axiomatizations of the Kripke accessibility re-

lation between possible worlds.

It seems to me that our treatment

can be used to combine both of Morgan’s methods, and has two fur-

ther advantages. First, concepts and individuals can be separately

quantiﬁed. Second, functions from things to concepts of them per-

mit relations between concepts of things that could not otherwise be

expressed.

Although the formalism leads in almost the opposite direction, the

present paper is much in the spirit of (Carnap 1956). We appeal to

his ontological tolerance in introducing concepts as objects, and his

section on intensions for robots expresses just the attitude required

for artiﬁcial intelligence applications.

We have not yet investigated the matter, but plausible axioms for

necessity or knowledge expressed in terms of concepts may lead to

the paradoxes discussed in (Kaplan and Montague 1960) and (Mon-

tague 1963). Our intuition is that the paradoxes can be avoided by

restricting the axioms concerning knowledge of facts about knowledge

and necessity of statements about necessity. The restrictions will be

somewhat unintuitive as are the restrictions necessary to avoid the

paradoxes of naive set theory.

Chee K. Yap (1977) proposes virtual semantics for intensional log-

ics as a generalization of Carnap’s individual concepts. Apart from

the fact that Yap does not stay within conventional ﬁrst order logic,

we don’t yet know the relation between his work and that described

here.

I am indebted to Lewis Creary, Patrick Hayes, Donald Michie,

Barbara Partee and Peter Suzman for discussion of a draft of this

paper. Creary in particular has shown the inadequacy of the formalism

for expressing all readings of the ambiguous sentence “Pat knows that

Mike knows what Joan last asserted”. There has not been time to

modify the formalism to ﬁx this inadequacy, but it seems likely that

concepts of concepts are required for an adequate treatment.

11 References

Carnap, Rudolf (1956)). Meaning and Necessity). University of Chicago

Press.

Church, Alonzo (1951a). The Need for Abstract Entities in Se-

mantic Analysis, in Contributions to the Analysis and Synthesis of

Knowledge). Proceedings of the American Academy of Arts and Sci-

ences, 80). No. 1 (July 1951), 100–112. Reprinted in The Structure of

Language). edited by Jerry A. Fodor and Jerrold Katz, Prentice-Hall

Church, Alonzo (1951b). A formulation of the logic of sense and

In: P. Henle (ed.), Essays in honor of Henry Sheﬀer).

denotation.

pp. 3–24. New York.

Frege, Gottlob (1892). ¨Uber Sinn und Bedeutung. Zeitschrift f¨ur

Philosophie und Philosophische Kritik 100:25–50. Translated by H.

Feigl under the title “On Sense and Nominatum” in H. Feigl and W.

Sellars (eds.) Readings in Philosophical Analysis). New York 1949.

Translated by M. Black under the title “On Sense and Reference” in

P. Geach and M. Black, Translations from the Philosophical Writings

of Gottlob Frege). Oxford, 1952.

Kaplan, David (1969). Quantifying In, from Words and Objec-

tions: Essays on the Work of W.V. Quine). edited by D. Davidson

and J. Hintikka, (Dordrecht-Holland: D. Reidel Publishing Co.), pp.

178–214. Reprinted in (Linsky 1971).

Kaplan, David and Montague, Richard (1960). A Paradox Re-

gained, Notre Dame Journal of Formal Logic 1:79–90. Reprinted in

(Montague 1974).

Linsky, Leonard, ed.(1971) Reference and Modality). Oxford Read-

ings in Philosophy, Oxford University Press.

McCarthy, J. (1963). Towards a Mathematical Science of Compu-

tation, in Proceedings of IFIP Congress 1962). North-Holland Pub-

lishing Co., Amsterdam.

McCarthy, J. and Hayes, P.J. (1969). Some Philosophical Prob-

lems from the Standpoint of Artiﬁcial Intelligence. Machine Intelli-

gence 4). pp. 463–502 (eds Meltzer, B. and Michie, D.). Edinburgh:

Edinburgh University Press. (Reprinted in B. L. Webber and N. J.

Nilsson (eds.), Readings in Artiﬁcial Intelligence, Tioga, 1981, pp.

431–450; also in M. J. Ginsberg (ed.), Readings in Nonmonotonic

Reasoning, Morgan Kaufmann, 1987, pp. 26–45; also in this volume,

pp. 000–000.)

McCarthy, John (1979): “Ascribing Mental Qualities to Machines”

in Philosophical Perspectives in Artiﬁcial Intelligence, Ringle, Martin

(ed.), Harvester Press, July 1979. Reprinted in (McCarthy 1990).

McCarthy, John (1990): Formalizing Common Sense, Ablex, Nor-

wood, New Jersey

Montague, Richard (1963). Syntactical Treatments of Modality,

with Corollaries on Reﬂexion Principles and Finite Axiomatizability,

Acta Philosophica Fennica 16:153–167. Reprinted in (Montague 1974).

Montague, Richard (1974). Formal Philosophy). Yale University

Press

Morgan, Charles G. (1976). Methods for Automated Theorem

Proving in Nonclassical Logics, IEEE Transactions on Computers).

vol. C-25, No. 8, August 1976

Quine, W.V.O. (1956). Quantiﬁers and Propositional Attitudes,

Journal of Philosophy). 53. Reprinted in (Linsky 1971).

Quine, W.V.O. (1961). From a Logical Point of View). Harper

and Row.

Yap, Chee K. (1977). A Semantical Analysis of Intensional Log-

ics). Research Report, IBM Thomas J. Watson Research Center,

Yorktown Heights, New York. RC 6893 (#29538).

