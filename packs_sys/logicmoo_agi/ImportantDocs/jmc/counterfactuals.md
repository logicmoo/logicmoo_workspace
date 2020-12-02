Link¨oping Electronic Articles in

Computer and Information Science

Vol. 3(1999): nr 2

Useful Counterfactuals

Tom Costello

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

Link¨oping University Electronic Press

Link¨oping, Sweden

http://www.ep.liu.se/ea/cis/1999/002/

Published on July 1, 1999 by

Link¨oping University Electronic Press

581 83 Link¨oping, Sweden

Link¨oping Electronic Articles in

Computer and Information Science

ISSN 1401-9841

Series editor: Erik Sandewall

c(cid:13)1999 Tom Costello

John McCarthy

Typeset by the authors using LATEX

Formatted using ´etendu style

Recommended citation:

<Authors>. <Title>. Link¨oping Electronic Articles in

Computer and Information Science, Vol. 3(1999): nr 2.

http://www.ep.liu.se/ea/cis/1999/002/. July 1, 1999.

This URL will also contain a link to the authors’ home pages.

The publishers will keep this article on-line on the Internet

(or its possible replacement network in the future)

for a period of 25 years from the date of publication,

barring exceptional circumstances as described separately.

The on-line availability of the article implies

a permanent permission for anyone to read the article on-line,

to print out single copies of it, and to use it unchanged

for any non-commercial research and educational purpose,

including making copies for classroom use.

This permission can not be revoked by subsequent

transfers of copyright. All other uses of the article are

conditional on the consent of the copyright owners.

The publication of the article on the date stated above

included also the production of a limited number of copies

on paper, which were archived in Swedish university libraries

like all other written works published in Sweden.

The publisher has taken technical and administrative measures

to assure that the on-line version of the article will be

permanently accessible using the URL stated above,

unchanged, and permanently equal to the archived printed copies

at least until the expiration of the publication period.

For additional information about the Link¨oping University

Electronic Press and its procedures for publication and for

assurance of document integrity, please refer to

its WWW home page: http://www.ep.liu.se/

or by conventional mail to the address stated above.

Abstract

Counterfactual conditional sentences can be useful in artiﬁcial

intelligence as they are in human aﬀairs.

In particular, they

allow reasoners to learn from experiences that they did not quite

have.

Our tools for making inferences from counterfactuals permit

inferring sentences that are not themselves counterfactual. This

is what makes them useful.

A simple class of useful counterfactuals involves a change

of one component of a point in a space provided with a carte-

sian product structure. We call these cartesian counterfactuals.

Cartesian counterfactuals can be modeled by assignment and

contents functions as in program semantics. We also consider

the more general tree-structured counterfactuals.

Introduction

(1): “If another car had come over the hill when you

passed that car, there would have been a head-on colli-

sion.”

is a useful counterfactual conditional sentence. The driver will esti-

mate how long passing took and how much time he would have had if

another car had come over the hill. In this he uses his observation of

the distance to the top of the hill when he got back in the right hand

lane and his estimate of how long it would have taken a car suddenly

coming over the hill to cover that distance.

If he regards the counterfactual (1) as true, he will be more cau-

tious about passing in the future. If he regards it as false, it will be

because he thinks he had plenty of time to complete the maneuver

and get back in the right lane. If he regards it merely as a material

implication with a false antecedent, he’ll believe it but won’t take it

as a reason to change his driving habits.

We discuss such useful counterfactuals for two reasons. (1) We

expect them to be useful in AI systems. (2) We have found counter-

factuals inferred from experience and having behavioral consequences

to admit a richer theory than counterfactuals cooked up to serve as

examples of the semantics.

In general, a counterfactual conditional sentence ( counterfactual

for short ) is a sentence

p implies q,

in which the antecedent p is false. If implies is taken to be the ordi-

nary mathematical logical implication ⊃, then all counterfactuals are

true. However, in ordinary life, it is often useful to consider certain

counterfactuals, as in the above example (1), as being possibly false.

Also (1) may be a useful counterfactual because it permits the driver

to learn from an experience he didn’t quite have.

We will use (cid:31) to represent counterfactual implication, so that (1)

may be written

Another car was coming when you passed (cid:31) there was

a head-on collision.

Q. What can the driver learn if he believes (1)?

A. Whatever he could learn from “Another car came when you

passed, and there was a head-on collision.”

Three points.

1. We ignore the possibility that the collision might interfere with

the driver’s ability to learn.

2. What can be learned depends on the driver’s mechanisms for

learning. However, we propose that he applies the same learning

mechanism to the almost experience as he would apply to the real

experience.

3. The hypothetical experience is not a full possible world, e.g.

the name of the driver of the car that came over the hill isn’t deter-

mined. It is a theoretical entity in an approximate theory. [McCarthy, 2000]

treats approximate theories in some detail, but the simplest approxi-

mate theories for discussing counterfactuals are the theories of carte-

sian counterfactuals of the present article.

One straightforward inference from (1) is (2)

Another car comes when you pass under suﬃciently

similar circumstances ⊃ there is a head-on collision.

Thus a counterfactual conditional is used to infer a correspond-

ing ordinary conditional, univerally quantiﬁed over “similar circum-

stances”. Believing the counterfactual (1) lets us make the same gen-

eralization from the counterfactual conditional that we would make

had the collision occurred.

There is some resemblance between this inference and those used

to infer a scientiﬁc law, but there are two important diﬀerences. (1)

“similar circumstances” is often not spelled out in language. It can

be just circumstances that look the same. (2) The inference is from

a single experience; more is needed to infer a scientiﬁc law.

While counterfactuals are used in daily life, they have been most

studied in philosophy. The main philosophical goal has been to assign

meaning to these sentences. The Lewis/Stalnaker [Stalnaker, 1968]

[Stalnaker and Thomason, 1970] approach is the leading one. David

Lewis [Lewis, 1973] deﬁnes the truth of a counterfactual using the

notion of possible worlds. He posits that possible worlds are ordered

by comparative possibility. He then regards p (cid:31) q as true provided

q is true in the nearest possible worlds where p is true. He gives no

way of judging the closeness of worlds save that of considering the

counterfactuals. Thus it seems diﬃcult to evaluate the truth of any

particular counterfactual based on other evidence.

Moreover, he Lewis/Stalnaker approach oﬀers no way of inferring

non-counterfactual sentences from counterfactuals.

The truth of a counterfactual does not just depend on the state

of the world the way a direct observation does. There is always a

theory—in the above example, an implicit theory of driving shared by

the passenger and the driver. The theory is based on their experience

and what they have learned and been taught about driving.

2 Uses of Counterfactuals

For AI purposes we ask what kinds of counterfactuals are useful to

humans, as they are also likely to be the kinds useful to computer

programs. Concentrating on counterfactuals that are useful in that

believing them usefully aﬀects behavior may also have some philo-

sophical beneﬁts, because the reasoning leading to a useful counter-

factual and the useful conclusions drawn from it provide some guid-

ance about the best kind of theory.

We begin with some examples of the uses of particular counter-

factuals.

1. The counterfactual head-on collision of the introduction.

2. Two ski instructors see a pupil fall on a hill. One says “If

he had bent his knees more, he would not have fallen”. The

other disagrees and claims “If he had put his weight on his

downhill ski, he would not have fallen”. This example is from

[McCarthy, 1979]. Each one suggests a speciﬁc kind of lesson.

3. Our ski instructors could view the world in a diﬀerent way, and

both assent to the counterfactual, “if he had two more lessons

he would not have fallen”.

4. If Caesar had been in charge in Korea, he would have used

nuclear weapons.

If Caesar had been in charge in Korea, he

would have used catapults. Diﬀerent theoretical structures give

diﬀerent counterfactuals. Maybe the ﬁrst suggests that if we

are serious about winning, we should be more like Caesar. The

second doesn’t suggest anything.

5. If there had been one more book in that box you would not

have been able to lift it. The lesson is not to put too many

books in a box.

6. If wishes were horses, beggars would ride. We are far from

proposing a way of drawing conclusions from metaphors. This

very abstract counter-factual would have a very approximate

theory

Each of these counterfactuals tells us something about how the

world works. We can use this advice in future, if we ﬁnd ourselves in

a similar situation. The notion that the counterfactual is applicable

on similar occasions is important. If we are to use a counterfactual to

predict in a new occasion s, there must be some kind of test, whether

or not the new occasion s is suﬃciently similar to the situation that

gave rise to the counterfactual. This test is given by an approximate

theory.

The kind of approximate theories that are relevant to counterfac-

tuals are those covering only some aspects of the world. The theory

used in the car example does not include facts that determine whether

or not another car can come over the hill. Likewise the skier theory

does not determine if a skier bends his knees. The theories allowing

inferences about Caesar in Korea respectively relate to his character

as a general and the weapons he had. “If wishes were horses . . .

”, takes only advantage of the semantic parallel between having a

wish and having a material object. Approximate theories are more

thoroughly discussed in [McCarthy, 2000].

2.1 Learning

A skier might conclude that had he bent his knees on a certain slope

he would not have fallen. The skier can learn from this. It is im-

portant, if we are to use counterfactuals for learning that we can

recognize that they are sometimes false. In our ﬁrst example we can

imagine the response that “If there were another car it would have

been visible in time for me to avoid it”. This new counterfactual can

also be true or false.

The performance of learning algorithms improves when they have

more examples. Counterfactuals are one way to collect more examples

than can be found by direct experience. Often it is better to imagine

a data point than to experience it.

2.2 Prediction in similar circumstances

In so far as our knowledge of the world is incomplete, new sentences

can tell us more about the world. Every counterfactual we are told

gives us more information about how the world would be, if things

were only slightly diﬀerent, relative to some unstated approximate

theory. This information can later be used if we ﬁnd ourselves in a

situation with only a small number of diﬀerences between it and the

present, so that the approximate theory is applicable to both. The

counterfactual

“If there had been one more book in that box you

would not have been able to lift it.”

tell us that in future situations, that satisfy the unstated theory the

speaker considers, boxes with more books in them will be too heavy

to lift. This diﬀers from the learning we considered earlier, as this

is inferring a universal from a counterfactual, rather than using the

counterfactual as an instance in a learning algorithm.

If the approximate theory is unstated, to use this counterfactual

we need to infer what theory was used. A natural default to use here

is to assume that the speaker is using the same theory that you ﬁnd

appropriate to describe the situation.

We can apply counterfactuals in other situations because the the-

ories on which they are based are approximate. The truth of the

counterfactual only depends on certain features of the situation, and

when these features re-occur, the same inference may be made. In a

later section we give an example of where we can derive new facts,

that do not mention counterfactuals, from a counterfactual. In our

skiing domain, we show that we can derive a fact about the world

(that a certain slope is a turn), from the truth of a counterfactual,

(“if he had put his weight on his downhill ski, he would not have

fallen”).

Counterfactuals are useful for other purposes in AI. Ginsberg

[Ginsberg, 1986] suggests that they are useful for planning. They

also are closely related to the notion of causality, as discussed in

[Pearl, 1988] [Geﬀner, 1992],[Pearl, 2000].

3 Detailed Examples

Cartesian counterfactuals involve a counterfactual sentence, a theory,

a frame, and a world/point in the frame. The sentence is interpreted

by the theory, relative to the frame. We move through the frame,

from the current world, to a new point in the frame.

3.1 Rectangular co-ordinates

Example: 1 We consider a cartesian space of three components, x,

y, z. The theory provides a co-ordinate system for the space. We are

interested in the distance from a point to the origin:

s = (cid:112)x2 + y2 + z2

This sentence is our approximate theory.

Now consider the point (1, 2, 1). This will act as our current world.

We ask whether,

y = 3 (cid:31) s = √19.

(2)

Our cartesian structure implies that x and z hold their particular

values 1, 1. Therefore we have

s = √1 + 9 + 1, s = √11, s (cid:54)= √19.

Therefore (2) is an example of an untrue counterfactual. The coun-

terfactual “if y were 3 then s would be √11” is true, i.e. y = 3 (cid:31) s =

√11.

This example has all the essential elements of a cartesian coun-

terfactual. We have a cartesian space, whose points are the models

of the approximate theory and whose structure is given by the co-

ordinates. Each of these co-ordinates in this example is the value of

a variable. To ﬁnd the truth value of a counterfactual we change the

co-ordinate as speciﬁed by the left hand side and evaluate the right

hand side.

The co-ordinate frame we chose here, using the variables x, y, z,

is not the only possible frame. A transformation of those variables

would give a similar theory but with diﬀerent counterfactuals.. One

possibility is,

x(cid:48) = x + y, y, z,

Our initial state has the following values.

x(cid:48) = 3, y = 2, z = 1,

Given this co-ordinate system, the counterfactual y = 3 (cid:31) s = √19

is true, whereas it was false in the previous frame.

In this example, s was uniquely determined by the values of the

co-ordinates. We can imagine that another value r, is not uniquely

determined—we only know that it is smaller than s. Thus we have,

r ≤ (cid:112)x2 + y2 + z2.

Again, let us choose x, y, z to have the values (1, 2, 1). In this case

y = 3 (cid:31) r ≤ √11 is true, as we know that r ≤ s and s = √11.

Furthermore, we know that y = 3 (cid:31) r ≥ 10 is false. However,

when we cannot uniquely determine all other components in terms

of the co-ordinates, some counterfactuals are indeterminate. For in-

stance, we do not know the truth value of y = 3 (cid:31) r = √6.

Thus diﬀerent co-ordinate frame make diﬀerent counterfactuals

true. Thus three factors inﬂuence the truth of a counterfactual:

1. The space of possible states.

2. The co-ordinate frame on the space of possible states.

3. The current state.

The above rectangular co-ordinate system example hasn’t enough

structure to prefer one theory over another. However, suppose it were

speciﬁed that x, y and z were the co-ordinates along the walls and

the height of a point starting from the corner of a room. Then there

would be some reason for preferring the x-y-z theory and its asso-

ciated counterfactuals to the x(cid:48)-y-z theory and its associated coun-

terfactuals. When there is a clear reason to prefer one theory, its

counterfactuals can have a somewhat objective character. These are

the most useful. Even so, this would be a useful counterfactual only

imbedded in a larger theory that includes some goal.

3.2 The Almost Crash—Elaborated

We elaborate the almost-crash example slightly to say “might have

been a head-on collision” instead of “would have been a head-on

collision”. In this uncertain world, it is more realistic.

Suppose the driver estimates (triggered by the passenger’s state-

ment) that it will take 7 ± 2 seconds to complete the pass and get

back in the right lane. He also estimates that it will take 9 ± 3 sec-

onds to drop back and get in line and that if he stays in the left lane

and a car comes over the hill at that instant, he collision will occur

in 9 ± 5 seconds. He concludes that if a car comes over the hill at

that embarrassing moment the probability of a collision is 0.2. He

estimates the probability of a car coming over the hill at that instant

is between 0.001 and 0.0001. He concludes that the odds, while small,

are unacceptable.

We have expressed this example numerically, as might be appro-

priate for a robot. Formalizing this aspect of human behavior might

not be so numerical.

We can suppose that his estimate of the required passing time is

not an a priori estimate based on a theory of driving but is based

on how rapidly he was overtaking the other car, i.e.

is based on

this experience. Thus he learns to change his driving rules from

an experience of collision that he didn’t quite have. Much case-based

reasoning in real life is based on counterfactuals, although the theories

described in [Lenz et al., 1998, Leake and Plaza, 1997] do not include

counterfactual cases.

3.3 Formalization of the Skiing Examples

We now look at an axiomatization that formalizes some examples

of counterfactuals in a skiing domain. We sketch a formalization

of skiing, which includes both some facts about lessons, and some

facts about what happens when you ski. The formalization uses the

situation calculus discussed in [McCarthy and Hayes, 1969].

The analysis is designed to highlight several points.

1. Co-ordinate frames give a simple way of deﬁning semantics for

counterfactuals.

2. Diﬀerent frames are appropriate for diﬀerent points of view.

3. Diﬀerent frames can lead to diﬀerent counterfactuals.

We consider four counterfactuals. The ﬁrst two are from our

previously mentioned two ski instructors. Before we consider the

counterfactuals, we consider the following story.

Junior had 25 dollars. He went to a ski slope and

spent his money on one cheap lesson, that cost 25 dol-

lars. Cheap lessons teach you one skill each lesson, either

bending your knees on bumps, or if you have learned that,

then placing your weight on your downhill ski on turns.

Expensive lessons cost twice as much, but teach two skills

per lesson, in the same order. Thus, Junior learns to bend

his knees. He then goes skiing, at a time picked out by

situation 1, but when he comes to Slope4 he falls.

Slope4 is a turn, and according to our theory, unless

you place your weight on your downhill ski on turns you

fall. We also believe that unless you bend your knees on

a bump you fall, and that these behaviors happen only if

you have learned these skills.

Our ﬁrst counterfactual states that ‘ ‘if he had actually bent his

knees in situation 1 then he would not have fallen in the next situa-

tion”. This will be represented by the following counterfactual.

Actual(result(Bend, S1)) (cid:31) ¬holds(Fallen, Next(S1))

(3)

The other states that ‘ ‘if he had actually put his weight on the down-

hill ski in situation 1 then he would not have fallen in the next situ-

ation”. In our logical formalization this will be written.

Actual(result(Down, S1)) (cid:31) ¬holds(Fallen, Next(S1))

(4)

The diﬀerence can be resolved by ﬁnding out whether Slope4 has a

bump or is a turn. As Slope4 is a turn, the ﬁrst is false and the second

is true. Here, the counterfactuals are evaluated in a frame where what

Actually happens is a component. Thus, we can change the fact that

he bent his knees, without changing any other co-ordinate, and thus

infer that he does not fall.

Both our instructors can agree that ‘ ‘if he had chosen more ex-

pensive lessons, then he would not have fallen”.

Choice = Expensive (cid:31) ¬holds(Fallen, Next(S1))

(5)

Here they choose to keep the number of lessons ﬁxed, but make the

lessons better.

Our hero does not assent to the above conditional. He knows

that “if he chose the expensive lessons, he would not have had any

lessons”, as he cannot aﬀord it. Thus he believes that he would fall

if he chose expensive lessons,

Choice = Expensive (cid:31) holds(Fallen, Next(S1)).

(6)

Thus he still would not have learned the requisite skills. Expensive

lessons cost more, so he would not have been able to take as many

lessons.

The reason that the ski instructors diﬀer from our hero on this

conditional is that they allow the amount of money to vary, while our

hero allows the number of lessons to vary. They use diﬀerent frames

and thus get diﬀerent results.

We give an axiomatization of part of this domain in Section 5,

and now present a set of frames that gives the results for the ﬁrst two

counterfactuals.

3.4 Co-ordinate frames for Skiing

With the axiomatization of the above story given below we can now

consider the ﬁrst two counterfactuals. In each case we take an ap-

proximate theory, which will be some subset of the consequences of

the axioms in Section 5 , and a co-ordinate frame. From the theory

and the frame we judge the truth of the counterfactual. We show that

choosing diﬀerent approximate theories, or choosing diﬀerent frames

can lead to diﬀerent choices.

We give the co-ordinate frame that the two instructors use. They

choose what situations were actual, and what ﬂuents held at the

situation S1, and the type of Slope4 as their frame. Here tr is a truth

value. The co-ordinates in the frame are the following terms, (two of

these are propositions, and so take on the values true or false).

Next(S1), Typeslope4,

holds(Fallen, S1), holds(Skiing4, S1).

As their core approximate theory they take the axioms about the

eﬀects of various ski moves, 15, the axiom relating Next and Actual,

16, and the unique names and domain closure axioms and the frame

axioms 18. As the current world, they choose the values,

Next(S1) = result(Falls, S1) Typeslope4 = Turn

¬holds(Fallen, S1)

holds(Skiing4, S1)

as they agree that Junior did fall on a slope with a bump, while skiing

Slope4.

With this frame the following counterfactuals are false and true,

respectively.

Actual(result(Bend, S1)) (cid:31) ¬holds(Fallen, Next(S1))

Actual(result(Down, S1)) (cid:31) ¬holds(Fallen, Next(S1))

In contrast the counterfactual

Actual(result(Bend, S1)) ∧ Typeslope4 = Bump (cid:31)

¬holds(Fallen, Next(S1))

is true. We prove the ﬁrst two claims formally after introducing the

necessary machinery in the next section.

(7)

(8)

4 Cartesian Counterfactuals via State Vec-

tors

In the previous section we claimed that certain counterfactuals were

true, given some co-ordinate frames. We now give a preliminary

axiomatization in terms of state vectors that allows us to formally

prove these statements.

We can deﬁne cartesian counterfactuals in terms of state vectors

[McCarthy, 1962]. The value of a variable x in a state vector ξ is

c(x, ξ), while the state vector that is like ξ, save that x has been

assigned the value v, is a(x, v, ξ). We can axiomatize a and c as

follows.

∀x, v.ξ.c(x, a(x, v, ξ)) = v,

∀x, y, v, ξ.x (cid:54)= y → c(y, a(x, v, ξ)) = c(y, ξ)

(9)

(10)

(11)

(12)

(13)

The numerical example of subsection 3.1 is expressed as follows.

Let ξ0 represent the actual state of the world. We have

c(x, ξ0) = 1

c(y, ξ0) = 2

c(z, ξ0) = 1

We are interested in the function

s(ξ) = (cid:112)c(x, ξ)2 + c(y, ξ)2 + c(z, ξ)2.

The counterfactual

takes the form

y = 3 (cid:31) s = 7

s(a(y, 3, ξ0)) = 7.

It is obviously false.

Notice that while cartesian counterfactuals give a meaning to “if

x were 7”, they do not give a meaning to “if x + y were 7. This

is a feature, not a bug, because in ordinary common sense, counter-

factuals easily constructed from meaningful counterfactuals are often

without meaning.

In the above the “variables” x, y, and z are logically constants.

When we need to quantify over such variables, we need new variables

with an appropriate typographical distinction.

4.1 Relating State vectors to Propositions

The above way of rephrasing a counterfactual as a statement about

state-vectors is insuﬃcient in general. It does not specify how each

step is to be done in other cases. There are three steps.

1. The ﬁrst is to describe the current world as the contents of a

state vector. This is equation 10 above.

2. We now relativize all the other terms, those that are not co-

ordinates, in the theory, making them functions of a state vector

ξ. We replace the terms t that are co-ordinates by their the

contents functions applied to the term, i.e. c(t, xi). This gives

the equation 11 above.

3. The third and ﬁnal step is to interpret left hand side as a mod-

iﬁcation of the state vector. This step yields equation 13 in the

above example. We then judge the relativized right hand side

applied to the new state vector. That is we ask if the relativized

right hand side follows from the values of the co-ordinates ini-

tially, and the relativized theory.

Our state vector does not have values for all terms, just those in

our co-ordinate frame. Thus, for our numerical example, the set of

co-ordinates is,

x, y, z

We need to distinguish between the value of a constant x and its

name x. Here x in bold font refers to the name x, while x in plain

font denotes the value of x.

Let the state vector ξ assign these the terms1 nx, ny, nz. The

propositions that the state vector ξ encodes are thus the three propo-

sitions,

x = nx,

y = ny,

z = nz

Here we have used x, in plain font, as the proposition states that

the value of x is the value of the term nx.

We assign a co-ordinate a value using the a assignment function.

We can determine the value of a co-ordinate in a state vector using

c, the contents function.

We call the set of co-ordinates our frame, and we assume that

they are exactly the names of terms that are true of the predicate f .

Rather than assume that there is only a single frame, we can make

the frame an argument of c and a. This gives the axioms,

∀x, v, ξ, f.f (x) → c(x, a(x, v, ξ, f ), f ) = v

∀x, y, v, ξ, f.f (x) ∧ f (y) ∧ x (cid:54)= y →

c(y, a(x, v, ξ, f ), f ) = c(y, ξ, f )

However, for notational convenience we assume that the frame is

recoverable from the state vector, so we keep the ternary a and the

binary c whenever what frame we are using is clear from the context.

Our state vector thus uniquely determines the values of the co-

ordinates in the frame. From these values, we can determine a theory,

the set of sentences implied by the statements that the co-ordinates

have the values in the state vector.

The propositions that our state vector gives us may not be all the

facts about the world. In our numerical example, the extra fact,

s = (cid:112)x2 + y2 + z2

was also the case.

We usually insist that our approximate theory is consistent with

our frame. That is, the propositions true at every point in our frame

are consistent with the approximate theory.

1We alternately suppose that the state vector assigns values to the names of

constants, but for reasons of generality is is sometimes useful to be able to specify

that a constant is equal to a term, rather than to a value.

To reiterate, a co-ordinate frame, f , is a tuple of names of terms.

In the simplest case these are constants. A point p in a co-ordinate

frame f is a tuple of terms, equal in number to the co-ordinates of

f , and having the type/sort of the corresponding term in f . In the

simplest case, this is a tuple of values, the denotation of the terms.

The state vector that assigns the co-ordinates f the terms p is denoted

ξ(f, p).

The relativization of a theory A to a state vector variable ξ, writ-

ten A(ξ) and a set of co-ordinates X is the theory resulting from

replacing each co-ordinate x by c(x, ξ), and replacing each func-

tion, predicate and constant k not in X by a function from ξ to

the type/sort of k.

Deﬁnition: 1 Let fi be a term in a co-ordinate frame f , and let n

be a term, and let p = p1, . . . , pn be a point in f , the current world,

and let A be an approximate theory.

A counterfactual sentence fi = n (cid:31) ψ, is true in (cid:104)A, f, p(cid:105), if and

only if,

(cid:94)

j≤m

c(fj, ξ0) = pj, A(ξ0) |= ψ(a(fi, n, ξ0)).

We now consider our ﬁrst example. We deﬁne our frame as fol-

lows.

∀var.f0(var) ≡ (var = x) ∨ (var = y)

∨(var = z)

It is important to note that x, y, z are names of constants, not vari-

ables themselves.

We now deﬁne our initial state vector ξ0.

c(x, ξ0, f0) = 1, c(y, ξ0, f0) = 2,

c(z, ξ0, f0) = 1

We need unique names axioms for the names of terms. We use natural

numbers as their own names, for simplicity.

We then have that

c(x, a(y, 3, ξ0, f0), f0) = 1.

We can derive this from the second property of a and c, and the fact

that x (cid:54)= y.

We have x = 1, y = 3, z = 1 s = (cid:112)x2 + y2 + z2 |= s = √11.

We have this set of sentences as our theory, as these are the for-

mulas that result from the terms x, y etc. to their values, 1, etc. in

the state vector.

We now prove that the ﬁrst two counterfactuals that we consid-

ered in our skiing story are true and false respectively. To show this

we need to introduce an axiomatization of skiing, which we do in the

nest section. The axiomatization is only needed for proving the later

theorems, and may be skipped by the un-interested reader.

5 Axiomatization

This section presents a formalization of a skiing domain in the situa-

tion calculus. The major novelty of this domain is that it allows some

reasoning about the actions of an agent. Normally, in the situation

calculus all sequences of actions are considered. Here we look at what

sequences would happen given some facts about our agent’s beliefs

and some rules about how he acts.

We include the notion of an actual time line picked out of the

possible time lines in the situation calculus. This follows Reiter and

Pinto [Pinto and Reiter, 1993]. We enrich this with a Next partial

function, that picks out the next actual situation.

We have ﬁve sorts, the usual situation, ﬂuent and action sorts,

plus sorts for terrain types and skills.

We have ﬁve actions Skislope4, Falls and Bend, Down, and Wait.

We have ﬂuents, Fallen, Skiing4, Learned(sk) for every skill sk. Our

two skills are Bendknees and Downhillski.

We have that after his one lesson our hero knows how to bend his

knees.

holds(Learned(Bendknees), S0)

¬holds(Learned(Downhillski), S0)

¬holds(Fallen, S0)

¬holds(Skiing4, S0)

(14)

We now have some facts about what the actual time line is, given the

state of the world, and the knowledge of our hero. If our hero sees

a bump, and has learned to bend his knees, he will actually do so,

otherwise he will actually fall. If he sees a turn, then he will put his

weight on his downhill ski if he has learned to do that, otherwise he

will actually fall. We also add that if he does the wrong thing, then

he will fall, and add the results of falling etc.

∀s.Typeslope4 = Bump

∧holds(Learned(Bendknees), s) →

Actual(result(Bend, result(Skislope4, s)))

(15)

∀s.Typeslope4 = Bump

∧¬holds(Learned(Bendknees), s) →

Actual(result(Falls, result(Skislope4, s)))

∀s.Typeslope4 = Turn

∧holds(Learned(Downhillski), s) →

Actual(result(Down, result(Skislope4, s)))

∀s.Typeslope4 = Turn∧

¬holds(Learned(Downhillski), s) →

Actual(result(Falls, result(Skislope4, s)))

(16)

(17)

(18)

∀s.Typeslope4 = Turn ∧ holds(Skiing4, s) →

holds(Fallen, result(Bend, s))

∀s.Typeslope4 = Bump ∧ holds(Skiing4, s) →

holds(Fallen, result(Down, s))

∀s.holds(Fallen, result(Falls, s))

S1 = result(Skislope4, S0)

Actual(S0)

Actual(result(Skislope4, S0))

∀as.s (cid:54)= S0 ∧ s (cid:54)= S1 ∧ ¬∃a.s = r(a, S1) →

Actual(s) ≡ Actual(result(Wait, s))

These axioms are enough to pick out an actual timeline, of skiing

Slope4, then carrying out an action that depends on the terrain of

Slope4 and Junior’s knowledge, and then waiting for ever.

We now state that after every actual situation there is a unique

actual situation, that picked out by Next. We take this to be the

deﬁnition of a partial function Next, so that sentences containing

Next are notations for sentences containing Actual.

∀s.result(a, s) = Next(s)

≡ Actual(result(a, s))

def

We also add that Slope4 is a turn.

Typeslope4 = Turn

We also need to add unique names, domain closure, and frame

axioms.

We have as our unique names axioms, fully unique names, except

for the function Actual, the situation constant S1 and the constant

Typeslope4.

We now add our frame axioms for each ﬂuent.

∀a, s.holds(Skiing4, result(a, s)) ≡ a = Skislope4

∀s, a.a (cid:54)= Falls ∧ (a = Bend →

(a = Down → Typeslope4 (cid:54)= Bump∨

Typeslope4 (cid:54)= Turn ∨ ¬holds(Skiing4, s))∧

¬holds(Skiing4, s))

→ (holds(Fallen, s) ≡ holds(Fallen, result(a, s)))

∀s, f.f (cid:54)= Skiing4 →

holds(f, s) ≡ holds(f, result(Wait, s))

∀a, s, sk.holds(Learned(sk), s) ≡

holds(Learned(sk), result(a, s))

These are enough frame axioms to generate explanation closure ax-

ioms for every ﬂuent.

6 Deriving Counterfactuals

Theorem: 1 The counterfactual

Actual(result(Bend, S1)) (cid:31) holds(Fallen, Next(S1))

is true, in the theory A axiomatized by the axioms about the eﬀects of

various ski moves, 15, the axiom relating Next and Actual, 16, and

the unique names axioms and the frame axioms 18, with the following

frame,

Next(S1), Typeslope4,

holds(Skiing4, S1), holds(Fallen, S1),

and the following current world,

Next(S1) = result(Falls, S1) Typeslope4 = Turn

¬holds(Fallen, S1)

holds(Skiing4, S1)

Proof: We use the ﬁrst property of our assignment functions to

derive that, in our new world, Actual(result(Bend, S1)).

Actual(result(Bend, S1)) is a shorthand for the result of equating

a term in the frame, Next(S1), to the term result(Bend, S1).

We use the second property of our assignment function a to derive

that holds(Skiing4, S1). This follows as holds(Skiing4, S1) is in the

frame, and holds(Skiing4, S1) is true in our current world, so we have

that its value persists. We also show that Typeslope4 = Turn in the

same way.

We now use the eﬀect axiom to derive that

Finally the deﬁnition of Next, which is in the core theory gives us

holds(Fallen, result(Bend, S1)).

holds(Fallen, Next(S1)).

Theorem: 2 The counterfactual

Actual(result(Down, S1)) (cid:31) ¬holds(Fallen, Next(S1))

is true, in the theory A axiomatized above, with the frame and current

world speciﬁed above.

Proof: We use the ﬁrst property of our assignment functions to

derive that in our new world, Actual(result(Down, S1)) as above.

We use the second property of our assignment functions to derive

that holds(Skiing4, S1) and Typeslope4 = Turn as we did previously.

We now use the frame axiom 18 to derive that

¬holds(Fallen, result(Down, S1)),

using the given fact that holds(Skiing4, S1) and Typeslope4 = Turn.

Finally the deﬁnition of Next gives us

¬holds(Fallen, Next(S1)).

7 Deriving Facts from Counterfactuals

In this section we show that we can derive facts that do not contain

counterfactual implications from counterfactuals.

We take as our theory, the axioms in Section 5, save for the last

axiom Typeslope4 = Turn. From the counterfactual,

Actual(result(Down, S1)) (cid:31) ¬holds(Fallen, Next(S1))

we derive that, Typeslope4 = Turn.

Theorem: 3 Let the theory A be axiomatized by the axioms in Sec-

tion 5 save 17 and the counterfactual

Actual(result(Down, S1)) (cid:31) ¬holds(Fallen, Next(S1)),

judged relative to the approximate theory B axiomatized by the axioms

about the eﬀects of various ski moves, 15, the axiom relating Next and

Actual 16, and the unique names axioms and the frame axioms 18.

Let f (var) be deﬁned as follows:

∀var.f (var) ≡

var = Next(S1)∨

var = holds(Fallen, S1)

var = holds(Skiing4, S1)

Then, A |= Typeslope4 = Turn.

Proof: We ﬁrst note that in A, the co-ordinates have the values,

Falls, t, t. We expand the deﬁnition of a counterfactual being true,

namely,

c(Next(S1), ξ0) = result(Falls, S1),

c(holds(Fallen, S1), ξ0) = t,

c(holds(Skiing4, S1), ξ0) = t,

B(ξ0) |=

¬holds(Fallen, c(Next(S1), a(Next(S1), result(Down, S1), ξ0))).

to get, by replacing the function c everywhere its value at that argu-

ment,

Next(S1) = result(Down, S1),¬holds(Fallen, S1),

holds(Skiing4, S1), B |=

¬holds(Fallen, result(Down, S1))

This is a meta-theoretic statement, so we translate this into second

order logic, by quantifying over predicates, functions and objects in

the standard way2.

¬holds(cid:48)(Fallen(cid:48), S1(cid:48))∧

∀ ¯Z.Next(cid:48)(S1(cid:48)) = result(cid:48)(Down(cid:48), S1(cid:48))∧

holds(cid:48)(Skiing4(cid:48), S1(cid:48)) ∧ B[ ¯Z/ ¯Z (cid:48)] →

¬holds(cid:48)(Fallen(cid:48), Next(cid:48)(S1(cid:48))),

2We do not need to quantify over the domain, as in this case we have enough

axioms to ensure that the domains of each of our sorts are ﬁxed.

where ¯Z is the tuple of all constant symbols in the language, and ¯Z (cid:48)

is a tuple of variables, equal in type and arity to the constants in ¯Z,

we write variables in Z (cid:48) as Next(cid:48) etc.

This second order sentence captures the truth of the counterfac-

tual. We can conjoin this with the other axioms in the theory A, and

ask whether it implies, Typeslope4 = Turn in second order logic.

It does, as can be seen from instantiating the universally quanti-

ﬁed variables, with their corresponding constants, except for Actual(cid:48)

and Next(cid:48) which are instantiated by the predicate P true of the small-

est set of situations satisfying the formulas below, and the partial

# function, Next(cid:48)(cid:48)(s) = result(a, s) ≡ P (result(a, s)), deﬁned from P ,

P (S0) ∧ P (result(Skislope4, S0))∧

P (result(Down, result(Skislope4, S0)))

∧∀s.s (cid:54)= S0 ∧ s (cid:54)= result(Skislope4, S0)∧

P (result(Wait, s))

s (cid:54)= (result(Down, result(Skislope4, S0)) ∧ P (s) →

The result of instantiating these terms, and noting that the left hand

side is derivable from the theory A gives the sentence.

¬holds(Fallen, result(Down, S1))

However, we also have holds(Skiing4, s1), and the axiom,

∀s.Typeslope4 = Bump ∧ holds(Skiing4, s) →

holds(Fallen, result(Down, s))

Instantiating this with S1 and simplifying gives Typeslope4 (cid:54)= Bump.

As we know by domain closure that

Typeslope4 = Bump ∨ Typeslope4 = Turn,

we have

as required.

Typeslope4 = Turn,

8 Bayesian Networks

This approach can be seen to be similar to modeling systems with

structured equations [Simon, 1953]

[Druzdel and Simon, 1994], or

Bayesian networks [Balke and Pearl, 1994b] [Balke and Pearl, 1994a]

[Pearl, 1995]. Rather than have equations that give the value of a

variable, we have arbitrary propositions. The dependency relation-

ships are captured by the frame, rather than links.

In our model,

exogenous variables are in the frame, as are the functions that give

the value of the other variables. Updating the Bayes net can then be

seen to be updating the frame.

One major diﬀerence between our approach and structural equa-

tion models or Bayesian networks is that we consider arbitrary propo-

sitions, and consider these relative to a background approximate the-

ory. This approximate theory can be rich, that is, not completely

describable. The other major diﬀerence is that Bayesian networks

focus on the probability distribution of certain variables, rather than

on facts in general.

We now brieﬂy sketch Galles’s and Pearl’s formalization of causal

models, their generalization of structural equations.

Deﬁnition: 2 A causal model is a triple M = (cid:104)U, V, F(cid:105), where U is

a set of variables, called exogenous variables, V is a set of variables,

disjoint from U , called endogenous variables, and F is a set of func-

tions fi, from U ∪ (V /vi) to vi for each vi in V . We shall sometimes

represent a function fi by a deﬁning equation for vi.

If X is a tuple of variables in V , and x a tuple of values for the

elements of X, we write X = x, for the set of equations that result

in equating the elements of x with the corresponding value in x.

A sub-model MX of M is the causal model,

where

Mx = (cid:104)U, V, FX(cid:105)

FX = {fi : Vi /∈ X} ∪ {X = x}

If Y is a variable in V , the potential response of Y to equating X

to x is the solution for Y of the equations FX .

The counterfactual X = x (cid:31) Y = y is true if y is the potential

response of Y to equating X to x.

We now show how to represent causal model as a cartesian frame.

Deﬁnition: 3 Let M = (cid:104)U, V, F(cid:105), be a causal model where U =

{u1, . . . , um}, V = (cid:104)v1, . . . , vn(cid:105), and

F = {vi = fi(u1, . . . , um, v1, . . . , vi−1, vi+1, . . . , vn)|i ≤ n}.

The cartesian frame for M is in the equational language, with

variables, ui|i ≤ m and functions, vi of n + m − 1 arguments, and

functions.

The cartesian frame M F is the set of function terms vi, and the

current world M W is the tuple,

f1(u1, . . . , um, v2, . . . , vn),

. . . , fi(u1, . . . , um, v1, . . . , vi−1, vi+1, . . . , vn),

. . . f1(u1, . . . , um, v1, . . . , vn−1).

We now show that X = x (cid:31) Y = y is true in a causal model M if

# and only if X = x (cid:31) Y = y is true in the cartesian frame M F , with

current world M W .

Theorem: 4 X = x (cid:31) Y = y is true in a causal model M if and

# only if (cid:86) X = x (cid:31) Y = y is true in the cartesian frame M F , with

current world M W .

Proof: We prove this by showing that the equations in sub-model

MX are exactly the formulas encoded by the result of updating the

state vector ξ(M F , M W ) by each element of X = x.

The sub-model MX is the causal model,

where

Mx = (cid:104)U, V, FX(cid:105)

FX = {fi : vi /∈ X} ∪ {X = x}

The formulas encoded by a(vi, z, ξ(M F , M W )) are

The formulas encoded by the sequence of assignments is

{fi : vi (cid:54)= z} ∪ {vi = z}.

{fi : vi /∈ X} ∪ {X = x}.

To show this, it is enough to realize that no vi can appear twice in

X, and thus every distinct update updates a distinct variable.

Thus the equations in sub-model MX is exactly the formulas en-

coded. Finally, the conditions for y being a potential response of Y

to equating X to x are exactly the conditions for Y = y following

from the equations,

{fi : vi /∈ X} ∪ {X = x}.

9 An Application of Cartesian Counterfactu-

als

Cartesian counterfactuals have been used to answer challenge ques-

tions in the U.S. DARPA (Defense Advanced Projects Agency) High

Performance Knowledge Bases (HPKB) program.

In this section

brieﬂy explain what this project does, and then we some examples of

how cartesian counterfactuals are used.

The HPKB program is developing technology to build large (10

to 100 thousand axioms), reusable knowledge bases that can answer

questions on a speciﬁc topic. One branch of the project considers

Crisis Analysis. That is, it attempts to answer the kinds of questions

intelligence analysts would ask when a crisis erupted somewhere in

the world.

The HPKB project is competitive. The research groups are divid-

ing into two groups, and build competing systems, which are tested

yearly on their ability to answer newly posed questions about a cho-

sen topic. These questions range from the (seemingly) simple, “What

is the diﬀerence between sarin and anthrax?”, to quite involved coun-

terfactuals.

In order to even meaningfully state the counterfactuals, we need

to give some background information. Here we give an extract from

a scenario, modiﬁed by string substitution to be set in the world of

the Lord of the Rings.

Mordor supplies a hobbit terrorist group, the Hobbit

Liberation Army (HLA), with money, explosives, small

arms, and advisors and oﬀers the use of a training facil-

ity near Gondor. Mordor arranges to acquire weapons-

grade anthrax from a Ranger Maﬁa cell. Mordor con-

tacts an Elvish biological weapons expert, known only as

Feanor, who agrees to supply Mordor with equipment and

weapons-engineering skills to complete the weaponization

of several small biological devices.

...

The orcs of Mordor, aided by the nefarious Elvish biolog-

ical weapons expert, successfully build a small, hobbit-

portable anthrax sprayer. Separately, arrangements are

made with the HLA to begin training exercises in Mordor

at the western terrorist training camp.

...

Although entirely hypothetical, the scenario is very detailed, running

to 15 pages.

Given this scenario, (called the Y1 Phase II LOTR Scenario) the

following counterfactual was posed.

How would the Y1 Phase II Lord of the Rings Sce-

nario be aﬀected if BW experts of Mordor did not pro-

vide advanced technology and scientiﬁc expertise aid to a

terrorist group?

This question can be stated in KIF (knowledge interchange for-

mat) as follows.

(and

(not (occurs-in ?event (scenario-minus

?event1 Y2-scenario)))

Y2-Scenario ))

(occurs-in ?event (day ?n

(supply ?event1)

(object-acted-on ?event1 ?aid)

(indirect-object-to ?event1 ?group)

(terrorist-group ?group)

(expertise ?aid)

(actor ?event1 ?experts)

(citizens-of ?expert Mordor)

(expert ?experts)

(information-about ?aid advanced-technology)

(actor ?event ?actor)

)

In the system built by the one team, to which the Formal Reason-

ing Group is associated, this query is sent to a theorem prover (ATP),

which extracts binding from a knowledge base for the variables (the

names preceded by a question mark). The binding are then trans-

lated into a natural language answer, and the proof into a natural

language explanation of the answer. We will not discuss these parts

further, as they do not bear on the use of cartesian counterfactuals.

The system ﬁnd the correct answer, namely Mordor would not

have been able to weaponize the anthrax, by reasoning that had they

not received the elvish aid, then they would not have been able to

weaponize the material, as biological weapon engineering skills are

necessary for this task. The reasoning is about 40 resolution steps,

and involves 7 rules and 30 ground facts.

In the case of this counterfactual, the frame is the facts true at

the beginning of the scenario, and the events that occur before the

elvish aid arrives. The events that occur later are not in the frame,

as they are not independent of the earlier events.

Counterfactuals are included in the HPKB project as they are a

very natural way to explore an evolving situation. When thinking

about the present, past cases, and counterfactual variants of them

are often employed.

10 Theories admitting counterfactuals

As stated earlier, diﬀerent theories admit diﬀerent counterfactuals.

Our goal is to make this more deﬁnite for common sense theories, but

theories involving diﬀerential equations (and diﬀerence equations) ad-

mit particular kinds of counterfactuals in informative ways. We dis-

cuss them ﬁrst and then move on to common sense theories for which

counterfactuals are more important.

10.1 Theories given by diﬀerential equations

Theories given by diﬀerential equations give some clearcut examples.

The solutions are determined by boundary conditions. If the the-

ory includes both the diﬀerential equations and the boundary con-

ditions, the most obvious counterfactual is to keep the diﬀerential

equations and change the boundary conditions to a diﬀerent set of

admissible boundary conditions.

A simple example is provided by the equations of celestial mechan-

ics regarding the planets as point masses. We can use the equations

to predict the future of the system from the present but with Mars

having a diﬀerent position and velocity than it has at present. We

can also solve the equations supposing a sudden change in the mass

of Mars in this theory.

A more complex theory in which Mars has a distribution of mass,

which might be necessary to predict the future positions of Deimos

and Phobos (the moons of Mars), would not consider a sudden change

of the mass of Mars that didn’t specify how the new mass was dis-

tributed as a deﬁnite alternate initial state.

Other counterfactuals are not meaningful in celestial mechanics,

e.g. what if Mars had a circular orbit. This isn’t meaningful, because

even if Mars started along a circle, you would have to make arbitrary

assumptions in order to keep it there and might end up violating the

law of conservation of momentum.

However, the diﬀerential equations don’t need to involve time

to admit counterfactuals. A theory that gives the distribution of

potential in a region as a function of the distribution on the boundary

can also consider altered values on the boundary.

The same considerations that apply to diﬀerential equations apply

to diﬀerence equations.

What if the deuteron had a mass one percent larger than it does?

For chemistry and the theory of atomic spectra this is a reasonable

counterfactual. For example, its eﬀect on spectra and the rate of

chemical reactions could be predicted. However, this is a meaningless

counterfactual for nuclear physics, because it doesn’t say whether

the extra mass is in the proton or the neutron or somewhere else.

Quantum mechanics doesn’t tolerate giving a proton or neutron a

diﬀerent mass in a particular atom, because it violates the notion of

identical particles required to apply the anti-symmetry rule for wave

functions.

Thus we see that counterfactuals reasonable at one level of theory

may be unreasonable at a more fundamental level.

Such counterfactuals in physics are appropriately handled infor-

mally — as long as the physicists are people. Robotic common sense

reasoning requires a formal treatment.

10.2 Common sense theories

Situation calculus theories are somewhat similar to celestial mechan-

ics in the kinds of counterfactuals they admit. However, diﬀerent

situation calculus formalizations of the same phenomenon may ad-

mit diﬀerent counterfactuals.

Consider two similar theories, the Yale Shooting Problem with

the additional statement what walking implies being alive, and the

Yale Shooting Problem with the additional rule that shooting causes

someone to stop walking (when the gun is loaded). These theories

are equivalent when we add induction, as the domain constraint can

be derived by an induction.

We now consider what might happen if the gun was a pellet gun.

Pellet guns wounds, though painful are not fatal. Thus, we remove

the eﬀect axiom that states shooting kills. In one case, we are left with

the eﬀect axiom that shooting stops someone walking, even though

it does not kill them—a reasonable conclusion. In the other, we are

left with no eﬀects, as the second eﬀect axiom was a consequence of

the domain constraint and the axiom that shooting kills.

Thus how we axiomatize our theory can alter the truth of coun-

terfactuals.

11 Non-cartesian counterfactuals

So far we have considered cartesian counterfactuals. In this section

we consider how we can go beyond this basic case. The essential

restriction that characterizes cartesian counterfactuals is that every

point in the product space is a meaningful state of aﬀairs. Thus,

if we have a frame with co-ordinates x, y and z, we can choose any

values for x, y and z, say nx, ny, and nz, and the theory that we

get when we add x = nx, y = ny, z = nz to our approximate theory

correctly predicts the truth of counterfactuals with premises asserting

the values of x, etc.

Sometimes there are assignments of co-ordinates that are not

meaningful. This can be because the co-ordinates having those values

breaks some rule, or is impossible. In other cases, though those val-

ues of the co-ordinates are not excluded by our approximate theory,

it may be that our approximate theory does not correctly predict the

outcome for those values. Finally, so co-ordinates are not meaningful

given when other co-ordinates have certain values. For instance, a

choice of whether to bend ones knees or not does not make sense

unless you are skiing.

This is made clearer by some examples.

The ﬁrst example shows a case where some values of the co-

In this theory

ordinates are excluded by the approximate theory.

x is the height of the bottom of a spring, and y the height of the

top of the spring, s is then the height of the spring, and f the force

outwards on each endpoint. l is the length of the spring at rest, and

e the elasticity. In this case, Hooke’s Law tells us “ut tensio sic vis”,

i.e. the force is proportional to the extension of the spring, where e

is the proportion, and further, it is in the opposite direction.

Consider the following approximate theory,

x > y

s = x − y

f = −e ∗ (s − l)

l = 2

e = 1

In the current world x = 3, y = 5, so the spring is at its nominal

length.

Then, if we have x, y as our co-ordinate frame, we have that if

x = 1 (cid:31) s = 4, further we have that x = 1 (cid:31) f = −2, that is, the

force on the bottom of the string is 2 units upwards.

What would happen if we moved the bottom of the string to

position 6. In the real world, this would be either impossible, (as we

would try to push something through itself), or possibly we might

end up with a reversed situation.

In either case, the approximate

theory is no longer the case. We can tell this because the theory

makes the following counterfactual true x = 6 (cid:31) ⊥.

However, sometimes we will go outside the range of meaning-

fulness of our approximate theory without reaching a contradiction

inside it. Consider our spring theory above. Suppose that the spring

breaks3 when it is stretched to length 6.

Our theory tells us that y = 10 > f = −10. This is incorrect,

but we cannot tell this from our approximate theory alone. In the

previous case we could detect a problem because of inconsistency. In

this case, we cannot detect the problem in that way.

In both these cases, out theory was cartesian in a certain region,

but outside that region, that simple structure broke down, and the

cartesian structure no longer provided meaningful answers.

12 Tree Structures of Possibilities

To go beyond cartesian counterfactuals, it is useful to consider trees

of possibilities. A node in the tree corresponds to ﬁxing some of the

aspects of the entity being considered and regarding the others as

variable.

To ﬁx the ideas, let the entity be the world and its history—but

there are others.

We consider trees with ﬁnite branching and ﬁnite depth. We may

regard the leaves as possible worlds.

A theory of counterfactuals based on the trees can have more

structure than theories based solely on the possible worlds.

Returning to the adventures of Junior, we consider the node A

from which the possibilities branch according to whether Junior went

surﬁng, went skiing or stayed home. If Junior went skiing, it makes

sense to ask whether he took cheap skiing lessons, expensive skiing

lessons or none. These are edges leading from the node leading from

the edge in which he went skiing. If he went surﬁng, there are no

edges leading from that node corresponding to the diﬀerent skiing

lessons. [Therefore, we don’t have a cartesian product structure, but

we could force one by putting edges for the kinds of ski lessons leading

from the node in which he went surﬁng. We don’t do that.]

3Steel springs will tend to stretch permanently when stretched or deformed

beyond a certain point, but other materials, such as glass will break almost im-

mediately outside their elastic region, especially if pulled quickly.

Now let us suppose that Junior had lunch on the given day, re-

gardless of whether he skied or surfed. If he went skiing or surﬁng, we

suppose that at lunch that he may have had either a hamburger or a

hot dog or a pizza. If he stayed home he had chicken soup. In fact

suppose that he had a hamburger. Now consider the counterfactual,

“If Junior had had a hot dog, he would have had indigestion.” This

counterfactual may be stated either about the skiing node or about

the surﬁng node. Let’s put in another branch on whether Junior

telephoned Deborah or Sheila or neither.

We can imagine the tree of possibilities to have been constructed

in two ways. in one the split on what Junior had for lunch precedes

in the tree the split on whom he telephoned. We say “precedes in the

tree”, because we are not concerned with the temporal order of these

events, ignoring the fact that if he telephoned Deborah he had to

do it before lunch whereas telephoning Sheila would have been done

after lunch.

Let the variable x have the value 1, 2 or 3 according to whether

Junior went skiing, surﬁng or stayed home. Let y have one of these

values according to whether he had expensive lessons, cheap lessons

or none. Let z have values according to what he had for lunch and

w have values according to whom he telephoned.

We can label the edges of the tree. The edges from node A

are labeled, x = 1, x = 2, and x = 3. We can use a notation

for this reminiscent of the notation used for state vectors and write

a(x, 1, A) or more explicitly a(“Junior went skiing”, A). However, in

the tree case, we don’t have all the nice properties of the state vector

case. The edges leading from the node a(x, 1, A) are labelled y = 1,

y = 2 and y = 3. However, the edges leading from a(x, 2, A) and

a(x, 3, A) have no y-labels nor do any of their successors in the tree,

i.e. a(y, 1, a(x, 2, A)) is undeﬁned. If Junior didn’t go skiing, there is

no branch according to what kind of lessons he took.

Here are the trees. Tree 1 puts skiing, surﬁng and home on the

edges leading from A, and Tree 2 puts whom he telephoned on these

edges.

The z and w situations are simpler. Provided the value of x is 1

or 2, the tree may be arranged to put the edges labelled with z before

or after the edges labelled with w. We can write, for example,

(∀αβγ)(γ (cid:54)= 1

→ a(w, β, a(z, α, a(x, γ, A))) = a(w, β, a(z, α, a(x, γ, A))))

This is a local cartesian product case. We can say that the variables

z and w commute. The relation between z and x is more complex,

because telephoning and having hot dogs for lunch arise only when x

is 1 or 2. Also when x = 1, z and w may jointly commute with y.

We say that a pair of co-ordinates x1, x2 is locally cartesian under

(19)

a condition φ, when

φ → ∀v1, v2.a(x1, v1, a(x2, v2, A)) = a(x1, v1, a(x2, v2, A))

(20)

This notion make sense for sets of co-ordinates. A set of co-ordinates

are locally cartesian under a condition φ, when all orders of assign-

ment agree.

We can now write

(∀α)(α = 1 ∨ α = 2) →

Eats(HotDog, a(x, α, A))

(cid:31) Holds(Indigestion, N ext(a(x, α, A)))

The branching tree of propositional possibilities is isomorphic

to the structure of compound conditional expressions discussed in

[McCarthy, 1963].

13 Conclusions

We have shown the usefulness of counterfactuals in theories with a

cartesian product structure, showing how to infer such counterfactu-

als and how to make inferences from them. We also consider more

general tree-structured counterfactuals.

We do not claim there is a single set of true counterfactuals.

Rather, a counterfactual can only be judged relative to a background

theory. In the cartesian case, the theory involves a choice of a “co-

ordinate frame”.

These counterfactuals give information about how the world be-

haves, so that in future situations the reasoner can better predict

what will happen.

To be useful, a counterfactual needs to be imbedded in a theory

that includes goals or a notion of utility.

The theories inhabited by counterfactuals are usually approxi-

mate theories of the world and sometimes involve concepts and ob-

jects that are not fully deﬁned. Approximate theories and approxi-

mate objects and their relationships are discussed in [McCarthy, 2000].

Acknowledgments

A preliminary version of this article was accepted for the 1998 NCAI

but was withdrawn in disagreement with AAAI’s policy of requir-

ing assignment of copyright. This research has been partly sup-

ported by ARPA contract no. USC 621915, the ARPA/Rome Labo-

ratory planning initiative under grant (ONR) N00014-94-1-0775 and

ARPA/AFOSR under (AFOSR) grant # F49620-97-1-0207.

References

[Balke and Pearl, 1994a] Balke, A. and Pearl, J. (1994a). Counter-

factuals and Policy Analysis in Structural Models. In Besnard, P.

and Hanks, S., editors, Uncertainty in Artiﬁcial Intelligence 11,

pages 11–18. Morgan Kaufmann.

[Balke and Pearl, 1994b] Balke, A. and Pearl, J. (1994b). Coun-

terfactuals Probabilities: Computational Methods, Bounds, and

Applications. In de Mantaras, R. L. and Poole, D., editors, Un-

certainty in Artiﬁcial Intelligence 10, pages 46–54. Morgan Kauf-

mann, San Mateo, California.

[Druzdel and Simon, 1994] Druzdel, M. J. and Simon, H. A. (1994).

A structured probabilistic representation of action.

In Proceed-

ings of the Tenth Annual Conference on Uncertainty in Artiﬁcial

Intelligence (UAI-94), pages 154–161.

[Geﬀner, 1992] Geﬀner, H. (1992). Default Reasoning: Causal and

Conditional Theories. ACM Doctoral dissertation award: 1990.

MIT Press.

[Ginsberg, 1986] Ginsberg, M. L. (1986). Counterfactuals. Artiﬁcial

Intelligence, 30.

[Leake and Plaza, 1997] Leake, D. and Plaza, E., editors (1997).

Case-Based Reasoning Research and Development, Proceedings

of the 2nd International Conference on Case-Based Reasoning

(ICCBR-97), Lecture Notes in Artiﬁcial Intelligence, Berlin.

Springer-Verlag.

[Lenz et al., 1998] Lenz, M., Bartsch-Sporl, B., H-D., B., and Wess,

S., editors (1998). Case-Based Reasoning Technology: from foun-

dations to applications. Number 1400 in Lecture Notes In AI.

Springer-Verlag.

[Lewis, 1973] Lewis, D. (1973). Counterfactuals. Harvard University

Press.

[McCarthy, 1962] McCarthy, J. (1962). Towards a mathematical sci-

ence of computation. In Information Processing ’62, pages 21–28.

North-Holland. Proceedings of 1962 IFIP Congress.

[McCarthy, 1963] McCarthy, J. (1963). A Basis for a Mathematical

Theory of Computation4. In Braﬀort, P. and Hirschberg, D., ed-

itors, Computer Programming and Formal Systems, pages 33–70.

North-Holland, Amsterdam.

[McCarthy, 1979] McCarthy, J. (1979). Ascribing mental quali-

ties to machines5.

In Ringle, M., editor, Philosophical Perspec-

tives in Artiﬁcial Intelligence. Harvester Press. Reprinted in

[McCarthy, 1990].

[McCarthy, 1990] McCarthy, J. (1990). Formalizing Common Sense:

Papers by John McCarthy. Ablex Publishing Corporation.

4http://www-formal.stanford.edu/jmc/basis.html

5http://www-formal.stanford.edu/jmc/ascribing.html

[McCarthy, 2000] McCarthy, J. (2000). Approximate objects and

approximate theories6. In Cohn, A. G., Giunchiglia, F., and Sel-

man, B., editors, KR2000: Principles of Knowledge Representation

and Reasoning,Proceedings of the Seventh International conference,

pages 519–526. Morgan-Kaufman.

[McCarthy and Hayes, 1969] McCarthy, J. and Hayes, P. J. (1969).

Some Philosophical Problems from the Standpoint of Artiﬁcial In-

telligence7. In Meltzer, B. and Michie, D., editors, Machine Intel-

ligence 4, pages 463–502. Edinburgh University Press.

[Pearl, 1988] Pearl, J. (1988). Embracing causality in formal reason-

ing. Artiﬁcial Intelligence, 35:259–271.

[Pearl, 1995] Pearl, J. (1995). Causation, Action and Counterfac-

tuals.

In Gammerman, A., editor, Computational Learning and

Probabilistic Reasoning, pages 235–255. John Wiley and Sons, New

York.

[Pearl, 2000] Pearl, J. (2000). Causality: models, reasoning, and in-

ference. Cambridge University Press.

[Pinto and Reiter, 1993] Pinto, J. and Reiter, R. (1993). Adding a

time line to the situation calculus. In Second Symposium on Logical

Formalizations of Commonsense Reasoning, pages 172–177.

[Simon, 1953] Simon, H. A. (1953). Causal ordering and identiﬁa-

bility. In Studies in Econometric Method., number 14 in Cowles

Commission for Research in Economics., monograph III. John Wi-

ley and Sons, Inc., New York.

[Stalnaker, 1968] Stalnaker, R. (1968). A theory of conditionals. In

Rescher, N., editor, Studies in logical theory, number 2 in American

philosophical quarterly monograph series. Blackwell, Oxford. Also

appears in Ifs, (ed., by W. Harper, R. C. Stalnaker and G. Pearce),

Reidel, Dordrecht, 1981.

[Stalnaker and Thomason, 1970] Stalnaker, R. and Thomason, R. H.

(1970). A semantic analysis of conditional logic. Theoria, 36:23–42.

6http://www.formal.stanford.edu/jmc/approximate.html

7http://www-formal.stanford.edu/jmc/mcchay69.html

