OVERCOMING UNEXPECTED

OBSTACLES

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

2001 Apr 14, 3:13 p.m.

Abstract

The present note illustrates how logical formalizations of common

sense knowledge and reasoning can achieve some of the open-endedness

of human common sense reasoning. A plan is made to ﬂy from Glasgow

to Moscow and is shown by circumscription to lead to the traveller

arriving in Moscow. Then a fact about an unexpected obstacle—

the traveller losing his ticket—is added without changing any of the

previous facts, and the original plan can no longer be shown to work

if it must take into account the new fact. However, an altered plan

that includes buying a replacement ticket can now be shown to work.

The formalism used is a modiﬁcation of one developed by Vladimir

Lifschitz, and I have been informed that the modiﬁcation isn’t correct,

and I should go back to Lifschitz’s original formalism. April 14,2001:

I still haven’t done it, so this article has to be regarded as tentative. I

hope to ﬁx the problems without going back to Lifschitz’s formalism,

which I ﬁnd awkward.

Introduction

In contrast to reasoning within a formal theory of the conventional sort used

in science or operations research, common sense reasoning (McCarthy 1959)

is open-ended. More facts than were originally taken into account may turn

out to be relevant. Formalizing common sense requires a formal system that

preserves this open-endedness. It can be done by formalizing nonmonotonic

reasoning.

We present a straightforward example of how a system might take into

account new facts. An unexpected obstacle vitiates the inference that the

usual sequence of actions will achieve a goal. Then, without changing any

existing premise, a system can infer that inserting a suitable new action in

the sequence achieves the goal.

1. We use a general formalism for describing the eﬀects of actions.

It is a variant due to Vladimir Lifschitz (1987) of the situation calculus

(McCarthy and Hayes 1969).

2. Speciﬁc facts concerning travel by airplane from one city to another

are given. The need for a ﬂight to exist and for the traveller to have a ticket

are made explicit preconditions.

3. Facts relevant for ﬂying from Glasgow to Moscow via London are

mentioned, i.e. the ﬂights are mentioned.

4. The circumscription formalism of (McCarthy 1980) and (McCarthy 1986)is used to minimize certain predicates, i.e. precond, noninertial, causes,

occurs while allowing the predicate holds to vary.

5. It can then be inferred (nonmonotonically) that ﬂying from Glasgow

to London and then ﬂying to Moscow results in being in Moscow.

6. Facts giving the consequences of losing a ticket and buying a ticket are

included. They do not change the result of the previous inference.

7. An assertion that the ticket is lost in London is then added to the

previous facts. Now it can no longer be inferred that the previous plan

succeeds. However, it can be inferred that the plan of ﬂying to London, then

buying a ticket and then ﬂying to Moscow does succeed.

This example shows that it is possible to make a formalism that (1) can

be used to infer that a certain plan will succeed, (2) can no longer infer that

the plan will succeed when an obstacle is asserted to exist, (3) can be used

to infer that a diﬀerent plan that includes actions to overcome the obstacle

will succeed.

Our formulas include only the parameters needed to illustrate the reason-

ing. They don’t even include the traveller, i.e. the person whose actions are

reasoned about. From the point of view of demonstrating full common sense

reasoning this is a blemish. However, we believe that the very formulas used

here can be preserved provided we enter a suitable context. Formal reasoning

about contexts is discussed in (McCarthy 1993).

2 The Formulas

Here are the formulas.

holds(not p, s) ≡ ¬holds(p, s)

This relates the operator not as applied to ﬂuents to logical negation.

succeeds(a, s) ≡ (∀p)(precond(p, a) ⊃ holds(p, s)).

This tells us that an action succeeds in a situation s if all its preconditions

hold in the situation. Actually, it’s a deﬁnition of the predicate succeeds.

succeeds(a, s) ∧ causes(a, p) ⊃ holds(p, result(a, s)).

If an action succeeds in a situation and it is one that causes a ﬂuent to hold,

then the ﬂuent holds in the situation that results from the preformance of

the action.

¬noninertial(p, a) ∧ holds(p, s) ⊃ holds(p, result(a, s))

This tells us that unless an action aﬀects a ﬂuent, then the ﬂuent holds after

the action if it held before the action.

occurs(e, s) ⊃ outcome s = outcome result(e, s)

This and the next axiom give the eﬀects of events diﬀerent from actions.

(∀e¬occurs(e, s)) ⊃ outcome s = s

occurs(e, s) ⊃ outcome s = outcome result(e, s)

rr(a, s) = outcome result(a, s)

This is an abbreviation for the situation that results from an action after all

the events that occur after it have happened.

causes(f ly(x, y), at y)

This is the ﬁrst axiom speciﬁcally about the eﬀects of ﬂying. It says that

ﬂying from x to y causes being at y.

precond(at x, f ly(x, y))

You must be at x to ﬂy from there to y.

precond(hasticket, f ly(x, y))

Also you must have a ticket.

precond(existsf light(x, y), f ly(x, y))

And there must be a ﬂight.

The eﬀect of losing a ticket.

causes(loseticket, not hasticket)

The eﬀect of buying a ticket.

causes(buyticket, hasticket)

holds(at Glasgow, S0)

This is the ﬁrst fact about the initial situation S0. The traveller is at Glas-

gow.

holds(hasticket, S0)

He has a ticket in S0

holds(existsf light(Glasgow, London), S0)

holds(existsf light(London, M oscow), S0)

The necessary ﬂights exist.

circum(F acts; causes, precond, noninertial, occurs; holds)

This is the circumscription of the predicates causes, precond, noninertial

and occurs with holds allowed to vary that is done with the conjunction

(called F acts) of these axioms. Understanding this may require reading

(McCarthy 1987); (Lifschitz 1987) would also help. Once the circumscription

has been done, we can show

holds(atM oscow, rr(f ly(London, M oscow), rr(f ly(Glasgow, London), S0))),

but not if we add

occurs(loseticket, result(f ly(Glasgow, London), S0)).

However, in this case we can show

holds(atM oscow, rr(f ly(London, M oscow), rr(buyticket, rr(f ly(Glasgow, London), S0)))).3 Avoiding Considering Preconditions

It is a precondition for air travel without additional actions that one be

clothed, holds(clothed(traveller), s), that one not be lame holds(not lame(traveller), s),and holds(speaks-English(traveller), s), etc. With a bow towards later ex-

plaining how to make this happen using formalized contexts ((McCarthy 1989)

(McCarthy 1991,1992) we’ll abbreviate the above to the propositional ﬂuents

clothed, not lame and speaks-English. In one respect these conditions are

similar to the condition that one have a ticket. However, one is willing to

specify as part of the formalization of air travel that one have a ticket, but

it is not reasonable to refer explicitly to these other conditions.

Here’s an approach to doing it. The simplest approach would be to have a

ﬂuent ab7(traveller), abbreviated ab7, and have sentences not clothed ⊃ ab7,

etc. We then use not ab7 as a precondition for ﬂying. We then circumscribe

ab7. This doesn’t work well enough for two reasons. First we still have to

mention all these other conditions in the circumscription and circumscribe

them also. Second, suppose one of the conditions fails, e.g. the traveller is

lame so a wheelchair must be provided. Then we lose not ab7, and we haven’t

got rid of the other conditions.

At present I think the ﬁrst problem has to be solved by some form of

present resembling the scope of (Etherington et al. 1991). If we circumscribe

it, we are jumping to the conclusion that the interfering phenomena aren’t

present The second problem may perhaps be solved by introducing a parame-

ter exceptions to ab7 and requiring that none of the exceptions be unresolved.

Both of these ideas require details.

Lifschitz, Vladimir (1987): “Formal theories of action”, in: The Frame

Problem in Artiﬁcial Intelligence, Proceedings of the 1987 Workshop, 1987.

4 References

References

Etherington, D. E., S. Kraus, and D. Perlis. 1991. Nonmonotonicity and

the scope of reasoning. Artiﬁcial Intelligence 52:221–262.

McCarthy, J. 1959. Programs with Common Sense1.

In Mechanisa-

tion of Thought Processes, Proceedings of the Symposium of the National

Physics Laboratory, 77–84, London, U.K. Her Majesty’s Stationery Oﬃce.

Reprinted in (McCarthy 1990).

McCarthy, J. 1980. Circumscription—A Form of Non-Monotonic Reason-

ing2. Artiﬁcial Intelligence 13:27–39. Reprinted in (McCarthy 1990).

McCarthy, J. 1986. Applications of Circumscription to Formalizing Com-

mon Sense Knowledge3. Artiﬁcial Intelligence 28:89–116. Reprinted in

(McCarthy 1990).

McCarthy, J. 1987. Generality in artiﬁcial intelligence. Communications

of the Association for Computing Machinery 30:1030–1035. Reprinted in

(McCarthy 1990).

McCarthy, J. 1989. Artiﬁcial Intelligence, Logic and Formalizing Common

Sense4. In R. Thomason (Ed.), Philosophical Logic and Artiﬁcial Intelli-

gence. Kl¨uver Academic.

1http://www-formal.stanford.edu/jmc/mcc59.html

2http://www-formal.stanford.edu/jmc/circumscription.html

3http://www-formal.stanford.edu/jmc/applications.html

4http://www-formal.stanford.edu/jmc/ailogic.html

McCarthy, J. 1990. Formalizing Common Sense: Papers by John Mc-

Carthy. Ablex Publishing Corporation.

McCarthy, J. 1993. Notes on Formalizing Context5. In IJCAI-93.

McCarthy, J., and P. J. Hayes. 1969. Some Philosophical Problems from

the Standpoint of Artiﬁcial Intelligence6.

In B. Meltzer and D. Michie

(Eds.), Machine Intelligence 4, 463–502. Edinburgh University Press.

/@steam.stanford.edu:/u/ftp/jmc/glasgow.tex: begun Sat Apr 14 13:33:30 2001, latexed April 14, 2001 at 3:13 p.m.

5http://www-formal.stanford.edu/jmc/context.html

6http://www-formal.stanford.edu/jmc/mcchay69.html

