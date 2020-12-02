- ACTIONS AND OTHER EVENTS IN SITUATION CALCULUS

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

Abstract

vention one situation at a time.

This article presents a situation calculus for-

malism featuring events as primary and the

usual actions as a special case. Events that

are not actions are called internal events and

actions are called external events. The eﬀects

of both kinds of events are given by eﬀect

axioms of the usual kind. The actions are

assumed to be performed by an agent as is

usual in situation calculus. An internal event

e occurs in situations satisfying an occurrence

assertion for that event.

A formalism involving actions and internal

events describes what happens in the world

more naturally than the usual formulations

involving only actions supplemented by state

constraints. Ours uses only ordinary logic

without special causal implications.

It also

seems to be more elaboration tolerant.

The ﬁrst example is the buzzer with only in-

ternal events and which cannot be treated at

all with state constraints, because the system

never settles down to a steady state.

Our second example is the stuﬀy room sce-

nario. One occurrence axiom states that

when both vents are blocked and the room

isn’t stuﬀy, the event Getstuf f y occurs.

State constraints are unneeded. The stuﬀy

room formalization tolerates an elaboration

asserting that when the room becomes stuﬀy

someone unblocks a vent. If we further add

that someone else then ﬁnds the room cold

and blocks the vent again, we get a system

that oscillates.

The third example is the blocks world.

The nonmonotonic reasoning involves cir-

cumscribing occurrences, changes, and pre-

Then we oﬀer a general viewpoint on the sit-uation calculus and its applications to realworld problems. It relates the formalism of[MH69] which regards a situation as a snap-shot of the world to situation calculus theo-ries involving only a few ﬂuents.

Introduction: Actions and otherevents

This article emphasizes the idea that an action by anagent is a particular kind of event. The idea of eventis primary and an action is a special case. The treat-ment is simpler than those regarding events as naturalactions.

The main features of our treatment are as follows.1. There are the usual eﬀect axioms involving thefunction Result(e, s), the situation that results whenevent e occurs in situation s.

2. There are occurrence axioms giving conditions foran event to occur. They have the form conditions(s) →occurs(e, s).

3. The theory distinguishes between external eventsfor which occurrence axioms are not given and internalevents governed by occurrence axioms. Older treat-ments of situation calculus often do not provide forinternal events. Usually human actions are properlytreated as external events, but if the theory containsassertions that a person will perform a certain action,then such an assertion can be given by an occurrenceaxiom, and the action is an internal event. We in-clude an example of an elaboration of the theory ofGinsberg’s stuﬀy room scenario [GS88] that uses anoccurrence axiom to assert that a person will unblocka vent when the room becomes stuﬀy. Thus an actiondetermines what event will occur.

As an example, three of the axioms of the stuﬀy roomphenomenon are

Holds(Blocked1, s) ∧ Holds(Blocked2, s)∧¬Holds(Stuf f y, s)

→ Occurs(Getstuf f y, s),(2)Holds(Stuf f y, Result(Getstuf f y, s)),Holds(Blocked1, Result(Block1, s)).

andGetstuf f y is an internal event and occurs all by itselfwhen the vents are blocked.

We use circumscription to minimize occurrences, tominimize change (frame problem), and to minimize theﬂuents that prevent actions and other events (qualiﬁ-cation problem).

Treating internal and external events by the same for-malism admits elaborations that turn some instancesof external events into internal events. Thus we canelaborate the stuﬀy room scenario by adjoining an oc-currence axiom saying that when the room becomesstuﬀy, someone unblocks a vent, which makes the roomunstuﬀy. The further elaboration that when a vent isunblocked, someone blocks it again, perhaps from feel-ing cold, causes the system to oscillate and never settledown.

An external event can create a situation in which theoccurrence axiom for an internal event is satisﬁed.This leads to a new situation in which a new internalevent can occur. When no more internal events occurthe process settles down, and we can infer a statementabout the resulting stable state. Stable states are usu-ally characterized by state constraints. In physics thesestates often minimize potential energy.

The next three sections discuss examples, a buzzerwhich has only internal events, the stuﬀy room sce-nario, and the blocks world.

3 Formalizing a buzzer

- can be either an external or internal event. Elaborat-

- ing the theory by giving an occurrence axiom for the

- action makes it an internal action in the elaborated

- theory.

- 4. Our theories are nonmonotonic and minimize cer-

- tain predicates situation by situation. The approach

- is proposed only when information about the future

- is obtained only by projection from earlier situations.

- Thus it is not appropriate for the stolen car scenario.

- We use internal events instead of state constraints in

- the stuﬀy room example. Thus we say that when the

- vents are blocked, the room becomes stuﬀy rather than

- regarding stuﬃness as a state constraint. This is closer

- to human common sense reasoning and natural lan-

- guage usage, as well as being logically simpler.

- We begin with formalizing a buzzer which has only

- internal events, continue with the stuﬀy room scenario

- which has both. Our third example is the blocks world.

- After these examples, we discuss the nonmonotonic

- reasoning.

- Then we oﬀer a general viewpoint on the situation cal-

- culus and its applications to real world problems. It

- relates the formalism of [MH69] which regards a situ-

- ation as a snapshot of the world to situation calculus

- theories involving only a few ﬂuents.

- 2 The situation calculus formalism

- Situations are denoted by the letter s decorated with

- subscripts. Constants like S0 are capitalized, and vari-

- ables are lower case. However, we do not assume that

- all situations are generated from a big bang situation

- S0 as does Reiter [Rei01].

that

the

- The sentence Holds(pf luent, s) asserts

- propositional ﬂuent pf luent holds in the situation s.

- Sometimes we write just pf luent(s), but the nota-

- tion with Holds allows quantifying over ﬂuents. We

- also have term ﬂuents, and V alue(tf luent, s) gives the

- value of tf luent in the situation s. We also sometimes

- write just tf luent(s).

- Result(e, s) denotes the situation that arises when the

- event e occurs in the situation s. In this simple for-

- malism, neither situations nor events have durations.

- Occurs(e, s) is the assertion that the event e occurs in

- the situation s.

- N ext(s) is the next situation after s. It is deﬁned by

Figure 1: A buzzer.

Occurs(e, s) → N ext(s) = Result(e, s)

(1)

- for those situations in which an occurrence assertion

Figure 1 displays a buzzer consists of a relay connectedto a battery by a switch that is opened when the relay- operates. If the switch is on, the relay operates and

- opens the switch which turns oﬀ the relay which closes

- the switch. Thus the circuit oscillates and never settles

- down to a stable state.

- The buzzer formalization has only internal events—at

- least once it is started, and this makes its operation

- easy to formalize.

- State constraint axioms for formalizing a buzzer anal-

- ogous to those often used for the stuﬀy room scenario

- would be immediately contradictory, asserting that the

- relay is on if and only if it is oﬀ. Our present situation

- calculus formalism follows human common sense rea-

- soning directly and requires no special causal formal-

- ism or logic with implications not equivalent to their

- contrapositives.

- There are eﬀect axioms and occurrence axioms. The

- former are well known and give the eﬀects of events.

- The latter assert that in situations in which certain

- ﬂuents hold, certain events will occur.

- We distinguish between the ﬂuent On(Sw) asserting

- that the switch is on and the event Onn(Sw) that turns

- the switch on. The ﬂuent holding in a situation is

- asserted by Holds(On(Sw), s). Likewise for the ﬂuent

- On(R) and the event Onn(R) that concern the relay.

- We also have Oﬀ and Oﬀf for the switch and the relay.

- Eﬀect axioms:

e = Onn(R) ∨ e = Oﬀf (R)

→ Holds(On(Sw), Result(e, s))≡ Holds(On(Sw), s)).(6)These frame assertions tell what doesn’t change. Theyare few enough in this case, since there are few actionsand few ﬂuents. In general it is more eﬃcient to saywhat does change. In this case we have

Changes(Onn(R), On(R), s),

Changes(Of f f (R), On(R), s),

Changes(Onn(Sw), On(Sw), s),

Changes(Of f f (Sw), On(Sw), s).and(7)In section 6 we describe how to get the frame assertionsby circumscribing Changes(e, f, s).

Let an initial situation, called S0, be given by¬Holds(On(Sw), S0) ∧ ¬Holds(On(R), S0)(8)We can proceed a step at a time. We haveOccurs(Onn(Sw), S0)

(9)in accordance with (4). Hence

N ext(S0) = Result(Onn(Sw), S0),(10)and therefore, letting

S1 = N ext(S0),

(11)Holds(On(R), Result(Onn(R), s))

¬Holds(On(R), Result(Oﬀf (R), s))

Holds(On(Sw), Result(Onn(Sw), s)

¬Holds(On(Sw), Result(Oﬀf (Sw), s)).

(3)

we have

- Occurrence axioms:

¬Holds(On(Sw), s) ∧ Holds(On(R), s)

→ Occurs(Oﬀf (R), s)

Holds(On(Sw), s) ∧ ¬Holds(On(R), s)

→ Occurs(Onn(R), s))

Holds(On(R), s) ∧ Holds(On(Sw), s)

→ Occurs(Oﬀf (Sw), s)

¬Holds(On(R), s) ∧ ¬Holds(On(Sw), s)

→ Occurs(Onn(Sw), s)

- Note that each of the above occurrence axioms has a

- second term in the precondition. They are needed to

- avoid unwanted concurrent events.

- Frame assertions—for now axioms:

¬Holds(On(R), S1) ∧ Holds(On(Sw), S1).(12)Some elaborations of the buzzer axioms will be worthdoing.

1. Allow the action of stopping the buzzer to occur atany situation.

(4)

2. Consider the action of stopping the buzzer as aconcurrent event.

3. A concurrency elaboration along the lines of[McC92] and [MC98] might be to have two non syn-chronized buzzers B1 and B2 with no guaranteed tem-poral relation between the events involving B1 and B2.4 The stuﬀy room scenario

e = Onn(Sw) ∨ e = Oﬀf (Sw)

→ Holds(On(R), Result(e, s))

≡ Holds(On(R), s)).

(5)

A problem arises when the well-known stuﬀy room sce-nario is formalized with a state constraint that whenboth vents are blocked by pillows the room is stuﬀyand changes in ﬂuents are minimized. This can lead- to the unintended model that when one vent is already

- blocked the action of blocking the other event causes

- the blocked vent to become unblocked in order to min-

- imize change. Some complication of the formalism is

- required to deal with the phenomenon. Direct for-

- malization in terms of actions and events avoids the

- diﬃculty. Also it corresponds better to the way we

- humans think about the problem, i.e. we think about

- the room becoming stuﬀy.

- We use ﬂuents Blocked1, Blocked2, and Stuﬀy. We

- have the action events Block1, Unblock1, Block2,

- Unblock2 and the internal events Getstuﬀy and

- Ungetstuﬀy. 1

- Eﬀect axioms:

Holds(Blocked1, Result(Block1, s))

Holds(Blocked2, Result(Block2, s))

¬Holds(Blocked1, Result(U nblock1, s))

¬Holds(Blocked2, Result(U nblock2, s))

Holds(Stuf f y, Result(Getstuf f y, s))

¬Holds(Stuf f y, Result(U ngetstuf f y, s))

(14)

- Occurrence axioms:

- Holds(Blocked1, s) ∧ Holds(Blocked2, s)

∧¬Holds(Stuf f y, s)

- (¬Holds(Blocked1, s) ∨ ¬Holds(Blocked2, s))

→ Occurs(Getstuf f y, s)

and

∧Holds(Stuf f y, s)

→ Occurs(U ngetstuf f y, s)

(15)

- The frame axioms are

Changes(Block1, Blocked1, s),

Changes(Block2, Blocked2, s),

Changes(U nblock1, Blocked1, s),

Changes(U nblock2, Blocked2, s),

Changes(Getstuf f y, Stuf f y, s),

Changes(U ngetstuf f y, Stuf f y, s).

and

- 1One of the referees suggested that using the ﬂuents

- Blocked1 and Blocked2 and the corresponding actions was

- too special, and we should say that the room is stuﬀy when

- all the vents are blocked. We can accommodate his pref-

- erence by introducing the vents as objects and using the

- axiom

(∀vent)(Holds(Blocked(vent), s))

→ Occurs(Getstuf f y, s)

(13)

- and a corresponding axiom for the eﬀect of unblocking

- a vent.

- This is just a step towards a general commonsense the-

- ory of the eﬀects of ventilation on stuﬃness. Such a the-

- ory would have to take into account the fact that blocking

- the vents does not make the room stuﬀy under all circum-

- stances. For now it’s simpler to just consider the particular

- room with exactly two vents.

How they work is described in section 6.

We need to distinguish between internal events likeGetstuf f y and external events like Block1. As weshall see, an external event may be an internal eventof a more comprehensive narrative, e.g. one in whichBlock1 occurs when Mike is annoyed by cold air com-ing from V ent1.

We can tell a simple sequential story by ﬁrst describingS0, e.g. by

¬Holds(Blocked1, S0) ∧ ¬Holds(Blocked2, S0)∧¬Holds(Stuf f y, S0).

We can now write the narrative

S1 = Result∗(Block1, S0)

S2 = Result∗(Block2, S1)

S3 = Result∗(U nblock2, S2)

S4 = Result∗(Block2, S3),

etc.Here Result∗(e, s) is like the Rr of [McC95]. It is theresult of doing a followed by the occurrence of what-ever internal events occur. The assumption is thatsome sequence of internal events will occur after whichthe situation remains the same until another externalevent occurs. Result∗(e, s) is undeﬁned in the buzzerexample in which internal events occur forever.Result∗ requires an induction axiom or schema. Here’sone candidate:

P (s) ∧ (∀s e)(P (s) ∧ Occurs(e, s) → P (Result(e, s)))→ P (Result∗(e, s)).

(16)

The function N ext∗ has the same relation to Result∗that N ext has to Result. It gives the next situation towhich no occurrence assertion applies. N ext∗ satisﬁesResult∗(e, s) = N ext∗(Result(e, s)),

(∀e)(¬Occurs(e, s)) → N ext∗(s) = s,

Occurs(e, s) → N ext∗(s) = N ext∗(Result(e, s)).andIn the present case we will have

S1 = Result∗(Block1, S0) = Result(Block1, S0).because

event will

Result(Block1, S0). However, we’ll have

internal

no

occurinS2 = Result∗(Block2, S1)

= Result(Getstuf f y, Result(Block2, S1)),(17)(18)(19)(20)(21)- because now the internal event Getstuf f y will oc-

- cur.

Thus we’ll have ¬Holds(Stuf f y, S1) but

- Holds(Stuf f y, S2), ¬Holds(Stuf f y, S3),

- and Holds(Stuf f y, S4).

Still more brieﬂy

- We can write

S4 = N ext∗(N ext∗(N ext∗(N ext∗(S0))))= Result∗(Block2, Result∗(U nblock2,Result∗(Block2, Result∗(Block1, S0))))(26)- S4 = Result∗(Block2, Result∗(U nblock2,

Result∗(Block2, Result∗(Block1, S0))))

4.2 Two elaborations of the stuﬀy room- = Result(Getstuf f y, Result(Block2,

scenario

Result(U ngetstuf f y, Result(U nblock2,

Result(Getstuf f y, Result(Block2,

Result(Block1, S0))))))),

(22)

- which can also be written

The ﬁrst elaboration says that when Pat ﬁnds theroom stuﬀy he unblocks vent2. We have

Holds(Stuf f y, s) → Occurs(Does(P at, U nblock2), s),(27)- S4 = Result∗(Block1; Block2; U nblock2; Block2, S0)

or, more elaborately,

= Result(Block1; Block2; Getstuf f y; U nblock2;

U ngetstuf f y; Block2; Getstuf f y, S0).(23)

- Here we extend the meaning of Result to allow a se-

- quence of events as an argument.

- 4.1 Telling stories using Occurs and N ext

- Another way of telling stories is to always use Occurs.

- An external event is axiomatized by asserting that it

- occurs.

- The above story is then given by

Occurs(Block1, S0)

S1 = N ext(S0) = Result(Block1, S0)

Occurs(Block2, S1)

S1(cid:48) = N ext(S1) = Result(Block2, S1)

Occurs(Getstuﬀy, S1(cid:48)), by inference

S2 = N ext(S1(cid:48)) = Result(Getstuf f y, S1(cid:48))

Occurs(U nblock2, S2)

S2(cid:48) = N ext(S2) = Result(U nblock2, S2)

Occurs(U ngetstuf f y, S2(cid:48))by inference

S3 = N ext(S2(cid:48)) = Result(U ngetstuf f y, S2(cid:48))

Occurs(Block2, S3)

S3(cid:48) = N ext(S3) = Result(Block2, S3)

Occurs(Getstuf f y, S3(cid:48))by inference

S4 = N ext(S3(cid:48)) = Result(Getstuf f y, S3(cid:48)).

Holds(Stuf f y, s) ∧ ¬Holds(U ncomf orable-P at, s)→ Occurs(Becomes-U ncomf ortable(P at), s),Holds(U ncomf ortable, P at,

Result(Becomes-U ncomf orable(P at), s))Holds(U ncomf ortable, P at, s)

→ Occurs(Does(P at, U nblock-V ent2), s),¬Holds(Blocked2, Result(Does(P at, U nblock-V ent2), s)).(28)(24) remains the same except that perhaps we shouldchange the notation so that instead of S3 and S3(cid:48) wewrite S2(cid:48)(cid:48) and S2(cid:48)(cid:48)(cid:48), since these are now intermediatesituations. The situation S4 is now unstable.Now let’s add a second elaboration in which Mike ﬁndsthe room cold when there is an unblocked vent andblocks vent2. It is expressed by adding

Holds(U nstuf f y, s) → Occurs(Does(M ike, Block2), s).(29)With both of these elaborations, we get an oscillation;Pat unblocks vent2 and Mike blocks it again. Result∗and N ext∗ are no longer deﬁned.

5 The blocks world

(24)

Assume enough unique names axioms.

- We can also write the story more brieﬂy as

Occurs(Block1, S0)

S1 = N ext∗(S0) = Result∗(Block1, S0)

Occurs(Block2, S1)

S2 = N ext∗(S1) = Result∗(Block2, S1)

Occurs(U nblock2, S2)

S3 = N ext∗(S2) = Result∗(U nblock2, S2)

Occurs(Block2, S3)

S4 = N ext∗(S3) = Result∗(Block2, S3)

(25)

The blocks world involves the frame problem in a moresigniﬁcant way than do the buzzer and the stuﬀy roomscenarios.

We use the predicate P revents(p, e, s) to say that amove is prevented by there being a block on top ofthe block to be moved or on the destination unless thedestination is the table. We thereby skip the use of theﬂuent Clear(x) prevalent in many blocks world sitcalctheories.

(30)

(31)

(33)

(34)

- Here’s the eﬀect axiom for moving a block.

- (∀p)(¬(P revents(p, M ove(x, y), s) ∧ Holds(p, s)))

→

- (Holds(On(x, y), Result(M ove(x, y), s)))

- ∧

- ((Holds(On(x, z), s)) ∧ z (cid:54)= y

→ ¬Holds(On(x, z), Result(M ove(x, y), s)))),

- and here are the axioms for prevention:

- P revents(On(z, x), M ove(x, y), s)

- y (cid:54)= T able → P revents(On(z, y), M ove(x, y), s).

and

- We adopt the usual way of emphasizing the frame

- problem by introducing the action of painting a block

- a certain color. Thus

- (∀p)(¬(P revents(p, P aint(x, c), s) ∧ Holds(p, s)))

→ Holds(Color(x, color), Result(P aint(x, color), s)),

(32)

- or, using object valued, i.e. non propositional, ﬂuents,

of the event that led to the situation. We are givingup the possibility of trading and abnormality in onesituation for an abnormality in another.

Doing the nonmonotonic reasoning in situations suc-cessively corresponds to the way people predict theconsequences of sequences of actions and events.Itseems to give the same conclusions as Yoav Shoham’schronological minimization [Sho88] but is computa-tionally more straightforward. Like chronological min-imization, it avoids the Yale shooting problem and itsfriends.2

However, we advocate this only for projection prob-lems, i.e. reasoning about the future from informationabout the past. The method is not appropriate for thestolen car scenario in which one has to reason froman assertion (that the car is missing) about a latersituation. 3

With the present formalism, the person or agent set-ting up the problem must know that projection for-ward in time is appropriate. It would be better if thiswere a consequence of the formalized facts.Now let’s consider circumscribing at each situationseparately. The simplest case is when we have a pred-icate F oo(x, y, s).

- (∀p)(¬(P revents(p, P aint(x, c), s) ∧ Holds(p, s)))

→ V alue(Color(x),

Result(P aint(x, color), s)) = color.

We write the axioms

- The change axioms for the blocks world are

Changes(P aint(x, c), Color(x), s),

Holds(On(x, z), s)

→ Changes(M ove(x, y), On(x, z), s)

∧Changes(M ove(x, y), On(x, y), s).

F oo(cid:48) ≤s F oo ≡ (∀x y)(F oo(cid:48)(x, y, s) → F oo(x, y, s)),(F oo(cid:48) <s F oo) ≡ (F oo(cid:48) ≤s F oo) ∧ ¬(F oo(cid:48) =s F oo),F oo(cid:48) =s F oo ≡ (∀x y)(F oo(cid:48)(x, y, s) ≡ F oo(x, y, s)).(35)Then the circumscription of F oo(x, y, s) takes the form- The nonmonotonic reasoning associated with the

- blocks world will be discussed after the section dealing

- with nonmonotonic reasoning in situation calculus in

- general.

Axiom(F oo, vars, s) ∧ (∀f oo(cid:48) vars(cid:48))(Axiom(f oo(cid:48), vars(cid:48))→ ¬(f oo(cid:48) <s F oo)).

(36)Here vars stands for a list of the entities being variedas F oo is minimized.

- 6 Nonmonotonic reasoning—situation

by situation

- We use circumscription to minimize the events that

- occur in a situation, the ﬂuents that might prevent

- an event from having its standard eﬀect, and the

- changes in ﬂuents.

In contrast to the formalism of

- [McC86] which minimized predicates over all the ar-

- guments, we minimize for each successive situation

- separately. However, in doing this minimization in

- s we take as ﬁxed the Holds(f, s) sentences and the

- V alue(exp, s) = . . . sentences inferred from the eﬀects

2The ideas of internal and external events of the pre-ceding sections are independent of the formalism used fornonmonotonic reasoning. For example, Golog [Rei01] orthe Causal Calculator [aA01] could be used—perhaps withsome modiﬁcations for the buzzer and the oscillating stuﬀyroom.

3Actually part of the stolen car scenario can be treatedprovided we don’t suppose that the car being missing is tobe projected from information about the past. Certainlywe can go forward from the situation in which the car ismissing to further events in the future. Likewise, in thestory of Junior’s travels [McC92], we can assert that Juniorloses his ticket to Moscow in London and reason forwardfrom that fact.

- This spells out to

Axiom(F oo, vars, s) ∧ (∀f oo(cid:48) vars(cid:48))

(Axiom(f oo(cid:48), vars(cid:48)) ∧ ((∀x y)(f oo(cid:48)(x, y, s)

→ F oo(x, y, s))

→ (∀xy)(F oo(x, y, s) ≡ f oo(cid:48)(x, y, s)))).

(37)

- Call this formula Circ(Axiom; F oo; vars; s). This is

- the notation of [Lif94] with the addition of the argu-

- ment s to say that s is kept ﬁxed.

(38)

(39)

- The general frame axioms are

- ¬Changes(e, p, s)

- for propositional ﬂuents and

- ¬Changes(e, f, s)

→ (Holds(p, Result(e, s)) ≡ Holds(p, s))

→ V alue(f, Result(e, s)) = V alue(f, s).

- for general ﬂuents.

- Suppose we allow complex ﬂuents, say p And q when

- p and q are propositional ﬂuents. We then need an

- axiom

Changes(e, p, s) ∨ Changes(e, q, s)

→ Changes(e, p And q, s).

(40)

- Similar axioms are required for the other propositional

- functions of ﬂuents and for the compositions of non-

- propositional ﬂuents.

- [This leads to diﬃculties when we want to delimit what

- changes, since there are arbitrarily complex composi-

- tions of ﬂuents. We’ll conﬁne ourselves to elementary

- ﬂuents for now by not putting compositions in the lan-

- guage.]

- In these circumscriptions we also minimize Holds.

- This tolerates elaborations like

- Holds(W eak, s) → P revents(W eak, M ove(x, y), s).

(41)

- If Holds(W eak, s) isn’t asserted, M ove(x, y) will not

- be prevented.

- Lin and Shoham, [LS95] consider a theory of action

- to be provably correct if doing the nonmonotonic rea-

- soning results in a complete nonmonotonic theory of

- the action. This seems like a worthy goal, but I don’t

- know if the present theory achieves it.

- 7 Actions and other events

- The previous sections presented a formalism adequate

- for the examples discussed. In this section we discuss

the situation calculus in general and its connectionwith the real world. We also discuss relations betweendiﬀerent situation calculus theories, e.g.theories atdiﬀerent levels of detail, 4 with actions by agents asa special case. Thus an action term a is consideredan abbreviation of the event term Does(person, a).Besides eﬀect axioms formalizing Result(e, s) [do(e, s)in Canada and its colonies], there are occurrence ax-ioms asserting that in situations satisfying certain ex-pressions in the ﬂuents, an event e occurs—writtenOccurs(e, s). 5

Before giving eﬀect and occurrence axioms, we presentsome general considerations concerning situation cal-culus and its applications.

7.1 Situation calculus and the real worldThere have been many formulations of situation cal-culus.

[MH69] regarded a situation as a snapshot of the worldat some instant of time. Such a system could not beknown and described completely, but a person or pro-gram could know facts about a situation, i.e. the val-ues of some ﬂuents, and could infer some consequencesof some actions from these facts. Situations are exam-ples of rich entities, i.e. entities involving more detailthan can be speciﬁed. Poor entities have ﬁnitely de-scribable structures.

However, theories of action and change6 often use amore limited notion of situation. Thus Raymond Re-iter [Rei01] and his colleagues regard situations as thenodes of a tree based at an initial situation S0 andwhose edges branching from a situation s are the ac-4[McC59] proposed mathematical logic as a tool for rep-resenting facts about the consequences of actions and usinglogical reasoning to plan sequences of actions that wouldachieve goals. Situation calculus as a formalism was pro-posed in [McC63] and elaborated in [MH69]. The name“situation calculus” was ﬁrst used in [MH69] but wasn’t de-ﬁned there. [McC86] proposed to solve the frame and quali-ﬁcation problems by circumscription, but the proposed so-lution to the frame problem was incorrect.[Sha97] and[Rei01] describe several situation calculus formalisms andgive references.

5I suspect I need to pound the table a little here. Ac-tions are just a kind of event, and formalized reasoningabout actions and change need to treat events as the gen-eral case and those events which are actions as special.This has long seemed obvious to me, but I ﬁnd that manyother researchers don’t want to use the same formalism forevents that are not actions of agents and those which are.The consequence has been the introduction of extensionsto logic for treating what are called domain constraints,most of which are better treated by formalizing events.6“events and change” would be better terminology- tions that may be taken in s. Other researchers, in-

- cluding Murray Shanahan [Sha97] and myself, use S0

- as just a name for some situation whose consequences

- are of interest.

- The viewpoint of this article is that a situation s is

- arbitrary element of a space Sits of situations, i.e. s

- bears the same relation to Sits as a group element

- bears to a group. Situation calculus theories relate

- situations, ﬂuents and action by axioms, i.e. are ab-

- stract structures satisfying the theory.

- A robot can use a poor situation calculus theory T to

- decide what to do in a world of rich situations. For

- example, the robot’s blocks world theory may only al-

- low specifying that one block is on another, not where

- it is located on the other. Suppose we have a map-

- ping Observe from a subset of rich situations to poor

- situations. When the robot observes a world situa-

- tion s to which the theory T applies, it obtains a poor

- situation Observe(s) ∈ Sits(T ). Using the theory T ,

- the robot infers that a certain action a will advance

- its goal.

It then performs an action Execute(a) in

- the world. If the theory T corresponds to the world

- properly, Result(Execute(a), s) will be an improved

- situation.

- It isn’t the purpose of this paper to develop a theory of

- the correspondence between rich real world situations

- and those of limited sitcalc domains. However, the

- way we formalize sitcalc is motivated by the hope of

- making these correspondences in a later theory.

- Whether an event is external depends on the theory.

- If we can formulate when an event e will occur, then

- we can make our theory more powerful by including

- an occurrence axiom for that event. If we assume a

- deterministic world, the limiting case is a theory in

- which all events are internal.

- 8 Elaboration tolerance

- An important feature of human common sense is that

- human knowledge of a phenomenon is often readily

- elaborated to take new information into account. It is

- important that logical theories of common sense phe-

- nomena also have this property.

[McC99] has a de-

- tailed discussion.

- Situation calculus theories beneﬁt from several kinds of

- elaboration. Section 4 discusses elaborating the stuﬀy

- room theory by adding occurrence axioms for a per-

- son being motivated to open a vent when the room be-

- comes stuﬀy. [McC92] constructs a theory of a persons

- travel planning which can be elaborated by adding a

- sentence asserting that he loses his airplane ticket at

a certain point in his journey. Because the reasoningdepends on minimizing occurrences, we can no longerconclude that the original travel plan will succeed.In general, elaboration tolerance concerns making iteasy to modify a theory, but the simplest kind of elab-oration is to add one or more sentences to an existingtheory. It is desirable that elaborations be doable inthis way as much as possible. [McC99] discusses whenthis can and cannot be done for a given theory and howto make theories for which elaboration by conjoiningsentences is possible. Theories expressed in naturallanguage have this kind of elaboration tolerance to ahigh extent.

9 Extensions of the formalism andproblems they present

The basic situation calculus admits many useful ex-tensions. The ideas of this section are tentative.9.1 Concurrency

There are two limiting cases of concurrency that canbe treated in the situation calculus.

Easy concurrency:

Two or more events, say e1 and e2 occur in a situations and result in the same next situation N ext(s). Theﬂuents that hold in N ext(s) are those determined bythe eﬀect axioms for e1 and e2 separately. Thus if wemove a block and paint it concurrently, it will haveboth the new location and the new color in N ext(s).General concurrency:

Two processes, starting, say from initial situations S0and S0(cid:48) take place and aﬀect diﬀerent sets of ﬂuents.If nothing is said about the timing of the processesand no axioms of interaction are given, nothing canbe inferred about the relative timing of the processes.Moreover, what can be inferred about the values of theﬂuents in successive situations is exactly what can beinferred by the processes taken separately. Thus LouisPasteur was elected to the French Academy of Sci-ences in 1862 concurrently with certain battles of theAmerican Civil War, but historians mention neitherprocess in connection with the other. This is a limit-ing case, i.e. the case of zero interaction. Two theoriesof separate processes can be combined by taking theconjunction of their axioms. The combined theory is aconservative extension of each separate theory. It canbe useful to elaborate the combined theory by givingaxioms for the interaction. [McC95] and [MC98] treatelaborating theories of two non-interacting processes- by adding axioms of interaction. Those articles treat

- Junior traveling in Europe and Daddy stacking gold

- blocks in New York. There is no interaction until we

- adjoin assertions about Junior losing an airplane ticket

- and asking Daddy for money, thus forcing Daddy to

- sell one of the blocks he was stacking.

- We hope to combine the ideas of the two above-

- mentioned articles with those of this article in future

- work.

- 9.2 Events whose occurrence depends on the

past

- Suppose we want George to unblock both vents when

- the room becomes stuﬀy. When he has unblocked one

- vent, the room becomes unstuﬀy, so the physical sit-

- uation is as it was when he blocked the ﬁrst vent, so

- he needs to remember that the room was previously

- stuﬀy. We can make occurrences depend on past situ-

- ations by adding for each event e an additional eﬀect

- axiom

P ast(Result(e, s)) = s.

(42)

- Notice that P ast(P ast, s)) is the situation two events

- back.

- We can have George unblock Vent1 after he has un-

- blocked Vent2 and the room has become unstuﬀy by

- introducing the occurrence axiom

- Stuf f y(P ast(P ast(s)) → Occurs(U nblock1, s).

- The history as just described does not say what events

- occurred. This information is provided by having for

- each event e the axiom

Lastevent(Result(e, s)) = e.

(44)

- Notice that this formalization is noncommittal as to

- whether the information is in an actor’s memory.

- This seems neat, and maybe it will be useful.

- 9.3 “Branching time” and “linear time”

- We can tell a story by saying what occurs in

- each situation.

In some situations what occurs

- is determined by an occurrence action and de-

- pends on the ﬂuents holding in the situation.

In

- other situations, we simply provide an axiom, e.g.

- Occurs(Birth(Benjamin-Franklin), S1806). This is a

- linear time theory.

- However, linear time and branching time are some-

- times appropriately used together. Suppose we wish

to say that the actor will take the low road or the highroad according to which will get him to Scotland ﬁrst.We can write

if [Arrival-time(Result(Take-Low-Road, s))≤ Arrival-time(Result(Take-High-Road, s))]then Occurs(Take-Low-Road, s)

else Occurs(Take-High-Road, s).

(45)Here we have used a branching time criterion for alinear time action.

This is not as elaborate as actual human behavior inwhich mental events occur calculating which route willlead to earliest arrival.

9.4

Induction in the situation calculusSeveral kinds of mathematical induction seem to be re-quired. For example, one may want to prove a propo-sition P (N ext∗(s)) by showing that it is true for s andis preserved by the events that occur between s andN ext∗(S). A related kind of induction is needed toprove that something is true for all situations arisingin the operation of a buzzer. The simplest case of theN ext∗ induction might be to show that a block un-moved by each of a sequence of events is in the sameposition in N ext∗(s).

The simplest situation calculus is Reiter’s [Rei01]. Theformula is

(43)

[P (S0)∧((∀a s)(P (s) → P (Result(a, s))))] → (∀s)P (s).Here are two formulas

[P (s) ∧ ((∀e s)(P (s) ∧ Occurs(e, s) → P (N ext(s))))]→ P (N ext∗(s)).

(47) is appropriate when N ext∗(s) is deﬁned.When N ext∗(s) is not deﬁned, as in the buzzer case,we can use s ≤ s(cid:48) to mean that s(cid:48) is a distant successorof s and have the axiom.

[P (s) ∧ s ≤ s(cid:48)

→ P (s(cid:48)).

∧((∀e s)(P (s) ∧ Occurs(e, s) → P (N ext(s))))](46)(47)(48)9.5 Formalizing Oscillations

The buzzer oscillates, i.e. the situation repeats againand again. So does the stuﬀy room scenario with thetwo elaborations that cause Vent2 to become blocked- and unblocked repeatedly. However, we don’t need a

- complete repetition of the situation to have oscillation.

- Suppose, or example, we add a clock to the buzzer, a

- natural number valued ﬂuent that each event incre-

- ments by 1. Then although the whole situation would

- not repeat, we would still want to consider the system

- as oscillatory.

- This suggests a relative notion of oscillatory, i.e. oscil-

- latory with respect to certain ﬂuents.

- Moreover, we would like to consider the buzzer as oscil-

- lating even if we provide for it stopping its oscillation

- by being turned oﬀ.

- As we have described the buzzer, it cannot be turned

- oﬀ.

Likewise the stuﬀy room process cannot be

- changed once we have added the elaborations about

- people blocking and unblocking the vent. See (27) and

- (29).

- Here’s a way of putting interventions into the formal-

- ism.

- Let a be an action, e.g. stopping that damn buzzer.

- The following two axioms describe an elaboration that

- interpolates an action after a normal internal action.

- In the buzzer case it would be opening an additional

- switch in the circuit. The additional switch isn’t in

- Fig. 1 or described in section 3.

It is bad or dangerous to have more than one yellowblock, but perhaps only if one is not a special favoriteof the emperor or if one is just about to die anyway.The point is that common sense (at least human levelcommon sense) requires that such constraints tolerateelaboration. Human level common sense also allowsthe constraint to become an action precondition as aresult of some inference. This inference should takeplace within the logical formalization.

Lin and Reiter include the following formula.(∀x y s)(P oss(P aint(x, y), s)

≡ (N earby(x, s) ∧ Haspaint(y, s)

∧(∀x1)(Color(x1, Y ellow, s) ∧ y = Y ellow → x = x1))).(51)This formula is specialized to the emperor tolerat-If he tolerates 7 yellowing just one yellow block.

blocks, we had better use set notation, i.e.refer tocard({x|Color(x, Y ellow)}) ≤ 7.7

There are some domain constraints that are not natu-rally formalized by internal actions. One is the blocksworld constraint that a block may not be on top ofitself. Formulas like

Above(T op(block), Bottom(block), s)(52)or even

Occurs(a, s) ∧ External(a) ∧ Occurs(e, s)

→ N ext(s) = Result(a, Result(e, s))

(49)

Height(T op(block), s) − Height(Bottom(block), s)≥ 1.0cm

- and

- Occurs(e, s) ∧ (∀e(cid:48))(Occurs(e(cid:48), s) → e(cid:48) = e)

→ N ext(s) = Result(e, s).

(50)

- This is a limited kind of concurrency. Only certain

- kinds of interventions can be done this way.

- 9.6 State constraints after all

- As was shown in Section 4, the condition for a room

- being stuﬀy is better formalized with eﬀect axioms,

- occurrence axioms, and the events Getstuf f y and

- U ngetstuf f y. Lin and Reiter [LR94] consider the Em-

- peror’s decree that no more than one object (block)

- be yellow, which may be regarded as a domain con-

- straint. They point out that it is more eﬃcient to

- encode the constraint as a precondition that a block

- may be painted yellow only if no block is already yel-

- low. Their way of expressing this does not readily

- elaborate to require that no more than seven blocks

- be yellow.

tell more about the world than the simple(53)¬On(block, T op(block), s).

(54)An important application for the direct use of stateconstraints is when an event starts a process that even-tually leads to an equilibrium state. For example, ifI drop a coin on the ﬂoor it will bounce around for awhile and then settle down. It will reach equilibriumin a second or so, and I am interested in whether thecoin ends up heads or tails rather than in the processof its settling down. In the case of the coin the equilib-rium condition, at least what we want to know aboutit, is easy to state, namely

On(coin, f loor, Result∗(Drop(coin, s)))∧(Heads(coin, Result∗(s))

∨T ails(coin, Result∗(s))),

(55)where using Result∗ means that we are skipping bysome internal events, in this case not formalized.- I think logical AI needs a more complex treatment. It

- seems to me that eﬃciency conﬂicts with generality.

7I pound the table here because of some resistance tothe idea that axiomatic set theory makes logical AI easier.- Another example may be concocted from the elabo-

- rated stuﬀy room scenario. While Pat and Mike dis-

- agree in their preferences, under normal circumstances

- we can suppose they will come to an agreement in some

- short time. One will defer to the other in the matter

- of the blocked vents. As with the coins, the theory

- of eventual agreement doesn’t predict what the agree-

- ment will be.

- More generally, Aarati Parmar suggests that internal

- events are evoked by any non-equilibrium situations.

- 9.7 Javier Pinto’s formalism

- The work closest to the present is [Pin98b], as one of

- the referees forcefully pointed out. There are substan-

- tial diﬀerences, both in approach and in the formalisms

- motivated by the diﬀerent approaches.

- Pinto uses the Reiter notion of situations as trees built

- from the initial situation S0 by iterations of forming

- do(a, s) where a is an action and s a previously formed

- situation term.

- Pinto (as does Reiter) builds time, represented by a

- real number, into his situation calculus formalism. It

- seems to me that making time ﬁt the tree structure of

- situation terms leads to complications. Pinto has ﬁve

- diﬀerent occur predicates, whereas we have only one.

- His occurrence axioms all have time parameters. Our

- occurrence axioms involve only situations and ﬂuents

- and are therefore simpler. The examples of the present

- article do not involve time explicitly. When time must

- be explicit, we propose to treat the passage of time

- as an independent situation calculus process running

- concurrently with the processes we are treating.

- Pinto includes the following interesting examples. We

- show how our method treats a few of them.

- 1. “The sun will rise tomorrow at 6:03 am.” Here

- we have two concurrent processes: the passage of time

- and the path of the sun through the sky. The sentence

- describes an interaction.

- We can represent the sentence by

V alue(T ime, s) = T ime(T omorrow603am)

→ Occurs(Sunrise, s),

- where we are not taking into account the explicit in-

- dexical of tomorrow, and the implicit indexical that

- sunrise being a 6:03am must refer to a speciﬁc lati-

- tude and longitude.

that of having eaten the fruit, giving rise to an event.That’s how the present paper would treat it, i.e.Holds(Has-occurred(Eat(F orbidden-f ruit)), s)→ Occurs(Does(God, Expel(Eater)), s),together with the general moving ﬁnger axiomsOccurs(e, s) → Holds(Has-occurred(e, N ext(s))),and Holds(Has-occurred(e, s) ∧ s < s(cid:48)→ Holds(Has-occurred(e, s(cid:48))).(56)(57)3. “The train to Ottawa leaves every day at 7 pm.”where it is understood that this scheduled event maynot occur under exceptional circumstances.V alue(T ime, s) = T ime(7pm)

∧¬P revented(T rain-Leaves-f or-Ottowa, s)→ Occurs(T rain-Leaves-f or-Ottowa, s).“If my neighbor’s burglar alarm goes oﬀ4.

while I am at home,

the police.”Pinto treats this example and the previous one byslightly diﬀerent formalisms, one involving a predi-cate occurspo(action, time) and the other a predicateoccursct(action, time, action2).

I will call

5. The Miller-Shanahan [RM94] example of the brief-case.

6. “My house has a burglar alarm. If the alarm is con-nected, I have exactly 60 seconds to deactivate it afteropening the main door. If I am unable to disconnectthe alarm, it will go oﬀ.”

7. “Upon an insertion into EMP or an update to EMP,the new SAL is checked, and if it exceeds $100,000,then the JobTitle of this employee is added to HPAID,assuming it was not there already.”

Holds(Checksalary, Result(Insert(EM P, y, s)))∧Holds(Checksalary, Result(U pdate(EM P, y, s))).Holds(Checksalary(employee, s) →

Occurs(Add(J obT itle(employee), HP AID), s)[Pin98a] introduces occurs(a, s), where a is a “naturalaction”. Natural actions partly correspond to internalevents. The article is dedicated to concurrent events,to which I hope devote a separate article.10 Concluding remarks

- 2. “If you eat the forbidden fruit you will be expelled.”

- Pinto treats this as one event causing another but re-

- marks that it might be better formalized as a state, i.e.

Events that are not actions have been previouslyused—at least by Fangzhen Lin [Lin98], Sheila McIl-raith [McI00], and Javier Pinto.

- Occurrence axioms are even more important in the

- treatment of concurrent events in situation calculus—

- to be the subject of another article.

- This work beneﬁted from discussions with Eyal Amir,

- Tom Costello, Ron Fadel, Hector Levesque, Vladimir

- Lifschitz, Fangzhen Lin, Sheila McIlraith, Leora Mor-

- genstern, Aarati Parmar, Raymond Reiter, and Tran

- Son and the comments of three anonymous referees.

- This research was partly supported by SRI Subcon-

- tract No. 34-000144 under SPAWAR Prime Contract

- No. N66001-00-C-8018.

- References

- [aA01] Texas

Causal

http://www.cs.utexas.edu/users/tag/cc.

Action

calculator

Group

home

at

page,

Austin.

2001.

- [GS88] Matthew L. Ginsberg and David E. Smith.

Reasoning about action I: A possible worlds

approach. Artiﬁcial Intelligence, 35(2):165–

195, 1988.

- [Lif94] Vladimir Lifschitz. Circumscription.

In

J. A. Robinson Dov M. Gabbay, C. J. Hog-

ger, editor, Handbook of logic in artiﬁcial in-

telligence and logic programmin, volume 3,

pages 297–352. Oxford, 1994.

- [Lin98] Fangzhen Lin. On the relationships between

static and dynamic causal rules in the situa-

tion calculus. In Charles L. Ortiz, Jr., editor,

Working Notes of the AAAI Spring Sympo-

sium on Prospects for a Commonsense The-

ory of Causation, pages 38–43, Menlo Park,

CA, 1998. American Association for Artiﬁ-

cial Intelligence.

- [LR94] Fangzhen Lin and Ray Reiter. State con-

straints revisited. Journal of Logic and Com-

putation, 4:655–678, 1994.

- [LS95]

Fangzhen Lin and Yoav Shoham. Provably

correct theories of action.

Journal of the

ACM, 42(2):293–320, March 1995.

- [MC98] John McCarthy and Tom Costello. Com-

bining narratives.

In Proceedings of Sixth

Intl. Conference on Principles of Knowledge

Representation and Reasoning, pages 48–59.

Morgan-Kaufman, 1998.

- [McC59] John McCarthy. Programs with Common

Sense8.

In Mechanisation of Thought Pro-

cesses, Proceedings of the Symposium of the

- 8http://www-formal.stanford.edu/jmc/mcc59.html

National Physics Laboratory, pages 77–84,London, U.K., 1959. Her Majesty’s Sta-tionery Oﬃce. Reprinted in [McC90].[McC63] John McCarthy.

Situations, actions andcausal laws. Technical Report Memo 2, Stan-ford University Artiﬁcial Intelligence Labo-ratory, Stanford, CA, 1963. Reprinted in[Min68].

[McC86] John McCarthy. Applications of Circum-scription to Formalizing Common SenseKnowledge9. Artiﬁcial Intelligence, 28:89–116, 1986. Reprinted in [McC90].[McC90] John McCarthy.

Formalizing CommonSense: Papers by John McCarthy. AblexPublishing Corporation, 1990.

[McC92] John McCarthy. Overcoming unexpected ob-stacles10. Web only, 1992.

[McC95] John McCarthy.

Situation Calculus withConcurrent Events and Narrative11. 1995.Web only, partly superseded by [MC98].[McC99] John McCarthy. Elaboration tolerance12.web only for now, 1999.

[McI00] Sheila A. McIlraith. An axiomatic solutionto the ramiﬁcation problem (sometimes). Ar-tiﬁcial Intelligence, 116(1–2):87–121, 2000.[MH69] John McCarthy and Patrick J. Hayes. SomePhilosophical Problems from the Standpointof Artiﬁcial Intelligence13. In B. Meltzer andD. Michie, editors, Machine Intelligence 4,pages 463–502. Edinburgh University Press,1969. Reprinted in [McC90].

[Min68] Marvin Minsky, editor. Semantic informa-tion processing. MIT Press, 1968.[Pin98a] Javier A. Pinto. Concurrent actions andinteracting eﬀects.

In Anthony G. Cohn,Lenhart Schubert, and Stuart C. Shapiro, ed-itors, KR’98: Principles of Knowledge Rep-resentation and Reasoning, pages 292–303.Morgan Kaufmann, San Francisco, Califor-nia, 1998.

9http://www-formal.stanford.edu/jmc/applications.html10http://www-formal.stanford.edu/jmc/glasgow.html11http://www-formal.stanford.edu/jmc/narrative.html12http://www-formal.stanford.edu/jmc/elaboration.html13http://www-formal.stanford.edu/jmc/mcchay69.html- [Pin98b] Javier A. Pinto. Occurrences and narratives

as constraints in the branching structure of

the situation calculus. Journal of Logic and

Computation, 8(6):777–808, 1998.

- [Rei01] Raymond Reiter. Knowledge in Action.

M.I.T. Press, 2001.

- [RM94] R.S.Miller and M.P.Shanahan. Narratives in

the situation calculus. Journal of Logic and

Computation, 4(5):513–530, 1994.

- [Sha97] Murray Shanahan. Solving the Frame Prob-

lem, a mathematical

the

common sense law of inertia. M.I.T. Press,

1997.

investigation of

- [Sho88] Yoav Shoham. Chronological ignorance: Ex-

periments in nonmonotonic temporal reason-

ing. Artiﬁcial Intelligence, 36(3):279–331,

1988.

