SITUATION CALCULUS WITH

CONCURRENT EVENTS AND

NARRATIVE

John McCarthy, Stanford University

January 28, 2001

If this version is relevant, you may cite it.

Abstract

Concurrent events are treated merely by not forbidding them. Nar-

rative is treated as a collection of situations and events and relations

among them. Narrative is easier than planning, because it does not

require that the eﬀects of events be guaranteed. Prediction is harder

than planning, because it requires that the actions be inferred from

the motives of the actors.

Introduction—Objectives of Situa-

tion Calculus

The logic approach to AI ([McCarthy, 1959] and [McCarthy, 1989]) is

to make a computer program that represents what it knows about the

world in general, what it knows about the situation it is in, and also

its goals all as sentences in some mathematical logical language. It

then infers logically what action is appropriate for achieving its goal

and does it. Since 1980 it has been widely known that nonmonotonic

inference must be included. The actions it can perform include some

that generate sentences by other means than logical inference, e.g. by

observation of the world or by the use of special purpose non-logical

problem solvers.

Simpler behaviors, e.g. actions controlled by servomechanisms or

reﬂexes can be integrated with logic. The actions decided on by

logic can include adjusting the parameters of ongoing reﬂexive ac-

tions. Thus a person can decide to walk faster when he reasons that

otherwise he will be late, but this does not require that reason control

each step of the walking. 1

Situation calculus is an aspect of the logic approach to AI. A sit-

uation is a snapshot of the world at some instant. Situations are rich

objects in that it is not possible to completely describe a situation,

only to say some things about it. From facts about situations and

general laws about the eﬀects of actions and other events, it is possi-

ble to infer something about future situations. Situation calculus was

ﬁrst discussed in [McCarthy, 1963], but [McCarthy and Hayes, 1969]

was the ﬁrst widely read paper about it.

In this formalization of action in situation calculus, we treat three

kinds of problem—narrative, planning and prediction.

Of these, narrative seems to be the simplest. A narrative is an

account of what happened. We treat it by giving some situations and

some events and some facts about them and their relations. Situations

in a narrative are partially ordered in time. The real situations are

totally ordered, but the narrative does not include full information

about this ordering. Thus the temporal relations between situations

in diﬀerent places need only be described to the extent needed to

describe their interactions.

In situation calculus as it was originally envisaged and has been

used, events (mainly actions) in a situation produce next situations,

e.g. s(cid:48) = result(e, s). The original theory did not envisage more

than one event occurring in a situation, and it did not envisage inter-

mediate situations in which events occur. However, rarely did people

write axioms that forbade these possibilities; it’s just that no-one took

advantage of them. 2

Our present formalism doesn’t really change the basic formalism

of the situation calculus much; it just takes advantage of the fact that

the original formalism allows treating concurrent events even though

concurrent events were not originally supposed to be treatable in that

1Thus I protect my ﬂank from the disciples of Rod Brooks.

2Reiter and Lifschitz did write such axioms.

formalism. Gelfond, Lifschitz and Rabinov [?] treat concurrent events

in a diﬀerent way from what we propose here.

In a narrative, it is not necessary that what is said to hold in a sit-

uation be a logical consequence (even nonmonotonically) of what was

said to hold about a previous situation and known common sense facts

about the eﬀects of events. In the ﬁrst place, in stories new facts about

situations are often added, e.g. “When Benjamin Franklin arrived in

London it was raining”. In the second place, we can have an event

like tossing a coin in which neither outcome has even a nonmonotonic

preference.

Nevertheless, some narratives are anomalous.

If we record that

Junior ﬂew to Moscow, and, in the next situation mentioned, assert

that he is in Peking, a reader will feel that something has been left

out, some Gricean implicature [Grice, 1989] has been violated. We

want to introduce a concept of a proper narrative, but it isn’t clear

exactly what it should be. The ﬂuents holding in a new situation

should be reasonable outcomes of the events that have been reported,

except for those ﬂuents which are newly asserted, e.g.

that it was

raining in London when Franklin arrived. Perhaps the assertions that

do not follow from previous events should sometimes be tagged as

such. The word “but” does this in ordinary language, so maybe we

want a but construction. Of course, “but” is used in discourses that

are not narratives.

In interpreting the following formalizations, we regard situations

as rich objects and events as poor. In fact, we are inclined to take a de-

terministic view within any single narrative. In principle, every event

that occurs in a situation and every fact about following situations is

an inevitable consequence of the facts about the situation. Thus it

is a fact about a situation that a coin is tossed and that it comes up

tails. However, such facts are only occasionally consequences of the

facts about the situation that are actually stated in the narrative.

Perhaps narrative seems easy, since it is not yet clear what facts

must be included in a narrative and what assertions should be in-

ferrable from a narrative.

2 Nonmonotonic Reasoning from Nar-

ratives

This section is informal, because we want to discuss what the con-

sequences of a narrative should be before discussing how to make

circumscription or some other nonmonotonic formalism do what we

want. Here are some kinds of inference we want to be able to make.

Preconditions An action has only those preconditions that can be

inferred from the facts at hand.

Ramiﬁcations Only the eﬀects of an event that can be inferred from

the narrative are relevant to the future course of the events men-

tioned in the narrative.

Presence of objects The only objects satisfying certain ﬂuents in a

situation are those for which it follows from what is stated. Some

of the “it follows” assertions are inferred nonmonotonically. One

child will infer that another child has parents but not that the

child has a dog.

Normal eﬀect An event has its normal eﬀect unless something pre-

vents it.

Occurrences Only such events occur in a situation or its successors

as are asserted or inferred or which don’t aﬀect conclusions that

might be drawn from their nonoccurrence.

This condition must be formalized very carefully, as is apparent

when we elaborate a particular event as a sequence of smaller

events. “How did he buy the Kleenex? He took it oﬀ the shelf,

put it on the counter, paid the clerk and took it home.” A nar-

rative that just mentions buying the Kleenex should not allow

nonmonotonic reasoning that excludes this particular elabora-

tion. Moreover, if we elaborate in this way, we don’t want to

exclude subsequent elaboration of component events, e.g. elab-

orating paying the clerk into oﬀering a bill, taking the change,

etc.

Inertia Events change only those ﬂuents they can be inferred to

change. Fluents or ﬂuent valued functions may be declared to

be dependent by statements like

dependent(distance).

(1)

This statement would have the eﬀect of making

distance(x, y)

change with changes in x and y and have no inertia of its own.

Processes that have started in a situation continue until some-

thing changes their course or they terminate as called for in their

axiomatizations.

Obstacles Only such obstacles arise to events having their normal

eﬀects as can be inferred.

Actions Minimize unmotivated actions.

We will very likely use something like the Reiter and Levesque

technique of a two stage minimization. (Reiter’s Research Ex-

cellence lecture and subsequent discussions.) (Advice to use this

technique may serve as an example of the declarative expression

of heuristics.)

3 Glasgow, London, Moscow and New

York Narratives

The object of this section is to give narratives illustrating the treat-

ment of concurrent events in two cases. The ﬁrst is when two sub-

narratives do not interact, and the second is when they do. The ﬁrst

sub-narrative is ordinary block stacking (as discussed in many situa-

tion calculus papers), and we suppose the stacking to be done by a

person called Daddy in New York. In the second sub-narrative, the ac-

tor is named Junior, and he wants to ﬂy from Glasgow to Moscow via

London. The story is taken from an earlier unpublished but widely

circulated manuscript [McCarthy, 1992] discussing how circumscrip-

tion could be used to treat an unexpected obstacle to a plan. In this

case, Junior may or may not lose his ticket in London. The change is

made by adding a single sentence to the facts. Without that sentence,

one can conclude that ﬂying to London and then to Moscow will get

Junior to Moscow. With it he must buy another ticket in London,

i.e. we can no longer conclude that the original sequence of actions

will work, but we can conclude that the revised sequence that includes

buying a ticket in London will work.

Because we want to treat interacting events, we make life more

complicated for Junior. If he loses his ticket, he must wire Daddy in

New York for money. Daddy, who normally indulges Junior, has to

interrupt his block stacking and sell a block in order to get the money

to send Junior.

Narrative 1

In this narrative Junior doesn’t lose his ticket and gets to Moscow

without asking for help. Daddy stacks blocks in New York. There is

no interaction, and nothing is said about the time relations between

the two sub-narratives.

holds(at(Junior, Glasgow), S0)

holds(has(Junior, T icket1), S0)

holds(has(Junior, T icket2), S0)

is-ticket(T icket1, Glasgow, London)

is-ticket(T icket2, London, M oscow)

holds(exists-f light(Glasgow, London), S0)

holds(exists-f light(London, M oscow), S0)

occurs(does(Junior, f ly(Glasgow, London)), S0)

S0 < S1

holds(at(Junior, London), S1)

(2)

(3)

(4)

(5)

(6)

(7)

(8)

(9)

(10)

(11)

When Junior is in London, inertia gets us that the ﬂights still exist,

and Junior still has Ticket2. As for Ticket1, we would infer that he

still has it unless we brought in the fact that a ticket is used up when

one takes the ﬂight the ticket is for. That is certainly a part of the

knowledge of anyone who travels using tickets. Thus someone who had

travelled by bus would infer it about airplane travel. Indeed it could

be inferred from more general principles about commerce, e.g. that

a seller doesn’t want to allow the buyer to get an arbitrary number

of what he has a paid for one of. However, anyone who travels has

the more speciﬁc information and doesn’t need to infer it from general

principles about commerce. Indeed he may never have formulated any

general principles about commerce.

occurs(does(Junior, f ly(London, M oscow)), S1)

(12)

S1 < S2

holds(at(Junior, M oscow), S2)

Now we begin Daddy’s life as a block stacker. We have stated no

relation between the situations S0 and S0(cid:48) and know nothing of their

temporal relations. If we asserted

time S0 < time S0(cid:48) < time S1,

(15)

then we could conclude that Junior still had the tickets in S0(cid:48). Also

asserting S0(cid:48) = S0 would do no harm to the conclusions drawn about

either subnarrative. We have asserted that Daddy has the three blocks

mentioned, and we would like to be able to draw the nonmonotonic

conclusion that these are all the blocks he has.

holds(at(Daddy, N Y ), S0(cid:48))

holds(has(Daddy, Block1), S0(cid:48))

holds(has(Daddy, Block2), S0(cid:48))

holds(has(Daddy, Block3), S0(cid:48))

holds(on(Block1, T able), S0(cid:48))

holds(on(Block2, T able), S0(cid:48))

holds(on(Block3, top Block1), S0(cid:48))

(13)

(14)

(16)

(17)

(18)

(19)

(20)

(21)

(22)

occurs(does(Daddy, move(Block3, T able)), S0(cid:48))

(23)

holds(on(Block3, T able), S1(cid:48))

occurs(does(Daddy, move(Block2, top Block1)), S1(cid:48))

(26)

holds(on(Block2, top Block1), S2(cid:48))

occurs(does(Daddy, move(Block3, top Block2)), S2(cid:48))

(29)

S0(cid:48) < S1(cid:48)

S1(cid:48) < S2(cid:48)

S2(cid:48) < S3(cid:48)

(24)

(25)

(27)

(28)

(30)

(31)

holds(on(Block3, top Block2), S3(cid:48))

We can imagine that blocks being clear is a precondition for moving

them. The preceding subnarrative does not violate this precondition,

but in a narrative we don’t ordinarily have to show that preconditions

are satisﬁed. We should be able to conclude via inertia that Daddy

has the three blocks in the ﬁnal situation.

Narrative 2

Now Junior loses the ticket and sends a telegram to Daddy asking

for money. Daddy, who normally indulges Junior, sells a block and

sends Junior the money, who buys a London-Moscow ticket and goes

on to Moscow. I chose a telegram rather than a telephone call, be-

cause I would not want to tell about a telephone call as a sequence

of statements by Junior and Daddy but rather to regard its result as

a joint action, e.g. an agreement that Junior and Daddy would do

certain actions.

Note also we haven’t treated what Daddy now knows as the result

of the telegram. It seems that treating knowledge and treating agree-

ment are similar in their requirement for treating intentional entities.

The intentional state that Junior has requested that Daddy send him

the money is not merely that Daddy knows that Junior wants Daddy

to send him the money. Also the agreement is likely to have some-

thing like a bit of narrative as an argument, e.g. a set of actions that

Junior and Daddy will do with only partial time relations between the

actions.

holds(at(Junior, Glasgow), S0)

holds(has(Junior, T icket1), S0)

holds(has(Junior, T icket2), S0)

is-ticket(T icket1, Glasgow, London)

is-ticket(T icket2, London, M oscow)

holds(exists-f light(Glasgow, London), S0)

holds(exists-f light(London, M oscow), S0)

occurs(does(Junior, f ly(Glasgow, London)), S0)

(39)

S0 < S1

holds(at(Junior, London), S1)

Up to here, narrative 2 is the same as narrative 1. Also insert here

the sentences between equations (16) and (31).

occurs(loses(Junior, T icket2), S1)

(42)

We want to regard losing the ticket as something that happens to

Junior rather than as something he does. That’s why we don’t write

does(Junior, lose T icket2). The bad consequences of doing the latter

would arise when we get around to writing laws that quantify over

voluntary actions.

(32)

(33)

(34)

(35)

(36)

(37)

(38)

(40)

(41)

We will use some of the same names now for situations that are

diﬀerent than in narrative 1.

S1 < S2

¬holds(has(Junior, T icket2), S2)

value(cash Junior, S2) < value(airf are(London, M oscow), S2)

e1 = does(Junior, telegraph(Daddy, request send airf are(London, M oscow)))occurs(e1, S2)

S2 < S3(cid:48)

occurs(receives(Daddy, telegram-f rom(Junior, request send airf are(London, M oscow))), S3(cid:48))value(cash Daddy, S3(cid:48)) < value(airf are(London, M oscow), S2)

occurs(does(Daddy, sell Block3), S3(cid:48))

S3(cid:48) < S4(cid:48)

value(cash Daddy, S4(cid:48)) > value(airf are(London, M oscow), S2)

occurs(does(Daddy, send(Junior, airf are(London, M oscow), S2)), S4(cid:48))

(43)

(44)

(45)

(46)

(47)

(48)

(49)

(50)

(51)

(52)

(53)

(54)

(55)

(56)

(57)

(58)

(59)

(60)

(61)

(63)

(64)

¬holds(has(Daddy, Block3), S4(cid:48))

S4(cid:48) < S3

value(cash Junior, S3) > value(airf are(London, M oscow), S3)

occurs(does(Junior, buy T icket3), S3)

is-ticket(T icket3, London, M oscow)

holds(has(Junior, T icket3), S4)

occurs(does(Junior, f ly(London, M oscow)), S4)

(62)

holds(at(Junior, M oscow), S5)

Interpolating unconnected situations and events into a narrative

should not harm the conclusions. For example, we could put situa-

tions S0.5 and S0.7 between S0 and S1, i.e. time(S0) < time(S0.5) <

time(S0.7) < time(S1), and suppose that Junior reads a book on the

airplane during the inner interval. The previous statements about

what holds when Junior arrives in London should still seem ok. How-

ever, suppose we postulate that Junior spent time in Peking on the

way from Glasgow to London. This would make the narrative anoma-

lous, but some geograhical knowledge is required to make the anomaly

apparent.

S3 < S4

S4 < S5

4 Elaboration of Narratives

Suppose we are asked, “How did Junior ﬂy from Glasgow to Lon-

don?” and want to respond with facts about taking a taxi to the

airport, presenting his ticket at the check-in counter, going to the

gate, getting on the airplane, taking his assigned seat, etc. We can

add this additional narrative with its intermediate situations, and we

can throw in reading the book if we like. There is no reason to discard

occurs(does(Junior, f ly(Glasgow, London)), S0). We merely have a

redundant way of reaching the same conclusion. However, we would

like a sentence relating the more detailed narrative to the less detailed

narrative, i.e. of asserting that one realizes the other. For this we

will at least need narratives as objects, and this has not yet been

introduced.

Note that the relation elaborates(N 2, N 1), when we get around

to introducing it, will not be like the relation between a subroutine

call and the subroutine. N 1 will not in any sense be the deﬁnition

of N 2. N 2 could be realized in a number of ways, only one of which

corresponds to N 1.

The elaboration involved in telling about Junior reading a book is

of a diﬀerent kind from that involved in telling about his taking a taxi

to the airport, because reading the book is a parallel operation rather

than a means of accomplishing part of the travel. Reading should be

simpler to treat. In fact it may be more like Daddy stacking blocks in

New York.

Suppose, however, that we want to treat reading the book as a sim-

ple sequential situation calculus account using the function result(a, s).

We will need to encapsulate the reading narrative in some way. The

obvious way to do it is by using a context in which we do the reasoning

about reading, e.g. what has to be read ﬁrst in order to understand

the subsequent chapters of the book, etc. It is not obvious what to

call this context, but for now let’s give it an arbitrary name c21. By

the way, using the context theory of [?] requires that the sentences of

the whole narrative be true in an outer context—call it c0.

What should be the language of this context c21, and what should

the initial sentences p such that ist(c21, p)? c21 should also have

some nonmonotonic rules speciﬁc to reasoning within it. For example,

it may assume some kinds of normality of the reader, e.g. that he

knows the language of his reading material. This assumption will

be realized by applying some defaults to facts in the common sense

database about reading.

There are two possible approaches to forming c21, i.e. to asserting

what is true in it. The ﬁrst approach is to derive these facts from the

situation in some way. The second approach is to state them by ﬁat.

(As Russell put it, the advantages of the axiomatic method are the

same as the advantages of theft over honest toil.) We take the latter

approach with the consequence that we will later have more work to

do when we want to lift conclusions from c21 to an outer context.

c21 should be adapted to precisely the reasoning that has to be done

about Junior’s reading. Thus if we have the formalism in good shape,

nothing about the fact that Junior is travelling by airplane should

need to aﬀect c21.

5 Planning

We would like to treat the circumstances of the previous narrative

from the point of view of planning. In that case we need to be explicit

about the consequences of actions and other events.

6 Prediction

Let us consider the purposes of Junior and Daddy and predict what

actions they will take and what the outcome will be. Of course, Junior

losing the ticket will be an unpredicted event. We just throw it in,

but then we should be able to predict what Junior and Daddy will

subsequently do.

7 Elaborations

Events are composed of subevents and objects are composed of sub-

objects. In the real world, such elaborations have detail far beyond

what a human or robot can know. Moreover, events and objects, etc.

can be elaborated in a variety of ways.

7.1

I

n section 2 we mentioned elaborating the purchase of a box of Kleenex.

Since buy a box of Kleenex might be accomplished in a variety of ways

realizes(take(Kleenex1); place(Kleenex1, Counter1); pay(Clerk1); take(Kleenex1), buy(Kleenex1), s).we need to write something like

(65)

This is too simple, because it is sequential. I suppose the answer is

that an event is realized by a subnarrative. If so one is tempted to the

further complication of allowing events that result in many situations.

7.2 Elaboration of Narratives

Here is a start on a formalization. Narratives are ﬁrst class objects.

We will be interested in a relation elaborates(N 2, N 1) asserting that

narrative N 2 is an elaboration of narrative N 1. Intutively, narratives

are collections of events and situtaions and relations among them.

Tentatively, we will not use sets in our formalism. Instead we make

the narrative an additional parameter of sentences concerning situa-

tions and events—thus we have occurs(e, s, N ). Entering a context

associated with the narrative N permits writing occurs(e, s) as has

been our custom.

There are two ways of looking at narratives that elaborate other

narratives, and maybe we need both of them.

Suppose we have elaborates(N 2, N 1). We may be asserting that

N 1 occurred, and N 2 also occurred and gives more detail. On the

other hand, we may regard N 2 as a mere hypothetical elaboration of

N 1, even a counterfactual elaboration.

We need parts of narratives, and an axiom saying that an elabo-

ration of a part of a narrative extends to an elaboration of the whole

in the obvious way.

7.3 Elaboration of Objects

The elaboration of objects is presumably like the elaboration of events,

but it is likely to be more complicated, because objects are three

dimensional.

8 Role of Context

It might be a good idea when starting a narrative or to achieve a goal

to begin with an almost empty context, e.g. with just the task in it.

Then the narrative itself comes in sequentially and related facts are

retrieved from the common sense database. This permits nonmono-

tonic reasoning that the only events that have certain eﬀects are those

that can be shown to do so on the basis of the facts that have been

retrieved.

9 Philosophical Considerations

Reality is the determinist limit of nondeterminist approximations. In

what a human or robot can know about the world, many events are not

inevitable. In any human account, it did not have to be raining when

Benjamin Franklin ﬁrst arrived in London. Indeed maybe it wasn’t.

Even if the world is deterministic, any achievable description of it is

nondeteterministic. Elaborations of particular narratives sometime

remove some of the nondeterminism by accounting for the causes of

particular events and for ﬂuents holding in the results of these events.

Therefore, it may be worthwhile to regard the world as determinist

and suppose that every event has causes whether we know them or

not. Thus any particular nondeterminism is potentially eliminable.

It might be supposed that quantum mechanics vitiates these con-

siderations, but I don’t think it requires modiﬁcations on the com-

mon sense level. Free will in a determinist world is discussed in

[[McCarthy and Hayes, 1969]].

10 Other Approaches

11 Remarks

1. I have always felt that the careful classiﬁcation of the ways in

which events can overlap is unnecessary for almost all common

sense reasoning.

I think this article shows it. Moreover, it is

also usually unnecessary to combine concurrent events into com-

pound events as do Gelfond and Lifschitz [?].

12 Acknowledgments

This article has beneﬁtted from discussions with Sasa Buvac, Tom

Costello, Fausto Giunchiglia, Fiora Pirri, Murray Shanahan.

13 Scaﬀolding

This section will be removed from the ﬁnal paper. It is included now

only so that its remarks will appear when the document is latexed.

The word “but” can play a role in narrative. Suppose an event

leads to a situation, but the properties being asserted about the situ-

ation are not what would normally follow from the occurrence of the

event. Perhaps a good narrative should label the anomalous ﬂuents

of the new situation with “but”.

cannot

We have not yet treated being able to prove that a person cannot

accomplish something or that something cannot happen. The easiest

way to think about this may be to have Junior try to prevent Daddy

from doing something.

quotes from Russell and Bell

common sense informatic situation

facts vs. what is known

It seems that ¡ should not be transitive. Narratives should be

objects.

Fluents should be inferred to persist as long as there is no event

in the narrative or directly following from the narrative that would

change this.

It may be advantageous to treat processes by introducing a ﬂuents

that persist and whose persistence determines that some secondary

ﬂuents change in a speciﬁed way, e.g.

f alling ⊃ s = 1/2gt2.

(66)

We need to be able to declare some ﬂuents as dependent on others

so that their change or persistence is not inferred separately, e.g.

depends(distance(x, y), location(x), location(y))

(67)

Actually it might suﬃce to write

dependent(distance)

(68)

Probably Vladimir has thought about this possibility and should

be asked. Whether a ﬂuent is dependent may depend on context.

Maybe we should distinguish between asserting the dependence of

ﬂuents and that of functions whose value is a ﬂuent (as in the present

example).

It looks like we may need priorities to handle the rules about what

persists because of the narrative or what follows from the narrative.

Somehow he got to Moscow after losing his ticket. What is the

semantics of “somehow”?

Events:

losing ticket

buying ticket

moving block

selling block

sending telegram with messaage

sending money

fly(x,y)

Can these be handled in a uniform way? An event is realized by

a sequence of subevents, actually by a subnarrative. Indeed suppose

that how a person performs an action involves delegating some of

the work to another person. It is usually unnecessary to completely

specify the sequential or temporal relations of the work performed by

the diﬀerent people.

The biggest strain on the single history interpretation will come

with counterfactuals or trying to compare the outcomes of diﬀerent

strategies. That’s where the free will approximation comes in. Thus

free will is an approximate theory in a determinist world.

References

[Grice, 1989] Grice, H. P. “Studies in the Way of Words”, Harvard

University Press, Cambridge, Mass., 1989.

[McCarthy, 1959] McCarthy, John (1959): “Programs with Com-

mon Sense”, in Proceedings of the Teddington Conference on

the Mechanization of Thought Processes, Her Majesty’s Sta-

tionery Oﬃce, London. Reprinted in [McCarthy, 1990].

[McCarthy, 1963] McCarthy, John (1963): “Situations, Actions

and Causal Laws.” Stanford Artiﬁcial Intelligence Project:

Memo 2. (First appearance of situation calculus).

[McCarthy and Hayes, 1969] McCarthy, John and P.J. Hayes:

“Some Philosophical Problems from the Standpoint of Arti-

ﬁcial Intelligence”, in D. Michie (ed), Machine Intelligence

4, American Elsevier, New York, NY, 1969. Reprinted in

[McCarthy, 1990].

[McCarthy, 1989] McCarthy, John (1989): “Artiﬁcial Intelligence

and Logic” in Thomason, Richmond (ed.) Philosophical Logic

and Artiﬁcial Intelligence (Dordrecht ; Kluwer Academic,

c1989).

[McCarthy, 1990] McCarthy, John: Formalizing Common Sense,

Ablex, Norwood, New Jersey, 1990.

[McCarthy, 1992] McCarthy, John (1992): “Overcoming Unex-

pected Obstacles”, unpublished manuscript with some errors

in the formalizations.

/@sail.stanford.edu:/u/jmc/e93/narrative.tex: begun 1993 Jul 5, latexed 2001 Jan 28 at 4:52 p.m.

