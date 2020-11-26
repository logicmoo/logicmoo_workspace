What Will Self-Aware Computer Systems Be?John Mccarthy, Stanford UniversityMccarthy@Stanford.EduHttp://Www-Formal.Stanford.Edu/Jmc/August 9, 2004

- • Darpa Wants To Know, And There’s A Workshop T

- • The Subject Is Ready For Basic Research.

- • Short Term Applications May Be Feasible.

- • Self-Awareness Is Mainly Applicable To Programs With

- tent Existence.

WHAT WILL SELF-AWARE SYSTEMS BE AWARE- • Easy aspects of state: battery level, memory available,

- • Ongoing activities: serving users, driving a car

- • Knowledge and lack of knowledge

- • purposes, intentions, hopes, fears, likes, dislikes

- • Actions it is free to choose among relative to external

- straints. That’s where free will comes from.

- • Permanent aspects of mental state, e.g.

- beliefs,

long term- • Episodic memory—only partial in humans, probably

- animals, but readily available in computer systems.

HUMAN SELF-AWARENESS—1- • Human self-awareness is weak but improves with age.

- • Five year old but not three year old.

I used to think- contained candy because of the cover, but now I kno

- crayons. He will think it contans candy,

- • Simple examples: I’m hungry, my left knee hurts from

- my right knee feels normal, my right hand is making

I

- • Intentions:

- Zealand some day. I do not intend to die.

intend to have dinner,

I

intend to- • I exist in time with a past and a future. Philosophers

- lot about what this means and how to represent it.

- • Permanent aspects of ones mind: I speak English and

- French and Russian. I like hamburgers and caviar. I cannot

- my blood pressure without measuring it.

HUMAN SELF-AWARENESS—2- • What are my choices? (Free will is having choices.)

- • Habits: I know I often think of you. I often have breakfast

- the Pennsula Creamery.

- • Ongoing processes: I’m typing slides and also getting

- • Juliet hoped there was enough poison in Romeo’s

- her.

- • More: fears, wants (sometimes simultaneous but incompatible)

- • Permanent compared with instantaneous wants.

MENTAL EVENTS (INCLUDING ACTIONS)- • choose to believe

- • remember

- • consider

- • Infer

- • decide

- • forget

- • realize

- • ignore

MACHINE SELF-AWARENESS- • Easy self-awareness: battery state, memory left

- • Straightorward s-a: the program itself, the programming

- guage specs, the machine specs.

- • Self-simulation: Any given number of steps, can’t do

- “Will I ever stop?”, “Will I stop in less than n steps in general—in

- less than n steps.

- • Its choices and their inferred consequences (free will)

- • “I hope it won’t rain tomorrow”. Should a machine

- be aware that it hopes? I think it should sometimes.

- • ¬Knows(I, T T elephone(M M ike)), so I’ll have to look

WHY WE NEED CONCEPTS AS OBJECTS- We had ¬Knows(I, T T elephone(M M ike)), and I’ll have

- up.

- Suppose T elephone(M ike) = “321-758000. If we write

- ¬Knows(I, T elephone(M ike)), then substitution would

- ¬Knows(I, “321-758000), which doesn’t make sense.

- There are various proposals for getting around this.

- advocated is some form of modal logic. My proposal is

- individual concepts as objects, and represent them b

- symbols, e.g. doubling the ﬁrst letter.

- There’s more about why this is a good idea in my “First

- theories of individual concepts and propositions”

WE ALSO NEED CONTEXTS AS OBJECTS- We write

c : p

- to assert p while in the context c. Terms also can

- using contexts. c : e is an expression e in the context

- The main application of contexts as objects is to assert

- between the objects denoted by diﬀerent expressions in

- contexts. Thus we have

c : Does(J oe, a) = SpecializeActor(c, J oe) : a,- or, more generally,

SpecializesActor(c, c0, J oe) → c : Does(J oe, a)) = c- Such relations between expressions in diﬀerent contexts

- using a situation calculus theory in which the actor is

- itly represented in an outer context in which there is

- one actor.

- We also need to express the relation between an external

- in which we refer to the knowledge and awareness of

- and AutoCar1’s internal context in which it can use “I”.

SELF-AWARENESS EXPRESSED IN LOGICALFORMULAS—1

- Pat is aware of his intention to eat dinner at home.

- c(Awareness(P at)) : Intend(I, M M od(AAt(HHome), E

- Awareness(P at) is a context. Eat(Dinner) denotes the

- act of eating dinner, logically diﬀerent from eating Steak

- M od(At(Home), Eat(Dinner)) is what you get when

- the modiﬁer “at home” to the act of eating dinner. Intend

- says that I intend X. The use of I is appropriate

- context of a person’s (here Pat’s) awareness.

- We should extend this to say that Pat will eat dinner

- unless his intention changes. This can be expressed b

- like

¬Ab17(P at, x, s) ∧ Intends(P at, Does(P at, x), s→ (∃s0 > s)Occurs(Does(P at, x), s).- in the notation of (McCarthy 2002).

FORMULAS—2

- • AutoCar1 is driving John from Oﬃce to Home. AutoCa

- aware of this. Autocar1 becomes aware that it is low

- gen. AutoCar1 is permanently aware that it must ask p

- to stop for gas, so it asks for permission. Etc., Etc. These

- are expressed in a context C0.

C0 :

Driving(I, J ohn, Home1)

∧Aware(DDriving(II, J J ohn, HHome)

∧OccursBecomes(Aware(I, LLowf uel(AAutoCar1)))∧OccursBecomes(W ant(I, SStopAt(GGasStation1)))∧

QUESTIONS

- • Does the lunar explorer require self-awareness? What

- the entries in the recent DARPA contest?

- • Do self-aware reasoning systems require dealing with

- opacity? What about explicit contexts?

- • Where does tracing and journaling involve self-awareness?

- • Does an online tutoring program (for example, a program

- teaches a student Chemistry) need to be self aware?

- • What is the simplest self-aware system?

- • Does self-awareness always involve self-monitoring?

- • In what ways does self-awareness diﬀer from awareness

- agents? Does it require special forms of representation

- tecture?

REFERENCES

- Some Philosophical Problems from the Standpoint of

- Intelligence

- John McCarthy and Patrick J. Hayes

- Machine Intelligence 4, 1969

- also http://www-formal.stanford.edu/jmc/mcchay69.html

- Actions and other events in situation calculus

- John McCarthy

- KR2002

- also http://www-formal.stanford.edu/jmc/sitcalc.html.

