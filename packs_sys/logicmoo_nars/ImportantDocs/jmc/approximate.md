- APPROXIMATE OBJECTS AND APPROXIMATE THEORIES

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

Abstract

We propose to extend the ontology of logical

AI to include approximate objects, approx-

imate predicates and approximate theories.

Besides the ontology we treat the relations

among diﬀerent approximate theories of the

same phenomena.

Approximate predicates can’t have complete

if-and-only-if deﬁnitions and usually don’t

even have deﬁnite extensions. Some approx-

imate concepts can be reﬁned by learning

more and some by deﬁning more and some

by both, but it isn’t possible in general to

make them well-deﬁned. Approximate con-

cepts are essential for representing common

sense knowledge and doing common sense

reasoning. Assertions involving approximate

concepts can be represented in mathematical

logic.

A sentence involving an approximate con-

cept may have a deﬁnite truth value even

if the concept is ill-deﬁned.

It is deﬁnite

that Mount Everest was climbed in 1953 even

though exactly what rock and ice is included

in that mountain is ill-deﬁned. Likewise, it

harms a mosquito to be swatted, although

we haven’t a sharp notion of what it means

to harm a mosquito. Whatif(x,p), which de-

notes what x would be like if p were true, is

an important kind of approximate object.

The article treats successively approximate

objects,

and for-

malisms for describing how one object or the-

ory approximates another.

approximate

theories,

Our discussion will be adequate ifit has as much clearness as thesubject matter admits of, for pre-cision is not to be sought for alikein all discussions, any more thanin all the products of the crafts.—Aristotle, Nicomachean EthicsIntroduction

We propose to extend the ontology of logical AI toinclude approximate objects, approximate predicatesand approximate theories. Besides the ontology wediscuss relations among diﬀerent approximations tothe same or similar phenomena.

The article will be as precise as we can make it. Weapply Aristotle’s remark to the approximate theoriesthemselves. The article treats three topics.Approximate objects These are not fully deﬁned,e.g. the wishes of the United States. They maybe approximations to more fully deﬁned objects orthey may be intrinsically approximate (partial).We give lots of examples, because we don’t haveprecise deﬁnitions.

Approximate theories These often involve neces-sary conditions and suﬃcient conditions but lackconditions that are both necessary and suﬃcient.Relations among approximate entities It is of-ten necessary to relate approximate entities, e.g.objects or theories, to less approximate entities,e.g. to relate a theory in which one block is juston another to a theory in which a block may bein various positions on another.

In principle, AI theories, e.g. the original proposalsfor situation calculus, have allowed for rich entitieswhich could not be fully deﬁned. However, almost- all theories used in existing AI research has not taken

- advantage of this generality. Logical AI theories have

- resembled formal scientiﬁc theories in treating well-

- deﬁned objects in well-deﬁned domains. Human-level

- AI will require reasoning about approximate entities.

- Approximate predicates can’t have complete if-and-

- only-if deﬁnitions and usually don’t even have def-

- inite extensions. Moreover, approximate entities of-

- ten don’t have equality conditions. Some approximate

- concepts can be reﬁned by learning more and some by

- deﬁning more and some by both, but it isn’t possible

- in general to make them well-deﬁned. Approximate

- concepts are essential for representing common sense

- knowledge and doing common sense reasoning. In this

- article, assertions involving approximate concepts are

- represented in mathematical logic.

- A sentence involving an approximate concept may

- have a deﬁnite truth value even if the concept is ill-

- deﬁned. It is deﬁnite that Mount Everest was climbed

- in 1953 even though exactly what rock and ice is in-

- cluded in that mountain is ill-deﬁned. We discuss the

- extent to which we can build solid intellectual struc-

- tures on such swampy conceptual foundations.

- Quantitative approximation is one kind considered—

- but not the most interesting or the kind that re-

- quires logical innovation. Fuzzy logic involves a semi-

- quantitative approximation, although there are exten-

- sions as mentioned in [Zad99].

- For AI purposes, the key problem is relating diﬀerent

- approximate theories of the same domain. For this

- we use mathematical logic fortiﬁed with contexts as

- objects. Further innovations in logic may be required

- to treat approximate concepts as ﬂexibly in logic as

- people do in thought and language.

- Looked at in suﬃcient detail, all concepts are approx-

- imate, but some are precise enough for a given pur-

- pose. McCarthy’s weight measured by a scales is pre-

- cise enough for medical advice, and can be regarded

- as exact in a theory of medical advice. On the other

- hand, McCarthy’s purposes are approximate enough

- so that almost any discussion of them is likely to bump

- against their imprecision and ambiguity.

- Many concepts used in common sense reasoning are

- imprecise. Here are some questions and issues that

- arise.

- 1. What rocks and ice constitute Mount Everest?

- 2. When can it be said that one block is on another

block, so that

On(Block1, Block2) may be asserted?

Let there be an axiomatic theory in situation cal-culus in which it can be shown that a sequence ofactions will have a certain result. Now supposethat a physical robot is to observe that one blockis or is not on another and determine the actionsto achieve a goal using situation calculus. It is im-portant that the meanings of On(Block1, Block2)used in solving the problem theoretically and thatused by the robot correspond well enough so thatcarrying out a plan physically has the desired ef-fect. How well must they correspond?3. What are the logical relations between diﬀerentlogical speciﬁcations of an approximate object?4. What are the relations between diﬀerent approx-imate logical theories of a domain?

We claim

1. The common sense informatic situation often in-volves concepts which cannot be made precise.This is a question of the information available andnot about computation power. It is not a specif-ically human limitation and will apply to com-puters of any possible power. This is not a claimabout physics; it may be that a discoverable setof physical laws will account for all phenomena.It is rather a question of the information actuallyavailable about particular situations by people orrobots with limited opportunities to observe andcompute.

2. Much pedantry in science and in philosophy re-sults from demanding if-and-only-if deﬁnitionswhen this is inappropriate.

3. None of the above objects and predicates admitsa completely precise if-and-only-if deﬁnition.4. Formalized scientiﬁc theories, e.g. celestial me-chanics and the blocks world often have preciseconcepts. They are necessary tools for under-standing and computation. However, they areimbedded in common sense knowledge about howto apply them in world of imprecise concepts.Moreover, the concepts are precise only within thetheory. Most AI theories have also had this char-acter.

5. Almost all concepts are approximate in the fullcommon sense informatic situation. However,many are precise, i.e. have if-and-only-if deﬁni-tions in particular contexts. Thus in the contextof a particular grocery, an object is a lemon if andonly if it is a small yellow fruit.

- 6. The human reasoning processes involving com-

mon sense knowledge correspond only partly to

the mathematical and logical reasoning processes

that humans have used so successfully within sci-

entiﬁc and mathematical theories.

- 7. Nevertheless, mathematical logic is an ap-

propriate tool for representing this knowl-

edge and carrying out this reasoning in in-

telligent machines. The use of logic will cer-

tainly require adaptations, and the logic itself may

require modiﬁcations.

- 8. Key tools for common sense reasoning by ma-

chines will be approximate concepts and approxi-

mate theories. These are in addition to formalized

non-monotonic reasoning and formal theories of

context.

- 9. The most important notion of approximation for

common sense reasoning is not the familiar nu-

merical approximation but a new kind of logical

approximation appropriate for common sense rea-

soning. These mostly diﬀer from the approxima-

tions of fuzzy logic.

- In the subsequent sections of this article, tools will be

- proposed for reasoning with approximate concepts.

- The article treats successively approximate objects, ap-

- proximate theories, and formalisms for describing how

- one object or theory approximates another.

- 2 What kinds of approximate

concepts are there?

- Concepts can be approximate in at least two ways.

- On one hand, a concept may approximate another

- more deﬁnite but incompletely known concept. This

- situation is prevalent with natural kinds. Lemons are

- a deﬁnite species, but no-one knows all about them.

- In particular, a child may be barely able to tell lemons

- from other yellow fruit but nevertheless has a concept

- of lemon that may be adequate to ﬁnd the lemons in

- the supermarket. The child can improve its concept

- of lemon by learning more. The fact that there isn’t a

- continuum of fruits ranging from lemons to grapefruit

- is an important part of the fact that lemons form a

- natural kind. This fact also makes it possible for bi-

- ologists to learn speciﬁc facts about lemons, e.g. to

- sequence lemon DNA.

- On the other hand, the legal concept of a person’s

- taxable income is reﬁned by deﬁning more. Taxable

- income is partly a natural kind. A person’s concept of

his own taxable income is an approximation to the lessapproximate legal concept. He could learn more or itcould be deﬁned more as the legal concept is deﬁnedmore. However, learning more about the legal concepteventually reaches a point where there is no further re-ﬁnement on which people thinking independently willagree. There isn’t a true notion of taxable income foreconomists to discover.

My concept of my taxable income and even my taxaccountant’s concept of my taxable income has bothaspects. More can be learned about what deductionsare allowed, and also the concept gets reﬁned by thecourts.

Here are some examples.

2.1 What if ?

Whatif(p, x) is what x would be if p were true. Ex-amples: (1) John McCarthy if he had gone to Harvardrather than to Caltech as an undergraduate. (2) Mycar if it hadn’t been backed into today. (3) The cake Iwould have baked if I had known you were coming. (4)What the world would be like today if Pickett’s chargehad been successful. (5) What would have happenedif another car had come over the hill when you passedthat Mercedes just now.

Whatif(p, x) is an intrinsically approximate concept.How approximate depends on p and x. “What if an-other car had come over the hill when you passed”is much less approximate than“What if wishes werehorses”.

[CM99] treats useful counterfactual condi-tional sentences and gives many examples.Whatif can serve as the foundation for other con-cepts, including counterfactual conditional sentencesand statements of causality.

[CM99] treats usefulcounterfactual conditional sentences and gives manyexamples.

Fiction provides an interesting class of approximateobjects and theories, especially historical ﬁction, inwhich the author tries to ﬁt his characters and theirlives into a background of historical fact. Commonsense knowledge tells us that Sherlock Holmes wouldhave had a mother, but Conan Doyle does not provideus with a name. A deﬁnite address is given, but therewas never a house there corresponding to Doyle’s de-scription. The author need only deﬁne his world toa point that lets the reader answer the questions theauthor wants the reader to ask.

- 2.2 Mount Everest.

- It is clear that we cannot hope to formulate a useful

- deﬁnition of Mount Everest that would tell about every

- rock whether it is part of the mountain. However, we

- might suppose that there is a truth of the matter for

- every rock even though we cannot know it. Our axioms

- would then be weaker than the truth. The question

- would be settled for some rocks and not for others.

- Not even this is appropriate. The concept of the

- territory of Mount Everest may be further reﬁned in

- the future—and reﬁned in incompatible ways by diﬀer-

- ent people. If we suppose that there is a truth about

- what rocks are part of the mountain, then the people

- reﬁning it in diﬀerent ways would get to argue fruit-

- lessly about which deﬁnition is getting closer to the

- truth. On the other hand, there is a truth of the mat-

- ter, which may someday be discovered, about whether

- Mallory and Irvine reached the summit in 1924.

- Consider two theories of mountain climbing, T 1 and

- T 2. Besides these theories, there is T 3 based on

- plate tectonics that tells us that Everest is still get-

- ting higher.

- In the simpler theory T 1, there is a list of names of

- mountains paired with lists of climbing expeditions or

- names of climbers. As a logical theory it would have

- sentences like.

There are other approximate theories involving MountEverest.

One such theory that lists names of mountains andthe continents containing them.

Thus we haveA less approximate theoryIn(Annapurna, Asia).

gives countries, e.g.

In(Annapurna, Nepal).Astill

less approximate theory gives locations, e.g.Location(Annapurna) = (28◦32(cid:48), 83◦53(cid:48)).2.3 The wants and actions of the UnitedStates

In 1990 the United States wanted Iraq to withdrawfrom Kuwait. Evidence for this proposition was pro-vided by the statements of U.S. oﬃcials and sendingtroops to Saudi Arabia. It was correctly inferred fromthis proposition that the U.S. would do something toimplement its desires. This inference was made withonly an approximate notion of “the US wants”.Nevertheless, the facts can be expressed by formulaslike the following.

Says(President(USA), x) → Says(USA, x),(2)Says(President(USA),

Wants(USA, Leaves(Iraq, Kuwait))),(3)Climbed(Everest, 1953, {Hillary, Tenzing}).

(1)

Says(USA, Wants(USA, Leaves(Iraq, Kuwait))),(4)- The larger theory T 2 contains routes up the mountain

- of the various parties. Routes are approximate entities.

- T 1 is an approximation to T 2, but T 1, may be re-

- garded as not approximate at all. In particular, it can

- be complete, e.g. it decides any sentence in its limited

- language.

- T 1 and T 2 may be related using formalized contexts

- as in [McC93a] or [MB97], but we won’t do that here.

- One approximate theory may be less approximate than

- another. We want to discuss relations between sen-

- tences in a theory and sentences in a less approximate

- theory. It makes the ideas neater if we imagine that

- there are true and complete theories of the world even

- if they’re not ﬁnitely expressible and none of the lan-

- guage of any of them is known. This permits regarding

- approximating the world as a case of one theory ap-

- proximating another. If this is too platonic for your

- taste, you can regard approximating the world as a

- diﬀerent notion than that of one theory approximat-

- ing another.

Says(entity, Wants(entity, x)) → wants(entity, x),wants(USA, Leaves(Iraq, Kuwait)),(5)(6)wants(x, y) → (∃z)(Does(x, z) ∧ Achieves(z, y)). (7)From these we infer

(∃z)(Does(USA, z)∧Achieves(z, Leaves(Iraq, Kuwait)).(8)We have not introduced all the necessary qualiﬁca-tions, and we have not used a proper theory of actions.There also should be some more theory of Wants, Says,and Does.

- Someone with a suﬃciently detailed knowledge of

- events in the Middle East and of the American de-

- cision making community might not need “The US

- wants . . . ”, because he could work directly with the

- various decision makers and the motivations and ef-

- fects of their actions. The rest of us must make do

- with more approximate concepts. This will apply even

- more to future students of 20th century history.

- A fuzzy logic theory would take “The US wants”

- for granted and concentrate on “The US moderately

- wants” and “The US strongly wants”.

- 2.4 The Blocks World as an Approximate

Theory

- The usual AI situation calculus blocks world has a

- propositional ﬂuent On(x, y) asserting that block x is

- on block y. We can assert Holds(On(x, y), s) about

- some situation s and have the action Move(x, y) that

- moves block x on top of block y.

- Suppose this formalism is being used by a robot acting

- in the real world. The concepts denoted by On(x, y),

- etc. are then approximate concepts, and the theory

- is an approximate theory. Our goal is to relate this

- approximate theory to the real world. Similar consid-

- erations would apply if we were relating it to a more

- comprehensive but still approximate theory.

other, so it can happen that it isn’t justiﬁed to sayeither that x is on y or that it isn’t. Cond1(s) andCond2(s) need not be mutually exclusive. In thatcase the theory associated with Cblocks would beinconsistent. However, unless there are strong lift-ing rules the inconsistency within Cblocks cannotinfect the rest of the reasoning.

Notice that the theory in the context Cblocks approx-imates a theory in which blocks can be in diﬀerentorientations on each other or in the air or on the tablein quite diﬀerent sense than numerical approximation.2.5 Relating two blocks world theoriesprevious

theory T1blocks world

Our

usesHolds(On1(b1, b2), s).

Our less approximate newtheory T2 uses Holds(On2(b1, b2, d), σ) where d is adisplacement of b1 from being centered on b2. SinceT2 has another parameter for On, many situations σcan correspond to a single situation s in T1.We may have the relations

Holds(On2(b1, b2, d)), σ)

→ Holds(On1(b1, b2)), St1(σ))Holds(On1(b1, b2)), St1(σ))

→ (∃d)Holds(On2(b1, b2, d)), σ).(10)- We use formalized contexts as in [McC93b] and

- [MB97]. and let Cblocks be a blocks world context

- with a language allowing On(x, y), etc.

Here St1(σ) is the T1-situation corresponding to σ.For simplicity we are assuming that every T2-situationhas a corresponding T1-situation.

- Holds(On(x, y), s) is approximate in at least the fol-

- lowing respects.

T2 is a tiny step from T1 in the direction of the realworld.

- • In the original intended interpretation of the sit-

uation calculus, a situation s is a snapshot of the

world at a given time. According to the theory of

relativity, distant simultaneity is ill-deﬁned. This

is the least of our worries.

- • Whether block x is on block y may be ambigu-

ous in the real world. Block x may be partly on

and partly oﬀ. We can handle the relation by

sentences

Cond1(s) → Ist(Cblocks, Holds(On(x, y), s))

Cond2(s) → Ist(Cblocks, Holds(Not On(x, y), s)).

(9)

Cond1(s) and Cond2(s) are respectively condi-

tions in the outer context on the situation s that x

shall be on y and x shall not be on y in the context

Cblocks. These need not be the negations of each

Suppose a robot uses T1 as a theory of the blocksworld and takes actions accordingly, but the real worldcorresponds to T2. This is quite a simpliﬁcation, butmaybe it has enough of the right formal properties.The simplest case is where there are two blocks, andthe initial situation is represented by

{Holds(On1(B1, Table), S0), Holds(On1(B2, Table), S0)}in T1, but the real world facts are

{Holds(On2(B1, Table, 3.0), 2.1 cm),Holds(On(B2, Table, 4.5), 3.2 cm)}where

S0 = St1(σ0).

(11)(12)(13)The goal

is Holds(On(B1, B2), which might alsobe written as (λs)Holds(On(B1, B2), s). Anyway- the robot

- Move(B1, B2) and infers that

infers

that

the appropriate action is

- Holds(On1(B1, B2), Result(Move1(B1, B2), S0)),

- where we are omitting various qualiﬁcations.

- In T2, the form of an action is Move2(b1, b2, d), and

- the eﬀect of a move action is given by

c(Today) : Harms(Swat(M1073543907), M1073543907)(18)(14)

as a proposition about this particular mosquito.A person’s wealth at a given time can be measuredas an amount of money. His wealth increases as he ispaid and decreases as he spends money. However, overa period of 10,000 years, the wealth or welfare of thisindividual is undeﬁned.

- Holds(On(b1, b2, d), Result2(Move2(b1, b2, d), σ)).

Nevertheless, wealth and welfare are useful concepts.- The translation of a move action in T1 to a move action

- in T2 may be given by

(15)

- Action12(Move1(b1, b2), St1(σ))

- =

Move2(b1, b2, Displacement(b1, b2, σ)).

(16)

- The key point is that the move in T2 corresponding to

- a move in T1 depends on the blocks being moved and

- also on the situation.

Fiction provides an interesting class of approximateobjects and theories, especially historical ﬁction, inwhich the author tries to ﬁt his characters and theirlives into a background of historical fact. Commonsense knowledge tells us that Sherlock Holmes wouldhave had a mother, but Conan Doyle does not provideus with a name. A deﬁnite address is given, but therewas never a house there corresponding to Doyle’s de-scription. The author need only deﬁne his world toa point that lets the reader answer the questions theauthor wants the reader to ask.

- The success of the one step plan worked out in T1 in

- the less approximate world T2 is expressed by

2.7 States of motion

- St1(Result2(Action12(Move1(B1, B2), St1(σ0))))

- =

Result1(Move(B1, B2), St1(σ0)).

- The success of multi-step plans would be expressed by

- longer correspondence formulas.

- These are commutativity relations.

- 2.6 Temporary entities; wealth and welfare

- In natural language, the present tense of the verb “to

- be” is used for asserting an intrinsic property of an

- entity and for asserting a property that is expected to

- hold long enough to provide a constant background for

- other events under discussion.

- The wealth or welfare of a human or animal is such a

- temporary property.

- The welfare of a mosquito over a short time is deﬁn-

- able. It harms the mosquito to be squashed and helps

- it if it ﬁnds exposed skin from which to extract blood.

- Over a year the welfare of an individual mosquito is

- not deﬁnable. If it is to be deﬁned, the concepts, e.g.

- descendants, will be quite diﬀerent.

- This suggest using contexts. We have

These present a general reason for using approximateconcepts.

Suppose a robot is walking from here to there in dis-crete steps.

(17)

Holds(Walking(R2D2, Here, There), s)describes a situation that can persist. Sentences givingthe robot’s position at a given time must be frequentlyupdated. An important human reason for forming anapproximate concept is to get a ﬂuent that will per-sist. This reason will also apply to robots but perhapsto a lesser extent, since computers can perform morefrequent updating than can people.

2.8 Folk physics and folk psychology asapproximate theories

For example, the concept of “X believes P” is approxi-mate both in the criterion for belief and in what is theobject of a belief. These notions will also be approxi-mate for robots.

Much of the criticism of folk psychology may comefrom demanding that it be more precise than is rea-sonable. Aristotle’s aphorism applies here.- 3 Propositional approximate theories

- Many topics take an especially simple form when one

- uses propositions instead of predicates—and accepts

- the reduced expressivity.

- Here is one approach to deﬁning approximate propo-

- sitional theories.

- Let reality, e.g. the situation in a room, be given by

- the values of the propositional variables r1, . . . , rn. As-

- sume that reality is not directly observable. n may be

- very large, e.g. like Avogadro’s number.

- Let the values of the propositions o1, . . . , ok be observ-

- able. They are functions of reality given by

oi = Oi(r1, . . . , rn),

- where k is a modest number corresponding to how

- many bits we can actually observe.

- We suppose that we want to know the values of

- q1, . . . , ql, which are related to reality by

qi = Qi(r1, . . . , rn),

- where l is also a modest number.

- An approximate theory AT is given by functions

- Q(cid:48)

- i(o1, . . . , ok), i.e. AT undertakes to give what we

- want to know in terms of the observations.

- If we are lucky in how reality turns out, the Q(cid:48) func-

- tions correspond to the Q functions, i.e.

The logical elements are treated as boolean functions,deﬁned by their truth tables. The theory deﬁnes thebehavior of any circuit in terms of composition of thefunctions associated with the logical elements. Theoutputs of a circuit are given by the theory for anycombination of boolean inputs. Fan-in and fan-outrestrictions are outside the logical theory, as are timingconsiderations.

Now consider sequential circuits including ﬂipﬂops.Now what happens is deﬁned only for some combi-nations of inputs. For example, the behavior of a Dﬂipﬂop is not deﬁned when its 0 and 1 inputs are giventhe same value, whether that value be 0 or 1. The be-havior is only deﬁned when the inputs are opposite.The manufacturer does not say what will happen whenthese and other restrictions are not fulﬁlled, does notwarrant that two of his ﬂipﬂops will behave the sameor that a ﬂipﬂop will retain whatever behavior it hasin these forbidden cases.

This makes the concept of D ﬂipﬂop itself approxi-mate, perhaps not in the same sense as some otherapproximate theories.

Thus the theory of sequential circuits is an approxi-mate theory, and it is not an approximation to a deﬁ-nite less approximate theory of purely digital circuits.This is in spite of the fact that there is (or can be)an electronic theory of these digital circuits which de-scribes their behavior. In that theory one D ﬂipﬂop isdiﬀerent from another and changes its behavior as itages.

Lucky(r1, . . . , rn) → qi = Q(cid:48)

i(o1, . . . , ok)

4 When an approximate conceptbecomes precise

- for i = 1, . . . , l, i.e.

Lucky(r1, . . . , rn) → [Qi(r1, . . . , rn)

≡ Q(cid:48)

i(O1(r1, . . . , rn), . . . Ok(r1, . . . , rn))].

Suppose an approximate concept represented by apredicate p(x) has a suﬃcient condition suf f (x) anda necessary condition nec(x). Thus we have- If we are very fortunate we may be able to know when

- we are lucky, and we have

KnowLucky(o1, . . . , ok) → Lucky(r1, . . . , rn).

- At the moment, we have no useful propositional ap-

- proximate theories in mind, and the reader should

- remember Einstein’s dictum “Everything should be

- made as simple as possible—but not simpler.”

- 3.1 Approximate Theories of Digital Circuits

- Consider ﬁrst combinational circuits built from logic

- elements, i.e. without bridges and other sneak paths.

(∀x)(suf f (x) → p(x)), and

(∀x)(p(x) → nec(x)).

(19)In general the suﬃcient and the necessary conditionswill not coincide, i.e. we will not have

(∀x)(nec(x) ≡ suﬀ(x)).

(20)However, they made coincide with some restriction onx, i.e. we may have

(∀x)(special(x) → (nec(x) ≡ suﬀ(x)).(21)- Another way an approximate concept may become def-

- inite is by a mapping from the space in which it is ﬁrst

- formalized into more restricted space. We’ll combine

- specialization with mapping in

[McC93b] John McCarthy.

malizing

1993.

formal.stanford.edu/jmc/context.html.context.

Available

as

NotesInonfor-IJCAI93,http://www-[Zad99]

Lotﬁ A. Zadeh. From computing with num-bers to computing with words—from ma-nipulation of measurements to manipula-tion of perceptions. IEEE Transactions onCircuits and Systems—I. Fundamental the-ory and applications, 45(1):105–119, Jan-uary 1999.

(∀x)(special(x) → (nec(f (x)) ≡ suﬀ(f (x)),

(22)

- where the function f maps a subset of the original

- domain into a specialized domain in which the concept

- p(x) becomes deﬁnite.

- 5 Conclusions, remarks, and

acknowledgements

- The present paper is exploratory. The large number

- of approximate concepts we discuss are approximate

- in diﬀerent ways, but we don’t have a classiﬁcation.

- Many of the applications of approximate objects will

- result from theories connecting diﬀerent levels and

- kinds of approximation.

- I thank Eyal Amir, Johan van Benthem, Saˇsa Buvaˇc,

- Tom Costello, Aarati Parmar for helpful comments.

- This research has been partly supported by ARPA

- contract no. USC 621915, the ARPA/Rome Labora-

- tory planning initiative under grant (ONR) N00014-

- 94-1-0775 and ARPA/AFOSR under (AFOSR) grant

- # F49620-97-1-0207.

- References

- [CM99]

- [MB97]

Tom Costello and John McCarthy. Useful

Counterfactuals1. Electronic Transactions

on Artiﬁcial Intelligence, 1999. submitted

1999 July.

John McCarthy and Saˇsa Buvaˇc.

For-

malizing context (expanded notes).

In

A. Aliseda, R.J. van Glabbeek,

and

D. Westerst˚ahl, editors, Computing Natural

Language. Center for the Study of Language

and Information, Stanford University, 1997.

- [McC90]

John McCarthy.

Formalizing Common

Sense: Papers by John McCarthy. Ablex

Publishing Corporation, 1990.

- [McC93a] John McCarthy. Notes on Formalizing Con-

text2. In IJCAI-93, 1993.

- 1http://www-formal.stanford.edu/jmc/counterfactuals.html

- 2http://www-formal.stanford.edu/jmc/context.html

