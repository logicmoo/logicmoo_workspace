FROM HERE TO HUMAN-LEVEL AI

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

jmc@cs.stanford.edu

http://www-formal.stanford.edu/jmc/

Abstract

It is not surprising that reaching

human-level AI has proved to be dif-

ﬁcult and progress has been slow—

though there has been important

progress. The slowness and the de-

mand to exploit what has been dis-

covered has led many to mistakenly

redeﬁne AI, sometimes in ways that

preclude human-level AI—by rele-

gating to humans parts of the task

that human-level computer programs

would have to do. In the terminology

of this paper, it amounts to settling

for a bounded informatic situation in-

stead of the more general common

sense informatic situation.

the

“brittleness”

Overcoming

of

present AI systems and reaching

human-level AI requires programs

that deal with the common sense

informatic situation—in which the

phenomena to be taken into account

in achieving a goal are not ﬁxed in

advance.

We discuss reaching human-level AI,

emphasizing logical AI and especially

emphasizing representation problems

of

reasoning.

Ideas

reasoning in the com-

mon sense informatic situation in-

information and of

for

clude nonmonotonic reasoning, ap-proximate concepts, formalized con-texts and introspection.

1 What is Human-Level AI?

The ﬁrst scientiﬁc discussion of human levelmachine intelligence was apparently by AlanTuring in the lecture [Turing, 1947]. The no-tion was ampliﬁed as a goal in [Turing, 1950],but at least the latter paper did not say whatwould have to be done to achieve the goal.Allen Newell and Herbert Simon in 1954 werethe ﬁrst people to make a start on program-ming computers for general intelligence. Theywere over-optimistic, because their idea ofwhat has to be done to achieve human-level in-telligence was inadequate. The General Prob-lem Solver (GPS) took general problem solv-ing to be the task of transforming one expres-sion into another using an allowed set of trans-formations.

Many tasks that humans can do, humans can-not yet make computers do. There are two ap-proaches to human-level AI, but each presentsdiﬃculties. It isn’t a question of deciding be-tween them, because each should eventuallysucceed; it is more a race.

1. If we understood enough about how thehuman intellect works, we could simulateit. However, we don’t have have suﬃ-

cient ability to observe ourselves or others

to understand directly how our intellects

work. Understanding the human brain

well enough to imitate its function there-

fore requires theoretical and experimental

success in psychology and neurophysiol-

ogy. 1 See [Newell and Simon, 1972] for

the beginning of the information process-

ing approach to psychology.

- 2. To the extent that we understand the

problems achieving goals in the world

presents to intelligence we can write intel-

ligent programs. That’s what this article

is about.

- What problems does the world present to in-

- telligence? More narrowly, we consider the

- problems it would present to a human scale

- robot faced with the problems humans might

- be inclined to relegate to suﬃciently intelli-

- gent robots. The physical world of a robot

- contains middle sized objects about which its

- sensory apparatus can obtain only partial in-

- formation quite inadequate to fully determne

- the eﬀects of its future actions.

Its mental

- world includes its interactions with people and

- also meta-information about the information

- it has or can obtain.

- Our approach is based on what we call the

- common sense informatic situation. In order

- to explain the common sense informatic situ-

- ation, we contrast it with the bounded infor-

- matic situation that characterizes both formal

- scientiﬁc theories and almost all (maybe all)

- experimental work in AI done so far.2

- 1Recent work with positron emission tomography has

- identiﬁed areas of the brain that consume more glucose

- when a person is doing mental arithmetic. This knowledge

- will help build AI systems only when it becomes possible

- to observe what is going on in these areas during mental

- arithmetic.

- 2The textbook [David Poole and Goebel, 1998] puts it

- this way. “To get human-level computational intelligence

- it must be the agent itself that decides how to divide up

- the world, and which relationships to reason about.

A formal theory in the physical sciences dealswith a bounded informatic situation. Scientistsdecide informally in advance what phenomenato take into account. For example, much ce-lestial mechanics is done within the Newtoniangravitational theory and does not take into ac-count possible additional eﬀects such as out-gassing from a comet or electromagnetic forcesexerted by the solar wind.

If more phenom-ena are to be considered, a person must makea new theory. Probabilistic and fuzzy uncer-tainties can still ﬁt into a bounded informaticsystem; it is only necessary that the set of pos-sibilities (sample space) be bounded.Most AI formalisms also work only in abounded informatic situation. What phenom-ena to take into account is decided by a personbefore the formal theory is constructed. Withsuch restrictions, much of the reasoning can bemonotonic, but such systems cannot reach hu-man level ability. For that, the machine willhave to decide for itself what information isrelevant. When a bounded informatic systemis appropriate, the system must construct orchoose a limited context containing a suitabletheory whose predicates and functions connectto the machine’s inputs and outputs in an ap-propriate way. The logical tool for this is non-monotonic reasoning.

2 The Common Sense InformaticSituation

The key to reachingContention:

human-level AI is making systems thatoperate successfully in the commonsense informatic situation.

In general a thinking human is in what we callthe common sense informatic situation ﬁrstdiscussed in3 [McCarthy, 1989].

It is moregeneral than any bounded informatic situation.The known facts are incomplete, and there isno a priori limitation on what facts are rel-3http://www-formal.stanford.edu/jmc/ailogic.html- evant.

It may not even be decided in ad-

- vance what phenomena are to be taken into

- account. The consequences of actions cannot

- be fully determined. The common sense in-

- formatic situation necessitates the use of ap-

- proximate concepts that cannot be fully de-

- ﬁned and the use of approximate theories in-

- volving them. It also requires nonmonotonic

- reasoning in reaching conclusions.

- The common sense informatic situation also

- includes some knowledge about the system’s

- mental state.

- A nice example of the common sense infor-

- matic situation is illustrated by an article in

- the American Journal of Physics some years

- ago. It discussed grading answers to a physics

- problem. The exam problem is to ﬁnd the

- height of a building using a barometer. The

- intended solution is to measure the air pres-

- sure at the top and bottom of the building

- and multiply the diﬀerence by the ratio of the

- density of mercury to the density of air.

- However, other answers may be oﬀered.

(1)

- drop the barometer from the top of the build-

- ing and measure the time before it hits the

- ground. (2) Measure the height and length of

- the shadow of the barometer and measure the

- length of the shadow of the building. (3) Rap-

- pel down the building using the barometer as

- a measuring rod. (4) Lower the barometer on

- a string till it reaches the ground and measure

- the string. (5) Oﬀer the barometer to the jani-

- tor of the building in exchange for information

- about the height. (6) Ignore the barometer,

- count the stories of the building and multiply

- by ten feet.

- Clearly it is not possible to bound in advance

- the common sense knowledge of the world

- that may be relevant to grading the prob-

- lem. Grading some of the solutions requires

- knowledge of the formalisms of physics and the

- physical facts about the earth, e.g.

the law

- of falling bodies or the variation of air pres-

sure with altitude. However, in every case,the physics knowledge is embedded in com-mon sense knowledge. Thus before one canuse Galileo’s law of falling bodies s = 12 gt2, oneneeds common sense information about build-ings, their shapes and their roofs.

Bounded informatic situations are obtained bynonmonotonically inferring that only the phe-nomena that somehow appear to be relevantare relevant.

In the barometer example, thestudent was expected to infer that the barom-eter was only to be used in the conventionalway for measuring air pressure. For example,a reasoning system might do this by apply-ing circumscription to a predicate relevant in aformalism containing also metalinguistic infor-mation, e.g. that this was a problem assignedin a physics course. Formalizing relevance ina useful way promises to be diﬃcult.Common sense facts and common sense rea-soning are necessarily imprecise. The impreci-sion necessitated by the common sense infor-matic situation applies to computer programsas well as to people.

Some kinds of imprecision can be representednumerically and have been explored with theaid of Bayesian networks, fuzzy logic and simi-lar formalisms. This is in addition to the studyof approximation in numerical analysis and thephysical sciences.

3 The Use of Mathematical LogicWhat about mathematical logical languages?Mathematical

logic was devised to formal-ize precise facts and correct reasoning.Itsfounders, Leibniz, Boole and Frege, hoped touse it for common sense facts and reasoning,not realizing that the imprecision of conceptsused in common sense language was often anecessary feature and not always a bug. Thebiggest success of mathematical logic was informalizing mathematical theories. Since the- common sense informatic situation requires

- using imprecise facts and imprecise reason-

- ing, the use of mathematical logic for common

- sense has had limited success. This has caused

- many people to give up. Gradually, extended

- logical languages and even extended forms of

- mathematical logic are being invented and de-

- veloped.

- It is necessary to distinguish between mathe-

- matical logic and particular mathematical log-

- ical languages. Particular logical languages

- are determined by a particular choice of con-

- cepts and the predicate and function symbols

- to represent them. Failure to make the dis-

- tinction has often led to error. When a par-

- ticular logical language has been shown inad-

- equate for some purpose, some people have

- concluded that logic is inadequate. Diﬀerent

- concepts and diﬀerent predicate and function

- symbols might still succeed. In the words of

- the drive-in movie critic of Grapevine, Texas,

- “I’m surprised I have to explain this stuﬀ.”

- The pessimists about logic or some particular

- set of predicates might try to prove a theorem

- about its inadequacies for expressing common

- sense.4

- Since it seems clear that humans don’t use

- logic as a basic internal representation formal-

- ism, maybe something else will work better

- for AI. Researchers have been trying to ﬁnd

- this something else since the 1950s but still

- haven’t succeeded in getting anything that is

- ready to be applied to the common sense in-

- formatic situation. Maybe they will eventually

- succeed. However, I think the problems listed

- in the later sections of this article will apply

- to any approach to human-level AI.

- Mathematical logic has been concerned with

- how people ought to think rather than how

- people do think. We who use logic as a basic

- 4G¨odel’s theorem is not relevant to this, because the

- question is not one of decideability or of characterizing

- truth.

AI formalism make programs reason logically.However, we have to extend logic and extendthe programs that use it in various ways.One important extension was the developmentof modal logic starting in the 1920s and usingit to treat modalities like knowledge, belief andobligation. Modalities can be treated eitherby using modal logic or by reifying conceptsand sentences within the standard logic. Myopinion is that reiﬁcation in standard logic ismore powerful and will work better.A second extension was the formalization ofnonmonotonic reasoning beginning in the late1970s—with circumscription and default logicand their variants as the major proposals.Nonmonotonic logic has been studied both aspure mathematics and in application to AIproblems, most prominently to the formaliza-tion of action and causality. Several variantsof the major formalisms have been devised.Success so far has been moderate, and it isn’tclear whether greater success can be obtainedby changing the the concepts and their rep-resentation by predicate and function symbolsor by varying the nonmonotonic formalism. 5We need to distinguish the actual use of logicfrom what Allen Newell,

[Newell, 1981] and[Newell, 1993], calls the logic level and whichwas also proposed in [McCarthy, 1979]6.4 Approximate Concepts andApproximate Theories

Other kinds of imprecision are more funda-mental for intelligence than numerical impre-cision. Many phenomena in the world are ap-propriately described in terms of approximateconcepts. Although the concepts are impre-cise, many statements using them have precisetruth values. We oﬀer two examples: the con-5One referee for KR96 foolishly and arrogantly pro-posed rejecting a paper on the grounds that the inadequacyof circumscription for representing action was known.6http://www-formal.stanford.edu/jmc/ascribing.html- cept of Mount Everest and the concept of the

- welfare of a chicken. The exact pieces of rock

- and ice that constitute Mount Everest are un-

- clear. For many rocks, there is no truth of the

- matter as to whether it is part of Mount Ever-

- est. Nevertheless, it is true without qualiﬁca-

- tion that Edmund Hillary and Tenzing Norgay

- climbed Mount Everest in 1953 and that John

- McCarthy never set foot on it.

- The point of this example is that it is possi-

- ble and even common to have a solid knowl-

- edge structure from which solid conclusions

- can be inferred based on a foundation built on

- the quicksand of approximate concepts with-

- out deﬁnite extensions.

- As for the chicken, it is clear that feeding it

- helps it and wringing its neck harms it, but

- it is unclear what its welfare consists of over

- the course of the decade from the time of its

- hatching. Is it better oﬀ leading a life of poul-

- try luxury and eventually being slaughtered

- or would it be better oﬀ escaping the chicken

- yard and taking its chances on starvation and

- foxes? There is no truth of the matter to be

- determined by careful investigation of chick-

- ens. When a concept is inherently ap-

- proximate, it is a waste of time to try to

- give it a precise deﬁnition. Indeed diﬀer-

- ent eﬀorts to deﬁne such a concept precisely

- will lead to diﬀerent results—if any.

- Most human common sense knowledge in-

- volves approximate concepts, and reaching

- human-level AI requires a satisfactory way

- of representing information involving approxi-

- mate concepts.

- 5 Nonmonotonic Reasoning

- Common sense reasoning is also imprecise in

- that it draws conclusions that might not be

- made if there were more information. Thus

- common sense reasoning is nonmonotonic.

I

- will not go into the details of any of the pro-

posals for handling nonmonotonic reasoning.In particular, getting from the common senseinformatic situation to a bounded informaticsituation needs nonmonotonic reasoning.6 Elaboration Tolerance

Human abilities in the common sense infor-matic situation also include what may becalled elaboration tolerance—the ability toelaborate a statement of some facts withouthaving to start all over. Thus when we beginto think about a problem, e.g. determiningthe height of a building, we form a boundedcontext and try to solve the problem within it.However, at any time more facts can be added,e.g. about the precision with which the timefor the barometer to fall can be estimated us-ing a stop watch and also the possibilities ofacquiring a stop watch.

Elaboration Tolerance7 discusses about 25elaborations of the Missionaries and Cannibalsproblem.

What I have so far said so far about ap-proximate concepts, nonmonotonic reasoningand elaboration tolerance is independent ofwhether mathematical logic, human languageor some other formalism is used.

In my opinion, the best AI results so far havebeen obtained using and extending mathemat-ical logic.

7 Formalization of Context

A third extension of mathematical logic in-volves formalizing the notion of context8[McCarthy, 1993]. Notice that when logicaltheories are used in human communicationand study, the theory is used in a contextwhich people can discuss from the outside. Ifcomputers are to have this facility and are to7http://www-formal.stanford.edu/jmc/elaboration.html8http://www-formal.stanford.edu/jmc/context.html- work within logic, then the “outer” logical lan-

- guage needs names for contexts and sentences

- giving their relations and a way of entering a

- context. Clearly human-level AI requires rea-

- soning about context.

- Human-level AI also requires the ability to

- transcend the outermost context the system

- has used so far. Besides in [McCarthy, 1993],

- this is also discussed in Making Robots

their Mental States9

- Conscious

- [McCarthy, 1996].

of

- Further work includes

- [Buvaˇc et al., 1995].

[Buvaˇc, 1996] and

- 8 Reasoning about

Events—Especially Actions

- Reasoning about actions has been a major AI

- activity, but this paper will not discuss my or

- other people’s current approaches, concentrat-

- ing instead on the long range problem of reach-

- ing human level capability. We regard actions

- as particular kinds of events and therefore pro-

- pose subsuming reasoning about actions under

- the heading of reasoning about events.

- Most reasoning about events has concerned

- determining the eﬀects of an explicitly given

- sequence of actions by a single actor. Within

- this framework various problems have been

- studied.

- • The frame problem concerns not having

to state what does not change when an

event occurs.

- • The qualiﬁcation problem concerns not

having to state all the preconditions of an

action or other event. The point is both

to limit the set of preconditions and also

to jump to the conclusion that unstated

others will be fulﬁlled unless there is evi-

dence to the contrary. For example, wear-

ing clothes is a precondition for airline

- 9http://www-formal.stanford.edu/jmc/consciousness.html

travel, but the travel agent will not tellhis customer to be sure and wear clothes.• The ramiﬁcation problem concerns how totreat side-eﬀects of events other than theprincipal eﬀect mentioned in the event de-scription.

Each of these involves elaboration tolerance,e.g.

adding descriptions of the eﬀects ofadditional events without having to changethe descriptions of the events already de-scribed. When I wrote about applications ofcircumscription to formalizing commonsense10 [McCarthy, 1986], I hoped that a sim-ple abnormality theory would suﬃce for all ofthem. That didn’t work out when I tried it,but I still think a common nonmonotonic rea-soning mechanism will work. Tom Costello’sdraft “The Expressive Power of Circumscript-tion” 11 argues that simple abnormality theo-ries have the same expressive power as moreelaborate nonmonotonic formalisms that havebeen proposed.

Human level

intelligence requires reasoningabout strategies of action,

action pro-grams.

It also requires considering multipleactors and also concurrent events and contin-uous events. Clearly we have a long way togo.

i.e.

Some of these points are discussed in a drafton narrative12 [McCarthy, 1995].

Introspection

People have a limited ability to observe theirown mental processes. For many intellectualtasks introspection is irrelevant. However, itis at least relevant for evaluating how one isusing one’s own thinking time. Human-levelAI will require introspective ability.10http://www-formal.stanford.edu/jmc/applications.html11http://www-formal.stanford.edu/tjc/expressive.html12http://www-formal.stanford.edu/jmc/narrative.html- That robots also need introspection13

- is argued and how to do it is discussed in

- [McCarthy, 1996].

summarized as that of succeeding in the com-mon sense informatic situation.

The problems include:

- 10 Heuristics

- The largest qualitative gap between human

- performance and computer performance is in

- the area of heuristics, even though the gap is

- disguised in many applications by the millions-

- fold speed advantage of computers. The gen-

- eral purpose theorem proving programs run

- very slowly, and the special purpose programs

- are very specialized in their heuristics.

- I think the problem lies in our present in-

- ability to give programs domain and prob-

- lem dependent heuristic advice.

In my Ad-

- vice Taker paper14 [McCarthy, 1959] I adver-

- tised that the Advice Taker would express its

- heuristics declaratively. Maybe that will work,

- but neither I nor anyone else has been able to

- get a start on the problem in the ensuing al-

- most 40 years. Joseﬁna Sierra-Santibanez re-

- ports on some progress in a forthcoming arti-

- cle.

- Another possibility is to express the advice in

- a procedure modiﬁcation language, i.e. to ex-

- tend elaboration tolerance to programs. Of

- course, every kind of modularity, e.g. object

- orientation, gives some elaboration tolerance,

- but these devices haven’t been good enough.

- Ideally, a general purpose reasoning system

- would be able to accept advice permitting it

- to run at a ﬁxed ratio speed of speeds to a

- special purpose program, e.g. at 1/20 th the

- speed.

- 11 Summary

- Conclusion: Between us and human-level in-

- telligence lie many problems. They can be

common sense knowledge of the worldMany important aspects of what thisknowledge is in and how it can berepresented are still unsolved questions.This is particularly true of knowledge ofthe eﬀects of actions and other events.epistemologically adequate languagesare

languages

These

expressingwhat a person or robot can ac-the world15tually learn about

[McCarthy and Hayes, 1969].

for

elaboration tolerance What

personknows can be elaborated without startingall over.

anonmonotonic reasoning Perhaps new sys-tems are needed.

contexts as objects This subject is just be-ginning. See the references of section 7.introspection AI systems will need to exam-ine their own internal states.

action The present puzzles of formalizing ac-tion should admit a uniform solution.I doubt that a human-level intelligent programwill have structures corresponding to all theseentities and to the others that might have beenlisted. A generally intelligent logical programprobably needs only its monotonic and non-monotonic reasoning mechanisms plus mecha-nisms for entering and leaving contexts. Therest are handled by particular functions andpredicates.

- 13http://www-formal.stanford.edu/jmc/consciousness.html

- 14http://www-formal.stanford.edu/jmc/mcc59.html

15http://www-formal.stanford.edu/jmc/mcchay69.html- 12 Remarks and Acknowledgements

- 1. To what extent will all these problems

have to be faced explicitly by people

working with neural nets and connection-

ist systems? The systems I know about

are too primitive for the problems even to

arise. However, more ambitious systems

will inhabit the common sense informatic

situation. They will have to be elabora-

tion tolerant and will require some kind

of mental model of the consequences of

actions.

- 2.

- 3. I got useful suggestions from Eyal Amir,

Saˇsa Buvaˇc and Tom Costello.

- 4. Some additional relevant papers are in my

book [McCarthy, 1990] and on my Web

site16.

- 5. My understanding that I should prepare a

printable version of this invited talk came

rather late. I expect that both the spoken

version and the 1996 November Web ver-

sion will have better explanations of the

important concepts.

- 6. This work was partly supported by ARPA

(ONR) grant N00014-94-1-0775.

- 13 Conclusion

- Many will ﬁnd dismayingly large the list of

- tasks that must be accomplished in order to

- to reach human-level logical intelligence. Per-

- haps fewer but more powerful ideas would sim-

- plify the list. Others will claim that a sys-

- tem that evolves intelligence as life does will

- be more straightforward to build. Maybe, but

- the advocates of that approach have been at it

- as long as we have and still aren’t even close.

- So it’s a race.

- 16http://www-formal.stanford.edu/jmc/

It will be much more scientiﬁcally satisfying tounderstand human level artiﬁcial intelligencelogically than just achieve it by a computer-ized evolutionary process that produced an in-telligent but incomprehensible result. In fact,the logical approach would be worth pursuingeven if the intellectually lazy evolutionary ap-proach won the race.

References

[Buvaˇc, 1996] Buvaˇc, S. (1996). Quantiﬁca-In Proceedings oftional logic of context.

the Thirteenth National Conference on Ar-tiﬁcial Intelligence.

[Buvaˇc et al., 1995] Buvaˇc, S., Buvaˇc, V., andMason, I. A. (1995). Metamathematics ofcontexts. Fundamenta Informaticae, 23(3).[David Poole and Goebel, 1998] David Poole,A. M. and Goebel, R. (1998). Computa-tional Intelligence. Oxford.

[McCarthy, 1959] McCarthy, J. (1959). Pro-grams with Common Sense17.

In Mecha-nisation of Thought Processes, Proceedingsof the Symposium of the National PhysicsLaboratory, pages 77–84, London, U.K. HerMajesty’s Stationery Oﬃce. Reprinted inMcC90.

[McCarthy, 1979] McCarthy, J. (1979). As-cribing mental qualities to machines18.InRingle, M., editor, Philosophical Perspec-tives in Artiﬁcial Intelligence. HarvesterPress. Reprinted in [McCarthy, 1990].[McCarthy, 1986] McCarthy, J. (1986). Ap-plications of Circumscription to Formaliz-ing Common Sense Knowledge19. Artiﬁ-cial Intelligence, 28:89–116. Reprinted in[McCarthy, 1990].

17http://www-formal.stanford.edu/jmc/mcc59.html18http://www-formal.stanford.edu/jmc/ascribing.html19http://www-formal.stanford.edu/jmc/applications.html[Newell and Simon, 1972] Newell, A. and Si-mon, H. A. (1972). Human Problem Solving.Prentice–Hall, Englewood Cliﬀs, NJ.[Turing, 1950] Turing, A. (1950). Computingmachinery and intelligence. Mind.[Turing, 1947] Turing, A. M. (1947).Lec-ture to the london mathematical society.In The Collected Works of A. M. Tur-ing, volume Mechanical Intelligence. North-Holland. This was apparently the ﬁrst pub-lic introduction of AI, typescript in theKing’s College archive, the book is 1992.- [McCarthy, 1989] McCarthy, J. (1989). Ar-

- tiﬁcial Intelligence, Logic and Formalizing

- Common Sense20.

In Thomason, R., edi-

- tor, Philosophical Logic and Artiﬁcial Intel-

- ligence. Kl¨uver Academic.

- [McCarthy, 1990] McCarthy, J. (1990). For-

- malization of common sense, papers by John

- McCarthy edited by V. Lifschitz. Ablex.

- [McCarthy, 1993] McCarthy,

(1993).

In

Available on http://www-

- Notes on Formalizing Context21.

- IJCAI-93.

- formal.stanford.edu/jmc/.

J.

- [McCarthy, 1995] McCarthy, J. (1995). Situa-

- tion Calculus with Concurrent Events and

- Narrative22. Contents subject to change.

- Reference will remain.

- [McCarthy, 1996] McCarthy, J. (1996). Mak-

- ing Robots Conscious of

their Mental

- States23. In Muggleton, S., editor, Machine

- Intelligence 15. Oxford University Press.

- [McCarthy and Hayes, 1969] McCarthy,

J.

- and Hayes, P. J. (1969). Some Philosophical

- Problems from the Standpoint of Artiﬁcial

- Intelligence24.

In Meltzer, B. and Michie,

- D., editors, Machine Intelligence 4, pages

- 463–502. Edinburgh University Press.

- [Newell, 1981] Newell, A. (1981). The knowl-

- edge level. AI Magazine, 2(2):1–20. Origi-

- nally delivered as the Presidential Address,

- American Association for Artiﬁcial Intelli-

- gence, AAAI80, Stanford, CA, August 1980.

- [Newell, 1993] Newell, A. (1993). Reﬂections

- on the knowledge level. Artiﬁcial Intelli-

- gence, 59(1-2):31–38.

- 20http://www-formal.stanford.edu/jmc/ailogic.html

- 21http://www-formal.stanford.edu/jmc/context.html

- 22http://www-formal.stanford.edu/jmc/narrative.html

- 23http://www-formal.stanford.edu/jmc/consciousness.html

- 24http://www-formal.stanford.edu/jmc/mcchay69.html

