- Contents

- 1 PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS

OF LOGICAL AI

1.1 Philosophical Presuppositions . . . . . . . . . . . . . . . . . .

1.2 Scientiﬁc Presuppositions . . . . . . . . . . . . . . . . . . . . . 10

CONTENTS- Chapter 1

- PHILOSOPHICAL AND

- SCIENTIFIC

- PRESUPPOSITIONS OF

- LOGICAL AI

Extinguished theologians lie about the cradle of every

science as the strangled snakes beside that of Hercules.

—T. H. Huxley (Darwin’s bulldog)1

- Abstract: Many ideas from philosophy, especially from recent analytic phi-

- losophy, are usable for AI. However, some philosophical points of view make

- assumptions that have the eﬀect of excluding the possibility of AI. Likewise

- work on AI is not neutral with regard to philosophical issues. This chapter

- presents what we consider the presuppositions of logical AI and also some

- scientiﬁc presuppositions, i.e. some results of science that are relevant. We

- emphasize the relation to AI rather than philosophy itself.

- 1.1 Philosophical Presuppositions

- Q. Why bother stating philosophical presuppositions? Why not just get on

- with the AI?

1Progress in AI may extinguish some philosophies, but don’t stand on one foot.

4CHAPTER 1. PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS OF LOGICAL AIA. AI shares many concerns with philosophy—with metaphysics, episte-mology, philosophy of mind and other branches of philosophy. This is becauseAI concerns the creation of an artiﬁcial mind. However, AI has to treat thesequestions in more detail than philosophers customarily consider relevant.2In principle, an evolutionary approach need not involve philosophical pre-suppositions. However, many putative evolutionary approaches are crippledby impoverished philosophical assumptions. For example, the systems oftenonly admit patterns in appearance and can’t even represent reality behindappearance. (McCarthy ) presents a challenge to learning systems to learnreality behind appearance.

AI research not based on stated philosophical presuppositions usuallyturns out to be based on unstated philosophical presuppositions. These areoften so wrong as to interfere with developing intelligent systems.

That it should be possible to make machines as intelligent as humansinvolves some philosophical premises, although the possibility is probablyaccepted by a majority of philosophers. The way we propose to build in-telligent machines, i.e. via logical AI, makes more presuppositions, some ofwhich may be new.

This chapter concentrates on stating the presuppositions and their rela-tions to AI without much philosophical argument. A later chapter presentsarguments and discusses other opinions.

objective world The world exists independently of humans. The facts ofmathematics and physical science are independent of there being peopleto know them. Intelligent Martians and robots will need to know thesame facts.

A robot also needs to believe that the world exists independently ofitself. Science tells us that humans evolved in a world which formerlydid not contain humans. Given this, it is odd to regard the world as ahuman construct. It is even more odd to program a robot to regard theworld as its own construct. What the robot believes about the worldin general doesn’t arise for the limited robots of today, because thelanguages they are programmed to use can’t express assertions aboutthe world in general. This limits what they can learn or can be told—and hence what we can get them to do for us.

2Compare the treatment of counterfactual conditional sentences in CostelloMcC99 withthat in (Lewis 1973).

- 1.1. PHILOSOPHICAL PRESUPPOSITIONS

- correspondence theory of truth and reference A logical robot repre-

sents what it believes about the world by logical sentences. Some of

these beliefs we build in; others come from its observations and still

others by induction from its experience. Within the sentences it uses

terms to refer to objects in the world.

In every case, we try to design it so that what it will believe about

the world is as accurate as possible, though not usually as detailed as

possible. Debugging and improving the robot includes detecting false

beliefs about the world and changing the way it acquires information to

maximize the correspondence between what it believes and the facts of

world. The terms the robot uses to refer to entities need to correspond

to the entities so that the sentences will express facts about these en-

tities. We have in mind both material objects and other entities, e.g.

plans.

Already this involves a philosophical presupposition—that which is

called the correspondence theory of truth. AI also needs a correspon-

dence theory of reference , i.e. that a mental structure can refer to an

external object and can be judged by the accuracy of the reference.

As with science, a robot’s theories are tested experimentally, but the

concepts robots use are often not deﬁned in terms of experiments. Their

properties are partially axiomatized, and some axioms relate terms to

observations.

The important consequence of the correspondence theory is that when

we design robots, we need to keep in mind the relation between appear-

ance, the information coming through the robot’s sensors, and reality.

Only in certain simple cases, e.g. the position in a chess game, does the

robot have suﬃcient access to reality for this distinction to be ignored.

Some robots react directly to their inputs without memory or infer-

ences. It is our scientiﬁc (i.e. not philosophical) contention that these

are inadequate for human-level intelligence, because the world contains

too many important entities that cannot be observed directly.

A robot that reasons about the acquisition of information must itself be

aware of these relations. In order that a robot should not always believe

what it sees with its own eyes, it must distinguish between appearance

and reality. (McCarthy ) presents a challenge problem requiring the

discovery of reality behind appearance.

6CHAPTER 1. PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS OF LOGICAL AIscience Science is substantially correct in what it tells us about the world,and scientiﬁc activity is the best way to obtain more knowledge. 20thcentury corrections to scientiﬁc knowledge mostly left the old scien-tiﬁc theories as good approximations to reality. Much “postmodernphilosophy” is in opposition to this.

mind and brain The human mind is an activity of the human brain. Thisis a scientiﬁc proposition, supported by all the evidence science hasdiscovered so far. However, the dualist intuition of separation betweenmind and body is related to the sometimes weak connections betweenthought and action. Dualism has some use as a psychological abstrac-tion.

common sense Common sense ways of perceiving the world and commonopinion are also substantially correct. When general common senseerrs, it can often be corrected by science, and the results of the correc-tion may become part of common sense if they are not too mathemat-ical. Thus common sense has absorbed the notion of inertia. However,its mathematical generalization, the law of conservation of momentumhas made its way into the common sense of only a small fraction ofpeople—even among the people who have taken courses in physics.From Socrates on philosophers have found many inadequacies in com-mon sense usage, e.g. common sense notions of the meanings of words.The corrections are often elaborations, making distinctions blurred incommon sense usage. Unfortunately, there is no end to philosophicalelaboration, and the theories become very complex. However, someof the elaborations seem essential to avoid confusion in some circum-stances. Here’s a candidate for the way out of the maze.

Robots will need both the simplest common sense usages and to beable to tolerate elaborations when required. For this we have pro-posed two notions—contexts as formal objects (McCarthy 1993) and(McCarthy and Buvaˇc 1997) and elaboration tolerance (McCarthy 1999)33Hilary Putnam (Putnam 1975) discusses two notions concerning meaning proposed byprevious philosophers which he ﬁnds inadequate. These are

(I) That knowing the meaning of a term is just a matter of being in a

certain “psychological state” (in the sense of “psychological state” in which

states of memory and psychological dispositions are “psychological states”; no

- 1.1. PHILOSOPHICAL PRESUPPOSITIONS

- science embedded in common sense Science is embedded in common

sense. Galileo taught us that the distance s that a dropped body falls

in time t is given by the formula

s =

gt2.

To use this information, the English (or its logical equivalent) is just

as essential as the formula, and common sense knowledge of the world

is required to make the measurements required to use or verify the

formula.

- possibility of AI According to some philosophers’ views, artiﬁcial intelli-

gence is either a contradiction in terms (Searle 1984) or intrinsically

impossible (Dreyfus 1992) or (Penrose 1994). The methodological ba-

sis of these arguments has to be wrong and not just the arguments

themselves. We hope to deal with this elsewhere.

- mental qualities treated individually AI has to treat mind in terms of

components rather than regarding mind as a unit that necessarily has

all the mental features that occur in humans. Thus we design some

very simple systems in terms of the beliefs we want them to have and

debug them by identifying erroneous beliefs. (McCarthy 1979) treats

this. Ascribing a few beliefs to thermostats has led to controversy.

- third person point of view We ask “How does it (or he) know?”, “What

does it perceive?” rather than how do I know and what do I perceive.

one thought that knowing the meaning of a word was a continuous state of

consciousness, of course.)

(II) That the meaning of a term (in the sense of “intension”) determines

its extension (in the sense that sameness of intension entails sameness of ex-

tension).

Suppose Putnam is right in his criticism of the general correctness of (I) and (II). His

- own ideas are more elaborate.

It may be convenient for a robot to work mostly in contexts within a larger context

- Cphil1 in which (I) and (II) (or something even simpler) hold. However, the same robot,

- if it is to have human level intelligence, must be able to transcend Cphil1 when it has to

- work in contexts to which Putnam’s criticisms of the assumptions of Cphil1 apply.

It is interesting, but perhaps not necessary for AI at ﬁrst, to characterize those contexts

- in which (I) and (II) are correct.

8CHAPTER 1. PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS OF LOGICAL AIThis presupposes the correspondence theory of truth. It applies to howwe look at robots, but also to how we want robots to reason about theknowledge of people and other robots. Some philosophers, e.g. JohnSearle, insist with Descartes on a ﬁrst person point of view.

rich ontology Our theories involve many kinds of entity—material objects,situations, properties as objects, contexts, propositions, indivdual con-cepts, wishes, intentions. When one kind A of entity might be deﬁnedin terms of others, we will often prefer to treat A separately, becausewe may later want to change our ideas of its relation to other entities.We often consider several related concepts, where others have tried toget by with one. Suppose a man sees a dog. Is seeing a relation betweenthe man and the dog or a relation between the man and an appearanceof a dog? Some purport to refute calling seeing a relation between theman and the dog by pointing out that the man may actually see ahologram or picture of the dog. AI needs the relation between the manand the appearance of a dog, the relation between the man and the dogand also the relation between dogs and appearances of them. None ismost fundamental.

natural kinds The entities the robot must refer to often are rich with prop-erties the robot cannot know all about. The best example is a naturalkind like a lemon. A child buying a lemon at a store knows enoughproperties of the lemons that occur in the stores he frequents to dis-tinguish lemons from other fruits in the store. Experts know moreproperties of lemons, but no-one knows all of them. AI systems alsohave to distinguish between sets of properties that suﬃce to recognizean object in particular situations and the natural kinds of some objects.To a child, all kinds are natural kinds, i.e. kinds about which the childis ready to learn more. The idea of a concept having an if-and-only-ifdeﬁnition comes later—perhaps at ages 10–13. Taking that further,natural kind seems to be a context relative notion. Thus some partof income tax law is a natural kind to me, whereas it might have anif-and-only-if deﬁnition to an expert.

Curiously enough, many of the notions studied in philosophy are notnatural kinds, e.g. proposition, meaning, necessity. When they areregarded as natural kinds, then fruitless arguments about what they- 1.1. PHILOSOPHICAL PRESUPPOSITIONS

really are take place. AI needs these concepts but must be able to work

with limited notions of them.

- approximate entities Many of the philosophical arguments purporting to

show that naive common sense is hopelessly mistaken are wrong. These

arguments often stem from trying to force intrinsically approximate

concepts into the form of if-and-only-if deﬁnitions.

Our emphasis on the ﬁrst class character of approximate entities may

be new. It means that we can quantify over approximate entities and

also express how an entity is approximate. An article on approximate

theories and approximate entities is forthcoming.

- compatibility of determinism and free will A logical robot needs to con-

sider its choices and the consequences of them. Therefore, it must

regard itself as having free will even though it is a deterministic device.

We discuss our choices and those of robots by considering non-determinist

approximations to a determinist world—or at least a world more deter-

minist than is needed in the approximation. The philosophical name

for this view is compatibilism. I think compatibilism is a requisite for

AI research reaching human-level intelligence.

In practice, regarding an observed system as having choices is necessary

when ever a human or robot knows more about the relation of the

system to the environment than about what goes on within the system.

This is discussed in (McCarthy 1996).

- mind-brain distinctions I’m not sure whether this point is philosophical

or scientiﬁc. The mind corresponds to software, perhaps with an in-

ternal distinction between program and knowledge. Software won’t

do anything without hardware, but the hardware can be quite simple.

Some hardware conﬁgurations can run many diﬀerent programs con-

currently, i.e. there can be many minds in the same computer body.

Software can also interpret other software.

Confusion about this is the basis of the Searle Chinese room fallacy

(Searle 1984). The man in the hypothetical Chinese room is inter-

preting the software of a Chinese personality. Interpreting a program

does not require having the knowledge possessed by that program. This

10CHAPTER 1. PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS OF LOGICAL AIwould be obvious if people could interpret other personalities at a prac-tical speed, but Chinese room software interpreted by an unaided hu-man might run at 10−9 the speed of an actual Chinese.

If one settles for a Chinese conversation on the level of Eliza (Weizenbaum 1965),then, according to Weizenbaum (1999 personal communication), theprogram can be hand simulated with reasonable performance.

1.2 Scientiﬁc Presuppositions

Some of the premises of logical AI are scientiﬁc in the sense that they aresubject to scientiﬁc veriﬁcation. This may also be true of some of the premiseslisted above as philosophical.

innate knowledge The human brain has important innate knowledge, e.g.that the world includes three dimensional objects that usually persisteven when not observed. This was learned by evolution. Acquiringsuch knowledge by learning from sense data will be quite hard. It isbetter to build it into AI systems.

Diﬀerent animals have diﬀerent innate knowledge. Dogs know aboutpermanent objects and will look for them when they are hidden. Verylikely, cockroaches don’t know about objects.

Identifying human innate knowledge has been the subject of recent psy-chological research. See (Spelke 1994) and the discussion in (Pinker 1997)and the references Pinker gives. In particular, babies and dogs knowinnately that there are permanent objects and look for them when theygo out of sight. We’d better build that in.

middle out Humans deal with middle-sized objects and develop our knowl-edge up and down from the middle. Formal theories of the world mustalso start from the middle where our experience informs us. Eﬀortsto start from the most basic concepts, e.g. to make a basic ontologyare unlikely to succeed as well as starting in the middle. The ontologymust be compatible with the idea that the basic entities in the ontologyare not the basic entities in the world. More basic entities are knownless well than the middle entities.

- 1.2. SCIENTIFIC PRESUPPOSITIONS

- universality of intelligence Achieving goals in the world requires that an

agent with limited knowledge, computational ability and ability to ob-

serve use certain methods. This is independent of whether the agent

is human, Martian or machine. For example, playing chess-like games

eﬀectively requires something like alpha-beta pruning. Perhaps this

should be regarded as a scientiﬁc opinion (or bet) rather than as philo-

sophical.

- universal expressiveness of logic This is a proposition analogous to the

Turing thesis that Turing machines are computationally universal—

anything that can be computed by any machine can be computed by

a Turing machine. The expressiveness thesis is that anything that can

be expressed, can be expressed in ﬁrst order logic. Some elaboration of

the idea is required before it will be as clear as the Turing thesis.4

- suﬃcient complexity yields essentially unique interpretations A robot

that interacts with the world in a suﬃciently complex way gives rise to

an essentially unique interpretation of the part of the world with which

it interacts. This is an empirical, scientiﬁc proposition, but many peo-

ple, especially philosophers (see (Quine 1969), (Putnam 1975), (Dennett 1971),(Dennett 1998)), take its negation for granted. There are often many

interpretations in the world of short descriptions, but long descriptions

almost always admit at most one.

The most straightforward example is that a simple substitution cipher

cryptogram of an English sentence usually has multiple interpretations

if the text is less than 21 letters and usually has a unique interpretation

if the text is longer than 21 letters. Why 21? It’s a measure of the

redundancy of English. The redundancy of a person’s or a robot’s

interaction with the world is just as real—though clearly much harder

to quantify.

We expect these philosophical and scientiﬁc presuppositions to become

- more important as AI begins to tackle human level intelligence.

4First order logic isn’t the best way of expressing all that can be expressed any more

- than Turing machines are the best way of expressing computations. However, with set

- theory, what can be expressed in stronger systems can apparently also be expressed in

- ﬁrst order logic.

12CHAPTER 1. PHILOSOPHICAL AND SCIENTIFIC PRESUPPOSITIONS OF LOGICAL AI- Bibliography

Dennett, D. 1998. Brainchildren: Essays on Designing Minds. MIT Press.

Dennett, D. C. 1971.

68(4):87–106.

Intentional systems. The Journal of Philosophy

Dreyfus, H. 1992. What Computers still can’t Do. M.I.T. Press.

Lewis, D. 1973. Counterfactuals. Harvard University Press.

McCarthy, J. n.d. appearance and reality5. web only for now, and

perhaps for the future. not publishable on paper, because it contains an

essential imbedded applet.

McCarthy, J. 1979. Ascribing mental qualities to machines6. In M. Ringle

(Ed.), Philosophical Perspectives in Artiﬁcial Intelligence. Harvester Press.

Reprinted in (McCarthy 1990).

McCarthy, J. 1990. Formalizing Common Sense: Papers by John Mc-

Carthy. 355 Chestnut Street, Norwood, NJ 07648: Ablex Publishing Cor-

poration.

McCarthy, J. 1993. Notes on Formalizing Context7. In IJCAI-93.

McCarthy, J. 1996. Making Robots Conscious of their Mental States8. In

S. Muggleton (Ed.), Machine Intelligence 15. Oxford University Press. to

appear in 1999.

McCarthy, J. 1999. Elaboration tolerance9. to appear.

5http://www-formal.stanford.edu/jmc/appearance.html

6http://www-formal.stanford.edu/jmc/ascribing.html

7http://www-formal.stanford.edu/jmc/context.html

8http://www-formal.stanford.edu/jmc/consciousness.html

9http://www-formal.stanford.edu/jmc/elaboration.html

BIBLIOGRAPHYMcCarthy, J., and S. Buvaˇc. 1997. Formalizing context (expanded notes).In A. Aliseda, R. v. Glabbeek, and D. Westerst˚ahl (Eds.), ComputingNatural Language. Center for the Study of Language and Information,Stanford University.

Penrose, R. 1994. Shadows of the Mind: A Search for the Missing Scienceof Consciousness. Oxford: Oxford University Press.

Pinker, S. 1997. How the Mind Works. Norton.

Putnam, H. 1975. The meaning of “meaning”. In K. Gunderson (Ed.),Language, Mind and Knowledge, Vol. VII of Minnesota Studies in thePhilosophy of Science, 131–193. University of Minnesota Press.

Quine, W. V. O. 1969. Propositional objects. In Ontological Relativity andother Essays. Columbia University Press, New York.

Searle, J. R. 1984. Minds, Brains, and Science. Cambridge, Mass.: Har-vard University Press.

Spelke, E. 1994. Initial knowlege: six suggestions. Cognition 50:431–445.Weizenbaum, J. 1965. ELIZA—a computer program for the study of natu-ral language communication between man and machine. Communicationsof the Association for Computing Machinery 9(1):36–45.

/@steam.stanford.edu:/u/jmc/w99/logicalai.tex: begun Wed Jan 6 11:17:02 1999, latexed December 4, 1999 at 6:43 p.m.