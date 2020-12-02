Combining Narratives

John McCarthy

Computer Science Department

Stanford University

Stanford, CA 94305

Tom Costello

Computer Science Department

Stanford University

Stanford, CA 94305

Abstract

A theory is elaboration tolerant to the extent that

new information can be incorporated with only

simple changes. The simplest change is con-

joining new information, and only conjunctive

changes are considered in this paper.

In gen-

eral adding information to a theory should of-

ten change, rather than just enlarge, its conse-

quences, and this requires that some of the rea-

soning be non-monotonic.

Our theories are narratives—accounts of sets of

events, not necessarily given as sequences. A

narrative is elaboration tolerant to the extent that

new events, or more detail about existing events,

can be added by just adding more sentences.

We propose a new version of the situation calcu-

lus which allows information to be added easily.

In particular, events concurrent with already de-

scribed events can be introduced without modi-

fying the existing descriptions, and more detail

of events can be added. A major beneﬁt is that

if two narratives do not interact, then they can be

consistently conjoined.

- 1 OBJECTIVES OF SITUATION

CALCULUS

- The logical approach to AI ([McC59] and [McC89]) is to

- make a computer program that represents what it knows

- about the world in general, what it knows about the situ-

- ation it is in, and also its goals, all as sentences in some

- mathematical logical language. The program then infers

- logically what action is appropriate for achieving its goal

- and does it. Since 1980 it has been widely known that non-

- monotonic inference must be included. The actions our

- program can perform include some that generate sentences

- by other means than logical inference, e.g. by observation

of the world or by the use of special purpose non-logicalproblem solvers.

Simpler behaviors, e.g. actions controlled by servomecha-nisms or reﬂexes can be integrated with logic. The actionsdecided on by logic can include adjusting the parametersof ongoing reﬂexive actions. Thus a person can decide towalk faster when he reasons that otherwise he will be late,but this does not require that reason control each step of thewalking.1

Situation calculus is an aspect of the logic approach to AI.A situation is a snapshot of the world at some instant. Sit-uations are rich2 objects in that it is not possible to com-pletely describe a situation, only to say some things aboutit. From facts about situations and general laws about theeffects of actions and other events, it is possible to infersomething about future situations. Situation calculus wasﬁrst discussed in [McC63], but [MH69] was the ﬁrst widelyread paper about it.

In this formalization of action in situation calculus, thereare at least three kinds of problem—narrative, planningand prediction. Of these, narrative seems to be the simplestfor humans. A narrative is an account of what happened.We treat it by giving some situations and some events andsome facts about them and their relations. Situations in anarrative are partially ordered in time. The real situationsare totally ordered3, but the narrative may not include fullinformation about this ordering. Thus the temporal rela-tions between situations need only be described to the ex-tent needed to describe their interactions. Situations oc-curring entirely in different places give the most obviousexamples, but even actions by the same person in the sameplace may not interact as far as the inferences we draw. If1Thus we protect our ﬂank from the disciples of Rod Brooks.2Though rich, situations are still approximate, partial objects.The idea will be developed elsewhere.

3Hypothetical situations need not be totally ordered; the situ-ation where Oswald missed Kennedy is neither in the past nor thefuture.

- we state that the traveler on certain ﬂight reads a book and

- also drinks a Coca-cola, we humans don’t need to know

- any temporal relations between the two events unless they

- interact.

- In situation calculus as it was originally envisaged (and has

- been used,) events (mainly actions) in a situation produce

- next situations, e.g. s(cid:48) = Result(e, s). The original the-

- ory did not envisage more than one event occurring in a

- situation, and it did not envisage intermediate situations in

- which events occur. However, rarely did people write ax-

- ioms that forbade4 these possibilities; it’s just that no-one

- took advantage of them.

- Our present formalism doesn’t really change the basic for-

- malism of the situation calculus much; it just takes advan-

- tage of the fact that the original formalism allows treat-

- ing concurrent events even though concurrent events were

- not originally supposed to be treatable in that formalism.

- Gelfond, Lifschitz and Rabinov [GLR91] treat concurrent

- events in a different way from what we propose here.

- In a narrative,

it is not necessary that what is said to

- hold in a situation be a logical consequence (even non-

- monotonically) of what was said to hold about a previous

- situation and known common sense facts about the effects

- of events. In the ﬁrst place, in stories new facts about sit-

- uations are often added, e.g. “When Benjamin Franklin

- arrived in London it was raining”. In the second place, we

- can have an event like tossing a coin in which neither out-

- come has even a non-monotonic preference.5

- In interpreting the following formalizations, we regard sit-

- uations as rich objects and events as poor. In fact, we are

- inclined to take a deterministic view within any single nar-

- rative. In principle, every event that occurs in a situation

- and every fact about following situations is an inevitable

- consequence of the facts about the situation. Thus it is a

- fact about a situation that a coin is tossed and that it comes

- up tails. However, such facts are only occasionally conse-

- quences of the facts about the situation that we are aware

- of in the narrative.

- Perhaps narrative seems easy, since it is not yet clear what

- facts must be included in a narrative and what assertions

- should be inferable from a narrative. We have however a

- basic model that handles some of the more basic features.

2 ELABORATION TOLERANT

REASONING

A formalization of a phenomenon is elaboration tolerantto the extent that it permits elaborations of the descriptionwithout requiring completely redoing the basis of the for-malization. In particular, it would be unfortunate to haveto change the predicate symbols. Ideally the elaboration isachieved by adding sentences, rather than by changing sen-tences. Often when we add sentences we need to use someform of non-monotonic reasoning. This is because we of-ten want to add information that we would previously haveassumed was false. Unless we use non-monotonicity wewould get inconsistency. In this paper we concentrate onthe easier case when there is no need for non-monotonicity.Natural language descriptions of phenomena seem to bemore elaboration tolerant than any existing formalizations.Here are the two major kinds of elaboration tolerance thatwe examine in this paper.

2.1 NON-INTERACTING EVENTS

Allowing the addition of a description of a second phe-nomenon that doesn’t interact with the ﬁrst. In this casethe conclusions that can be drawn about the combined nar-rative are just the conjunction of the conclusions about thecomponent narratives. To infer the obvious consequencesof events we need to assume that some other events do notoccur. In this paper, a major novelty is that we do not as-sume that no other events occur. We only state that thereare no events that would cause an event in our narrative tofail. Thus a narrative about stacking blocks will state thatthe only block moving actions6 are those mentioned. Ablock stacking narrative will not say that no traveling eventsoccur. Nor will a narrative about traveling makes claimsabout what block stacking events happen. This allows non-interacting narratives to be consistently conjoined.Previous proposals could not conjoin two narratives, asthey either assumed that the events that happened werepicked out by the result function, or they assumed that theonly events that occurred were those mentioned.2.1.1 DETAIL OF EVENTS

- 4Reiter [Rei93] did write such axioms.

- 5Nevertheless, some narratives are anomalous. If we record

- that Junior ﬂew to Moscow, and, in the next situation mentioned,

- assert that he is in Peking, a reader will feel that something has

- been left out.

- We want to introduce a concept of a proper narrative, this is a

- narrative without anomalies. The ﬂuents holding in a new situa-

- tion should be reasonable outcomes of the events that have been

- reported, except for those ﬂuents which are newly asserted, e.g.

- that it was raining in London when Franklin arrived.

We can add details of an event. On the airplane from Glas-gow to London, Junior read a book and drank a Coca-Cola.If we make the assumption that other relevant events do6More precisely, no other actions that would move the blocksmentioned in the narrative occur. Other blocks might be stackedin Baghdad, if our narrative is about New York. Perhaps a theoryof context, that would interpret a statement about all blocks in ournarrative, as a statement about all the blocks in New York could beused here.

- not happen, we can elaborate by adding another event, so

- long as it is compatible with what we have said. However

- the notion of relevant must be formalized very carefully, as

- is apparent when we elaborate a particular event as a se-

- quence of smaller events. “How did he buy the Kleenex?

- He took it off the shelf, put it on the counter, paid the clerk

- and took it home.” A narrative that just mentions buying

- the Kleenex should not exclude this particular elaboration.

- Moreover, if we elaborate in this way, we don’t want to

- exclude subsequent elaboration of component events, e.g.

- elaborating paying the clerk into offering a bill, taking the

- change, etc. Our formalism allows details of an event to be

- added by conjoining extra sentences.

- 3 MODIFYING THE SITUATION

CALCULUS

- Formalisms such as the situation calculus of McCarthy and

- Hayes [MH69], and the event calculus of Kowalski [KS97]

- have been used to represent and reason about a changing

- world c.f [Sha97]. Neither of these formalisms is exactly

- what is needed to represent the kind of narratives we wish

- to consider.

- The situation calculus in its most limited version does not

- allow us to represent what events occur explicitly—rather

- every sequence of events is assumed to occur. We can spec-

- ify that a particular sequence of events occurs by introduc-

- ing a predicate, actual true of just the sequence of situa-

- tions that occur7. This is not ideal, as it forces us to decide

- what events happened earlier, before we name the events

- that happen later.

- For this reason we use a modiﬁed situation calculus, adding

- a new predicate Occurs(e, s), that states what events oc-

- cur. Thus, rather than the function Result(e, s) serving two

- purposes, stating that e occurred at s, and designating the

- resulting situation, we split these two functions. We keep

- Result(e, s), but it now only denotes the result of doing

- e in s when e Occurs at s. If e does not occur, then the

- value of this function is an arbitrary situation8. This adds

- an event calculus style of presentation to the underlying

- situation calculus formalism. In particular, it allows us to

- specify a sequence of events, without making any claims as

- to what other events may have happened in the meantime.

- 3.1 OUR ONTOLOGY OF SITUATIONS

- Reiter has suggested that the situations in the situation cal-

- culus be deﬁned axiomatically. He suggests the following

- 7Pinto and Reiter [PR95] actually do this.

- 8We could choose instead to make Result a partial function,

- but this introduces the difﬁculties of partial functions.

four axioms9

S0 : ∀s.¬s < S0

∀a, s, s(cid:48).s < Result(a, s (cid:48)) ≡ (s = s (cid:48) ∨ s < s (cid:48))P : ∀a, a(cid:48), s, s(cid:48).Result(a, s) = Result(a (cid:48), s (cid:48)) →Ind : ∀φ.(φ(S0) ∧ (∀a.φ(s) → φ(Result(a, s)))) →a = a(cid:48) ∧ s = s(cid:48)

∀s.φ(s).

which determine equality of situations, relative to equalityof events or actions. These axioms are categorical, that isrelative to an interpretation of equality of actions, there is aunique model of situations.

Rather than use these axioms, which state that no other sit-uations exist between s and Result(a, s), we choose to saythat situations can be ordered by a < predicate, which is astrict partial order, which we axiomatize as follows.∀a, s.s < Result(a, s),

∀s, s(cid:48), s(cid:48)(cid:48).s < s(cid:48) → ¬(s(cid:48) < s),

∀s, s, s(cid:48), s(cid:48)(cid:48).s < s(cid:48) ∧ s(cid:48) < s(cid:48)(cid:48) → s < s(cid:48)(cid:48)(1)The predicate < is similar to the f uture(s, s(cid:48)) predicate,introduced by [MH69], which is true when s(cid:48) is in the fu-ture of s. We ﬁnd it useful to write this in inﬁx notation,and to use s ≤ s(cid:48) as the non-strict version. It also is usefulto write s ≤ s(cid:48) ≤ s(cid:48)(cid:48) for s ≤ s(cid:48) ∧ s(cid:48) ≤ s(cid:48)(cid:48).

4 SPECIFYING THE EFFECTS OFEVENTS

In the situation calculus it is usual to specify the effects ofactions by writing effect axioms, like10,

∀s.Holds(Loaded , s) → Holds(Dead , Result(Shoot , s)).If we move to a formalism that allows other events to occurbetween s and Result(e, s), then this way of specifyingchange needs to be adjusted. It is possible that somethingmight occur in the time between s and Result(Shoot , s)that causes the event to have a different result. For thisreason it seems natural to allow the preconditions, thosethings that hold on the left hand side, to mention propertiesof all times between s and Result(Shoot , s).In previous versions of the situation calculus the precondi-tions for an event were always modeled as a set of ﬂuents,namely those ﬂuents that had to hold at s, for the event tohave an effect at Result(a, s). If we allow other things to9Reiter’s notation differ from ours, he uses do(a, s), while weuse Result(a, s). We use ≤ s(cid:48) as a shorthand for s < s(cid:48) ∨ s = s(cid:48).Reiter writes < as (cid:60).

10As is customary in Logical A.I. we write Holds(Dead , s)without saying who is dead. We can suppose the events occur ina context and lifting rules exist to make this Dead(V ictim) inan outer context. The outer context may contain further precondi-tions, like that shooter is present.

- happen during an event, we cannot just specify the precon-

- ditions that must hold at the beginning of the event.

- Consider a plane journey from Glasgow to London. It is

- necessary that the plane be in working order for the entire

- ﬂight. It is also necessary to be in Glasgow at the beginning

- of the ﬂight, but clearly, there is no need for this precondi-

- tion to persist for the entire ﬂight. It is necessary to have

- a ticket, until the airline steward takes it from you. This is

- an example of a precondition, “having a ticket” that must

- hold neither just at the moment the event starts, nor for the

- entire duration.

- Consider another example from the Yale Shooting Prob-

- lem. In order to successfully shoot a person, the gun must

- be loaded when the trigger is pulled, but the target must re-

- main in the cross-hairs until the bullet hits. We represent

- the fact the target is in the cross-hairs by aimed. Thus we

- write:

∀s.Occurs(Shoot , s) ∧ Holds(Loaded , s)∧

(cid:18) ∀s(cid:48)(cid:48).s ≤ s(cid:48)(cid:48) ∧ s(cid:48)(cid:48) < Result(Shoot , s) →

Holds(Aimed , s (cid:48)(cid:48))

Holds(Dead , Result(Shoot , s)).

(cid:19) →

sets of ﬂuents. The ﬁrst set need only hold at the start of anevent, while the others must persist for the entire event.It might seem that this does not model preconditions thatneed hold for only part of the duration of the event. How-ever, we can model these by using other deﬁned ﬂuents.Thus we can write that “It is necessary to have a ticket, un-til the airline steward takes it from you.” using a new ﬂuentF1 deﬁned by

∀s.Holds(F1 , s) ≡ Holds(Has(Ticket), s)∨Holds(Takenby(Steward , Ticket), s).This ﬂuent should hold for the entire12 duration of theﬂight. HoldsD(f , s, e) is a shorthand for ∀s(cid:48).s ≤ s(cid:48) <Result(e, s) → Holds(f , s (cid:48)).

We can write “It

the plane be inworking order for the entire ﬂight” using the ﬂuentW orkingOrder,

is necessary that

∀s, l, l(cid:48), t.HoldsD(WorkingOrder , s, Fly(l , l (cid:48), t))∧HoldsD(F1 , s, Fly(l , l (cid:48), t))) →

Holds(at(l (cid:48)), Result(Fly(l , l (cid:48), t), s))We can add “ It is necessary to be in Glasgow13 at the be-ginning of the ﬂight”

- A possible objection to this example is that if the target

- arrives in the cross-hairs at any time before impact and re-

- mains in the path of the bullet, then they will be killed. In

- this case we write,

∀s, l, l(cid:48), t.Holds(at(l ), s)∧

HoldsD(WorkingOrder , s, Fly(l , l (cid:48), t))∧HoldsD(F1 , s, Fly(l , l (cid:48), t))) →

Holds(at(l (cid:48)), Result(Fly(l , l (cid:48), t), s))∀s.Occurs(Shoot , s) ∧ Holds(Loaded , s)∧

(cid:18)∃s1.s1 < Result(Shoot , s) ∧ ∀s (cid:48)(cid:48).s1 ≤ s (cid:48)(cid:48)∧

s(cid:48)(cid:48) < Result(Shoot , s) → Holds(Aimed , s (cid:48)(cid:48))(cid:19) →

Holds(Dead , Result(Shoot , s)).

- However, both these examples show the need for precondi-

- tions to be richer than a statement of what properties hold

- just when the event occurs. What possible properties can

- occur as preconditions is important because we wish to

- know what kinds of axioms can occur as effect axioms.

- Before we consider how to represent preconditions, we re-

- call how we represented all possible preconditions earlier.

- If we wish to introduce a predicate that can parameterize

- all effect axioms in the old-fashioned situation calculus can

- write11,

∀a, f, g.Changes(a, f, g)

¬(Holds(f , s) ≡ Holds(f , Result(a, s))

def

= ∀s.Holds(g, s) →

Now that our preconditions are represented as two sets14,rather than one, we redeﬁne Changes as follows:∀e, f, g1, g2.Changes(e, f, g1, g2)

∀s.(∀f1.g2(f1) → Holds(f1 , s))∧

(∀f1.g1(f1) → HoldsD(f1 , s, e)) →

¬(Holds(f , s) ≡ Holds(f , Result(e, s)))def

=

We ﬁnd it useful to introduce a predicate Succeeds(e, f, s)deﬁned as,

∃g1, g2.Changes(e, f, g1, g2)∧

Holds(g2 , s) ∧ HoldsD(g1 , s, e).

(2)Frame Axioms

We usually would write a frame axiom for a ﬂuent, sayOn(A, T op(B)), block A is on the top of block B, and anaction, in this case Shoot as,

- following [Cos97]. g is a predicate on ﬂuents that encodes

- the preconditions. Parameterizing all effect axioms allows

- us to minimize effect axioms easily.

- However, as we want to have preconditions that can extend

- over the duration of the event, we need more than one set

- of ﬂuents. For this reason we allow as preconditions, two

- 11We will slightly abuse notation and write Holds(g, s) for

- (∀f (cid:48).g(f (cid:48)) → Holds(f (cid:48), s)), when g is a predicate on ﬂuents.

∀s.Holds(On(A, Top(B )), s) ≡

Holds(On(A, Top(B )), Result(Shoot , s)).12The entire duration is taken to be up to, but not including theendpoint. It is sometimes natural that the endpoint should not beneeded as a precondition.

13We write the general formula with a variable l for Glasgow.14Here we assume that we have a ﬂuent function ¬, such that∀s, f.Holds(f , s) ≡ ¬Holds(¬f , s). In the absence of the ﬂuentfunction ¬, we would need four sets, two for positive ﬂuents andtwo for negative ﬂuents.

- However, since Result no longer encodes what events oc-

- curred, we need to say something like, “if no event that

- could change the ﬂuent On(A, T op(B)) occurred in the

- interval between s and s(cid:48) and On(A, T op(B)) held at s

- then On(A, T op(B)) will hold at s(cid:48).” It is notable that this

- needs the notion of Changes that we introduced above.

- Thus we write,

that the action of moving A to the top of C occurred at thesituation S0.

∀a, l.Holds(On(a, l ), S0 ) ≡

(a = A ∧ l = T op(B)) ∨ (a (cid:54)= A ∧ l = T able),Occurs(Move(A, Top(C )), S0 ).

We can now write our frame axiom, which in this case is,Frame Axiom :

∀s, s(cid:48), f.s ≤ s(cid:48)∧





∀s(cid:48)(cid:48), a.s < Result(a, s (cid:48)(cid:48)) ≤ s (cid:48) →

¬(Occurs(a, s (cid:48)(cid:48))∧

Succeeds(a, f, s(cid:48)(cid:48)))

(Holds(f , s) ≡ Holds(f , s (cid:48)))



 →

(3)

∀s, s(cid:48), a, c.s ≤ s(cid:48)∧





∀s(cid:48)(cid:48), a(cid:48), e.e = Move(a, Top(a (cid:48)))∧

s < Result(e, s (cid:48)(cid:48)) ≤ s (cid:48) ∧ a (cid:54)= c →

Succeeds(e, On(a, T op(a(cid:48))), s(cid:48)(cid:48))(cid:19)Holds(On(a, Top(c)), s (cid:48)) (cid:19)¬(cid:18) Occurs(e, s (cid:48)(cid:48))∧

→(cid:18) Holds(On(a, Top(c)), s) ≡- This will generate all of our frame axioms if we mini-

- mize Changes, varying Succeeds and Holds, and allow-

- ing the domain to vary as in [Cos98b, Cos98a]. We do

- not consider using non-monotonic reasoning to minimize

- Changes here, as we wish to stress other issues. Thus we

- explicitly axiomatize the result of the minimization, much

- in the same way as Reiter[Rei91] uses an explanation clo-

- sure axiom and an explicit statement of what events can

- change what ﬂuents.

This states that a block a is on a block c in a situation sif and only if a is on c in s(cid:48), so long as there is no evente, of moving moving a to T op(a(cid:48)), which occurs, and issuccessful.

Some writers like to think that if an event that might changea ﬂuent f occurs, but fails, then the ﬂuent f should be un-determined. We can weaken our frame axiom, so that evenif the event of moving a block a(cid:48) to a fails, then our blocka might not be clear. We write this as,

- 4.1 WHAT EVENTS OCCUR?

- Before we consider combining narratives, we address

- a problem that arises in our new formalism that was not

- present in the earlier versions of the situation calculus.

- Even frame axioms like this are not enough to allow us to

- carry out the simple reasoning we could carry out in pre-

- vious versions of the situation calculus. We also need to

- know that certain events do not occur.

- Consider the following example of moving a block. We

- have the action that moves a block, but to move a block

- successfully it must be clear. For instance if someone else

- puts another block on top of the block we are moving to,

- then our action will fail.

- Thus our only effect axiom is the following one, which

- states moving block a onto block c succeeds, if a and c

- are clear for the entire duration, and a is not equal to c.

∀s, a, c, e, l.

Holds(On(a, l ), s)∧

HoldsD(Clear (Top(c)), s, e)

a (cid:54)= c ∧ e = Move(a, Top(c))∧

l (cid:54)= top(c) ∧ HoldsD(Clear (Top(a)), s, e)∧









(cid:18)Holds(On(a, Top(c)), Result(e, s))∧

(l (cid:54)= T able → ¬Holds(On(a, l ), Result(e, s)))(cid:19)

→

- For this example we need some other facts about the world,

- these are given in an Appendix. We are also told that the

- only block that is not on the table is A, which is on B, and

∀s, s(cid:48), a, c, a(cid:48), c(cid:48), b(cid:48).(s ≤ s(cid:48)∧

¬



∀s(cid:48)(cid:48), a(cid:48).s ≤ Result(a, s (cid:48)(cid:48)) ≤ s (cid:48) →

Occurs(Move(a, a (cid:48)), s (cid:48)(cid:48)) ∧ ∃g1 , g2 .



Changes(cid:18)Move(a, a (cid:48)),

→(cid:18) Holds(On(a, Top(c)), s) ≡

Holds(On(a, Top(c)), s (cid:48)). (cid:19)On(a, T op(c)), g1, g2(cid:19)In general we shall prefer the stronger frame axiom.We wish to prove that

Holds(On(A, Top(C )), Result(Move(A, Top(C )), S0 )),however, we can only prove the weaker,

∀e.e = Move(A, Top(C ))∧

HoldsD(Clear (A), s, e) ∧ HoldsD(Clear (C ), s, e) →Holds(On(A, Top(C )), Result(e, S0 ))Thus, we need to prove that A and C remain clear duringthe move action in order to show that the action is suc-cessful. To show that they remain clear we need to useour frame axiom. But, all we can prove is that the Ablock remains clear if there is no successful move actionMove(b1 , Top(A)) that occurs, and whose result is beforeResult(Move(A, C ), S0 ). As we allow situations beforeS0, we can imagine that there was a move in progress thatplaced a block on C just after S0.

Thus we explicitly state that no action that might put some-thing on A or C occurred in the interval15, save of course15We need to state that no event whose result lies in the interval- the action of putting A on the top of C.

∀s, b1, e.e = Move(b1 , Top(A)) →

≤ Result(Move(A, Top(C )), S0 )(cid:19)

¬(cid:18) Occurs(e, s) ∧ S0 ≤ Result(e, s)

¬(cid:18) Occurs(e, s) ∧ S0 ≤ Result(e, s)

≤ Result(Move(A, Top(C )), S0 ) (cid:19)

∀s, b1, e.e = Move(b1 , Top(C )) ∧ (s (cid:54)= S0 ∨ b (cid:54)= b1 ) →

- We can now prove that

Holds(On(A, Top(C )), Result(Move(A, Top(C )), S0 )),

- as the above principal allows us to prove that A and C are

- clear for the entire interval.

- We cannot prove that C remains on the Table however,

- as there may be events that put C on top of other blocks.

- These events will not make C unclear, so they do not block

- the action of putting A on C. To prove that C remains on

- the table we would need,

∀s, x.¬(cid:32) Occurs(Move(C , x ), s)∧

≤ Result(Move(A, C ), S0 ).(cid:33)

S0 ≤ Result(Move(C , x ), s)

- It is notable that there was no need to state that no other

- actions occurred. It sufﬁced16 to say that no other events

- occurred that might cause a precondition of an event in our

- narrative to fail. The notion that we need only state that

- certain events did not occur becomes very important if we

- wish to axiomatize domains in a way that will later allow

- them to be conjoined. In fact, the motivating property for

- developing this new axiomatization was to allow separate

- axiomatizations, that do not interfere with each other, to

- be conjoined. This is not possible in the old-style situation

- calculus, as we explicitly list the sequence of actions that

- occurs. It is also not possible if we state that the only events

- that occur are those mentioned, as is sometimes done in

- narrative reasoning.

- The reasoning that we did was not signiﬁcantly more

- difﬁcult than the usual reasoning in the situation calcu-

- or at the endpoints, thus the use of ≤. Sometimes, especially

- when we are checking preconditions of events, we will only need

- to show that nothing had an effect strictly before the end, and this

- we will only need to show <. When we try to use inertia we will

- need to show the ≤.

- 16In this paper we state that the other events do not hap-

- pen monotonically.

These statements can be inferred non-

- monotonically from sentences that tell which occurrences and

- what ﬂuents are explicitly stated to occur and hold in our nar-

- rative, and the axiomatization of Changes. A ﬂuent is relevant if

- it is a precondition or an effect of a stated event that occurs, or if

- the ﬂuent’s value is stated in the narrative. This gives us a notion

- of what the relevant ﬂuents and events are in terms of what ﬂuents

- and events are explicitly given in the narrative. We then state that

- no other events occur that would change the effects of the relevant

- ﬂuents. We avoid explaining this reasoning, as the machinery we

- currently use is quite complex.

lus. We needed to check a few more conditions, namelythat blocks remained clear, but strategies, such as goalregression[Rei91] continue to be effective.

4.2 EVENTS WITH MULTIPLE EFFECTSIn general an event may have more than one effect. Thepreconditions for each effect may differ, thus preconditionsmay be parameterized by the effect. Furthermore, each ef-fect may occur at a different time.

4.2.1 Extending Result

If there is more than one effect of an event e, wewrite Result(e, s) for the time of the main effect, andResult(e, f , s) for the time of the effect of changing f.For instance, ﬂying from Glasgow to London has as itsresulting situation the situation where you arrive in Lon-don. However, another effect of this event is to no longerhave your ticket, as the air-hostess takes it from you. Thesituation where she takes the ticket off you is picked out byResult(Fly(Glasgow , LHR, T1 ), takenby(Steward , ticket), s).At this situation, the ﬂuent Has(ticket) is also made false.We use other situations like the time the airline-stewardtakes your ticket, rather than explicit times, as explicittimes, like all numerical values are less natural—the num-bers are hard to get. The statement that you no longer haveyour ticket after the air-hostess takes it is very intuitive,while the statement that your no longer have your ticketafter n minutes, for some deﬁnition of n is not.4.2.2 Implied events

We can deal with events having multiple effects at differ-ent times by stating that certain events trigger other events.Thus we might write,

∀s, l, l(cid:48).occurs(F ly(l, l(cid:48), T icket), s) →occurs(take(Steward, T icket), s).This is an alternate way to model the notion that the airlinesteward takes your ticket during the ﬂight.

We now consider narratives in two domains. One concernsstacking blocks, the other a plane journey. We show thatwe can axiomatize these two narratives separately, but insuch a way that their conjunction is consistent.5 GLASGOW, LONDON, MOSCOW ANDNEW YORK

The object of this section is to give narratives illustratingthe treatment of concurrent events in two cases. The ﬁrstis when two sub-narratives do not interact, and the second- is when they do. The ﬁrst sub-narrative is ordinary block

- stacking (as discussed in many situation calculus papers),

- and we suppose the stacking to be done by a person called

- Daddy in New York.

- In the second sub-narrative, the actor is named Junior, and

- he wants to ﬂy from Glasgow to Moscow via London. The

- story is taken from earlier web-published but widely cir-

- culated manuscripts [McC92] discussing how circumscrip-

- tion could be used to treat an unexpected obstacle to a plan,

- and [McC95] how narratives should be represented. This

- story is also used by Shanahan in [Sha97] in Chapter 10 as

- an example to motivate a use of context.

- These two sub-narratives do not interact, and thus give an

- example of our ﬁrst goal, a treatment of non-interacting

- narratives that can be conjoined consistently.

- Because we want to treat interacting events, we make life

- more complicated for Junior. If he loses his ticket, he must

- wire Daddy in New York for money. Daddy, who normally

- indulges Junior, has to interrupt his block stacking and sell

- a block in order to get the money to send Junior. In this part

- of the narrative we have an example of adding details to an

- event. We state the event of Junior getting money occurs,

- we also give a sequence of events, Daddy stacking blocks

- until block3 is clear, then selling block 3, receiving money

- and sending it to Junior. The sequence realizes the single

- event of getting money. We show that both statements are

- consistent with each other, and the explanation can be con-

- sistently conjoined onto the narrative that mentions only

- the ﬁrst event.

- The following uses the axiomatizations of traveling and

- commerce and blocks-world in the Appendix. In the text

- we only give those axioms that are particular to the story.

- We give axiomatizations of both the narratives where Ju-

- nior loses his ticket, and contacts Daddy who sends him

- money, that Daddy raises by selling a block (In New York,

- blocks are made of Gold). Naturally Daddy has to clear

- the block before selling it, so the narratives interact in a

- non-trivial way.

- Narrative 1

- In this narrative Junior doesn’t lose his tickets, T1 and T2

- and gets to Moscow without asking for help. Daddy stacks

- blocks in New York. There is no interaction, and noth-

- ing is said about the time relations between the two sub-

- narratives.

Holds(At(J , Glasgow ), S0 )

Occurs(Fly(Glasgow , LHR, T1 ), S0 )

Dest(T1) = LHR ∧ Source(T1) = Glasgow

Holds(Has(J , T1 ), S0 )

Dest(T2) = M oscow ∧ Source(T2) = LHR

Holds(Has(J , T2 ), S0 )

Result(Fly(Glasgow , LHR, T1 ), S0 ) < S1

We should be able to infer:

Holds(At(J , LHR), S1 )

To infer this we need to know that,

∀s(cid:48), e.S0 ≤ Result(e, s (cid:48)) <

Result(Does(J , Fly(Glasgow , LHR, T1 )), S0 ) →¬(Occurs(e, s (cid:48)) ∧ Succeeds(e, Has(J , T1 ), s (cid:48)))That is, no event occurs that would cause Junior to losehis ticket before he has to give it to the air-hostess17. Weactually state the following stronger fact18, that no eventsthat would cause Junior to no longer have a ticket occur,save of course ﬂying from Glasgow.

∀s(cid:48), e.(t = T1 ∨ t = T2)∧

(e (cid:54)= F ly(Glasgow, LHR, T1) ∨ s(cid:48) (cid:54)= S0)∧S0 ≤ Result(e, s (cid:48)) < S1 →

¬(Occurs(e, s (cid:48)) ∧ Succeeds(e, Has(J , t), s (cid:48)))(5)When Junior is in London, inertia, and the instance of theabove axiom with t = T2, gets us that Junior still has theticket to Moscow. As for the ticket to London, we wouldinfer that he does not have it as we brought up the fact thata ticket is used up when one takes the ﬂight the ticket isfor. That is certainly a part of the knowledge of anyonewho travels using tickets. Thus someone who had traveledby bus would infer it about airplane travel. Indeed it couldbe inferred from more general principles about commerce,e.g. that a seller doesn’t want to allow the buyer to get anarbitrary number of what he has a paid for one of. However,anyone who travels has the more speciﬁc information anddoesn’t need to infer it from general principles about com-merce. Indeed he may never have formulated any generalprinciples about commerce.

Occurs(Fly(LHR, Moscow , T2 ), S1 )

Result(Fly(LHR, Moscow , T2 ), S1 ) < S2(6)We wish to infer,

Holds(At(Junior , Moscow ), S2 )

Again we need to know that no bad events occur, that is,Junior doesn’t lose any tickets.

∀s(cid:48), e.(e (cid:54)= F ly(LHR, M oscow, T2) ∨ s(cid:48) (cid:54)= S1)∧S1 ≤ Result(e, s (cid:48)) < S2 →

(7)¬(Occurs(e, s (cid:48)) ∧ Succeeds(e, Has(J , T2 ), s (cid:48)))We call these sentences N ar1J, that is the sentences from4 to 7. Now we begin Daddy’s life as a block stacker.17If we wished that the air-hostess took Junior’s ticket at an-other time, we might use our three argument version of result andwrite,

∀s(cid:48), e.S0 ≤ Result(e, s (cid:48)) <

Result(cid:18) Does(J, F ly(Glasgow, LHR, T1)),¬(Occurs(e, s (cid:48)) ∧ Succeeds(e, Has(J , T1 ), s (cid:48))).Has(J, T1), S0

(cid:19) →(4)

18Whether or not the stronger fact is warranted depends onwhether we wish to state that no event that might cause Juniorto lose his ticket happens, or no event that does cause Junior tolose his ticket happens.

- We have no ≤ relation between the situations S0 and S0(cid:48)

- and know nothing of their temporal relations.

If we as-

- serted S0 < S0(cid:48) < S1, then we could conclude that Ju-

- nior still had the tickets in S0(cid:48). Also asserting S0(cid:48) = S0

- would do no harm to the conclusions drawn about either

- sub-narrative.

Holds(At(D , NY ), S0 (cid:48))

Holds(Has(D , A1 ), S0 (cid:48))

Holds(Has(D , A2 ), S0 (cid:48))

Holds(Has(D , A3 ), S0 (cid:48))

Holds(On(A3 , Top A1 ), S0 (cid:48))

∀b.Holds(Clear (b), S0 (cid:48)) ≡ b (cid:54)= A1

Holds(On(A1 , Table), S0 (cid:48))

Holds(On(A2 , Table), S0 (cid:48))

Occurs(Does(D , Move(A3 , Table)), S0 (cid:48))

Result(Does(D , Move(A3 , Table)), S0 (cid:48)) < S1 (cid:48)

Occurs(Does(D , Move(A2 , Top A1 )), S1 (cid:48))

Result(Does(D , Move(A2 , Top A1 )), S1 (cid:48)) < S2 (cid:48)

Occurs(Does(D , Move(A3 , Top A2 )), S2 (cid:48))

Result(Does(D , Move(A3 , Top A2 )), S2 (cid:48)) < S3 (cid:48)

(8)

- We also need to know that no other actions that would in-

- terrupt the block stacking19 occur.

∀s(cid:48), e, a, b.S0(cid:48) ≤ Result(e, s (cid:48)) ≤ S3 (cid:48)∧

(s(cid:48) (cid:54)= S0(cid:48) ∨ e = Move(A3 , Table))∧

(s(cid:48) (cid:54)= S1(cid:48) ∨ e = Move(A2 , Top A1 ))∧

(s(cid:48) (cid:54)= S2(cid:48) ∨ e = Move(A3 , Top A2 )) →

¬(Occurs(e, s (cid:48)) ∧ Succeeds(e, On(a, Top(b)), s (cid:48)))

- We call the sentences from 8 to 9 N ar1D We now notice

- that if B is the axiomatization of blocksworld in the Ap-

- pendix, and T is the axiomatization of traveling, then

B ∧ T ∧ N ar1D ∧ N ar1J |=

Holds(On(A3 , Top A2 ), S3 (cid:48))∧

Holds(On(A3 , Table), S1 (cid:48))∧

Holds(At(J , LHR), S1 )∧

Holds(At(J , Moscow ), S2 )

Junior, sells a block and sends Junior the money, who buysa London-Moscow ticket and goes on to Moscow.We chose a telegram rather than a telephone call, becausewe would not want to tell about a telephone call as a se-quence of statements by Junior and Daddy but rather toregard its result as a joint action, e.g. an agreement thatJunior and Daddy would do certain actions.

Note also we haven’t treated what Daddy now knows asthe result of the telegram. It seems that treating knowledgeand treating agreement are similar in their requirement fortreating intentional entities. The intentional state that Ju-nior has requested that Daddy send him the money is notmerely that Daddy knows that Junior wants Daddy to sendhim the money. Also the agreement is likely to have some-thing like a bit of narrative as an argument, e.g. a set of ac-tions that Junior and Daddy will do with only partial timerelations between the actions.

Here we include sentences 4 and 5. Up to here, narrative 2is the same as narrative 1. We will also need the sentences8 and 9.

(9)

Occurs(Loses(J , T2 ), S1 )

(11)This contradicts 7, which stated that no event that lostthe ticket happened before S2. We want to regard los-ing the ticket as something that happens to Junior ratherthan as something he does. That’s why we don’t writedoes(J, lose ticket(LHR, M oscow)). The bad conse-quences of doing the latter would arise when we get aroundto writing laws that quantify over voluntary actions. Wewill use some of the same names now for situations thatare different than in narrative 1.

(10)

- Thus we can derive the obvious conclusions of our narra-

- tive. We further note that the two narratives are consistent.

Result(Loses(J , T2 ), S1 ) < S2

¬Holds(Has(J , Cash), S2 )

(12)<table align="center">
	<tr align="center">
		<td>3</td>
	</tr>
	<tr align="center">
		<td>2</td>
	</tr>
	<tr align="center">
		<td>1</td>
	</tr>
	<tr align="center">
	</tr>
	<tr align="center">
		<td>2</td>
	</tr>
	<tr align="center">
		<td>1</td>
		<td>3</td>
	</tr>
	<tr align="center">
	</tr>
	<tr align="center">
		<td></td>
		<td></td>
		<td>3</td>
	</tr>
	<tr align="center">
	</tr>
	<tr align="center">
		<td>3</td>
	</tr>
	<tr align="center">
		<td></td>
		<td></td>
	</tr>
</table>

- Narrative 2

- In this narrative Junior loses the ticket and sends a telegram

- to Daddy asking for money. Daddy, who normally indulges

- 19If we wish to restrict

this to block stacking in New

- York we would add a conjunct Holds(In(a, New York ), s (cid:48)) ∧

- Holds(In(b, New York ), s (cid:48)) to the left hand side of the implica-

- tion.

Daddy getting money to Junior. Here we are treating Daddyas if his actions were determined by our inputs. Sometimesit is useful to describe people in that way. In more elaboratenarratives we would need to reason about Daddy mentalprocesses, but for this case we can treat him as an automa-ton.

The following axiom characterizes what Daddy does when- he receives a request from Junior.

from 4 to 5 and 8 and 9 and 11 to 15.

Happens(Gets(J, Cash)),

∀s.Holds



(J, Request Send Cash)), s (cid:19)

Result(cid:18)Receives(D, T elegram-f rom



(J, Request Send Cash)), S2(cid:19) (14)

S3.5 = Result(cid:18)Receives(D, T elegram-f rom

N ar2 |=

¬Holds(Has(J , T2 ), S2 )∧

Holds(Has(D , Cash), S4 (cid:48))∧

Holds(Has(J , Cash), S3 )∧

Holds(Has(J ), T2 , S4 )∧

Holds(At(J , Moscow ), S5 )

- This is an example of a triggered action, as we have the

- deﬁning rule for Holds(Happens(e), s),

N ar2 ∧ Dep ∧ U |=

(16)(17)Most interestingly we can derive the occurrence of a trig-gered action:

Occurs(cid:0) gets(J, Cash), S3.5 (cid:1)

We have two explanations for Junior receiving the money,the gets event, and the send event. We cannot tell whichhappens ﬁrst, or if they happen simultaneously. Thus ourformalism allows us to add detail of an event without con-tradiction.

5.1 ELABORATIONS

Interpolating unconnected situations and events into a nar-rative does not harm the conclusions. For example, wecould put situations S0.5 and S0.7 between S0 and S1,and suppose that Junior reads a book on the airplane dur-ing the inner interval. The previous statements about whatholds when Junior arrives in London should still seem ok.Indeed we have that when we add

Occurs(Read (J , Book ), S 1 .5 )

∧S1 < S1.5 < Result(Read (J , Book ), S 1 .5 ) < S2∀s.Holds(Intelligent(P ), Result(Read (P , Book ), s))we can conclude all our previous sentences, plus somemore about Junior’s intelligence, something we earlier didnot have an opinion about.

In our second narrative, after S2 we have two possible ex-planations of how Junior gets the cash to buy his ticket.One explanation is that Daddy always gets cash to Junior.We also have the more detailed explanation that Daddysells A3 and sends the proceeds to Junior. The more de-tailed explanation is an elaboration of how Daddy got themcash to Junior. If is worth noting that both explanations canco-exist in our narrative without inconsistencies.5.2 ELABORATION OF NARRATIVESSuppose we are asked, “How did Junior ﬂy from Glasgowto London?” and want to respond with facts about tak-ing a taxi to the airport, presenting his ticket at the check-in counter, going to the gate, getting on the airplane, tak-ing his assigned seat, etc. We can add this additional nar-rative with its intermediate situations, and we can throwin reading the book if we like. There is no reason todiscard Occurs(Does(J , Fly(Glasgow , LHR, T1 )), S0 ).∀e, s.Holds(Happens(e), s) ≡ Occurs(e, s).

- We now state that the money arrives before S3, when Ju-

- nior buys the ticket.

Result(Gets(J , Cash), S3 .5 ) < S3

- We now give the other facts about occurrences.

S3(cid:48) =

Result(Does(J , Telegraph(D , Request Send Cash)), S2 )

¬Holds(Has(D , Cash), S3 (cid:48))

Occurs(Does(D , Sell A3 ), S3 (cid:48))

Result(Does(D , Sell A3 ), S3 (cid:48)) < S4 (cid:48)

Occurs(Does(D , Send (J , Cash)), S4 (cid:48))

Result(Does(D , Send (J , Cash)), S4 (cid:48)) < S3

Occurs(Does(J , Buy ticket(T2 )), S3 )

Result(Does(J , Buy ticket(T2 )), S3 ) < S4

Occurs(Does(J , Fly(LHR, Moscow )), S4 )

Result(Does(J , Fly(LHR, Moscow )), S4 ) < S5

- We also need to know that no events occur that would divert

- the money in the meantimes between these events and the

- result of the previous events.

∀s(cid:48), e.(e (cid:54)= Does(D, Send(J, Cash)) ∨ s(cid:48) (cid:54)= S4(cid:48))∧

Result(Does(D , SellA3 ), S3 (cid:48)) ≤

Result(e, s (cid:48)) ≤ S4 (cid:48) →

(15)

¬(cid:18) Occurs(e, s (cid:48))∧

Succeeds(e, Has(D, Cash), s(cid:48)) (cid:19)

∀s(cid:48), e.(e (cid:54)= Does(J, Buy ticket(T2)) ∨ s(cid:48) (cid:54)= S3)∧

Result(Does(D , Send (J , Cash)), S4 (cid:48)) ≤

Result(e, s (cid:48)) ≤ S3 →

¬(cid:18) Occurs(e, s (cid:48))∧

Succeeds(e, Has(J, Cash), s(cid:48)) (cid:19)

∀s(cid:48), e.(e (cid:54)= F ly(LHR, M oscow, T2) ∨ s(cid:48) (cid:54)= S4)∧

Result(Does(J , buy ticket(T2 )), S3 ) ≤

Result(e, s (cid:48)) ≤ S4 →

¬(cid:18) Occurs(e, s (cid:48))∧

Succeeds(e, Has(J, T2), s(cid:48)) (cid:19)

- We now consider the consequences of narrative two. Let

- N ar2 be those sentences directly above and the sentences

- We merely have a redundant way of reaching the same con-

- clusion. This is allowed in our formalism, and this property

- is demonstrated by the two ways in which we describe how

- Daddy gets the money for Junior.

- However, we would like a sentence relating the more de-

- tailed narrative to the less detailed narrative, i.e. of assert-

- ing that one realizes the other. For this we will at least need

- narratives as objects, and this has not yet been introduced.

- Note that the relation Elaborates(N 2, N 1), when we get

- around to introducing it, will not be like the relation be-

- tween a subroutine call and the subroutine. N 1 will not in

- any sense be the deﬁnition of N 2. N 2 could be realized in

- a number of ways, only one of which corresponds to N 1.

- 5.3 PLANNING AND PREDICTION

- We would like to treat the circumstances of the previous

- narrative from the point of view of planning. In that case

- we need to be explicit about the consequences of actions

- and other events. The difference between planning and nar-

- rative is that in narrative we know that events and actions

- will succeed. This allows us to make assumptions we oth-

- erwise could not make. We can also assume that all the

- important effects of actions are mentioned. When we plan

- we need to show that we have taken into account all the

- important effects.

- Let us consider the purposes of Junior and Daddy and pre-

- dict what actions they will take and what the outcome will

- be. Of course, Junior losing the ticket will be an unpre-

- dicted event. We just throw it in, but then we should be

- able to predict what Junior and Daddy will subsequently

- do. This seems more difﬁcult than either planning or pred-

- ication.

- 5.4 PHILOSOPHICAL CONSIDERATIONS

- Reality may be regarded as the deterministic limit of non-

- determinist approximations. In what a human or robot can

- know about the world many events are not inevitable. In

- any human account, it did not have to be raining when Ben-

- jamin Franklin ﬁrst arrived in London. Indeed, maybe it

- wasn’t. Even if the world is deterministic, any achievable

- description of it is nondeterministic. Elaborations of par-

- ticular narratives sometime remove some of the nondeter-

- minism by accounting for the causes of particular events

- and for ﬂuents holding in the results of these events.

- Therefore, it may be worthwhile to regard the world as de-

- terminist and suppose that every event has causes whether

- we know them or not. Thus any particular nondeterminism

- is potentially eliminable.

- It might be supposed that quantum mechanics vitiates these

- considerations, but we don’t think it requires modiﬁcations

on the common sense level. Free will in a determinist worldis discussed in [MH69].

5.4.1 REMARKS

We have always felt that the careful classiﬁcation of theways in which events can overlap is unnecessary for almostall common sense reasoning. We think this article shows it.Moreover, it is also usually unnecessary to combine con-current events into compound events as do Gelfond, Lifs-chitz and Rabinov [GLR91].

Acknowledgments

This research was partly funded by DARPA (ONR) grant #N00014-94-0775 under ARPI and by the Air Force Ofﬁceof Scientiﬁc Research Grant # F49620-97-1-0207 under theDARPA HPKB project and New World Vistas.References

[Cos97] Tom Costello. Non-monotonicity and Change.PhD thesis, Stanford University, 1997.[Cos98a] Tom Costello. Domain Formula Circumscrip-tion. Journal of Logic Language and Informa-tion, 1998. to appear.

[Cos98b] Tom Costello. Minimizing the Effects of Ac-tions. In Proceedings of the Fifth InternationalSymposium on Commonsense Reasoning, 1998.[GLR91] Michael Gelfond, Vladimir Lifschitz,andArkady Rabinov. What are the limitations ofthe situation calculus?

In Robert Boyer, edi-tor, Automated Reasoning: Essays in Honor ofWoody Bledsoe, pages 167–179. Kluwer Aca-demic, Dordrecht, 1991.

[KS97] R. Kowalski and F. Sadri. Reconciling the Sit-uation Calculus and Event Calculus. Journal ofLogic Programming, 31:39–58, 1997.[McC59] John McCarthy.

Programs with CommonSense20.

In Mechanisation of Thought Pro-cesses, Proceedings of the Symposium of the Na-tional Physics Laboratory, pages 77–84, Lon-don, U.K., 1959. Her Majesty’s Stationery Of-ﬁce. Reprinted in McC90.

[McC63] J. McCarthy. Situations, Actions and CausalLaws. Technical Report Memo 2, Stanford Uni-versity Artiﬁcial Intelligence Project, Stanford,CA, 1963.

20http://www-formal.stanford.edu/jmc/mcc59.html- [McC89] John McCarthy. Artiﬁcial intelligence, logic

and formalizing common sense.

In Richmond

Thomason, editor, Philosophical Logic and Ar-

tiﬁcial Intelligence. Kl¨uver Academic, 1989.

Overcoming an Unex-

- [McC92] John McCarthy.

pected Obstacle.

Available as http://www-

formal.stanford.edu/jmc/elaboration.html, 1992.

- [McC95] John McCarthy. Situation Calculus with Con-

current Events and Narrative21. 1995. Contents

subject to change. URL will remain.

- [MH69]

J. McCarthy and P. Hayes. Some Philosophical

Problems from the Standpoint of Artiﬁcial Intel-

ligence.

In D. Michie, editor, Machine Intelli-

gence 4, pages 463–502. Edinburgh University

Press, Edinburgh, UK, 1969.

- [PR95]

J. Pinto and R. Reiter. Reasoning about Time

in the Situation Calculus. Annals of Mathemat-

ics and Artiﬁcial Intelligence, 14(2-4):251–268,

September 1995.

- [Rei91] R. Reiter. The frame problem in the situation

calculus: A simple solution (sometimes) and

a completeness result for goal regression.

In

V. Lifschitz, editor, Artiﬁcial Intelligence and

Mathematical Theory of Computation, pages

359–380. Academic Press, 1991.

- [Rei93] R. Reiter.

Proving properties of states in

the situation calculus. Artiﬁcial Intelligence,

64(2):337–351, December 1993.

- [Sha97] Murray Shanahan.

Solving the Frame Prob-

lem, a mathematical investigation of the com-

mon sense law of inertia. M.I.T. Press, 1997.

- APPENDIX

- BLOCKSWORLD

- Our blocks world has 4 sorts, situations s, blocks b, loca-

- tions l and actions a. These are all disjoint.

- We have a situation constant S0, other situation constant

- Sn and Sn(cid:48) for various n’s, a set of block constants

- A1, . . . , An, . . ., where n ∈ ω and one block location con-

- stant T able. We also have constants A = A1, B = A2 and

- C = A3.

- All blocks are unique, but we do not postulate domain clo-

- sure.

Ai (cid:54)= Aj|i (cid:54)= j

- 21http://www-formal.stanford.edu/jmc/narrative.html

We have block locations22, which are the T op of a block,or are the T able.

∀l.∃b.T op(b) = l ∨ l = T able

All distinct block location terms denote distinct locations.∀b, b(cid:48).T op(b) = T op(b(cid:48)) → b = b(cid:48)

∀b.T op(b) (cid:54)= T able

We have a function from actions and situations to situa-tions, Result(a, s), and a function from blocks and loca-tions to actions, Move(b, l ), which gives the action whereblock b is moved to location l.

All distinct action terms are distinct.

∀b, b(cid:48), l, l(cid:48).Move(b, l ) = Move(b (cid:48), l (cid:48)) → b = b (cid:48) ∧ l = l (cid:48)We have the foundational axioms for situation calculus weconsidered earlier.

We have ﬂuents, Holds(On(b, l ), s) which states that bis on location l in situation s, and Holds(Clear (l ), s).Holds(Clear (l ), s) is fully deﬁned in terms of On.∀l, s.Holds(Clear (l ), s) ≡

(∃b.∀b(cid:48).l = T op(b(cid:48)) ∧ ¬Holds(On(b (cid:48), Top(b)), s))∨l = T able

We now add the obvious deﬁnition of Changes forM ove(b, l) actions. That is, there is a change in On(b, l(cid:48))and On(b, l) when g2 contains On(b, l(cid:48)) for an l(cid:48) not equalto l, and l

(cid:54)= T op(b), and g1 contains Clear(l) andClear(top(b)).

∀b, b(cid:48), l, l(cid:48), g1, g2.

Changes(M ove(b, l), On(b(cid:48), l(cid:48)), g1, g2) ≡l (cid:54)= l(cid:48) ∧ l (cid:54)= T op(b) ∧ b = b(cid:48)∧

(cid:18) (b = b(cid:48) ∧ g(¬On(b(cid:48), l(cid:48))))∨

(b (cid:54)= b(cid:48) ∧ g(On(b(cid:48), l(cid:48))))

G(Clear(l)) ∧ G(Clear(l(cid:48)))

(cid:19) ∧

This concludes the axiomatization of blocksworld, wecould add domain constraints, but this is not necessary forthe reasoning we do in this paper. We now present an ax-iomatization of traveling, followed by an axiomatization ofbuying selling and sending and receiving.

TRAVELING AND COMMERCE

Our events are ﬂying, doing actions, getting, receiving andlosing. Our actions are selling, sending, telegraphing. Dis-tinct event terms are distinct. We have two people con-stants, Junior and Daddy. We have among our objects Cash,22These are not the same as geographical locations like NewYork or London. We use l to range over both, which is unfortu-nate.

- a message (Request Send Cash), cities, including Lon-

- don, Glasgow and Moscow. We also have tickets, and func-

- tions which yield destination Dest and source Source of a

- ﬂight, given a ticket for that ﬂight. We have unique names

- for all ﬂuent terms, and thus for all the terms that can ap-

- pear in ﬂuents. Our ﬂuent forming functions are At which

- takes a person and a place, Has which takes a persona and

- a thing, Happens, which takes an event, and the earlier ﬂu-

- ents of On and Clear. Our sorts are disjoint, and the sorts

- of variables are to be inferred by their use.

∀s, P, l, t.Holds(At(P , l ), s) ∧ Source(t) = l ∧

Dest(t) = l(cid:48)∧

(∀s(cid:48).s ≤ s(cid:48) < Result(Fly(l , l (cid:48), t), s) →

Holds(Has(P , t), s (cid:48))) →

Holds(At(P , l (cid:48)), Result(Fly(l , l (cid:48), t), s))∧

¬Holds(Has(P , t), Result(Fly(l , l (cid:48), t), s))

- This is equivalent to

∀l, l(cid:48), t, P.

Changes(F ly(l, l(cid:48), t), Has(P, t), g1, g2) ←

Source(t) = l ∧ Dest(t) = l(cid:48)∧

g2(At(P, l)) ∧ g1(Has(P, t))

- As this is the only effect axiom for ﬂying, we can change

- this to the equivalence.

∀l, l(cid:48), t, P, g1, g2.

Changes(F ly(l, l(cid:48), t), Has(P, t), g1, g2) ≡

Source(t) = l ∧ Dest(t) = l(cid:48)∧

g2(At(P, l)) ∧ g1(Has(P, t))

∀s, P, o.¬Holds(has(P , o), Result(lose(P , o), s))

- This immediately gives,

∀s, P, g1, g2, o.Changes(Lose(P, o), Has(P, o), g1, g2)

- as any set of preconditions is sufﬁcient. We add the obvi-

- ous axioms that describe Changes for the following effect

- axioms using the same method.

∀r, P, s.Holds(Happens(Receives(P (cid:48),

T elegram-f rom(P, r))),

Result(Does(P , Telegraph(P (cid:48), r )), s))

Happens(Gets(J, Cash)),

∀s.Holds



Result(cid:18)Receives(D, T elegram-f rom(J,

Request Send Cash)), s

∀s, P, o.Holds(Has(P , o), s) ∧ Holds(Clear (o), s) →

Holds(Has(P , Cash), Result(Does(P , Sell (o)), s))

∀s, P, o.Holds(Has(P , o), s) ∧ Holds(Clear (o), s) →

¬Holds(Has(P , o), Result(Does(P , Sell (o)), s))

(cid:19)



∀s, P, P (cid:48), o.Holds(Has(P , o), s) →

Holds(Has(P (cid:48), o), Result(Does(P , Send (P (cid:48), o)), s))

∀s, P.Holds(Has(P , Cash), Result(Gets(P , Cash), s))

